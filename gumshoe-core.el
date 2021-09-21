;;; gumshoe-core.el --- Scoped spatial and temporal POINT movement tracking -*- lexical-binding: t; -*-

;; Copyright (C) 2021 overdr0ne

;; Author: overdr0ne
;; Version: 2.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: tools
;; URL: https://github.com/Overdr0ne/gumshoe

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Gumshoe is a collection of global minor modes that quietly
;; keep tabs on your Point movements so you can retrace your steps if you
;; ever need a reminder of where you’ve been.  Each mode keeps a log local
;; to some scope.

;; Gumshoes log any movements outside their minimum follow distance.
;; They will also log any position you idle at for a while.
;; You may then use their log to backtrack to previous locations.

;;; Code:

(require 'eieio)
(require 'cl-lib)
(require 'subr-x)
(require 'ring)

(defgroup gumshoe nil
  "The gumshoe movement tracker."
  :group 'convenience
  :prefix "gumshoe-")

(defcustom gumshoe-log-len 300
  "Length of gumshoe--backlog’s ring-buffer."
  :type 'integer)
(defcustom gumshoe-follow-distance 15
  "Gumshoe logs movements beyond this Euclidean distance from previous entry."
  :type 'integer)
(defcustom gumshoe-horizontal-scale 4
  "Horizontal follow distances are divided by this factor."
  :type 'integer)
(defcustom gumshoe-idle-time 60
  "Gumshoe automatically logs your position if you’ve been idle at POINT for this amount of time."
  :type 'integer)
(defcustom gumshoe-show-footprints-p t
  "Display footprint overlays when backtracking?"
  :type 'boolean)

(defcustom gumshoe-entry-type 'gumshoe--entry
  "Type of entry Gumshoe should use in the backlog."
  :type 'symbol)

(defcustom gumshoe-slot-schema '(time buffer position line)
  "Entry slot order for perusing the backlog."
  :type 'list)

(defface gumshoe--footprint-face
  '((t
     :inherit highlight
     :box (:line-width -3 :style released-button)
     :weight bold))
  "Face for footprint overlays.")
(defface gumshoe--current-footprint-face
  '((t :inherit match
       :box (:line-width -3 :style released-button)
       :weight bold))
  "Face for footprint overlays.")

(defcustom gumshoe-display-buffer-action '((display-buffer-reuse-window display-buffer-same-window))
  "`display-buffer-action’ to use when jumping through the backlog.

See `display-buffer' for more information"
  :type 'list)

(defclass gumshoe--entry ()
  ((filename :initform (buffer-file-name)
             :documentation "The full path of this entry.")
   (buffer :initform (current-buffer)
           :documentation "The buffer object of this entry."
           :printer buffer-file-name)
   (position :initform (point)
             :documentation "Buffer position of this entry.")
   (line :initform (buffer-substring (line-beginning-position) (line-end-position))
         :documentation "A formatted line containing the position of this entry.")
   (time :initform (current-time-string)
         :documentation "Indicates the date and time of this entry.")
   (major-mode :initform (symbol-name major-mode)
               :documentation "Major mode of this entry."))
  "Entry class for Gumshoe’s backlog.")

(cl-defmethod gumshoe--valid-p ((self gumshoe--entry))
  "Return t if SELF is valid."
  (not (cl-check-type self gumshoe--entry)))

(cl-defmethod gumshoe--jump ((self gumshoe--entry))
  "Jump Point to buffer and position in SELF."
  (with-slots (buffer position) self
    (pop-to-buffer buffer)
    (goto-char position)))

;;; filter predicates
(cl-defmethod gumshoe--in-current-buffer-p ((entry gumshoe--entry))
  "Check if ENTRY in the current perspective."
  (equal (oref entry buffer) (current-buffer)))

(defclass gumshoe--backlog ()
  ((ring :initform (make-ring gumshoe-log-len)
         :documentation "Ring-buffer to remember the previous editing position.")
   (entry-type :initform 'gumshoe--entry :initarg :entry-type
               :documentation "Type of entries in the log ring."))
  "Gumshoe’s backlog for tracking POINT positions.")

(defclass gumshoe--backtracker ()
  ((backlog :initform nil :initarg :backlog
            :documentation "Ring-buffer to remember the previous editing position.")
   (filtered :initform nil
             :documentation "The filtered log list used when backtracking.")
   (footprints :initform nil
               :documentation "An overlay indicating previous locations.")
   (index :initform 0
          :documentation "Current index backwards into the log when backtracking.")
   (backtrackingp :initform nil
                  :documentation "Flag indicating when a gumshoe is using the log to backtrack.")
   (startp :initform nil
           :documentation "Flag indicating when backtracking has begun.")
   (msg :initform ""
        :documentation "Stores info for the user during backtracking."))
  "Gumshoe’s backtracker keeps track of backtracking state.")

(cl-defmethod gumshoe--clean ((self gumshoe--backlog))
  "Cleanup entries from SELF without a buffer."
  (with-slots (ring) self
    (let ((i 0)
          (n (ring-length ring)))
      (while (< i n)
        (let* ((entry (ring-ref ring i))
               (buffer (oref entry buffer)))
          (if (buffer-live-p buffer)
              (cl-incf i)
            (ring-remove ring i)))))))

;;; Peruse
(defun gumshoe--format-record (rec format-string slot-spec)
  "Format REC according to FORMAT-STRING using SLOT-SPEC fields."
  (let* ((slot-vals (mapcar #'(lambda (slot) (slot-value rec slot)) slot-spec)))
    (apply #'format format-string slot-vals)))
(defun gumshoe--format-records (rec-list format-string slot-spec)
  "Format records in REC-LIST according to FORMAT-STRING using SLOT-SPEC fields."
  (mapcar #'(lambda (rec) (gumshoe--format-record rec format-string slot-spec)) rec-list))
(defun gumshoe--peruse (recs slot-spec &optional entry-filter)
  "Peruse SLOT-SPEC fields of RECS.

Pre-filter results with ENTRY-FILTER."
  (let* ((entries recs)
         (format-schema (string-join (mapcar #'symbol-name slot-spec) "|"))
         (prompt (concat "(" format-schema "): "))
         (format-components (mapcar #'(lambda (_) "%s") slot-spec))
         (format-string (string-join format-components "|"))
         (filtered-entries (if entry-filter
                               (seq-filter entry-filter entries)
                             entries))
         (entry-strings (gumshoe--format-records filtered-entries format-string slot-spec))
         (candidates (cl-mapcar #'list entry-strings filtered-entries))
         (candidate (completing-read prompt candidates)))
    (gumshoe--jump (cadr (assoc candidate candidates)))))

;; tracking
(defun gumshoe--line-number-at (pos)
  "Return column number at POS."
  (save-excursion
    (goto-char pos)
    (current-column)))
(cl-defmethod gumshoe--distance-to ((self gumshoe--entry))
  "Return the Euclidean distance between point and SELF."
  (let* ((pos (oref self position))
         (line (line-number-at-pos pos))
         (dline (abs (- line
                        (line-number-at-pos (point)))))
         (column (gumshoe--line-number-at pos))
         (dcolumn (abs (- column
                          (current-column))))
         (dcolumn-scaled (/ dcolumn gumshoe-horizontal-scale)))
    (sqrt (+ (expt dline 2) (expt dcolumn-scaled 2)))))
(cl-defmethod gumshoe--end-of-leash-p ((last-entry gumshoe--entry))
  "Check if LAST-ENTRY is outside gumshoe’s boundary."
  (> (gumshoe--distance-to last-entry)
     gumshoe-follow-distance))
(cl-defmethod gumshoe--equal ((self gumshoe--entry) (other gumshoe--entry))
  "Return t if SELF and OTHER are approximately equal."
  (and
   (equal (oref self filename) (oref other filename))
   (equal (oref self position) (oref other position))))
(cl-defmethod gumshoe--log-if-necessary ((self gumshoe--backlog) &optional alarmp)
  "Check current position and log in SELF if needed.

Log automatically if ALARMP is t."
  (with-slots (ring entry-type) self
    (unless (minibufferp)
      (let ((new-entry (funcall entry-type)))
        (when (or (ring-empty-p ring)
                  (let ((latest-entry (ring-ref ring 0)))
                    (and (not (gumshoe--equal new-entry latest-entry))
                         (or alarmp
                             (not (gumshoe--in-current-buffer-p latest-entry))
                             (gumshoe--end-of-leash-p latest-entry)))))
          (ring-insert ring new-entry))))))

(cl-defmethod gumshoe--pre-track ((self gumshoe--backtracker))
  "Initialize backtracking for SELF if necessary."
  (with-slots (backtrackingp startp) self
    (unless backtrackingp
      (setf startp t))
    (setf backtrackingp nil)))

(cl-defmethod gumshoe--post-track ((self gumshoe--backtracker))
  "Log the current position to SELF if not backtracking."
  (with-slots (backtrackingp backlog footprints) self
    (unless backtrackingp
      (gumshoe--hide-footprints footprints)
      (gumshoe--log-if-necessary backlog))))

;;; footprints
(cl-defmethod gumshoe--mark-footprint ((self gumshoe--entry) id face)
  "Add footprint overlay to SELF, labeled with ID, using FACE."
  (with-slots (position buffer) self
    (message (buffer-name buffer))
    (let* ((label (int-to-string id))
           (overlay (make-overlay position position buffer)))
      (when (and buffer (> (buffer-size buffer) 1))
        (put-text-property 0 (length label) 'face face label)
        (overlay-put overlay 'after-string label))
      overlay)))
(defun gumshoe--replace-footprint (footprints index face)
  "Add footprint overlay at footprint INDEX in FOOTPRINTS, using FACE."
  (let* ((label (int-to-string (- (length footprints) index)))
         (footprint (nth index footprints)))
    (put-text-property 0 (length label) 'face face label)
    (overlay-put footprint 'after-string label)))
(defun gumshoe--hl-current-footprint (footprints prev-index cur-index)
  "Replace PREV-INDEX with CUR-INDEX as current footprint in FOOTPRINTS."
  (gumshoe--replace-footprint footprints prev-index 'gumshoe--footprint-face)
  (gumshoe--replace-footprint footprints cur-index 'gumshoe--current-footprint-face))
(defun gumshoe--mark-footprints (entries)
  "Display footprints for all ENTRIES."
  (let ((i 1) footprints)
    (dolist (entry (reverse entries))
      (push (gumshoe--mark-footprint entry i 'gumshoe--footprint-face)
            footprints)
      (cl-incf i))
    footprints))
(defun gumshoe--hide-footprints (footprints)
  "Hide footprints in FOOTPRINTS."
  (while footprints
    (let ((footprint (pop footprints)))
      (delete-overlay footprint))))

;;; backtracking
(cl-defmethod gumshoe--increment-index ((self gumshoe--backtracker) incrementer)
  "Increment index in SELF with INCREMENTER function.

In particular, notify users if index would go outside log boundaries."
  (with-slots (index msg filtered footprints) self
    (setf index (funcall incrementer index 1))
    (cond
     ((>= index (length filtered))
      (setf msg "This is the earliest entry...")
      (setf index (- (length filtered) 1)))
     ((< index 0)
      (setf msg "This is the latest entry...")
      (setf index 0))
     (t
      (setf msg (format "Gumshoe: entry #%i"
                        (- (length filtered) index)))))))
(cl-defmethod gumshoe--init-backtracking ((self gumshoe--backtracker) filter)
  "FILTER SELF, and reset slots to start backtracking."
  (with-slots (backlog filtered footprints msg startp index) self
    (gumshoe--clean backlog)
    (setf filtered (if filter
                       (seq-filter filter (ring-elements (oref backlog ring)))
                     (ring-elements (oref backlog ring))))
    (when gumshoe-show-footprints-p
      (setf footprints (gumshoe--mark-footprints filtered)))
    (setf startp nil)
    (setf index 0)))
(cl-defmethod gumshoe--backtrack ((self gumshoe--backtracker) incrementer filter)
  "Backtrack using INCREMENTER in SELF.

Only including results satisfying FILTER.
INCREMENTER increments the index in SELF."
  (with-slots (startp backtrackingp index filtered msg footprints) self
    (let ((prev-index index))
      (when startp (gumshoe--init-backtracking self filter))
      (gumshoe--increment-index self incrementer)
      (setf startp nil)
      (when gumshoe-show-footprints-p (gumshoe--hl-current-footprint footprints prev-index index))
      (if (not filtered)
          (setf msg "I haven’t recorded any entries here yet...")
        (gumshoe--jump (nth index filtered)))
      (setf backtrackingp t)
      (when msg (message msg)))))
(cl-defmethod gumshoe--timer-callback ((self gumshoe--backtracker))
  "Called by timer to log current position in SELF."
  (with-slots (backtrackingp backlog) self
    (unless backtrackingp
      (gumshoe--log-if-necessary backlog t))))

;;; Mode definition
(defclass gumshoe--mode ()
  ((backlog :initform nil
            :initarg :backlog
            :documentation "Stores previous contexts recorded by Gumshoe.")
   (backtracker :initform nil
                :documentation "Stores the backtracking state.")
   (timer :initform nil
          :documentation "Global idle timer that logs position for `gumshoe--global-backlog’ after `gumshoe-idle-time'."))
  "Gumshoe mode information.")
(cl-defmethod gumshoe--init ((self gumshoe--mode) entry-type)
  "Initialize SELF with ENTRY-TYPE, setting hooks and timers."
  (with-slots (backlog backtracker timer) self
    (setf backlog (gumshoe--backlog :entry-type entry-type))
    (setf backtracker (gumshoe--backtracker
                       :backlog backlog))
    (setf timer (run-with-idle-timer gumshoe-idle-time t
                                     (apply-partially #'gumshoe--timer-callback backtracker)))
    (add-hook 'pre-command-hook
              (apply-partially #'gumshoe--pre-track backtracker))
    (add-hook 'post-command-hook
              (apply-partially #'gumshoe--post-track backtracker)))
  self)
(cl-defmethod gumshoe--shutdown ((self gumshoe--mode))
  "Shutdown SELF, removing hooks and cancelling timers."
  (with-slots (backtracker timer) self
    (remove-hook 'pre-command-hook
                 (apply-partially #'gumshoe--pre-track backtracker))
    (remove-hook 'post-command-hook
                 (apply-partially #'gumshoe--post-track backtracker))
    (cancel-timer timer))
  self)

(defvar gumshoe-mode nil
  "Contains global data for gumshoe-mode.")
;;;###autoload
(define-minor-mode global-gumshoe-mode
  "Toggle global Gumshoe minor mode.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When enabled, Gumshoe logs point movements when they exceed the
`gumshoe-follow-distance', or when the user is idle longer than
`gumshoe-idle-time'."
  :init-value nil
  :lighter " Gumshoe:global"
  :group 'gumshoe
  :global t
  (if global-gumshoe-mode
      (setf gumshoe-mode (gumshoe--init (gumshoe--mode) 'gumshoe--entry))
    (setf gumshoe-mode (gumshoe--shutdown gumshoe-mode))))

(defun global-gumshoe-buf-mode (&optional _)
  "Obsolete mode for buffer local tracking."
  (interactive) (global-gumshoe-mode +1))
(make-obsolete 'global-gumshoe-buf-mode 'global-gumshoe-mode "2.0")

;;; interface setup
(defmacro gumshoe--make-xface (backtrack-back-name backtrack-forward-name peruse-name filter-name)
  "Make a command interface for the given filter.

BACKTRACK-BACK-NAME and BACKTRACK-FORWARD-NAME are names for the backtracking
commands.
A command will be generated for perusal called PERUSE-NAME.
Results will be filtered using FILTER-NAME function."
  `(progn
     (defun ,peruse-name ()
       (interactive)
       (gumshoe--peruse (ring-elements (oref (oref gumshoe-mode backlog) ring))
                        gumshoe-slot-schema
                        #',filter-name))
     (defun ,backtrack-back-name () (interactive) (gumshoe--backtrack (oref gumshoe-mode backtracker) #'+ #',filter-name))
     (defun ,backtrack-forward-name () (interactive) (gumshoe--backtrack (oref gumshoe-mode backtracker) #'- #',filter-name))))
(gumshoe--make-xface gumshoe-backtrack-back gumshoe-backtrack-forward gumshoe-peruse-globally gumshoe--valid-p)
(gumshoe--make-xface gumshoe-buf-backtrack-back gumshoe-buf-backtrack-forward gumshoe-peruse-in-buffer gumshoe--in-current-buffer-p)

(provide 'gumshoe-core)
;;; gumshoe-core.el ends here
