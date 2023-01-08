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

(defcustom gumshoe-prefer-same-window nil
  "Prefer jumping using the window where point currently is."
  :type 'boolean)

(defface gumshoe--peruse-separator-face
  '((t
     :inherit diary))
  "Face for peruse separators.")
(defface gumshoe--footprint-face
  '((t
     :inherit highlight
     :box (:line-width -3 :style sunken)
     :weight bold))
  "Face for footprint overlays.")
(defface gumshoe--current-footprint-face
  '((t :inherit match
       :box (:line-width -3 :style sunken)
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
               :documentation "Major mode of this entry.")
   (window :initform (get-buffer-window (current-buffer))
           :documentation "Window of this entry."))
  "Entry class for Gumshoe’s backlog.")

(cl-defmethod gumshoe--valid-p ((self gumshoe--entry))
  "Return t if SELF is valid."
  (not (cl-check-type self gumshoe--entry)))

(cl-defmethod gumshoe--jump ((self gumshoe--entry))
  "Jump Point to buffer and position in SELF."
  (with-slots (buffer position) self
    (if gumshoe-prefer-same-window
        (pop-to-buffer-same-window buffer)
      (pop-to-buffer buffer))
    (goto-char position)))

(cl-defmethod gumshoe--dead-p ((self gumshoe--entry))
  "Check if SELF is dead."
  (let* ((buffer (oref self buffer))
         (pos (oref self position)))
    (or (not (buffer-live-p buffer))
        (with-current-buffer buffer
          (>= pos (point-max))))))

;;; filter predicates
(cl-defmethod gumshoe--in-current-buffer-p ((entry gumshoe--entry))
  "Check if ENTRY in the current perspective."
  (equal (oref entry buffer) (current-buffer)))

(cl-defmethod gumshoe--in-current-window-p ((entry gumshoe--entry))
  "Check if ENTRY in the current window."
  (equal (oref entry window) (get-buffer-window (current-buffer))))

(defclass gumshoe--backtracker ()
  ((backlog :initform (make-ring gumshoe-log-len)
            :documentation "Ring-buffer to remember the previous editing position.")
   (filter :initform #'gumshoe--valid-p
           :documentation "Filter used when backtracking.")
   (filtered :initform nil
             :documentation "The filtered log list used when backtracking.")
   (footprints :initform nil
               :documentation "An overlay indicating previous locations.")
   (index :initform 0
          :documentation "Current index backwards into the log when backtracking.")
   (msg :initform ""
        :documentation "Stores info for the user during backtracking."))
  "Gumshoe’s backtracker keeps track of backtracking state.")

(cl-defmethod gumshoe--clean-recent (ring)
  "Cleanup recent dead entries from RING."
  (unless (ring-empty-p ring)
    (let ((i 0)
          (continuep t))
      (while (and continuep
                  (< i (ring-length ring)))
        (let ((entry (ring-ref ring i)))
          (if (gumshoe--dead-p entry)
              (ring-remove ring i)
            (setq continuep nil)))))))

(cl-defmethod gumshoe--clean (ring)
  "Cleanup dead entries from RING."
  (unless (ring-empty-p ring)
    (let ((i 0))
      (while (< i (ring-length ring))
        (let ((entry (ring-ref ring i)))
          (if (gumshoe--dead-p entry)
              (ring-remove ring i)
            (cl-incf i)))))))
;;; Peruse
(defun gumshoe--format-record (rec format-string slot-spec)
  "Format REC according to FORMAT-STRING using SLOT-SPEC fields."
  (let* ((slot-vals (mapcar #'(lambda (slot)
				                (ignore-error invalid-slot-name
				                  (slot-value rec slot))) slot-spec)))
    (apply #'format format-string slot-vals)))
(defun gumshoe--format-records (rec-list format-string slot-spec)
  "Format records in REC-LIST according to FORMAT-STRING using SLOT-SPEC fields."
  (mapcar #'(lambda (rec) (gumshoe--format-record rec format-string slot-spec)) rec-list))
(defun gumshoe--peruse (recs slot-spec &optional entry-filter)
  "Peruse SLOT-SPEC fields of RECS.

Pre-filter results with ENTRY-FILTER."
  (let* ((entries recs)
         (format-schema (string-join (mapcar #'symbol-name slot-spec) (propertize "|" 'face 'gumshoe--peruse-separator-face)))
         (prompt (concat (propertize "(" 'face 'gumshoe--peruse-separator-face)
			             format-schema
			             (propertize ")" 'face 'gumshoe--peruse-separator-face) ": "))
         (format-components (mapcar #'(lambda (_) "%s") slot-spec))
	     (separator (propertize "|" 'face 'gumshoe--peruse-separator-face))
         (format-string (string-join format-components separator))
         (filtered-entries (if entry-filter
                               (seq-filter entry-filter entries)
                             entries))
         (entry-strings (gumshoe--format-records filtered-entries format-string slot-spec))
         (candidates (cl-mapcar #'list entry-strings filtered-entries))
         (candidate (completing-read prompt candidates)))
    (gumshoe--jump (cadr (assoc candidate candidates)))))

;; tracking
(defun gumshoe--column-at (pos)
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
         (column (gumshoe--column-at pos))
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
(defun gumshoe--log-if-necessary (ring &optional alarmp)
  "Check current position and log in RING if significant.

Log automatically if ALARMP is t."
  (unless (or global-gumshoe-backtracking-mode (minibufferp))
    (gumshoe--clean-recent ring)
    (let ((new-entry (funcall gumshoe-entry-type)))
      (when (or (ring-empty-p ring)
                (let ((latest-entry (ring-ref ring 0)))
                  (and (not (gumshoe--equal new-entry latest-entry))
                       (or alarmp
                           (not (gumshoe--in-current-buffer-p latest-entry))
                           (gumshoe--end-of-leash-p latest-entry)))))
        (ring-insert ring new-entry)))))

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
  (when footprints
    (gumshoe--replace-footprint footprints prev-index 'gumshoe--footprint-face)
    (gumshoe--replace-footprint footprints cur-index 'gumshoe--current-footprint-face)))
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
(cl-defmethod gumshoe--init-backtracking ((self gumshoe--backtracker) _filter)
  "FILTER SELF, and reset slots to start backtracking."
  (with-slots (backlog filter filtered footprints msg index) self
    (setf filter _filter)
    (gumshoe--clean backlog)
    (setf filtered (if filter
                       (seq-filter filter (ring-elements backlog))
                     (ring-elements backlog)))
    (when gumshoe-show-footprints-p
      (setf footprints (gumshoe--mark-footprints filtered)))
    (setf index -1)))
(cl-defmethod gumshoe--backtrack ((self gumshoe--backtracker) incrementer)
  "Backtrack using INCREMENTER in SELF.

Only including results satisfying FILTER.
INCREMENTER increments the index in SELF."
  (with-slots (index filter filtered msg footprints) self
    (let ((prev-index index))
      (gumshoe--increment-index self incrementer)
      (when gumshoe-show-footprints-p (gumshoe--hl-current-footprint footprints prev-index index))
      (if (not filtered)
          (setf msg "I haven’t recorded any entries here yet...")
        (gumshoe--jump (nth index filtered)))
      (when msg (message msg)))))
(defun gumshoe-backtrack-back ()
  "Backtrack back in the Gumshoe backlog."
  (interactive)
  (gumshoe--backtrack (oref gumshoe-mode backtracker) #'+))

(defun gumshoe-backtrack-forward ()
  "Backtrack back in the Gumshoe backlog."
  (interactive)
  (gumshoe--backtrack (oref gumshoe-mode backtracker) #'-))

(cl-defmethod gumshoe--timer-callback ((self gumshoe--backtracker))
  "Called by timer to log current position in SELF."
  (with-slots (backlog) self
    (gumshoe--log-if-necessary backlog t)))

(defvar global-gumshoe-backtracking-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap keyboard-quit] 'gumshoe-backtrack-quit)
    map)
  "Transient keymap activated during global-gumshoe-backtracking-mode.")
(define-minor-mode global-gumshoe-backtracking-mode
  "A transient global mode to start Gumshoe backtracking."
  :global t
  :keymap global-gumshoe-backtracking-mode-map
  (if global-gumshoe-backtracking-mode
      (progn
        (push `(global-gumshoe-backtracking-mode . ,global-gumshoe-backtracking-mode-map)
              minor-mode-map-alist))
    (setf minor-mode-map-alist (assoc-delete-all 'global-gumshoe-backtracking-mode minor-mode-map-alist))))

;;; Mode definition
(defclass gumshoe--mode ()
  ((backtracker :initform nil
                :documentation "Stores the backtracking state.")
   (timer :initform nil
          :documentation "Global idle timer that logs position for `gumshoe--global-backlog’ after `gumshoe-idle-time'."))
  "Gumshoe mode information.")
(cl-defmethod gumshoe--init ((self gumshoe--mode))
  "Initialize SELF, setting hooks and timers."
  (with-slots (backtracker timer) self
    (setf backtracker (gumshoe--backtracker))
    (add-hook 'post-command-hook
              (apply-partially #'gumshoe--log-if-necessary (oref backtracker backlog)))
    (setf timer (run-with-idle-timer gumshoe-idle-time t
                                     (apply-partially #'gumshoe--timer-callback backtracker))))
  self)
(cl-defmethod gumshoe--shutdown ((self gumshoe--mode))
  "Shutdown SELF, removing hooks and cancelling timers."
  (with-slots (backtracker timer) self
    (remove-hook 'post-command-hook
                 (apply-partially #'gumshoe--log-if-necessary (oref backtracker backlog)))
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
      (setf gumshoe-mode (gumshoe--init (gumshoe--mode)))
    (setf gumshoe-mode (gumshoe--shutdown gumshoe-mode))))

(defun global-gumshoe-buf-mode (&optional _)
  "Obsolete mode for buffer local tracking."
  (interactive) (global-gumshoe-mode +1))
(make-obsolete 'global-gumshoe-buf-mode 'global-gumshoe-mode "2.0")

;;; interface setup
(defun gumshoe-backtrack-quit ()
  "Quit backtracking."
  (interactive)
  (global-gumshoe-backtracking-mode -1)
  (let ((backtracker (oref gumshoe-mode backtracker)))
    (remove-hook 'post-command-hook
                 (apply-partially #'gumshoe--post-track backtracker))
    (with-slots (footprints) backtracker
      (gumshoe--hide-footprints footprints))))

(defmacro gumshoe--make-xface (backtrack-name peruse-name filter-name)
  "Make a command interface for the given filter.

BACKTRACK-BACK-NAME and BACKTRACK-FORWARD-NAME are names for the backtracking
commands.
A command will be generated for perusal called PERUSE-NAME.
Results will be filtered using FILTER-NAME function."
  `(progn
     (defun ,peruse-name ()
       (interactive)
       (gumshoe--peruse (ring-elements (oref (oref gumshoe-mode backtracker) backlog))
                        gumshoe-slot-schema
                        #',filter-name))
     (defun ,backtrack-name ()
       (interactive)
       (global-gumshoe-backtracking-mode +1)
       (gumshoe--init-backtracking (oref gumshoe-mode backtracker) #',filter-name)
       (gumshoe-backtrack-back))))
(gumshoe--make-xface gumshoe-backtrack gumshoe-peruse-globally gumshoe--valid-p)
(gumshoe--make-xface gumshoe-buf-backtrack gumshoe-peruse-in-buffer gumshoe--in-current-buffer-p)
(gumshoe--make-xface gumshoe-win-backtrack gumshoe-peruse-in-window gumshoe--in-current-window-p)

(provide 'gumshoe-core)
;;; gumshoe-core.el ends here
