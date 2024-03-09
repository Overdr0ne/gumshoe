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
  "Log context after this idle time."
  :type 'integer)
(defcustom gumshoe-footprint-radius 1
  "This is used to calculate what nearby footprints should be covered."
  :type 'integer)
(defcustom gumshoe-show-footprints-p t
  "Display footprint overlays when backtracking?"
  :type 'boolean)
(defcustom gumshoe-footprint-strategy 'delete-overlapping
  "Strategy for creating a footprint."
  :type '(radio (const :tag "Delete overlapping footprints" delete-overlapping)
                (const :tag "Cover overlapping footprints" cover-old)
                (const :tag "Show all footprints" nil)))

(defcustom gumshoe-cover-old-footprints-p t
  "Initially cover any old footprints when backtracking.

The old footprints are still there, but won’t be revealed until you reach them.
Set to nil if you would like all footprints displayed at once."
  :type 'boolean)
(defcustom gumshoe-entry-type 'gumshoe--entry
  "Type of entry Gumshoe should use in the backlog."
  :type 'symbol)

(defcustom gumshoe-slot-schema '(time buffer position line)
  "Entry slot order for perusing the backlog."
  :type 'list)

(defcustom gumshoe-peruse-separator "|"
  "Separator to be used between gumshoe slots."
  :type 'string)

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
     :box (:line-width -3 :style pressed-button)
     :weight bold))
  "Face for footprint overlays.")
(defface gumshoe--current-footprint-face
  '((t :inherit match
       :box (:line-width -3 :style released-button)
       :weight bold))
  "Face for footprint overlays.")

(defcustom gumshoe-ignore-predicates '(minibufferp
                                       gumshoe--ignore-mode-p)
  "A list of predicates that will block gumshoe from logging when true."
  :type '(repeat function))

(defcustom gumshoe-ignored-major-modes
  '(fundamental-mode
    exwm-mode
    helm-major-mode)
  "Don't remember places in buffers in these major modes."
  :type '(repeat symbol))

(defcustom gumshoe-ignored-minor-modes
  '(ctrlf-mode
    isearch-mode
    global-gumshoe-backtracking-mode)
  "Don't remember places in buffers in these minor modes."
  :type '(repeat symbol))

(defun gumshoe--overlay-is-footprint-p (overlay)
  "Return non-nil if OVERLAY is a gumshoe--entry."
  (if-let ((entry (overlay-get overlay 'container)))
      (object-of-class-p entry 'gumshoe--entry)
    nil))

(defun gumshoe--footprints-at (position)
  (seq-filter 'gumshoe--overlay-is-footprint-p (overlays-in (- position gumshoe-footprint-radius) (+ position gumshoe-footprint-radius))))

(defun gumshoe--remove-footprint-entry (ring footprint)
  (ring-remove ring
               (ring-member ring
                            (overlay-get footprint 'container)))
  (delete-overlay footprint))

(defun gumshoe--remove-footprint-entries-at (position ring)
  (mapc (apply-partially #'gumshoe--remove-footprint-entry ring)
        (gumshoe--footprints-at position)))

(defun gumshoe--cover-old-footprints-at (position)
  (let* ((footprints (gumshoe--footprints-at position)))
    (dolist (footprint-i footprints)
      (overlay-put footprint-i 'after-string ""))))

(defun gumshoe--ignore-mode-p ()
  "Return non-nil if current buffer's major mode is ignored."
  (or (member major-mode gumshoe-ignored-major-modes)
      (cl-some (lambda (mode)
                 (and (boundp mode)
                      (eval mode)))
               gumshoe-ignored-minor-modes)))

(defcustom gumshoe-auto-cancel-backtracking-p t
  "Automatically cancel backtracking when non-backtracking commands are entered during backtracking."
  :type 'boolean)

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
           :documentation "Window of this entry.")
   (footprint-overlay :initform nil
                      :documentation "Footprint overlay.
This must be set manually because overlays cannot be garbage collected.")   )
  "Entry class for Gumshoe’s backlog.")

(cl-defmethod gumshoe--valid-p ((self gumshoe--entry))
  "Return t if SELF is valid."
  (not (cl-check-type self gumshoe--entry)))

(cl-defmethod gumshoe--jump ((self gumshoe--entry))
  "Jump Point to buffer and position in SELF."
  (let ((position (overlay-start (oref self footprint-overlay))))
    (with-slots (buffer) self
     (if gumshoe-prefer-same-window
         (pop-to-buffer-same-window buffer)
       (pop-to-buffer buffer))
     (goto-char position))))

(cl-defmethod gumshoe--dead-p ((self gumshoe--entry))
  "Check if SELF is dead."
  (let* ((buffer (oref self buffer))
         (pos (overlay-start (oref self footprint-overlay))))
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

(defun gumshoe--delete (ring index)
  "Delete entry at INDEX from RING."
  (let ((entry (ring-ref ring index)))
    (delete-overlay (slot-value entry 'footprint-overlay)))
  (ring-remove ring index))

(cl-defmethod gumshoe--clean-recent (ring)
  "Cleanup recent dead entries from RING."
  (unless (ring-empty-p ring)
    (let ((i 0)
          (continuep t))
      (while (and continuep
                  (< i (ring-length ring)))
        (let ((entry (ring-ref ring i)))
          (if (gumshoe--dead-p entry)
              (gumshoe--delete ring i)
            (setq continuep nil)))))))

(cl-defmethod gumshoe--clean (ring)
  "Cleanup dead entries from RING."
  (unless (ring-empty-p ring)
    (let ((i 0))
      (while (< i (ring-length ring))
        (let ((entry (ring-ref ring i)))
          (if (gumshoe--dead-p entry)
              (gumshoe--delete ring i)
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
         (format-schema (string-join (mapcar #'symbol-name slot-spec) (propertize gumshoe-peruse-separator 'face 'gumshoe--peruse-separator-face)))
         (prompt (concat (propertize "(" 'face 'gumshoe--peruse-separator-face)
			                   format-schema
			                   (propertize ")" 'face 'gumshoe--peruse-separator-face) ": "))
         (format-components (mapcar #'(lambda (_) "%s") slot-spec))
	       (separator (propertize gumshoe-peruse-separator 'face 'gumshoe--peruse-separator-face))
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
  (let* ((pos (overlay-start (oref self footprint-overlay)))
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
   (oref self footprint-overlay)
   (oref other footprint-overlay)
   (equal (overlay-start (oref self footprint-overlay))
          (overlay-start (oref other footprint-overlay)))))
(cl-defmethod gumshoe--add-entry (ring (entry gumshoe--entry))
  "Add entry to the RING."
  (ring-insert ring entry)
  (when (eq gumshoe-footprint-strategy 'delete-overlapping)
    (gumshoe--remove-footprint-entries-at (point) ring))
  (let ((overlay (make-overlay (point) (point))))
    (overlay-put overlay 'container entry)
    (oset entry footprint-overlay overlay)))
(defun gumshoe--log-if-necessary (ring &optional alarmp)
  "Check current position and log in RING if significant.

Log automatically if ALARMP is t."
  (unless (cl-some #'funcall gumshoe-ignore-predicates)
    (gumshoe--clean-recent ring)
    (let ((new-entry (funcall gumshoe-entry-type)))
      (when (or (ring-empty-p ring)
                (let ((latest-entry (ring-ref ring 0)))
                  (and (not (gumshoe--equal new-entry latest-entry))
                       (or alarmp
                           (not (gumshoe--in-current-buffer-p latest-entry))
                           (gumshoe--end-of-leash-p latest-entry)))))
        (gumshoe--add-entry ring new-entry)))))

;;; footprints
(cl-defmethod gumshoe--mark-footprint ((self gumshoe--entry) id face)
  "Add footprint overlay to SELF, labeled with ID, using FACE."
  (with-slots (buffer footprint-overlay) self
    (message (buffer-name buffer))
    (let* ((label (int-to-string id)))
      (when (and buffer (> (buffer-size buffer) 1))
        (put-text-property 0 (length label) 'face face label)
        (overlay-put footprint-overlay 'after-string label)))))
(defun gumshoe--replace-footprint (entries index face)
  "Add footprint overlay at footprint INDEX in FOOTPRINTS, using FACE."
  (let* ((label (int-to-string (- (length entries) index)))
         (entry (nth index entries))
         (footprint-overlay (slot-value entry 'footprint-overlay))
         (position (overlay-start footprint-overlay)))
    (when gumshoe-cover-old-footprints-p (gumshoe--cover-old-footprints-at position))
    (put-text-property 0 (length label) 'face face label)
    (overlay-put footprint-overlay 'after-string label)))
(defun gumshoe--hl-current-footprint (entries prev-index cur-index)
  "Replace PREV-INDEX with CUR-INDEX as current footprint in FOOTPRINTS."
  (when entries
    (gumshoe--replace-footprint entries prev-index 'gumshoe--footprint-face)
    (gumshoe--replace-footprint entries cur-index 'gumshoe--current-footprint-face)))
(defun gumshoe--mark-footprints (entries)
  "Display footprints for all ENTRIES."
  (let ((i 1))
    (dolist (entry (reverse entries))
      (let ((position (overlay-start (oref entry footprint-overlay))))
        (when (eq gumshoe-footprint-strategy 'cover-old)
          (gumshoe--cover-old-footprints-at position))
        (gumshoe--mark-footprint entry i 'gumshoe--footprint-face)
        (cl-incf i)))))
(defun gumshoe--hide-footprints (entries)
  "Hide footprints in FOOTPRINTS."
  (dolist (entry entries)
    (with-slots (footprint-overlay) entry
      (overlay-put footprint-overlay 'after-string ""))))

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
      (setf msg (format "Gumshoe: entry #%i: %i"
                        (- (length filtered) index)
                        (length filtered)))))))
(cl-defmethod gumshoe--init-backtracking ((self gumshoe--backtracker) _filter)
  "FILTER SELF, and reset slots to start backtracking."
  (with-slots (backlog filter filtered msg index) self
    (setf filter _filter)
    (gumshoe--clean backlog)
    (setf filtered (if filter
                       (seq-filter filter (ring-elements backlog))
                     (ring-elements backlog)))
    (when gumshoe-show-footprints-p
      (gumshoe--mark-footprints filtered))
    (setf index -1)))
(cl-defmethod gumshoe--backtrack ((self gumshoe--backtracker) incrementer)
  "Backtrack using INCREMENTER in SELF.

Only including results satisfying FILTER.
INCREMENTER increments the index in SELF."
  (with-slots (index filter filtered msg backlog) self
    (let ((prev-index index)
          (entries (ring-elements backlog)))
      (gumshoe--increment-index self incrementer)
      (when gumshoe-show-footprints-p
        (gumshoe--hl-current-footprint filtered prev-index index))
      (if (not filtered)
          (setf msg "I haven’t recorded any entries here yet...")
        (gumshoe--jump (nth index filtered)))
      (when msg (message msg)))))
(defun global-gumshoe-backtracking-mode-back ()
  "Backtrack back in the Gumshoe backlog."
  (interactive)
  (gumshoe--backtrack (oref gumshoe-mode backtracker) #'+))

(defun global-gumshoe-backtracking-mode-forward ()
  "Backtrack back in the Gumshoe backlog."
  (interactive)
  (gumshoe--backtrack (oref gumshoe-mode backtracker) #'-))

(defun gumshoe--backtracking-p ()
  "Was the last command a backtracking command?"
  (let ((backtracking-commands
         '(global-gumshoe-backtracking-mode-back
           global-gumshoe-backtracking-mode-forward
           gumshoe-backtrack
           gumshoe-buf-backtrack
           gumshoe-win-backtrack)))
    (cl-some (lambda (cmd) (equal this-command cmd))
             backtracking-commands)))

(defun gumshoe--auto-cancel-backtracking ()
  "Automatically cancel last backtracking command if necessary."
  (unless (gumshoe--backtracking-p)
    (gumshoe-backtrack-quit)))

(cl-defmethod gumshoe--timer-callback ((self gumshoe--backtracker))
  "Called by timer to log current position in SELF."
  (with-slots (backlog) self
    (gumshoe--log-if-necessary backlog t)))

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
  (let* ((backtracker (oref gumshoe-mode backtracker))
         (backlog (oref backtracker backlog))
         (entries (ring-elements backlog)))
    (gumshoe--hide-footprints entries)))

(defvar global-gumshoe-backtracking-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap keyboard-quit] 'gumshoe-backtrack-quit)
    (define-key map [remap gumshoe-backtrack-back] 'global-gumshoe-backtracking-mode-back)
    (define-key map [remap gumshoe-backtrack-forward] 'global-gumshoe-backtracking-mode-forward)
    (define-key map [remap gumshoe-buf-backtrack-back] 'global-gumshoe-backtracking-mode-back)
    (define-key map [remap gumshoe-buf-backtrack-forward] 'global-gumshoe-backtracking-mode-forward)
    (define-key map [remap gumshoe-win-backtrack-back] 'global-gumshoe-backtracking-mode-back)
    (define-key map [remap gumshoe-win-backtrack-forward] 'global-gumshoe-backtracking-mode-forward)
    (define-key map [remap backward-paragraph] 'global-gumshoe-backtracking-mode-back)
    (define-key map [remap forward-paragraph] 'global-gumshoe-backtracking-mode-forward)
    map)
  "Transient keymap activated during global-gumshoe-backtracking-mode.")
(define-minor-mode global-gumshoe-backtracking-mode
  "A transient global mode to start Gumshoe backtracking."
  :global t
  :keymap global-gumshoe-backtracking-mode-map
  (if global-gumshoe-backtracking-mode
      (progn
        (when gumshoe-auto-cancel-backtracking-p
          (add-hook 'post-command-hook
                    #'gumshoe--auto-cancel-backtracking))
        (push `(global-gumshoe-backtracking-mode . ,global-gumshoe-backtracking-mode-map)
              minor-mode-map-alist))
    (setf minor-mode-map-alist (assoc-delete-all 'global-gumshoe-backtracking-mode minor-mode-map-alist))
    (when gumshoe-auto-cancel-backtracking-p
      (remove-hook 'post-command-hook
                   #'gumshoe--auto-cancel-backtracking))))

(defmacro gumshoe--make-xface (backtrack-name peruse-name filter-name)
  "Make a command interface for the given filter.

BACKTRACK-NAME is the name of the backtracking command.

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
       (gumshoe--backtrack (oref gumshoe-mode backtracker) #'+))))
(gumshoe--make-xface gumshoe-backtrack gumshoe-peruse-globally gumshoe--valid-p)
(gumshoe--make-xface gumshoe-buf-backtrack gumshoe-peruse-in-buffer gumshoe--in-current-buffer-p)
(gumshoe--make-xface gumshoe-win-backtrack gumshoe-peruse-in-window gumshoe--in-current-window-p)

(make-obsolete 'gumshoe-backtrack-back 'gumshoe-buf-backtrack "3.0")
(make-obsolete 'gumshoe-backtrack-forward 'gumshoe-buf-backtrack "3.0")
(make-obsolete 'gumshoe-buf-backtrack-back 'gumshoe-buf-backtrack "3.0")
(make-obsolete 'gumshoe-buf-backtrack-forward 'gumshoe-buf-backtrack "3.0")
(make-obsolete 'gumshoe-win-backtrack-back 'gumshoe-win-backtrack "3.0")
(make-obsolete 'gumshoe-win-backtrack-forward 'gumshoe-win-backtrack "3.0")

(provide 'gumshoe-core)
;;; gumshoe-core.el ends here
