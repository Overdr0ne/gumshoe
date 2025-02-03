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
(require 'context)

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
(defcustom gumshoe-idle-time 60
  "Log context after this idle time."
  :type 'integer)
(defcustom gumshoe-footprint-radius 1
  "This is used to calculate what nearby footprints should be covered."
  :type 'integer)
(defcustom gumshoe-show-footprints-p t
  "Display footprint overlays when backtracking?"
  :type 'boolean)
(defcustom gumshoe-footprint-strategy 'cover-old
  "Strategy for creating a footprint."
  :type '(radio (const :tag "Delete overlapping footprints" delete-overlapping)
                (const :tag "Cover overlapping footprints" cover-old)
                (const :tag "Show all footprints" nil)))

(defcustom gumshoe-cover-old-footprints-p t
  "Initially cover any old footprints when backtracking.

The old footprints are still there, but won’t be revealed until you reach them.
Set to nil if you would like all footprints displayed at once."
  :type 'boolean)
(defcustom gumshoe-entry-type 'context
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
  "Return non-nil if OVERLAY is a context."
  (let ((entry (overlay-get overlay 'container)))
    (if entry
        (object-of-class-p entry 'context)
      nil)))

(defun gumshoe--footprints-at (position)
  (seq-filter 'gumshoe--overlay-is-footprint-p (overlays-in (- position gumshoe-footprint-radius) (+ position gumshoe-footprint-radius))))

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

(defcustom gumshoe-backlog-type 'ring
  "The data structure holding the backlog."
  :type '(radio (const :tag "The backlog is organized into a ring buffer." ring)
                (const :tag "The backlog is organized into a tree of timelines." tree)))

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
    (context--jump (cadr (assoc candidate candidates)))))

;; tracking
(cl-defmethod gumshoe--end-of-leash-p ((last-entry context))
  "Check if LAST-ENTRY is outside gumshoe’s boundary."
  (> (context--distance-to last-entry)
     gumshoe-follow-distance))

;;; footprints
(cl-defmethod gumshoe--mark-footprint ((self context) id face)
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
(if (eq gumshoe-backlog-type 'tree)
    (require 'gumshoe-tree)
  (require 'gumshoe-ring))

(defclass gumshoe--backtracker ()
  ((backlog :initform nil
            :initarg :backlog
            :documentation "Ring-buffer to remember the previous editing position.")
   (filter :initform #'context--valid-p
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
    (message "timeline %s" (gumshoe--construct-timeline backlog))
    (setf filtered
          (if filter
              (seq-filter filter (gumshoe--construct-timeline backlog))
            (gumshoe--construct-timeline backlog)))
    (when gumshoe-show-footprints-p
      (gumshoe--mark-footprints filtered))
    (setf index -1)))
(cl-defmethod gumshoe--backtrack ((self gumshoe--backtracker) incrementer)
  "Backtrack using INCREMENTER in SELF.

Only including results satisfying FILTER.
INCREMENTER increments the index in SELF."
  (with-slots (index filter filtered msg backlog current) self
    (let ((prev-index index))
      (gumshoe--increment-index self incrementer)
      (when gumshoe-show-footprints-p
        (gumshoe--hl-current-footprint filtered prev-index index))
      (if (not filtered)
          (setf msg "I haven’t recorded any entries here yet...")
        (context--jump (nth index filtered)))
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
    (setf backtracker (gumshoe--backtracker :backlog (gumshoe--backlog-init gumshoe-log-len)))
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
         (entries (gumshoe--construct-timeline backlog)))
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
       (gumshoe--peruse (gumshoe--get-timeline (oref (oref gumshoe-mode backtracker) backlog))
                        gumshoe-slot-schema
                        #',filter-name))
     (defun ,backtrack-name ()
       (interactive)
       (global-gumshoe-backtracking-mode +1)
       (gumshoe--init-backtracking (oref gumshoe-mode backtracker) #',filter-name)
       (gumshoe--backtrack (oref gumshoe-mode backtracker) #'+))))
(gumshoe--make-xface gumshoe-backtrack gumshoe-peruse-globally context--valid-p)
(gumshoe--make-xface gumshoe-buf-backtrack gumshoe-peruse-in-buffer context--in-current-buffer-p)
(gumshoe--make-xface gumshoe-win-backtrack gumshoe-peruse-in-window context--in-current-window-p)

(make-obsolete 'gumshoe-backtrack-back 'gumshoe-buf-backtrack "3.0")
(make-obsolete 'gumshoe-backtrack-forward 'gumshoe-buf-backtrack "3.0")
(make-obsolete 'gumshoe-buf-backtrack-back 'gumshoe-buf-backtrack "3.0")
(make-obsolete 'gumshoe-buf-backtrack-forward 'gumshoe-buf-backtrack "3.0")
(make-obsolete 'gumshoe-win-backtrack-back 'gumshoe-win-backtrack "3.0")
(make-obsolete 'gumshoe-win-backtrack-forward 'gumshoe-win-backtrack "3.0")

(provide 'gumshoe-core)
;;; gumshoe-core.el ends here
