;;; gumshoe.el --- Scoped spatial and temporal POINT movement tracking -*- lexical-binding: t; -*-

;; Copyright (C) 2021 overdr0ne

;; Author: overdr0ne
;; Version: 1.0
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
(require 'ring)

(defgroup gumshoe nil
  "The gumshoe movement tracker."
  :group 'convenience
  :prefix "gumshoe-")

(defcustom gumshoe-log-len 100
  "Length of gumshoe--backlog’s ring-buffer."
  :type 'integer)
(defcustom gumshoe-follow-distance 15
  "Gumshoe logs movements beyond this Euclidean distance from previous entry."
  :type 'integer)
(defcustom gumshoe-horizontal-scale 3
  "Horizontal follow distances are divided by this factor."
  :type 'integer)

(defcustom gumshoe-idle-time 60
  "Gumshoe automatically logs your position if you’ve been idle at POINT for this amount of time."
  :type 'integer)

(defcustom gumshoe-display-buffer-action '((display-buffer-reuse-window display-buffer-same-window))
  "`display-buffer-action’ to use when jumping through the backlog.

See `display-buffer' for more information"
  :type 'list)

(defclass gumshoe--backlog ()
  ((log :initform (make-ring gumshoe-log-len)
        :documentation "Ring-buffer to remember the previous editing position.")
   (index :initform 0
          :documentation "Current index backwards into the log when backtracking.")
   (backtrackingp :initform nil
                  :documentation "Flag indicating when a gumshoe is using the log to backtrack.")
   (:documentation "Gumshoe’s backlog for tracking POINT positions.") ))

(defclass gumshoe--entry ()
  ((filename :initform (buffer-file-name)
             :documentation "Ring-buffer to remember the previous editing position.")
   (buffer :initform (current-buffer)
           :documentation "Ring-buffer to remember the previous editing position.")
   (position :initform (point-marker)
             :documentation "Current index backwards into the log when backtracking.")
   (line :initform (buffer-substring (line-beginning-position) (line-end-position))
         :documentation "Flag indicating when a gumshoe is using the log to backtrack.")
   (time :initform (current-time-string)
         :documentation "Flag indicating when a gumshoe is using the log to backtrack.")
   (mode :initform (symbol-name major-mode)
         :documentation "Flag indicating when a gumshoe is using the log to backtrack.")
   (perspective :initform (persp-current-name)
                :documentation "Flag indicating when a gumshoe is using the log to backtrack.")
   (:documentation "Gumshoe’s backlog for tracking POINT positions.")))

(cl-defmethod gumshoe--jump ((self gumshoe--entry))
  (with-slots (buffer position) self
    (switch-to-buffer buffer)
    (goto-char position)))

(defclass gumshoe--pretty-entry ()
  ((filename :initform "" :initarg :filename
             :documentation "Ring-buffer to remember the previous editing position.")
   (buffer :initform "" :initarg :buffer
           :documentation "Ring-buffer to remember the previous editing position.")
   (position :initform "" :initarg :position
             :documentation "Current index backwards into the log when backtracking.")
   (line :initform "" :initarg :line
         :documentation "Flag indicating when a gumshoe is using the log to backtrack.")
   (time :initform "" :initarg :time
         :documentation "Flag indicating when a gumshoe is using the log to backtrack.")
   (mode :initform "" :initarg :mode
         :documentation "Flag indicating when a gumshoe is using the log to backtrack.")
   (perspective :initform "" :initarg :perspective
                :documentation "Flag indicating when a gumshoe is using the log to backtrack.")
   (:documentation "Gumshoe’s backlog for tracking POINT positions.")))

(cl-defmethod gumshoe--prettify ((src gumshoe--entry))
  (with-slots (filename buffer position line time mode perspective) src
    (gumshoe--pretty-entry
     :filename filename
     :buffer (buffer-name buffer)
     :position (marker-position position)
     :line line
     :time time
     :mode mode
     :perspective perspective)))

(defun gumshoe--format-record (rec format-string slot-spec)
  (let* ((slot-vals (mapcar #'(lambda (slot) (slot-value rec slot)) slot-spec)))
    (apply #'format format-string slot-vals)))

(defun gumshoe--format-records (rec-list format-string slot-spec)
  (mapcar #'(lambda (rec) (gumshoe--format-record rec format-string slot-spec)) rec-list))

(defcustom gumshoe--field-schema '(perspective buffer position line)
  "Entry slots to view when browsing the log."
  :type 'list)

(defun gumshoe--peruse (entries slot-spec &optional entry-filter)
  "Peruse the BACKLOG."
  (let* ((format-schema (string-join (mapcar #'symbol-name slot-spec) ":"))
         (prompt (concat "(" format-schema "): "))
         (format-components (mapcar #'(lambda (_) "%s") slot-spec))
         (format-string (string-join format-components ":"))
         (filtered-entries (if entry-filter
                               (seq-filter entry-filter entries)
                             entries))
         (prettified-entries (mapcar #'gumshoe--prettify filtered-entries))
         (entry-strings (gumshoe--format-records prettified-entries format-string slot-spec))
         (candidates (cl-mapcar #'list entry-strings filtered-entries))
         (candidate (completing-read prompt candidates)))
    (gumshoe--jump (cadr (assoc candidate candidates)))))

(defun gumshoe-peruse ()
  (interactive)
  (gumshoe--peruse (ring-elements (oref gumshoe--global-backlog log))
                   gumshoe--field-schema))

(defmethod gumshoe--in-current-persp-p ((entry gumshoe--entry))
  "Check if entry in the current perspective."
  (equal (oref entry perspective) (persp-current-name)))
(defun gumshoe-persp-peruse ()
  (interactive)
  (gumshoe--peruse (ring-elements (oref gumshoe--global-backlog log))
                   gumshoe--field-schema
                   #'gumshoe--in-current-persp-p))

(defmethod gumshoe--in-current-buffer-p ((entry gumshoe--entry))
  "Check if entry in the current perspective."
  (equal (oref entry buffer) (current-buffer)))
(defun gumshoe-buffer-peruse ()
  (interactive)
  (gumshoe--peruse (ring-elements (oref gumshoe--global-backlog log))
                   gumshoe--field-schema
                   #'gumshoe--in-current-buffer-p))

(defun gumshoe--line-number-at (pos)
  "Return column number at POS."
  (save-excursion
    (goto-char pos)
    (current-column)))

(cl-defmethod gumshoe--distance-to ((entry gumshoe--entry))
  "Return the Euclidean distance between point and ENTRY."
  (let* ((pos (oref entry position))
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

(defun gumshoe--ring-current-index (ring)
  (nth 1 ring))
(defun gumshoe--ring-next-index (ring)
  (ring-plus1 (gumshoe--ring-current-index ring)
              (ring-size ring)))
(defun gumshoe--gen-id (ring)
  (number-to-string (gumshoe--ring-next-index ring)))
(cl-defmethod gumshoe--equal ((self gumshoe--entry) (other gumshoe--entry))
  "Is SELF equal to OTHER."
  (and (equal (oref self perspective) (oref other perspective))
       (equal (oref self buffer) (oref other buffer))
       (equal (oref self position) (oref other position))))

(defun gumshoe--log-current-position (ring)
  "Log new backlog entry to RING."
  (let ((new-entry (gumshoe--entry)))
    (when (or (ring-empty-p ring)
              (not (gumshoe--equal new-entry (ring-ref ring 0))))
      ;; (push (gumshoe--gen-id ring) new-entry)
      (ring-insert ring new-entry))))

(cl-defmethod gumshoe--changed-file-p ((last-entry gumshoe--entry))
  (not (equal (current-buffer)
              (oref last-entry buffer))))

(cl-defmethod gumshoe--track ((self gumshoe--backlog))
  "Log the current position to SELF if necessary."
  (unless self (error "Gumshoe argument self is nil"))
  (with-slots (backtrackingp log index) self
    (unless (or backtrackingp (minibufferp))
      (setf index 0)
      (if (ring-empty-p log)
          (gumshoe--log-current-position log)
        (let ((last-entry (ring-ref log 0)))
          (when (or (gumshoe--changed-file-p last-entry)
                    (gumshoe--end-of-leash-p last-entry))
            (gumshoe--log-current-position log)))))
    (setf backtrackingp nil)))

(defun gumshoe--ring-reset (ring)
  "Reset all entries in RING to nil."
  (let ((i 0))
    (while (< i (ring-length ring))
      (ring-remove ring i))))

(cl-defmethod gumshoe--backtrack-back ((self gumshoe--backlog))
  "Jump backward one position in SELF."
  (with-slots (backtrackingp log index) self
    (setf backtrackingp t)
    (unless (ring-empty-p log)
      (when (gumshoe--equal (ring-ref log index) (gumshoe--entry))
        (cl-incf index))
      (gumshoe--jump (ring-ref log index))
      (cl-incf index))))

(cl-defmethod gumshoe--backtrack-forward ((self gumshoe--backlog))
  "Jump forward one position in the SELF backlog."
  (with-slots (backtrackingp log index) self
    (setf backtrackingp t)
    (unless (or (ring-empty-p log)
                (eq index 0))
      (cl-decf index)
      (when (gumshoe--equal (ring-ref log index) (gumshoe--entry))
        (cl-decf index))
      (gumshoe--jump (ring-ref log index)))))

(defun gumshoe--timer-callback (backlog-var)
  "Called by timer to log current position in BACKLOG-VAR."
  (unless (symbol-value backlog-var)
    (set backlog-var (gumshoe--backlog)))
  (unless (oref (symbol-value backlog-var) backtrackingp)
    (gumshoe--log-current-position (oref (symbol-value backlog-var) log))))

(defun gumshoe--start-timer (backlog-var timer-var)
  "Start TIMER-VAR timer to log BACKLOG-VAR position by name.

Set TIMER-VAR globally such that it can be cancelled on revert."
  (set timer-var
       (run-with-idle-timer gumshoe-idle-time t
                            (apply-partially #'gumshoe--timer-callback backlog-var))))

(defun gumshoe--pre-command-callback (backlog-var)
  "Triggers tracking for BACKLOG-VAR, initializing it if necessary."
  (unless (symbol-value backlog-var)
    (set backlog-var (gumshoe--backlog)))
  (gumshoe--track (symbol-value backlog-var)))

(defun gumshoe--add-hooks (backlog-var)
  "Add hooks using the BACKLOG-VAR by name.

Reference the variable by name because the value will change depending on context."
  (add-hook 'pre-command-hook
            (apply-partially #'gumshoe--pre-command-callback backlog-var)))

(defmacro gumshoe--make-commands (backlog-var backtrack-back-name backtrack-forward-name)
  "Make the command interface for BACKLOG-VAR by name.

BACKTRACK-BACK-NAME and BACKTRACK-FORWARD-NAME are names for the backtracking commands."
  `(progn
     (defun ,backtrack-back-name () (interactive) (gumshoe--backtrack-back ,backlog-var))
     (defun ,backtrack-forward-name () (interactive) (gumshoe--backtrack-forward ,backlog-var))))

(defmacro gumshoe--mode-init (backlog-var timer-var backtrack-back-name backtrack-forward-name)
  "Initialize gumshoe mode for BACKLOG-VAR.

Set timer for TIMER-VAR.

Set BACKTRACK-BACK-NAME and BACKTRACK-FORWARD-NAME commands."
  `(progn
     (gumshoe--add-hooks ',backlog-var)
     (gumshoe--start-timer ',backlog-var ',timer-var)
     (gumshoe--make-commands ,backlog-var ,backtrack-back-name ,backtrack-forward-name)))

(defmacro gumshoe--revert (backlog-var timer-var)
  "Revert BACKLOG-VAR and TIMER-VAR."
  `(progn
     (remove-hook 'pre-command-hook
                  (apply-partially #'gumshoe--pre-command-callback ,backlog-var))
     (cancel-timer ,timer-var)))

(defvar gumshoe--global-backlog nil
  "A class of symbol `gumshoe--backlog' with global scope.")
(defvar gumshoe--global-timer nil
  "Global idle timer that logs position for `gumshoe--global-backlog’ after `gumshoe-idle-time'.")
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
      (gumshoe--mode-init gumshoe--global-backlog
                          gumshoe--global-timer
                          gumshoe-backtrack-back
                          gumshoe-backtrack-forward)
    (gumshoe--revert gumshoe--global-backlog gumshoe--global-timer)))

(defvar gumshoe--persp-backlog nil
  "A class of symbol `gumshoe--backlog' with perspective scope.")
(defvar gumshoe--persp-timer nil
  "Global idle timer that logs position for `gumshoe--persp-backlog’ after`gumshoe-idle-time'.")
;; suppress warning that this function isn’t defined because perspective
;; is required
(declare-function persp-make-variable-persp-local "perspective.el")
(defun gumshoe--persp-created-callback ()
  "Create a new backlog for every new persp.

This wouldn’t be necessary if persp initialized local variables to
nil, because there is no other way to know generically whether that
variable actually belongs to that perspective generically."
  (setq gumshoe--persp-backlog (gumshoe--backlog)))
(defun gumshoe--global-gumshoe-persp-mode-enable ()
  "Enable the symbol `global-gumshoe-persp-mode’."
  (require 'perspective)
  (persp-make-variable-persp-local 'gumshoe--persp-backlog)
  (add-hook 'persp-created-hook
            #'gumshoe--persp-created-callback)
  (gumshoe--mode-init gumshoe--persp-backlog
                      gumshoe--persp-timer
                      gumshoe-persp-backtrack-back
                      gumshoe-persp-backtrack-forward))
;;;###autoload
(define-minor-mode global-gumshoe-persp-mode
  "Toggle persp Gumshoe minor mode.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When enabled, Gumshoe logs point movements when they exceed the

`gumshoe-follow-distance', or when the user is idle longer than
`gumshoe-idle-time'."
  :init-value nil
  :lighter " Gumshoe:persp"
  :group 'gumshoe
  :global t
  (if global-gumshoe-persp-mode
      (gumshoe--global-gumshoe-persp-mode-enable)
    (gumshoe--revert gumshoe--persp-backlog gumshoe--persp-timer)))

(defvar-local gumshoe--buf-backlog nil
  "A class of gumshoe--backlog with buffer scope.")
(defvar gumshoe--buf-timer nil
  "Global idle timer that logs position for `gumshoe--buf-backlog’ after `gumshoe-idle-time'.")
;;;###autoload
(define-minor-mode global-gumshoe-buf-mode
  "Toggle global Gumshoe minor mode.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When enabled, Gumshoe logs point movements when they exceed the
`gumshoe-follow-distance', or when the user is idle longer than
`gumshoe-idle-time'."
  :init-value nil
  :lighter " Gumshoe:buf"
  :group 'gumshoe
  :global t
  (if global-gumshoe-buf-mode
      (gumshoe--mode-init gumshoe--buf-backlog
                          gumshoe--buf-timer
                          gumshoe-buf-backtrack-back
                          gumshoe-buf-backtrack-forward)
    (gumshoe--revert gumshoe--buf-backlog gumshoe--buf-timer)))

(provide 'gumshoe)
;;; gumshoe.el ends here
