;;; gumshoe.el --- tracks your movements... -*- lexical-binding: t; -*-

;; Copyright (C) 2021 overdr0ne

;; Author: overdr0ne
;; Keywords: tools

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

;; This gumshoe logs any movements outside his minimum follow distance.
;; Use the log to then backtrack to previous locations.

;;; Code:

(require 'eieio)

(defgroup gumshoe nil
  "The gumshoe movement tracker."
  :group 'convenience
  :prefix "gumshoe-")

(defcustom gumshoe-log-len 100
  "Length of gumshoe’s log ring-buffer."
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

(defclass gumshoe ()
  ((log :initform nil
        :documentation "Ring-buffer to remember the previous editing position.")
   (log-index :initform 0
              :documentation "Current index backwards into the log when backtracking.")
   (timer :initform nil
          :documentation "Idle timer used to log position after `gumshoe-idle-time'.")
   (backtrackingp :initform nil
                  :documentation "Flag indicating when gumshoe is backtracking, to pause tracking.")
   (initializedp :initform nil
                 :documentation "Flag indicating whether gumshoe mode has been initialized.")
   (:documentation "Gumshoe keeps track of your movements.") ))

(cl-defmethod initialize ((self gumshoe))
  "Initialize SELF in SCOPE if it is not already."
  (with-slots (log) self
    (setq log (make-ring gumshoe-log-len))
    (ring-insert log (point-marker)))
  self)

(defun gumshoe--line-number-at (pos)
  "Return column number at POS."
  (save-excursion
    (goto-char pos)
    (current-column)))

(defun gumshoe--distance-to (marker)
  "Return the Euclidean distance between point and MARKER."
  (let* ((pos (marker-position marker))
         (line (line-number-at-pos pos))
         (dline (abs (- line
                        (line-number-at-pos (point)))))
         (column (gumshoe--line-number-at pos))
         (dcolumn (abs (- column
                          (current-column))))
         (dcolumn-scaled (/ dcolumn gumshoe-horizontal-scale)))
    (sqrt (+ (math-pow dline 2) (math-pow dcolumn-scaled 2)))))

(defun gumshoe--end-of-leash-p (last-mark)
  "Check if LAST-MARK is outside gumshoe’s boundary."
  (> (gumshoe--distance-to last-mark)
     gumshoe-follow-distance))

(cl-defmethod track ((self gumshoe))
  "Log the current position if necessary."
  (with-slots (backtrackingp log log-index) self
    (unless (or backtrackingp (minibufferp))
      (setf log-index 0)
      (let ((last-mark (ring-ref log 0)))
        (when (or (not (eq (current-buffer)
                           (marker-buffer last-mark)))
                  (gumshoe--end-of-leash-p last-mark))
          (ring-insert log (point-marker)))))
    (setf backtrackingp nil)))

(defun gumshoe--ring-clean (ring)
  "Cleanup markers from RING without a buffer."
  (let ((i 0))
    (while (< i (ring-length ring))
      (let ((marker (ring-ref ring i)))
        (if (marker-buffer marker)
            (setq i (1+ i))
          (ring-remove ring i))))))

(defun gumshoe--ring-reset (ring)
  (let ((i 0))
    (while (< i (ring-length ring))
      (ring-remove ring i))))

(defun gumshoe--jump-to-marker (marker)
  (let ((buf  (marker-buffer marker)))
    (when buf
      (pop-to-buffer buf)
      (goto-char marker))))

(cl-defmethod backtrack-back ((self gumshoe))
  "Jump backward one position in the `gumshoe--log'."
  (with-slots (backtrackingp log log-index) self
    (setf backtrackingp t)
    (unless (ring-empty-p log)
      (setf log-index (1+ log-index))
      (gumshoe--jump-to-marker (ring-ref log log-index)))))

(cl-defmethod backtrack-forward ((self gumshoe))
  "Jump forward one position in the `gumshoe--log'."
  (with-slots (backtrackingp log log-index) self
    (setf backtrackingp t)
    (unless (or (ring-empty-p log)
                (eq log-index 0))
      (setf log-index (1- log-index))
      (gumshoe--jump-to-marker (ring-ref log log-index)))))

(defun gumshoe--log-current-position (ring)
  "Add current position to the ring as a marker."
  (when (or (ring-empty-p ring)
            (not (equal (point-marker) (ring-ref ring 0))))
    (ring-insert ring (point-marker))))

(defmacro start-timer (gumshoe-var timer-var)
  "Start the idle timer to log GUMSHOE-VAR position by name.

Set TIMER-VAR globally such that it can be cancelled on revert."
  `(setf ,timer-var
         (run-with-idle-timer gumshoe-idle-time t
                              #'(lambda ()
                                  (gumshoe--log-current-position (oref ,gumshoe-var log))))))

(defmacro gumshoe--add-hooks (gumshoe-var)
  "Add gumshoe hooks using the GUMSHOE-VAR by name.

Reference the variable by name because the value will change depending on context."
  `(progn
     (add-hook 'pre-command-hook #'(lambda () (track ,gumshoe-var)))
     ;; garbage collect dangling markers for killed buffer
     (add-hook 'kill-buffer-hook #'(lambda () (gumshoe--ring-clean (oref ,gumshoe-var log))))))

(defmacro gumshoe--make-commands (gumshoe-var backtrack-back-name backtrack-forward-name)
  "Make the command interface for GUMSHOE-VAR by name.

BACKTRACK-BACK-NAME and BACKTRACK-FORWARD-NAME are names for the backtracking commands."
  `(progn
     (defun ,backtrack-back-name () (interactive) (backtrack-back ,gumshoe-var))
     (defun ,backtrack-forward-name () (interactive) (backtrack-forward ,gumshoe-var))))

(defmacro gumshoe--mode-init (gumshoe-var timer-var backtrack-back-name backtrack-forward-name)
  "Initialize gumshoe mode for GUMSHOE-VAR.

Set BACKTRACK-BACK-NAME and BACKTRACK-FORWARD-NAME commands."
  `(progn
     (gumshoe--add-hooks ,gumshoe-var)
     (start-timer ,gumshoe-var ,timer-var)
     (gumshoe--make-commands ,gumshoe-var ,backtrack-back-name ,backtrack-forward-name)))

(defmacro gumshoe--revert (gumshoe-var timer-var)
  "Shutdown Gumshoe mode if it is not already."
  `(with-slots (log) ,gumshoe-var
     (remove-hook 'pre-command-hook #'(lambda () (track ,gumshoe-var)))
     (remove-hook 'kill-buffer-hook #'(lambda () (gumshoe--ring-clean log)))
     (cancel-timer ,timer-var)))

(defvar gumshoe--global nil
  "A class of gumshoe with global scope.")
(defvar gumshoe--global-timer nil
  "Global idle timer that logs position for `gumshoe--global’ after `gumshoe-idle-time'.")
(defun global-gumshoe-mode-enable ()
  (setq gumshoe--global (initialize (gumshoe)))
  (gumshoe--mode-init gumshoe--global
                      gumshoe--global-timer
                      gumshoe-backtrack-back
                      gumshoe-backtrack-forward))
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
      (global-gumshoe-mode-enable)
    (gumshoe--revert gumshoe--global gumshoe--global-timer)))

(defvar gumshoe--persp nil
  "A class of gumshoe with perspective scope.")
(defvar gumshoe--persp-timer nil
  "Global idle timer that logs position for `gumshoe--persp’ after`gumshoe-idle-time'.")
(defun global-gumshoe-persp-mode-enable ()
  (require 'perspective)
  (setq gumshoe--persp (initialize (gumshoe)))
  (persp-make-variable-persp-local 'gumshoe--persp)
  ;; Each new perspective must store its own gumshoe.
  ;; So global-gumshoe-persp-mode must be enabled at init.
  ;; Just keep the gumshoe around after disable rather than
  ;; trying to purge every reference. That’s probably what
  ;; the user wants anyway, to keep any old marks.
  (add-hook 'persp-created-hook
            #'(lambda ()
                (setf gumshoe--persp (initialize (gumshoe)))))
  (gumshoe--mode-init gumshoe--persp
                      gumshoe--persp-timer
                      gumshoe-persp-backtrack-back
                      gumshoe-persp-backtrack-forward))
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
      (global-gumshoe-persp-mode-enable)
    (gumshoe--revert gumshoe--persp gumshoe--persp-timer)))

(provide 'gumshoe)
;;; gumshoe.el ends here
