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

(defgroup gumshoe nil
  "The gumshoe movement tracker."
  :group 'convenience
  :prefix "gumshoe-")

(defcustom gumshoe-log-len 100
  "Length of gumshoe’s log ring-buffer."
  :type 'integer)
(defvar gumshoe--log (make-ring gumshoe-log-len)
  "Ring-buffer to remember the previous editing position.")

(defcustom gumshoe-follow-distance 15
  "Gumshoe logs movements beyond this Euclidean distance from previous entry."
  :type 'integer)
(defcustom gumshoe-horizontal-scale 3
  "Horizontal follow distances are divided by this factor."
  :type 'integer)

(defcustom gumshoe-idle-time 60
  "Gumshoe automatically logs your position if you’ve been idle at POINT for this amount of time."
  :type 'integer)

(defvar gumshoe--backtracking-p nil
  "Flag indicating when gumshoe is backtracking, to pause tracking.")
(defvar gumshoe--log-index 0
  "Current index backwards into the log when backtracking.")

(define-minor-mode global-gumshoe-mode
  "Toggle global Gumshoe minor mode.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When enabled, Gumshoe logs point movements when they exceed the
`gumshoe-follow-distance', or when the user is idle longer than
`gumshoe-idle-time'."
  nil
  " Gumshoe"
  :global t
  (ring-insert gumshoe--log (point-marker))
  (add-hook 'pre-command-hook 'gumshoe--track)
  (add-hook 'kill-buffer-hook #'gumshoe--clean-log)
  (run-with-idle-timer gumshoe-idle-time t #'gumshoe-log-current-position))

(defun gumshoe--line-number-at-pos (pos)
  "Return column number at POINT."
  (save-excursion
    (goto-char pos)
    (current-column)))

(defun gumshoe--distance-to (marker)
  "Return the Euclidean distance between point and MARKER."
  (let* ((pos (marker-position marker))
         (line (line-number-at-pos pos))
         (dline (abs (- line
                        (line-number-at-pos (point)))))
         (column (gumshoe--line-number-at-pos pos))
         (dcolumn (abs (- column
                          (current-column))))
         (dcolumn-scaled (/ dcolumn gumshoe-horizontal-scale)))
    (sqrt (+ (math-pow dline 2) (math-pow dcolumn-scaled 2)))))

(defun gumshoe--end-of-leash-p (last-mark)
  "Check if LAST-MARK is outside gumshoe’s boundary."
  (> (gumshoe--distance-to last-mark)
     gumshoe-follow-distance))

(defun gumshoe--track ()
  "Track the previous editing position in `gumshoe--log'."
  (unless (or gumshoe--backtracking-p (minibufferp))
    (setq gumshoe--log-index 0)
    (let ((last-mark (ring-ref gumshoe--log 0)))
      (when (or (not (eq (current-buffer)
                         (marker-buffer last-mark)))
                (gumshoe--end-of-leash-p last-mark))
        (ring-insert gumshoe--log (point-marker)))))
  (setq gumshoe--backtracking-p nil))

(defun gumshoe--ring-clean (ring)
  "Gumshoe cleanup markers from RING without a buffer."
  (let ((i 0))
    (while (< i (ring-length ring))
      (let ((marker (ring-ref ring i)))
        (if (marker-buffer marker)
            (setq i (1+ i))
          (ring-remove ring i))))))
(defun gumshoe--clean-log ()
  "Cleanup dead markers from gumshoe--log."
  (gumshoe--ring-clean gumshoe--log))

(defun gumshoe--jump-to-marker (marker)
  (let ((buf  (marker-buffer marker)))
    (when buf
      (pop-to-buffer buf)
      (goto-char marker))))

(defun gumshoe-backtrack-back ()
  "Jump backward one position in the `gumshoe--log'."
  (interactive)
  (setq gumshoe--backtracking-p t)
  (unless (ring-empty-p gumshoe--log)
    (setq gumshoe--log-index (1+ gumshoe--log-index))
    (gumshoe--jump-to-marker (ring-ref gumshoe--log gumshoe--log-index))))

(defun gumshoe-backtrack-forward ()
  "Jump forward one position in the `gumshoe--log'."
  (interactive)
  (setq gumshoe--backtracking-p t)
  (unless (or (ring-empty-p gumshoe--log)
              (eq gumshoe--log-index 0))
    (setq gumshoe--log-index (1- gumshoe--log-index))
    (gumshoe--jump-to-marker (ring-ref gumshoe--log gumshoe--log-index))))

(defun gumshoe-log-current-position ()
  "Add current position to the `gumshoe--log’ as a marker."
  (interactive)
  (unless (equal (point-marker) (ring-ref gumshoe--log 0))
    (ring-insert gumshoe--log (point-marker))))

(with-eval-after-load 'consult
  (require 'consult)

  (defun consult-gumshoe ()
    "Jump to a marker in the `gumshoe--log'.

The command supports preview of the currently selected marker position. "
    (interactive)
    ;; Overwrite the global-mark-ring with my-markers
    (let ((global-mark-ring-cpy global-mark-ring)
          (global-mark-ring (ring-elements gumshoe--log)))
      (consult--minibuffer-with-setup-hook
          ;; Restore the global-mark-ring in the minibuffer to keep recursive editing intact
          (lambda () (setq global-mark-ring global-mark-ring-cpy))
        (consult-global-mark))))
)


(provide 'gumshoe)
;;; gumshoe.el ends here
