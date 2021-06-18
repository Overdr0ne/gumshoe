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

(defvar-local gumshoe--log-len 100)
(defvar-local gumshoe--log (make-ring gumshoe--log-len)
  "Buffer-local marker to remember the previous editing position.")
(ring-insert gumshoe--log (point))
(defvar-local gumshoe--prev-position 1
  "Buffer-local marker to remember the previous editing position.")

(defvar-local gumshoe--min-delta 15)
(defvar-local gumshoe--horizontal-scale 3)

(add-hook 'pre-command-hook 'gumshoe--track)

(defun gumshoe--line-number-at-pos (pos)
  "Return column number at POINT."
  (save-excursion
    (goto-char pos)
    (current-column)))

(defun gumshoe--delta (pos)
  (let* ((line (line-number-at-pos pos))
         (dline (abs (- line
                        (line-number-at-pos (point)))))
         (column (gumshoe--line-number-at-pos pos))
         (dcolumn (abs (- column
                          (current-column))))
         (dcolumn-scaled (/ dcolumn gumshoe--horizontal-scale)))
    (sqrt (+ (math-pow dline 2) (math-pow dcolumn-scaled 2)))))

;; (defun gumshoe--backtracking-p ()
;;   (eq this-command 'gumshoe-backtrack))

(defvar-local gumshoe--backtracking-p nil)
(defvar-local gumshoe--log-index 0)
(defun gumshoe--track ()
  "Track the previous editing position in `gumshoe--log'."
  (unless (or gumshoe--backtracking-p (minibufferp))
    (setq gumshoe--log-index 0)
    (when (> (gumshoe--delta (ring-ref gumshoe--log gumshoe--log-index))
             gumshoe--min-delta)
      (ring-insert gumshoe--log (point))))
  (setq gumshoe--backtracking-p nil))

(add-hook 'kill-buffer-hook 'gumshoe--prev-pos-kill-buffer-hook)

(defun gumshoe--prev-pos-kill-buffer-hook ()
  "Reclaim the buffer-local marker."
  (setq gumshoe--log nil))

(defun gumshoe-backtrack ()
  "Jump to the previous editing position."
  (interactive)
  (setq gumshoe--backtracking-p t)
  (unless (ring-empty-p gumshoe--log)
    (setq gumshoe--log-index (1+ gumshoe--log-index))
    (goto-char (ring-ref gumshoe--log gumshoe--log-index))))

(provide 'gumshoe)
;;; gumshoe.el ends here
