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

(defvar gumshoe--log-len 100
  "Length of gumshoe’s log ring-buffer.")
(defvar gumshoe--log (make-ring gumshoe--log-len)
  "Ring-buffer to remember the previous editing position.")
(ring-insert gumshoe--log (point-marker))

(defvar gumshoe--follow-distance 15
  "Gumshoe logs movements beyond this Euclidean distance from previous entry.")
(defvar gumshoe--horizontal-scale 3
  "Horizontal distances are divided by this factor.")

(defvar gumshoe--backtracking-p nil
  "Flag indicating when gumshoe is backtracking, to pause tracking.")
(defvar gumshoe--log-index 0
  "Current index backwards into the log when backtracking.")

(add-hook 'pre-command-hook 'gumshoe--track)

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
         (dcolumn-scaled (/ dcolumn gumshoe--horizontal-scale)))
    (sqrt (+ (math-pow dline 2) (math-pow dcolumn-scaled 2)))))

(defun gumshoe--end-of-leash-p (last-mark)
  "Check if LAST-MARK is outside gumshoe’s boundary."
  (> (gumshoe--distance-to last-mark)
     gumshoe--follow-distance))

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

(defun gumshoe--jump-to-marker (marker)
  (let ((buf  (marker-buffer marker)))
    (pop-to-buffer buf)
    (goto-char marker)))

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

(with-eval-after-load 'consult
  (require 'consult)

  (defun consult--gumshoe-candidates ()
    "Return alist of lines containing gumshoe markers.
The alist contains (string . position) pairs."
    (consult--forbid-minibuffer)
    (let ((candidates)
          (gumshoe--log-list (ring-elements gumshoe--log)))
      (save-excursion
        (dolist (marker gumshoe--log-list)
          (with-current-buffer (marker-buffer marker)
            (let ((pos (marker-position marker)))
              (when (and pos (consult--in-range-p pos))
                (goto-char pos)
                ;; `line-number-at-pos' is a very slow function, which should be replaced everywhere.
                ;; However in this case the slow line-number-at-pos does not hurt much, since
                ;; the mark ring is usually small since it is limited by `mark-ring-max'.
                (push (consult--location-candidate
                       (concat (buffer-name (marker-buffer marker))
                               ":"
                               (consult--line-with-cursor marker))
                       marker
                       (line-number-at-pos pos consult-line-numbers-widen))
                      candidates))))))
      (nreverse (delete-dups candidates))))

  (defun consult-gumshoe ()
    "Jump to a marker in the `gumshoe--log'.

The command supports preview of the currently selected marker position. "
    (interactive)
    (setq gumshoe--backtracking-p t)
    (consult--read
     (consult--with-increased-gc (consult--gumshoe-candidates))
     :prompt "Go to mark: "
     :annotate (consult--line-prefix)
     :category 'consult-location
     :sort nil
     :require-match t
     :lookup #'consult--lookup-location
     :history '(:input consult--line-history)
     :state (consult--jump-state))))


(provide 'gumshoe)
;;; gumshoe.el ends here
