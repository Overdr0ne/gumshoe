;;; gumshoe-completionist.el --- completionist plugin for gumshoe  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 overdr0ne

;; Author: overdr0ne
;; Version: 3.1
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

;; Completionist integration for gumshoe backlog browsing.
;; Provides a persistent side-window interface for navigating the backlog.

;;; Code:

(require 'gumshoe)
(require 'gumshoe-lib)
(require 'completionist)

(defun gumshoe-completionist--get-candidates (backlog filter-fn)
  "Get formatted candidates from BACKLOG, filtered by FILTER-FN."
  (let ((entries (gumshoe--construct-timeline backlog)))
    (gumshoe--filter-format-objs entries gumshoe-slot-schema filter-fn)))

(defun gumshoe-completionist--collector (backlog filter-fn)
  "Collect and format entries from BACKLOG for completionist display.
FILTER-FN is applied to filter entries (or nil for no filtering).
Returns a completion table with metadata to preserve time-based order."
  (let ((candidates (mapcar #'car (gumshoe-completionist--get-candidates backlog filter-fn))))
    ;; Return completion table with metadata to preserve order
    (lambda (string pred action)
      (if (eq action 'metadata)
          '(metadata (display-sort-function . identity))
        (complete-with-action action candidates string pred)))))

(defun gumshoe-completionist--handler (backlog filter-fn)
  "Return a handler function that looks up entries from BACKLOG using FILTER-FN."
  (lambda (candidate-string)
    (when-let ((entry (cdr (assoc candidate-string
                                  (gumshoe-completionist--get-candidates backlog filter-fn)))))
      (context--jump entry))))

;;;###autoload
(defun gumshoe-completionist-backlog ()
  "Show gumshoe backlog in a completionist side window."
  (interactive)
  (let* ((backlog (oref (oref gumshoe-mode backtracker) backlog))
         (window-sides-slots '(1 1 1 1))
         (action '((display-buffer-in-side-window)
                   (window-width . 35)
                   (preserve-size . t)
                   (completionist-count . 50)
                   (side . right)
                   (slot . 0))))
    (completionist--complete
     "backlog:"
     (lambda () (gumshoe-completionist--collector backlog #'context--valid-p))
     (gumshoe-completionist--handler backlog #'context--valid-p)
     " *gumshoe backlog*"
     action
     nil    ; focus the completion buffer
     nil))) ; default vertical display mode

;;;###autoload
(defun gumshoe-completionist-backlog-buffer ()
  "Show gumshoe backlog filtered to current buffer."
  (interactive)
  (let* ((backlog (oref (oref gumshoe-mode backtracker) backlog))
         (window-sides-slots '(1 1 1 1))
         (action '((display-buffer-in-side-window)
                   (window-width . 35)
                   (preserve-size . t)
                   (completionist-count . 50)
                   (side . right)
                   (slot . 0))))
    (completionist--complete
     "backlog (buffer):"
     (lambda () (gumshoe-completionist--collector backlog #'context--in-current-buffer-p))
     (gumshoe-completionist--handler backlog #'context--in-current-buffer-p)
     " *gumshoe backlog (buffer)*"
     action
     nil
     nil)))

;;;###autoload
(defun gumshoe-completionist-backlog-window ()
  "Show gumshoe backlog filtered to current window."
  (interactive)
  (let* ((backlog (oref (oref gumshoe-mode backtracker) backlog))
         (window-sides-slots '(1 1 1 1))
         (action '((display-buffer-in-side-window)
                   (window-width . 35)
                   (preserve-size . t)
                   (completionist-count . 50)
                   (side . right)
                   (slot . 0))))
    (completionist--complete
     "backlog (window):"
     (lambda () (gumshoe-completionist--collector backlog #'context--in-current-window-p))
     (gumshoe-completionist--handler backlog #'context--in-current-window-p)
     " *gumshoe backlog (window)*"
     action
     nil
     nil)))

;;;###autoload
(defun gumshoe-completionist-markers ()
  "Show gumshoe markers in a completionist side window."
  (interactive)
  (let* ((backlog (oref (oref gumshoe-mode backtracker) backlog))
         (window-sides-slots '(1 1 1 1))
         (action '((display-buffer-in-side-window)
                   (window-width . 35)
                   (preserve-size . t)
                   (completionist-count . 50)
                   (side . right)
                   (slot . 0))))
    (completionist--complete
     "markers:"
     (lambda () (gumshoe-completionist--collector backlog #'context--marker-context-p))
     (gumshoe-completionist--handler backlog #'context--marker-context-p)
     " *gumshoe markers*"
     action
     nil
     nil)))

(provide 'gumshoe-completionist)
;;; gumshoe-completionist.el ends here
