;;; consult-gumshoe.el --- Consult integration for gumshoe -*- lexical-binding: t; -*-

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

;; Consult integration for gumshoe, providing live preview functionality.
;; As you scroll through backlog entries, gumshoe automatically jumps to
;; each location to preview it.

;;; Code:

(require 'consult)
(require 'context)
(require 'gumshoe-lib)

(defun consult-gumshoe--preview-state ()
  "Create a preview state function for gumshoe entries.
Returns a closure that handles preview actions by jumping to context entries."
  (let ((current-entry nil)
        (original-window (selected-window))
        (original-buffer (current-buffer))
        (original-point (point)))
    (lambda (action cand)
      (pcase action
        ('preview
         ;; cand is the context object thanks to consult--lookup-candidate
         (when cand
           (setq current-entry cand)
           (condition-case nil
               (context--jump cand)
             (error nil))))
        ('return
         ;; Final selection - stay at the selected entry
         (when cand
           (context--jump cand)))
        ((or 'exit 'abort)
         ;; User cancelled - restore original position
         (when (and (not cand) original-window original-buffer)
           (when (window-live-p original-window)
             (select-window original-window))
           (when (buffer-live-p original-buffer)
             (set-buffer original-buffer)
             (goto-char original-point))))))))

(defun consult-gumshoe--peruse (objs slot-spec &optional obj-filter)
  "Peruse SLOT-SPEC fields of OBJS with live preview.

Pre-filter results with OBJ-FILTER. As you scroll through candidates,
gumshoe will jump to each entry to preview it."
  (let* ((format-schema (string-join (mapcar #'symbol-name slot-spec)
                                     (propertize gumshoe-peruse-separator
                                                'face 'gumshoe--peruse-separator-face)))
         (prompt (concat (propertize "(" 'face 'gumshoe--peruse-separator-face)
                        format-schema
                        (propertize ")" 'face 'gumshoe--peruse-separator-face) ": "))
         (filtered-objs (gumshoe--filter-format-objs objs slot-spec obj-filter))
         ;; Use consult--candidate property for proper lookup
         (candidates (mapcar (lambda (pair)
                              (propertize (car pair) 'consult--candidate (cdr pair)))
                            filtered-objs)))
    (consult--read
     candidates
     :prompt prompt
     :sort nil  ; Preserve time-based order
     :require-match t
     :category 'gumshoe-context
     :lookup #'consult--lookup-candidate
     :state (consult-gumshoe--preview-state))))

;;; Command interface

;;;###autoload
(defun consult-gumshoe-peruse-in-backlog ()
  "Peruse all entries in the backlog with live preview."
  (interactive)
  (let* ((backlog (oref (oref gumshoe-mode backtracker) backlog))
         (entries (gumshoe--construct-timeline backlog)))
    (consult-gumshoe--peruse entries gumshoe-slot-schema #'context--valid-p)))

;;;###autoload
(defun consult-gumshoe-peruse-in-buffer ()
  "Peruse entries in current buffer with live preview."
  (interactive)
  (let* ((backlog (oref (oref gumshoe-mode backtracker) backlog))
         (entries (gumshoe--construct-timeline backlog)))
    (consult-gumshoe--peruse entries gumshoe-slot-schema #'context--in-current-buffer-p)))

;;;###autoload
(defun consult-gumshoe-peruse-in-window ()
  "Peruse entries in current window with live preview."
  (interactive)
  (let* ((backlog (oref (oref gumshoe-mode backtracker) backlog))
         (entries (gumshoe--construct-timeline backlog)))
    (consult-gumshoe--peruse entries gumshoe-slot-schema #'context--in-current-window-p)))

;;;###autoload
(defun consult-gumshoe-peruse-markers ()
  "Peruse marker entries with live preview."
  (interactive)
  (let* ((backlog (oref (oref gumshoe-mode backtracker) backlog))
         (entries (gumshoe--construct-timeline backlog)))
    (consult-gumshoe--peruse entries gumshoe-slot-schema #'context--marker-context-p)))

(provide 'consult-gumshoe)
;;; consult-gumshoe.el ends here
