;;; gumshoe-peruse.el --- Peruse interface for browsing backlog -*- lexical-binding: t; -*-

;; Copyright (C) 2025 overdr0ne

;; Author: overdr0ne
;; Version: 4.0
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
;; Peruse interface for visually browsing the backlog using completing-read.

;;; Code:

(require 'context)
(require 'gumshoe-lib)

(defun gumshoe--peruse (objs slot-spec &optional obj-filter)
  "Peruse SLOT-SPEC fields of OBJS.

Pre-filter results with OBJ-FILTER."
  (let* ((format-schema (string-join (mapcar #'symbol-name slot-spec) (propertize gumshoe-peruse-separator 'face 'gumshoe--peruse-separator-face)))
         (prompt (concat (propertize "(" 'face 'gumshoe--peruse-separator-face)
			                   format-schema
			                   (propertize ")" 'face 'gumshoe--peruse-separator-face) ": "))
         (candidates (gumshoe--filter-format-objs objs slot-spec obj-filter))
         ;; Create completion table with metadata to preserve time-based order
         (collection (lambda (string pred action)
                       (if (eq action 'metadata)
                           '(metadata (display-sort-function . identity))
                         (complete-with-action action candidates string pred))))
         (candidate (completing-read prompt collection)))
    (context--jump (cdr (assoc candidate candidates)))))

(provide 'gumshoe-peruse)
;;; gumshoe-peruse.el ends here
