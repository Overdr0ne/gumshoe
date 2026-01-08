;;; gumshoe-peruse.el --- Peruse interface for browsing backlog -*- lexical-binding: t; -*-

;; Copyright (C) 2025 overdr0ne

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
;; Peruse interface for visually browsing the backlog using completing-read.

;;; Code:

(require 'context)
(require 'gumshoe-lib)

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

(provide 'gumshoe-peruse)
;;; gumshoe-peruse.el ends here
