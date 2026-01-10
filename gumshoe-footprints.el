;;; gumshoe-footprints.el --- Footprint overlay management -*- lexical-binding: t; -*-

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
;; Functions for creating and managing footprint overlays during backtracking.

;;; Code:

(require 'context)
(require 'gumshoe-lib)

(defun gumshoe--cover-old-footprints-at (position)
  "Hide footprint labels at POSITION."
  (let* ((footprints (gumshoe--footprints-at position)))
    (dolist (footprint-i footprints)
      (overlay-put footprint-i 'after-string ""))))

(cl-defmethod gumshoe--mark-footprint ((self context) id face)
  "Add footprint overlay to SELF, labeled with ID, using FACE."
  (with-slots (buffer overlay) self
    (message (buffer-name buffer))
    (let* ((label (int-to-string id)))
      (when (and buffer (> (buffer-size buffer) 1))
        (put-text-property 0 (length label) 'face face label)
        (overlay-put overlay 'after-string label)))))

(defun gumshoe--replace-footprint (entries index face)
  "Add footprint overlay at footprint INDEX in ENTRIES, using FACE."
  (let* ((label (int-to-string (- (length entries) index)))
         (entry (nth index entries))
         (overlay (slot-value entry 'overlay))
         (position (overlay-start overlay)))
    (when (eq gumshoe-footprint-strategy 'cover-old)
      (gumshoe--cover-old-footprints-at position))
    (put-text-property 0 (length label) 'face face label)
    (overlay-put overlay 'after-string label)))

(defun gumshoe--hl-current-footprint (entries prev-index cur-index)
  "Replace PREV-INDEX with CUR-INDEX as current footprint in ENTRIES."
  (when entries
    (gumshoe--replace-footprint entries prev-index 'gumshoe--footprint-face)
    (gumshoe--replace-footprint entries cur-index 'gumshoe--current-footprint-face)))

(defun gumshoe--mark-footprints (entries)
  "Display footprints for all ENTRIES."
  (let ((i 1))
    (dolist (entry (reverse entries))
      (let ((position (overlay-start (oref entry overlay))))
        (when (eq gumshoe-footprint-strategy 'cover-old)
          (gumshoe--cover-old-footprints-at position))
        (gumshoe--mark-footprint entry i 'gumshoe--footprint-face)
        (cl-incf i)))))

(defun gumshoe--hide-footprints (entries)
  "Hide footprints in ENTRIES."
  (dolist (entry entries)
    (with-slots (overlay) entry
      (overlay-put overlay 'after-string ""))))

(provide 'gumshoe-footprints)
;;; gumshoe-footprints.el ends here
