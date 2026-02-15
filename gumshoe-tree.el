;;; gumshoe-tree.el --- Tree-based backlog implementation -*- lexical-binding: t; -*-

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
;; Tree-based backlog implementation for gumshoe.
;; Requires the dash library.

;;; Code:

;; (require 'dash)
(require 'etree)
(require 'cl-generic)
(require 'context)
(require 'gumshoe-lib)

;; Specialize etree cleanup for context objects to handle overlay cleanup
(cl-defmethod etree--cleanup ((entry context))
  "Clean up overlay resources held by context ENTRY."
  (context--cleanup entry))

(cl-defmethod gumshoe--clean-root ((self etree--tree))
  "Remove dead entries from the root of SELF."
  (let* (
         (timeline (gumshoe--construct-timeline-nodes self))
         iter
         (continuep t))
    (while (and continuep
                timeline)
      (setf iter (car timeline))
      (if (gumshoe--dead-p iter)
          (setf iter (etree--remove self iter))
        (setq continuep nil))
      (setf timeline (cdr timeline))))
  self)

(cl-defmethod gumshoe--clean-recent ((self etree--tree))
  "Clean up recent dead entries from SELF."
  (unless (eq (oref self current) (oref self root))
    (let ((iter (oref self current))
          (continuep t))
      (while (and continuep
                  iter)
        (if (gumshoe--dead-p iter)
            (etree--remove self iter)
          (setq continuep nil))
        (setq iter (oref self current))))))

(cl-defmethod gumshoe--dead-p ((self etree--node))
  "Return t if SELF references a dead entry."
  (if self
      (context--dead-p (oref self entry))
    t))
(cl-defmethod gumshoe--clean ((self etree--tree))
  "Remove all dead entries from SELF."
  (unless (eq (oref self current) (oref self root))
    (gumshoe--clean-recent self)
    (gumshoe--clean-root self)
    (mapc #'etree--delete
          (etree--collect self 'gumshoe--dead-p))))

(cl-defmethod gumshoe--remove-footprint-entry ((self etree--tree) footprint)
  "Remove FOOTPRINT entry from SELF."
  (when-let ((tree-node (overlay-get footprint 'tree-node)))
    (etree--remove self tree-node)
    (delete-overlay footprint)))

(cl-defmethod gumshoe--remove-footprint-entries-at ((self etree--tree) position)
  "Remove footprint entries at POSITION from SELF."
  (when position
    (mapc (apply-partially #'gumshoe--remove-footprint-entry self)
          (gumshoe--footprints-at position))))

;;; filter predicates
(cl-defmethod gumshoe--in-current-buffer-p ((self etree--node))
  "Check if SELF references an entry in the current buffer."
  (let ((entry (oref self entry)))
    (equal (oref entry buffer) (current-buffer))))

(cl-defmethod gumshoe--in-current-window-p ((self etree--node))
  "Check if SELF references an entry in the current window."
  (let ((entry (oref self entry)))
    (equal (oref entry window) (get-buffer-window (current-buffer)))))

(cl-defmethod gumshoe--construct-timeline-nodes ((self etree--tree))
  "Return the path from root to current node in SELF."
  (etree--path self))
(cl-defmethod gumshoe--construct-timeline ((self etree--tree))
  "Return entries along the path from root to current node in SELF."
  (reverse (mapcar (lambda (node) (oref node entry))
                   (etree--path self))))

(cl-defmethod gumshoe--add-entry ((self etree--tree) (entry context))
  "Add ENTRY to SELF."
  (let* ((new-node (etree--node :entry entry)))
    (cl-assert (and new-node (object-of-class-p new-node 'etree--node)))
    (when (eq gumshoe-footprint-strategy 'delete-overlapping)
      (gumshoe--remove-footprint-entries-at self (point)))
    ;; Set tree-node property on overlay (overlay should already exist from gumshoe--make-entry)
    (when-let ((overlay (oref entry overlay)))
      (overlay-put overlay 'tree-node new-node))
    (etree--insert self new-node)))

(cl-defmethod gumshoe--log-if-necessary ((self etree--tree) &optional alarmp)
  "Check current position and log in SELF if significant.

Log automatically if ALARMP is t."
  (unless (cl-some #'funcall gumshoe-ignore-predicates)
    (gumshoe--clean-recent self)
    (let ((new-entry (gumshoe--make-entry)))
      (when (or (not (oref self current))
                (not (oref (oref self current) entry))
                (let ((latest-entry (oref (oref self current) entry)))
                  (and (not (context--equal new-entry latest-entry))
                       (or alarmp
                           (not (context--in-current-buffer-p latest-entry))
                           (gumshoe--end-of-leash-p latest-entry)))))
        (gumshoe--add-entry self new-entry)))))

(cl-defmethod gumshoe--log ((self etree--tree))
  "Manually log current position in SELF as a marker."
  (unless (cl-some #'funcall gumshoe-ignore-predicates)
    (let ((new-entry (gumshoe--make-entry)))
      (oset new-entry category "marker")
      (gumshoe--add-entry self new-entry))))

(provide 'gumshoe-tree)
;;; gumshoe-tree.el ends here
