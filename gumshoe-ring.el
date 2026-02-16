;;; gumshoe-ring.el --- Ring-based backlog implementation -*- lexical-binding: t; -*-

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
;; Ring-based backlog implementation for gumshoe.

;;; Code:

(require 'ring)
(require 'eieio)
(require 'cl-generic)
(require 'cl-lib)
(require 'gumshoe-context)
(require 'gumshoe-lib)

(defclass gumshoe--ring ()
  ((ring :initform nil
         :initarg :ring
         :documentation "The underlying Emacs ring buffer."))
  "Wrapper class for Emacs ring buffer to enable method dispatch.")

(cl-defmethod gumshoe--delete ((self gumshoe--ring) index)
  "Delete entry at INDEX from SELF and clean up its overlay."
  (with-slots (ring) self
    (let ((entry (ring-ref ring index)))
      (when (object-of-class-p entry 'gumshoe-context)
        (gumshoe-context--cleanup entry))
      (ring-remove ring index))))

(cl-defmethod gumshoe--clean-recent ((self gumshoe--ring))
  "Cleanup recent dead entries from SELF."
  (with-slots (ring) self
    (unless (ring-empty-p ring)
      (let ((i 0)
            (continuep t))
        (while (and continuep
                    (< i (ring-length ring)))
          (let ((entry (ring-ref ring i)))
            (if (gumshoe-context--dead-p entry)
                (gumshoe--delete self i)
              (setq continuep nil))))))))

(cl-defmethod gumshoe--clean ((self gumshoe--ring))
  "Cleanup dead entries from SELF."
  (with-slots (ring) self
    (unless (ring-empty-p ring)
      (let ((i 0))
        (while (< i (ring-length ring))
          (let ((entry (ring-ref ring i)))
            (if (gumshoe-context--dead-p entry)
                (gumshoe--delete self i)
              (cl-incf i))))))))

(cl-defmethod gumshoe--remove-footprint-entry ((self gumshoe--ring) footprint)
  "Remove FOOTPRINT entry from SELF."
  (with-slots (ring) self
    (unless (ring-empty-p ring)
      (let* ((container (overlay-get footprint 'container))
             (index (ring-member ring container)))
        (when index
          (delete-overlay footprint)
          (ring-remove ring index))))))

(cl-defmethod gumshoe--remove-footprint-entries-at ((self gumshoe--ring) position)
  "Remove footprint entries at POSITION from SELF."
  (mapc (apply-partially #'gumshoe--remove-footprint-entry self)
        (gumshoe--footprints-at position)))

(cl-defmethod gumshoe--add-entry ((self gumshoe--ring) (entry gumshoe-context))
  "Add ENTRY to SELF."
  (with-slots (ring) self
    (ring-insert ring entry)))

(cl-defmethod gumshoe--log-if-necessary ((self gumshoe--ring) &optional alarmp)
  "Check current position and log in SELF if significant.

Log automatically if ALARMP is t."
  (with-slots (ring) self
    (unless (cl-some #'funcall gumshoe-ignore-predicates)
      (when (eq gumshoe-footprint-strategy 'delete-overlapping)
        (gumshoe--remove-footprint-entries-at self (point)))
      (gumshoe--clean-recent self)
      (let ((new-entry (gumshoe--make-entry)))
        (when (or (ring-empty-p ring)
                  (let ((latest-entry (ring-ref ring 0)))
                    (and (not (gumshoe-context--equal new-entry latest-entry))
                         (or alarmp
                             (not (gumshoe-context--in-current-buffer-p latest-entry))
                             (gumshoe--end-of-leash-p latest-entry)))))
          (gumshoe--add-entry self new-entry))))))

(cl-defmethod gumshoe--log ((self gumshoe--ring))
  "Manually log current position in SELF as a marker."
  (unless (cl-some #'funcall gumshoe-ignore-predicates)
    (let ((new-entry (gumshoe--make-entry)))
      (oset new-entry category "marker")
      (gumshoe--add-entry self new-entry))))

(cl-defmethod gumshoe--construct-timeline ((self gumshoe--ring))
  "Construct timeline from SELF."
  (with-slots (ring) self
    (ring-elements ring)))

(provide 'gumshoe-ring)
;;; gumshoe-ring.el ends here
