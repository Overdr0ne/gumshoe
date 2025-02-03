;;; context.el --- library for storing and interacting with meta-data on user context  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Sam

;; Author: Sam <scmorris.dev@gmail.com>
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

;; This is a library for storinng and using and manipulating data on user context

;;; Code:

(defclass context ()
  ((filename :initform (buffer-file-name)
             :documentation "The full path of this entry.")
   (buffer :initform (current-buffer)
           :documentation "The buffer object of this entry."
           :printer buffer-file-name)
   (position :initform (point)
             :documentation "Buffer position of this entry.")
   (line :initform (buffer-substring (line-beginning-position) (line-end-position))
         :documentation "A formatted line containing the position of this entry.")
   (time :initform (current-time-string)
         :documentation "Indicates the date and time of this entry.")
   (major-mode :initform (symbol-name major-mode)
               :documentation "Major mode of this entry.")
   (window :initform (get-buffer-window (current-buffer))
           :documentation "Window of this entry.")
   (footprint-overlay :initform nil
                      :documentation "Footprint overlay.
This must be set manually because overlays cannot be garbage collected.")   )
  "Entry class for Gumshoe’s backlog.")

(defcustom gumshoe-horizontal-scale 4
  "Horizontal follow distances are divided by this factor."
  :type 'integer)

(cl-defmethod context--valid-p ((self context))
  "Return t if SELF is valid."
  (not (cl-check-type self context)))

(cl-defmethod context--jump ((self context))
  "Jump Point to buffer and position in SELF."
  (let ((position (overlay-start (oref self footprint-overlay))))
    (with-slots (buffer) self
      (if gumshoe-prefer-same-window
          (pop-to-buffer-same-window buffer)
        (pop-to-buffer buffer))
      (goto-char position))))

(cl-defmethod context--dead-p ((self context))
  "Check if SELF is dead."
  (if (oref self footprint-overlay)
      (let* ((buffer (oref self buffer))
             (pos (overlay-start (oref self footprint-overlay))))
        (when (or (not pos)
                  (not (buffer-live-p buffer))
                  (with-current-buffer buffer
                    (>= pos (point-max))))
          (message "SAMSAMSAM: dead entry %s" self)
          (message "SAMSAMSAM: dead overlay %s" (oref self footprint-overlay))
          (message "SAMSAMSAM: dead pos %s" (overlay-start (oref self footprint-overlay)))
          t))
    t))

;;; filter predicates
(cl-defmethod context--in-current-buffer-p ((entry context))
  "Check if ENTRY in the current perspective."
  (equal (oref entry buffer) (current-buffer)))

(cl-defmethod context--in-current-window-p ((entry context))
  "Check if ENTRY in the current window."
  (equal (oref entry window) (get-buffer-window (current-buffer))))

(defun context--column-at (pos)
  "Return column number at POS."
  (save-excursion
    (goto-char pos)
    (current-column)))
(cl-defmethod context--distance-to ((self context))
  "Return the Euclidean distance between point and SELF."
  (let* ((pos (overlay-start (oref self footprint-overlay)))
         (line (line-number-at-pos pos))
         (dline (abs (- line
                        (line-number-at-pos (point)))))
         (column (context--column-at pos))
         (dcolumn (abs (- column
                          (current-column))))
         (dcolumn-scaled (/ dcolumn gumshoe-horizontal-scale)))
    (sqrt (+ (expt dline 2) (expt dcolumn-scaled 2)))))
(cl-defmethod context--equal ((self context) (other context))
  "Return t if SELF and OTHER are approximately equal."
  (and
   (equal (oref self filename) (oref other filename))
   (oref self footprint-overlay)
   (oref other footprint-overlay)
   (equal (overlay-start (oref self footprint-overlay))
          (overlay-start (oref other footprint-overlay)))))

(when (require 'perspective nil t)
  (defclass context-persp (context)
    ((perspective :initform (persp-current-name)
                  :documentation "Flag indicating when a gumshoe is using the log to backtrack."))
    "Entry class for Gumshoe’s backlog, with perspectives.")

  (cl-defmethod context--in-current-persp-p ((entry context-persp))
    "Check if ENTRY in the current perspective."
    (equal (oref entry perspective) (persp-current-name)))

  (cl-defmethod context--equal ((self context-persp) (other context-persp))
    "Check if SELF and OTHER are approximately equal."
    (and
     (equal (oref self perspective) (oref other perspective))
     (equal (oref self filename) (oref other filename))
     (equal (oref self position) (oref other position))))

  (cl-defmethod context--jump ((self context-persp))
    "Jump Point to buffer, perspective and position in SELF."
    (with-slots (buffer perspective footprint-overlay) self
      (persp-switch perspective)
      (if gumshoe-prefer-same-window
          (pop-to-buffer-same-window buffer)
        (pop-to-buffer buffer))
      (let ((position (overlay-start footprint-overlay)))
        (goto-char position))))
  )

(provide 'context)
;;; context.el ends here
