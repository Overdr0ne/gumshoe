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

(require 'eieio)

(defgroup context nil
  "Library for storing and interacting with user context metadata."
  :group 'convenience
  :prefix "context-")

(defcustom context-prefer-same-window nil
  "Prefer jumping using the window where point currently is."
  :type 'boolean
  :group 'context)

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
   (category :initform nil
             :documentation "Identifier for context category.")
   (overlay :initform nil
            :documentation "Overlay for visual context information.
This must be set manually because overlays cannot be garbage collected.")   )
  "Entry class for tracking user context information.")

(defcustom context-horizontal-scale 4
  "Horizontal follow distances are divided by this factor."
  :type 'integer
  :group 'context)

(cl-defmethod context--valid-p ((self context))
  "Return t if SELF is valid."
  (cl-typep self 'context))

(cl-defmethod context--jump ((self context))
  "Jump Point to buffer and position in SELF."
  (let ((position (overlay-start (oref self overlay))))
    (with-slots (buffer) self
      (if context-prefer-same-window
          (pop-to-buffer-same-window buffer)
        (pop-to-buffer buffer))
      (goto-char position))))

(cl-defmethod context--dead-p ((self context))
  "Check if SELF is dead."
  (if-let* ((overlay (oref self overlay))
            (buffer (overlay-buffer overlay))
            (pos (overlay-start overlay)))
      (if (or (not buffer)
              (not pos)
              (not (buffer-live-p buffer))
              (with-current-buffer buffer
                (>= pos (point-max))))
          t
        nil)
    t))

;;; filter predicates
(cl-defmethod context--in-current-buffer-p ((entry context))
  "Check if ENTRY in the current perspective."
  (equal (oref entry buffer) (current-buffer)))

(cl-defmethod context--in-current-window-p ((entry context))
  "Check if ENTRY in the current window."
  (equal (oref entry window) (get-buffer-window (current-buffer))))

(cl-defmethod context--marker-context-p ((entry context))
  "Check if ENTRY in the current window."
  (equal (oref entry category) "marker"))

(defun context--column-at (pos)
  "Return column number at POS."
  (save-excursion
    (goto-char pos)
    (current-column)))
(cl-defmethod context--distance-to ((self context))
  "Return the Euclidean distance between point and SELF."
  (if-let* ((overlay (oref self overlay))
            (buf (overlay-buffer overlay))
            (_ (equal buf (current-buffer)))
            (pos (overlay-start overlay))
            (line (with-current-buffer buf (line-number-at-pos pos)))
            (dline (abs (- line
                           (line-number-at-pos (point)))))
            (column (context--column-at pos))
            (dcolumn (abs (- column
                             (current-column))))
            (dcolumn-scaled (/ dcolumn context-horizontal-scale)))
      (sqrt (+ (expt dline 2) (expt dcolumn-scaled 2)))
    (message "failed to get distance for context %s" self)
    1000))
(cl-defmethod context--equal ((self context) (other context))
  "Return t if SELF and OTHER are approximately equal."
  (and
   (equal (oref self filename) (oref other filename))
   (oref self overlay)
   (oref other overlay)
   (equal (overlay-start (oref self overlay))
          (overlay-start (oref other overlay)))))

(cl-defmethod context--cleanup ((self context))
  "Clean up resources (like overlays) held by SELF."
  (let ((overlay (oref self overlay)))
    (when (and overlay (overlayp overlay))
      (delete-overlay overlay)
      (oset self overlay nil))))

(provide 'context)
;;; context.el ends here
