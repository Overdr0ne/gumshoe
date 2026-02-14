;;; persp-context.el --- Perspective.el integration for context -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Sam

;; Author: Sam <scmorris.dev@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emacs "27.1") (perspective "2.0") (gumshoe "4.0"))
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

;; This module provides perspective.el integration for the context library.
;; It defines a persp-context subclass that includes perspective information.

;;; Code:

(require 'eieio)
(require 'perspective)
(require 'context)

(defclass persp-context (context)
  ((perspective :initform (and (fboundp 'persp-current-name) (persp-current-name))
                :documentation "The perspective name for this context entry."))
  "Entry class for context with perspective tracking.")

(cl-defmethod context--in-current-persp-p ((entry persp-context))
  "Check if ENTRY in the current perspective."
  (equal (oref entry perspective) (persp-current-name)))

(cl-defmethod context--equal ((self persp-context) (other persp-context))
  "Check if SELF and OTHER are approximately equal."
  (and
   (equal (oref self perspective) (oref other perspective))
   (equal (oref self filename) (oref other filename))
   (equal (oref self position) (oref other position))))

(cl-defmethod context--jump ((self persp-context))
  "Jump Point to buffer, perspective and position in SELF."
  (with-slots (buffer perspective overlay) self
    (persp-switch perspective)
    (if context-prefer-same-window
        (pop-to-buffer-same-window buffer)
      (pop-to-buffer buffer))
    (let ((position (overlay-start overlay)))
      (goto-char position))))

(provide 'persp-context)
;;; persp-context.el ends here
