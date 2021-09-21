;;; gumshoe-persp.el --- perspective support for gumshoe  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  overdr0ne

;; Author: overdr0ne
;; Version: 2.0
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

;; This is intended to serve as a template for anyone else who would wish to
;; extend gumshoe with their own context metadata.  In this case, I add a field
;; for a perspective, create a filter for that field, specialize the default
;; jumper to switch to the perspective on jump, and create a command interface
;; for all that.  You could certainly do more, or less, but this mirrors what I
;; did for the buffer-local interface.  Go wild.

;;; Code:

(require 'gumshoe-core)
(require 'perspective)

(defcustom gumshoe-persp-auto-config t
  "Automatically set the backlog type to `gumshoe--persp-entry’ if t."
  :group 'gumshoe
  :type 'boolean)

(defclass gumshoe--persp-entry (gumshoe--entry)
  ((perspective :initform (persp-current-name)
                :documentation "Flag indicating when a gumshoe is using the log to backtrack."))
  "Entry class for Gumshoe’s backlog, with perspectives.")

(cl-defmethod gumshoe--in-current-persp-p ((entry gumshoe--persp-entry))
  "Check if ENTRY in the current perspective."
  (equal (oref entry perspective) (persp-current-name)))

(cl-defmethod gumshoe--equal ((self gumshoe--persp-entry) (other gumshoe--persp-entry))
  "Check if SELF and OTHER are approximately equal."
  (and
   (equal (oref self perspective) (oref other perspective))
   (equal (oref self filename) (oref other filename))
   (equal (oref self position) (oref other position))))

(cl-defmethod gumshoe--jump ((self gumshoe--persp-entry))
  "Jump Point to buffer and position in SELF."
  (with-slots (buffer position perspective) self
    (persp-switch perspective)
    (pop-to-buffer buffer)
    (goto-char position)))

(define-minor-mode global-gumshoe-persp-mode
  "Toggle global Gumshoe minor mode.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When enabled, Gumshoe logs point movements when they exceed the
`gumshoe-follow-distance', or when the user is idle longer than
`gumshoe-idle-time'."
  :init-value nil
  :lighter " Gumshoe:persp"
  :group 'gumshoe
  :global t
  (if global-gumshoe-persp-mode
      (setf gumshoe-mode (gumshoe--init (gumshoe--mode) 'gumshoe--persp-entry))
    (setf gumshoe-mode (gumshoe--shutdown gumshoe-mode))))

(gumshoe--make-xface gumshoe-persp-backtrack-back gumshoe-persp-backtrack-forward gumshoe-peruse-in-persp gumshoe--in-current-persp-p)

(provide 'gumshoe-persp)
;;; gumshoe-persp.el ends here
