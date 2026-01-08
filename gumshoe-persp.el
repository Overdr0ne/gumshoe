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

;; Note: gumshoe.el is already loaded when this file is required
;; (via with-eval-after-load), so no need to require it again
(require 'perspective)
(require 'context)

(defcustom gumshoe-persp-auto-config t
  "Automatically set the backlog type to `context-persp’ if t."
  :group 'gumshoe
  :type 'boolean)

(defun global-gumshoe-persp-mode (&optional _)
  "Obsolete mode for persp local tracking."
  (interactive) (global-gumshoe-mode +1))
(make-obsolete 'global-gumshoe-persp-mode 'global-gumshoe-mode "2.0")

(setf gumshoe-entry-type 'context-persp)
(gumshoe--make-xface gumshoe-persp-backtrack gumshoe-peruse-in-persp context--in-current-persp-p)

(make-obsolete 'gumshoe-persp-backtrack-back 'gumshoe-persp-backtrack "3.0")
(make-obsolete 'gumshoe-persp-backtrack-forward 'gumshoe-persp-backtrack "3.0")
(define-key global-gumshoe-backtracking-mode-map [remap gumshoe-persp-backtrack-forward] 'global-gumshoe-backtracking-mode-forward)
(define-key global-gumshoe-backtracking-mode-map [remap gumshoe-persp-backtrack-back] 'global-gumshoe-backtracking-mode-back)

(provide 'gumshoe-persp)
;;; gumshoe-persp.el ends here
