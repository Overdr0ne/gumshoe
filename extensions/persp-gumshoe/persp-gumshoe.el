;;; persp-gumshoe.el --- Perspective support for gumshoe  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  overdr0ne

;; Author: overdr0ne
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

;; This package provides perspective.el integration for gumshoe.
;; It extends gumshoe's context tracking to include perspective information,
;; allowing you to backtrack through your movement history within specific
;; perspectives.
;;
;; Usage:
;;   (require 'persp-gumshoe)
;;
;; This will automatically configure gumshoe to track perspective information
;; and provide perspective-aware backtracking commands.

;;; Code:

(require 'perspective)
(require 'gumshoe)
(require 'persp-context)

(defcustom persp-gumshoe-auto-config t
  "Automatically set the backlog type to `persp-context' if t."
  :group 'gumshoe
  :type 'boolean)

(defun global-persp-gumshoe-mode (&optional _)
  "Obsolete mode for persp local tracking."
  (interactive) (global-gumshoe-mode +1))
(make-obsolete 'global-persp-gumshoe-mode 'global-gumshoe-mode "2.0")

(setf gumshoe-entry-type 'persp-context)
(gumshoe--make-xface gumshoe-persp-backtrack gumshoe-peruse-in-persp context--in-current-persp-p)

(make-obsolete 'gumshoe-persp-backtrack-back 'gumshoe-persp-backtrack "3.0")
(make-obsolete 'gumshoe-persp-backtrack-forward 'gumshoe-persp-backtrack "3.0")
(define-key global-gumshoe-backtracking-mode-map [remap gumshoe-persp-backtrack-forward] 'global-gumshoe-backtracking-mode-forward)
(define-key global-gumshoe-backtracking-mode-map [remap gumshoe-persp-backtrack-back] 'global-gumshoe-backtracking-mode-back)

(provide 'persp-gumshoe)
;;; persp-gumshoe.el ends here
