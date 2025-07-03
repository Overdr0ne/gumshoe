;;; gumshoe-lib.el --- helper functions for gumshoe  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Samuel Morris

;; Author: Samuel Morris <scmorris.dev@gmail.com>
;; Keywords: lisp

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

;; 

;;; Code:

(require 'context)

(defgroup gumshoe nil
  "The gumshoe movement tracker."
  :group 'convenience
  :prefix "gumshoe-")

(defcustom gumshoe-log-len 300
  "Length of gumshoe--backlog’s ring-buffer."
  :type 'integer)
(defcustom gumshoe-follow-distance 15
  "Gumshoe logs movements beyond this Euclidean distance from previous entry."
  :type 'integer)
(defcustom gumshoe-idle-time 60
  "Log context after this idle time."
  :type 'integer)
(defcustom gumshoe-footprint-radius 1
  "This is used to calculate what nearby footprints should be covered."
  :type 'integer)
(defcustom gumshoe-show-footprints-p t
  "Display footprint overlays when backtracking?"
  :type 'boolean)
(defcustom gumshoe-footprint-strategy 'cover-old
  "Strategy for creating a footprint."
  :type '(radio (const :tag "Delete overlapping footprints" delete-overlapping)
                (const :tag "Cover overlapping footprints" cover-old)
                (const :tag "Show all footprints" nil)))

(defcustom gumshoe-cover-old-footprints-p t
  "Initially cover any old footprints when backtracking.

The old footprints are still there, but won’t be revealed until you reach them.
Set to nil if you would like all footprints displayed at once."
  :type 'boolean)
(defcustom gumshoe-entry-type 'context
  "Type of entry Gumshoe should use in the backlog."
  :type 'symbol)

(defcustom gumshoe-slot-schema '(time buffer position line)
  "Entry slot order for perusing the backlog."
  :type '(repeat symbol))

(defcustom gumshoe-peruse-separator "|"
  "Separator to be used between gumshoe slots."
  :type 'string)

(defcustom gumshoe-prefer-same-window nil
  "Prefer jumping using the window where point currently is."
  :type 'boolean)

(defface gumshoe--peruse-separator-face
  '((t
     :inherit diary))
  "Face for peruse separators.")
(defface gumshoe--footprint-face
  '((t
     :inherit highlight
     :box (:line-width -3 :style pressed-button)
     :weight bold))
  "Face for footprint overlays.")
(defface gumshoe--current-footprint-face
  '((t :inherit match
       :box (:line-width -3 :style released-button)
       :weight bold))
  "Face for footprint overlays.")

(defcustom gumshoe-ignore-predicates '(minibufferp
                                       gumshoe--ignore-mode-p)
  "A list of predicates that will block gumshoe from logging when true."
  :type '(repeat function))

(defcustom gumshoe-ignored-major-modes
  '(fundamental-mode
    exwm-mode
    helm-major-mode)
  "Don't remember places in buffers in these major modes."
  :type '(repeat symbol))

(defcustom gumshoe-ignored-minor-modes
  '(ctrlf-mode
    isearch-mode
    global-gumshoe-backtracking-mode)
  "Don't remember places in buffers in these minor modes."
  :type '(repeat symbol))

(defun gumshoe--overlay-is-footprint-p (overlay)
  "Return non-nil if OVERLAY is a context."
  (if-let ((entry (overlay-get overlay 'container)))
      (object-of-class-p entry 'context)
    nil))

(defun gumshoe--footprints-at (position)
  (seq-filter 'gumshoe--overlay-is-footprint-p (overlays-in (- position gumshoe-footprint-radius) (+ position gumshoe-footprint-radius))))

;; tracking
(cl-defmethod gumshoe--end-of-leash-p ((last-entry context))
  "Check if LAST-ENTRY is outside gumshoe’s boundary."
  (> (context--distance-to last-entry)
     gumshoe-follow-distance))

(provide 'gumshoe-lib)
;;; gumshoe-lib.el ends here
