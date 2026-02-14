;;; gumshoe-lib.el --- Helper functions for gumshoe  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Samuel Morris

;; Author: Samuel Morris <scmorris.dev@gmail.com>
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

;; Shared configuration, customization variables, faces, and utility
;; functions used by all gumshoe modules.

;;; Code:

(require 'context)
(require 'cl-generic)

;; Generic methods for backlog implementations (ring and tree)
(cl-defgeneric gumshoe--clean (backlog)
  "Cleanup dead entries from BACKLOG.")

(cl-defgeneric gumshoe--clean-recent (backlog)
  "Cleanup recent dead entries from BACKLOG.")

(cl-defgeneric gumshoe--construct-timeline (backlog)
  "Construct timeline from BACKLOG.")

(cl-defgeneric gumshoe--log-if-necessary (backlog &optional alarmp)
  "Check current position and log in BACKLOG if significant.
Log automatically if ALARMP is t.")

(cl-defgeneric gumshoe--log (backlog)
  "Manually log current position in BACKLOG as a marker.")

(cl-defgeneric gumshoe--add-entry (backlog entry)
  "Add ENTRY to BACKLOG.")

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
  :type 'string
  :group 'gumshoe)

;; Obsolete alias for backward compatibility
(define-obsolete-variable-alias 'gumshoe-prefer-same-window 'context-prefer-same-window "3.1"
  "Use context-prefer-same-window instead.")

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
  "Get all footprint overlays at POSITION within radius."
  (seq-filter 'gumshoe--overlay-is-footprint-p (overlays-in (- position gumshoe-footprint-radius) (+ position gumshoe-footprint-radius))))

(defun gumshoe--ignore-mode-p ()
  "Return non-nil if current buffer's major mode is ignored."
  (or (member major-mode gumshoe-ignored-major-modes)
      (cl-some (lambda (mode)
                 (and (boundp mode)
                      (eval mode)))
               gumshoe-ignored-minor-modes)
      ;; Also ignore if we just exited backtracking mode
      (memq last-command '(gumshoe-backtrack-quit
                           gumshoe-backtrack-cancel))))

;; tracking
(cl-defmethod gumshoe--end-of-leash-p ((last-entry context))
  "Check if LAST-ENTRY is outside gumshoe's boundary."
  (> (context--distance-to last-entry)
     gumshoe-follow-distance))

(defun gumshoe--make-entry ()
  "Create a new context entry with overlay at current point."
  (let ((entry (funcall gumshoe-entry-type))
        (overlay (make-overlay (point) (point) (current-buffer))))
    (overlay-put overlay 'container entry)
    (oset entry overlay overlay)
    entry))

;; Object formatting utilities
(defun gumshoe--format-obj (obj format-string slot-spec)
  "Format OBJ according to FORMAT-STRING using SLOT-SPEC fields."
  (let* ((slot-vals (mapcar #'(lambda (slot)
				                        (ignore-error invalid-slot-name
				                          (slot-value obj slot))) slot-spec)))
    (apply #'format format-string slot-vals)))

(defun gumshoe--format-objs (obj-list format-string slot-spec)
  "Format objects in OBJ-LIST according to FORMAT-STRING using SLOT-SPEC fields."
  (mapcar #'(lambda (obj) (gumshoe--format-obj obj format-string slot-spec)) obj-list))

(defun gumshoe--filter-format-objs (objs slot-spec &optional obj-filter)
  "Filter and format OBJS according to SLOT-SPEC.
Returns alist of (formatted-string . obj) pairs.
If OBJ-FILTER is provided, only objects passing the filter are included."
  (let* ((format-components (mapcar #'(lambda (_) "%s") slot-spec))
         (separator (propertize gumshoe-peruse-separator 'face 'gumshoe--peruse-separator-face))
         (format-string (string-join format-components separator))
         (filtered-objs (if obj-filter
                            (seq-filter obj-filter objs)
                          objs))
         (obj-strings (gumshoe--format-objs filtered-objs format-string slot-spec)))
    (cl-mapcar #'cons obj-strings filtered-objs)))

(provide 'gumshoe-lib)
;;; gumshoe-lib.el ends here
