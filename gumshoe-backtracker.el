;;; gumshoe-backtracker.el --- Backtracker class and methods -*- lexical-binding: t; -*-

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
;; Backtracker class and methods for managing backtracking state.

;;; Code:

(require 'eieio)
(require 'cl-lib)
(require 'gumshoe-context)
(require 'gumshoe-lib)
(require 'gumshoe-footprints)

(defclass gumshoe--backtracker ()
  ((backlog :initform nil
            :initarg :backlog
            :documentation "Ring-buffer to remember the previous editing position.")
   (filter :initform #'gumshoe-context--valid-p
           :documentation "Filter used when backtracking.")
   (filtered :initform nil
             :documentation "The filtered log list used when backtracking.")
   (footprints :initform nil
               :documentation "An overlay indicating previous locations.")
   (index :initform 0
          :documentation "Current index backwards into the log when backtracking.")
   (last-position :initform nil
                  :documentation "The entry where the user last stopped backtracking.")
   (msg :initform ""
        :documentation "Stores info for the user during backtracking."))
  "Gumshoe's backtracker keeps track of backtracking state.")

(cl-defmethod gumshoe--increment-index ((self gumshoe--backtracker) incrementer)
  "Increment index in SELF with INCREMENTER function.

In particular, notify users if index would go outside log boundaries."
  (with-slots (index msg filtered footprints) self
    (setf index (funcall incrementer index 1))
    (cond
     ((>= index (length filtered))
      (setf msg "This is the earliest entry...")
      (setf index (- (length filtered) 1)))
     ((< index 0)
      (setf msg "This is the latest entry...")
      (setf index 0))
     (t
      (setf msg (format "Gumshoe: entry #%i: %i"
                        (- (length filtered) index)
                        (length filtered)))))))

(cl-defmethod gumshoe--init-backtracking ((self gumshoe--backtracker) filter-pred)
  "Initialize SELF for backtracking using FILTER-PRED as the filter predicate."
  (with-slots (backlog filter filtered msg index) self
    (setf filter filter-pred)
    (gumshoe--clean backlog)
    (message "timeline %s" (gumshoe--construct-timeline backlog))
    (setf filtered
          (if filter
              (seq-filter filter (gumshoe--construct-timeline backlog))
            (gumshoe--construct-timeline backlog)))
    (when gumshoe-show-footprints-p
      (gumshoe--mark-footprints filtered))
    (setf index -1)))

(cl-defmethod gumshoe--jump-to-index ((self gumshoe--backtracker) new-index &optional message)
  "Jump to NEW-INDEX in SELF's filtered timeline.
Optionally display MESSAGE."
  (with-slots (index filtered msg) self
    (let ((prev-index index))
      (setf index new-index)
      (when message
        (setf msg message))
      (when gumshoe-show-footprints-p
        (gumshoe--hl-current-footprint filtered prev-index index))
      (if (not filtered)
          (setf msg "I haven't recorded any entries here yet...")
        (gumshoe-context--jump (nth index filtered)))
      (when msg (message msg)))))

(cl-defmethod gumshoe--backtrack ((self gumshoe--backtracker) incrementer)
  "Backtrack using INCREMENTER in SELF.

Only including results satisfying FILTER.
INCREMENTER increments the index in SELF."
  (with-slots (index msg filtered) self
    (gumshoe--increment-index self incrementer)
    (gumshoe--jump-to-index self index msg)))

(cl-defmethod gumshoe--timer-callback ((self gumshoe--backtracker))
  "Called by timer to log current position in SELF."
  (with-slots (backlog) self
    (gumshoe--log-if-necessary backlog t)))

(provide 'gumshoe-backtracker)
;;; gumshoe-backtracker.el ends here
