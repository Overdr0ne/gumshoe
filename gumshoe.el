;;; gumshoe.el --- Scoped spatial and temporal POINT movement tracking -*- lexical-binding: t; -*-

;; Copyright (C) 2021 overdr0ne

;; Author: overdr0ne
;; Version: 3.1
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
;; Gumshoe is a global minor mode that quietly keep tabs on your Point
;; movements so you can retrace your steps if you ever need a reminder of
;; where you've been.

;; Gumshoe logs any movements outside their minimum follow distance.
;; They will also log any position you idle at for a while.
;; You may then use their log to backtrack to previous locations.

;;; Code:

(require 'eieio)
(require 'cl-lib)
(require 'subr-x)
(require 'cl-generic)
(require 'context)
(require 'gumshoe-lib)
(require 'gumshoe-backtracker)
(require 'gumshoe-footprints)
(require 'gumshoe-peruse)
(require 'gumshoe-ring)
(require 'gumshoe-tree nil t)

(defcustom gumshoe-auto-cancel-backtracking-p t
  "Automatically cancel backtracking when non-backtracking commands are entered."
  :type 'boolean
  :group 'gumshoe)

(defcustom gumshoe-display-buffer-action '((display-buffer-reuse-window display-buffer-same-window))
  "`display-buffer-action' to use when jumping through the backlog.

See `display-buffer' for more information"
  :type '(alist :key-type symbol :value-type sexp)
  :group 'gumshoe)

(defcustom gumshoe-backlog-type 'ring
  "The data structure holding the backlog."
  :type '(radio (const :tag "The backlog is organized into a ring buffer." ring)
                (const :tag "The backlog is organized into a tree of timelines." tree))
  :group 'gumshoe)

(defun gumshoe--backlog-init (log-len)
  "Create a new backlog based on `gumshoe-backlog-type'.
LOG-LEN is the maximum number of entries for ring-based backlogs."
  (if (eq gumshoe-backlog-type 'tree)
      (etree--tree)
    (gumshoe--ring :ring (make-ring log-len))))

;;; Mode definition
(defclass gumshoe--mode ()
  ((backtracker :initform nil
                :documentation "Stores the backtracking state.")
   (timer :initform nil
          :documentation "Global idle timer that logs position for `gumshoe--global-backlog' after `gumshoe-idle-time'."))
  "Gumshoe mode information.")

(defcustom gumshoe-mode nil
  "Contains global data for gumshoe-mode."
  :type 'gumshoe--mode
  :group 'gumshoe)

(cl-defmethod gumshoe--init ((self gumshoe--mode))
  "Initialize SELF, setting hooks and timers."
  (with-slots (backtracker timer) self
    (setf backtracker (gumshoe--backtracker :backlog (gumshoe--backlog-init gumshoe-log-len)))
    (add-hook 'post-command-hook
              (apply-partially #'gumshoe--log-if-necessary (oref backtracker backlog)))
    (setf timer (run-with-idle-timer gumshoe-idle-time t
                                     (apply-partially #'gumshoe--timer-callback backtracker))))
  self)

(cl-defmethod gumshoe--shutdown ((self gumshoe--mode))
  "Shutdown SELF, removing hooks and cancelling timers."
  (with-slots (backtracker timer) self
    (remove-hook 'post-command-hook
                 (apply-partially #'gumshoe--log-if-necessary (oref backtracker backlog)))
    (cancel-timer timer))
  self)

;;;###autoload
(define-minor-mode global-gumshoe-mode
  "Toggle global Gumshoe minor mode.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When enabled, Gumshoe logs point movements when they exceed the
`gumshoe-follow-distance', or when the user is idle longer than
`gumshoe-idle-time'."
  :init-value nil
  :lighter " Gumshoe:global"
  :group 'gumshoe
  :global t
  (if global-gumshoe-mode
      (setf gumshoe-mode (gumshoe--init (gumshoe--mode)))
    (setf gumshoe-mode (gumshoe--shutdown gumshoe-mode))))

(defun global-gumshoe-buf-mode (&optional _)
  "Obsolete mode for buffer local tracking."
  (interactive) (global-gumshoe-mode +1))
(make-obsolete 'global-gumshoe-buf-mode 'global-gumshoe-mode "2.0")

;;; Backtracking mode movement commands
(defun global-gumshoe-backtracking-mode-back ()
  "Backtrack back in the Gumshoe backlog."
  (interactive)
  (gumshoe--backtrack (oref gumshoe-mode backtracker) #'+))

(defun global-gumshoe-backtracking-mode-forward ()
  "Backtrack forward in the Gumshoe backlog."
  (interactive)
  (gumshoe--backtrack (oref gumshoe-mode backtracker) #'-))

(defun gumshoe--backtracking-p ()
  "Was the last command a backtracking command?"
  (let ((backtracking-commands
         '(global-gumshoe-backtracking-mode-back
           global-gumshoe-backtracking-mode-forward
           gumshoe-backtrack
           gumshoe-marker-backtrack
           gumshoe-buf-backtrack
           gumshoe-win-backtrack
           gumshoe-backtrack-restart
           gumshoe-backtrack-cancel
           gumshoe-backtrack-resume)))
    (cl-some (lambda (cmd) (equal this-command cmd))
             backtracking-commands)))

(defun gumshoe--auto-cancel-backtracking ()
  "Automatically cancel last backtracking command if necessary."
  (unless (gumshoe--backtracking-p)
    (gumshoe-backtrack-quit)))

;;; High-level backtracking commands
(defun gumshoe-backtrack-quit ()
  "Quit backtracking."
  (interactive)
  (let* ((backtracker (oref gumshoe-mode backtracker)))
    (with-slots (filtered index last-position backlog) backtracker
      ;; Save the current position before quitting
      (when (and filtered (>= index 0) (< index (length filtered)))
        (setf last-position (nth index filtered))))
    (global-gumshoe-backtracking-mode -1)
    (let ((entries (gumshoe--construct-timeline (oref backtracker backlog))))
      (gumshoe--hide-footprints entries))))

(defun gumshoe-backtrack-restart ()
  "Restart backtracking from the latest entry in the backlog."
  (interactive)
  (gumshoe--jump-to-index (oref gumshoe-mode backtracker) 0 "Restarted at latest entry..."))

(defun gumshoe-backtrack-cancel ()
  "Return to the latest entry and quit backtracking."
  (interactive)
  (gumshoe--jump-to-index (oref gumshoe-mode backtracker) 0)
  (gumshoe-backtrack-quit))

(defun gumshoe-backtrack-resume ()
  "Resume backtracking at last stopped position.
If never backtracked before, start at the latest entry.
If the previous entry has been cleaned or wrapped, notify user and start
at the earliest available entry."
  (interactive)
  (let* ((backtracker (oref gumshoe-mode backtracker)))
    (with-slots (last-position backlog filter) backtracker
      (global-gumshoe-backtracking-mode +1)
      (gumshoe--init-backtracking backtracker (or filter #'context--valid-p))
      (if (not last-position)
          ;; Never backtracked before, start at latest
          (progn
            (gumshoe--backtrack backtracker #'+)
            (message "Starting backtrack at latest entry..."))
        ;; Try to find the last position in the current filtered timeline
        (with-slots (filtered) backtracker
          (let ((resume-index (cl-position last-position filtered :test #'context--equal)))
            (if resume-index
                ;; Found the entry, jump to it
                (gumshoe--jump-to-index backtracker resume-index
                                       (format "Resumed at entry #%i"
                                               (- (length filtered) resume-index)))
              ;; Entry not found (cleaned or wrapped), start at earliest
              (progn
                (gumshoe--jump-to-index backtracker (- (length filtered) 1)
                                       "Previous entry not found (cleaned or wrapped). Starting at earliest entry...")
                (message "Previous backtrack position has been cleaned or wrapped. Starting at earliest entry.")))))))))

;;; Backtracking mode definition
(defvar global-gumshoe-backtracking-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap keyboard-quit] 'gumshoe-backtrack-quit)
    (define-key map [remap gumshoe-backtrack-back] 'global-gumshoe-backtracking-mode-back)
    (define-key map [remap gumshoe-backtrack-forward] 'global-gumshoe-backtracking-mode-forward)
    (define-key map [remap gumshoe-buf-backtrack-back] 'global-gumshoe-backtracking-mode-back)
    (define-key map [remap gumshoe-buf-backtrack-forward] 'global-gumshoe-backtracking-mode-forward)
    (define-key map [remap gumshoe-win-backtrack-back] 'global-gumshoe-backtracking-mode-back)
    (define-key map [remap gumshoe-win-backtrack-forward] 'global-gumshoe-backtracking-mode-forward)
    (define-key map [remap backward-paragraph] 'global-gumshoe-backtracking-mode-back)
    (define-key map [remap forward-paragraph] 'global-gumshoe-backtracking-mode-forward)
    (define-key map (kbd "C-c") 'gumshoe-backtrack-cancel)
    (define-key map (kbd "C-r") 'gumshoe-backtrack-restart)
    (define-key map (kbd "C-v") 'gumshoe-backtrack-resume)
    map)
  "Transient keymap activated during global-gumshoe-backtracking-mode.")

(define-minor-mode global-gumshoe-backtracking-mode
  "A transient global mode to start Gumshoe backtracking."
  :global t
  :keymap global-gumshoe-backtracking-mode-map
  :group 'gumshoe
  (if global-gumshoe-backtracking-mode
      (progn
        (when gumshoe-auto-cancel-backtracking-p
          (add-hook 'post-command-hook
                    #'gumshoe--auto-cancel-backtracking))
        (push `(global-gumshoe-backtracking-mode . ,global-gumshoe-backtracking-mode-map)
              minor-mode-map-alist))
    (setf minor-mode-map-alist (assoc-delete-all 'global-gumshoe-backtracking-mode minor-mode-map-alist))
    (when gumshoe-auto-cancel-backtracking-p
      (remove-hook 'post-command-hook
                   #'gumshoe--auto-cancel-backtracking))))

;;; Command interface macro
(defmacro gumshoe--make-xface (backtrack-name peruse-name filter-name)
  "Make a command interface for the given filter.

BACKTRACK-NAME is the name of the backtracking command.

A command will be generated for perusal called PERUSE-NAME.
Results will be filtered using FILTER-NAME function."
  `(progn
     (defun ,peruse-name ()
       (interactive)
       (gumshoe--peruse (gumshoe--construct-timeline (oref (oref gumshoe-mode backtracker) backlog))
                        gumshoe-slot-schema
                        #',filter-name))
     (defun ,backtrack-name ()
       (interactive)
       (global-gumshoe-backtracking-mode +1)
       (gumshoe--init-backtracking (oref gumshoe-mode backtracker) #',filter-name)
       (gumshoe--backtrack (oref gumshoe-mode backtracker) #'+))))

;; Generate scoped backtracking and peruse commands
(gumshoe--make-xface gumshoe-backtrack gumshoe-peruse-globally context--valid-p)
(gumshoe--make-xface gumshoe-buf-backtrack gumshoe-peruse-in-buffer context--in-current-buffer-p)
(gumshoe--make-xface gumshoe-win-backtrack gumshoe-peruse-in-window context--in-current-window-p)

;; Markers for manually marking a position
(defun gumshoe-drop-marker ()
  "Drop a \"marker\" at point."
  (interactive)
  (gumshoe--log (oref (oref gumshoe-mode backtracker) backlog)))

(gumshoe--make-xface gumshoe-marker-backtrack gumshoe-peruse-markers context--marker-context-p)

;; Obsolete functions
(make-obsolete 'gumshoe-backtrack-back 'gumshoe-buf-backtrack "3.0")
(make-obsolete 'gumshoe-backtrack-forward 'gumshoe-buf-backtrack "3.0")
(make-obsolete 'gumshoe-buf-backtrack-back 'gumshoe-buf-backtrack "3.0")
(make-obsolete 'gumshoe-buf-backtrack-forward 'gumshoe-buf-backtrack "3.0")
(make-obsolete 'gumshoe-win-backtrack-back 'gumshoe-win-backtrack "3.0")
(make-obsolete 'gumshoe-win-backtrack-forward 'gumshoe-win-backtrack "3.0")

(provide 'gumshoe)
;;; gumshoe.el ends here
