;;; gumshoe-test.el --- tests for gumshoe            -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2021 Free Software Foundation, Inc.

;; Author: overdr0ne
;; Keywords: internal

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

;; This packages provides the tests for `ert'.  They can be executed
;; from the command line as well by calling "make test".
;; Based on abo-abo/ivy tests

;;; Code:

(require 'ert)
(require 'gumshoe)

(message "%s" (emacs-version))

;;; Module Loading Tests

(ert-deftest gumshoe-test-load-lib ()
  "Test that gumshoe-lib loads without errors."
  (should (require 'gumshoe-lib nil t))
  (should (fboundp 'gumshoe--ignore-mode-p))
  (should (fboundp 'gumshoe--make-entry))
  (should (boundp 'gumshoe-log-len))
  (should (boundp 'gumshoe-follow-distance)))

(ert-deftest gumshoe-test-load-context ()
  "Test that context loads without errors."
  (should (require 'context nil t))
  (should (fboundp 'context--valid-p))
  (should (fboundp 'context--jump))
  (should (class-p 'context)))

(ert-deftest gumshoe-test-load-backtracker ()
  "Test that gumshoe-backtracker loads without errors."
  (should (require 'gumshoe-backtracker nil t))
  (should (class-p 'gumshoe--backtracker))
  (should (fboundp 'gumshoe--init-backtracking))
  (should (fboundp 'gumshoe--jump-to-index))
  (should (fboundp 'gumshoe--backtrack)))

(ert-deftest gumshoe-test-load-footprints ()
  "Test that gumshoe-footprints loads without errors."
  (should (require 'gumshoe-footprints nil t))
  (should (fboundp 'gumshoe--mark-footprints))
  (should (fboundp 'gumshoe--hide-footprints))
  (should (fboundp 'gumshoe--cover-old-footprints-at)))

(ert-deftest gumshoe-test-load-peruse ()
  "Test that gumshoe-peruse loads without errors."
  (should (require 'gumshoe-peruse nil t))
  (should (fboundp 'gumshoe--peruse)))

(ert-deftest gumshoe-test-load-ring ()
  "Test that gumshoe-ring loads and defines backlog-init."
  (should (require 'gumshoe-ring nil t))
  (should (class-p 'gumshoe--ring))
  (should (fboundp 'gumshoe--backlog-init))
  (should (fboundp 'gumshoe--log-if-necessary))
  (should (fboundp 'gumshoe--construct-timeline)))

(ert-deftest gumshoe-test-load-tree ()
  "Test that gumshoe-tree loads and defines backlog-init."
  (skip-unless (require 'dash nil t))
  (should (require 'gumshoe-tree nil t))
  (should (fboundp 'gumshoe--backlog-init))
  (should (fboundp 'gumshoe--log-if-necessary))
  (should (fboundp 'gumshoe--construct-timeline)))

(ert-deftest gumshoe-test-load-main ()
  "Test that main gumshoe module loads without errors."
  (should (require 'gumshoe nil t))
  (should (class-p 'gumshoe--mode))
  (should (fboundp 'global-gumshoe-mode))
  (should (fboundp 'gumshoe-backtrack))
  (should (fboundp 'gumshoe-backtrack-quit))
  (should (fboundp 'gumshoe-backtrack-restart))
  (should (fboundp 'gumshoe-backtrack-cancel))
  (should (fboundp 'gumshoe-backtrack-resume))
  (should (fboundp 'gumshoe-drop-marker)))

;;; Backlog Initialization Tests

(ert-deftest gumshoe-test-backlog-init-ring ()
  "Test that ring backlog can be initialized."
  (require 'gumshoe-ring)
  (let ((backlog (gumshoe--backlog-init 10)))
    (should backlog)
    (should (gumshoe--ring-p backlog))))

(ert-deftest gumshoe-test-backlog-init-tree ()
  "Test that tree backlog can be initialized."
  (skip-unless (require 'dash nil t))
  (require 'gumshoe-tree)
  (require 'etree)
  (let ((backlog (gumshoe--backlog-init 10)))
    (should backlog)
    (should (etree--tree-p backlog))))

;;; Mode Tests

(ert-deftest gumshoe-mode-toggle ()
  (global-gumshoe-mode +1)
  (global-gumshoe-mode -1))

(ert-deftest gumshoe-backtrack ()
  "Test basic backtracking functionality."
  (let (start gumshoe-show-footprints-p)
    (save-excursion
      (global-gumshoe-mode +1)
      (setf gumshoe-show-footprints-p nil)
      ;; Start backtracking
      (gumshoe-backtrack)
      (setf start (point))
      ;; Navigate back and forward
      (gumshoe-backtracking-back)
      (gumshoe-backtracking-forward)
      (should (equal start (point)))
      ;; Cancel backtracking
      (gumshoe-backtrack-cancel)
      (global-gumshoe-mode -1))))

;;; Optional Dependency Tests

(ert-deftest gumshoe-test-core-without-perspective ()
  "Test that core gumshoe functionality works without perspective loaded."
  ;; Ensure we're testing core functionality
  (should (require 'gumshoe nil t))
  (should (fboundp 'global-gumshoe-mode))

  ;; Test mode can be enabled without perspective
  (global-gumshoe-mode +1)
  (should gumshoe-mode)
  (should (oref gumshoe-mode backtracker))
  (should (oref gumshoe-mode timer))

  ;; Test basic commands work
  (should (fboundp 'gumshoe-backtrack))
  (should (fboundp 'gumshoe-buf-backtrack))
  (should (fboundp 'gumshoe-win-backtrack))
  (should (fboundp 'gumshoe-drop-marker))

  ;; Test backlog works
  (let ((backlog (oref (oref gumshoe-mode backtracker) backlog)))
    (should backlog)
    ;; Should be able to construct timeline
    (should (listp (gumshoe--construct-timeline backlog))))

  ;; Clean up
  (global-gumshoe-mode -1)
  ;; gumshoe-mode is set to the shutdown mode object, not nil
  ;; Just verify mode is disabled
  (should-not global-gumshoe-mode))

(ert-deftest gumshoe-test-core-without-completionist ()
  "Test that core gumshoe functionality works without completionist loaded."
  ;; Core commands should work without completionist
  (should (require 'gumshoe nil t))

  (global-gumshoe-mode +1)
  (should gumshoe-mode)

  ;; Test perusal with completing-read (doesn't require completionist)
  (should (fboundp 'gumshoe-peruse-globally))
  (should (fboundp 'gumshoe-peruse-in-buffer))
  (should (fboundp 'gumshoe-peruse-in-window))
  (should (fboundp 'gumshoe-peruse-markers))

  ;; Clean up
  (global-gumshoe-mode -1))

(ert-deftest gumshoe-test-core-without-consult ()
  "Test that core gumshoe functionality works without consult loaded."
  ;; Core commands should work without consult
  (should (require 'gumshoe nil t))

  (global-gumshoe-mode +1)
  (should gumshoe-mode)

  ;; Standard peruse commands should work without consult
  (should (fboundp 'gumshoe-peruse-globally))

  ;; Clean up
  (global-gumshoe-mode -1))

(ert-deftest gumshoe-test-optional-persp-loading ()
  "Test that gumshoe-persp only loads when perspective is available."
  (if (featurep 'perspective)
      ;; If perspective is loaded, gumshoe-persp should be available after loading
      (progn
        (require 'gumshoe-persp nil t)
        (should (featurep 'gumshoe-persp)))
    ;; If perspective is not loaded, gumshoe should still work
    (should (require 'gumshoe nil t))
    (global-gumshoe-mode +1)
    (should gumshoe-mode)
    (global-gumshoe-mode -1)))

(ert-deftest gumshoe-test-optional-completionist-loading ()
  "Test that gumshoe-completionist is truly optional."
  (if (featurep 'completionist)
      ;; If completionist is loaded, the integration should work
      (progn
        (should (require 'gumshoe-completionist nil t))
        (should (fboundp 'gumshoe-completionist-backlog)))
    ;; If completionist is not loaded, gumshoe should still work
    (should (require 'gumshoe nil t))
    (global-gumshoe-mode +1)
    (should gumshoe-mode)
    ;; gumshoe-completionist should fail to load without completionist
    ;; (it has a hard require on completionist)
    (should-error (require 'gumshoe-completionist))
    (global-gumshoe-mode -1)))

(ert-deftest gumshoe-test-optional-consult-loading ()
  "Test that gumshoe-consult is truly optional."
  (if (featurep 'consult)
      ;; If consult is loaded, the integration should work
      (progn
        (should (require 'gumshoe-consult nil t))
        (should (fboundp 'gumshoe-consult-peruse-in-backlog)))
    ;; If consult is not loaded, gumshoe should still work
    (should (require 'gumshoe nil t))
    (global-gumshoe-mode +1)
    (should gumshoe-mode)
    ;; gumshoe-consult commands shouldn't exist
    (should-not (require 'gumshoe-consult nil t))
    (global-gumshoe-mode -1)))

(ert-deftest gumshoe-test-entry-creation-without-optional-deps ()
  "Test that context entries can be created without optional dependencies."
  (require 'context)
  (require 'gumshoe-lib)

  ;; Create a context entry
  (with-temp-buffer
    (insert "test content\n")
    (goto-char (point-min))
    (let ((entry (gumshoe--make-entry)))
      (should entry)
      (should (context-p entry))
      (should (oref entry overlay))
      (should (buffer-live-p (oref entry buffer)))
      (should (numberp (oref entry position)))
      ;; Clean up
      (context--cleanup entry))))

(ert-deftest gumshoe-test-backlog-operations-without-optional-deps ()
  "Test basic backlog operations work without optional dependencies."
  (require 'gumshoe)

  (global-gumshoe-mode +1)
  (let* ((backtracker (oref gumshoe-mode backtracker))
         (backlog (oref backtracker backlog)))

    ;; Test adding entries - use text-mode to avoid fundamental-mode ignore
    (with-temp-buffer
      (text-mode)  ; Use text-mode instead of fundamental-mode
      (insert "test line 1\n")
      (insert "test line 2\n")
      (insert "test line 3\n")

      (goto-char (point-min))
      ;; Force logging with alarmp t
      (gumshoe--log-if-necessary backlog t)

      (goto-char (point-max))
      ;; Force logging with alarmp t
      (gumshoe--log-if-necessary backlog t)

      ;; Should have entries in backlog
      (let ((timeline (gumshoe--construct-timeline backlog)))
        (should (>= (length timeline) 2))))

    ;; Clean up
    (global-gumshoe-mode -1)))

(defun gumshoe-test-run-tests ()
  (let ((test-sets
         '(gumshoe-mode-toggle))
        (unexpected 0))
    (dolist (test-set test-sets)
      (cl-incf
       unexpected
       (ert-stats-completed-unexpected
        (ert-run-tests-batch test-set))))
    (kill-emacs (if (zerop unexpected) 0 1))))

(provide 'gumshoe-test)
;;; gumshoe-test.el ends here
