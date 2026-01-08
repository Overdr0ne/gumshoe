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
  (should (fboundp 'gumshoe--peruse))
  (should (fboundp 'gumshoe--format-record)))

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
      (global-gumshoe-backtracking-mode-back)
      (global-gumshoe-backtracking-mode-forward)
      (should (equal start (point)))
      ;; Cancel backtracking
      (gumshoe-backtrack-cancel)
      (global-gumshoe-mode -1))))

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
