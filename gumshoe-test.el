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

(ert-deftest gumshoe-mode-toggle ()
  (global-gumshoe-mode +1)
  (global-gumshoe-mode -1))

(ert-deftest gumshoe-backtrack ()
  (let (start gumshoe-show-footprints-p)
    (save-excursion
      (global-gumshoe-mode +1)
      (setf gumshoe-show-footprints-p nil)
      (call-interactively #'gumshoe-backtrack-back)
      (setf start (point))
      (call-interactively #'gumshoe-backtrack-back)
      (call-interactively #'gumshoe-backtrack-forward)
      (should (equal start (point)))
      (call-interactively #'forward-char)
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
