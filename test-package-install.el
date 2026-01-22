;;; test-package-install.el --- Test local package installation -*- lexical-binding: t; -*-

;; This script simulates package.el installing gumshoe from the local directory
;; to verify it compiles without errors (like it would on MELPA).

(require 'package)

;; Create a temporary package directory
(setq package-user-dir (make-temp-file "gumshoe-test-" t))
(setq package-archives nil)  ; Don't use any remote archives
(package-initialize)

(message "Installing gumshoe from local directory...")
(message "Package dir: %s" package-user-dir)

;; Install from the current directory
(package-install-file default-directory)

(message "\n=== Installation Summary ===")
(if (file-exists-p (expand-file-name "gumshoe-20*.elc" package-user-dir))
    (message "SUCCESS: gumshoe installed and compiled")
  (message "FAILED: gumshoe did not compile"))

;; Check for compilation errors
(with-current-buffer "*Compile-Log*"
  (goto-char (point-min))
  (let ((errors 0)
        (warnings 0))
    (while (re-search-forward "^.*:\\([0-9]+\\):\\([0-9]+\\): *\\(Error\\|Warning\\)" nil t)
      (let ((type (match-string 3)))
        (if (string= type "Error")
            (setq errors (1+ errors))
          (setq warnings (1+ warnings)))))
    (message "Compilation errors: %d" errors)
    (message "Compilation warnings: %d" warnings)
    (when (> errors 0)
      (message "\n=== FAIL: Compilation errors found ===")
      (goto-char (point-min))
      (while (re-search-forward "^.*Error:.*$" nil t)
        (message "  %s" (match-string 0)))
      (kill-emacs 1))
    (when (> warnings 0)
      (message "\n=== Compilation warnings found ===")))
  (message ""))

(message "=== PASS: No compilation errors ===")
