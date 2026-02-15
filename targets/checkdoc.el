(require 'checkdoc)

(let ((files (directory-files-recursively
              default-directory "\\.el\\'" nil
              (lambda (dir)
                (not (string-match-p "/\\(scratch\\|targets\\|\\.git\\)$" dir)))))
      (errors 0))
  (advice-add 'display-warning :before
              (lambda (&rest _) (setq errors (1+ errors))))
  (dolist (f files)
    (let ((base (file-name-nondirectory f)))
      (unless (or (string-match-p "\\(-test\\|test-\\)" base)
                  (member base '("elpa.el")))
        (checkdoc-file f))))
  (if (> errors 0)
      (progn
        (message "FAIL: checkdoc found %d warning(s)" errors)
        (kill-emacs 1))
    (message "PASS: All files pass checkdoc")
    (kill-emacs 0)))
