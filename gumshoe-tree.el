(require 'dash)
(require 'etree)

(cl-defmethod gumshoe--clean-root ((self etree--tree))
  (let* (
         (timeline (gumshoe--construct-timeline-nodes self))
         iter
         (continuep t))
    (while (and continuep
                timeline)
      (setf iter (car timeline))
      (if (gumshoe--dead-p iter)
          (setf iter (etree--remove self iter))
        (setq continuep nil))
      (setf timeline (cdr timeline))))
  self)

(cl-defmethod gumshoe--clean-recent ((self etree--tree))
  "Cleanup recent dead entries from RING."
  (unless (eq (oref self current) (oref self root))
    (let ((iter (oref self current))
          (continuep t))
      (while (and continuep
                  iter)
        (if (gumshoe--dead-p iter)
            (etree--remove self iter)
          (setq continuep nil))
        (setq iter (oref self current)))
      )))

(cl-defmethod gumshoe--dead-p ((self etree--node))
  (if self
      (gumshoe--dead-p (oref self entry))
    t))
(cl-defmethod gumshoe--clean ((self etree--tree))
  (unless (eq (oref self current) (oref self root))
    (gumshoe--clean-recent self)
    (gumshoe--clean-root self)
    (mapc #'etree--delete
          (etree--collect self 'gumshoe--dead-p)))
  )

(cl-defmethod gumshoe--remove-footprint-entry ((self etree--tree) footprint)
  (etree--remove self (overlay-get footprint 'container))
  (delete-overlay footprint))

(defun gumshoe--overlay-is-footprint-p (overlay)
  "Return non-nil if OVERLAY is a gumshoe--entry."
  (let ((container (overlay-get overlay 'container)))
    (if container
        (object-of-class-p container 'etree--node)
      nil)))

(cl-defmethod gumshoe--remove-footprint-entries-at ((self etree--tree) position)
  (mapc (apply-partially #'gumshoe--remove-footprint-entry self)
        (gumshoe--footprints-at position)))

;;; filter predicates
(cl-defmethod gumshoe--in-current-buffer-p ((self etree--node))
  "Check if ENTRY in the current perspective."
  (let ((entry (oref self entry)))
    (equal (oref entry buffer) (current-buffer))))

(cl-defmethod gumshoe--in-current-window-p ((self etree--node))
  "Check if ENTRY in the current window."
  (let ((entry (oref self entry)))
    (equal (oref entry window) (get-buffer-window (current-buffer)))))

(cl-defmethod gumshoe--construct-timeline-nodes ((self etree--tree))
  (etree--path self))
(cl-defmethod gumshoe--construct-timeline ((self etree--tree))
  (mapcar (lambda (node) (oref node entry))
          (etree--path self)))

(cl-defmethod gumshoe--add-entry ((self etree--tree) (entry gumshoe--entry))
  "Add ENTRY to SELF"
  (let* ((new-node (etree--node :entry entry))
         (tree (etree--insert self new-node)))
    (cl-assert (and new-node (object-of-class-p new-node 'etree--node)))
    (when (eq gumshoe-footprint-strategy 'delete-overlapping)
       (gumshoe--remove-footprint-entries-at self (point)))
     (let ((overlay (make-overlay (point) (point))))
       (overlay-put overlay 'container new-node)
       (oset entry footprint-overlay overlay))
    ))
(cl-defmethod gumshoe--log-if-necessary ((self etree--tree) &optional alarmp)
  "Check current position and log in RING if significant.

Log automatically if ALARMP is t."
  (unless (cl-some #'funcall gumshoe-ignore-predicates)
    (gumshoe--clean-recent self)
    (let ((new-entry (funcall gumshoe-entry-type)))
      (when (or (not self) (not (oref self current))
                (let ((latest-entry (oref (oref self current) entry)))
                  (and (not (gumshoe--equal new-entry latest-entry))
                       (or alarmp
                           (not (gumshoe--in-current-buffer-p latest-entry))
                           (gumshoe--end-of-leash-p latest-entry)))))
        (gumshoe--add-entry self new-entry)))))

(defun gumshoe--backlog-init (log-len)
  (etree--tree))

(provide 'gumshoe-tree)
