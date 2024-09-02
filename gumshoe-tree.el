(require 'dash)

(defclass gumshoe--node ()
  (
   (entry :initform (gumshoe--entry)
          :initarg :entry)
   (children :initform nil
             :initarg :children)
   (parent :initform nil
           :initarg :parent))
  "A node in a tree containing a gumshoe entry.")

(defclass gumshoe--tree ()
  (
   (root :initform nil
         :initarg :root
         :documentation "Root of the tree.")
   (current :initform nil
            :initarg :current
            :documentation "Current node in the tree."))
  "A tree to keep track of the target’s movements as they move forward and backward in time.")

(cl-defmethod gumshoe--insert ((self gumshoe--tree) (new gumshoe--node))
  (if (not self)
      (gumshoe--tree self :root new :current new)
    (if (not (oref self root))
        (progn (oset self root new) (oset self current new))
      (let* ((current (oref self current)))
        (oset current children (cons new (oref current children)))
        (oset new parent current)
        (oset self current new)
        )))
  self
  )

(cl-defmethod gumshoe--preorder ((self gumshoe--tree))
  (let* (
         (stk (list (oref self root)))
         (preorder nil)
         (i 0)
         iter
         )
    (while stk
      (setq iter (pop stk))
      (push iter preorder)
      (dolist (child (oref iter children)) (setf stk (cons child stk)))
      (cl-incf i))
    preorder)  )

(cl-defmethod gumshoe--dfs ((self gumshoe--tree) (key gumshoe--node))
  (let* (
         (stk (list (oref self root)))
         (i 0)
         iter
         (continuep t)
         foundp)
    (while (and stk continuep)
      (setq iter (pop stk))
      (dolist (child (oref iter children)) (setf stk (cons child stk)))
      (when (eq iter key)
        (setq continuep nil)
        (setf foundp t))
      (cl-incf i))
    foundp))

(cl-defmethod gumshoe--delete ((self gumshoe--node))
  (when self
    (when (oref self parent)
      (oset (oref self parent) children (oref self children)))
    (dolist (child (oref self children))
      (oset child parent (oref self parent)))))

(cl-defmethod gumshoe--remove ((self gumshoe--tree) (key gumshoe--node))
  (when (eq key (oref self current))
    (oset self current (oref (oref self current) parent)))
  (when (eq key (oref self root))
    (oset self root (car (oref (oref self root) children))))
  (gumshoe--delete key))

(cl-defmethod gumshoe--clean-root ((self gumshoe--tree))
  (let* (
         (timeline (gumshoe--construct-timeline-nodes self))
         iter
         (continuep t))
    (while (and continuep
                timeline)
      (setf iter (car timeline))
      (if (gumshoe--dead-p iter)
          (setf iter (gumshoe--remove self iter))
        (setq continuep nil))
      (setf timeline (cdr timeline))))
  self)

(cl-defmethod gumshoe--clean-recent ((self gumshoe--tree))
  "Cleanup recent dead entries from RING."
  (unless (eq (oref self current) (oref self root))
    (let ((iter (oref self current))
          (continuep t))
      (while (and continuep
                  iter)
        (if (gumshoe--dead-p iter)
            (gumshoe--remove self iter)
          (setq continuep nil))
        (setq iter (oref self current)))
      ))
  )

(cl-defmethod gumshoe--mapc ((self gumshoe--tree) f)
  (let* (
         (stk (list (oref self root)))
         (preorder nil)
         (i 0)
         iter
         )
    (while stk
      (setq iter (pop stk))
      (funcall f iter)
      (push iter preorder)
      (dolist (child (oref iter children)) (setf stk (cons child stk)))
      (cl-incf i))
    preorder))

(cl-defmethod gumshoe--collect ((self gumshoe--tree) predicate)
  (let* (
         (stk (list (oref self root)))
         (preorder nil)
         (i 0)
         iter
         )
    (while (and stk (car stk))
      (setq iter (pop stk))
      (when (funcall predicate iter)
        (push iter preorder))
      (dolist (child (oref iter children)) (setf stk (cons child stk)))
      (cl-incf i))
    preorder))
(cl-defmethod gumshoe--dead-p ((self gumshoe--node))
  (if self
      (gumshoe--dead-p (oref self entry))
    t))
(cl-defmethod gumshoe--clean ((self gumshoe--tree))
  (unless (eq (oref self current) (oref self root))
    (gumshoe--clean-recent self)
    (gumshoe--clean-root self)
    (mapc #'gumshoe--delete
          (gumshoe--collect self 'gumshoe--dead-p)))
  )

(cl-defmethod gumshoe--remove-footprint-entry ((self gumshoe--tree) footprint)
  (gumshoe--remove self (overlay-get footprint 'container))
  (delete-overlay footprint))

(cl-defmethod gumshoe--remove-footprint-entries-at ((self gumshoe--tree) position)
  (mapc (apply-partially #'gumshoe--remove-footprint-entry self)
        (gumshoe--footprints-at position)))

;;; filter predicates
(cl-defmethod gumshoe--in-current-buffer-p ((self gumshoe--node))
  "Check if ENTRY in the current perspective."
  (let ((entry (oref self entry)))
    (equal (oref entry buffer) (current-buffer))))

(cl-defmethod gumshoe--in-current-window-p ((self gumshoe--node))
  "Check if ENTRY in the current window."
  (let ((entry (oref self entry)))
    (equal (oref entry window) (get-buffer-window (current-buffer)))))

(cl-defmethod gumshoe--construct-timeline-nodes ((self gumshoe--tree))
  (let ((stk (list (oref self root)))
        timeline
        (continuep t)
        (i 0)
        (iter (oref self current)))
    (while (and continuep
                iter)
      (push iter timeline)
      (setf iter (oref iter parent))
      (when (eq iter (oref self root)) (setf continuep nil)))
    timeline))
(cl-defmethod gumshoe--construct-timeline ((self gumshoe--tree))
  (mapcar (lambda (node) (oref node entry))
          (gumshoe--construct-timeline-nodes self)))

(cl-defmethod gumshoe--add-entry ((self gumshoe--tree) (entry gumshoe--entry))
  "Add ENTRY to SELF"
  (gumshoe--insert self (gumshoe--node :entry entry))
  (when (eq gumshoe-footprint-strategy 'delete-overlapping)
    (gumshoe--remove-footprint-entries-at self (point)))
  (let ((overlay (make-overlay (point) (point))))
    (overlay-put overlay 'container entry)
    (oset entry footprint-overlay overlay)))
(cl-defmethod gumshoe--log-if-necessary ((self gumshoe--tree) &optional alarmp)
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
  (gumshoe--tree))

(provide 'gumshoe-tree)
