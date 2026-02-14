;;; etree.el --- A library for working with trees    -*- lexical-binding: t; -*-

;; Copyright (C) 2025 overdr0ne

;; Author: overdr0ne
;; Version: 4.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: internal, convenience, convenience
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

;; This is a library for working with trees in a mutable, object-oriented
;; style.

;;; Code:

(require 'eieio)
(require 'cl-generic)

;; Generic cleanup method for entries stored in tree nodes
;; Clients can specialize this for their own entry types if cleanup is needed
(cl-defgeneric etree--cleanup (_entry)
  "Clean up resources held by ENTRY.
Default implementation does nothing. Specialize this method for entry types
that require cleanup (e.g., overlays, processes, buffers)."
  nil)

(defclass etree--node ()
  (
   (entry :initform nil
          :initarg :entry)
   (children :initform nil
             :initarg :children)
   (parent :initform nil
           :initarg :parent))
  "A node in a tree.")

(defclass etree--tree ()
  (
   (root :initform nil
         :initarg :root
         :documentation "Root of the tree.")
   (current :initform nil
            :initarg :current
            :documentation "Current node in the tree."))
  "A tree to keep track of the target’s movements as they move forward and backward in time.")

(cl-defmethod etree--insert ((self etree--tree) (new etree--node))
  (if (not self)
      (etree--tree self :root new :current new)
    (if (not (oref self root))
        (progn (oset self root new) (oset self current new))
      (let* ((current (oref self current)))
        (oset current children (cons new (oref current children)))
        (oset new parent current)
        (oset self current new)
        )))
  self
  )

(cl-defmethod etree--preorder ((self etree--tree))
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

(cl-defmethod etree--dfs ((self etree--tree) (key etree--node))
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

(cl-defmethod etree--delete ((self etree--node))
  "Delete SELF from tree and clean up entry resources."
  (when self
    (let ((entry (oref self entry)))
      (when entry
        (etree--cleanup entry)))
    (when (oref self parent)
      (oset (oref self parent) children (oref self children)))
    (dolist (child (oref self children))
      (oset child parent (oref self parent)))))

(cl-defmethod etree--remove ((self etree--tree) (key etree--node))
  (when (eq key (oref self current))
    (oset self current (oref (oref self current) parent)))
  (when (eq key (oref self root))
    (oset self root (car (oref (oref self root) children))))
  (etree--delete key))

(cl-defmethod etree--mapc ((self etree--tree) f)
  (let* ((stk (list (oref self root)))
         (preorder nil)
         (i 0)
         iter)
    (while stk
      (setq iter (pop stk))
      (funcall f iter)
      (push iter preorder)
      (dolist (child (oref iter children)) (setf stk (cons child stk)))
      (cl-incf i))
    preorder))

(cl-defmethod etree--collect ((self etree--tree) predicate)
  (let* ((stk (list (oref self root)))
         (preorder nil)
         (i 0)
         iter)
    (while (and stk (car stk))
      (setq iter (pop stk))
      (when (funcall predicate iter)
        (push iter preorder))
      (dolist (child (oref iter children)) (setf stk (cons child stk)))
      (cl-incf i))
    preorder))

(cl-defmethod etree--path ((self etree--tree))
  "Return the path from the root to the current node."
  (let (path
        (continuep t)
        (iter (oref self current)))
    (while (and continuep
                iter)
      (push iter path)
      (setf iter (oref iter parent))
      (when (eq iter (oref self root)) (setf continuep nil)))
    path))

(provide 'etree)
;;; etree.el ends here
