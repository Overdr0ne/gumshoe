(require 'ring)
(require 'eieio)
(require 'cl-lib)

(cl-defmethod gumshoe--delete (ring index)
  "Delete entry at INDEX from RING."
  (let ((entry (ring-ref ring index)))
    (delete-overlay (slot-value entry 'overlay)))
  (ring-remove ring index))

(cl-defmethod gumshoe--clean-recent (ring)
  "Cleanup recent dead entries from RING."
  (unless (ring-empty-p ring)
    (let ((i 0)
          (continuep t))
      (while (and continuep
                  (< i (ring-length ring)))
        (let ((entry (ring-ref ring i)))
          (if (context--dead-p entry)
              (gumshoe--delete ring i)
            (setq continuep nil)))))))

(cl-defmethod gumshoe--clean (ring)
  "Cleanup dead entries from RING."
  (unless (ring-empty-p ring)
    (let ((i 0))
      (while (< i (ring-length ring))
        (let ((entry (ring-ref ring i)))
          (if (context--dead-p entry)
              (gumshoe--delete ring i)
            (cl-incf i)))))))

(cl-defmethod gumshoe--remove-footprint-entry (ring footprint)
  (ring-remove ring
               (ring-member ring
                            (overlay-get footprint 'container)))
  (delete-overlay footprint))

(cl-defmethod gumshoe--remove-footprint-entries-at (position ring)
  (mapc (apply-partially #'gumshoe--remove-footprint-entry ring)
        (gumshoe--footprints-at position)))

(cl-defmethod gumshoe--add-entry (ring (entry context))
  "Add entry to the RING."
  (ring-insert ring entry)
  (when (eq gumshoe-footprint-strategy 'delete-overlapping)
    (gumshoe--remove-footprint-entries-at (point) ring))
  (let ((overlay (make-overlay (point) (point) (current-buffer))))
    (overlay-put overlay 'container entry)
    (oset entry overlay overlay)
    (message "SAMSAM add entry overlay %s" overlay)
    )
  (message "SAMSAM add entry overlay after %s" (oref entry overlay))
  )
(cl-defmethod gumshoe--log-if-necessary (ring &optional alarmp)
  "Check current position and log in RING if significant.

Log automatically if ALARMP is t."
  (unless (cl-some #'funcall gumshoe-ignore-predicates)
    (gumshoe--clean-recent ring)
    (let ((new-entry (funcall gumshoe-entry-type)))
      (when (or (ring-empty-p ring)
                (let ((latest-entry (ring-ref ring 0)))
                  (and (not (context--equal new-entry latest-entry))
                       (or alarmp
                           (not (context--in-current-buffer-p latest-entry))
                           (gumshoe--end-of-leash-p latest-entry)))))
        (gumshoe--add-entry ring new-entry)))))

(defun gumshoe--backlog-init (log-len)
  (make-ring gumshoe-log-len))

(defun gumshoe--construct-timeline (backlog)
  (ring-elements backlog))

(provide 'gumshoe-ring)
