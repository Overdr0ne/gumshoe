(require 'ring)
(require 'eieio)
(require 'cl-generic)
(require 'cl-lib)
(require 'context)
(require 'gumshoe-lib)

(defclass gumshoe--ring ()
  ((ring :initform nil
         :initarg :ring
         :documentation "The underlying Emacs ring buffer."))
  "Wrapper class for Emacs ring buffer to enable method dispatch.")

(cl-defmethod gumshoe--delete ((self gumshoe--ring) index)
  "Delete entry at INDEX from SELF and clean up its overlay."
  (with-slots (ring) self
    (let ((entry (ring-ref ring index)))
      (when (object-of-class-p entry 'context)
        (context--cleanup entry))
      (ring-remove ring index))))

(cl-defmethod gumshoe--clean-recent ((self gumshoe--ring))
  "Cleanup recent dead entries from SELF."
  (with-slots (ring) self
    (unless (ring-empty-p ring)
      (let ((i 0)
            (continuep t))
        (while (and continuep
                    (< i (ring-length ring)))
          (let ((entry (ring-ref ring i)))
            (if (context--dead-p entry)
                (gumshoe--delete self i)
              (setq continuep nil))))))))

(cl-defmethod gumshoe--clean ((self gumshoe--ring))
  "Cleanup dead entries from SELF."
  (with-slots (ring) self
    (unless (ring-empty-p ring)
      (let ((i 0))
        (while (< i (ring-length ring))
          (let ((entry (ring-ref ring i)))
            (if (context--dead-p entry)
                (gumshoe--delete self i)
              (cl-incf i))))))))

(cl-defmethod gumshoe--remove-footprint-entry ((self gumshoe--ring) footprint)
  "Remove FOOTPRINT entry from SELF."
  (with-slots (ring) self
    (unless (ring-empty-p ring)
      (let* ((container (overlay-get footprint 'container))
             (index (ring-member ring container)))
        (when index
          (delete-overlay footprint)
          (ring-remove ring index))))))

(cl-defmethod gumshoe--remove-footprint-entries-at ((self gumshoe--ring) position)
  "Remove footprint entries at POSITION from SELF."
  (mapc (apply-partially #'gumshoe--remove-footprint-entry self)
        (gumshoe--footprints-at position)))

(cl-defmethod gumshoe--add-entry ((self gumshoe--ring) (entry context))
  "Add ENTRY to SELF."
  (with-slots (ring) self
    (ring-insert ring entry)))

(cl-defmethod gumshoe--log-if-necessary ((self gumshoe--ring) &optional alarmp)
  "Check current position and log in SELF if significant.

Log automatically if ALARMP is t."
  (with-slots (ring) self
    (unless (cl-some #'funcall gumshoe-ignore-predicates)
      (when (eq gumshoe-footprint-strategy 'delete-overlapping)
        (gumshoe--remove-footprint-entries-at self (point)))
      (gumshoe--clean-recent self)
      (let ((new-entry (gumshoe--make-entry)))
        (when (or (ring-empty-p ring)
                  (let ((latest-entry (ring-ref ring 0)))
                    (and (not (context--equal new-entry latest-entry))
                         (or alarmp
                             (not (context--in-current-buffer-p latest-entry))
                             (gumshoe--end-of-leash-p latest-entry)))))
          (gumshoe--add-entry self new-entry))))))

(cl-defmethod gumshoe--log ((self gumshoe--ring))
  "Manually log current position in SELF as a marker."
  (unless (cl-some #'funcall gumshoe-ignore-predicates)
    (let ((new-entry (gumshoe--make-entry)))
      (oset new-entry category "marker")
      (gumshoe--add-entry self new-entry))))

(defun gumshoe--backlog-init (log-len)
  "Create a new gumshoe--ring with LOG-LEN capacity."
  (gumshoe--ring :ring (make-ring log-len)))

(cl-defmethod gumshoe--construct-timeline ((self gumshoe--ring))
  "Construct timeline from SELF."
  (with-slots (ring) self
    (ring-elements ring)))

(provide 'gumshoe-ring)
