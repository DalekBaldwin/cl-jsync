(in-package :jsync)

(defvar *map* nil)
(defvar *count* nil)

(defun alist-p (obj)
  (and (listp obj)
       (every #'consp obj)))

(defun jsync-object-p (obj)
  (and (listp obj)
       (every #'consp obj)
       (assoc '! obj)))

(defun check-jsync-header (parsed)
  (equal parsed
         '(%+JSYNC+ . "1.0")))

(defmethod internalize-jsync ((parsed null))
  nil)

(defmethod internalize-jsync (parsed)
  parsed)


(defmethod internalize-jsync ((parsed list))
  (let* ((node-type (and (every #'consp parsed) (assoc '! parsed)))
         (node-id (and node-type (assoc '& parsed))))
    (cond
      ((equal (cdr node-type) "!map")
       (make-instance 'reified-hash-table
                      :id (if node-id (cdr node-id) 0)
                      :contained-pairs
                      (asdf::while-collecting (c)
                        (dolist (pair (remove node-type (remove node-id parsed)))
                          ;; treat like a slot for now
                          (let ((*print-case* :downcase))
                            (c (format nil "~a" (car pair))))
                          (c (internalize-jsync (cdr pair)))))))
      ((null node-type)
       (internalize-list parsed))
      (t
       (make-instance 'reified-object
                      :object-class (cdr node-type)
                      :id (if node-id (cdr node-id) 0)
                      :slots
                      (mapcar
                       (lambda (pair)
                         (cons
                          (let ((*print-case* :downcase))
                            ;; will remove leading colon from keyword symbols
                            (format nil "~a" (car pair)))
                          (internalize-jsync (cdr pair))))
                       (remove node-type (remove node-id parsed))))))))

(defun internalize-string (parsed)
  (if (eq (cl-ppcre:scan "[.]+[!&%*]" parsed) 0)
      (subseq parsed 1)
      parsed))

(defmethod internalize-jsync ((parsed string))
  (cond
    ((zerop (length parsed))
     nil)
    ((eq (elt parsed 0) #\*)
     (make-instance 'reified-reference
                    :id (subseq parsed 1)))
    ((string= parsed "!!null")
     nil)
    (t (internalize-string parsed))))

(defun check-list-header (header)
  (and (stringp header)
       (or (null (cl-ppcre:split "![^ ]+ &[^ ]+" header))
           (null (cl-ppcre:split "&[^ ]+ ![^ ]+" header))
           (null (cl-ppcre:split "![^ ]+" header))
           (null (cl-ppcre:split "&[^ ]+" header)))
       ;;(eq (elt header 0) #\&)
       ))

(defun get-tag-from-list-header (header)
  (car (cl-ppcre:all-matches-as-strings "![^ ]+" header)))

(defun get-id-from-list-header (header)
  (car (cl-ppcre:all-matches-as-strings "&[^ ]+" header)))

(defun internalize-list (parsed)
  (cond
    #+nil
    ((null parsed)
     parsed)
    ((null (cdr parsed))
     (internalize-jsync (car parsed)))
    ((check-list-header (car parsed))
     (let ((tag (get-tag-from-list-header (car parsed)))
           (id (get-id-from-list-header (car parsed))))
       (if (string= tag "!!seq")
           (make-instance 'reified-sequence
                          :id (if id (subseq id 1) 0)
                          :contained-sequence
                          (mapcar #'internalize-jsync (cdr parsed)))
           (make-instance 'reified-cons
                          :id (if id (subseq id 1) 0)
                          :contained-car (internalize-jsync (cadr parsed))
                          :contained-cdr (internalize-jsync (cddr parsed))))))
    (t
     (make-instance 'reified-cons
                    :id 0
                    :contained-car (internalize-jsync (car parsed))
                    :contained-cdr (internalize-jsync (cdr parsed))))))


(defmacro with-validation-environment (&body body)
  `(with-interface ((stateful:<hash-table> <equal>) stateful:<map>)
     (let ((*map* (empty))
           (*count* 0)
           (*print-circle* t))
       ,@body)))

(with-interface ((stateful:<hash-table> <equal>) stateful:<map>)
  (defmethod validate-internalized-jsync ((obj null))
    obj)
  (defmethod validate-internalized-jsync ((obj string))
    obj)
  (defmethod validate-internalized-jsync ((obj number))
    obj)
  (defmethod validate-internalized-jsync (obj)
    (let* ((id (id obj))
           (mapping (lookup *map* id)))
      (cond
        (mapping
         (error "Object with id ~A already internalized." id))
        ((and (numberp id) (zerop id))
         (validate-descendants obj))
        (t
         (insert *map* id obj)
         (setf (id obj) (incf *count*))
         (validate-descendants obj)))))
  (defmethod validate-internalized-jsync ((obj reified-reference))
    (let ((mapping (lookup *map* (id obj))))
      (if mapping
          (setf (id obj) (id mapping))
          (error "Object with id ~A not internalized." (id obj))))
    obj)
  (defmethod validate-descendants ((obj reified-object))
    (dolist (slot (slots obj))
      (setf (cdr slot)
            (list (validate-internalized-jsync (cdr slot)))))
    obj)

  (defmethod validate-descendants ((obj reified-sequence))
    (mapc #'validate-internalized-jsync (contained-sequence obj))
    ;;(setf (contained-sequence obj)
    ;;      (mapcar #'validate-internalized-jsync (contained-sequence obj)))
    ;;(map nil #'validate-internalized-jsync (contained-sequence obj))
    obj
    )
  (defmethod validate-descendants ((obj reified-cons))
    ;;(validate-internalized-jsync (contained-car obj))
    ;;(validate-internalized-jsync (contained-cdr obj))
    (setf (contained-car obj) (validate-internalized-jsync (contained-car obj)))
    (setf (contained-cdr obj) (validate-internalized-jsync (contained-cdr obj)))
    obj
    )
  (defmethod validate-descendants ((obj reified-hash-table))
    
    )
  )

(defmacro with-construction-environment (&body body)
  `(with-interface ((stateful:<hash-table> <eql>) stateful:<map>)
     (let ((*map* (empty))
           (*print-circle* t))
       ,@body)))

(with-interface ((stateful:<hash-table> <eql>) stateful:<map>)
  (defmethod nativize ((obj reified-cons))
    (let ((nativized
           (cons (nativize (contained-car obj))
                 (nativize (contained-cdr obj)))))
      (insert *map* (id obj) nativized)
      nativized))


  (defmethod nativize ((obj reified-sequence))
    (let ((nativized
           (map 'vector #'nativize (contained-sequence obj))
            ;;(mapcar #'nativize (contained-sequence obj))
            ))
      (insert *map* (id obj) nativized)
      nativized))

  (defmethod nativize ((obj reified-hash-table))
    (let ((table (make-hash-table))
          (pairs (contained-pairs obj)))
      
      (values) ;; do something eventually...
      ))
  
  (defmethod nativize (obj)
    ;; crucially, this includes reified-references
    obj)

  (defmethod assemble ((obj reified-reference))
    (lookup *map* (id obj))))

(defmethod nativize ((obj reified-object))
  (let* ((class-info (stateful:lookup
                      (stateful:<hash-table> <equal>)
                      ;;*serializable-classes*
                      (serializable-classes
                       (closer-mop:class-prototype (find-class 'serializable-metaclass)))
                      (object-class obj)))
         (nativized
          (make-instance (car class-info))))
    (dolist (slot (slots obj))
      (setf (slot-value nativized
                        (cdr (assoc (car slot) (cadr class-info)
                                    :test #'equal)))
            (nativize (cadr slot))))
    (stateful:insert (stateful:<hash-table> <eql>) *map* (id obj) nativized)
    nativized))

(defmethod assemble ((obj cons))
  (if (eq (type-of (car obj)) 'reified-reference)
    (setf (car obj) (assemble (car obj)))
    (assemble (car obj)))
  (if (eq (type-of (cdr obj)) 'reified-reference)
    (setf (cdr obj) (assemble (cdr obj)))
    (assemble (cdr obj)))
  obj)

(defmethod assemble ((obj standard-object))
  (dolist (slot-def (closer-mop:class-slots (class-of obj)))
    (let* ((name (closer-mop:slot-definition-name slot-def))
           (value (slot-value obj name)))
      (if (eq (type-of value) 'reified-reference)
          (setf (slot-value obj name) (assemble value))
          (assemble value))))
  obj)


(defmethod assemble ((obj vector))
  (loop for i from 0
       for e across obj
       do (setf (elt obj i) (assemble e)))
  obj)

(defmethod assemble (obj)
  obj)

(defun decode-jsync (jsync)
  (with-construction-environment
    (assemble
        (nativize
         (with-validation-environment
           (validate-internalized-jsync
            (internalize-jsync (with-input-from-string (in jsync)
                                 (let ((json:*json-symbols-package* :cl-jsync))
                                   (json:decode-json in))))))))))
