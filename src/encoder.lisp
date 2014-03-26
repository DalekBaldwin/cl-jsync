(in-package :jsync)

(defvar *map* nil
  "Maps object to ID.")
(defvar *class-map* nil)
(defvar *incoming* nil
  "Maps object to number of incoming references, not including the first.")
(defvar *count* nil
  "Number of referenced objects saved so far.")
(defvar *reified* nil
  "Maps object to boolean indicating whether it has already been reified.")
(defvar *memory* nil
  "Map native object to reified object.")
(defvar *schematize-classes* nil
  "Not used... yet.")

(defvar *minify* t)
(defvar *shallow* t)

(with-interface ((stateful:<hash-table> <eql>) stateful:<map>)
  (defun counter ()
    (let ((count 0))
      (lambda (msg)
        (case msg
          (inc (incf count))
          (get count)))))
  (defun mark-incoming (loc)
    (funcall (lookup *incoming* loc) 'inc))
  (defun incoming (loc)
    (funcall (lookup *incoming* loc) 'get))
  (defmethod register (obj)
    (let ((id (incf *count*)))
      (insert *map* obj id)
      (insert *incoming* obj (counter))))
  (defmethod register ((obj standard-class))
    (let ((id (incf *count*)))
      (insert *map* obj id)
      (insert *incoming* obj (counter))
      (insert *class-map* obj id)))
  (defun link (native reified)
    (insert *memory* native reified))
  (defun registered (obj)
    (lookup *map* obj))
  (defun mark-reified (obj)
    (insert *reified* obj t))
  (defun reified-p (obj)
    (lookup *reified* obj))
  )


(defmacro with-marshalling-environment (&body body)
  `(with-interface ((stateful:<hash-table> <eql>) stateful:<map>)
     (let ((*map* (empty))
           (*class-map* (empty))
           (*incoming* (empty))
           (*reified* (empty))
           (*memory* (empty))
           (*count* 0)
           (*print-circle* t))
       ,@body)))


;;; PHASE 1 - TRAVERSAL AND ID REGISTRATION

(defmethod traverse ((obj t))
  (cond
    ((registered obj)
     (mark-incoming obj)) ;; augment-in-degree ?
    (t
     (register obj)
     (traverse-descendants obj))))

(defmethod traverse ((obj null))
  (values))
(defmethod traverse ((obj number))
  (values))
(defmethod traverse ((obj symbol))
  (values))
(defmethod traverse ((obj string))
  ;; we probably should eventually treat strings as objects that can be referred
  ;; to rather than as inline constants like numbers and symbols
  (values))
(defmethod traverse ((obj character))
  (values))

(defmethod traverse-descendants ((obj t))
  (values))

(defmethod traverse-descendants ((obj built-in-class))
  (values))

(defmethod traverse-descendants ((obj standard-class))
  (mapc #'traverse
        (catch 'trap-unbound
          (handler-bind ((unbound-slot
                          #'(lambda (c)
                              (declare (ignore c))
                              (throw 'trap-unbound nil))))
            (closer-mop:class-precedence-list obj)))))

#+nil
(defmethod traverse-descendants ((obj sb-pcl::slot-class))
  (values))



(defmethod traverse-descendants ((obj standard-object))
  (mapc (lambda (slot-def)
               (traverse (slot-value obj (closer-mop:slot-definition-name slot-def))))
             (closer-mop:class-slots (class-of obj))))

(defmethod traverse-descendants ((obj sequence))
  (map nil #'traverse obj))

(defmethod traverse-descendants ((obj cons))
  (traverse (car obj))
  (traverse (cdr obj)))

(defmethod traverse-descendants ((obj hash-table))
  (maphash
   (lambda (key value)
     (traverse key)
     (traverse value))
   obj))


;; for objects with readable print representations, we are done
(defmethod reify ((obj null))
  obj)
(defmethod reify ((obj number))
  obj)
(defmethod reify ((obj symbol))
  (let ((*print-case* :downcase))
    (format nil "~a" obj)))
(defmethod reify ((obj string))
  obj)
(defmethod reify ((obj character))
  obj)

(defmethod reify ((obj built-in-class))
  (class-name obj))

;;(defmethod reify ((obj sb-pcl::slot-object))
;;  (class-name obj))

(defmethod reify-full ((obj standard-class) id)
  (make-instance
   'reified-class
   :id id
   :name (class-name obj)
   :cpl (mapcar
         (lambda (cls)
           (reify cls))
         (catch 'trap-unbound
           (handler-bind ((unbound-slot
                           #'(lambda (c)
                               (declare (ignore c))
                               (throw 'trap-unbound nil))))
             (closer-mop:class-precedence-list obj))))
   :slot-list (mapcar #'closer-mop:slot-definition-name
                      (closer-mop:class-slots obj))))



(defmethod reify-full ((obj standard-object) id)
  (let ((klass (class-of obj)))
    (make-instance
     'reified-object
     :id id
     :object-class
     (let ((*print-case* :downcase))
       (format nil "~a" (class-name klass)))
     :slots
     (asdf::while-collecting (c)
       (dolist (slot-def (closer-mop:class-slots klass))
         ;; ignore :class-allocated slots for now... may need to reify classes?
         (when (eq (closer-mop:slot-definition-allocation slot-def) :instance)
           (c (list
               (let ((*print-case* :downcase))
                 (format nil "~a" (closer-mop:slot-definition-name slot-def)))
               (reify (slot-value obj (closer-mop:slot-definition-name slot-def)))))))))))

(defun simple-list-p (obj)
  ;; could replace this with (every #'atom obj)... not sure about performance implications
  (and (listp obj) (not (listp (car obj)))
       (or (null (cdr obj))
           (simple-list-p (cdr obj)))))

(defmethod reify-full ((obj sequence) id)
  (make-instance
   'reified-sequence
   :id id
   :contained-sequence (map 'list (lambda (s) (reify s)) obj)))

(defmethod reify-full ((obj cons) id)
  (make-instance
   'reified-cons
   :id id
   :contained-car (reify (car obj))
   :contained-cdr (reify (cdr obj)))
  #+nil
  (if (simple-list-p obj)
      (reify-full (coerce obj 'simple-vector) id)
      (make-instance
       'reified-cons
       :id id
       :contained-car (reify (car obj))
       :contained-cdr (reify (cdr obj)))))

(defmethod reify-full ((obj hash-table) id)
  (make-instance
   'reified-hash-table
   :id id
   :test (hash-table-test obj)
   :contained-pairs (asdf::while-collecting (c)
                      (maphash
                       (lambda (key value)
                         (c (reify key))
                         (c (reify value)))
                       obj))))



;; reify eagerly -- define children before we finish defining parents
(defmethod reify ((obj t))
  (let ((id (registered obj)))
    (cond
      ;; get rid of this case and this method might be equivalent to traverse...
      ((zerop (incoming obj))

       (reify-full obj 0)
       ;;(make-instance 'reified-label
       ;;               :id 0
       ;;               :object (reify-full obj))
       )
      ((reified-p obj)
       ;;id
       (make-instance 'reified-reference
                      :id id))
      (*shallow*
       (mark-reified obj)
       (link obj (reify-full obj id))
       (make-instance 'reified-reference
                      :id id))
      (t
       (mark-reified obj)
       (reify-full obj id)
       ;;(make-instance 'reified-label
       ;;               :id id
       ;;               :object (reify-full obj))
       ))))

(defun reify-document (root)
  (let ((id (registered root)))
    (mark-reified root)
    (link root (reify-full root id))))



(defmethod externalize-jsync ((obj reified-reference) stream)
  (format stream "\"*~A\"" (id obj)))

(defmethod externalize-jsync ((obj null) stream)
  (format stream "\"!!null\""))

(defmethod externalize-jsync ((obj number) stream)
  (format stream "~A" obj))

(defmethod externalize-jsync ((obj symbol) stream)
  (format stream "\"~A\"" obj))

(defmethod externalize-jsync ((obj character) stream)
  (format stream "\"~A\"" obj))

(defmethod externalize-jsync ((obj string) stream)
  (format stream
          (if (eq (cl-ppcre:scan "[.]*[!&%*]" obj) 0)
              "\".~A\"" ;; escape JSYNC special character prefixes
              "\"~A\"")
          obj))



(defun format-jsync (stream arg colon at &rest params)
  (declare (ignorable arg colon at params))
  (externalize-jsync arg stream))

;;; write-only code begins here

(defmethod externalize-jsync ((obj reified-cons) stream)
  (format stream
          "\[~[~:;~:*\"&~A\",~]~/cl-jsync:format-jsync/,~/cl-jsync:format-jsync/\]"
          (id obj) (contained-car obj) (contained-cdr obj))
  )

;; composite keys not yet supported
(defmethod externalize-jsync ((obj reified-hash-table) stream)
  (format stream
          (if *minify*
              "\{\"!\":\"!map\",~[~:;~:*\"&\":\"~A\",~]~{~/cl-jsync:format-jsync/:~/cl-jsync:format-jsync/~^,~}\}"
              "\{\"!\":\"!map\",~&~[~:;~:*\"&\":\"~A\",~&~]~{~/cl-jsync:format-jsync/:~/cl-jsync:format-jsync/~^,~&~}\}")
          (id obj) (contained-pairs obj)))
(defmethod externalize-jsync ((obj reified-sequence) stream)
  (format stream
          "\[~[\"!!seq\",~:;~:*\"&~A !!seq\",~]~{~/cl-jsync:format-jsync/~^,~}\]"
          (id obj) (contained-sequence obj)))
(defmethod externalize-jsync ((obj reified-object) stream)
  (format stream
    (if *schematize-classes*
        ;; "slots":["slot value 1", "slot value 2", ...]
        (if *minify*
            "\{\"!\":\"~A\",~[~:;~:*\"&\":\"~A\",~]\"slots\":\[~{~{~*~/cl-jsync:format-jsync/~}~^,~}\]\}"
            "\{\"!\":\"~A\",~&~[~:;~:*\"&\":\"~A\",~&~]\"slots\":\[~{~{~*~/cl-jsync:format-jsync/~}~^,~}\]\}")
        ;; "slot name":"slot value", ...
        (if *minify*
            "\{\"!\":\"~A\",~[~:;~:*\"&\":\"~A\",~]~{~{\"~A\":~/cl-jsync:format-jsync/~}~^,~}\}"
            "\{\"!\":\"~A\",~&~[~:;~:*\"&\":\"~A\",~&~]~{~{\"~A\":~/cl-jsync:format-jsync/~}~^,~&~}\}"))
    (object-class obj) (id obj) (slots obj)))

(defmethod externalize-jsync ((obj reified-class) stream)
  (format stream
          (if *minify*
              "\{\"!\":\"class\",~[~:;~:*\"&\":\"~A\",~]\"cpl\":\[~{~/cl-jsync:format-jsync/~^,~}\],\"slots\":\[~{\"~A\"~^,~}\]\}"
              "\{\"!\":\"class\",~&~[~:;~:*\"&\":\"~A\",~&~]\"cpl\":\[~{~/cl-jsync:format-jsync/~^,~}\],~&\"slots\":\[~{\"~A\"~^,~}\]\}")
          (name obj) (id obj) (cpl obj) (slots obj)))

(defmethod marshal ((obj t))
  (with-marshalling-environment
    (traverse obj)
    (reify obj)
    ))

(defun externalize-jsync-document (stream)
  (format stream
          "[{\"%JSYNC\":\"1.0\"},~%[~{~/cl-jsync:format-jsync/~^,~%~}]]"
          (asdf::while-collecting (c)
            (for-each (stateful:<hash-table> <eql>) *memory*
                      (lambda (pair)
                        (c (cdr pair)))))))



#+nil
(defun encode-jsync (obj &optional stream)
  (externalize-jsync (marshal obj) stream))

(defun encode-jsync (obj &optional stream)
  (with-marshalling-environment
    (traverse obj)
    (reify-document obj)
    (externalize-jsync-document stream)
    ))

(defun encode-jsync-object (obj &optional stream)
  (let ((*shallow* nil))
    (with-marshalling-environment
      (traverse obj)
      (externalize-jsync (reify obj) stream))))

