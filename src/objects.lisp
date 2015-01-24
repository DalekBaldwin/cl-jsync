(in-package :jsync)

#+nil
(with-interface ((stateful:<hash-table> <equal>) stateful:<map>)
  (defvar *serializable-classes* (empty)))

#+nil
(defmacro def-serializable-class (name direct-superclasses direct-slots &rest options)
  `(with-interface ((stateful:<hash-table> <equal>) stateful:<map>)
     (defclass ,name ,direct-superclasses ,direct-slots  ,@options (:metaclass serializable-metaclass)
               )
     
     (closer-mop:finalize-inheritance (find-class ',name))
     (stateful:insert (stateful:<hash-table> <equal>) *serializable-classes*
             (let ((*print-case* :downcase))
               (format nil "~A" ',name ;(symbol-name ',name)
                                                     ))
             (list ',name
                   (mapcar
                    (lambda (x)
                      (cons
                       (let ((*print-case* :downcase))
                         (format nil "~A"
                                 (closer-mop:slot-definition-name x)
                                 ;;(symbol-name (closer-mop:slot-definition-name x))
                                 ))
                       (closer-mop:slot-definition-name x)))
                    (closer-mop:class-slots (find-class ',name))))
             ;;(let ((*print-case* :downcase)) (format nil "~A") (symbol-name ',name))
             ;;(cons (let ((*print-case* :downcase)) (format nil "~A") (symbol-name ',name))
             ;;    (list ',name (mapcar
             ;;                  (lambda (x)
             ;;                    (cons
             ;;                     (let ((*print-case* :downcase)) (format nil "~A") (symbol-name (slot-name x)))
             ;;                     (slot-name x)))
             ;;                  (closer-mop:class-slots (find-class ',name)))))
           )))

(defmacro def-serializable-class (name direct-superclasses direct-slots &rest options)
  `(defclass ,name ,direct-superclasses ,direct-slots  ,@options (:metaclass serializable-metaclass)))



(defclass serializable-metaclass (standard-class)
  (;; class-allocated slot seems reasonable... but how
   ;; to access it without going through an instance??
   (serializable-classes
    :allocation :class
    :accessor serializable-classes
    :initform (empty (stateful:<hash-table> <equal>)))))

(defmethod closer-mop:validate-superclass
    ((class serializable-metaclass) (superclass standard-class))
  t)



(defclass serializable-slot-definition (closer-mop:standard-slot-definition)
  ())

(defclass serializable-direct-slot-definition
    (closer-mop:standard-direct-slot-definition
     serializable-slot-definition)
  ((serializable
    :initarg :serializable
    :initform t
    :accessor serializable)))

(defclass serializable-effective-slot-definition
    (closer-mop:standard-effective-slot-definition
     serializable-slot-definition)
  ())

(defmethod closer-mop:validate-superclass
    ((class serializable-slot-definition)
     (superclass closer-mop:standard-slot-definition))
  t)

(defvar *effective-slot-definition-class*)

;; p-cos recommends this pattern to work around the lack of modularity
;; in MOP's handling of direct slot definition initargs -- for reference:
;; http://www.rhinocerus.net/forum/lang-lisp/388779-effective-slot-definitions.html
(defmethod closer-mop:effective-slot-definition-class
    ((class serializable-metaclass) &rest initargs)
  *effective-slot-definition-class*
  #+nil
  (if (getf initargs :serializable)
      (find-class 'serializable-effective-slot-definition)
      (call-next-method)))

(defmethod closer-mop:direct-slot-definition-class
    ((class serializable-metaclass) &rest initargs)
  (find-class 'serializable-direct-slot-definition)
  #+nil
  (if (getf initargs :serializable)
      (find-class 'serializable-direct-slot-definition)
      (call-next-method)))

(defmethod closer-mop:compute-effective-slot-definition ((class serializable-metaclass) name dslots)
  (let ((*effective-slot-definition-class*
         (if (every #'serializable dslots)
             (find-class 'serializable-effective-slot-definition)
             (find-class 'closer-mop:standard-effective-slot-definition))))
    (call-next-method))
  #+nil
  (let ((eslot (call-next-method)))
    (if (typep (car dslots) 'serializable-slot-definition)
        (change-class eslot 'serializable-effective-slot-definition :serializable t)
        eslot)
    #+nil
    (or
     (dolist (slotd dslots nil)
       (when (typep slotd 'serializable-slot-definition)
         (return (change-class eslot 'serializable-effective-slot-definition))))
     eslot)))

(defmethod serializable-slots ((class serializable-metaclass))
  (loop for slot in (closer-mop:class-slots class)
     when (typep slot 'serializable-slot-definition)
     collect slot))

(defmethod initialize-instance :after ((class serializable-metaclass) &rest initargs)
  (closer-mop:finalize-inheritance class) ;; necessary?
  (stateful:insert (stateful:<hash-table> <equal>)
                   (serializable-classes
                    (closer-mop:class-prototype
                     (find-class 'serializable-metaclass)))
                   ;;(serializable-classes class)
                   ;;*serializable-classes*
                   (let ((*print-case* :downcase)) (format nil "~A" (class-name class)))
                   (list (class-name class)
                         (mapcar
                          (lambda (x)
                            (cons
                             (let ((*print-case* :downcase))
                               (format nil "~A" (closer-mop:slot-definition-name x)))
                             (closer-mop:slot-definition-name x)))
                          (serializable-slots class)))))








(defclass reified-reference ()
  ((id
    :accessor id
    :initarg :id)))

(defclass reified-sequence ()
  ((id
    :accessor id
    :initarg :id)
   (contained-sequence
    :accessor contained-sequence
    :initarg :contained-sequence)))

(defclass reified-cons ()
  ((id
    :accessor id
    :initarg :id)
   (contained-car
    :accessor contained-car
    :initarg :contained-car)
   (contained-cdr
    :accessor contained-cdr
    :initarg :contained-cdr)))

(defclass reified-hash-table ()
  ((id
    :accessor id
    :initarg :id)
   (test
    :accessor test
    :initarg :test)
   (contained-pairs
    :accessor contained-pairs
    :initarg :contained-pairs)))

(defclass reified-object ()
  ((id
    :accessor id
    :initarg :id)
   
   ;; string with name of class -- might change to actual (reified?) class metaobject
   (object-class
    :accessor object-class
    :initarg :object-class)
   (slots
    :accessor slots
    :initarg :slots)))

(defclass reified-class ()
  ((id
    :accessor id
    :initarg :id)
   (name
    :accessor name
    :initarg :name)
   (cpl
    :accessor cpl
    :initarg :cpl)
   (slot-list
    :accessor slot-list
    :initarg :slot-list)))

