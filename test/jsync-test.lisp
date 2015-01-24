(in-package :jsync-test)

(defsuite* test-all)

;; The universality of this expression implies consistency as a middleman in a
;; JSYNC ecosystem
#+nil
(string= (encode-jsync (decode-jsync (encode-jsync *object*)))
         (encode-jsync (decode-jsync (encode-jsync (decode-jsync (encode-jsync *object*))))))

;; There will be cases where this is true if the order of traversal of unordered
;; composite objects does not match the order in which their components are nativized
#+nil
(and
 (not (string= (encode-jsync *object*)
               (encode-jsync (decode-jsync (encode-jsync *object*)))))
 (string= (encode-jsync (decode-jsync (encode-jsync *object*)))
              (encode-jsync (decode-jsync (encode-jsync (decode-jsync (encode-jsync *object*)))))))

;; ... Does that all make sense?



(def-serializable-class a-lisp-object ()
  ((a-slot
    :accessor a-slot
    :initarg :a-slot)
   (another-slot
    :accessor another-slot
    :initarg :another-slot)))

(deftest test-circular-list ()
  (let ((circle (encode-jsync-object '#1=(1 2 . #1#))))
    (is (string= circle
                 (encode-jsync-object (decode-jsync circle))))))

(deftest test-there-and-back-again ()
  (let ((the-object (make-instance 'a-lisp-object)))
    (setf (a-slot the-object) '#1=(1 2 . #1#))
    (setf (another-slot the-object) the-object)
    (let ((jsync (encode-jsync-object the-object)))
      (is (string= jsync
                   (encode-jsync-object (decode-jsync jsync)))))))
