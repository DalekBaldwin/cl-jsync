(in-package :cl)

(pushnew :cl-jsync *features*)

#+nil
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (named-readtables:find-readtable :cl-jsync)
    (named-readtables:defreadtable :cl-jsync
      (:merge :standard)
      (:case :invert))))

#+nil
(named-readtables:in-readtable :cl-jsync)

(defpackage :cl-jsync
  (:nicknames :jsync)
  (:use :cl :interface)
  (:export
   #:encode-jsync
   #:encode-jsync-object
   #:decode-jsync
   #:def-serializable-class))
