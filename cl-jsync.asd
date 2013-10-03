;;;; -*- lisp -*-

(require "asdf")
(defpackage :jsync-system
  (:use :cl :asdf))
(in-package :jsync-system)

(defsystem :cl-jsync
  :name "cl-jsync"
  :description "JSYNC in Lisp. JSYNC (JSon Yaml eNCoding) is a data-interchange format that provides the full graph-serialization power of YAML with a strictly JSON-compliant syntax."
  :author "Dalek Baldwin"
  :maintainer "Dalek Baldwin"
  :licence "BSD"
  :components
  ((:static-file "cl-jsync.asd")
   (:module :src
            ;;:serial t
            :components ((:file "package")
                         (:file "objects" :depends-on ("package"))
                         (:file "encoder" :depends-on ("objects"))
                         (:file "decoder" :depends-on ("objects")))))
  :depends-on (:cl-ppcre :lisp-interface-library :cl-json :closer-mop))

(defsystem :cl-jsync-test
  :depends-on (:cl-jsync :stefil)
  :components
  ((:module :test
            :components ((:file "package")
                         (:file "jsync-test" :depends-on ("package"))))))

