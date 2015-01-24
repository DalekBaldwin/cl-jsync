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
            :components ((:file "package")
                         (:file "objects")
                         (:file "encoder")
                         (:file "decoder"))
            :serial t))
  :depends-on (:cl-ppcre :lisp-interface-library :cl-json :closer-mop))

(defsystem :cl-jsync-test
  :components
  ((:module :test
            :components ((:file "package")
                         (:file "jsync-test" :depends-on ("package")))))
  :depends-on (:cl-jsync :stefil))

