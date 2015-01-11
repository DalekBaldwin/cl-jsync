(defpackage #:cl-jsync-test
  (:use :jsync :cl :stefil)
  (:nicknames #:jsync-test)
  (:export
   #:test-all
   #:test-there-and-back-again
   #:test-circular-list
   )
  #+nil ;; may want to do more internal tests later
  (:import-from :jsync
                
                ;; encoder.lisp
                :counter
                :mark-incoming
                :incoming
                :register
                :link
                :registered
                :mark-reified
                :reified-p
                :with-marshalling-environment
                :traverse
                :traverse-descendants
                :reify
                :reify-full
                :simple-list-p
                :reify-document
                :externalize-jsync
                :format-jsync
                :marshal
                :externalize-jsync-document
                
                ;; decoder.lisp
                :alist-p
                :jsync-object-p
                :check-jsync-header
                :internalize-jsync
                :internalize-string
                :check-list-header
                :get-tag-from-list-header
                :get-id-from-list-header
                :internalize-list
                :with-validation-environment
                :validate-internalized-jsync
                :with-construction-environment
                :nativize
                :assemble
                
                ;; objects.lisp
                :serializable-slots
                :id
                :reified-reference
                :reified-sequence
                :contained-sequence
                :reified-cons
                :contained-car
                :contained-cdr
                :reified-hash-table
                :test
                :contained-pairs
                :reified-object
                :object-class
                :slots
                :reified-class
                :name
                :cpl
                :slot-list)
  )
