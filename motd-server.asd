(require :asdf)

(asdf:defsystem #:motd-server
  :description "Lisp Message of the Day (Common Lisp Server)"
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20140825"
  :license "unlicense"
  :in-order-to ((asdf:test-op (asdf:load-op :motd-server-test)))
  :perform (asdf:test-op (o c)
             (uiop/package:symbol-call :motd-server-test :run-all-tests))
  :depends-on (:motd-commands :interface :ironclad :cl-base64 :trivial-utf-8)
  :components ((:static-file "README.md")
               (:static-file "UNLICENSE")
               (:module "core"
                :components ((:file "package")
                             (:file "db" :depends-on ("package"))
                             (:file "api" :depends-on ("package"))
                             (:file "types" :depends-on ("package"
                                                         "api"))
                             (:file "authenticate" :depends-on ("package"
                                                                "db"
                                                                "api"))
                             (:file "retrieve" :depends-on ("package"
                                                            "db"
                                                            "api"))
                             (:file "insert" :depends-on ("package"
                                                          "db"
                                                          "api"))
                             (:file "remove" :depends-on ("package"
                                                          "db"
                                                          "api"))))))
