(require :asdf)

(asdf:defsystem #:motd-server
  :description "Lisp Message of the Day (Common Lisp Server)"
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20140825"
  :license "unlicense"
  :depends-on (:aserve :split-sequence)
  :components ((:static-file "README.md")
               (:static-file "UNLICENSE")
               (:file "package")

               (:module "db"
                :depends-on ("package")
                :components ((:file "interface")))

               (:module "front-end"
                :depends-on ("package")
                :components ((:file "interface")
                             (:file "aserve" :depends-on ("interface"))))

               (:file "authenticate" :depends-on ("package"
                                                  "db"
                                                  "front-end"))
               (:file "retrieve" :depends-on ("package"
                                              "db"
                                              "front-end"))))

(defmethod asdf:perform ((op asdf:test-op)
                         (system (eql (asdf:find-system '#:motd-server))))
  (asdf:load-system '#:motd-server-test)
  (funcall (find-symbol "RUN-ALL-TESTS" '#:motd-server-test)))

(asdf:defsystem #:motd-server-test
  :description "Unit tests for Lisp Message of the Day (Common Lisp Client)"
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20140819"
  :license "unlicense"
  :depends-on (#:nst #:motd-server)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "run-all" :depends-on ("package"))))))
