(require :asdf)

(asdf:defsystem #:motd-server-test
  :description "Unit tests for Lisp Message of the Day (Common Lisp Client)"
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20140819"
  :license "unlicense"
  :depends-on (#:nst #:motd-server)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "run-all" :depends-on ("package"))))))
