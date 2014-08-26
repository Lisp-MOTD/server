(require :asdf)

(asdf:defsystem #:motd-server-aserve
  :description "Front end for the MOTD-SERVER which uses AllegroServe."
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20140826"
  :license "unlicense"
  :depends-on (:motd-server :aserve :split-sequence)
  :components ((:static-file "UNLICENSE")
               (:module "front-end/aserve"
                :components ((:file "package")
                             (:file "aserve" :depends-on ("package"))))))
