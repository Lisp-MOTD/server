(require :asdf)

(asdf:defsystem #:motd-server-aserve+sqlite3
  :description "Combination of AllegroServe front-end with sqlite3 back-end."
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20140828"
  :license "unlicense"
  :depends-on (:motd-server :motd-server-aserve :motd-server-sqlite3)
  :components ((:static-file "UNLICENSE")
               (:module "full/aserve+sqlite3"
                :components ((:file "package")
                             (:file "aserve+sqlite3"
                                    :depends-on ("package"))))))
