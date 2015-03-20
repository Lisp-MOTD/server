(require :asdf)

(asdf:defsystem #:motd-server-sqlite3
  :description "Sqlite3 backend support for the Lisp Message of the Day server."
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20140826"
  :license "unlicense"
  :depends-on (:motd-server :interface :dbd-sqlite3 :dbi)
  :components ((:static-file "UNLICENSE")
               (:module "db/sqlite3"
                :components ((:file "package")
                             (:file "util" :depends-on ("package"))
                             (:file "queries" :depends-on ("package"))
                             (:file "sqlite3" :depends-on ("package"
                                                           "queries"))
                             (:file "retrieve" :depends-on ("package"
                                                            "util"
                                                            "sqlite3"))
                             (:file "insert" :depends-on ("package"
                                                          "util"
                                                          "sqlite3"
                                                          "retrieve"))
                             (:file "remove" :depends-on ("package"
                                                          "util"
                                                          "sqlite3"))
                             (:file "impl" :depends-on ("package"
                                                        "sqlite3"
                                                        "insert"
                                                        "retrieve"
                                                        "remove"))))))
