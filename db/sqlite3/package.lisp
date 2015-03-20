(defpackage #:motd-server-sqlite3
  (:use :cl)
  (:export :=sqlite3-motd-db=)
  (:import-from :motd-server :make-=motd-db=-implementation))
