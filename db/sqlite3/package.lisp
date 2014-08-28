(defpackage #:motd-server-sqlite3
  (:use :cl)
  (:export :open-sqlite3-motd-database
           :close-sqlite3-motd-database))
