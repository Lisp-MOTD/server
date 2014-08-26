(in-package #:motd-server-sqlite3)


(defclass sqlite3-motd-db (motd-server:motd-db)
  ((db-handle :reader db-handle
              :writer %db-handle
              :documentation "Handle to the sqlite3 database connection."))
  (:documentation "Sqlite3 subclass of MOTD-DB."))

(defmethod initialize-instance :after ((db sqlite3-motd-db)
                                       &key
                                         (db-name #P"./motd.sqlite3")
                                         &allow-other-keys)
  (let ((handle (dbi:connect :sqlite3
                             :database-name db-name)))
    ;; TODO: figure out how to make sure this is serialized
    (%db-handle handle db)))
