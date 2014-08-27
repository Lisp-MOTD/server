(in-package #:motd-server-sqlite3)

(defmethod motd-server:retrieve-all-motds-after ((db sqlite3-motd-db)
                                                 message-id)
  (let ((results (dbi:execute (retrieve-after db) message-id)))
    (dbi:fetch-all results)))

(defmethod motd-server:retrieve-most-recent-motds ((db sqlite3-motd-db)
                                                   how-many-to-retrieve)
  (let ((results (dbi:execute (retrieve-recent db) how-many-to-retrieve)))
    (dbi:fetch-all results)))

(defmethod motd-server:retrieve-all-proposed-motds ((db sqlite3-motd-db))
  (let ((results (dbi:execute (retrieve-proposed db))))
    (dbi:fetch-all results)))

(defmethod motd-server:retrieve-all-tags ((db sqlite3-motd-db))
  (let ((results (dbi:execute (retrieve-tags db))))
    (dbi:fetch-all results)))
