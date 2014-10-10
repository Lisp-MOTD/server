(in-package #:motd-server-sqlite3)

(defmethod motd-server:remove-translation ((db sqlite3-motd-db)
                                           message-id
                                           language)
  (dbi:do-sql (db-handle db) +remove-translation-statement+
              message-id
              language))

(defmethod motd-server:remove-tag ((db sqlite3-motd-db)
                                   message-id
                                   tag)
  (dbi:do-sql (db-handle db) +remove-tag-statement+
              message-id
              tag))
