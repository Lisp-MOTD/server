(in-package #:motd-server-sqlite3)

(defmethod motd-server:insert-translation ((db sqlite3-motd-db)
                                           message-id
                                           language
                                           text)
  (dbi:do-sql (db-handle db) +insert-translation-statement+
              message-id
              language
              text))

(defmethod motd-server:insert-tag ((db sqlite3-motd-db)
                                   message-id
                                   tag)
  (dbi:do-sql (db-handle db) +insert-tag-statement+
              message-id
              tag))
