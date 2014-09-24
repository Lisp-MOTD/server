(in-package #:motd-server-sqlite3)

(defmethod motd-server:insert-translation ((db sqlite3-motd-db)
                                           message-id
                                           language
                                           text)
  (dbi:do-sql (db-handle db) +insert-translation-statement+
              message-id
              language
              text))
