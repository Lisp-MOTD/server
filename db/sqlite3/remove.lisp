(in-package #:motd-server-sqlite3)

(defun remove-translation (db message-id language)
  (dbi:do-sql (db-handle db) +remove-translation-statement+
              message-id
              language))

(defun remove-tag (db message-id tag)
  (dbi:do-sql (db-handle db) +remove-tag-statement+
              message-id
              tag))
