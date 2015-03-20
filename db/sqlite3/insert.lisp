(in-package #:motd-server-sqlite3)

(defun results-to-message-ids (results)
  (mapcar (lambda (entry)
            (plist-bind (id) entry
              id))
          results))

(defun propose-message (db)
  (dbi:with-transaction (db-handle db)
    (dbi:do-sql (db-handle db) +propose-message-statement+)
    (let* ((results (dbi:execute (get-last-inserted-message-id db)))
           (ids (results-to-message-ids (dbi:fetch-all results))))
      (when ids
        (motd-commands:new-motd-succeeded (first ids))))))

(defun insert-translation (db message-id language text)
  (dbi:do-sql (db-handle db) +insert-translation-statement+
              message-id
              language
              text))

(defun insert-tag (db message-id tag)
  (dbi:do-sql (db-handle db) +insert-tag-statement+
              message-id
              tag))
