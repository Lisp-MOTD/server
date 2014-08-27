(in-package #:motd-server-sqlite3)

(defclass sqlite3-motd-db (motd-server:motd-db)
  ((db-handle :reader db-handle
              :writer %db-handle
              :documentation "Handle to the sqlite3 database connection.")
   (retrieve-after :reader retrieve-after
                   :writer %retrieve-after
                   :documentation "Prepared statement for retrieving
                   all messages newer than a given message.")
   (retrieve-recent :reader retrieve-recent
                   :writer %retrieve-recent
                   :documentation "Prepared statement for retrieving
                   the most recent messages.")
   (retrieve-proposed :reader retrieve-proposed
                   :writer %retrieve-proposed
                   :documentation "Prepared statement for retrieving
                   the proposed messages.")
   (retrieve-tags :reader retrieve-tags
                  :writer %retrieve-tags
                  :documentation "Prepared statement for retrieving
                  all of the tags currently in use."))
  (:documentation "Sqlite3 subclass of MOTD-DB."))

(defconstant +retrieve-after-statement+
  "SELECT msg.id, msg.expiration,
          pub.timestamp,
          info.language, info.text, info.tag
     FROM messages AS msg
     JOIN published_messages AS pub ON msg.id = pub.message_id
     JOIN (
        SELECT message_id, language, text, NULL AS tag
          FROM message_translations AS trn
        UNION
        SELECT message_id, NULL AS language, NULL AS text, tag
          FROM message_tags AS tag
     ) AS info ON msg.id = info.message_id
     WHERE msg.id IN ( SELECT p1.message_id
                         FROM published_messages AS p1
                         WHERE timestamp > ( SELECT timestamp
                                               FROM published_messages AS p2
                                               WHERE p2.message_id = ? ) )")

(defconstant +retrieve-recent-statement+
  "SELECT msg.id, msg.expiration,
          pub.timestamp,
          info.language, info.text, info.tag
     FROM messages AS msg
     JOIN published_messages AS pub ON msg.id = pub.message_id
     JOIN (
        SELECT message_id, language, text, NULL AS tag
          FROM message_translations AS trn
        UNION
        SELECT message_id, NULL AS language, NULL AS text, tag
          FROM message_tags AS tag
     ) AS info ON msg.id = info.message_id
     WHERE msg.id IN ( SELECT message_id
                         FROM published_messages
                         ORDER BY timestamp DESC
                         LIMIT ? )")

(defconstant +retrieve-proposed-statement+
  "SELECT msg.id, msg.expiration,
          info.language, info.text, info.tag
     FROM messages AS msg
     JOIN (
        SELECT message_id, language, text, NULL AS tag
          FROM message_translations AS trn
        UNION
        SELECT message_id, NULL AS language, NULL AS text, tag
          FROM message_tags AS tag
     ) AS info ON msg.id = info.message_id
     WHERE msg.id NOT IN ( SELECT message_id
                             FROM published_messages )")

(defconstant +retrieve-tags-statement+
  "SELECT DISTINCT tag FROM message_tags")

(defmethod initialize-instance :after ((db sqlite3-motd-db)
                                       &key
                                         (db-name #P"./motd.sqlite3")
                                         &allow-other-keys)
  (let ((handle (dbi:connect :sqlite3
                             :database-name db-name)))
    ;; TODO: figure out how to make sure this is serialized
    (%db-handle handle db)
    (%retrieve-after (dbi:prepare handle +retrieve-after-statement+) db)
    (%retrieve-recent (dbi:prepare handle +retrieve-recent-statement+) db)
    (%retrieve-proposed (dbi:prepare handle +retrieve-proposed-statement+) db)
    (%retrieve-tags (dbi:prepare handle +retrieve-tags-statement+) db)
    (ensure-schema-loaded db)))

(defparameter *schema*
  (list
   "CREATE TABLE IF NOT EXISTS messages (
      id INTEGER PRIMARY KEY ASC AUTOINCREMENT,
      expiration INTEGER
    )"

   "CREATE TABLE IF NOT EXISTS published_messages (
      message_id INTEGER REFERENCES messages (id) ON DELETE CASCADE,
      timestamp INTEGER
    )"

   "CREATE TABLE IF NOT EXISTS message_translations (
      message_id INTEGER REFERENCES messages (id) ON DELETE CASCADE,
      language TEXT,
      text TEXT,

      CONSTRAINT unique_translation UNIQUE (message_id, language)
    )"

   "CREATE TABLE IF NOT EXISTS message_tags (
      message_id INTEGER REFERENCES messages (id) ON DELETE CASCADE,
      tag TEXT,

      CONSTRAINT unique_tag UNIQUE (message_id, tag)
    )"))

(defun ensure-schema-loaded (db)
  (let ((handle (db-handle db)))
    (dolist (cmd *schema*)
      (dbi:execute (dbi:prepare handle cmd)))))

(defparameter *canned-data-inserts*
  (list
   "INSERT INTO messages (id) VALUES
      (1),
      (2),
      (3),
      (4),
      (5),
      (6),
      (7)"

   "INSERT INTO published_messages (message_id,timestamp) VALUES
      (1,1),
      (2,2),
      (3,3),
      (4,5),
      (5,4)"

   "INSERT INTO message_translations (message_id, language, text) VALUES
      (1, \"EN\", \"Message 1\"),
      (2, \"EN\", \"Message 2\"),
      (2, \"FR\", \"Le Message Deux\"),
      (2, \"ES\", \"El Message Dos\"),
      (3, \"EN\", \"Message 3\"),
      (3, \"FR\", \"Le Message Trois\"),
      (4, \"EN\", \"Message 4\"),
      (5, \"EN\", \"Message 5\"),
      (6, \"EN\", \"Message 6\"),
      (7, \"EN\", \"Message 7\")"

   "INSERT INTO message_tags (message_id, tag) VALUES
      (1, \"COMMON-LISP\"),
      (2, \"EMACS-LISP\"),
      (3, \"COMMON-LISP\"),
      (3, \"ILC\"),
      (4, \"COMMON-LISP\"),
      (5, \"COMMON-LISP\"),
      (7, \"COMMON-LISP\")"))

(defun ensure-canned-data-loaded (db)
  (let ((handle (db-handle db)))
    (dolist (insert *canned-data-inserts*)
      (dbi:execute (dbi:prepare handle insert)))))
