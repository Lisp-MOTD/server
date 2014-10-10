(in-package :motd-server)

(defvar *motd-db* nil
  "Handle to the current database.")

(defclass motd-db ()
  ()
  (:documentation "Base class for the MOTD database."))

(defgeneric retrieve-all-motds-after (db message-id))
(defgeneric retrieve-most-recent-motds (db how-many-to-retrieve))
(defgeneric retrieve-all-proposed-motds (db))
(defgeneric retrieve-all-tags (db))

(defgeneric retrieve-public-key (db user-name))

(defgeneric propose-message (db))

(defgeneric insert-translation (db message-id language text))
(defgeneric remove-translation (db message-id language))

(defgeneric insert-tag (db message-id tag))
(defgeneric remove-tag (db message-id tag))
