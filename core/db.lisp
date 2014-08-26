(in-package :motd-server)

(defvar *motd-db* nil
  "Handle to the current database.")

(defclass motd-db ()
  ()
  (:documentation "Base class for the MOTD database."))

(defgeneric retrieve-all-motds-after (db message-id))
(defgeneric retrieve-most-recent-motds (db how-many-to-retrieve))
(defgeneric retrieve-all-proposed-messages (db))
(defgeneric retrieve-all-tags (db))
