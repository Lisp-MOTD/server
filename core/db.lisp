(in-package :motd-server)

(defvar *motd-db-connection-info* nil
  "The info required to open the *MOTD-DB* using the =MOTD-DB= interface.")

(defvar =motd-db= nil
  "Interface type for the current interface.")

(defvar *motd-db* nil
  "Handle to the current database.")

(interface:define-interface =motd-db= ()
  (open-database (db-connection-info))
  (close-database (handle))
  (retrieve-all-motds-after (handle message-id))
  (retrieve-most-recent-motds (handle how-many-to-retrieve))
  (retrieve-all-proposed-motds (handle))
  (retrieve-all-tags (handle))
  (retrieve-public-key (handle user-name))
  (propose-message (handle))
  (insert-translation (handle message-id language text))
  (remove-translation (handle message-id language))
  (insert-tag (handle message-id tag))
  (remove-tag (handle message-id tag)))

(defun motd-open-database ()
  (setf *motd-db* (open-database =motd-db= *motd-db-connection-info*)))

(defun motd-close-database ()
  (unwind-protect
       (close-database =motd-db= *motd-db*)
    (setf *motd-db* nil)))
