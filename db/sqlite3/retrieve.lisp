(in-package #:motd-server-sqlite3)

(defmethod motd-server:retrieve-all-motds-after ((db sqlite3-motd-db)
                                                 message-id))

(defmethod motd-server:retrieve-most-recent-motds ((db sqlite3-motd-db)
                                                   how-many-to-retrieve))

(defmethod retrieve-all-proposed-messages ((db sqlite3-motd-db)))

(defmethod retrieve-all-tags ((db sqlite3-motd-db)))
