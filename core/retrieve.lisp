(in-package :motd-server)

(defconstant +how-many-to-retrieve-limit+ 4)

(defmethod get-all-motds-after (message-id)
  (retrieve-all-motds-after *motd-db* message-id))

(defmethod get-most-recent-motds ((how-many-to-retrieve integer))
  (let ((how-many-to-retrieve (min how-many-to-retrieve
                                   +how-many-to-retrieve-limit+)))
    (retrieve-most-recent-motds *motd-db* how-many-to-retrieve)))

(defmethod get-all-proposed-motds ()
  (retrieve-all-proposed-motds *motd-db*))

(defmethod get-all-tags ()
  (retrieve-all-tags *motd-db*))
