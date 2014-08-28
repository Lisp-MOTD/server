(in-package :motd-server)

(defconstant +how-many-to-retrieve-limit+ 50)

(defmethod get-all-motds-after (message-id)
  (retrieve-all-motds-after *motd-db* message-id))

(defmethod get-most-recent-motds ((how-many-to-retrieve integer))
  (let ((how-many (min how-many-to-retrieve
                       +how-many-to-retrieve-limit+)))
    (list* how-many-to-retrieve
           (retrieve-most-recent-motds *motd-db* how-many))))

(defmethod get-all-proposed-motds ()
  (retrieve-all-proposed-motds *motd-db*))

(defmethod get-all-tags ()
  (retrieve-all-tags *motd-db*))
