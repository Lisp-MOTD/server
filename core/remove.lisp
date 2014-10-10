(in-package :motd-server)

(defmethod delete-translation (message-id
                               (language string))
  (remove-translation *motd-db* message-id language))

(defmethod delete-translation (message-id
                               (language symbol))
  (delete-translation message-id (symbol-name language)))
