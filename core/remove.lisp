(in-package :motd-server)

(defmethod delete-translation (message-id (language string))
  (remove-translation =motd-db= *motd-db* message-id language))

(defmethod delete-translation (message-id (language symbol))
  (delete-translation message-id (symbol-name language)))

(defmethod delete-tag (message-id (tag string))
  (remove-tag =motd-db= *motd-db* message-id tag))

(defmethod delete-tag (message-id (tag symbol))
  (delete-tag message-id (symbol-name tag)))
