(in-package :motd-server)

(defmethod new-motd ()
  (propose-message =motd-db= *motd-db*))

(defmethod add-translation (message-id (language string) text)
  (insert-translation =motd-db= *motd-db* message-id language text))

(defmethod add-translation (message-id (language symbol) text)
  (add-translation message-id (symbol-name language) text))

(defmethod add-tag (message-id (tag string))
  (insert-tag =motd-db= *motd-db* message-id tag))

(defmethod add-tag (message-id (tag symbol))
  (add-tag message-id (symbol-name tag)))
