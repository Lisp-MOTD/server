(in-package :motd-server)

(defmethod add-translation (message-id
                            (language string)
                            text)
  (insert-translation *motd-db* message-id language text))

(defmethod add-translation (message-id
                            (language symbol)
                            text)
  (add-translation message-id (symbol-name language) text))
