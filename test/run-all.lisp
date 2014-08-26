(in-package :motd-server-test)

(defun run-all-tests ()
  (let ((*print-pretty* t))
    (nst:nst-cmd :run-package #.*package*)))
