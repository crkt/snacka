(defpackage :shitchat.api
  (:use :common-lisp))

(in-package :shitchat.api)
(require 'sb-bsd-sockets)

(defparameter *localhost-address* '(127 0 0 1))


;;; Protocol functions, TLV
(defun message-to-TLV (command message)
  (concatenate 'string command "/" (write-to-string (length message)) "/" message (list #\return #\linefeed)))

(defun connect-to-server ()
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-connect socket *localhost-address* 3000)
    socket))

(defun send-message (socket message)
  (sb-bsd-sockets:socket-send socket
                              message
                              nil
                              :external-format :utf-8))

(defun receive-loop (conn &key (maxsize 65536) (output nil))
  (let ((stream (sb-bsd-sockets:socket-receive conn nil maxsize)))
    (when stream
      (with-input-from-string (s stream)
        (format output (read-line s)))))
  (receive-loop conn :output output))


(defun handle-connection (conn &key (input nil))
  (let* ((user-input (read-line input)))
    (cond ((equal user-input "quit") (progn
                                (sb-bsd-sockets:socket-close conn)
                                nil))
          (t (progn (send-message conn (message-to-TLV "send" user-input))
                    (handle-connection conn :input input))))))

(defun main ()
  (let* ((conn (connect-to-server))
         (recv-thread (sb-thread:make-thread #'receive-loop :arguments (list conn :output *standard-output*)))
         (send-thread (sb-thread:make-thread #'handle-connection :arguments (list conn :input *standard-input*))))))
