;;; check-websocket.el --- Check if websocket is available -*- lexical-binding: t; -*-

(message "Checking websocket availability...")

(if (featurep 'websocket)
    (message "websocket feature is already loaded")
  (message "websocket feature is NOT loaded"))

(condition-case err
    (progn
      (require 'websocket)
      (message "Successfully loaded websocket library")
      (message "websocket-version: %s" (if (boundp 'websocket-version) 
                                           websocket-version 
                                         "version not defined")))
  (error
   (message "ERROR loading websocket: %s" (error-message-string err))
   (message "You may need to install it with: M-x package-install RET websocket RET")))