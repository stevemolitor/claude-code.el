;;; claude-code-ws.el --- WebSocket server for Claude Code integration -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Stephen Molitor

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the Apache License, Version 2.0.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;;; Commentary:
;; WebSocket server implementation for Claude Code IDE integration.
;; Implements RFC 6455 WebSocket protocol for communication with Claude Code CLI.

;;; Code:

(require 'cl-lib)
(require 'bindat)

;;;; Constants

(defconst claude-code-ws-guid "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  "WebSocket GUID as specified in RFC 6455.")

(defconst claude-code-ws-opcodes
  '((continuation . 0)
    (text . 1)
    (binary . 2)
    (close . 8)
    (ping . 9)
    (pong . 10))
  "WebSocket opcode definitions.")

;;;; Data structures

(cl-defstruct claude-code-ws-server
  "WebSocket server structure."
  process      ; Network process
  port         ; Server port
  clients      ; List of connected clients
  on-open      ; Callback: (client)
  on-message   ; Callback: (client message)
  on-close     ; Callback: (client code reason)
  on-error     ; Callback: (client error)
  on-ping      ; Callback: (client data)
  )

(cl-defstruct claude-code-ws-client
  "WebSocket client connection."
  process      ; Network process
  server       ; Parent server
  state        ; :connecting, :open, :closing, :closed
  buffer       ; Accumulation buffer for frames
  )

;;;; WebSocket frame structure

(defconst claude-code-ws-frame-bindat-spec
  '((fin u8)
    (rsv u8) 
    (opcode u8)
    (mask u8)
    (payload-len u8))
  "Bindat spec for WebSocket frame header (first 2 bytes).")

;;;; Public API

(cl-defun claude-code-ws-create-server (&key port 
                                                  (on-open #'ignore)
                                                  (on-message #'ignore)
                                                  (on-close #'ignore)
                                                  (on-error #'ignore)
                                                  (on-ping #'ignore))
  "Create a WebSocket server on PORT.
Callbacks:
- ON-OPEN: Called when client connects (client)
- ON-MESSAGE: Called when text message received (client message)
- ON-CLOSE: Called when client disconnects (client code reason)
- ON-ERROR: Called on errors (client error)
- ON-PING: Called on ping frames (client data)"
  (let* ((server (make-claude-code-ws-server
                  :port port
                  :clients nil
                  :on-open on-open
                  :on-message on-message
                  :on-close on-close
                  :on-error on-error
                  :on-ping on-ping))
         (process (make-network-process
                   :name (format "claude-code-ws-server-%d" port)
                   :server t
                   :host "127.0.0.1"
                   :service port
                   :family 'ipv4
                   :filter #'claude-code-ws--server-filter
                   :sentinel #'claude-code-ws--server-sentinel
                   :log (lambda (server client message)
                          (claude-code-ws--accept-client server client message))
                   :plist `(:ws-server ,server))))
    (setf (claude-code-ws-server-process server) process)
    server))

(defun claude-code-ws-stop-server (server)
  "Stop the WebSocket SERVER and close all client connections."
  (when server
    ;; Close all clients
    (dolist (client (claude-code-ws-server-clients server))
      (claude-code-ws-close client 1001 "Server shutting down"))
    ;; Stop server process
    (when-let ((process (claude-code-ws-server-process server)))
      (delete-process process))))

(defun claude-code-ws-send (client message)
  "Send text MESSAGE to WebSocket CLIENT."
  (when (and client 
             (eq (claude-code-ws-client-state client) :open)
             (process-live-p (claude-code-ws-client-process client)))
    (let ((frame (claude-code-ws--create-text-frame message)))
      (process-send-string (claude-code-ws-client-process client) frame))))

(cl-defun claude-code-ws-close (client &optional (code 1000) (reason ""))
  "Close WebSocket CLIENT connection with CODE and REASON."
  (when client
    (let ((process (claude-code-ws-client-process client)))
      (when (process-live-p process)
        (condition-case nil
            (let ((frame (claude-code-ws--create-close-frame code reason)))
              (process-send-string process frame))
          (error nil))
        (delete-process process)))))

(defun claude-code-ws-server-port (server)
  "Get the port number of the WebSocket SERVER."
  (claude-code-ws-server-port server))

;;;; Internal functions - Connection handling

(defun claude-code-ws--accept-client (server client-process message)
  "Accept new CLIENT-PROCESS connection to SERVER."
  (let* ((ws-server (plist-get (process-plist server) :ws-server))
         (client (make-claude-code-ws-client
                  :process client-process
                  :server ws-server
                  :state :connecting
                  :buffer "")))
    ;; Set up client process
    (set-process-filter client-process #'claude-code-ws--client-filter)
    (set-process-sentinel client-process #'claude-code-ws--client-sentinel)
    (set-process-plist client-process `(:ws-client ,client))
    ;; Add to server's client list
    (push client (claude-code-ws-server-clients ws-server))))

(defun claude-code-ws--client-filter (process string)
  "Handle incoming data STRING from client PROCESS."
  (let ((client (plist-get (process-plist process) :ws-client)))
    (when client
      (setf (claude-code-ws-client-buffer client)
            (concat (claude-code-ws-client-buffer client) string))
      (condition-case err
          (cond
           ;; Handle WebSocket handshake
           ((eq (claude-code-ws-client-state client) :connecting)
            (claude-code-ws--handle-handshake client))
           ;; Handle WebSocket frames
           ((eq (claude-code-ws-client-state client) :open)
            (claude-code-ws--handle-frames client)))
        (error
         (let ((server (claude-code-ws-client-server client)))
           (funcall (claude-code-ws-server-on-error server) 
                    client (error-message-string err))
           (claude-code-ws-close client 1002 "Protocol error")))))))

(defun claude-code-ws--client-sentinel (process event)
  "Handle PROCESS sentinel EVENT for WebSocket client."
  (let ((client (plist-get (process-plist process) :ws-client)))
    (when (and client (string-match "\\(finished\\|exited\\|connection broken\\)" event))
      (setf (claude-code-ws-client-state client) :closed)
      (let ((server (claude-code-ws-client-server client)))
        ;; Remove from server's client list
        (setf (claude-code-ws-server-clients server)
              (delq client (claude-code-ws-server-clients server)))
        ;; Call close callback
        (funcall (claude-code-ws-server-on-close server) client 1006 "Abnormal closure")))))

(defun claude-code-ws--server-filter (process string)
  "Server process filter - should not receive data."
  nil)

(defun claude-code-ws--server-sentinel (process event)
  "Handle server PROCESS sentinel EVENT."
  (when (string-match "\\(finished\\|exited\\)" event)
    (let ((server (plist-get (process-plist process) :ws-server)))
      (when server
        (claude-code-ws-stop-server server)))))

;;;; WebSocket handshake

(defun claude-code-ws--handle-handshake (client)
  "Handle WebSocket handshake for CLIENT."
  (let ((buffer (claude-code-ws-client-buffer client)))
    (when (string-match "\r\n\r\n" buffer)
      (let* ((request (substring buffer 0 (match-end 0)))
             (remaining (substring buffer (match-end 0)))
             (headers (claude-code-ws--parse-headers request)))
        (if (claude-code-ws--valid-handshake-p headers)
            (progn
              ;; Send handshake response
              (claude-code-ws--send-handshake-response client headers)
              ;; Update client state
              (setf (claude-code-ws-client-state client) :open)
              (setf (claude-code-ws-client-buffer client) remaining)
              ;; Call open callback
              (let ((server (claude-code-ws-client-server client)))
                (funcall (claude-code-ws-server-on-open server) client))
              ;; Process any remaining data
              (when (> (length remaining) 0)
                (claude-code-ws--handle-frames client)))
          ;; Invalid handshake
          (process-send-string (claude-code-ws-client-process client)
                               "HTTP/1.1 400 Bad Request\r\n\r\n")
          (delete-process (claude-code-ws-client-process client)))))))

(defun claude-code-ws--parse-headers (request)
  "Parse HTTP headers from REQUEST string."
  (let ((lines (split-string request "\r\n"))
        (headers (make-hash-table :test 'equal)))
    (dolist (line (cdr lines))
      (when (string-match "^\\([^:]+\\):\\s-*\\(.+\\)$" line)
        (puthash (downcase (match-string 1 line))
                 (match-string 2 line)
                 headers)))
    headers))

(defun claude-code-ws--valid-handshake-p (headers)
  "Check if HEADERS represent a valid WebSocket handshake."
  (and (string= (gethash "upgrade" headers) "websocket")
       (string-match-p "\\bUpgrade\\b" (or (gethash "connection" headers) ""))
       (gethash "sec-websocket-key" headers)
       (string= (gethash "sec-websocket-version" headers) "13")))

(defun claude-code-ws--send-handshake-response (client headers)
  "Send WebSocket handshake response to CLIENT based on HEADERS."
  (let* ((key (gethash "sec-websocket-key" headers))
         (accept (claude-code-ws--calculate-accept-key key))
         (protocol (gethash "sec-websocket-protocol" headers))
         (response (concat "HTTP/1.1 101 Switching Protocols\r\n"
                          "Upgrade: websocket\r\n"
                          "Connection: Upgrade\r\n"
                          "Sec-WebSocket-Accept: " accept "\r\n")))
    ;; Add protocol if requested
    (when (and protocol (string= protocol "mcp"))
      (setq response (concat response "Sec-WebSocket-Protocol: mcp\r\n")))
    (setq response (concat response "\r\n"))
    (process-send-string (claude-code-ws-client-process client) response)))

(defun claude-code-ws--calculate-accept-key (key)
  "Calculate WebSocket accept key from client KEY."
  (base64-encode-string
   (secure-hash 'sha1 (concat key claude-code-ws-guid) nil nil t)
   t))

;;;; Frame handling

(defun claude-code-ws--handle-frames (client)
  "Process WebSocket frames in CLIENT's buffer."
  (catch 'need-more-data
    (while (> (length (claude-code-ws-client-buffer client)) 0)
      (let ((frame-data (claude-code-ws--parse-frame 
                         (claude-code-ws-client-buffer client))))
        (if frame-data
            (progn
              ;; Remove processed bytes from buffer
              (setf (claude-code-ws-client-buffer client)
                    (substring (claude-code-ws-client-buffer client)
                               (plist-get frame-data :bytes-consumed)))
              ;; Handle frame
              (claude-code-ws--handle-frame client frame-data))
          ;; Need more data
          (throw 'need-more-data nil))))))

(defun claude-code-ws--parse-frame (data)
  "Parse a WebSocket frame from DATA.
Returns plist with frame info or nil if incomplete."
  (cl-block nil
    (when (< (length data) 2)
      (cl-return nil))
    
    (let* ((byte0 (aref data 0))
           (byte1 (aref data 1))
           (fin (= (logand byte0 #x80) #x80))
           (opcode (logand byte0 #x0f))
           (masked (= (logand byte1 #x80) #x80))
           (payload-len (logand byte1 #x7f))
           (header-len 2)
           (actual-payload-len payload-len))
      
      ;; Extended payload length
      (cond
       ((= payload-len 126)
        (when (< (length data) 4)
          (cl-return nil))
        (setq actual-payload-len (+ (lsh (aref data 2) 8) (aref data 3)))
        (setq header-len 4))
       ((= payload-len 127)
        ;; We don't support payloads > 2^32 for simplicity
        (when (< (length data) 10)
          (cl-return nil))
        (setq actual-payload-len 
              (+ (lsh (aref data 6) 24)
                 (lsh (aref data 7) 16)
                 (lsh (aref data 8) 8)
                 (aref data 9)))
        (setq header-len 10)))
      
      ;; Add mask length
      (when masked
        (setq header-len (+ header-len 4)))
      
      ;; Check if we have full frame
      (when (< (length data) (+ header-len actual-payload-len))
        (cl-return nil))
      
      ;; Extract mask and payload
      (let ((mask-key (when masked
                        (substring data (- header-len 4) header-len)))
            (payload (substring data header-len 
                                (+ header-len actual-payload-len))))
        
        ;; Unmask payload if needed
        (when masked
          (setq payload (claude-code-ws--unmask-payload payload mask-key)))
        
        (list :fin fin
              :opcode opcode
              :payload payload
              :bytes-consumed (+ header-len actual-payload-len))))))

(defun claude-code-ws--unmask-payload (payload mask-key)
  "Unmask PAYLOAD using MASK-KEY."
  (let ((unmasked (make-string (length payload) 0)))
    (dotimes (i (length payload))
      (aset unmasked i 
            (logxor (aref payload i)
                    (aref mask-key (mod i 4)))))
    unmasked))

(defun claude-code-ws--handle-frame (client frame-data)
  "Handle parsed FRAME-DATA for CLIENT."
  (let ((opcode (plist-get frame-data :opcode))
        (payload (plist-get frame-data :payload))
        (server (claude-code-ws-client-server client)))
    (pcase opcode
      ;; Text frame
      (1 (funcall (claude-code-ws-server-on-message server) 
                  client payload))
      ;; Close frame
      (8 (let ((code (if (>= (length payload) 2)
                         (+ (lsh (aref payload 0) 8) (aref payload 1))
                       1005))
               (reason (if (> (length payload) 2)
                           (substring payload 2)
                         "")))
           (funcall (claude-code-ws-server-on-close server) 
                    client code reason)
           (claude-code-ws-close client code reason)))
      ;; Ping frame
      (9 (funcall (claude-code-ws-server-on-ping server) 
                  client payload)
         ;; Send pong
         (let ((pong-frame (claude-code-ws--create-pong-frame payload)))
           (process-send-string (claude-code-ws-client-process client) 
                                pong-frame)))
      ;; Pong frame - ignore
      (10 nil)
      ;; Unknown opcode
      (_ (claude-code-ws-close client 1002 "Unknown opcode")))))

;;;; Frame creation

(defun claude-code-ws--create-text-frame (text)
  "Create a text frame containing TEXT."
  (claude-code-ws--create-frame 1 (encode-coding-string text 'utf-8)))

(defun claude-code-ws--create-close-frame (code reason)
  "Create a close frame with CODE and REASON."
  (let ((payload (concat (string (lsh code -8) (logand code #xff))
                         (encode-coding-string reason 'utf-8))))
    (claude-code-ws--create-frame 8 payload)))

(defun claude-code-ws--create-pong-frame (data)
  "Create a pong frame with DATA."
  (claude-code-ws--create-frame 10 data))

(defun claude-code-ws--create-frame (opcode payload)
  "Create a WebSocket frame with OPCODE and PAYLOAD."
  (let* ((len (length payload))
         (frame (cond
                 ((< len 126)
                  (concat (string (logior #x80 opcode) len) payload))
                 ((< len 65536)
                  (concat (string (logior #x80 opcode) 126
                                  (lsh len -8) (logand len #xff))
                          payload))
                 (t
                  ;; Large payload
                  (concat (string (logior #x80 opcode) 127
                                  0 0 0 0  ; High 32 bits (always 0)
                                  (logand (lsh len -24) #xff)
                                  (logand (lsh len -16) #xff)
                                  (logand (lsh len -8) #xff)
                                  (logand len #xff))
                          payload)))))
    frame))

(provide 'claude-code-ws)
;;; claude-code-ws.el ends here