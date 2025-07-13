;;; claude-code-mcp.el --- MCP protocol server for Claude Code IDE integration -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Stephen Molitor

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the Apache License, Version 2.0.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;;; Commentary:
;; MCP (Model Context Protocol) server implementation for Claude Code.
;; Provides WebSocket-based communication channel between Claude CLI and Emacs.
;; All functions are private (claude-code--mcp-*) as this is internal infrastructure.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'websocket)
(require 'files)

;;;; Constants

(defconst claude-code--mcp-protocol-version "2024-11-05"
  "MCP protocol version supported.")

(defconst claude-code--mcp-port-min 10000
  "Minimum port number for WebSocket server.")

(defconst claude-code--mcp-port-max 65535
  "Maximum port number for WebSocket server.")

;;;; Session Structure

(cl-defstruct claude-code--mcp-session
  "MCP session for a Claude instance."
  server          ; websocket server process
  client          ; connected websocket client (if any)
  port            ; server port number
  project-dir     ; project directory path
  instance-name   ; instance name (e.g., "default", "tests")
  buffer          ; associated Claude terminal buffer
  deferred        ; hash table for deferred responses
  selection-timer ; timer for debounced selection updates
  last-selection  ; cached last selection to avoid duplicates
  request-id      ; counter for generating request IDs
  )

;;;; Global State

(defvar claude-code--mcp-sessions (make-hash-table :test 'equal)
  "Hash table mapping buffer to MCP session.")

;;;; Public API (called by claude-code.el)

(defun claude-code--mcp-start (project-dir instance-name buffer)
  "Start MCP server for PROJECT-DIR with INSTANCE-NAME in BUFFER.
Returns the session object."
  (let* ((port (claude-code--mcp-find-free-port))
         (session (make-claude-code--mcp-session
                   :port port
                   :project-dir project-dir
                   :instance-name instance-name
                   :buffer buffer
                   :deferred (make-hash-table :test 'equal)
                   :request-id 0)))
    ;; Create WebSocket server
    (condition-case err
        (let ((server (websocket-server
                       port
                       :host "127.0.0.1"
                       :on-open (lambda (ws)
                                  (claude-code--mcp-on-open session ws))
                       :on-message (lambda (ws frame)
                                     (claude-code--mcp-on-message session ws frame))
                       :on-close (lambda (ws)
                                   (claude-code--mcp-on-close session ws))
                       :on-error (lambda (ws type err)
                                   (claude-code--mcp-on-error session ws type err))
                       :protocol '("mcp"))))
          (setf (claude-code--mcp-session-server session) server)
          ;; Create lock file
          (claude-code--mcp-create-lockfile session)
          ;; Store session
          (puthash buffer session claude-code--mcp-sessions)
          ;; Start selection tracking if available
          (when (require 'claude-code-selection nil t)
            (claude-code--selection-start session))
          session)
      (error
       (message "Failed to start MCP server: %s" (error-message-string err))
       nil))))

(defun claude-code--mcp-stop (session)
  "Stop MCP server and clean up SESSION."
  (when session
    ;; Stop selection tracking
    (when (require 'claude-code-selection nil t)
      (claude-code--selection-stop session))
    ;; Close WebSocket server
    (when-let ((server (claude-code--mcp-session-server session)))
      (websocket-server-close server))
    ;; Remove lock file
    (claude-code--mcp-remove-lockfile session)
    ;; Remove from sessions
    (remhash (claude-code--mcp-session-buffer session) claude-code--mcp-sessions)))

;;;; Port Management

(defun claude-code--mcp-find-free-port ()
  "Find a free port in the allowed range."
  (let ((ports (number-sequence claude-code--mcp-port-min 
                                claude-code--mcp-port-max)))
    ;; Shuffle ports for randomness
    (setq ports (claude-code--mcp-shuffle-list ports))
    ;; Try ports until we find a free one
    (cl-loop for port in ports
             when (claude-code--mcp-port-available-p port)
             return port
             finally (error "No free ports available"))))

(defun claude-code--mcp-port-available-p (port)
  "Check if PORT is available for binding."
  (condition-case nil
      (let ((test-process
             (make-network-process
              :name "test"
              :server t
              :host "127.0.0.1"
              :service port
              :family 'ipv4)))
        (delete-process test-process)
        t)
    (error nil)))

(defun claude-code--mcp-shuffle-list (list)
  "Return a shuffled copy of LIST."
  (let ((copy (copy-sequence list)))
    (cl-loop for i from (length copy) downto 2
             do (let ((j (random i)))
                  (cl-rotatef (nth (1- i) copy) (nth j copy))))
    copy))

;;;; Lock File Management

(defun claude-code--mcp-lockfile-dir ()
  "Return the directory for lock files."
  (expand-file-name "~/.claude/ide/"))

(defun claude-code--mcp-lockfile-path (session)
  "Return the lock file path for SESSION."
  (expand-file-name 
   (format "%d.lock" (claude-code--mcp-session-port session))
   (claude-code--mcp-lockfile-dir)))

(defun claude-code--mcp-create-lockfile (session)
  "Create lock file for SESSION."
  (let* ((dir (claude-code--mcp-lockfile-dir))
         (file (claude-code--mcp-lockfile-path session))
         (content (json-encode
                   `((pid . ,(emacs-pid))
                     (workspaceFolders . ,(vector (claude-code--mcp-session-project-dir session)))
                     (ideName . "Emacs")
                     (transport . "ws")))))
    ;; Ensure directory exists
    (make-directory dir t)
    ;; Write lock file
    (with-temp-file file
      (insert content))
    ;; Make it readable
    (set-file-modes file #o644)))

(defun claude-code--mcp-remove-lockfile (session)
  "Remove lock file for SESSION."
  (let ((file (claude-code--mcp-lockfile-path session)))
    (when (file-exists-p file)
      (delete-file file))))

;;;; WebSocket Callbacks

(defun claude-code--mcp-on-open (session ws)
  "Handle WebSocket WS open for SESSION."
  (setf (claude-code--mcp-session-client session) ws)
  (message "Claude Code connected to MCP server on port %d" 
           (claude-code--mcp-session-port session)))

(defun claude-code--mcp-on-message (session ws frame)
  "Handle WebSocket message FRAME from WS for SESSION."
  (let* ((payload (websocket-frame-text frame))
         (message (condition-case err
                      (json-read-from-string payload)
                    (error
                     (claude-code--mcp-send-error ws nil -32700 
                                                  "Parse error" 
                                                  (error-message-string err))
                     nil))))
    (when message
      (claude-code--mcp-handle-message session ws message))))

(defun claude-code--mcp-on-close (session ws)
  "Handle WebSocket WS close for SESSION."
  (when (eq (claude-code--mcp-session-client session) ws)
    (setf (claude-code--mcp-session-client session) nil))
  (message "Claude Code disconnected from MCP server"))

(defun claude-code--mcp-on-error (session ws type err)
  "Handle WebSocket error ERR of TYPE from WS for SESSION."
  (message "MCP WebSocket error (%s): %s" type (error-message-string err)))

;;;; Helper Functions

(defun claude-code--mcp-find-session-by-ws (ws)
  "Find MCP session that owns WebSocket WS."
  ;; For now, just return the global test session if available
  (or (and (boundp 'test-mcp-standalone-session) test-mcp-standalone-session)
      (and (boundp 'claude-code-test-session) claude-code-test-session)))

;;;; Message Handling

(defun claude-code--mcp-handle-message (session ws message)
  "Handle JSON-RPC MESSAGE from WS for SESSION."
  (let ((id (alist-get 'id message))
        (method (alist-get 'method message))
        (params (alist-get 'params message)))
    (condition-case err
        (pcase method
          ;; Protocol initialization
          ("initialize"
           (claude-code--mcp-handle-initialize session ws id params))
          ;; Tool listing
          ("tools/list"
           (claude-code--mcp-handle-tools-list session ws id params))
          ;; Tool invocation
          ("tools/call"
           (claude-code--mcp-handle-tools-call session ws id params))
          ;; Prompts listing (empty for now)
          ("prompts/list"
           (claude-code--mcp-send-response ws id '((prompts . []))))
          ;; Unknown method
          (_
           (claude-code--mcp-send-error ws id -32601 
                                        (format "Method not found: %s" method))))
      (error
       (claude-code--mcp-send-error ws id -32603
                                    "Internal error"
                                    (error-message-string err))))))

(defun claude-code--mcp-handle-initialize (session ws id params)
  "Handle initialize request with ID and PARAMS from WS for SESSION."
  (claude-code--mcp-send-response
   ws id
   `((protocolVersion . ,claude-code--mcp-protocol-version)
     (capabilities . ((tools . ((listChanged . t)))
                      (prompts . ((listChanged . t)))
                      (resources . ((subscribe . t)
                                    (listChanged . t)))))
     (serverInfo . ((name . "claude-code-mcp")
                    (version . "0.1.0"))))))

(defun claude-code--mcp-handle-tools-list (session ws id params)
  "Handle tools/list request with ID and PARAMS from WS for SESSION."
  ;; Will be implemented when claude-code-tools.el is created
  (claude-code--mcp-send-response ws id '((tools . []))))

(defun claude-code--mcp-handle-tools-call (session ws id params)
  "Handle tools/call request with ID and PARAMS from WS for SESSION."
  ;; Will be implemented when claude-code-tools.el is created
  (claude-code--mcp-send-error ws id -32601 "No tools implemented yet"))

;;;; Response/Notification Sending

(defun claude-code--mcp-send-response (ws id result)
  "Send successful response with ID and RESULT to WS."
  (let ((response (json-encode
                   `((jsonrpc . "2.0")
                     (id . ,id)
                     (result . ,result)))))
    (websocket-send-text ws response)))

(defun claude-code--mcp-send-error (ws id code message &optional data)
  "Send error response with ID, CODE, MESSAGE and optional DATA to WS."
  (let ((response (json-encode
                   `((jsonrpc . "2.0")
                     (id . ,id)
                     (error . ((code . ,code)
                               (message . ,message)
                               ,@(when data `((data . ,data)))))))))
    (websocket-send-text ws response)))

(defun claude-code--mcp-send-notification (session method params)
  "Send notification with METHOD and PARAMS to SESSION's client."
  (when-let ((client (claude-code--mcp-session-client session)))
    (let ((notification (json-encode
                         `((jsonrpc . "2.0")
                           (method . ,method)
                           (params . ,params)))))
      (websocket-send-text client notification))))

;;;; Utility Functions

(defun claude-code--mcp-next-request-id (session)
  "Get next request ID for SESSION."
  (cl-incf (claude-code--mcp-session-request-id session)))

(provide 'claude-code-mcp)
;;; claude-code-mcp.el ends here
