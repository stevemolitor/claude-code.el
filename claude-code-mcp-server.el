;;; claude-code-mcp-server.el --- MCP (Model Context Protocol) server for Emacs -*- lexical-binding: t; -*-

;; Author: Claude AI
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, ai, mcp

;;; Commentary:
;; This implements a Model Context Protocol (MCP) server in Emacs Lisp.
;; It provides tools that can be called by Claude Code CLI via the MCP protocol.
;; Communication happens via TCP on port 8765, with mcp-proxy.sh bridging stdio.

;;; Code:

(require 'json)
(require 'cl-lib)

;;;; Configuration

(defcustom claude-code-mcp-port 8765
  "TCP port for the MCP server."
  :type 'integer
  :group 'claude-code)

(defcustom claude-code-mcp-enabled t
  "Whether MCP server functionality is enabled."
  :type 'boolean
  :group 'claude-code)

;;;; State variables

(defvar claude-code-mcp-server-process nil
  "The MCP TCP server process.")

(defvar claude-code-mcp-client-connections nil
  "List of active MCP client connections.")

;;;; MCP Tool definition macro

(defmacro claude-code-defmcp (name args docstring &rest body-and-properties)
  "Define an MCP tool function with embedded properties.
NAME is the function name, ARGS is the argument list, DOCSTRING is the documentation.
The remaining BODY-AND-PROPERTIES can contain :mcp-description, :mcp-schema, and function body."
  (let ((description nil)
        (schema nil)
        (body '()))
    
    ;; Parse body and properties
    (while body-and-properties
      (let ((item (car body-and-properties)))
        (cond
         ((eq item :mcp-description)
          (setq description (cadr body-and-properties))
          (setq body-and-properties (cddr body-and-properties)))
         ((eq item :mcp-schema)
          (setq schema (cadr body-and-properties))
          (setq body-and-properties (cddr body-and-properties)))
         (t
          (push item body)
          (setq body-and-properties (cdr body-and-properties))))))
    
    (setq body (nreverse body))
    
    `(progn
       (defun ,name ,args
         ,docstring
         ,@body)
       (put ',name :mcp-tool t)
       ,(when description
          `(put ',name :mcp-description ,description))
       ,(when schema
          `(put ',name :mcp-schema ',schema))
       ',name)))

;;;; JSON-RPC message handling

(defun claude-code-mcp-send-response (process response)
  "Send JSON-RPC RESPONSE to PROCESS."
  (let ((json-str (json-encode response)))
    (process-send-string process (concat json-str "\n"))))

(defun claude-code-mcp-process-message (process message-string)
  "Process a single MCP message from PROCESS."
  (condition-case err
      (when (and message-string (not (string-empty-p (string-trim message-string))))
        (let* ((request (json-parse-string message-string :object-type 'alist))
               (method (alist-get 'method request))
               (id (alist-get 'id request))
               (params (alist-get 'params request))
               (response (claude-code-mcp-handle-method method id params)))
          (when response
            (claude-code-mcp-send-response process response))))
    (error
     (let ((request-id (or (ignore-errors 
                            (alist-get 'id (json-parse-string message-string :object-type 'alist))) 
                           nil))
           (error-response `((jsonrpc . "2.0")
                            (id . ,request-id)
                            (error . ((code . -32603)
                                     (message . ,(format "Internal error: %s" (error-message-string err))))))))
       (claude-code-mcp-send-response process error-response)))))

(defun claude-code-mcp-handle-method (method id params)
  "Handle MCP METHOD with ID and PARAMS, return response."
  (cond
   ;; Initialize handshake
   ((string= method "initialize")
    `((jsonrpc . "2.0")
      (id . ,id)
      (result . ((protocolVersion . "2024-11-05")
                (capabilities . ((tools . ((listChanged . t)))))
                (serverInfo . ((name . "emacs-mcp-server")
                              (version . "1.0.0")))))))

   ;; List available tools
   ((string= method "tools/list")
    (let ((tools (claude-code-mcp-discover-tools)))
      `((jsonrpc . "2.0")
        (id . ,id)
        (result . ((tools . ,(apply #'vector tools)))))))

   ;; Call a tool
   ((string= method "tools/call")
    (let* ((tool-name (alist-get 'name params))
           (tool-args (alist-get 'arguments params))
           (result (claude-code-mcp-call-tool tool-name tool-args)))
      `((jsonrpc . "2.0")
        (id . ,id)
        (result . ((content . [((type . "text")
                               (text . ,result))]))))))

   ;; Method not found
   (t
    `((jsonrpc . "2.0")
      (id . ,id)
      (error . ((code . -32601)
               (message . ,(format "Method not found: %s" method))))))))

;;;; Tool discovery and execution

(defun claude-code-mcp-discover-tools ()
  "Discover all available MCP tools."
  (let ((tools '()))
    (mapatoms
     (lambda (symbol)
       (when (and (fboundp symbol)
                  (get symbol :mcp-tool))
         (let* ((name (symbol-name symbol))
                (description (or (get symbol :mcp-description) 
                                (format "MCP tool: %s" name)))
                (schema (or (get symbol :mcp-schema) '()))
                (input-schema (claude-code-mcp-build-json-schema schema)))
           (push `((name . ,name)
                   (description . ,description)
                   (inputSchema . ,input-schema))
                 tools)))))
    (nreverse tools)))

(defun claude-code-mcp-build-json-schema (schema)
  "Build JSON Schema from simplified schema format."
  ;; Handle quoted schemas
  (when (and (consp schema) (eq (car schema) 'quote))
    (setq schema (cadr schema)))
  
  (cond
   ;; Empty or nil schema - no parameters
   ((or (null schema) (equal schema '()))
    `((type . "object") 
      (properties . ,(make-hash-table :test 'equal))
      (required . ,(vector))
      (additionalProperties . :json-false)))
   
   ;; Simplified format - list of (name type description) tuples
   ((listp schema)
    (let ((properties (make-hash-table :test 'equal))
          (required '()))
      (dolist (item schema)
        (cond
         ;; (name . ("type" "description")) format  
         ((and (consp item) (>= (length item) 2))
          (let ((param-name (symbol-name (car item)))
                (param-info (cdr item)))
            (when (and (listp param-info) (>= (length param-info) 1))
              (let ((param-type (car param-info))
                    (param-desc (cadr param-info)))
                (push param-name required)
                (if param-desc
                    (puthash param-name `((type . ,param-type)
                                         (description . ,param-desc)) properties)
                  (puthash param-name `((type . ,param-type)) properties))))))))
      
      `((type . "object")
        (properties . ,properties)
        (required . ,(vconcat (nreverse required)))
        (additionalProperties . :json-false))))
   
   ;; Fallback
   (t `((type . "object")
        (properties . ,(make-hash-table :test 'equal))
        (required . ,(vector))
        (additionalProperties . :json-false)))))

(defun claude-code-mcp-call-tool (tool-name tool-args)
  "Call MCP tool TOOL-NAME with TOOL-ARGS."
  (let ((symbol (intern tool-name)))
    (if (and (fboundp symbol) (get symbol :mcp-tool))
        (condition-case err
            (let* ((func-args (help-function-arglist symbol))
                   (ordered-params (claude-code-mcp-map-args tool-args func-args)))
              (format "%s" (apply symbol ordered-params)))
          (error (format "Error executing %s: %s" tool-name (error-message-string err))))
      (format "Tool not found: %s" tool-name))))

(defun claude-code-mcp-map-args (params-alist func-args)
  "Map PARAMS-ALIST to FUNC-ARGS order."
  (let ((mapped-args '()))
    (dolist (arg-spec func-args)
      (unless (memq arg-spec '(&optional &rest &key))
        (let* ((arg-name (if (listp arg-spec) (car arg-spec) arg-spec))
               (arg-key (symbol-name arg-name))
               (param-value (alist-get (intern arg-key) params-alist)))
          ;; Convert JSON arrays (vectors) to Lisp lists
          (when (vectorp param-value)
            (setq param-value (append param-value nil)))
          (push param-value mapped-args))))
    (nreverse mapped-args)))

;;;; Network server

(defun claude-code-mcp-filter (process string)
  "Filter function for MCP TCP connections.
PROCESS is the network process, STRING is the received data."
  ;; Buffer incomplete messages
  (let ((buffer (or (process-get process :mcp-buffer) "")))
    (setq buffer (concat buffer string))

    ;; Process complete JSON-RPC messages (ended by newline)
    (let ((lines (split-string buffer "\n" t)))
      (if (string-suffix-p "\n" buffer)
          ;; All lines are complete messages
          (progn
            (process-put process :mcp-buffer "")
            (dolist (line lines)
              (claude-code-mcp-process-message process line)))
        ;; Last line is incomplete, save it for next call
        (let ((complete-lines (butlast lines))
              (incomplete-line (car (last lines))))
          (process-put process :mcp-buffer (or incomplete-line ""))
          (dolist (line complete-lines)
            (claude-code-mcp-process-message process line)))))))

(defun claude-code-mcp-sentinel (process event)
  "Sentinel function for MCP TCP connections.
PROCESS is the network process, EVENT is the event description."
  (when (string-match "^\\(closed\\|failed\\)" event)
    (setq claude-code-mcp-client-connections
          (delq process claude-code-mcp-client-connections))
    (message "MCP client disconnected")))

(defun claude-code-mcp-server-filter (server client message)
  "Accept new MCP client connections.
SERVER is the server process, CLIENT is the new client process, MESSAGE is unused."
  (push client claude-code-mcp-client-connections)
  (set-process-filter client #'claude-code-mcp-filter)
  (set-process-sentinel client #'claude-code-mcp-sentinel)
  (message "MCP client connected"))

;;;; Server management

(defun claude-code-mcp-start-server ()
  "Start the MCP TCP server."
  (interactive)
  (when (and claude-code-mcp-enabled
             (not (and claude-code-mcp-server-process
                      (process-live-p claude-code-mcp-server-process))))
    (setq claude-code-mcp-server-process
          (make-network-process
           :name "claude-code-mcp"
           :service claude-code-mcp-port
           :server t
           :family 'ipv4
           :host 'local
           :filter #'claude-code-mcp-server-filter))
    (message "Claude Code MCP server started on port %d" claude-code-mcp-port)))

(defun claude-code-mcp-stop-server ()
  "Stop the MCP TCP server."
  (interactive)
  ;; Stop server process
  (when (and claude-code-mcp-server-process
             (process-live-p claude-code-mcp-server-process))
    (delete-process claude-code-mcp-server-process)
    (setq claude-code-mcp-server-process nil))

  ;; Close all client connections
  (dolist (conn claude-code-mcp-client-connections)
    (when (process-live-p conn)
      (delete-process conn)))
  (setq claude-code-mcp-client-connections nil)

  (message "Claude Code MCP server stopped"))

;;;; Auto-start server

(when claude-code-mcp-enabled
  (claude-code-mcp-start-server))

(provide 'claude-code-mcp-server)
;;; claude-code-mcp-server.el ends here