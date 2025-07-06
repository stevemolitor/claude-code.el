;;; claude-code-selection.el --- Selection tracking for Claude Code IDE integration -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Stephen Molitor

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the Apache License, Version 2.0.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;;; Commentary:
;; Selection tracking for Claude Code MCP integration.
;; Monitors cursor position and text selection changes, sending notifications
;; to Claude so it knows what the user is looking at.
;; All functions are private (claude-code--selection-*) as this is internal infrastructure.

;;; Code:

(require 'cl-lib)
(require 'claude-code-mcp)

;;;; Constants

(defconst claude-code--selection-delay 0.05
  "Delay in seconds before sending selection update.")

;;;; Buffer-Local Variables

(defvar-local claude-code--selection-session nil
  "MCP session associated with current buffer for selection tracking.")

(defvar-local claude-code--selection-timer nil
  "Timer for debouncing selection updates.")

(defvar-local claude-code--selection-last-position nil
  "Last reported cursor position to avoid duplicate notifications.")

(defvar-local claude-code--selection-last-region nil
  "Last reported region to avoid duplicate notifications.")

;;;; Public API (called by claude-code-mcp.el)

(defun claude-code--selection-start (session)
  "Start selection tracking for SESSION."
  (with-current-buffer (claude-code--mcp-session-buffer session)
    ;; Set up tracking in the Claude buffer
    (setq-local claude-code--selection-session session)
    ;; Add hooks for all buffers
    (add-hook 'post-command-hook #'claude-code--selection-track-change)
    (add-hook 'window-selection-change-functions #'claude-code--selection-window-change)
    ;; Send initial selection
    (claude-code--selection-send-current session)))

(defun claude-code--selection-stop (session)
  "Stop selection tracking for SESSION."
  ;; Remove global hooks
  (remove-hook 'post-command-hook #'claude-code--selection-track-change)
  (remove-hook 'window-selection-change-functions #'claude-code--selection-window-change)
  ;; Cancel any pending timer
  (when-let ((buffer (claude-code--mcp-session-buffer session)))
    (with-current-buffer buffer
      (when claude-code--selection-timer
        (cancel-timer claude-code--selection-timer)
        (setq claude-code--selection-timer nil)))))

;;;; Selection Change Detection

(defun claude-code--selection-track-change ()
  "Track selection changes in current buffer (post-command-hook)."
  ;; Only track in file-visiting buffers
  (when (and buffer-file-name
             ;; Find session for current project
             (claude-code--selection-find-session-for-buffer))
    (claude-code--selection-schedule-update)))

(defun claude-code--selection-window-change (frame)
  "Track window selection changes in FRAME."
  ;; When switching windows, update selection
  (when-let ((window (frame-selected-window frame))
             (buffer (window-buffer window)))
    (with-current-buffer buffer
      (when (and buffer-file-name
                 (claude-code--selection-find-session-for-buffer))
        (claude-code--selection-schedule-update)))))

(defun claude-code--selection-find-session-for-buffer ()
  "Find MCP session for current buffer based on project.
Returns session or nil."
  ;; Check if we already have a cached session
  (if claude-code--selection-session
      claude-code--selection-session
    ;; Look for a session that matches our project
    (let ((file (buffer-file-name))
          (found-session nil))
      (when file
        (maphash (lambda (buffer session)
                   (let ((project-dir (claude-code--mcp-session-project-dir session)))
                     (when (and project-dir
                                (string-prefix-p (expand-file-name project-dir)
                                                 (expand-file-name file)))
                       (setq found-session session))))
                 claude-code--mcp-sessions))
      ;; Cache the session in this buffer
      (when found-session
        (setq-local claude-code--selection-session found-session))
      found-session)))

;;;; Debouncing

(defun claude-code--selection-schedule-update ()
  "Schedule a selection update with debouncing."
  (when claude-code--selection-session
    ;; Cancel existing timer
    (when claude-code--selection-timer
      (cancel-timer claude-code--selection-timer))
    ;; Schedule new update
    (setq claude-code--selection-timer
          (run-with-timer claude-code--selection-delay nil
                          #'claude-code--selection-send-update
                          (current-buffer)
                          claude-code--selection-session))))

(defun claude-code--selection-send-update (buffer session)
  "Send selection update for BUFFER to SESSION."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq claude-code--selection-timer nil)
      (claude-code--selection-send-current session))))

;;;; Selection Data Collection

(defun claude-code--selection-send-current (session)
  "Send current selection/cursor position for SESSION."
  (let* ((file-path (buffer-file-name))
         (point-pos (point))
         (has-region (use-region-p))
         (region-start (if has-region (region-beginning) point-pos))
         (region-end (if has-region (region-end) point-pos))
         (text (if has-region
                   (buffer-substring-no-properties region-start region-end)
                 ""))
         ;; Convert to line/column positions
         (start-line (line-number-at-pos region-start))
         (start-col (save-excursion
                      (goto-char region-start)
                      (current-column)))
         (end-line (line-number-at-pos region-end))
         (end-col (save-excursion
                    (goto-char region-end)
                    (current-column)))
         ;; Create position markers for duplicate detection
         (current-position (if has-region
                               (list region-start region-end)
                             point-pos))
         (current-region (if has-region
                             (list start-line start-col end-line end-col)
                           nil)))
    ;; Only send if something changed
    (when (or (not (equal current-position claude-code--selection-last-position))
              (not (equal current-region claude-code--selection-last-region)))
      ;; Update cached values
      (setq claude-code--selection-last-position current-position)
      (setq claude-code--selection-last-region current-region)
      ;; Send notification
      (claude-code--mcp-send-notification
       session
       "selection_changed"
       `((text . ,text)
         (filePath . ,file-path)
         (fileUrl . ,(concat "file://" file-path))
         (selection . ((start . ((line . ,(1- start-line))  ; 0-indexed
                                 (character . ,start-col)))
                       (end . ((line . ,(1- end-line))      ; 0-indexed
                               (character . ,end-col)))
                       (isEmpty . ,(not has-region)))))))))

;;;; Active Editor Tracking

(defun claude-code--selection-send-active-editor-change (session)
  "Send active editor change notification for SESSION."
  (let ((file-path (buffer-file-name)))
    (when file-path
      (claude-code--mcp-send-notification
       session
       "workspace/didChangeActiveEditor"
       `((filePath . ,file-path)
         (fileUrl . ,(concat "file://" file-path)))))))

(provide 'claude-code-selection)
;;; claude-code-selection.el ends here