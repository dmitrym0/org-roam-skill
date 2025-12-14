;;; org-roam-skill-utils.el --- Utility functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Tahir Butt
;; Keywords: outlines convenience

;;; Commentary:
;; Utility functions for org-roam-skill including orphan detection and stats.

;;; Code:

(require 'org-roam)
(require 'seq)

;;; Debugging

defvar org-roam-skill-debug nil
  "Enable debug logging if non-nil."

defun org-roam-skill-log (message)
  "Log a debug message if org-roam-skill-debug is non-nil."
  (when org-roam-skill-debug
    (message "[DEBUG] %s" message)))

;;;###autoload
(defun org-roam-skill-check-setup ()
  "Check if org-roam is properly set up.
Return a plist with status information."
  (interactive)
  (org-roam-skill-log "Checking org-roam setup...")
  (condition-case err
      (progn
        (org-roam-skill-log "org-roam loaded: %s" (featurep 'org-roam))
        (org-roam-skill-log "org-roam directory: %s" org-roam-directory)
        (org-roam-skill-log "org-roam database: %s" org-roam-db-location)
        (list :org-roam-loaded (featurep 'org-roam)
              :directory org-roam-directory
              :directory-exists (file-exists-p org-roam-directory)
              :database-location org-roam-db-location
              :database-exists (file-exists-p org-roam-db-location)
              :node-count (length (org-roam-node-list))))
    (error
     (message "Error checking org-roam setup: %s" (error-message-string err))
     (org-roam-skill-log "Error checking org-roam setup: %s" (error-message-string err))
     (list :error (error-message-string err)))))

;;;###autoload
(defun org-roam-skill-get-note-info (title)
  "Get comprehensive information about a note by TITLE.
Return a formatted string with all note details."
  (interactive "sNote title: ")
  (org-roam-skill-log "Getting note info for: %s" title)
  (condition-case err
      (let ((node (org-roam-node-from-title-or-alias title)))
        (org-roam-skill-log "Found node: %s" (if node "yes" "no"))
        (if node
            (progn
              (org-roam-skill-log "Node ID: %s" (org-roam-node-id node))
              (org-roam-skill-log "Node file: %s" (org-roam-node-file node))
              (format
               "Title: %s\nID: %s\nFile: %s\nTags: %s\nAliases: %s\nRefs: %s\nBacklinks: %d\nLevel: %d"
               (org-roam-node-title node)
               (org-roam-node-id node)
               (org-roam-node-file node)
               (or (org-roam-node-tags node) "none")
               (or (org-roam-node-aliases node) "none")
               (or (org-roam-node-refs node) "none")
               (length (org-roam-backlinks-get node))
               (org-roam-node-level node)))
          (progn
            (org-roam-skill-log "Note not found: %s" title)
            "Note not found")))
    (error
     (message "Error getting note info: %s" (error-message-string err))
     (org-roam-skill-log "Error getting note info: %s" (error-message-string err))
     (format "Error: %s" (error-message-string err)))))

;;;###autoload
(defun org-roam-skill-list-recent-notes (n)
  "List the N most recently modified notes.
Return a list of (id title file mtime) tuples."
  (interactive "nNumber of recent notes: ")
  (condition-case err
      (mapcar
       (lambda (node)
         (list (org-roam-node-id node)
               (org-roam-node-title node)
               (org-roam-node-file node)
               (org-roam-node-file-mtime node)))
       (seq-take
        (seq-sort
         (lambda (a b)
           (time-less-p (org-roam-node-file-mtime b)
                        (org-roam-node-file-mtime a)))
         (org-roam-node-list))
        n))
    (error
     (message "Error listing recent notes: %s" (error-message-string err))
     nil)))

;;;###autoload
(defun org-roam-skill-find-orphan-notes ()
  "Find notes that have no backlinks and no forward links.
Return a list of (id title file) tuples for orphaned notes."
  (interactive)
  (condition-case err
      (seq-filter
       (lambda (node-info)
         (let* ((node (org-roam-node-from-id (car node-info)))
                (backlinks (org-roam-backlinks-get node))
                (file (org-roam-node-file node))
                (has-forward-links nil))
           ;; Check for forward links
           (when (and file (file-exists-p file))
             (with-temp-buffer
               (insert-file-contents file)
               (goto-char (point-min))
               (when (re-search-forward "\[\[id:" nil t)
                 (setq has-forward-links t))))
           ;; Return if both are empty
           (and (null backlinks) (not has-forward-links))))
       (mapcar
        (lambda (node)
          (list (org-roam-node-id node)
                (org-roam-node-title node)
                (org-roam-node-file node)))
        (org-roam-node-list)))
    (error
     (message "Error finding orphan notes: %s" (error-message-string err))
     nil)))

;;;###autoload
(defun org-roam-skill-get-graph-stats ()
  "Get statistics about the org-roam graph.
Return a plist with various statistics."
  (interactive)
  (condition-case err
      (let* ((nodes (org-roam-node-list))
             (total-nodes (length nodes))
             (total-links 0)
             ;; Forward declare to avoid dependency on org-roam-skill-tags
             (tags (sort
                    (delete-dups
                     (flatten-list
                      (mapcar #'org-roam-node-tags nodes)))
                    #'string<)))
        (dolist (node nodes)
          (setq total-links (+ total-links (length (org-roam-backlinks-get node)))))
        (list :total-notes total-nodes
              :total-links total-links
              :unique-tags (length tags)
              :average-links-per-note (if (> total-nodes 0)
                                          (/ (float total-links) total-nodes)
                                        0)))
    (error
     (message "Error getting graph stats: %s" (error-message-string err))
     nil)))

(defun org-roam-skill--format-buffer ()
  "Format the current org buffer, aligning tables and cleaning up structure.
Should be called with point in an org-mode buffer."
  (save-excursion
    ;; Align all tables in the buffer
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*|" nil t)
      (beginning-of-line)
      (when (org-at-table-p)
        (org-table-align))
      (forward-line 1))

    ;; Clean up spacing around headings and content
    (org-mode-restart)))

;;;###autoload
(defun format-org-roam-note (title-or-id)
  "Format the org-roam note identified by TITLE-OR-ID.
Aligns tables and cleans up structure. Returns t on success."
  (interactive "sNote title or ID: ")
  (condition-case err
      (let* ((node (if (and (stringp title-or-id)
                            (string-match-p "^[0-9a-f]\{8\}-" title-or-id))
                       (org-roam-node-from-id title-or-id)
                     (org-roam-node-from-title-or-alias title-or-id)))
             (file (when node (org-roam-node-file node))))
        (unless node
          (error "Node not found: %s" title-or-id))
        (unless (file-exists-p file)
          (error "File not found: %s" file))

        (with-current-buffer (find-file-noselect file)
          (org-roam-skill--format-buffer)
          (save-buffer)
          t))
    (error
     (message "Error formatting note: %s" (error-message-string err))
     nil)))

(provide 'org-roam-skill-utils)
;;; org-roam-skill-utils.el ends here
