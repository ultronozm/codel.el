;;; codel.el --- some llm tools                      -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Paul D. Nelson

;; Author: Paul D. Nelson <ultrono@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; codel (pronounced like the verb "coddle", a synonym of "pamper") is
;; a collection of llm tools.  See README.org

;;; Code:

(require 'json)
(require 'url)
(require 'shr)

(defun codel-bash (command &optional _timeout)
  "Execute bash COMMAND with optional TIMEOUT."
  (let ((result (shell-command-to-string command)))
    (if (string-empty-p result)
        "Command executed successfully (no output)"
      result)))

(defun codel-glob-tool (pattern &optional path)
  "Find files matching PATTERN in PATH or default directory."
  (let* ((default-directory (or path default-directory))
         (files (file-expand-wildcards pattern)))
    (string-join files "\n")))

(defun codel-grep-tool (pattern &optional include path)
  "Search for PATTERN in files matching INCLUDE in PATH."
  (let* ((default-directory (or path default-directory))
         (include-arg (if include
                          (format "--include=\"%s\"" include)
                        ""))
         (command (format "grep -r -n -E %s %s ."
                          (shell-quote-argument pattern)
                          include-arg))
         (result (shell-command-to-string command)))
    (if (string-empty-p result)
        "No matches found"
      result)))

(defun codel-ls (path &optional ignore)
  "List files in PATH, optionally excluding files matching IGNORE patterns."
  (let ((files (directory-files path t nil t)))
    (when ignore
      (seq-do (lambda (pattern)
                (setq files (seq-filter (lambda (f)
                                          (not (string-match-p pattern f)))
                                        files)))
              ignore))
    (string-join (mapcar #'file-name-nondirectory files) "\n")))

(defun codel-view (file-path &optional limit offset)
  "View contents of FILE-PATH with optional LIMIT and OFFSET."
  (with-temp-buffer
    (insert-file-contents file-path)
    (let* ((lines (split-string (buffer-string) "\n"))
           (start (or offset 0))
           (end (if limit (min (+ start limit) (length lines)) (length lines)))
           (selected-lines (seq-subseq lines start end)))
      (string-join selected-lines "\n"))))

(defun codel-edit (file-path old-string new-string)
  "In FILE-PATH, replace OLD-STRING with NEW-STRING."
  (with-temp-buffer
    (insert-file-contents file-path)
    (let ((case-fold-search nil))
      (if (string= old-string "")
          (progn
            (erase-buffer)
            (insert new-string)
            (write-file file-path)
            (format "Created new file: %s" file-path))
        (goto-char (point-min))
        (let ((count 0))
          (while (search-forward old-string nil t)
            (setq count (1+ count)))
          (if (= count 0)
              (format "Error: Could not find text to replace in %s" file-path)
            (if (> count 1)
                (format "Error: Found %d matches for the text to replace in %s" count file-path)
              (goto-char (point-min))
              (search-forward old-string)
              (replace-match new-string t t)
              (write-file file-path)
              (format "Successfully edited %s" file-path))))))))

(defun codel-replace (file-path content)
  "Completely replace contents of FILE-PATH with CONTENT."
  (with-temp-buffer
    (insert content)
    (write-file file-path)
    (format "File replaced: %s" file-path)))

(defun codel-view-buffer (buffer-name &optional limit offset)
  "View contents of BUFFER-NAME with optional LIMIT and OFFSET."
  (with-current-buffer buffer-name
    (let* ((lines (split-string (buffer-string) "\n"))
           (start (or offset 0))
           (end (if limit (min (+ start limit) (length lines)) (length lines)))
           (selected-lines (seq-subseq lines start end)))
      (string-join selected-lines "\n"))))

(defun codel-edit-buffer (buffer-name old-string new-string)
  "In BUFFER-NAME, replace OLD-STRING with NEW-STRING."
  (with-current-buffer buffer-name
    (let ((case-fold-search nil))
      (save-excursion
        (goto-char (point-min))
        (let ((count 0))
          (while (search-forward old-string nil t)
            (setq count (1+ count)))
          (if (= count 0)
              (format "Error: Could not find text to replace in buffer %s" buffer-name)
            (if (> count 1)
                (format "Error: Found %d matches for the text to replace in buffer %s" count buffer-name)
              (goto-char (point-min))
              (search-forward old-string)
              (replace-match new-string t t)
              (format "Successfully edited buffer %s" buffer-name))))))))

(defun codel-replace-buffer (buffer-name content)
  "Completely replace contents of BUFFER-NAME with CONTENT."
  (with-current-buffer buffer-name
    (erase-buffer)
    (insert content)
    (format "Buffer replaced: %s" buffer-name)))

(defvar codel-tools
  `((:name "Bash"
           :function ,#'codel-bash
           :description "Executes bash commands"
           :args ((:name "command"
                         :type string
                         :description "Command to execute"
                         :required t)
                  (:name "timeout"
                         :type number
                         :description "Optional timeout in milliseconds (max 600000)")))

    (:name "GlobTool"
           :function ,#'codel-glob-tool
           :description "File pattern matching"
           :args ((:name "pattern"
                         :type string
                         :description "Glob pattern to match files"
                         :required t)
                  (:name "path"
                         :type string
                         :description "Directory to search in")))

    (:name "GrepTool"
           :function ,#'codel-grep-tool
           :description "Content search using regex"
           :args ((:name "pattern"
                         :type string
                         :description "Regex pattern to search in file contents"
                         :required t)
                  (:name "include"
                         :type string
                         :description "File pattern to include in search")
                  (:name "path"
                         :type string
                         :description "Directory to search in")))

    (:name "LS"
           :function ,#'codel-ls
           :description "Lists files and directories"
           :args ((:name "path"
                         :type string
                         :description "Absolute path to directory to list"
                         :required t)
                  (:name "ignore"
                         :type array :items (:type string)
                         :description "Array of glob patterns to ignore")))

    (:name "View"
           :function ,#'codel-view
           :description "Reads files"
           :args ((:name "file_path"
                         :type string
                         :description "Absolute path to the file to read"
                         :required t)
                  (:name "limit"
                         :type number
                         :description "Number of lines to read")
                  (:name "offset"
                         :type number
                         :description "Line number to start reading from")))

    (:name "Edit"
           :function ,#'codel-edit
           :description "Edits files"
           :args ((:name "file_path"
                         :type string
                         :description "Absolute path to the file to modify"
                         :required t)
                  (:name "old_string"
                         :type string
                         :description "Text to replace (must match exactly)"
                         :required t)
                  (:name "new_string"
                         :type string
                         :description "Text to replace old_string with"
                         :required t)))

    (:name "Replace"
           :function ,#'codel-replace
           :description "Completely overwrites files"
           :args ((:name "file_path"
                         :type string
                         :description "Absolute path to file to write"
                         :required t)
                  (:name "content"
                         :type string
                         :description "Content to write to the file"
                         :required t)))

    (:name "ViewBuffer"
           :function ,#'codel-view-buffer
           :description "Reads Emacs buffers"
           :args ((:name "buffer_name"
                         :type string
                         :description "Name of the buffer to read"
                         :required t)
                  (:name "limit"
                         :type number
                         :description "Number of lines to read")
                  (:name "offset"
                         :type number
                         :description "Line number to start reading from")))

    (:name "EditBuffer"
           :function ,#'codel-edit-buffer
           :description "Edits Emacs buffers"
           :args ((:name "buffer_name"
                         :type string
                         :description "Name of the buffer to modify"
                         :required t)
                  (:name "old_string"
                         :type string
                         :description "Text to replace (must match exactly)"
                         :required t)
                  (:name "new_string"
                         :type string
                         :description "Text to replace old_string with"
                         :required t)))

    (:name "ReplaceBuffer"
           :function ,#'codel-replace-buffer
           :description "Completely overwrites buffer contents"
           :args ((:name "buffer_name"
                         :type string
                         :description "Name of the buffer to overwrite"
                         :required t)
                  (:name "content"
                         :type string
                         :description "Content to write to the buffer"
                         :required t)))))

;;;###autoload
(defun codel-setup-gptel ()
  "Register `codel' tools for use with `gptel'.
Replaces any existing tools with the same name."
  (interactive)
  (require 'gptel)
  (declare-function gptel-make-tool "gptel")
  (declare-function gptel-tool-name "gptel")
  (defvar gptel-tools)
  (mapcar
   (lambda (spec)
     (let ((tool (apply #'gptel-make-tool spec)))
       (setq gptel-tools
             (cons tool (seq-remove
                         (lambda (existing)
                           (string= (gptel-tool-name existing)
                                    (gptel-tool-name tool)))
                         gptel-tools)))))
   codel-tools))

;;;###autoload
(defun codel-setup-ai-org-chat ()
  "Register `codel' tools for use with `ai-org-chat'.
Replaces any existing tools with the same name."
  (interactive)
  (require 'ai-org-chat)
  (require 'llm)
  (declare-function ai-org-chat-register-tool "ai-org-chat")
  (declare-function llm-make-tool "llm")
  (mapcar
   #'ai-org-chat-register-tool
   (mapcar (lambda (spec)
             (apply #'llm-make-tool spec))
           codel-tools)))

(provide 'codel)
;;; codel.el ends here
