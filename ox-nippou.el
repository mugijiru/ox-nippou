;;; ox-nippou.el --- Export nippou from org-mode file  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mugijiru

;; Author: mugijiru <106833+mugijiru@users.noreply.github.com>
;; URL: https://github.com/mugijiru/ox-nippou
;; Keywords: tools, docs
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1") (org-mode "9.7"))
;; Keywords: tools, docs

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

;;; Code:

(defcustom ox-nippou-journal-directory "~/org/journal"
  "Directory where journal org files are stored."
  :type 'directory
  :group 'ox-nippou)

(defcustom ox-nippou-journal-file-format "%Y%m%d.org"
  "Format for journal org file names."
  :type 'string
  :group 'ox-nippou)

(defconst ox-nippou-journal-tasks-heading "Tasks"
  "Heading for tasks in the journal.")

(defun ox-nippou--identify-journal-file (&optional date)
  "Find the journal org file for DATE."
  (let ((date (or date (current-time)))
        (file-name (format-time-string ox-nippou-journal-file-format date)))
    (expand-file-name file-name ox-nippou-journal-directory)))

(defun ox-nippou--find-journal-tasks-container (org-data)
  "Find the tasks container in the org data structure."
  (let ((tasks-heading (org-element-map org-data 'headline
                         (lambda (hl)
                           (when (string= (org-element-property :title hl) ox-nippou-journal-tasks-heading)
                             hl))
                         nil t)))
    (if tasks-heading
        tasks-heading
      (error "No tasks heading found in the journal file"))))

(defun ox-nippou--parse-journal-task (headline)
  "Parse a task from a org HEADLINE."
  (message "Parsing task: %s" (org-element-property :title headline))
  (let* ((title (org-element-property :title headline))
         (todo (org-element-property :todo-keyword headline)))
    (list :title title :todo (or todo "TODO"))))

(defun ox-nippou--extract-child-headlines (element)
  "Extract child headlines from ELEMENT."
  (let ((children (org-element-contents element)))
    (seq-filter
     (lambda (child)
       (eq (org-element-type child) 'headline))
     children)))

(defun ox-nippou--parse-journal (org-content)
  "Parse the journal content from ORG-CONTENT.

org-content format as

* DATE
** Tasks
*** TODO-KEYWORD TASK_NAME
** Other Headline

This function returns a list of tasks with their titles and todo keywords."
  (with-temp-buffer
    (insert org-content)
    (org-mode)
    (let* ((org-data (org-element-parse-buffer 'element))
           (tasks-container (ox-nippou--find-journal-tasks-container org-data))
           (children (ox-nippou--extract-child-headlines tasks-container)))
      (seq-map #'ox-nippou--parse-journal-task children))))

(provide 'ox-nippou)
;;; ox-nippou.el ends here
