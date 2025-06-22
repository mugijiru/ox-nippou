;;; ox-nippou.el --- Export nippou from org-mode file  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mugijiru

;; Author: mugijiru <106833+mugijiru@users.noreply.github.com>
;; URL: https://github.com/mugijiru/ox-nippou
;; Keywords: tools, docs
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1") (org "9.7") (markdown-mode "2.5"))
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

(require 'cl)
(require 'markdown-mode)

(defcustom ox-nippou-journal-directory "~/org/journal"
  "Directory where journal org files are stored."
  :type 'directory
  :group 'ox-nippou)

(defcustom ox-nippou-journal-file-format "%Y%m%d.org"
  "Format for journal org file names."
  :type 'string
  :group 'ox-nippou)

(defcustom ox-nippou-todo-state-mapping '(("todo" . '("TODO"))
                                          ("doing" . '("DOING" "WAITING"))
                                          ("done" . '("DONE")))
  "Mapping of nippou states to org todo states."
  :type '((alist :key-type string :value-type (repeat string)))
  :group 'ox-nippou)

(defcustom ox-nippou-no-tasks-string "No tasks"
  "String to display when there are no tasks in the nippou."
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
  (let* ((title (org-element-property :title headline))
         (category (org-element-property :CATEGORY headline))
         (todo (org-element-property :todo-keyword headline))
         (pull-request (org-element-property :PULL_REQUEST headline)))
    (list :title title :todo (or todo "TODO") :category category :pull-request pull-request)))

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
           (children (ox-nippou--extract-child-headlines tasks-container))
           (filtered-children
            (seq-filter (lambda (child)
                          (null (org-element-property :IGNORE_NIPPOU child)))
                        children)))
      (seq-map #'ox-nippou--parse-journal-task filtered-children))))

(defun ox-nippou--categorize-tasks (tasks)
  "Categorize TASKS based on their todo state using `loop` macro.

This function returns a hash table where keys are todo states.
"
  (let ((categorized-tasks (make-hash-table :test 'equal))
        (result))
    (loop for task in tasks
          for todo = (plist-get task :todo)
          for state = (or (car (cl-rassoc todo ox-nippou-todo-state-mapping
                                          :test (lambda (target journal-states)
                                                  (seq-contains-p (nth 1 journal-states) target))))
                          "unknown")
          do (puthash state
                      (cons task (gethash state categorized-tasks))
                      categorized-tasks))
    (loop for state in (mapcar 'car ox-nippou-todo-state-mapping)
          do (let ((tasks (gethash state categorized-tasks)))
               (push (list state tasks) result)))
    (nreverse result)))

(defun ox-nippou--generate-text-from-task (task)
  "Generate text representation of TASK."
  (let* ((title (plist-get task :title))
         (todo (plist-get task :todo))
         (category (plist-get task :category))
         (text (if category
                   (format "%s: %s" category title)
                 title)))
    (concat "- ["
            (if (string= todo "DONE") "x" " ")
            "] "
            text
            (when-let ((pull-request (plist-get task :pull-request)))
              (format "\n  - %s" pull-request)))))

(defun ox-nippou--generate-nippou-content (category-ordered-tasks)
  "Generate nippou content from CATEGORY-ORDERD-TASKS."
  (mapconcat (lambda (one-category-tasks)
               (let* ((key (car one-category-tasks))
                      (heading (concat "# " key "\n\n"))
                      (prefix (if (string= key "done")
                                  "- [x] "
                                "- [ ] "))
                      (tasks (nth 1 one-category-tasks)))
                 (if tasks
                     (concat heading
                             (mapconcat
                              'ox-nippou--generate-text-from-task
                              tasks "\n"))
                   (concat heading prefix ox-nippou-no-tasks-string))))
             category-ordered-tasks "\n\n"))

;;; autoloads
(defun ox-nippou-export-as-nippou (&optional date)
  "Export the journal tasks as nippou for DATE."
  (interactive)
  (let* ((journal-file (ox-nippou--identify-journal-file date))
         (org-content (with-temp-buffer
                        (insert-file-contents journal-file)
                        (buffer-string)))
         (tasks (ox-nippou--parse-journal org-content))
         (categorized-tasks (ox-nippou--categorize-tasks tasks))
         (nippou-content (ox-nippou--generate-nippou-content (nreverse categorized-tasks)))
         (buffer (get-buffer-create (concat "*Org export nippou* " (format-time-string "%Y-%m-%d" (or date (current-time)))))))
    (switch-to-buffer buffer)
    (erase-buffer)
    (insert nippou-content)
    (markdown-view-mode)))

(provide 'ox-nippou)
;;; ox-nippou.el ends here
