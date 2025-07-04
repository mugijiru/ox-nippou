;; -*- lexical-binding: t; -*-

(require 'ox-nippou)
(require 'ert)

(ert-deftest test-ox-nippou--identify-journal-file ()
  "Test the ox-nippou--find-journal-file function."
  (let ((ox-nippou-journal-directory "/tmp")
        (ox-nippou-journal-file-format "%Y%m%d.org"))
    (should (string= (ox-nippou--identify-journal-file)
                     (format-time-string "/tmp/%Y%m%d.org")))))

(ert-deftest test-ox-nippou--identify-journal-file-with-date-argument ()
  "Test the ox-nippou--find-journal-file function with a specific date."
  (let ((ox-nippou-journal-directory "/tmp")
        (ox-nippou-journal-file-format "%Y%m%d.org")
        (date (encode-time 0 0 0 1 1 2025)))
    (should (string= (ox-nippou--identify-journal-file date)
                     "/tmp/20250101.org"))))

(ert-deftest test-ox-nippou--parse-journal ()
  "Test the ox-nippou--parse-journal function with simple content."
  (let ((org-content "* 22日(日)\n** Tasks\n*** TODO Task 1\n*** DONE Task 2"))
    (should (equal (ox-nippou--parse-journal org-content)
                   '((:title "Task 1" :todo "TODO" :category nil :pull-request nil :document-url nil)
                     (:title "Task 2" :todo "DONE" :category nil :pull-request nil :document-url nil))))))

(ert-deftest test-ox-nippou--parse-journal-with-category ()
  "Test the ox-nippou--parse-journal function with category."
  (let ((org-content "* 22日(日)
** Tasks
*** TODO Task 1
:PROPERTIES:
:CATEGORY: category1
:END:
*** DONE Task 2"))
    (should (equal (ox-nippou--parse-journal org-content)
                   '((:title "Task 1" :todo "TODO" :category "category1" :pull-request nil :document-url nil)
                     (:title "Task 2" :todo "DONE" :category nil :pull-request nil :document-url nil))))))

(ert-deftest test-ox-nippou--parse-journal-with-pr-url ()
  "Test the ox-nippou--parse-journal function with category."
  (let ((org-content "* 22日(日)
** Tasks
*** TODO Task 1
:PROPERTIES:
:PULL_REQUEST: https://github.com/mugijiru/ox-nippou/pull/3
:END:
*** DONE Task 2"))
    (should (equal (ox-nippou--parse-journal org-content)
                   '((:title "Task 1" :todo "TODO" :category nil :pull-request "https://github.com/mugijiru/ox-nippou/pull/3" :document-url nil)
                     (:title "Task 2" :todo "DONE" :category nil :pull-request nil :document-url nil))))))

(ert-deftest test-ox-nippou--parse-journal-with-document-url ()
  "Test the ox-nippou--parse-journal function with document URL."
  (let ((org-content "* 22日(日)
** Tasks
*** TODO Task 1
:PROPERTIES:
:DOCUMENT_URL: https://kibe.la/mugijiru/3
:END:
*** DONE Task 2"))
    (should (equal (ox-nippou--parse-journal org-content)
                   '((:title "Task 1" :todo "TODO" :category nil :pull-request nil :document-url "https://kibe.la/mugijiru/3")
                     (:title "Task 2" :todo "DONE" :category nil :pull-request nil :document-url nil))))))

(ert-deftest test-ox-nippou--parse-journal-with-ignore-nippou-property ()
  "Test the ox-nippou--parse-journal function with IGNORE_NIPPOU property."
  (let ((org-content "* 22日(日)
** Tasks
*** TODO Task 1
:PROPERTIES:
:IGNORE_NIPPOU: t
:END:
*** DONE Task 2"))
    (should (equal (ox-nippou--parse-journal org-content)
                   '((:title "Task 2" :todo "DONE" :category nil :pull-request nil :document-url nil))))))


(ert-deftest test-ox-nippou--categorize-tasks ()
  "Test the ox-nippou--categorize-tasks function with simple tasks."
  (let* ((tasks '((:title "Task 1" :todo "TODO" :category "Foo" :pull-request "https://github.com/mugijiru/ox-nippou/pull/3" :document-url nil)
                  (:title "Task 2" :todo "DONE" :category "Foo" :pull-request nil :document-url "https://kibe.la/mugijiru/3")
                  (:title "Task 3" :todo "DOING" :category "Bar" :pull-request nil :document-url nil)
                  (:title "Task 4" :todo "DOING" :category nil :pull-request nil :document-url nil)))
         (result (ox-nippou--categorize-tasks tasks)))
    (should (equal '("todo" "doing" "done")
                   (mapcar 'car result)))
    (should (seq-set-equal-p (nth 1 (assoc "todo" result))
                             '((:title "Task 1" :todo "TODO" :category "Foo" :pull-request "https://github.com/mugijiru/ox-nippou/pull/3" :document-url nil))))
    (should (seq-set-equal-p (nth 1 (assoc "done" result))
                             '((:title "Task 2" :todo "DONE" :category "Foo" :pull-request nil :document-url "https://kibe.la/mugijiru/3"))))
    (should (seq-set-equal-p (nth 1 (assoc "doing" result))
                             '((:title "Task 3" :todo "DOING" :category "Bar" :pull-request nil :document-url nil)
                               (:title "Task 4" :todo "DOING" :category nil :pull-request nil :document-url nil))))))

(ert-deftest test-ox-nippou--generate-nippou-content ()
  "Test the ox-nippou--generate-nippou-content function with categorized tasks."
  (let* ((categorized-tasks)
         (todo-tasks '((:title "Task 1" :todo "TODO" :category nil :pull-request "https://github.com/mugijiru/ox-nippou/pull/3" :document-url nil)))
         (done-tasks '((:title "Task 2" :todo "DONE" :category nil :pull-request nil :document-url "https://kibe.la/mugijiru/3")))
         (doing-tasks '((:title "Task 3" :todo "DOING" :category nil :pull-request nil :document-url nil)
                        (:title "Task 4" :todo "WAITING" :category nil :pull-request nil :document-url nil))))
    (push `("todo" ,todo-tasks) categorized-tasks)
    (push `("doing" ,doing-tasks) categorized-tasks)
    (push `("done" ,done-tasks) categorized-tasks)
    (let ((result (ox-nippou--generate-nippou-content categorized-tasks)))
      (should (string= "# done

- [x] Task 2
  - https://kibe.la/mugijiru/3

# doing

- [ ] Task 3
- [ ] Task 4

# todo

- [ ] Task 1
  - https://github.com/mugijiru/ox-nippou/pull/3"
                              result)))))

(ert-deftest test-ox-nippou--generate-nippou-content-with-empty-state ()
  "Test the ox-nippou--generate-nippou-content function with categorized tasks."
  (let* ((categorized-tasks)
         (done-tasks '())
         (doing-tasks '())
         (todo-tasks '((:title "Task 1" :todo "TODO" :category nil :pull-request nil :document-url nil))))
    (push `("todo" ,todo-tasks) categorized-tasks)
    (push `("doing" ,doing-tasks) categorized-tasks)
    (push `("done" ,done-tasks) categorized-tasks)
    (let ((result (ox-nippou--generate-nippou-content categorized-tasks)))
      (should (string= "# done

- [x] No tasks

# doing

- [ ] No tasks

# todo

- [ ] Task 1"
                              result)))))

(ert-deftest test-ox-nippou--generate-nippou-content-with-customized-no-tasks-string ()
  "Test the ox-nippou--generate-nippou-content function with categorized tasks."
  (let* ((categorized-tasks)
         (done-tasks '())
         (doing-tasks '())
         (ox-nippou-no-tasks-string ":pear:")
         (todo-tasks '((:title "Task 1" :todo "TODO" :category nil :pull-request nil :document-url nil))))
    (push `("todo" ,todo-tasks) categorized-tasks)
    (push `("doing" ,doing-tasks) categorized-tasks)
    (push `("done" ,done-tasks) categorized-tasks)
    (let ((result (ox-nippou--generate-nippou-content categorized-tasks)))
      (should (string= "# done

- [x] :pear:

# doing

- [ ] :pear:

# todo

- [ ] Task 1"
                              result)))))

(ert-deftest test-ox-nippou--generate-nippou-content-with-category ()
  "Test the ox-nippou--generate-nippou-content function with categorized tasks."
  (let* ((categorized-tasks)
         (todo-tasks '((:title "Task 1" :todo "TODO" :category "Foo" :pull-request nil :document-url nil)))
         (done-tasks '((:title "Task 2" :todo "DONE" :category "Foo" :pull-request nil :document-url nil)))
         (doing-tasks '((:title "Task 3" :todo "DOING" :category "Bar" :pull-request nil :document-url nil)
                        (:title "Task 4" :todo "WAITING" :category nil :pull-request nil :document-url nil))))
    (push `("todo" ,todo-tasks) categorized-tasks)
    (push `("doing" ,doing-tasks) categorized-tasks)
    (push `("done" ,done-tasks) categorized-tasks)
    (let ((result (ox-nippou--generate-nippou-content categorized-tasks)))
      (should (string= "# done

- [x] Foo: Task 2

# doing

- [ ] Bar: Task 3
- [ ] Task 4

# todo

- [ ] Foo: Task 1"
                              result)))))

(ert-deftest test-ox-nippou-export-as-nippou ()
  "Test the ox-nippou--parse-journal-task function with a simple headline."
  (let ((org-todo-keywords '("TODO" "DOING" "|" "DONE"))
        (ox-nippou-journal-directory "/tmp")
        (ox-nippou-journal-file-format "%Y%m%d.org"))
    (with-temp-buffer
      (insert "* 22日(日)
** Tasks
*** DONE completed task
*** DONE completed task 2
:PROPERTIES:
:PULL_REQUEST: https://github.com/mugijiru/ox-nippou/pull/3
:END:
*** DOING progress task
:PROPERTIES:
:DOCUMENT_URL: https://kibe.la/mugijiru/3
:END:
*** DOING ignored task
:PROPERTIES:
:IGNORE_NIPPOU: t
:END:
*** TODO new task
** Other Headline
")
      (write-region (point-min) (point-max) "/tmp/20220122.org"))
    (with-temp-buffer
      (ox-nippou-export-as-nippou (encode-time 0 0 0 22 1 2022))
      (text-mode)
      (let ((nippou-content (buffer-substring-no-properties (point-min) (point-max))))
        (should (string= nippou-content
                         "# done

- [x] completed task 2
  - https://github.com/mugijiru/ox-nippou/pull/3
- [x] completed task

# doing

- [ ] progress task
  - https://kibe.la/mugijiru/3

# todo

- [ ] new task")))
      (kill-current-buffer))))
