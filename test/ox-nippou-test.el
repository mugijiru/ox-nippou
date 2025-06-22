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
                   '((:title "Task 1" :todo "TODO")
                     (:title "Task 2" :todo "DONE"))))))

(ert-deftest test-ox-nippou--categorize-tasks ()
  "Test the ox-nippou--categorize-tasks function with simple tasks."
  (let* ((tasks '((:title "Task 1" :todo "TODO")
                  (:title "Task 2" :todo "DONE")
                  (:title "Task 3" :todo "DOING")
                  (:title "Task 4" :todo "WAITING")))
         (result (ox-nippou--categorize-tasks tasks)))
    (should (equal '("todo" "doing" "done")
                   (mapcar 'car result)))
    (should (seq-set-equal-p (nth 1 (assoc "todo" result))
                             '((:title "Task 1" :todo "TODO"))))
    (should (seq-set-equal-p (nth 1 (assoc "done" result))
                             '((:title "Task 2" :todo "DONE"))))
    (should (seq-set-equal-p (nth 1 (assoc "doing" result))
                             '((:title "Task 3" :todo "DOING")
                               (:title "Task 4" :todo "WAITING"))))))

(ert-deftest test-ox-nippou--generate-nippou-content ()
  "Test the ox-nippou--generate-nippou-content function with categorized tasks."
  (let* ((categorized-tasks)
         (todo-tasks '((:title "Task 1" :todo "TODO")))
         (done-tasks '((:title "Task 2" :todo "DONE")))
         (doing-tasks '((:title "Task 3" :todo "DOING")
                        (:title "Task 4" :todo "WAITING"))))
    (push `("todo" ,todo-tasks) categorized-tasks)
    (push `("doing" ,doing-tasks) categorized-tasks)
    (push `("done" ,done-tasks) categorized-tasks)
    (let ((result (ox-nippou--generate-nippou-content categorized-tasks)))
      (should (string= "# done

- [ ] Task 2

# doing

- [ ] Task 3
- [ ] Task 4

# todo

- [ ] Task 1"
                              result)))))
