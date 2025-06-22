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
