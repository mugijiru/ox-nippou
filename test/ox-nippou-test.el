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
