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

(defun ox-nippou--identify-journal-file (&optional date)
  "Find the journal org file for DATE."
  (let ((date (or date (current-time)))
        (file-name (format-time-string ox-nippou-journal-file-format date)))
    (expand-file-name file-name ox-nippou-journal-directory)))

(provide 'ox-nippou)
;;; ox-nippou.el ends here
