;;; auto-browser-db.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords:

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

;;

;;; Code:

(defcustom auto-browser-db (file-name-concat user-emacs-directory "auto-browser.sqlite")
  "Path to the SQLite database file used for storing auto-browser data.")

(defcustom auto-browser-db-log-sql-p nil
  "Non-nil means log the SQL statements executed on the database.")

(defun auto-browser-db-execute (sql)
  "Execute the given SQL statement on the database specified by auto-browser-db."
  (let* ((db (sqlite-open auto-browser-db))
         (data (sqlite-execute db sql)))
    (when auto-browser-db-log-sql-p
      (print sql))
    (sqlite-close db)
    data))

(defun auto-browser-db-select (sql)
  "Execute the given SQL select statement on the database specified by auto-browser-db and return the resulting records."
  (let* ((db (sqlite-open auto-browser-db))
         (data (sqlite-select db sql)))
    (when auto-browser-db-log-sql-p
      (print sql))
    (sqlite-close db)
    data))

(provide 'auto-browser-db)
;;; auto-browser-db.el ends here
