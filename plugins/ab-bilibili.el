;;; ab-bilibili.el ---                                  -*- lexical-binding: t; -*-

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

(require 'auto-browser)
(require 'auto-browser-vui)
(require 'auto-browser-db)

;;; Custom Variables
(defcustom ab-bilibili-url "https://t.bilibili.com/"
  "Bilibili dynamic page URL."
  :type 'string)

(defcustom ab-bilibili-play-command (executable-find "mpv")
  "The executable path or command used to play Bilibili videos."
  :type 'string)

(defcustom ab-bilibili-only-show-unread t
  "Non-nil means display only unread Bilibili videos."
  :type 'bool)

(defcustom ab-bilibili-create-table-sql "CREATE TABLE IF NOT EXISTS bilibili_videos (url TEXT PRIMARY KEY,author TEXT,title TEXT,date TEXT, unread INTEGER DEFAULT 1);"
  "SQL statement used to create the Bilibili videos table."
  :type 'string)

(defcustom ab-bilibili-insert-sql "INSERT OR IGNORE INTO bilibili_videos (url, author, title, date) VALUES ('%s', '%s', '%s', '%s');"
  "SQL template for inserting new Bilibili video records into the database."
  :type 'string)

(defcustom ab-bilibili-select-sql "SELECT url, author, title, date FROM bilibili_videos %s order by date desc;"
  "SQL template for selecting Bilibili video records from the database."
  :type 'string)

(defcustom ab-bilibili-update-sql "UPDATE bilibili_videos SET unread = 0 WHERE url = '%s';"
  "SQL template for marking a Bilibili video as read in the database."
  :type 'string)

(defcustom ab-bilibili-header '((author . (:on-click (lambda (id value) (ab-bilibili-vui-filter 'author value))))
                                (title)
                                (date . (:sorted t :reversed t)))
  "Table header configuration for Bilibili video list.
Each element is (COLUMN-KEY . WIDTH-RATIO) where COLUMN-KEY is used
to look up values and WIDTH-RATIO scales column width relative to window.")

;;; Internal Variables

(defun ab-bilibili-vui-filter (key value)
  (let* ((props (vui-instance-props auto-browser-vui--instance))
         (table (plist-get props :table)))
    (vui-update-props
     auto-browser-vui--instance
     (plist-put props
                :table
                (auto-browser-vui-filter-by key value table)))))

(defvar ab-bilibili--default-handle nil
  "Default handler function for rendering Bilibili video data.")

(defvar-local ab-bilibili--vui-instance nil
  "Cached VUI component instance for the Bilibili table, reused across renders.")

;;; Internal Functions

(defun ab-bilibili--vui-render (items)
  ""
  (with-current-buffer (get-buffer-create "*auto-browser-bilibili*")
    (setq auto-browser-vui-selected-actions #'ab-bilibili-vui-actions)
    (auto-browser-vui-render
     :buffer-name "*auto-browser-bilibili*"
     :title "Bilibili"
     :id-key 'url
     :name 'ab-bilibili
     :headers ab-bilibili-header
     :table items)))

(defun ab-bilibili--db-insert-videos (items)
  "Insert a list of Bilibili video items into the database."
  (mapc #'ab-bilibili--db-insert-item items))

(defun ab-bilibili--db-create-table ()
  "Create the Bilibili video table in the database."
  (auto-browser-db-execute ab-bilibili-create-table-sql))

(defun ab-bilibili--db-insert-item (table)
  "Insert a single Bilibili video item into the database."
  (auto-browser-db-execute (format ab-bilibili-insert-sql
                                   (gethash 'url table)
                                   (string-replace "'" "" (gethash 'author table))
                                   (string-replace "'" ""(gethash 'title table))
                                   (gethash 'date table))))

(defun ab-bilibili--date-compute (date-diff)
  "Compute a time by subtracting the given offset from the current time."
  (time-subtract (current-time) date-diff))

(defun ab-bilibili--date-parse (date-str)
  "Parse a relative date string into a formatted absolute date and time string."
  (let ((date-diff-str (car (split-string date-str "·" t " +"))))
    (pcase date-diff-str
      ((pred (string-suffix-p "天前")) (format-time-string "%Y-%m-%d 00:00" (ab-bilibili--date-compute (* 24 3600 (string-to-number (substring date-diff-str 0 (- 0 2)))))))
      ((pred (string-prefix-p "昨天")) (concat (format-time-string "%Y-%m-%d" (ab-bilibili--date-compute (* 24 3600 1))) (substring date-diff-str 2)))
      ((pred (string-suffix-p "小时前")) (format-time-string "%Y-%m-%d %H:00" (ab-bilibili--date-compute (* 3600 (string-to-number (substring date-diff-str 0 (- 0 3)))))))
      ((pred (string-suffix-p "分钟前")) (format-time-string "%Y-%m-%d %H:%M" (ab-bilibili--date-compute (* 60 (string-to-number (substring date-diff-str 0 (- 0 3))))))))))

(defun ab-bilibili--item-parse (item)
  "Parse a Bilibili item string and return a hash table containing its metadata."
  (when-let* ((table (make-hash-table))
              (author (string-trim (dom-text(auto-browser-dom-query-selector-first (auto-browser-dom-parse-html item) ".bili-dyn-title__text"))))
              (title (string-trim (dom-text(auto-browser-dom-query-selector-first (auto-browser-dom-parse-html item) ".bili-dyn-card-video__title"))))
              (url (concat "https:" (dom-attr (auto-browser-dom-query-selector-first (auto-browser-dom-parse-html item) ".bili-dyn-card-video") 'href)))
              (date (ab-bilibili--date-parse (string-trim (dom-text(auto-browser-dom-query-selector-first (auto-browser-dom-parse-html item) ".bili-dyn-time"))))))
    (puthash 'author author table)
    (puthash 'title title table)
    (puthash 'url url table)
    (puthash 'date date table)
    table))

(defun ab-bilibili--htmls-parse (id &rest lst)
  ""
  (funcall ab-bilibili--default-handle (remove nil (mapcar #'ab-bilibili--item-parse lst))))

;;; Interactive Functions
(defun ab-bilibili-db-check-videos ()
  "Create the database table and insert dynamic Bilibili videos."
  (interactive)
  (ab-bilibili--db-create-table)
  (ab-bilibili-dynamic-videos #'ab-bilibili--db-insert-videos))

(defun ab-bilibili-db-videos ()
  "Update and display Bilibili dynamic videos from the database in a formatted table."
  (interactive)
  (ab-bilibili-db-check-videos)
  (ab-bilibili--vui-render
   (mapcar (lambda (item)
             (let ((table (make-hash-table)))
               (puthash 'url (nth 0 item) table)
               (puthash 'author (nth 1 item) table)
               (puthash 'title (nth 2 item) table)
               (puthash 'date (nth 3 item) table)
               table))
           (auto-browser-db-select
            (format ab-bilibili-select-sql
                    (if ab-bilibili-only-show-unread
                        "where unread = 1"
                      ""))))))

(defun ab-bilibili-dynamic-videos (&optional handle)
  "Fetch and parse Bilibili dynamic videos from the web using an optional handler."
  (interactive)
  (let* ((url (file-name-concat ab-bilibili-url "?tab=video"))
         (selector ".bili-dyn-item__main"))
    (setq ab-bilibili--default-handle (or handle nil))
    (auto-browser-run-linearly
     `((auto-browser-get-tab ,url t)
       (auto-browser-refresh)
       (auto-browser-wait-for-timeout 500)
       (auto-browser-locate-elements ,selector)
       (auto-browser-get-elements "html")
       (ab-bilibili--htmls-parse)))))

(defun ab-bilibili--mark-read (urls)
  "Mark each URL in URLS as read in the database and refresh the displayed video list."
  (let ((point (point)))
    (mapc #'auto-browser-db-execute (mapcar (lambda (url) (format ab-bilibili-update-sql url)) urls))
    (ab-bilibili-db-videos)
    (goto-char point)))

(defun ab-bilibili-mark-read (&optional url)
  "Mark the selected Bilibili video as read in the database and refresh the displayed video list."
  (interactive)
  (when (= 1 (auto-browser-db-execute (format ab-bilibili-update-sql url)))
    (ab-bilibili-db-videos)))

(defun ab-bilibili-open (&optional url)
  "Open the selected Bilibili video URL in a web browser and mark it as read in the database."
  (interactive)
  (browse-url url)
  (ab-bilibili-mark-read url))

(defun ab-bilibili-play (&optional url)
  "Play the selected Bilibili video using the configured external player and mark it as read in the database."
  (interactive)
  (start-process "*ab-bilibili-play*" nil ab-bilibili-play-command url)
  (ab-bilibili-mark-read url))

(defun ab-bilibili-vui-actions (url)
  "Actions for Auto Browser Bilibili Vui Table."
  (transient-define-prefix ab-bilibili-vui-actions-run ()
    "Actions for Auto Browser Bilibili."
    ["Auto Browser Bilibili"
     ("o" "Open in Browser"
      (lambda () (interactive) (ab-bilibili-open url)))
     ("p" "Play the video"
      (lambda () (interactive) (ab-bilibili-play url)))
     ("m" "Mark as Read"
      (lambda () (interactive)
        (if auto-browser-vui--selected
            (ab-bilibili--mark-read auto-browser-vui--selected)
          (ab-bilibili--mark-read (list url)))
        (setq auto-browser-vui--selected nil)))])

  (ab-bilibili-vui-actions-run))

(provide 'ab-bilibili)
;;; ab-bilibili.el ends here
