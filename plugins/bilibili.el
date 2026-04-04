;;; bilibili.el ---                                  -*- lexical-binding: t; -*-

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

(defcustom ab-bilibili-url "https://t.bilibili.com/"
  "Bilibili dynamic page URL."
  :type 'string)

(defcustom ab-bilibili-ctable-header '((author . 0.3) (title . 0.7))
  "Table header configuration for Bilibili video list.
Each element is (COLUMN-KEY . WIDTH-RATIO) where COLUMN-KEY is used
to look up values and WIDTH-RATIO scales column width relative to window."
  :type '(repeat (cons symbol number)))

(defun ab-bilibili-dynamic-videos ()
  "Fetch video list from Bilibili dynamic page and display in a table."
  (interactive)
  (let* ((url (file-name-concat ab-bilibili-url "?tab=video"))
         (selector ".bili-dyn-item__main"))
    (auto-browser-run-linearly
     `((auto-browser-get-tab ,url t)
       (auto-browser-locate-elements ,selector)
       (auto-browser-get-elements "html")
       (ab-bilibili-ctable-render)))))

(defun ab-bilibili-ctable-open ()
  "Open the selected video URL from the Bilibili table in a browser."
  (interactive)
  (let* ((cp (ctbl:cp-get-component))
         (row (ctbl:cp-get-selected-data-row cp))
         (item (car (last row)))
         (url (gethash 'url item)))
    (browse-url url)))

(defun ab-bilibili-ctable-render (id &rest lst)
  "Parse Bilibili video items from HTML and render as a clickable table.

ID is the trace ID from the browser workflow.
LST is a list of HTML strings representing video items.

Each video is parsed to extract author, title, and URL, then displayed
using `auto-browser-ctable-render'. Clicking a row opens the video URL."
  (let ((result))
    (dolist (item lst)
      (let ((table (make-hash-table))
            (author (string-trim (dom-text(auto-browser-dom-query-selector-first (auto-browser-dom-parse-html item) ".bili-dyn-title__text"))))
            (title (string-trim (dom-text(auto-browser-dom-query-selector-first (auto-browser-dom-parse-html item) ".bili-dyn-card-video__title"))))
            (url (concat "https:" (dom-attr (auto-browser-dom-query-selector-first (auto-browser-dom-parse-html item) ".bili-dyn-card-video") 'href))))
        (puthash 'author author table)
        (puthash 'title title table)
        (puthash 'url url table)
        (push table result)))
    (auto-browser-ctable-render :buffer-name "*auto-browser-bilibili*"
                                :headers ab-bilibili-ctable-header
                                :table (reverse result)
                                :actions #'ab-bilibili-ctable-open)))

(provide 'bilibili)
;;; bilibili.el ends here
