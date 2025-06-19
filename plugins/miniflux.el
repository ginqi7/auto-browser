;;; miniflux.el --- Miniflux for Auto Browser.               -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Qiqi Jin

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
(require 'auto-browser-dom)

(defcustom auto-browser-miniflux-base-url ""
  "The miniflux base URL."
  :type 'string)

(defcustom auto-browser-miniflux-rendering-functions nil
  "The miniflux rendering functions like `shr-external-rendering-functions`"
  :type 'cons)

(defun auto-browser-miniflux-open-unread ()
  "Open miniflux unread page."
  (interactive)
  (let ((url (file-name-concat auto-browser-miniflux-base-url "unread")))
    (auto-browser-run-linearly
     `((auto-browser-get-tab ,url)
       (auto-browser-locate-element "#main")
       (auto-browser-get-element "html")
       (auto-browser-miniflux-render-unread)))))

(defun auto-browser-miniflux-open-article ()
  "Open miniflux article page."
  (interactive)
  (let ((url (file-name-concat auto-browser-miniflux-base-url (tabulated-list-get-id))))
    (auto-browser-run-linearly
     `((auto-browser-get-tab ,url)
       (auto-browser-rewrite-image-to-base64)
       (auto-browser-locate-element "#main")
       (auto-browser-get-element "html")
       (auto-browser-miniflux-render-article)))))

(defun auto-browser-miniflux-render-article (trace-id html)
  "Render article HTML."
  (let ((shr-external-rendering-functions auto-browser-miniflux-rendering-functions))
    (auto-browser-render-html html "*miniflux-article*")))

(defun auto-browser-miniflux-render-unread (trace-id html)
  "Render Unread List by tabulated-list."
  (switch-to-buffer "*miniflux-unread*")
  (let ((buffer-read-only))
    (erase-buffer)
    (setq tabulated-list-entries
          (mapcar (lambda (item)
                    (list (plist-get item :url)
                          (vector
                           (plist-get item :title)
                           (plist-get item :author)
                           (plist-get item :time))))
                  (auto-browser-miniflux-html-parse html)))
    (miniflux-unread-mode)
    (goto-char (point-min))))

(define-derived-mode miniflux-unread-mode tabulated-list-mode "Contexts Menu"
  "Major mode for handling a list of miniflux unread."
  (setq tabulated-list-format [("Title" 50 t)
                               ("Author" 20 t :right-align t)
                               ("Time"  10 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'auto-browser-miniflux-open-article)
    (use-local-map map)))

(defun auto-browser-miniflux-html-parse (html)
  "Pares unread list HTML."
  (let* ((dom (auto-browser-dom-parse-html html))
         (title-doms (auto-browser-dom-query-selector-all dom "article header h2 a"))
         (titles (mapcar #'string-trim (mapcar #'dom-text title-doms)))
         (urls (mapcar (lambda (node) (dom-attr node 'href)) title-doms))
         (authors (mapcar
                   #'string-trim
                   (mapcar #'dom-text
                           (auto-browser-dom-query-selector-all dom ".item-meta-info-title a"))))
         (date (mapcar (lambda (node) (dom-attr node 'datetime))
                       (auto-browser-dom-query-selector-all dom ".item-meta-info-timestamp time")))
         (data))
    (cl-loop for title in titles
             for author in authors
             for time in date
             for url in urls
             do
             (setq data (append data
                                (list (list :title title
                                            :author author
                                            :time time
                                            :url url)))))
    data))

(provide 'miniflux)

;;; mini flux.el ends here
