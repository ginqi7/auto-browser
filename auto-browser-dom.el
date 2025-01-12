;;; auto-browser-dom.el --- Dom parser for Auto Browser.  -*- lexical-binding: t; -*-

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

(require 'dom)
(require 'seq)

(defun auto-browser-dom--parse-selector-str (selector-str)
  "
1. Tag Selector: div
2. Class Selector: .class
3. Id Selector: #id
4. Offspring Selector: div p
5. attribute selector: div[type=email]"
  (setq selector-str (string-trim selector-str))
  (if (string-match " " selector-str)
      (mapcar #'auto-browser-dom--parse-selector-str (split-string selector-str " "))
    (cond ((string-prefix-p "." selector-str)
           (append '(:class)
                   (auto-browser-dom--parse-selector-attribute
                    (substring selector-str 1))))
          ((string-prefix-p "#" selector-str)
           (append '(:id) (auto-browser-dom--parse-selector-attribute
                           (substring selector-str 1))))
          (t (append '(:tag) (auto-browser-dom--parse-selector-attribute
                              selector-str))))))

(defun auto-browser-dom--parse-selector-attribute (selector-str)
  (if (string-match "=" selector-str)
      (when (string-match "\\(.*\\)\\[\\(.*\\)=\\(.*\\)\\]" selector-str)
        (list
         (match-string 1 selector-str)
         :attr-key
         (match-string 2 selector-str)
         :attr-value
         (match-string 3 selector-str)))
    (list selector-str)))

(defun auto-browser-dom-search (dom selector)
  (dom-search
   dom
   (lambda (node)
     (let (match)
       (setq match
             (or
              (when (plist-get selector :tag)
                (string= (plist-get selector :tag) (dom-tag node)))
              (when (plist-get selector :class)
                (string= (plist-get selector :class) (dom-attr node 'class)))
              (when (plist-get selector :id)
                (string= (plist-get selector :id) (dom-attr node 'id)))))
       (when (and match (plist-get selector :attr-key))
         (setq match
               (string= (plist-get selector :attr-value)
                        (dom-attr node (intern (plist-get selector :attr-key))))))
       match))))


(defun auto-browser-dom--query-selector-all (doms selectors)
  (if selectors
      (auto-browser-dom--query-selector-all
       (mapcan (lambda (dom)
                 (auto-browser-dom-search dom (car selectors)))
               doms)
       (cdr selectors))
    doms))

(defun auto-browser-dom-query-selector-all (dom selector-str)
  (let ((one-or-more-selectors (auto-browser-dom--parse-selector-str selector-str)))
    (if (symbolp (car one-or-more-selectors))
        (auto-browser-dom-search dom one-or-more-selectors)
      (auto-browser-dom--query-selector-all (list dom) one-or-more-selectors))))

(provide 'auto-browser-dom)
;;; auto-browser-dom.el ends here
