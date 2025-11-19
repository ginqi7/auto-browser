;;; web-ai.el --- Web AI for Auto Browser.           -*- lexical-binding: t; -*-

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

(defcustom auto-browser-web-ai-url ""
  "Web AI URL."
  :type 'string)

(defcustom auto-browser-web-ai-input-selector ".n-input__textarea-el"
  "The selector of the input element."
  :type 'string)

(defcustom auto-browser-web-ai-response-regexp "(\\d+)?$"
  "The AI response matched URL regexp. python regexp."
  :type 'string)

(defun auto-browser-web-ai-input ()
  "Send Input string to Web AI."
  (interactive)
  (let ((prompt (read-string "Input your prompt: ")))
    (auto-browser-run-linearly
     `((auto-browser-get-tab ,auto-browser-web-ai-url)
       (auto-browser-locate-element auto-browser-web-ai-input-selector)
       (auto-browser-input ,(concat prompt))
       (auto-browser-monitor ".fa-paper-plane" #'auto-browser-web-ai-notify)))))

(defun auto-browser-web-ai-notify (&rest args)
  (auto-browser-web-ai-show-chat-box))

(defun auto-browser-web-ai-show-chat-box ()
  (interactive)
  (auto-browser-run-linearly
   `((auto-browser-get-tab ,auto-browser-web-ai-url)
     (auto-browser-locate-element ".aa-html-content" -1)
     (auto-browser-get-element "html")
     (auto-browser-web-ai-render-chat-box))))

(defun auto-browser-web-ai-render-chat-box (trace-id html)
  (with-current-buffer (get-buffer-create "*web-ai-box*")
    (erase-buffer)
    (insert html)
    (shr-render-region (point-min) (point-max))
    (pop-to-buffer (current-buffer))))

(provide 'web-ai)
;;; web-ai.el ends here
