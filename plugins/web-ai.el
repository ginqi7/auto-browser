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

(defcustom auto-browser-web-ai-input-selector "tag:textarea"
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
       (auto-browser-input ,(concat prompt "\n"))
       (auto-browser-wait-response auto-browser-web-ai-response-regexp)
       (auto-browser-show-dialogue ,prompt)))))

(defun auto-browser-show-dialogue (trace-id anwser ask)
  "Show AI dialogue."
  (switch-to-buffer "*web-ai*")
  (goto-char (point-max))
  (let ((buffer-read-only))
    (insert "# "
            ask "\n"
            anwser "\n")
   (setq buffer-read-only t)
   (markdown-mode)))

(provide 'web-ai)
;;; web-ai.el ends here
