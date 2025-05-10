;;; gemini.el ---                                    -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Qiqi Jin

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

(defcustom auto-browser-gemini-url "https://gemini.google.com/app"
  "Web AI URL."
  :type 'string)

(defcustom auto-browser-gemini-input-selector ".text-input-field_textarea-wrapper"
  "The selector of the input element."
  :type 'string)

(defcustom auto-browser-gemini-response-regexp "(\\d+)?$"
  "The AI response matched URL regexp. python regexp."
  :type 'string)

(defun auto-browser-gemini-input ()
  "Send Input string to Web AI."
  (interactive)
  (let ((prompt (read-string "Input your prompt: ")))
    (auto-browser-run-linearly
     `((auto-browser-get-tab ,auto-browser-gemini-url t)
       (auto-browser-locate-element auto-browser-gemini-input-selector)
       (auto-browser-input ,(concat prompt "\n"))))))

(defun auto-browser-gemini-show-chat-box ()
  (interactive)
  (auto-browser-run-linearly
   `((auto-browser-get-tab ,auto-browser-gemini-url t)
     (auto-browser-locate-element "#chat-history")
     (auto-browser-get-element "html")
     (auto-browser-gemini-render-chat-box))))

(defun auto-browser-gemini-render-chat-box (trace-id html)
  (with-current-buffer (get-buffer-create "*gemini-box*")
    (erase-buffer)
    (insert html)
    (shr-render-region (point-min) (point-max))
    (pop-to-buffer (current-buffer))))


(defun decode-base64-to-unicode (base64-str)
  "Decode a Base64 encoded string to a Unicode string."
  (let ((decoded-bytes (base64-decode-string base64-str)))
    (decode-coding-string decoded-bytes 'utf-8)))

(defun auto-browser-show-msg (trace-id base64-str)
  "Show AI dialogue."
  (with-current-buffer "*gemini*"
    (goto-char (point-max))
    (let ((buffer-read-only))
      (insert (decode-base64-to-unicode base64-str)))))

(defun auto-browser-show-dialogue (trace-id prompt)
  "Show AI dialogue."
  (switch-to-buffer "*gemini*")
  (goto-char (point-max))
  (let ((buffer-read-only))
    (insert "\n# " prompt "\n")
    (setq buffer-read-only t)
    (markdown-mode))
  (auto-browser-run-linearly
   `((auto-browser-stream-response ,auto-browser-gemini-response-regexp "auto-browser-show-msg"))
   trace-id))



(provide 'gemini)
;;; gemini.el ends here
