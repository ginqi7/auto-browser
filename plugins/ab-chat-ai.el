;;; ab-chat-ai.el --- Chat AI for Auto Browser.           -*- lexical-binding: t; -*-

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
;;; Custom Variables:
(defcustom auto-browser-save-directory (file-name-concat
                                        user-emacs-directory
                                        "auto-browser-chat-ai")
  "The directory to save web AI answers.")

(defcustom auto-browser-chat-ai-url ""
  "Web AI URL."
  :type 'string)

(defcustom auto-browser-chat-ai-input-selector "textarea"
  "The selector of the input element."
  :type 'string)

(defcustom auto-browser-chat-ai-response-regexp "(\\d+)?$"
  "The AI response matched URL regexp. python regexp."
  :type 'string)

(defcustom auto-browser-chat-ai-session-selector ".session"
 "The selector of the session element."
 :type 'string)

;;; Internal Variables:
(defvar auto-browser-chat-ai-default-callback nil)

(defun auto-browser-chat-ai-input (&optional prompt callback session)
  "Send Input string to Web AI."
  (interactive)
  (unless prompt
    (setq prompt (read-string "Input your prompt: ")))
  (unless callback
    (setq callback #'auto-browser-chat-ai-save-answer))
  (unless session
    (setq session 0))
  (auto-browser-run-linearly
   `((auto-browser-get-tab ,auto-browser-chat-ai-url t)
     (auto-browser-locate-element auto-browser-chat-ai-session-selector ,session)
     (auto-browser-click)
     (auto-browser-run-util-js "scroll_to_bottom.js")
     (auto-browser-wait-response ".*completions" ,(symbol-name callback))
     (auto-browser-locate-element ".n-input__textarea-el")
     (auto-browser-input ,prompt t))))

(defun auto-browser-chat-ai-render (traceId html)
  (funcall auto-browser-chat-ai-default-callback html))

(defun auto-browser-chat-ai-save-file-name ()
  (concat (file-name-concat auto-browser-save-directory
                            (format-time-string "%y%m%d#%H%M%S"))
          ".md"))

(defun auto-browser-chat-ai-data-parse (data)
  (let* ((prefix "data: ")
         (lines (split-string data "\n" t))
         (json-str (nth (- (length lines) 2) lines))
         (json-str (if (string-prefix-p prefix json-str)
                       (substring json-str (length prefix))
                     json-str))
         (json-str (string-trim json-str))
         (json-str (if (and (string-prefix-p "\"" json-str)
                            (string-suffix-p "\"" json-str))
                       (substring json-str 1 -1)
                     json-str))
         (json (json-parse-string json-str))
         (text (gethash "aiText" (gethash "data" json))))
    text))

(defun auto-browser-chat-ai-save-answer (data)
  (with-current-buffer (find-file-noselect (auto-browser-chat-ai-save-file-name))
    (erase-buffer)
    (insert (auto-browser-chat-ai-data-parse data))
    (save-buffer)
    (switch-to-buffer (current-buffer))))

  ;; (with-current-buffer (find-file-noselect (auto-browser-chat-ai-save-file-name))
  ;;   (erase-buffer)
  ;;   (insert html)
  ;;   (save-buffer)
  ;;   (shr-render-buffer (current-buffer)))

(defun auto-browser-chat-ai-copy-answer (data)
  (kill-new (auto-browser-chat-ai-data-parse data)))

(defun auto-browser-chat-ai-insert-answer (data)
  (insert (auto-browser-chat-ai-data-parse data)))

(provide 'ab-chat-ai)
;;; ab-chat-ai.el ends here
