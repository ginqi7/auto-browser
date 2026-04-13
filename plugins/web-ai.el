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
;;; Custom Variables:
(defcustom auto-browser-save-directory (file-name-concat
                                        user-emacs-directory
                                        "auto-browser-web-ai")
  "The directory to save web AI answers.")

(defcustom auto-browser-web-ai-url ""
  "Web AI URL."
  :type 'string)

(defcustom auto-browser-web-ai-input-selector "textarea"
  "The selector of the input element."
  :type 'string)

(defcustom auto-browser-web-ai-response-regexp "(\\d+)?$"
  "The AI response matched URL regexp. python regexp."
  :type 'string)

(defcustom auto-browser-web-ai-session-selector ".session"
 "The selector of the session element."
 :type 'string)

;;; Internal Variables:
(defvar auto-browser-web-ai-default-callback nil)

(defun auto-browser-web-ai-input (&optional prompt callback session)
  "Send Input string to Web AI."
  (interactive)
  (unless prompt
    (setq prompt (read-string "Input your prompt: ")))
  (if callback
      (setq auto-browser-web-ai-default-callback callback)
    (setq auto-browser-web-ai-default-callback #'auto-browser-web-ai-save-answer))
  (unless session
    (setq session 0))
  (auto-browser-run-linearly
   `((auto-browser-get-tab ,auto-browser-web-ai-url t)
     (auto-browser-locate-element auto-browser-web-ai-session-selector ,session)
     (auto-browser-click)
     (auto-browser-locate-element auto-browser-web-ai-input-selector)
     (auto-browser-input ,prompt t)
     (auto-browser-wait-for-element ".lucide-rotate-ccw" "auto-browser-web-ai-print"))))

;; (auto-browser-monitor ".fa-paper-plane" #'auto-browser-web-ai-notify)
;; .assistant:last-child

(defun auto-browser-web-ai-print(traceId)
  (auto-browser-run-linearly
   `((auto-browser-locate-element ".assistant:last-child .message-content")
     (auto-browser-get-element "html")
     (auto-browser-web-ai-render))
   traceId))

(defun auto-browser-web-ai-render (traceId html)
  (funcall auto-browser-web-ai-default-callback html))

(defun auto-browser-web-ai-save-file-name ()
  (concat (file-name-concat auto-browser-save-directory
                            (format-time-string "%y%m%d#%H%M%S"))
          ".html"))

(defun auto-browser-web-ai-save-answer (html)
  (with-current-buffer (find-file-noselect (auto-browser-web-ai-save-file-name))
    (erase-buffer)
    (insert html)
    (save-buffer)
    (shr-render-buffer (current-buffer))))

(defun auto-browser-web-ai-copy-answer (html)
  (kill-new (dom-text (auto-browser-dom-parse-html html))))

(defun auto-browser-web-ai-insert-answer (html)
  (insert (dom-texts (auto-browser-dom-parse-html html))))

(provide 'web-ai)
;;; web-ai.el ends here
