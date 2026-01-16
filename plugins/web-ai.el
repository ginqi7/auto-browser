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

(defcustom auto-browser-save-directory (file-name-concat
                                        user-emacs-directory
                                        "auto-browser-web-ai")
  "The directory to save web AI answers.")

(defcustom auto-browser-web-ai-url ""
  "Web AI URL."
  :type 'string)

(defcustom auto-browser-web-ai-input-selector ".n-input__textarea-el"
  "The selector of the input element."
  :type 'string)

(defcustom auto-browser-web-ai-response-regexp "(\\d+)?$"
  "The AI response matched URL regexp. python regexp."
  :type 'string)

(defvar auto-browser-web-ai-default-callback nil)

(defun auto-browser-web-ai-input (&optional prompt callback)
  "Send Input string to Web AI."
  (interactive)
  (unless prompt
    (setq prompt (read-string "Input your prompt: ")))
  (if callback
      (setq auto-browser-web-ai-default-callback callback)
    (setq auto-browser-web-ai-default-callback #'auto-browser-web-ai-save-answer))
  (auto-browser-run-linearly
   `((auto-browser-get-tab ,auto-browser-web-ai-url)
     (auto-browser-locate-element auto-browser-web-ai-input-selector)
     (auto-browser-input ,prompt t "**/completions")
     (auto-browser-web-ai-render))))
;; (auto-browser-monitor ".fa-paper-plane" #'auto-browser-web-ai-notify)

(defun auto-browser-web-ai-render (traceId data)
  (funcall auto-browser-web-ai-default-callback data))

(defun auto-browser-web-ai-save-file-name ()
  (concat (file-name-concat auto-browser-save-directory
                            (format-time-string "%y%m%d#%H%M%S"))
          ".md"))

(defun auto-browser-web-ai-save-answer (data)
  (with-current-buffer (find-file-noselect (auto-browser-web-ai-save-file-name))
    (erase-buffer)
    (insert data)
    (markdown-mode)
    (save-buffer)
    (switch-to-buffer (current-buffer))))

(defun auto-browser-web-ai-copy-answer (data)
  (kill-new data))

(defun auto-browser-web-ai-insert-answer (data)
  (insert data))

(provide 'web-ai)
;;; web-ai.el ends here
