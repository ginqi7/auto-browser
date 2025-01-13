;;; telegram.el ---                                  -*- lexical-binding: t; -*-

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

(defvar auto-browser-telegram--base-url "https://web.telegram.org/a")

(defvar auto-browser-telegram--buffer-name "*telegram*")

(defcustom auto-browser-telegram-rendering-functions nil
  "The anki rendering functions like `shr-external-rendering-functions`"
   :type 'cons)

(defun auto-browser-telegram-list-dialogues (&optional trace-id)
  "List dialogues"
  (interactive)
  (let* ((url auto-browser-telegram--base-url)
         (match-host t)
         (main-selector ".Transition"))
    (auto-browser-run-linearly
     `((auto-browser-get-tab ,url ,match-host)
       (auto-browser-locate-element ,main-selector)
       (auto-browser-get-element "html")
       (auto-browser-telegram-render-dialogue-list))
     trace-id)))

(defun auto-browser-telegram-parse-dialogue-list-html (html)
  "Pares dialogue list HTML."
  (let* ((dom (auto-browser-dom-parse-html html))
         (dialogues (auto-browser-dom-query-selector-all dom ".ListItem-button")))
    (mapcar #'auto-browser-telegram-parse-dialogue-dom dialogues)))

(defun auto-browser-telegram-parse-dialogue-dom (dialogue-dom)
  "Parse a single dialogua dom"
  (let ((title-dom (auto-browser-dom-query-selector-first dialogue-dom ".info-row h3"))
        (last-message-time-dom (auto-browser-dom-query-selector-first dialogue-dom ".LastMessageMeta span"))
        (last-message-dom (auto-browser-dom-query-selector-first dialogue-dom ".subtitle span span")))

    (list
     :title
     (dom-text title-dom)
     :last-message-time
     (dom-text last-message-time-dom)
     :last-message
     (string-replace "\n" " " (dom-text last-message-dom))
     :href
     (dom-attr dialogue-dom 'href))))

(defun auto-browser-telegram-render-dialogue-list (trace-id html)
  "Render dialogue list."
  (switch-to-buffer "*telegram-dialogues*")
  (let ((buffer-read-only))
    (erase-buffer)
    (setq tabulated-list-entries
          (mapcar (lambda (item)
                    (list (plist-get item :href)
                          (vector
                           (plist-get item :title)
                           (plist-get item :last-message)
                           (plist-get item :last-message-time))))
                  (auto-browser-telegram-parse-dialogue-list-html html)))
    (telegram-dialogues-mode)
    (goto-char (point-min))))

(defun auto-browser-telegram-open-dialogue ()
  "Open a dialogue."
  (interactive)
  (let ((url auto-browser-telegram--base-url)
        (entry (tabulated-list-get-entry))
        (href (tabulated-list-get-id)))
    (auto-browser-run-linearly
     `((auto-browser-get-tab ,url t)
       (auto-browser-locate-element ,(format "@href=%s" href))
       (auto-browser-click)
       (auto-browser-locate-element ".messages-container")
       (auto-browser-get-element "html")
       (auto-browser-telegram-render-dialogue-messages ,(aref entry 0))
       (auto-browser-rewrite-image-to-base64)
       (auto-browser-locate-element ".messages-container")
       (auto-browser-get-element "html")
       (auto-browser-telegram-render-dialogue-messages ,(aref entry 0))))))



(defun auto-browser-telegram-render-dialogue-messages (trace-id name html)
  "Render messages in a dialogue."
  (with-current-buffer (get-buffer-create (format "*%s*" name))
    (let ((message-date-groups
           (auto-browser-dom-query-selector-all
            (auto-browser-dom-parse-html html)
            ".message-date-group"))
          (shr-external-rendering-functions auto-browser-telegram-rendering-functions)
          date buffer-read-only)
      (switch-to-buffer (current-buffer))
      (erase-buffer)
      (dolist (message-date-group message-date-groups)
        (setq date (dom-text (auto-browser-dom-query-selector-first message-date-group "span")))
        (shr-insert-document (auto-browser-dom-parse-html (format "<h1>%s</h1>" date)))
        (auto-browser-telegram-parse-message-date-group message-date-group))
      (auto-browser--run-linearly trace-id))))


(defun auto-browser-telegram-parse-message-date-group (dom)
  "Parese message data group dom."
  (let ((message-groups (auto-browser-dom-query-selector-all
                         dom
                         "#message-group-[0-9]*")))
    (if message-groups
      (dolist (message-group message-groups)
        (setq sender-title (dom-text (auto-browser-dom-query-selector-first
                                      message-group ".sender-title")))
        (shr-insert-document (auto-browser-dom-parse-html (format "<h2>%s</h2>" sender-title)))
        (auto-browser-telegram-parse-message-group message-group))
      ;; If there are no message groups, just search messages.
      (auto-browser-telegram-parse-message-group dom))))



(defun auto-browser-telegram-parse-message-group (dom)
  "Find messages in message-group dom."
  (let ((messages (auto-browser-dom-query-selector-all
                   dom
                   "#message-[0-9]*"))
        (message-text)
        (webpage-content))
    (dolist (message-dom messages)
      (setq message-text-dom (auto-browser-dom-query-selector-all
                              dom
                              ".text-content"))
      (setq webpage-content-dom (auto-browser-dom-query-selector-all
                                 dom
                                 ".WebPage--content"))
      (shr-insert-document message-text-dom)
      (when webpage-content-dom
        (shr-insert-document webpage-content-dom)))))


(defun auto-browser-telegram-send-message (&optional msg)
  "Send Message"
  (interactive)
  (unless msg
    (setq msg (read-string "Input Message: ")))
  (auto-browser-run-linearly
     `((auto-browser-get-tab ,auto-browser-telegram--base-url t)
       (auto-browser-locate-element ".input-scroller-content")
       (auto-browser-input ,(format "%s\n" msg)))))

(define-derived-mode telegram-dialogues-mode tabulated-list-mode "Telegram Dialogues"
  "Major mode for handling a list of Telegram Dialogue."
  (setq tabulated-list-format [("Title" 30 t)
                               ("Last Message" 60 t)
                               ("Time"  20 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'auto-browser-telegram-open-dialogue)
    (use-local-map map)))

(provide 'telegram)
;;; telegram.el ends here
