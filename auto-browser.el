;;; auto-browser.el --- Controller for Chrome.       -*- lexical-binding: t; -*-

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

(require 'websocket-bridge)

(defvar auto-browser-py-path
  (concat (file-name-directory (or load-file-name
                                   (buffer-file-name)))
         "auto-browser.py"))

(defvar auto-browser-utils-directory
  (concat (file-name-directory (or load-file-name (buffer-file-name)))
          "utils"))

(defcustom auto-browser-python (executable-find "python3")
  "The Python interpreter."
  :type 'string)

(defvar auto-browser--call-chain nil)

(defun auto-browser-id-uuid ()
  "Return string with random (version 4) UUID. Copy from (org-id-uuid)"
  (let ((rnd (md5 (format "%s%s%s%s%s%s%s"
                          (random)
                          (org-time-convert-to-list nil)
                          (user-uid)
                          (emacs-pid)
                          (user-full-name)
                          user-mail-address
                          (recent-keys)))))
    (format "%s-%s-4%s-%s%s-%s"
            (substring rnd 0 8)
            (substring rnd 8 12)
            (substring rnd 13 16)
            (format "%x"
                    (logior
                     #b10000000
                     (logand
                      #b10111111
                      (string-to-number
                       (substring rnd 16 18) 16))))
            (substring rnd 18 20)
            (substring rnd 20 32))))

(defun auto-browser-start ()
  "Start auto-browser."
  (interactive)
  (setq auto-browser--call-chain nil)
  (websocket-bridge-server-start)
  (websocket-bridge-app-start
   "auto-browser"
   auto-browser-python
   auto-browser-py-path))

(defun auto-browser-stop ()
  "Stop auto-browser."
  (interactive)
  (setq auto-browser--call-chain nil)
  (websocket-bridge-app-exit "auto-browser"))

(defun auto-browser-restart ()
  "Restart auto-browser."
  (interactive)
  (auto-browser-stop)
  (auto-browser-start)
  (split-window-below -10)
  (other-window 1)
  (websocket-bridge-app-open-buffer "auto-browser"))

(defun auto-browser-get-tab (trace-id url &optional match_host)
  "Call grammarly function on current buffer by FUNC-NAME."
  (interactive)
  (websocket-bridge-call "auto-browser"
                         "get-tab"
                         trace-id
                         url
                         match_host))

(defun auto-browser-locate-element (trace-id locator)
  "Call grammarly function on current buffer by FUNC-NAME."
  (interactive)
  (websocket-bridge-call "auto-browser"
                         "locate-element"
                         trace-id
                         locator))

(defun auto-browser-run-js (trace-id js)
  "Call grammarly function on current buffer by FUNC-NAME."
  (interactive)
  (websocket-bridge-call "auto-browser"
                         "run-js"
                         trace-id
                         js))

(defun auto-browser-get-element (trace-id property)
  "Call grammarly function on current buffer by FUNC-NAME."
  (interactive)
  (websocket-bridge-call "auto-browser"
                         "get-element"
                         trace-id
                         property))

(defun auto-browser-get-cookies (trace-id)
  "Call grammarly function on current buffer by FUNC-NAME."
  (interactive)
  (websocket-bridge-call "auto-browser"
                         "get-cookies"
                         trace-id))

(defun auto-browser-run-util-js (trace-id util-name)
  "Call grammarly function on current buffer by FUNC-NAME."
  (interactive)
  (websocket-bridge-call "auto-browser"
                         "run-util-js"
                         trace-id
                         util-name))

(defun auto-browser-click (trace-id)
  "Call grammarly function on current buffer by FUNC-NAME."
  (interactive)
  (websocket-bridge-call "auto-browser"
                         "click"
                         trace-id))


(defun auto-browser-run-linearly (lst &optional trace-id)
  (setq auto-browser--call-chain (append lst auto-browser--call-chain))
  (unless trace-id
    (setq trace-id (auto-browser-id-uuid)))
  (auto-browser--run-linearly trace-id))

(defun auto-browser--run-linearly (traceId &rest args)
  (when auto-browser--call-chain
    (let ((func (car (car auto-browser--call-chain)))
          (parameters (cdr (car auto-browser--call-chain))))
      (setq auto-browser--call-chain (cdr auto-browser--call-chain))
      (eval (append
             (list func traceId)
             parameters
             args)))))

(defun auto-browser-rewrite-image-to-base64 (trace-id)
  (websocket-bridge-call "auto-browser"
                         "rewrite-image-to-base64"
                         trace-id))

(defun auto-browser-readability (trace-id html)
  (websocket-bridge-call "auto-browser"
                         "readability"
                         trace-id
                         html))

(defun auto-browser-input (trace-id input-str)
  (websocket-bridge-call "auto-browser"
                         "input"
                         trace-id
                         input-str))

(defun auto-browser-wait-response (trace-id url-pattern)
  (websocket-bridge-call "auto-browser"
                         "wait-response"
                         trace-id
                         url-pattern))

(defun auto-browser-stream-response (trace-id url-pattern callback)
  (websocket-bridge-call "auto-browser"
                         "stream-response"
                         trace-id
                         url-pattern
                         callback))


(defun auto-browser-render-html (html buffer)
  "Render HTML."
  (or (fboundp 'libxml-parse-html-region)
      (error "This function requires Emacs to be compiled with libxml2"))
  (switch-to-buffer buffer)
  (setq buffer-read-only nil)
  (erase-buffer)
  (shr-insert-document
   (with-temp-buffer
     (insert html)
     (libxml-parse-html-region (point-min) (point-max))))
  (setq buffer-read-only t)
  (goto-char (point-min)))

(provide 'auto-browser)
;;; auto-browser.el ends here
