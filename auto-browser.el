;;; auto-browser.el --- Controller for Chrome.       -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Package-Requires: ((websocket-bridge))
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

;;; Custom Variables

(defcustom auto-browser-py-path
  (concat (file-name-directory (or load-file-name (buffer-file-name))) "auto-browser.py")
  "The absolute file path to the auto-browser.py script located in the same directory as the library."
  :type 'string)

(defcustom auto-browser-utils-directory
  (concat (file-name-directory (or load-file-name (buffer-file-name))) "utils")
  "The absolute path to the utils directory located in the same directory as the library."
  :type 'string)

(defcustom auto-browser-python (executable-find "python3")
  "The Python interpreter."
  :type 'string)

;;; Internal Variables

(defvar auto-browser--call-chain nil
  "An internal variable used to store the sequence or history of function calls within the auto-browser library.")

;;; Internal Functions

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

(defun auto-browser--run-linearly (traceId &rest args)
  (when auto-browser--call-chain
    (let ((func (car (car auto-browser--call-chain)))
          (parameters (cdr (car auto-browser--call-chain))))
      (setq auto-browser--call-chain (cdr auto-browser--call-chain))
      ;; (print (append
      ;;         (list func traceId)
      ;;         parameters
      ;;         args))
      (eval (append
             (list func traceId)
             parameters
             args)))))

;;; APIs

(defun auto-browser-get-tab (trace-id url &optional match_host)
  "Retrieves a browser tab associated with the specified trace ID and URL, with an optional parameter to match only the host."
  (interactive)
  (websocket-bridge-call "auto-browser" "get-page" trace-id url match_host))

(defun auto-browser-locate-element (trace-id locator &optional nth timeout)
  "Locates an element in the browser using the specified trace ID and locator, with optional parameters for the occurrence index and timeout duration."
  (interactive)
  (websocket-bridge-call "auto-browser" "locate-element" trace-id locator nth timeout))

(defun auto-browser-locate-elements (trace-id locator)
  "Locates all elements in the browser matching the specified trace ID and locator."
  (interactive)
  (websocket-bridge-call "auto-browser" "locate-elements" trace-id locator))

(defun auto-browser-run-js (trace-id js)
  "Executes JavaScript code in the browser session associated with the specified trace ID."
  (interactive)
  (websocket-bridge-call "auto-browser" "run-js" trace-id js))

(defun auto-browser-get-element (trace-id property)
  "Retrieves a specific property of an element in the browser session associated with the specified trace ID."
  (interactive)
  (websocket-bridge-call "auto-browser" "get-element" trace-id property))

(defun auto-browser-get-elements (trace-id property)
  "Retrieves a specific property for all matching elements in the browser session associated with the specified trace ID."
  (interactive)
  (websocket-bridge-call "auto-browser" "get-elements" trace-id property))

(defun auto-browser-run-util-js (trace-id util-name)
  "Executes a specified utility JavaScript function in the browser session associated with the specified trace ID."
  (interactive)
  (websocket-bridge-call "auto-browser" "run-util-js" trace-id util-name))

(defun auto-browser-click (trace-id)
  "Performs a click action in the browser session associated with the specified trace ID."
  (interactive)
  (websocket-bridge-call "auto-browser" "click" trace-id))

(defun auto-browser-key-down (trace-id key)
  "Simulates a key down event for the specified key in the browser session associated with the specified trace ID."
  (interactive)
  (websocket-bridge-call "auto-browser" "key-down" trace-id key))

(defun auto-browser-key-up (trace-id key)
  "Simulates a key up event for the specified key in the browser session associated with the specified trace ID."
  (interactive)
  (websocket-bridge-call "auto-browser" "key-up" trace-id key))

(defun auto-browser-run-linearly (lst &optional trace-id)
  "Executes a list of browser commands sequentially for a specified trace ID, generating a new ID if none is provided."
  (setq auto-browser--call-chain (append lst auto-browser--call-chain))
  (unless trace-id
    (setq trace-id (auto-browser-id-uuid)))
  (auto-browser--run-linearly trace-id))

(defun auto-browser-rewrite-image-to-base64 (trace-id)
  "Converts images to base64 format for the browser session associated with the specified trace ID."
  (websocket-bridge-call "auto-browser" "rewrite-image-to-base64" trace-id))

(defun auto-browser-readability (trace-id html)
  "Processes the provided HTML to extract its main readable content for the specified trace ID."
  (websocket-bridge-call "auto-browser" "readability" trace-id html))

(defun auto-browser-input (trace-id input-str &optional enter responseMatch)
  "Inputs a string into the browser for the specified trace ID, with options to press the enter key and match a specific response."
  (websocket-bridge-call "auto-browser" "input" trace-id input-str enter responseMatch))

(defun auto-browser-monitor (trace-id selector callback)
  "Monitors a specified selector for the given trace ID and executes a callback function when changes occur."
  (websocket-bridge-call "auto-browser" "monitor" trace-id selector callback))

(defun auto-browser-wait-response (trace-id url-pattern callback)
  "Waits for a network response matching the specified URL pattern for the given trace ID and executes a callback when the response is received."
  (websocket-bridge-call "auto-browser" "wait-response" trace-id url-pattern callback))

(defun auto-browser-wait-element-stable (trace-id selector callback)
  "Waits for the element matching the specified selector to become stable for the given trace ID and executes a callback once stability is reached."
  (websocket-bridge-call "auto-browser" "wait-element-stable" trace-id selector callback))

(defun auto-browser-wait-for-element (trace-id selector callback)
  "Waits for the element matching the specified selector to appear for the given trace ID and executes a callback once the element is found."
  (websocket-bridge-call "auto-browser" "wait-for-element" trace-id selector callback))

(defun auto-browser-wait-for-timeout (trace-id timeout)
  "Waits for a specified duration for the given trace ID."
  (websocket-bridge-call "auto-browser" "wait-for-timeout" trace-id timeout))

(defun auto-browser-console (trace-id script)
  "Executes the specified script in the browser console for the given trace ID."
  (websocket-bridge-call "auto-browser" "console" trace-id script))

(defun auto-browser-refresh (trace-id)
  "Refreshes the current browser page for the given trace ID."
  (websocket-bridge-call "auto-browser" "refresh" trace-id))

(defun auto-browser-render-html (html buffer)
  "Renders the provided HTML string into a specified buffer using the shr engine, requiring libxml2 support for parsing."
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

;;; Interactive Functions

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

(provide 'auto-browser)
;;; auto-browser.el ends here
