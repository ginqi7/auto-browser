;;; asr.el ---                                       -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

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

(defcustom auto-browser-asr-url "https://bailian.console.aliyun.com/cn-beijing/?tab=model#/efm/model_experience_center/voice?currentTab=voiceAsr"
  "URL opened for the browser-based ASR experience page.")

(defcustom auto-browser-asr-handle #'print
  "Function called to handle ASR results from the browser workflow.")

(defun auto-browser-asr-start ()
  "Start the browser ASR workflow by opening the configured ASR page, locating the primary action button, and clicking it automatically."
  (interactive)
  (let* ((url auto-browser-asr-url)
         (selector "button.efm_ant-btn-primary"))
    (auto-browser-run-linearly
     `((auto-browser-get-tab ,url t)
       (auto-browser-locate-element ,selector 1)
       (auto-browser-click)))))

(defun auto-browser-asr-stop ()
  "Stop the browser ASR workflow by opening the configured ASR page, locating the pause button, and clicking it automatically."
  (interactive)
  (let* ((url auto-browser-asr-url)
         (selector "button.efm_ant-btn-icon-only:has(i.bl-icon-pause-line)"))
    (auto-browser-run-linearly
     `((auto-browser-get-tab ,url t)
       (auto-browser-locate-element ,selector)
       (auto-browser-click)))))

(defun auto-browser-asr-stop-and-get ()
  "Stop ASR recording in the browser workflow, then locate and retrieve the result content from the page HTML for downstream handling."
  (interactive)
  (let* ((url auto-browser-asr-url)
         (stop-selector "button.efm_ant-btn-icon-only:has(i.bl-icon-pause-line)")
         (get-selector "div.x-markdown.efm_ant-markdown"))
    (auto-browser-run-linearly
     `((auto-browser-get-tab ,url t)
       (auto-browser-locate-element ,stop-selector)
       (auto-browser-click)
       (auto-browser-locate-element ,get-selector)
       (auto-browser-get-element "html")
       (auto-browser-asr-get-html)))))

(defun auto-browser-asr-get ()
  "Retrieve the current ASR result content from the configured browser page by locating the markdown container and extracting its HTML."
  (interactive)
  (let* ((url auto-browser-asr-url)
         (selector "div.x-markdown.efm_ant-markdown"))
    (auto-browser-run-linearly
     `((auto-browser-get-tab ,url t)
       (auto-browser-locate-element ,selector)
       (auto-browser-get-element "html")
       (auto-browser-asr-get-html)))))

(defun auto-browser-asr-get-html (trace-id html)
  "Convert the provided HTML fragment into plain text and pass the trimmed result to the configured ASR result handler."
  (with-temp-buffer
    (shr-insert-document
     (with-temp-buffer
       (insert html)
       (libxml-parse-html-region (point-min) (point-max))))
    (funcall auto-browser-asr-handle (string-trim (buffer-substring-no-properties (point-min) (point-max))))))

(provide 'asr)
;;; asr.el ends here
