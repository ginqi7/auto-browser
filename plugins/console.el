;;; console.el ---                                   -*- lexical-binding: t; -*-

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

(defun auto-browser-console--get-next-org-source-block-position (position)
  "Get next source block position."
  (save-excursion
    (goto-char position)
    (when (re-search-forward "^[ \t]*#\\+BEGIN_SRC" nil t)
      (match-beginning 0))))

(defun auto-browser-console--get-org-source-block-results (element)
  "Get results element of current source block."
  (save-excursion
    (let* ((block-end (org-element-property :end element)) ; End of source code block
           (next-block-start (auto-browser-console--get-next-org-source-block-position block-end))) ; The starting position of the next source code block
      (goto-char block-end) ;Move to the end of the source code block.
      (when (re-search-forward "^[ \t]*#\\+RESULTS:" (or next-block-start (point-max)) t) ; Restrict search range
        (org-element-at-point)))))

(defun auto-browser-console-get-org-source-block-results ()
  "Get the #+RESULTS block of the current source block using org-element."
  (interactive)
  (require 'org-element) ; Ensure org-element is loaded.
  (let* ((element (org-element-at-point))) ; Get the element at the current cursor position
    (auto-browser-console--get-org-source-block-results element)))

(defun auto-browser-console-org-get-script ()
  "Get script from source block."
  (let* ((source-block (org-element-at-point))
         (content (org-element-property :value source-block)))
    content))

(defun auto-browser-console-org-get-url ()
  "Get URL from org config."
  (save-excursion
    (goto-char (point-min))
    (let ((url nil))
      (while (re-search-forward "^#\\+URL: \\(.+\\)" nil t)
        (setq url (match-string-no-properties 1)))
      (if url
          (string-trim url)
        (error "No #+URL: found in the file.")))))

(defun auto-browser-console-org-result (trace-id result)
  "Org mode for console result."
  (let* ((element (org-element-at-point))
         (results (auto-browser-console-get-org-source-block-results)))
    (when results
      (delete-region (org-element-property :begin results)
                     (org-element-property :end results)))
    (save-excursion
      (goto-char (org-element-property :end element))
      (insert (format "#+RESULTS:\n: %s\n\n" result)))))

(defun auto-browser-console-org-send ()
  "Send org block script to browser console."
  (interactive)
  (let ((url (auto-browser-console-org-get-url))
        (script (auto-browser-console-org-get-script)))
    (auto-browser-console-send url script #'auto-browser-console-org-result)))

(defun auto-browser-console-result (trace-id result)
  "Example for result."
  (print result))

(defun auto-browser-console-send (url script result-handle)
  "Send console script to browser."
  (auto-browser-run-linearly
   `((auto-browser-get-tab ,url)
     (auto-browser-console ,script)
     (,result-handle))))

(provide 'console)
;;; console.el ends here
