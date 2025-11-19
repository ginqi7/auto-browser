;;; ab-eww.el ---                                    -*- lexical-binding: t; -*-

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


(defun auto-browser-eww ()
  "Open the miniflux article page."
  (interactive)
  (let ((url (read-string "Web URL: ")))
    (auto-browser-run-linearly
     `((auto-browser-get-tab ,url)
       (auto-browser-rewrite-image-to-base64)
       (auto-browser-locate-element "html")
       (auto-browser-get-element "html")
       (auto-browser-eww-render)))))


(defun auto-browser-eww-render (trace-id html)
  "Render article HTML."
  (let ((shr-external-rendering-functions auto-browser-miniflux-rendering-functions))
    (auto-browser-render-html html "*ab-eww*")
    (with-current-buffer "*ab-eww*"
      (auto-browser-eww-mode 1))))

(define-minor-mode auto-browser-eww-mode
  "A minor mode in auto-browser eww buffer.")

(provide 'ab-eww)
;;; ab-eww.el ends here

