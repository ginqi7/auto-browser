;;; anki.el --- Anki for Auto-Browser                -*- lexical-binding: t; -*-

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

(defvar auto-browser-anki--study-url "https://ankiuser.net/study")

(defvar auto-browser-anki--buffer-name "*anki*")

(defcustom auto-browser-anki-rendering-functions nil
  "The anki rendering functions like `shr-external-rendering-functions`"
   :type 'cons)

(defun auto-browser-anki-study (&optional trace-id)
  "Show anki study page."
  (interactive)
  (let* ((url auto-browser-anki--study-url)
         (selector "#qa"))
    (auto-browser-run-linearly
     `((auto-browser-get-tab ,url)
       (auto-browser-anki-play-audio)
       (auto-browser-rewrite-image-to-base64)
       (auto-browser-locate-element ,selector)
       (auto-browser-get-element "html")
       (auto-browser-anki-show))
     trace-id)))

(defun auto-browser-anki-show (trace-id html)
  "Show anki HTML."
  (let ((shr-external-rendering-functions auto-browser-anki-rendering-functions))
    (auto-browser-render-html html auto-browser-anki--buffer-name)
    (anki-mode)))

(defun auto-browser-anki-play-audio (&optional trace-id)
  "Play Audio."
  (interactive)
  (let* ((url auto-browser-anki--study-url)
         (selector "tag:audio"))
    (auto-browser-run-linearly
     `((auto-browser-get-tab ,url)
       (auto-browser-locate-element ,selector)
       (auto-browser-run-js "this.play()"))
     trace-id)))

(defun auto-browser-anki-show-answer ()
  "Show anki answer page."
  (interactive)
  (let* ((selector "tag:button@text():Show Answer")
         (url auto-browser-anki--study-url))
    (auto-browser-run-linearly
     `((auto-browser-get-tab ,url)
       (auto-browser-locate-element ,selector)
       (auto-browser-run-js "this.click()")
       (auto-browser-anki-study)))))

(defun auto-browser-anki-easy ()
  "Mark the item status Easy."
  (interactive)
  (auto-browser-anki--mark "Easy"))

(defun auto-browser-anki-good ()
  "Mark the item status Good."
  (interactive)
  (auto-browser-anki--mark "Good"))

(defun auto-browser-anki-hard ()
  "Mark the item status Hard."
  (interactive)
  (auto-browser-anki--mark "Hard"))

(defun auto-browser-anki-again ()
  "Mark the item status Again."
  (interactive)
  (auto-browser-anki--mark "Again"))

(defun auto-browser-anki-mark ()
  "Mark the item status."
  (interactive)
  (auto-browser-anki--mark
   (completing-read "Mark Status: "
          '("Again" "Hard" "Good" "Easy"))))

(defun auto-browser-anki--mark (status)
  "Mark the item status."
  (let* ((selector (concat "tag:button@text():" status))
         (url auto-browser-anki--study-url))
    (auto-browser-run-linearly
     `((auto-browser-get-tab ,url)
       (auto-browser-locate-element ,selector)
       (auto-browser-run-js "this.click()")
       (auto-browser-anki-study)))))

(define-minor-mode anki-mode "An anki mode"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") 'auto-browser-anki-show-answer)
    (define-key map (kbd "p") 'auto-browser-anki-play-audio)
    (define-key map (kbd "g") 'auto-browser-anki-good)
    (define-key map (kbd "e") 'auto-browser-anki-easy)
    (define-key map (kbd "h") 'auto-browser-anki-hard)
    (define-key map (kbd "a") 'auto-browser-anki-again)
   map))

(provide 'anki)
;;; anki.el ends here
