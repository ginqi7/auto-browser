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

(defvar auto-browser-anki--decks-url "https://ankiweb.net/decks")

(defvar auto-browser-anki--buffer-name "*anki*")

(defvar auto-browser-anki--decks-buffer-name "*anki-decks*")

(defvar auto-browser-anki--question-page-p t
  "question or anwser.")

(defvar auto-browser-anki--counts nil
  "the number of study cards.")

(defcustom auto-browser-anki-rendering-functions nil
  "The anki rendering functions like `shr-external-rendering-functions`"
  :type 'cons)

(defun auto-browser-anki--set-page-type (trace-id html)
  (let ((button-text
         (dom-text
          (auto-browser-dom-query-selector-first
           (auto-browser-dom-parse-html html) "button"))))
    (if (string= button-text "Again")
        (setq auto-browser-anki--question-page-p nil)
      (setq auto-browser-anki--question-page-p t))
    (auto-browser--run-linearly trace-id)))

(defun auto-browser-anki--set-count (trace-id html)
  (let ((counts
         (auto-browser-dom-query-selector-all
          (auto-browser-dom-parse-html html) "span")))
    (setq auto-browser-anki--counts nil)
    (setq auto-browser-anki--counts
          (plist-put auto-browser-anki--counts
                     :new (dom-text (nth 0 counts))))
    (setq auto-browser-anki--counts
          (plist-put auto-browser-anki--counts
                     :learn (dom-text (nth 1 counts))))
    (setq auto-browser-anki--counts
          (plist-put auto-browser-anki--counts
                     :review (dom-text (nth 2 counts))))
    (auto-browser--run-linearly trace-id)))

(defun auto-browser-anki-study (&optional trace-id)
  "Show anki study page."
  (interactive)
  (let* ((url auto-browser-anki--study-url)
         (selector "#qa")
         (ansarea "#ansarea")
         (count-selector ".float-end"))
    (auto-browser-run-linearly
     `((auto-browser-get-tab ,url)
       (auto-browser-locate-element ,ansarea)
       (auto-browser-get-element "html")
       (auto-browser-anki--set-page-type)
       (auto-browser-locate-element ,count-selector)
       (auto-browser-get-element "html")
       (auto-browser-anki--set-count)
       (auto-browser-anki-play-audio)
       ;; Display HTML twice
       ;; first time without image
       (auto-browser-locate-element ,selector)
       (auto-browser-get-element "html")
       (auto-browser-anki-show)
       ;; second time show the image
       (auto-browser-rewrite-image-to-base64)
       (auto-browser-locate-element ,selector)
       (auto-browser-get-element "html")
       (auto-browser-anki-show))
     trace-id)))


(defun auto-browser-anki-decks (&optional trace-id)
  "Show anki study page."
  (interactive)
  (let* ((url auto-browser-anki--decks-url)
         (selector "t:main"))
    (auto-browser-run-linearly
     `((auto-browser-get-tab ,url)
       (auto-browser-locate-element ,selector)
       (auto-browser-get-element "html")
       (auto-browser-anki-decks-render))
     trace-id)))

(defun auto-browser-anki-decks-render (trace-id html)
  (with-current-buffer (get-buffer-create auto-browser-anki--decks-buffer-name)
    (setq tabulated-list-format [("Name" 30 t)
                                 ("Due" 10 t)
                                 ("New"  10 t)])
    (setq tabulated-list-padding 2)
    (setq tabulated-list-entries (auto-browser-anki-decks--to-entries html))
    (tabulated-list-init-header)
    (tabulated-list-print t)
    (tabulated-list-mode)
    (keymap-set tabulated-list-mode-map "<RET>" #'auto-browser-anki-select)
    (switch-to-buffer (current-buffer))))

(defun auto-browser-anki-select ()
  (interactive)
  (let* ((url auto-browser-anki--decks-url)
         (selector (tabulated-list-get-id)))
    (auto-browser-run-linearly
     `((auto-browser-get-tab ,url)
       (auto-browser-locate-element ,selector)
       (auto-browser-click)
       (auto-browser-anki-study)))))


(defun auto-browser-anki-decks--to-entries (html)
  (let (key name due new)
    (mapcar
     (lambda (dom)
       (setq key
             (string-trim
              (string-replace " " ""
                              (dom-text
                               (auto-browser-dom-query-selector-first
                                dom "button")))))
       (setq name key)
       (setq due
             (dom-text
              (auto-browser-dom-query-selector-all dom
                                                   ".number .due")))
       (setq new
             (dom-text
              (auto-browser-dom-query-selector-all dom
                                                   ".number .new")))
       (list key (vector name due new)))
     (auto-browser-dom-query-selector-all
      (auto-browser-dom-parse-html html) "div .light-bottom-border"))))


(defun auto-browser-add-count-info (html)
  (format "<h1>
             <span style=\"color: #00f;\">%s</span>
             <span style=\"color: #900;\">%s</span>
             <span style=\"color: #090;\">%s</span>
           </h1>%s"
          (plist-get auto-browser-anki--counts :new)
          (plist-get auto-browser-anki--counts :learn)
          (plist-get auto-browser-anki--counts :review)
          html))

(defun auto-browser-anki-show (trace-id html)
  "Show anki HTML."
  (let ((shr-external-rendering-functions auto-browser-anki-rendering-functions))

    (auto-browser-render-html (auto-browser-add-count-info html)
                              auto-browser-anki--buffer-name)
    (anki-mode)
    (auto-browser--run-linearly trace-id)))


(defun auto-browser-anki-play-audio (&optional trace-id)
  "Play Audio."
  (interactive)
  (let* ((url auto-browser-anki--study-url)
         (selector "tag:audio"))
    (auto-browser-run-linearly
     `((auto-browser-get-tab ,url)
       (auto-browser-locate-element ,selector nil 0.2)
       (auto-browser-run-js "this.play()"))
     trace-id)))

(defun auto-browser-anki-show-answer ()
  "Show anki answer page."
  (interactive)
  (let* ((selector "tag:button@text():Show Answer")
         (url auto-browser-anki--study-url))
    (when auto-browser-anki--question-page-p
      (auto-browser-run-linearly
       `((auto-browser-get-tab ,url)
         (auto-browser-locate-element ,selector)
         (auto-browser-run-js "this.click()")
         (auto-browser-anki-study))))))

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

(defun auto-browser-anki-refresh ()
  "Refresh."
  (interactive)
  (let* ((url auto-browser-anki--study-url))
    (auto-browser-run-linearly
     `((auto-browser-get-tab ,url)
       (auto-browser-refresh)
       (auto-browser-anki-study)))))

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
    (unless auto-browser-anki--question-page-p
      (auto-browser-run-linearly
       `((auto-browser-get-tab ,url)
         (auto-browser-locate-element ,selector)
         (auto-browser-run-js "this.click()")
         (auto-browser-anki-study))))))

(define-minor-mode anki-mode "An anki mode"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") 'auto-browser-anki-show-answer)
    (define-key map (kbd "p") 'auto-browser-anki-play-audio)
    (define-key map (kbd "g") 'auto-browser-anki-good)
    (define-key map (kbd "e") 'auto-browser-anki-easy)
    (define-key map (kbd "h") 'auto-browser-anki-hard)
    (define-key map (kbd "a") 'auto-browser-anki-again)
    (define-key map (kbd "j") 'auto-browser-anki-dwim)
    (define-key map (kbd "k") 'auto-browser-anki-again)
    (define-key map (kbd "r") 'auto-browser-anki-refresh)
    map))

(defun auto-browser-anki-dwim ()
  (interactive)
  (if auto-browser-anki--question-page-p
      (auto-browser-anki-show-answer)
    (auto-browser-anki-good)))


(provide 'anki)
;;; anki.el ends here
