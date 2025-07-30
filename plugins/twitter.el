;;; twitter.el ---                                   -*- lexical-binding: t; -*-

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

(defvar auto-browser-twitter--base-url "https://x.com")

(defvar auto-browser-twitter--buffer-name "*twitter*")

(defcustom auto-browser-twitter-rendering-functions nil
  "The twitter rendering functions like `shr-external-rendering-functions`"
  :type 'cons)

(defun auto-browser-twitter-following (&optional trace-id)
  "Show twitter study page."
  (interactive)
  (let* ((url (file-name-concat auto-browser-twitter--base-url "home"))
         (fowllowing-selector "text=Following"))
    (auto-browser-run-linearly
     `((auto-browser-get-tab ,url)
       (auto-browser-locate-element ,fowllowing-selector)
       (auto-browser-run-js "el => el.click()")
       (auto-browser-twitter-render)
       (auto-browser-twitter-next-tweet))
     trace-id)))

(defun auto-browser-twitter-render (trace-id)
  (auto-browser-run-linearly
   `((auto-browser-locate-element "main")
     (auto-browser-get-element "html")
     (auto-browser-twitter-following-render))
   trace-id))

(defun auto-browser-twitter-next-tweet ()
  "Show twitter study page."
  (interactive)
  (auto-browser-twitter-press "j"))

(defun auto-browser-twitter-previous-tweet ()
  "Show twitter study page."
  (interactive)
  (auto-browser-twitter-press "k"))

(defun auto-browser-twitter-tweet-detail ()
  "Show twitter study page."
  (interactive)
  (let* ((url (file-name-concat auto-browser-twitter--base-url "home"))
         (main-selector "main"))
    (auto-browser-run-linearly
     `((auto-browser-get-tab ,url t)
       (auto-browser-key-down "ENTER")
       (auto-browser-key-up "ENTER")
       (auto-browser-twitter-render)))))

(defun auto-browser-twitter-press (key)
  "Show twitter study page."
  (let* ((url (file-name-concat auto-browser-twitter--base-url "home"))
         (main-selector "main"))
    (auto-browser-run-linearly
     `((auto-browser-get-tab ,url t)
       (auto-browser-key-down ,key)
       (auto-browser-key-up ,key)
       (auto-browser-twitter-render)))))

(defun auto-browser-jump-to-shr-text-background ()
  "Jump to the next position with the specified text property."
  (interactive)
  (let ((prop '(:background "#ADD8E6" :extend t)))
    (text-property-search-forward 'face prop)))


(defun auto-browser-twitter--tweet-dom-selectedp (dom)
  (string-match "r-ukrunq" (dom-attr dom 'class)))

(defun auto-browser-twitter--tweet-dom-adp (dom)
  (member "Ad" (dom-strings dom)))

(defun auto-browser-twitter-html-parse (html)
  (let* ((dom (auto-browser-dom-parse-html html))
         (tweet-doms (auto-browser-dom-query-selector-all dom "article"))
         (user-name)
         (tweet-text)
         (datetime)
         (image)
         (selected)
         (ad-p)
         (tweets))
    (remove nil
            (mapcar
             (lambda (tweet-dom)
               (setq user-name (dom-texts (auto-browser-dom-query-selector-first tweet-dom "div[data-testid=User-Name] span")))
               (setq tweet-text (dom-texts (auto-browser-dom-query-selector-first tweet-dom "div[data-testid=tweetText]")))
               (setq datetime (dom-texts (auto-browser-dom-query-selector-first tweet-dom "div[data-testid=User-Name] time")))
               (setq selected (auto-browser-twitter--tweet-dom-selectedp tweet-dom))
               (setq image (auto-browser-dom-query-selector-first tweet-dom "div[aria-label=Image] img"))
               (setq ad-p (auto-browser-twitter--tweet-dom-adp tweet-dom))

               (when (not ad-p)
                 (list :user-name user-name
                       :tweet-text tweet-text
                       :datetime datetime
                       :image image
                       :selected selected)))
             tweet-doms))))

(defun auto-browser-twitter-following-render (trace-id html)
  "Show twitter HTML."
  (let ((shr-external-rendering-functions auto-browser-twitter-rendering-functions))

    (auto-browser-render-html
     (auto-browser-twitter-tweets-html
      (auto-browser-twitter-html-parse html))
     auto-browser-twitter--buffer-name)
    (twitter-list-mode)
    (auto-browser-jump-to-shr-text-background)
    (recenter)))


(defun auto-browser-twitter-tweets-html (tweets)
  (string-join
   (mapcar
    (lambda (tweet)
      (format "<div style=\"%s\">
               <h1>%s [%s]</h1>
               <span>%s<span>
               %s
               </div>"
              (when (plist-get tweet :selected)
                "background-color: '#ADD8E6';")
              (plist-get tweet :user-name)
              (plist-get tweet :datetime)
              (plist-get tweet :tweet-text)
              (when (plist-get tweet :image)
                (shr-dom-to-xml (plist-get tweet :image)))))
    tweets)
   "\n"))

(define-minor-mode twitter-list-mode "An twitter list mode"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") 'auto-browser-twitter-next-tweet)
    (define-key map (kbd "k") 'auto-browser-twitter-previous-tweet)
    (define-key map (kbd "<RET>") 'auto-browser-twitter-tweet-detail)
    map))



(provide 'twitter)
;;; twitter.el ends here
