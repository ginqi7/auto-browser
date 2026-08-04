;;; auto-browser-vui.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; Author: Qiqi Jin  <ginqi7@gmail.com>
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

(require 'vui)

(defun auto-browser-vui-sorted-by (sorted-key table)
  "Sort TABLE by the hash value of SORTED-KEY using string comparison."
  (sort table
        :key (lambda (row) (gethash sorted-key row))
        :lessp #'string<))

(defun auto-browser-vui--to-column (header)
  "Convert a single HEADER (a cons cell) into a vui column spec."
  (list :header (format "%s" (car header))
        :width 50 :truncate t
        :align :left))

(defun auto-browser-vui--to-columns (headers)
  "Convert HEADERS into a vui column spec list, prepending and appending an empty column."
  (append
   (list (list :header ""))
   (mapcar #'auto-browser-vui--to-column headers)
   (list (list :header ""))))

(cl-defun auto-browser-vui--run-actions (&key actions selected row id-key table)
  "Run the ACTIONS transient prefix with the appropriate rows.

If SELECTED is non-nil, filter TABLE to rows whose id-key is in SELECTED;
otherwise pass just ROW as a single-element list."
  (funcall
   actions
   (if selected
       (cl-remove-if-not (lambda (row) (member (gethash id-key row) selected)) table)
     (list row)))
  (sit-for 1)
  (vui-set-state :selected nil))

(cl-defun auto-browser-vui--to-row (&key headers selected actions table id-key row)
  "Build a vui row from ROW using HEADERS, SELECTED, ACTIONS, TABLE, and ID-KEY.

Each row contains a checkbox, cell values for each header, and an Actions button."
  (append
   (list
    (vui-checkbox :checked (member (gethash id-key row) selected)
                  :on-change
                  (lambda (v)
                    (if v
                        (vui-set-state :selected (push (gethash id-key row) selected))
                      (vui-set-state :selected (remove (gethash id-key row) selected))))))
   (mapcar (lambda (header) (gethash (car header) row)) headers)
   (list (vui-button "Actions" :on-click
                     (lambda ()
                       (when actions
                         (auto-browser-vui--run-actions
                          :id-key id-key
                          :actions actions
                          :selected selected
                          :row row
                          :table table)))))))

(cl-defun auto-browser-vui--to-rows (&key headers table sorted selected actions id-key)
  "Map TABLE to a list of vui rows, sorted by SORTED and filtered by SELECTED."
  (mapcar
   (apply-partially #'auto-browser-vui--to-row
                    :headers headers
                    :selected selected
                    :actions actions
                    :table table
                    :id-key id-key
                    :row)
   (auto-browser-vui-sorted-by sorted table)))

(cl-defun auto-browser-vui-render (&key name buffer-name headers table actions title id-key)
  "Render a vui table in BUFFER-NAME with the given parameters.

NAME is the component name, HEADERS defines columns, TABLE is the data,
ACTIONS is the transient prefix, TITLE is displayed as heading, and
ID-KEY is the hash key used to uniquely identify each row for selection."
  (eval
   `(vui-defcomponent ,name (headers table title actions id-key)
      :state ((selected nil)
              (sorted (car (car headers))))
      :render
      (vui-vstack
       (vui-heading (format "%s" title))
       (vui-hstack
        (vui-heading-2 "Sorted by: ")
        (vui-select :value sorted
                    :options (mapcar (lambda (header) (car header)) headers)
                    :prompt "Sorted by: "
                    :on-change (lambda (v) (vui-set-state :sorted v))))
       (vui-table
        :columns
        (auto-browser-vui--to-columns headers)
        :rows
        (auto-browser-vui--to-rows
         :headers headers
         :table table
         :sorted sorted
         :selected selected
         :actions actions
         :id-key id-key)
        :border :unicode)))
   t)
  (vui-mount (vui-component name
               :headers headers
               :table table
               :title title
               :actions actions
               :id-key id-key)
             buffer-name))

(provide 'auto-browser-vui)
;;; auto-browser-vui.el ends here
