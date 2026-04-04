;;; auto-browser-ctable.el ---                       -*- lexical-binding: t; -*-

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

(require 'ctable)

(cl-defun auto-browser-ctable-render (&key buffer-name headers table actions)
  "Render a clickable table in BUFFER-NAME using ctable.

HEADERS is a list of (KEY . WIDTH-RATIO) pairs where KEY is used
to look up values in each row hash-table, and WIDTH-RATIO scales
column width relative to `window-width'.

TABLE is a list of hash-tables, each representing a row.

ACTIONS is an optional function called when a table row is clicked."
  (with-current-buffer (get-buffer-create buffer-name)
    (let* ((column-model
            (mapcar
             (lambda (header)
               (make-ctbl:cmodel :title (format "%s" (car header))
                                 :align 'left
                                 :max-width (* (window-width) (cdr header))))
             headers))
           (data (mapcar
                  (lambda (row)
                    (append (mapcar (lambda (header) (gethash (car header) row)) headers)
                            (list row)))
                  table))
           (model (make-ctbl:model :column-model column-model :data data))
           (component)
           (inhibit-read-only t))
      (erase-buffer)
      (switch-to-buffer (current-buffer))
      (setq component (ctbl:create-table-component-region :model model))
      (goto-line 3)
      (when actions
        (ctbl:cp-add-click-hook component (lambda () (funcall actions)))))))

(provide 'auto-browser-ctable)
;;; auto-browser-ctable.el ends here
