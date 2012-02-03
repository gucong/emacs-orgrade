;; orgrade.el --- grade book built upon orgtbl that accelerates input

;; Author: Cong Gu <gucong43216@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; commentary:

;; The aim of this software is to accelerate grade input.
;; It employs orgtbl minor mode for advanced table and spreadsheet
;; functionality.

;; To setup score board mode, put this file in your `load-path'
;; and the following into your ~/.emacs :
;;     (require 'orgrade)
;;     (add-to-list 'auto-mode-alist '("\\.orgrade\\'" . orgrade-mode))

;; To start using score board mode,
;; open an empty score board file, say "sample.orgrade";
;; then M-x `orgrade-insert-template' (C-c t) to insert the template;
;; input your roster under the "name" column;
;; finally, start inserting data using M-x `orgrade-insert-entry' for single data
;; or M-x `orgrade-multiple-entry' (C-c i) for multiple data repeatedly.

;; To exchange data with other table or spreadsheet software,
;; use tsv (tab separated values) or csv (comma separated values) files.
;; see `org-table-export' (C-c e) and `org-table-import' for how to do this.

;; See table functionality of `org-mode' to do advanced table editting
;; and spreadsheet computation.

(require 'org-table)

(defun mark-line ()
  (interactive)
  (push-mark (point))
  (push-mark (beginning-of-line) t t)
  (end-of-line))

(defun org-table-get-field-clean (&optional n replace)
  "Return the value of the field in column N of current row.
text properties, leading and trailing whitepaces are discarded.
see `org-table-get-field'."
  (let ((str (org-table-get-field n replace)))
    (string-match "^[ ]*\\(.*?\\)[ ]*$" str)
    (substring-no-properties str (match-beginning 1) (match-end 1))))

(defun org-table-get-field-number (&optional n replace)
  "Return the value of the field in column N of current row as number.
see `org-table-get-field'."
  (string-to-number (org-table-get-field n replace)))

(defun orgrade-generate-roster ()
  "Generate a list of all names in the score board"
  (let ((roster nil) (pt (point)))
    (org-table-align)
    (goto-char (org-table-begin))
    (forward-line 4)
    (while (org-at-table-p)
      (setq roster (cons (org-table-get-field-clean 1) roster))
      (forward-line))
    (goto-char pt)
    (reverse roster)))

(defun orgrade-generate-events ()
  "Generate a list of all events in the score board"
  (let ((events nil) (cur-event nil) (i 3)
        (pt (point)))
    (org-table-align)
    (goto-char (org-table-begin))
    (setq cur-event (org-table-get-field-clean i))
    (while (not (equal cur-event ""))
      (setq events (cons (list cur-event i) events))
      (setq i (+ i 1))
      (setq cur-event (org-table-get-field-clean i)))
    (goto-char pt)
    events))

(defvar orgrade-template
  "|name|sec|sample event|
|-
|*full score*
|-
|sample name")

(defun orgrade-insert-template (&optional proceed)
  "insert the template for a score board file."
  (interactive
   (list (if (= (point-max) 1) (list t)
           (yes-or-no-p "This will erase the buffer. Proceed ?"))))
  (when proceed
    (erase-buffer)
    (insert orgrade-template)
    (org-table-align)
    (orgrade-mode)))

(defun orgrade-goto-name (name)
  "Go to the line of a given NAME."
  (interactive
   (list (ido-completing-read "name: " (orgrade-generate-roster) nil t)))
  (goto-char (point-min))
  (search-forward name)
  (org-table-beginning-of-field 0))

(defun orgrade-insert-entry (col name val)
  "Insert one entry with value VAL for NAME.
COL is the column number,

When used interactively, event name will be prompted instead of COL number,
If the event is not there yet, a new column will be created.

see `orgrade-multiple-entry'."
  (interactive
   (let ((events (orgrade-generate-events)))
    (list (or (cadr (assoc (ido-completing-read "event: " (reverse events)) events))
              (+ (cadar events) 1))
          (ido-completing-read "name: " (orgrade-generate-roster) nil t)
          (read-string "value: "))))
  (orgrade-goto-name name)
  (org-table-get-field col val)
  (org-table-align))

(defun orgrade-multiple-entry (&optional sec)
  "Interactively insert multiple entries.
If SEC is given, the sec column of all queried name will be updated with SEC.
New event will be automatically created when necessary.
see `orgrade-insert-entry'."
  (interactive "Psec: ")
  (let* ((events (orgrade-generate-events))
         (next (+ 1 (or (cadar events) 2)))
         (input (ido-completing-read "event: " (reverse events)))
         (hit (cadr (assoc input events)))
         (col (or hit next))
         (roster (orgrade-generate-roster)))
    (unless hit
      (goto-char (org-table-begin))
      (org-table-get-field col input))
    (loop do
          (let* ((name (ido-completing-read "name: " roster nil t))
                 (get-sec (save-excursion
                            (orgrade-goto-name name)
                            (org-table-get-field-clean 2))))
            (orgrade-insert-entry col name (read-string (format "sec: %s | value: " get-sec)))
            (when sec (orgrade-insert-entry 2 name (number-to-string sec)))
            (orgrade-goto-name name)
            (mark-line)))))

(defvar orgrade-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c t") 'orgrade-insert-template)
    (define-key map (kbd "C-c i") 'orgrade-multiple-entry)
    (define-key map (kbd "C-c n") 'orgrade-goto-name)
    (define-key map (kbd "C-c e") 'org-table-export)
    map)
  "keymap for `orgrade-mode'.")

(define-derived-mode orgrade-mode nil "orgrade"
  "score board mode
To start using score board mode,
open an empty score board file, say \"sample.orgrade\";
then M-x `orgrade-insert-template' (C-c t) to insert the template;
input your roster under the \"name\" column;
finally, start inserting data using M-x `orgrade-insert-entry' for single data
or M-x `orgrade-multiple-entry' (C-c i) for multiple data repeatedly.

To exchange data with other table or spreadsheet software,
use tsv (tab separated values) or csv (comma separated values) files.
see `org-table-export' (C-c e) and `org-table-import' for how to do this.

See table functionality of `org-mode' to do advanced table editting
and spreadsheet computation.
"
  (orgtbl-mode))

(provide 'orgrade)