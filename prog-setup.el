;;; prog-setup.el --- my personal setup for prog-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Andrew De Angelis

;; Author: Andrew De Angelis <andyjda@Andrews-MacBook-Air-2.local>
;; Keywords: local, languages

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
;;;###autoload
(defun prog-setup (&optional absolute)
  (interactive "P")
  (prog-setup-appearance)
  (prog-setup-commands))

;;;; appearance
(defun prog-setup-appearance (&optional absolute)
"Set preferred appearance for prog-mode"
(display-line-numbers-mode t)
(if absolute
    (absolute-line-numbers-setup)
  (relative-line-numbers-setup))
(electric-pair-local-mode t))

(defun adj/:around-goto-line-read-args (origfn)
  (let ((display-line-numbers 'absolute))
    (funcall origfn)))

(defun relative-line-numbers-setup ()
  (interactive)
  (display-line-numbers-mode t)
  (setq display-line-numbers 'relative)
  (defun adj/:around-goto-line-read-args (origfn)
    (let ((display-line-numbers 'absolute))
      (funcall origfn)))
  (advice-add 'goto-line-read-args :around #'adj/:around-goto-line-read-args))

(defun absolute-line-numbers-setup ()
  (interactive)
  (setq display-line-numbers 'absolute)
  (display-line-numbers-mode)
  (advice-remove 'goto-line-read-args #'adj/:around-goto-line-read-args))

(defun line-numbers-global (setup)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when display-line-numbers-mode
        (funcall setup)))))

(defun absolute-line-numbers-global ()
  (interactive)
  (line-numbers-global #'absolute-line-numbers-setup)
  (remove-hook 'prog-mode-hook #'prog-setup)
  (add-hook 'prog-mode-hook (lambda () (prog-setup 'absolute))))

(defun relative-line-numbers-global ()
(interactive)
(line-numbers-global #'relative-line-numbers-setup)
(remove-hook 'prog-mode-hook (lambda () (prog-setup 'absolute)))
(add-hook 'prog-mode-hook #'prog-setup))

;;;; commands
(defun prog-setup-commands ()
  "Setup some custom commands for prog-mode"
  (message "major-mode: %s" major-mode)

  (setq lang-specific-run-this nil)
  (make-local-variable 'lang-specific-run-this)

  (when (setq lang-specific-run-this (prog-setup-get-func-run-this))
    (progn
      (message "lang-specific-run-this %s" lang-specific-run-this)
      (message "binding it")
      (local-set-key (kbd "C-c r") #'run-this)))
  (message "here")

  ;; TODO: generalize this logic:
  ;; make a variable buffer local, then assign it to its lang-specific value
  ;; TODO: write the lang-specific vars for javascript and rust
  (setq lang-specific-format-quoted-thing nil)
  (setq lang-specific-format-thing nil)
  (setq lang-specific-format nil)
  (make-local-variable 'lang-specific-format-quoted-thing)
  (make-local-variable 'lang-specific-format-thing)
  (make-local-variable 'lang-specific-format)
  (setq lang-specific-format-quoted-thing #'python-format-thing)
  (setq lang-specific-format-thing #'python-format-thing)
  (setq lang-specific-format "%s(f\"%s : {%s}\")")
  (setq lang-specific-action "print")
  
  (local-set-key (kbd "C-M-p") #'prog-setup-generic-debug)
  ;; (local-set-key (kbd "C-M l") #'print-log)
  (message "done"))

;;;;; run-this
(defun prog-setup-get-func-run-this ()
  (pcase major-mode
    ('python-mode #'python-run-this)
    ('web-mode #'js-run-this)
    ('emacs-lisp-mode #'elisp-eval-region-or-buffer)
    ('json-mode nil) ;; ignore it
    (_ (progn
         (message
          "prog-setup: this mode does not have a #'run-this implementation: %s"
          major-mode)
         nil))))

(defun run-this ()
  "Run the current program.
Use the function specific in lang-specific-run-this"
  (interactive)
  (if (called-interactively-p 'any)
      (call-interactively lang-specific-run-this)
    (funcall lang-specific-run-this)))


;;;;; debug-statements
(defun python-debug (action &optional arg)
  (let* ((thing (if (or arg (current-line-empty-p))
        	    (read-from-kill-ring (format "%s :" action))
        	  (progn
        	    (make-it-quiet (dwim-kill))
        	    (pop kill-ring))))
         (thing (string-replace "\"" "'" thing)))
    (insert (format "%s(f\"%s : {%s}\")" action thing thing))))

(defun python-format-thing (thing)
  (string-replace "\"" "'" thing))

(defun prog-setup-generic-debug ()
  (interactive)
  (let* ((thing (prog-setup-get-thing-to-print))
         (named-thing (funcall lang-specific-format-quoted-thing thing))
         (thing (funcall lang-specific-format-thing thing)))
    (insert (format lang-specific-format lang-specific-action named-thing thing)))
  )
(defun prog-setup-get-thing-to-print ()
  "Get the string to print.
Use dwim-kill to get the current region or line."
  (progn
    (make-it-quiet (dwim-kill))
    (pop kill-ring)))

(provide 'prog-setup)

;;; prog-setup.el ends here
