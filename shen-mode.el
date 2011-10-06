;;; shen-mode.el --- A major mode for editing shen source code

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Version: 0.1
;; Keywords: languages, shen
;; Description: A major mode for editing shen source code

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A minor mode for editing shen source code.

;;; Code:
(require 'lisp-mode)
(require 'shen-functions)
(require 'imenu)

(defcustom shen-mode-hook '(turn-on-eldoc-mode)
  "Normal hook run when entering `shen-mode'."
  :type 'hook
  :group 'shen)

(defvar shen-mode-map
  ((lambda (map) (set-keymap-parent map lisp-mode-shared-map) map)
   (make-sparse-keymap))
  "Currently just inherits from `lisp-mode-shared-map'.")


;;; Fontification
(defconst shen-font-lock-keywords
  (eval-when-compile
    `(;; definitions
      (,(concat "(\\("
                (regexp-opt
                 '("define" "defmacro" "defprolog" "/." "synonyms"))
                "\\)\\>"
                "[ \t]*(?"
                "\\(\\sw+\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))
      ("(\\(lambda\\)\\>[ \t]*(?\\(\\sw+\\)?"
       (1 font-lock-keyword-face)
       (2 font-lock-variable-name-face nil t))
      ;; data types
      ("(\\(datatype\\)\\>[ \t]*(?\\(\\sw+\\)?"
       (1 font-lock-keyword-face)
       (2 font-lock-type-face nil t))
      ;; variables
      ("\\<\\([A-Z]\\w*\\)\\>" . font-lock-variable-name-face)
      ;; control structures
      (,(concat
         "("
         (regexp-opt
          (append
           '("let" "=" "eval-without-reader-macros" "freeze" "type") ; generic
           '("if" "and" "or" "cond")) t) ; boolean
         "\\>") . 1)
      ;; errors
      ("(\\(error\\)\\>" 1 font-lock-warning-face)
      ;; built-in
      (,(concat
         "("
         (regexp-opt
          (mapcar
           (lambda (it) (format "%s" it))
           (append
            '(intern function)                          ; symbols
            '(pos tlstr cn str string?)                 ; strings
            '(set value)                                ; assignment
            '(cons hd tl cons?)                         ; lists
            '(absvector address-> <-address absvector?) ; vector
            '(pr read-byte open close)                  ; stream
            '(get-time)                                 ; time
            '(+ - * / > < >= <= number?)                ; arithmetic
            '(fst snd tupple?)                          ; tuple
            '(@s @v @p)
            '(put get)                  ; property lists
            '(simple-error trap-error error-to-string) ; error
            ;; predicates
            (mapcar
             (lambda (it) (format "%s?" it))
             '(boolean character complex congruent cons element empty float
                       integer number provable rational solved string symbol
                       tuple variable))
            ;; misc functions
            (mapcar #'car shen-functions)
            shen-more-functions))
          t)
         "\\>")
       1 font-lock-builtin-face)
      ;; external global variables
      (,(concat
         (regexp-opt
          (mapcar
           (lambda (cnst) (format "*%s*" cnst))
           '("language" "implementation" "port" "porters"
             "stinput" "home-directory" "version"
             "maximum-print-sequence-size" "printer" "macros")) t)
         "\\>")
       1 font-lock-builtin-face)))
  "Default expressions to highlight in Shen mode.")

(defvar shen-mode-syntax-table
  (let ((table (make-syntax-table)))
    (dolist (pair '((?@  . "w")
                    (?_  . "w")
                    (?-  . "w")
                    (?+  . "w")
                    (??  . "w")
                    (?!  . "w")
                    (?<  . "w")
                    (?>  . "w")
                    (?/  . "w")
                    ;; comment delimiters
                    (?\\ . ". 14")
                    (?*  . ". 23")))
      (modify-syntax-entry (car pair) (cdr pair) table))
    table)
  "Syntax table to use in shen-mode.")


;;; Indentation
;; Copied from qi-mode, which in turn is from scheme-mode and from lisp-mode
(defun shen-indent-function (indent-point state)
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
      ;; car of form doesn't seem to be a symbol
      (progn
        (if (not (> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
        ;; Indent under the list or under the first sexp on the same
        ;; line as calculate-lisp-indent-last-sexp.  Note that first
        ;; thing on that line has to be complete sexp since we are
        ;; inside the innermost containing sexp.
        (backward-prefix-chars)
        (current-column))
      (let ((function (buffer-substring (point)
					(progn (forward-sexp 1) (point))))
	    method)
	(setq method (or (get (intern-soft function) 'shen-indent-function)
			 (get (intern-soft function) 'shen-indent-hook)))
	(cond ((or (eq method 'defun)
		   (and (null method)
			(> (length function) 3)
			(string-match "\\`def" function)))
	       (lisp-indent-defform state indent-point))
	      ((integerp method)
	       (lisp-indent-specform method state
				     indent-point normal-indent))
	      (method
               (funcall method state indent-point normal-indent)))))))

(defun shen-let-indent (state indent-point normal-indent)
  (let ((edge (- (current-column) 2)))
    (goto-char indent-point) (skip-chars-forward " \t")
    (if (looking-at "[-a-zA-Z0-9+*/?!@$%^&_:~]")
        ;; deeper indent because we're still defining local variables
        (lisp-indent-specform 5 state indent-point normal-indent)
      ;; shallow indent because we're in the body
      edge)))

(defun shen-package-indent (state indent-point normal-indent)
  (- (current-column) 8))

(put 'let 'shen-indent-function 'shen-let-indent)
(put 'lambda 'shen-indent-function 1)
(put 'package 'shen-indent-function 'shen-package-indent)
(put 'datatype 'shen-indent-function 1)


;;; Function documentation
(defun shen-current-function ()
  (ignore-errors
    (save-excursion
      (backward-up-list)
      (forward-char 1)
      (thing-at-point 'word))))

(defun shen-mode-eldoc ()
  (let ((func (assoc (intern (or (shen-current-function) "")) shen-functions)))
    (when func
      (format "%s%s:%s"
              (propertize (symbol-name (car func))
                          'face 'font-lock-function-name-face)
              (if (cadr func)  (concat "[" (cadr func) "]") "")
              (if (caddr func) (concat " " (caddr func)) "")))))

(defvar shen-imenu-generic-expression
  '(("Functions" "^\\s-*(\\(define\\)" 1)))


;;; Major mode definition
;; apparently some versions of Emacs don't have `prog-mode' defined
(unless (fboundp 'prog-mode)
  (defalias 'prog-mode 'fundamental-mode))

(define-derived-mode shen-mode prog-mode "shen"
  "Major mode for editing Shen code."
  :syntax-table shen-mode-syntax-table
  ;; set a variety of local variables
  ((lambda (local-vars)
     (dolist (pair local-vars)
       (set (make-local-variable (car pair)) (cdr pair))))
   `((adaptive-fill-mode . nil)
     (fill-paragraph-function . lisp-fill-paragraph)
     (indent-line-function . lisp-indent-line)
     (lisp-indent-function . shen-indent-function)
     (parse-sexp-ignore-comments . t)
     (comment-start . "\\* ")
     (comment-end . " *\\")
     (comment-add . 0)
     (comment-column . 32)
     (parse-sexp-ignore-comments . t)
     (comment-use-global-state . nil)
     (eldoc-documentation-function . shen-mode-eldoc)
     (imenu-case-fold-search . t)
     (imenu-generic-expression . ,shen-imenu-generic-expression)
     (mode-name . "Shen")
     (font-lock-defaults . (shen-font-lock-keywords)))))

(add-to-list 'auto-mode-alist '("\\.shen\\'" . shen-mode))

(provide 'shen-mode)
;;; shen-mode.el ends here
