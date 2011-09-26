;;; shen-mode.el --- A major mode for editing shen source code

;; Copyright (C) 2011 Eric Schulte

;; Author: Eric Schulte <schulte dot eric at gmail dot com>

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file borrows from qi-mode, clojure-mode and scheme-mode

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

(defconst shen-font-lock-keywords
  (eval-when-compile
    `( ;; definitions
      (,(concat "(\\("
                (regexp-opt
                 '("defun" "defmacro" "lambda" "/." "define" "defprolog"))
                "\\)\\>"
                "[ \t]*(?"
                "\\(\\sw+\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))
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
            '(simple-error error trap-error error-to-string) ; error
            ;; predicates
            (mapcar
             (lambda (it) (format "%s?" it))
             '(boolean character complex congruent cons element empty float
                       integer number provable rational solved string symbol
                       tuple variable))
            ;; misc functions
            '(append apply apt-credits apt-prompt cd collect concat difference
                     eval explode fix gensym head include include-all-but
                     inferences input length lineread)
            ))
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
       1 font-lock-builtin-face)
      ;; comments
      ("\\\\\\*[^\000]*?\\*\\\\" 0 font-lock-comment-face)))
  "Default expressions to highlight in Shen mode.")

;; ;; we can just let this inherit from prog-mode for now
;; (defvar shen-mode-syntax-table
;;   (let ((table (make-syntax-table))) table)
;;   "Syntax table to use in shen-mode.")

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

(put 'let 'shen-indent-function 'shen-let-indent)

(defun shen-current-function ()
  (save-excursion
    (backward-up-list)
    (forward-char 1)
    (thing-at-point 'word)))

(defun shen-mode-eldoc ()
  (let ((func (assoc (intern (shen-current-function)) shen-functions)))
    (format "%s[%s]: %s"
            (propertize (symbol-name (car func))
                        'face 'font-lock-function-name-face)
            (cadr func) (caddr func))))

(defvar shen-imenu-generic-expression
  '(("Functions" "^\\s-*(\\(define\\)" 1)))

(define-derived-mode shen-mode prog-mode "shen"
  "Major mode for editing Shen code."
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
     (font-lock-defaults
      . (shen-font-lock-keywords
         nil nil (("+-*/.<>=!?$%_&~^:@" . "w")) nil
         (font-lock-mark-block-function . mark-defun)
         (font-lock-syntactic-face-function
          . lisp-font-lock-syntactic-face-function))))))

(provide 'shen-mode)
;;; shen-mode.el ends here
