;;; pretty-custom -- set customizations programatically -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2018 James Alexander Feldman-Crough
;;
;; Author: James Alexander Feldman-Crough <https://quodli.bet>
;; Maintainer: James Alexander Feldman-Crough <https://quodli.bet>
;; Created: May 26, 2018
;; Modified: May 26, 2018
;; Version: 0.1.0
;; Keywords: customize
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; A simple way to set custom variables in your init file.
;;
;;; Code:
(require 'custom)

(defvar -pretty-custom-customizations nil
  "A list of customizations collected durining initialization.
If t, customizations have already been loaded, and
`custom-set-variable' should be called instead of adding to this
list.")

(defun -pretty-custom-trigger ()
  "Preform customizations."
  (unless (eq -pretty-custom-customizations t)
    (apply #'custom-set-variables
	   (reverse -pretty-custom-customizations))
    (setq -pretty-custom-customizations t))
  nil)

(add-hook 'after-init-hook #'-pretty-custom-trigger)

;;;###autoload
(defmacro custom! (sym value &rest args)
  "Set a custom variable to be loaded after init.
SYM is the variable name, VALUE is the value to set it to.

ARGS is a list of optional arguments:
- If the first argument is a string, use it as the comment
- After the keyword `:require', a list of symbols specifying
required features should be provided."
  (declare (indent 2)
           (doc-string 3))
  (let ((comment (when (stringp (car args))
                   (pop args)))
        (req-list (when (eq :require (car args))
                    (prog1 (cdr args)
                      (setq args nil)))))
    (when args
      (user-error "Unexpected arguments to `custom!': %s"
                  args))
    `(if (listp -pretty-custom-customizations)
	 (push (list (quote ,sym)
		     ,value           	 
		     nil
		     (quote ,req-list)
		     ,comment)
	       -pretty-custom-customizations)
       (mapc #'require (quote ,req-list))
       (customize-set-variable (quote ,sym) ,value ,comment))))

(provide 'pretty-custom)
