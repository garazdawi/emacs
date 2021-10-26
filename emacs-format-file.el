;;; https://www.cslab.pepperdine.edu/warford/BatchIndentationEmacs.html
;;;
;;; File: emacs-format-file
;;; Stan Warford
;;; 17 May 2006
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/packages/") t)
 )

; Setup use-package for easy MELPA installs
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package editorconfig)
(require 'editorconfig)
(editorconfig-mode)

(use-package erlang)
(require 'erlang-start)
(erlang-mode)

(defun emacs-format-function ()
   (editorconfig-apply)
   (indent-region (point-min) (point-max) nil)
   (untabify (point-min) (point-max))
   (save-buffer)
)
