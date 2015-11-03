; Setup package for MELPA installs
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

; Setup use-package for easy MELPA installs
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)
(setq use-package-always-ensure t)

; Add ~/.emacs.d to load path.
(setq load-path (cons "~/.emacs.d" load-path))

;; To make font loading faster.
(modify-frame-parameters nil '((wait-for-wm . nil)))
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono"))

(hl-line-mode t)

;; fix broken ubuntu emacs
(require 'iso-transl)

;; Erlang Mode
(use-package erlang)
(require 'erlang-start)
(setq auto-mode-alist (append auto-mode-alist
                              '(("\\.rel$" . erlang-mode)
                                ("\\.app$" . erlang-mode)
                                ("\\.appSrc$" . erlang-mode)
                                ("\\.app.src$" . erlang-mode)
                                ("\\.hrl$" . erlang-mode)
                                ("\\.erl$" . erlang-mode)
                                ("\\.yrl$" . erlang-mode))))

(add-hook 'erlang-mode-hook 'my-erlang-hook-function)
(defun my-erlang-hook-function ()
  (imenu-add-to-menubar "Functions")
  (setq erlang-skel-mail-address "lukas@erlang-solutions.com")
  (setq user-full-name "Lukas Larsson")
  (hs-minor-mode 1))

(require 'erlang-flymake)

(defun my-erlang-flymake-get-include-dirs ()
  (list (concat (erlang-flymake-get-app-dir) "include")
        "/ldisk/lukas/otp/lib/kernel/src/"
        "/ldisk/lukas/otp/lib/kernel/include/"))
(setq erlang-flymake-get-include-dirs-function 
      'my-erlang-flymake-get-include-dirs)

(setq distel-tags-compliant nil)

(setq *copyright-organization* "Erlang Solutions Ltd.")

;; Interactively Do Things (smart tab-completion in find file etc.)
(require 'ido)
(ido-mode t)

(use-package highlight-parentheses)
(require 'highlight-parentheses)
(defun turn-on-highlight-parentheses-mode ()
(highlight-parentheses-mode t))
(define-global-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  turn-on-highlight-parentheses-mode)
(global-highlight-parentheses-mode)

;; Xah Lee's ergonomic Ergomacs Keybindings
;; http://xahlee.org/emacs/ergonomic_emacs_keybinding.html
(load-file "~/.emacs.d/ergonomic_keybinding_dvorak.el")

;; Load my custom shortcuts
(load-file "~/.emacs.d/my-shortcuts.el")

;; Move mode line to top (into the header line actually)
(setq erc-mode-line-format nil)
(setq erc-header-line-format "%s %a. %n on %t (%m,%l) %o")

;; Color Themes
(use-package color-theme)
(require 'color-theme)
(require 'color-theme-tango-3)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-tango-3)))

;; Outlining
; Erlang is enabled in the erlang-mode-hook above.
(add-hook 'emacs-lisp-mode-hook (lambda () (hs-minor-mode 1)))

; Redo mode
(use-package redo+)
(require 'redo+)
(global-set-key (kbd "C-Z") 'redo)

(setq vc-checkout-switches "-r.")

(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

;; auto-complete stuff
(use-package popup)
(use-package auto-complete)
(require 'auto-complete)
(global-auto-complete-mode t)

(use-package markdown-mode)
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
(require 'whitespace)
(global-whitespace-mode t)

(setq-default c-default-style "k&r"
              c-basic-offset 4
              tab-width 8
              indent-tabs-mode nil)

(add-hook 'erlang-mode-hook 'flyspell-prog-mode)
(add-hook 'c-mode-common-hook 'flyspell-prog-mode)

(use-package ggtags)
(require 'ggtags)

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))
(defun my-ggtags-find-tag-dwim ()
  "Find and move to current file first"
  (ggtags-find-tag-dwim)
  (ggtags-navigation-start-file))

(global-set-key (kbd "M-.") 'my-ggtags-find-tag-dwim)

(use-package llvm-mode)
(require 'llvm-mode)
;; llvm coding standard
(c-add-style "llvm.org"
             '("gnu"
	       (fill-column . 80)
	       (c++-indent-level . 2)
	       (c-basic-offset . 2)
	       (indent-tabs-mode . nil)
	       (c-offsets-alist . ((arglist-intro . ++)
				   (innamespace . 0)
				   (member-init-intro . ++)))))

;; Files with "llvm" in their names will automatically be set to the
;; llvm.org coding style.
(add-hook 'c-mode-common-hook
	  (function
	   (lambda nil
	     (if (string-match "llvm" buffer-file-name)
		 (progn
		   (c-set-style "llvm.org"))))))

;;;
;;; Custom emacs functions
;;;

;; Kill all buffers
(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer 
          (delq (current-buffer) 
                (remove-if-not 'buffer-file-name (buffer-list)))))

;; Reload tags file
(defun find-file-upwards (file-to-find)
  "Recursively searches each parent directory starting from the default-directory. looking for a file with name file-to-find.  Returns the path to it or nil if not found."
  (labels
      ((find-file-r (path)
                    (let* ((parent (file-name-directory path))
                          (possible-file (concat parent file-to-find)))
                     (cond
                      ((file-exists-p possible-file) possible-file) ; Found
                       ;; The parent of ~ is nil and the parent of / is itself.
                       ;; Thus the terminating condition for not finding the file
                       ;; accounts for both.
                      ((or (null parent) (equal parent (directory-file-name parent))) nil) ; Not found
                      (t (find-file-r (directory-file-name parent))))))) ; Continue
   (find-file-r default-directory)))
(defun reload-tags-file ()
  "Load a new version of the first TAGS file found"
  (interactive)
  (let ((my-tags-file (find-file-upwards "TAGS")))
    (when my-tags-file
      (message "Loading tags file: %s" my-tags-file)
      (visit-tags-table my-tags-file))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-start 4)
 '(ac-modes (quote (emacs-lisp-mode lisp-interaction-mode c-mode cc-mode c++-mode java-mode perl-mode cperl-mode python-mode ruby-mode ecmascript-mode javascript-mode js2-mode php-mode css-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode erlang-mode)))
 '(cua-mode t nil (cua-base))
 '(safe-local-variable-values (quote ((c-continued-statement-offset . 2))))
 '(whitespace-style (quote (face trailing lines empty))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-constant-face ((t (:foreground "#73d216"))))
 '(font-lock-type-face ((t (:foreground "YellowGreen")))))
(put 'erase-buffer 'disabled nil)
