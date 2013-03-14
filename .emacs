; Add ~/.emacs.d to load path.
(setq load-path (cons "~/.emacs.d" load-path))

;(load-file "/usr/share/emacs/23.3/lisp/net/tramp.elc") 

;; To make font loading faster.
(modify-frame-parameters nil '((wait-for-wm . nil)))
;;(add-to-list 'default-frame-alist '(font . "Source Sans Pro"))

(hl-line-mode t)

;; Erlang Mode
(setq load-path (cons "~/.emacs.d/erlang" load-path))
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
        "/ldisk/lukas/eclipse/comte2/comte/include"
        "/ldisk/lukas/otp/lib/kernel/src/"
        "/ldisk/lukas/otp/lib/kernel/include/"))
(setq erlang-flymake-get-include-dirs-function 
      'my-erlang-flymake-get-include-dirs)

(setq distel-tags-compliant nil)

(setq *copyright-organization* "Erlang Solutions Ltd.")

;; Interactively Do Things (smart tab-completion in find file etc.)
(require 'ido)
(ido-mode t)

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

; a package which enables text selection with the shift and arrow keys
(load-library "pc-select")
(pc-selection-mode)

;; Load my custom shortcuts
(load-file "~/.emacs.d/my-shortcuts.el")

;; Move mode line to top (into the header line actually)
(setq erc-mode-line-format nil)
(setq erc-header-line-format "%s %a. %n on %t (%m,%l) %o")

;; Color Themes
(require 'color-theme)
(require 'color-theme-tango-3)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-tango-3)))

;; Outlining
; Erlang is enabled in the erlang-mode-hook above.
(add-hook 'emacs-lisp-mode-hook (lambda () (hs-minor-mode 1)))

(load-file "~/.emacs.d/graphviz-dot-mode.el") 

; Redo mode
(require 'redo+)
  (global-set-key (kbd "C-Z") 'redo)

(setq vc-checkout-switches "-r.")

(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

(setq path-to-ctags "/usr/bin/X11/ctags") ;; <- your ctags path here

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f %s/TAGS -e -R %s" path-to-ctags dir-name (directory-file-name dir-name)))
  )

;; auto-complete stuff
(setq load-path (cons "~/.emacs.d/auto-complete" load-path))
(require 'auto-complete)
(global-auto-complete-mode t)

(autoload 'django-html-mumamo-mode "~/.emacs.d/nxhtml/autostart.el")
(setq auto-mode-alist
      (append '(("\\.html?$" . django-html-mumamo-mode)) auto-mode-alist))
(setq mumamo-background-colors nil) 
(add-to-list 'auto-mode-alist '("\\.html$" . django-html-mumamo-mode))

;; Kill all buffers
(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer 
          (delq (current-buffer) 
                (remove-if-not 'buffer-file-name (buffer-list)))))

(setq load-path (cons "~/.emacs.d/markdown-mode" load-path))
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))
(setq show-trailing-whitespace 't)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ac-auto-start 4)
 '(ac-modes (quote (emacs-lisp-mode lisp-interaction-mode c-mode cc-mode c++-mode java-mode perl-mode cperl-mode python-mode ruby-mode ecmascript-mode javascript-mode js2-mode php-mode css-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode erlang-mode)))
 '(cua-mode t nil (cua-base))
 '(safe-local-variable-values (quote ((eval add-hook (quote write-file-hooks) (quote time-stamp)) (erlang-indent-level . 4) (erlang-indent-level . 2)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(font-lock-constant-face ((t (:foreground "#73d216"))))
 '(font-lock-type-face ((t (:foreground "YellowGreen")))))
(put 'erase-buffer 'disabled nil)
