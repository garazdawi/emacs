; Setup package for MELPA installs
;; packages
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
 )

; Setup use-package for easy MELPA installs
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)
(setq use-package-always-ensure t)

(load-theme 'adwaita t)

; Add ~/.emacs.d to load path.
(setq load-path (cons "~/.emacs.d/lisp" load-path))

(require 're-builder)
(setq reb-re-syntax 'string)

;; fix broken ubuntu emacs
(require 'iso-transl)

(use-package company)
(require 'company)

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
(add-hook 'erlang-mode-hook 'company-mode)
(defun my-erlang-hook-function ()
  (imenu-add-to-menubar "Functions")
  (setq erlang-skel-mail-address "lukas.larsson@erlang-solutions.com")
  (setq user-full-name "Lukas Larsson")
  (hs-minor-mode 1))

(require 'erlang-flymake)

(defun my-erlang-flymake-get-include-dirs ()
  (list (concat (erlang-flymake-get-app-dir) "include")
        ))
(setq erlang-flymake-get-include-dirs-function 
      'my-erlang-flymake-get-include-dirs)

(setq exec-path (append exec-path '("~/apps/erlang/bin")))

; Add ~/.emacs.d to load path.
(setq load-path (cons "~/.emacs.d/yaemep" load-path))

;; Install yaemep-completion-mode
;; (Completion command can be invoked with "M-TAB")
(require 'yaemep-completion-mode)

;; Install yaemep-etags-auto-gen-mode
;; (Use "M-." to go to thing at point and "M-," to go back")
(require 'yaemep-etags-auto-gen-mode)

;; Install yaemep-extra-erlang-menu-mode
(require 'yaemep-extra-erlang-menu-mode)

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

;; Outlining
; Erlang is enabled in the erlang-mode-hook above.
(add-hook 'emacs-lisp-mode-hook (lambda () (hs-minor-mode 1)))

; Redo mode
;(use-package redo+)
;(require 'redo+)
(global-set-key (kbd "C-Z") 'redo)

(setq vc-checkout-switches "-r.")

(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

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
(add-hook 'c-mode-common-hook 'company-mode)

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

(add-hook 'ggtags-mode
          (lambda ()
            (local-set-key (kbd "M-.") 'my-ggtags-find-tag-dwim)))

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

(use-package groovy-mode)
(require 'groovy-mode)

(use-package dockerfile-mode)
(require 'dockerfile-mode)

(use-package docker-compose-mode)
(require 'docker-compose-mode)

(use-package graphviz-dot-mode)
(require 'graphviz-dot-mode)

(use-package rust-mode)
(require 'rust-mode)

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

(defun gdb-beam-env (var)
  "Get an environment variable value from something set in ~./.bashrc"
  (shell-command-to-string (concat "/bin/bash -i -c 'echo -n $" var "' 2>/dev/null"))
)

(defun gdb-beam ()
  "Start a debug beam emulator"
  (interactive)
  (let ((ERL_TOP (gdb-beam-env "ERL_TOP"))
        (PATH (gdb-beam-env "PATH")))
    (let ((EMU_ARGS (replace-regexp-in-string
                     "\n" " "
                     (shell-command-to-string
                      (concat ERL_TOP "/bin/cerl -emu_args_exit"))))
          (BINDIR (replace-regexp-in-string
                   "\n\\'" ""
                   (concat
                    ERL_TOP "/bin/"
                    (shell-command-to-string
                     (concat ERL_TOP "/erts/autoconf/config.guess"))))
                  ))
      (progn
        (gdb (concat "gdb -i=mi " BINDIR "/beam.debug.smp"))
        (insert-string (concat "set args " EMU_ARGS))
        (comint-send-input)
        (insert-string (concat "source " ERL_TOP "/erts/etc/unix/etp-commands"))
        (comint-send-input)
        (insert-string (concat "set environment BINDIR=" BINDIR))
        (comint-send-input)
        (insert-string (concat "set environment PATH=" PATH ":" BINDIR))
        (comint-send-input)
        (insert-string "run")
        )
      )
    )
  )

(defun erl-test-env ()
  "Convert a otp_client_build env to export entries"
  (interactive)
  (progn
    (goto-char (point-min))
    (replace-regexp "^" "export ")
    (goto-char (point-min))
    (replace-regexp "$" "\"")
    (goto-char (point-min))
    (replace-string " = " "=\"")
    )
)

(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 4024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (highlight-parentheses-mode nil)
    (setq whitespace-style nil)
    (fundamental-mode)))

(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)

(setq line-number-display-limit-width 2000000)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-start 4)
 '(ac-modes
   (quote
    (emacs-lisp-mode lisp-interaction-mode c-mode cc-mode c++-mode java-mode perl-mode cperl-mode python-mode ruby-mode ecmascript-mode javascript-mode js2-mode php-mode css-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode erlang-mode)))
 '(cua-mode t nil (cua-base))
 '(gdb-create-source-file-list nil)
 '(package-selected-packages
   (quote
    (rust-mode docker-compose-mode dockerfile-mode groovy-mode solarized-theme use-package redo+ markdown-mode llvm-mode highlight-parentheses graphviz-dot-mode ggtags erlang color-theme)))
 '(safe-local-variable-values (quote ((c-continued-statement-offset . 2))))
 '(whitespace-style (quote (face trailing lines-tail empty))))
(put 'erase-buffer 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-line ((t (:background "gray")))))
