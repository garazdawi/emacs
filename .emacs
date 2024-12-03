; Setup package for MELPA installs
;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; https://stackoverflow.com/questions/26108655/error-updating-emacs-packages-failed-to-download-gnu-archive
; (setq package-check-signature nil)
(package-install 'gnu-elpa-keyring-update)

; Setup use-package for easy MELPA installs
(package-refresh-contents)

;; Define a utility function which either installs a package (if it is
;; missing) or requires it (if it already installed).
(defun package-require (pkg &optional require-name)
  "Install a package only if it's not already installed."
  (when (not (package-installed-p pkg))
    (package-install pkg))
  (if require-name
      (require require-name)
    (require pkg)))

(package-require 'use-package)
(setq use-package-always-ensure t)

(package-require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; (load-theme 'adwaita t)
(package-install 'alect-themes)
(load-theme 'alect-light-alt t)

; Add ~/.emacs.d to load path.
(setq load-path (cons "~/.emacs.d/lisp" load-path))

(package-require 'editorconfig)
(editorconfig-mode)

(require 're-builder)
(setq reb-re-syntax 'string)

;; fix broken ubuntu emacs
(require 'iso-transl)

(package-require 'company)
(package-require 'company-irony)

; Needs apt-get install libclang-8-dev
(package-require 'irony)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package flycheck-irony)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; Erlang Mode with lsp
(package-require 'erlang)
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

(with-eval-after-load 'lsp-mode
  ;; ELP, added as priority 0 (> -1) so takes priority over the built-in one
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("elp" "server"))
                    :major-modes '(erlang-mode)
                    :priority 0
                    :server-id 'erlang-language-platform))
  )


;; Set the path where we can find the correct erlang_lsp
;; (setq exec-path (append exec-path '("/home/eluklar/git/erlang-ls/_build/default/bin/")))
;; (setq exec-path (append exec-path '("/home/eluklar/git/erlang-ls/_build/dap/bin/")))

; (setq lsp-erlang-server-path "erlang_ls --log-level all")

(package-require 'which-key)
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

;; Customize prefix for key-bindings
(setq lsp-keymap-prefix "C-L")

;; Include the Language Server Protocol Clients
(package-require 'lsp-mode)

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]bootstrap\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]release\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]system\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]ebin\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]doc\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]emulator\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]make_test_dir\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]lib_src\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "SUITE_data\\'")
  )

;; Enable LSP for Erlang files
(add-hook 'erlang-mode-hook #'lsp)

(package-require 'elixir-mode)
; (add-hook 'elixir-mode-hook #'lsp)
(use-package lsp-mode
    :commands lsp
    :ensure t
    :diminish lsp-mode
    :hook
    (elixir-mode . lsp)
    :init
    (add-to-list 'exec-path "/home/eluklar/git/elixir-ls/release"))

;; Require and enable the Yasnippet templating system
(package-require 'yasnippet)
(yas-global-mode t)

;; Enable logging for lsp-mode
;; (setq lsp-log-io f)

;; Show line and column numbers
(add-hook 'erlang-mode-hook 'linum-mode)
(add-hook 'erlang-mode-hook 'column-number-mode)

;; Enable and configure the LSP UI Package
(package-require 'lsp-ui)
(setq lsp-ui-sideline-enable t)
;(setq lsp-ui-doc-enable nil)
(setq lsp-ui-doc-position 'bottom)
(setq lsp-inlay-hint-enable t)

;; Enable LSP Origami Mode (for folding ranges)
(package-require 'lsp-origami)
(add-hook 'origami-mode-hook #'lsp-origami-mode)
(add-hook 'erlang-mode-hook #'origami-mode)

;; Provide commands to list workspace symbols:
;; - helm-lsp-workspace-symbol
;; - helm-lsp-global-workspace-symbol
(package-install 'helm-lsp)

;; Which-key integration
(package-require 'which-key)
(add-hook 'erlang-mode-hook 'which-key-mode)
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

;; Always show diagnostics at the bottom, using 1/3 of the available space
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
              (display-buffer-reuse-window
               display-buffer-in-side-window)
              (side            . bottom)
              (reusable-frames . visible)
              (window-height   . 0.33)))

(setq *copyright-organization* "Erlang Solutions Ltd.")

;; Interactively Do Things (smart tab-completion in find file etc.)
(require 'ido)
(ido-mode t)

(package-require 'gist)

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

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
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

;; (require 'llvm-mode)
;; ;; llvm coding standard
;; (c-add-style "llvm.org"
;;              '("gnu"
;; 	       (fill-column . 80)
;; 	       (c++-indent-level . 2)
;; 	       (c-basic-offset . 2)
;; 	       (indent-tabs-mode . nil)
;; 	       (c-offsets-alist . ((arglist-intro . ++)
;; 				   (innamespace . 0)
;; 				   (member-init-intro . ++)))))

(package-require 'groovy-mode)
(defun groovy-mode-hook ()
  (setq tab-width 2
        indent-tabs-mode nil
        c-basic-offset 2)
  )
(add-hook 'groovy-mode-hook 'groovy-mode-hook)
(package-require 'dockerfile-mode)
(package-require 'docker-compose-mode)
(package-require 'graphviz-dot-mode)
(package-require 'rust-mode)

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

(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;; decode ANSI color escape sequences for .log files
(add-to-list 'auto-mode-alist '("\\.txt\\'" . display-ansi-colors))

(setq line-number-display-limit-width 2000000)

(defun my-xml-hook ()
  (setq nxml-child-indent 2)
)
(add-hook 'nxml-mode-hook 'my-xml-hook)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-start 4)
 '(ac-modes
   '(emacs-lisp-mode lisp-interaction-mode c-mode cc-mode c++-mode java-mode perl-mode cperl-mode python-mode ruby-mode ecmascript-mode javascript-mode js2-mode php-mode css-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode nxml-mode sgml-mode erlang-mode))
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes
   '("5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "df01ad8d956b9ea15ca75adbb012f99d2470f33c7b383a8be65697239086672e" "fa96a61e4eca5f339ad7f1f3442cb5a83696f6a45d9fe2a7bf3b75fc6912bb91" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default))
 '(flycheck-checker-error-threshold 1000)
 '(gdb-create-source-file-list nil)
 '(ispell-dictionary nil)
 '(package-selected-packages
   '(helm-lsp editorconfig lua-mode gist alect-themes rust-mode docker-compose-mode dockerfile-mode groovy-mode solarized-theme use-package redo+ markdown-mode llvm-mode highlight-parentheses graphviz-dot-mode ggtags erlang color-theme))
 '(safe-local-variable-values
   '((c-set-offset . 2)
     (eval when
           (fboundp 'c-toggle-comment-style)
           (c-toggle-comment-style 1))
     (eval c-set-offset 'innamespace 0)
     (c-indent-level . 2)
     (c-continued-statement-offset . 2)))
 '(whitespace-style '(face trailing lines-tail empty)))
(put 'erase-buffer 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-line ((t (:background "gray")))))
