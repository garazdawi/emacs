

;;; Color theme based on Tango Palette. Created by eproxus@gmail.com
(defun color-theme-tango-3 ()
  "A color theme based on Tango Palette."
  (interactive)
  (color-theme-install
   '(color-theme-tango-3
     ((background-color . "#2e3436")
      (background-mode . dark)
      (border-color . "#888a85")
      (cursor-color . "#fce94f")
      (foreground-color . "#d3d7cf")
      (mouse-color . "#8ae234"))
     ((help-highlight-face . underline)
      (ibuffer-dired-buffer-face . font-lock-function-name-face)
      (ibuffer-help-buffer-face . font-lock-comment-face)
      (ibuffer-hidden-buffer-face . font-lock-warning-face)
      (ibuffer-occur-match-face . font-lock-warning-face)
      (ibuffer-read-only-buffer-face . font-lock-type-face)
      (ibuffer-special-buffer-face . font-lock-keyword-face)
      (ibuffer-title-face . font-lock-type-face))

     (font-lock-builtin-face ((t (:foreground "#729fcf")))) ; guards, BIFs
     (font-lock-comment-face ((t (:foreground "#888a85")))) ; comments
     (font-lock-constant-face ((t (:foreground "#babdb6")))) ; atoms
     (font-lock-doc-face ((t (:foreground "#babdb6"))))
     (font-lock-keyword-face ((t (:foreground "#3465a4" :bold t)))) ; keywords, list comprehension operators, strings quoted with `' in comments
     (font-lock-string-face ((t (:foreground "#c29fbe")))) ; strings, $-notation for ASCII values, e.g. $0
     (font-lock-type-face ((t (:foreground "#eeeeec")))) ; function calls (internal and external)
     (font-lock-variable-name-face ((t (:foreground "#c4a000")))) ; variables
     (font-lock-warning-face ((t (:foreground "#d3d7cf")))) ; the '++' operator
     (font-lock-function-name-face ((t (:foreground "#729fcf" :underline t)))) ; function definitions, F/N notation
     (font-lock-preprocessor-face ((t (:foreground "#73d216"))))  ; attributes, macros, records

     (border ((t (:background "#888a85"))))
     (fringe ((t (:background "grey10"))))
     (mode-line ((t (:foreground "#eeeeec" :background "#555753"))))
     (region ((t (:background "#204a87"))))
     (comint-highlight-input ((t (:italic t :bold t))))
     (comint-highlight-prompt ((t (:foreground "#8ae234"))))
     (isearch ((t (:background "#f57900" :foreground "#2e3436"))))
     (isearch-lazy-highlight-face ((t (:foreground "#2e3436" :background "#e9b96e"))))
     (show-paren-match-face ((t (:background "#555753"))))
     (show-paren-mismatch-face ((t (:background "#a40000"))))
     (paren-face-match ((t (:bold t :foreground "#fce94f"))))
     (paren-face-mismatch ((t (:bold t :foreground "#fce94f" :background "#a40000"))))
     (minibuffer-prompt ((t (:foreground "#729fcf" :bold t))))
     (info-xref ((t (:foreground "#729fcf"))))
     (info-xref-visited ((t (:foreground "#ad7fa8"))))
     )))

(provide 'color-theme-tango-3)
