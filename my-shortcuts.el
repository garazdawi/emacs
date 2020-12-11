;;;
;;; Custom Keyboard Shortcuts
;;;

;;; === Custom keybindings ===================================================

;;; --- Keybindings using existing functions ---------------------------------

(global-set-key [f5] 'compile)
(global-set-key [f6] 'next-error)
(global-set-key [S-backspace] 'shrink-whitespaces)           ; Delete white space around cursor with Shift-BACKSPACE

(global-set-key (read-kbd-macro "M-<down>") 'flymake-goto-next-error) ; Next flymake error
(global-set-key (read-kbd-macro "M-<up>")   'flymake-goto-prev-error) ; Previous flymake error

(global-set-key (read-kbd-macro "C-R")   'query-replace)          ; search and replace with Ctrl-R
(global-set-key (read-kbd-macro "C-S-R") 'query-replace-regexp)   ; search and replace (regexp) with Ctrl-Shift-R

(global-set-key (read-kbd-macro "C-/")   'comment-region)         ; Comment the region with Ctrl-/
(global-set-key (read-kbd-macro "C-?")   'uncomment-region)       ; Uncomment the region with Ctrl-?

(global-set-key [\C-tab] 'other-window)                           ; Change window
(global-set-key (read-kbd-macro "C-B")   'ido-switch-buffer)      ; Change buffer

(global-set-key (read-kbd-macro "C-.")   'erl-find-source-under-point)     ; Jump to function definition
(global-set-key (read-kbd-macro "C-,")   'erl-find-source-unwind)          ; Jump back to where we came from

(global-set-key (read-kbd-macro "C-S-L")   'goto-line)              ; Jump to line

(global-set-key (read-kbd-macro "C-Q")   'save-buffers-kill-terminal) ; Quit Emacs

(global-set-key (read-kbd-macro "C-c") 'copy-all)
(global-set-key (read-kbd-macro "C-v") 'yank)

(global-set-key (read-kbd-macro "C-a") 'move-beginning-of-line) ; move-beginning-of-line

; search forward with Ctrl-F
(global-set-key (read-kbd-macro "C-F") 'isearch-forward)
(define-key isearch-mode-map (read-kbd-macro "C-F") (lookup-key isearch-mode-map "\C-s"))
(define-key minibuffer-local-isearch-map (read-kbd-macro "C-F")
  (lookup-key minibuffer-local-isearch-map "\C-S"))

; search forward (regexp) with Ctrl-Shift-F
(global-set-key (read-kbd-macro "C-S-F") 'isearch-forward-regexp)
(define-key isearch-mode-map (read-kbd-macro "C-S-F") (lookup-key isearch-mode-map "\C-M-r"))
(define-key minibuffer-local-isearch-map (read-kbd-macro "C-S-F")
  (lookup-key minibuffer-local-isearch-map "\C-M-R"))

;;; --- Keybindings using Adam's custom functions ---------------------------------

(global-set-key (read-kbd-macro "C-=") 'duplicate-current-line)     ; Duplicate a line with Ctrl-=
(global-set-key (read-kbd-macro "C-<down>") 'move-current-line-downward) ; Switch a line downwards with Ctrl-<down>
(global-set-key (read-kbd-macro "C-<up>") 'move-current-line-upward) ; Switch a line upwards with Ctrl-<up>

(global-set-key (read-kbd-macro "S-<delete>") 'nuke-line)            ; Delete the whole line with Shift-DEL

;;; === Custom functions =====================================================

(defun duplicate-current-line ()
  "Duplicate the current line. There is a little bug: when current line is the last line of the buffer, this will not work as expected. Anyway, that's ok for me."
  (interactive)
  (let ((previous-column (current-column)))
    (let ((start (progn (beginning-of-line) (point)))
          (end (progn (next-line 1) (beginning-of-line) (point))))
      (insert-buffer-substring (current-buffer) start end)
      (forward-line -1))
    (move-to-column previous-column)))

(defun move-current-line-downward ()
  "Move current line downward once."
  (interactive)
  (let ((previous-column (current-column)))
    (forward-line)
    (transpose-lines 1)
    (forward-line -1)
    (move-to-column previous-column)))

(defun move-current-line-upward ()
  "Move current line upward once."
  (interactive)
  (let ((previous-column (current-column)))
    (transpose-lines 1)
    (forward-line -2)
    (move-to-column previous-column)))

(defun nuke-line ()
  (interactive)
  (let ((previous-column (current-column)))
	(kill-whole-line 1)
	(move-to-column previous-column)))
