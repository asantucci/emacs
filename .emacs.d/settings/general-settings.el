;--------------------------------;
;;; General or Global Settings ;;;
;--------------------------------;

; set PATH, because we don't load .bashrc
; function from https://gist.github.com/jakemcc/3887459
(defun set-exec-path-from-shell-PATH ()
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo -n $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
 
(if window-system (set-exec-path-from-shell-PATH))

; language
(setq current-language-environment "English")

(setq inhibit-splash-screen t)         ; turn off splash screen
(setq inhibit-startup-screen 1)        ; don't show the startup screen
(menu-bar-mode 0)                      ; don't show the menu bar
(require 'tool-bar)                    ; don't show the tool bar
(tool-bar-mode 0)
(if window-system (scroll-bar-mode 0)) ; don't show the scroll bar

(setq confirm-kill-emacs 'yes-or-no-p) ; Confirm closing emacs
(defalias 'yes-or-no-p 'y-or-n-p)      ; Use y and n rather than yes and no

;; default to better frame titles
(setq frame-title-format
      (concat  "%b -- Emacs"))

; turn on mouse wheel support for scrolling
(require 'mwheel)
(mouse-wheel-mode 1)

; set command key to be meta instead of option
(if (system-is-mac)
    (setq ns-command-modifier 'meta))

; number of characters until the fill column 
(setq-default fill-column 80)

;; Use line wrapping
(load "longlines") ;; best not to include the ending ¡°.el¡± or ¡°.elc¡±
(add-hook 'text-mode-hook 'longlines-mode 1)

; each line of text gets one line on the screen (i.e., text will run
; off the left instead of wrapping around onto a new line)
;; (setq-default truncate-lines 1)
;; ; truncate lines even in partial-width windows
;; (setq truncate-partial-width-windows 1)

; default window width and height
(defun custom-set-frame-size ()
  (add-to-list 'default-frame-alist '(height . 50))
  (add-to-list 'default-frame-alist '(width . 178)))
(custom-set-frame-size)
(add-hook 'before-make-frame-hook 'custom-set-frame-size)

; window modifications
;; (global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
;; (global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
;; (global-set-key (kbd "S-C-<down>") 'shrink-window)
;; (global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Use iswitch buffer mode
(iswitchb-mode 1)
(global-set-key "\C-x\C-b" 'iswitchb-buffer-other-window)

; make end and home keys go to the start/end of buffers
(global-set-key (kbd "<end>") 'end-of-buffer)
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(define-key input-decode-map "\e[1;5A" [C-up])
(define-key input-decode-map "\e[1;5B" [C-down])

; always use spaces, not tabs, when indenting
(setq-default indent-tabs-mode nil)
; indentation styles
(setq c-basic-offset 8)
(setq c-default-style (quote (
    (c-mode . "bsd") 
    (java-mode . "java") 
    (awk-mode . "awk") 
    (other . "gnu"))))

(setq default-tab-width 4)  ;; Set tab width
(setq scroll-step 1)        ;; Set scroll step
(global-font-lock-mode t)   ;; Use font lock
(setq font-lock-maximum-decoration t) ;; To the max!

; ignore case when searching
(setq-default case-fold-search 1)

; set the keybinding so that you can use f4 for goto line
(global-set-key [f4] 'goto-line)

; require final newlines in files when they are saved
(setq require-final-newline 1)
; add a new line when going to the next line
(setq next-line-add-newlines t)

; show the current line and column numbers in the stats bar as well
(line-number-mode 1)
(column-number-mode 1)

; don't blink the cursor
;; (blink-cursor-mode 0)

; make sure transient mark mode is enabled (it should be by default,
; but just in case)
(transient-mark-mode 1)

; highlight parentheses when the cursor is next to them
(require 'paren)
(show-paren-mode 1)

; text decoration
(require 'font-lock)
;(setq font-lock-maximum-decoration 1)
(global-font-lock-mode 1)
(global-hi-lock-mode nil)
(setq jit-lock-contextually 1)
(setq jit-lock-stealth-verbose 1)

; if there is size information associated with text, change the text
; size to reflect it
(size-indication-mode 1)

; disable backup
(setq backup-inhibited t)
; disable auto save
(setq auto-save-default nil)

(provide 'general-settings)















