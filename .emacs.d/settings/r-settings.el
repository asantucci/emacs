;;; r-settings.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Andreas Santucci

;; Author: Andreas Santucci <asantucci@asantucci-mbp>
;; Keywords: a

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Charlie's Set up.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;------------------------------------------------
;; General stuff
;;------------------------------------------------

;;(setq x-select-enable-clipboard t)

(setq fill-column 80)       ;; Set autofill width
(setq default-tab-width 5)  ;; Set tab width
(setq scroll-step 1)        ;; Set scroll step
(global-font-lock-mode t)   ;; Use font lock
(setq font-lock-maximum-decoration t) ;; To the max!
(column-number-mode t)      ;; Show column number
(setq make-backup-files nil);; Do not make backup files
(tool-bar-mode 0)           ;; Get rid of toolbar
(menu-bar-mode 0)           ;; Get rid of  menu bar
(setq inhibit-splash-screen t) ;; Turn off splash screen
(setq confirm-kill-emacs 'yes-or-no-p) ;; Confirm closing emacs
(defalias 'yes-or-no-p 'y-or-n-p) ;; Use y and n rather than yes and no

;; Use auto-updating for document viewing (e.g., PDF aside LaTeX code)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b -- Emacs"))

;; Use Alt-F12 to get list of recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 50)
(setq recentf-max-menu-items 20)
(global-set-key [(control f12)] 'recentf-open-files)

;; Make using the buffer list more convenient by moving to list automatically
;(global-set-key (kbd "\C-x\C-b") 'buffer-menu-other-window)
; Replace a region by typing over it and kill by DEL
(delete-selection-mode 1)

;; Change region highlighting
; Make sure that parentheses are highlighted and set color
(show-paren-mode 1)
(require 'paren)
(set-face-foreground 'show-paren-match-face "yellow")
(set-face-background 'show-paren-match-face "firebrick4")
(set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)

;; Change tooltip font
;;(custom-set-faces
;; '(tooltip ((t (:background "white" :foreground "blue" :font "Courier New")))))

;; Tell emacs where is your personal elisp lib dir
;;(add-to-list 'load-path "C:/Users/Andreas/.emacs/")
;;(add-to-list 'load-path "C:/Program Files/emacs/GNU Emacs 24.1/lisp")

;; Use line wrapping
(load "longlines") ;; best not to include the ending ¡°.el¡± or ¡°.elc¡±
(add-hook 'LaTeX-mode-hook 'longlines-auto-wrap 1)
(add-hook 'text-mode-hook 'longlines-mode 1)

:;; Change region highlighting
;;(set-face-background 'region "CornflowerBlue")

;; Use icicles completion
;;(add-to-list 'load-path "C:/Program Files/emacs/GNU Emacs 24.1/Icicles")
;;(require 'icicles)
;;(icy-mode 1)

;; Use iswitch buffer mode
(iswitchb-mode 1)
(global-set-key "\C-x\C-b" 'iswitchb-buffer-other-window)

;;------------------------------------------------
;; Killing
;;------------------------------------------------

;; Define the function to kill the characters from the cursor
;; to the beginning of the current line
(defun backward-kill-line (arg)
  "Kill chars backward until encountering the end of a line."
  (interactive "p")
  (kill-line 0))
(global-set-key "\C-d" 'backward-kill-line)

;; Set keys for word kill to be like line kill
(global-set-key "\M-d" 'backward-kill-word)
(global-set-key "\M-k" 'kill-word)
(global-set-key [M-backspace] 'kill-whole-line)
(global-set-key (kbd "<delete>") 'delete-char)

;;------------------------------------------------
;; LaTeX stuff
;;------------------------------------------------

;; Start in PDF LaTeX mode
(add-hook 'LaTeX-mode-hook 'tex-pdf-mode 1)

;; Use Evince for PDF viewing from LaTeX
(defun pdfevince ()
   (add-to-list 'TeX-output-view-style
     (quote ("^pdf$" "." "evince %o %(outpage)")))
)
(add-hook  'LaTeX-mode-hook  'pdfevince  t) ; AUCTeX LaTeX mode

;; Use Flyspell in LaTeX mode
(add-hook 'TeX-mode-hook 'flyspell-mode t)
(add-hook 'LaTeX-mode-hook 'flyspell-mode t)
(add-hook 'text-mode-hook 'flyspell-mode)

;; Setup makefile key binding
(global-set-key (kbd "C-c m")
                      (lambda ()
                        (interactive)
				    (save-buffer)
                        (shell-command (concat "make"))))

;; Set up a latex file skeleton.
(define-skeleton latex
  "LaTeX Header File"
  nil
  "\\documentclass[12pt]{article}" \n
  \n
  "\\usepackage{amsmath}" \n
  "\\usepackage{amsthm}"  \n
  "\\usepackage{amsfonts}" \n
  "\\usepackage{color,soul}" \n
  "\\usepackage{framed}" \n
  "\\usepackage{hyperref}" \n
  \n
  "\\newtheorem{theorem}{Theorem}[section]" \n
  "\\newtheorem{lemma}[theorem]{Lemma}" \n
  "\\newtheorem{proposition}[theorem]{Proposition}" \n
  "\\newtheorem{corollary}[theorem]{Corollary}" \n
  \n
  "\\begin{document}" \n
  \n
  "\\end{document}")

(require 'autoinsert)
(auto-insert-mode)
(setq auto-insert-query nil)  ;; Automatically insert header without prompting.
(define-auto-insert "\\.tex\\'" 'latex)  ;; Insert for files ending in ".tex"

;;------------------------------------------------
;; ESS stuff
;;------------------------------------------------

;; Set path to R
;;(setq inferior-R-program-name "C:/Program Files/R/R-3.0.2/bin/x64/R.exe")

;;R stuff
(setq ess-eval-visibly-p nil)
(setq ess-ask-for-ess-directory nil)
;;(require 'ess-eldoc)

;;(load "C:/Program Files/emacs/GNU Emacs 24.1/site-lisp/ess/ess-site")
;;(load "C:/Program Files/emacs/GNU Emacs 24.1/lisp/ess-R-object-tooltip")

;;(define-key comint-mode-map [C-up] 'comint-previous-matching-input-from-input)
;;(define-key comint-mode-map [C-down] 'comint-next-matching-input-from-input)
;;(define-key inferior-ess-mode-map "\C-c\C-g" 'ess-R-object-tooltip)

(setq ess-local-process-name "R")
(setq ansi-color-for-comint-mode 'filter)
;(setq comint-prompt-read-only t)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)
(defun my-ess-start-R ()
  (interactive)
  (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
    (progn
     (delete-other-windows)
	(setq w1 (selected-window))
	(setq w1name (buffer-name))
	(setq w2 (split-window w1))
	(R)
	(set-window-buffer w2 "*R*")
	(set-window-buffer w1 w1name))))
  (defun my-ess-eval ()
    (interactive)
    (my-ess-start-R)
    (if (and transient-mark-mode mark-active)
	(call-interactively 'ess-eval-region)
      (call-interactively 'ess-eval-line-and-step)))
  (add-hook 'ess-mode-hook
	    '(lambda()
	       (local-set-key [(shift return)] 'my-ess-eval)))
;  (add-hook 'inferior-ess-mode-hook
;	    '(lambda()
;	       (local-set-key [C-up] 'comint-previous-input)
;	       (local-set-key [C-down] 'comint-next-input)))


;; Define the keybinding you want
(defun my-inferior-ess-mode-hook ()
  (local-set-key (kbd "<home>") 'comint-bol)
  (local-set-key "\C-d" 'comint-kill-input)
;  (local-set-key "\C-c \C-l" 'ess-eval-line-and-step)
;  (local-set-key "\C-d \M-l" 'ess-eval-line-and-go)
)

;; add the key-binding to the hook that gets called whenever you start an R session:
(add-hook 'inferior-ess-mode-hook 'my-inferior-ess-mode-hook)

;; Lastly, load time---shows that full .emacs was loaded
(setq display-time-day-and-date t) ;; Show date with time

(custom-set-variables
;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
 '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "Evince") (output-html "xdg-open")))))

;;(setq-default ispell-program-name "C:/Program Files (x86)/Aspell/bin/aspell.exe")

;; add auto complete
;;(add-to-list 'load-path "~/.emacs.d/")
;;(require 'auto-complete-config)
;;(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
;;(ac-config-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Charlies Stuff
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; New colors
;; Make sure that parentheses are highlighted and set color
;;(show-paren-mode 1)
;;(require 'paren)
;;(set-face-foreground 'show-paren-match-face "white")
;;(set-face-background 'show-paren-match-face "blue1")
;;(set-face-foreground 'show-paren-mismatch-face "white")
;;(set-face-background 'show-paren-mismatch-face "red3")
;;(set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)

;; Change region highlighting
;;(set-face-background 'region "SpringGreen")

;; More new colors, but this needs to go after you load ‘ess-site
;;(set-face-foreground 'font-lock-comment-face       "CornflowerBlue")
;;(set-face-foreground 'font-lock-string-face         "SeaGreen")
;;(set-face-foreground 'font-lock-type-face      "DarkGoldenrod")

;; Set header text
(require 'tempo)
(setq tempo-interactive t)

(tempo-define-template "R-header-1"
  '("################################################################################" n>
;;    "######################    PRIVILEGED AND CONFIDENTIAL     ######################" n>
;;    "###################### PREPARED AT THE REQUEST OF COUNSEL ######################" n>
    "################################################################################" n>
    "################################################################################" n>
    "###" n>
    "### Title: " (p "Title: " title) n>
    "###" n>
    "### Andreas Santucci" n>
;;    "### The Brattle Group" n>
    "###" n>
    "### Date: " (p "Date: " date) n>
    "###" n>
    "### Inputs: " n>
    "###" n>
    "### Dependencies: " n>
    "###" n>
    "################################################################################" n>
    "################################################################################" n> n>)
  nil
  "Insert R script header")

(tempo-define-template "R-header-2"
  '("##################################################" n>
    "###" n>
    "### " (p "Section: " title) n>
    "###" n>
    "##################################################" n> n>)
  nil
  "Insert R section header")

(tempo-define-template "R-header-3"
  '("##############################" n>
    "### " (p "Subsection: " title) n>
    "##############################" n> n>)
  nil
  "Insert R subsection header")

;; Set header key commands
(defun my-ess-mode-hook ()
  (local-set-key (kbd "C-h 1") 'tempo-template-R-header-1)
  (local-set-key (kbd "C-h 2") 'tempo-template-R-header-2)
  (local-set-key (kbd "C-h 3") 'tempo-template-R-header-3)
)

(add-hook 'ess-mode-hook 'my-ess-mode-hook)

;;Add autocomplete to R
;;(set-face-attribute 'ac-candidate-face nil   :background "LightSteelBlue1" :foreground "black")
;;(set-face-attribute 'ac-selection-face nil   :background "LightSteelBlue3" :foreground "black")
;;(set-face-attribute 'popup-tip-face    nil   :background "SlateGray2" :foreground "black")

(setq
      ac-use-comphist t
      ;; ac-auto-show-menu 1
      ;; ac-candidate-limit nil
      ac-delay 0.75
      ;; ac-disable-faces (quote (font-lock-comment-face font-lock-doc-face))
      ;; ac-ignore-case 'smart
      ;; ac-menu-height 10
      ;; ac-quick-help-delay 1.25
      ;; ac-quick-help-prefer-pos-tip t
      ac-use-quick-help nil
)

(provide 'r-settings)
