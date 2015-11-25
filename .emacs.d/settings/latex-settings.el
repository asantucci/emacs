
;-----------;
;;; LaTeX ;;;
;-----------;

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(include-plugin "auctex")
(load "auctex.el" -1 1 1)
(load "preview-latex.el" -1 1 1)

;; (add-hook 'LaTeX-mode-hook
;; 	  (lambda ()
;; 	    (tex-pdf-mode 1)
;;          (TeX-source-correlate-mode 1)))

;; ;; Use auto-updating for document viewing (e.g., PDF aside LaTeX code)
;; (add-hook 'doc-view-mode-hook 'auto-revert-mode)


(if (system-is-mac)
    (progn
      (require 'tex-site)
      (require 'font-latex)
      (setq TeX-view-program-list
	    (quote 
	     (("Skim" 
	       (concat "/Applications/Skim.app/Contents/SharedSupport/displayline"
		       " %n %o %b")))))
      (setq TeX-view-program-selection 
	    (quote (((output-dvi style-pstricks) "dvips and gv") 
		    (output-dvi "xdvi") 
		    (output-pdf "Skim")
		    (output-html "xdg-open")))))

  (if (system-is-linux)
      (setq TeX-view-program-selection 
	     (quote (((output-dvi style-pstricks) "dvips and gv") 
		     (output-dvi "xdvi")
		     (output-pdf "evince")
		     (output-html "xdg-open"))))))

;; Use Evince for PDF viewing from LaTeX
;; (defun pdfevince ()
;;    (add-to-list 'TeX-output-view-style
;;      (quote ("^pdf$" "." "evince %o %(outpage)"))))
;; (add-hook  'LaTeX-mode-hook  'pdfevince  t) ; AUCTeX LaTeX mode

(eval-after-load "tex"
  '(progn
     (setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))
     ;(add-to-list 'TeX-command-list '("View" "evince %g" TeX-run-command nil t :help "Run evince on your document"))
     (setq TeX-view-program-selection '((output-pdf "Evince")))))


; always start the server for inverse search
(setq-default TeX-source-correlate-start-server 0)

;; Use Flyspell in LaTeX mode
(add-hook 'TeX-mode-hook 'flyspell-mode t)
(add-hook 'LaTeX-mode-hook 'flyspell-mode t)
(add-hook 'text-mode-hook 'flyspell-mode)

;; Use line wrapping
(add-hook 'LaTeX-mode-hook 'longlines-auto-wrap 1)

;; Set up auto insert
(require 'autoinsert)
(auto-insert-mode)
(setq auto-insert-query nil)         ;; Automatically insert header without prompting.

;; Set up a latex file skeleton.
(define-skeleton latex-skeleton
  "LaTeX Header File"
  nil
  "\\documentclass[12pt]{article}" \n
  \n
  "\\usepackage{amsmath}"     \n
  "\\usepackage{amsthm}"      \n
  "\\usepackage{amsfonts}"    \n
  "\\usepackage{color,soul}"  \n
  "\\usepackage{framed}"      \n
  "\\usepackage{hyperref}"    \n
  \n
  "\\newtheorem{theorem}{Theorem}[section]"         \n
  "\\newtheorem{lemma}[theorem]{Lemma}"             \n
  "\\newtheorem{proposition}[theorem]{Proposition}" \n
  "\\newtheorem{corollary}[theorem]{Corollary}"     \n
  \n
  "\\begin{document}" \n
  \n
  "\\end{document}")

(define-auto-insert "\\.\\(rnw\\|tex\\)\\'" 'latex-skeleton)  ;; Insert for files ending in ".tex" and ".rnw"

(provide 'latex-settings)


;;------------------------------------------------
;; Old
;;------------------------------------------------

;; Start in PDF LaTeX mode
;; (add-hook 'LaTeX-mode-hook 'tex-pdf-mode 1)



;; Setup makefile key binding
;; (global-set-key (kbd "C-c m")
;;                       (lambda ()
;;                         (interactive)
;; 				    (save-buffer)
;;                         (shell-command (concat "make"))))
