(setq w32-apps-modifier 'super)
(custom-set-variables
 '(org-agenda-include-diary nil)
 '(org-agenda-start-on-weekday nil))

(add-hook 'org-mode-hook
	  (lambda ()
	    (variable-pitch-mode t)
	    (set-face-attribute 'org-table nil :inherit 'fixed-pitch)))

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

(global-set-key "\C-xk" 'kill-this-buffer)

(defun adam-org-sunrise () 
  (concat
   (nth 1 (split-string (diary-sunrise-sunset)))
   " Sunrise"))

(defun adam-org-sunset () 
  (concat
   (nth 4 (split-string (diary-sunrise-sunset)))
   " Sunset"))

(customize-set-variable 'tab-always-indent 'complete)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(defun count-words (&optional begin end)
  "count words between BEGIN and END (region); if no region defined, count words in buffer"
  (interactive "r")
  (let ((b (if mark-active begin (point-min)))
      (e (if mark-active end (point-max))))
    (message "Word count: %s" (how-many "\\w+" b e))))

(defadvice LaTeX-fill-region-as-paragraph (around LaTeX-sentence-filling)
  "Start each sentence on a new line."
  (let ((from (ad-get-arg 0))
        (to-marker (set-marker (make-marker) (ad-get-arg 1)))
        tmp-end)
    (while (< from (marker-position to-marker))
      (forward-sentence)
      ;; might have gone beyond to-marker --- use whichever is smaller:
      (ad-set-arg 1 (setq tmp-end (min (point) (marker-position to-marker))))
      ad-do-it
      (ad-set-arg 0 (setq from (point)))
      (unless (or
               (bolp)
               (looking-at "\\s *$"))
        (LaTeX-newline)))
    (set-marker to-marker nil)))

(ad-activate 'LaTeX-fill-region-as-paragraph)

(tool-bar-mode -1)

(customize-set-variable 'custom-enabled-themes (quote (wheatgrass)))
(customize-set-variable 'org-agenda-start-on-weekday nil)
(customize-set-variable 'org-babel-load-languages (quote ((emacs-lisp . t) (python . t))))
(customize-set-variable 'org-confirm-babel-evaluate nil)
(customize-set-variable 'org-src-fontify-natively t)

(customize-set-variable 'haskell-mode-hook (quote (turn-on-haskell-indent)))
(customize-set-variable 'ispell-dictionary nil)
(customize-set-variable 'org-agenda-include-diary nil)

(customize-set-variable
 'package-archives
 (quote
  (("gnu" . "http://elpa.gnu.org/packages/")
   ("marmalade" . "http://marmalade-repo.org/packages/")
   ("melpa" . "http://melpa.milkbox.net/packages/"))))


(setq calendar-latitude 53.3836)
(setq calendar-longitude 1.4669)

(ido-mode)


(customize-set-variable
 'gnus-select-method
 (quote
  (nnimap "personal"
	  (nnimap-address "imap.gmail.com")
	  (nnimap-server-port 993)
	  (nnimap-stream ssl))))
