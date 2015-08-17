(package-initialize)

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

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'visual-line-mode)

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
(scroll-bar-mode -1)
(menu-bar-mode -1)

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
(customize-set-variable 'TeX-PDF-mode t)
(customize-set-variable 'haskell-mode-hook (quote (turn-on-haskell-indent)))


(setq calendar-latitude 53.3836)
(setq calendar-longitude 1.4669)

(ido-mode)



(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-hook 'mail-mode-hook 'flyspell-mode)


;;Python checking stuff
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

;;Usenet stuff
(customize-set-variable 'gnus-select-method '(nntp "news.gwene.org"))

;; Mail stuff
(customize-set-variable
 'gnus-secondary-select-methods
 (quote
  ((nnmaildir "Professional"
	      (directory "~/Maildir/Professional"))
   (nnmaildir "Work"
	      (directory "~/Maildir/Work"))
   (nnmaildir "Personal"
	      (directory "~/Maildir/Personal")))))

(customize-set-variable
 'send-mail-function
 (quote smtpmail-send-it))
(customize-set-variable
 'sendmail-program
 "msmtp")
(customize-set-variable
 'message-send-mail-function
 (quote message-send-mail-with-sendmail))
(customize-set-variable
 'message-sendmail-envelope-from
 (quote header))
(customize-set-variable
 'message-sendmail-extra-arguments
 (quote ("--read-envelope-from")))
(customize-set-variable
 'message-sendmail-f-is-evil
 t)


;;Browser stuff
(customize-set-variable 'browse-url-browser-function 'eww-browse-url)

(customize-set-variable
 'jabber-account-list
 '(("rprospero@gmail.com" 
    (:network-server . "talk.google.com")
    (:connection-type . ssl))))

;;csharp
(customize-set-variable
 'csharp-make-tool
 "mcs")

;; Haskell Stuff

(setenv "PATH" (concat "~/.cabal/bin:" (getenv "PATH")))
(add-to-list 'exec-path "~/.cabal/bin")
(custom-set-variables '(haskell-tags-on-save t))

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
;(require 'flymake-haskell-multi)
(add-hook 'haskell-mode-hook 'flymake-haskell-multi-load)


;; Custom hot-keys


(global-set-key (kbd "C-x g") 'magit-status)

;; Enable windmove in orgmode
(windmove-default-keybindings)
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; Helm bindings
;(require 'helm-config)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-s SPC") 'helm-swoop)
(global-set-key (kbd "C-x 8 RET") 'helm-unicode)
(global-set-key (kbd "M-$") 'helm-flyspell-correct)

(set-fontset-font "fontset-default" nil 
                  (font-spec :size 20 :name "DejaVu Sans"))

(customize-set-variable 'helm-split-window-in-side-p t)
(helm-mode 1)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z


;; Twitter Stuff
(require 'twittering-mode)
(setq twittering-use-master-password t)
(twit)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'company-mode)

(require 'god-mode)
(global-set-key (kbd "<Scroll_Lock>") 'god-local-mode)

(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))
(setq cursor-type 'bar)

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

(add-to-list 'god-exempt-major-modes 'magit-mode)
(defun kdialog-popup (title msg)
  "Show a popup if we're on X, or echo it otherwise; TITLE is the title
of the message, MSG is the context.

Code stolen from: http://emacs-fu.blogspot.co.uk/2009/11/showing-pop-ups.html
"

  (interactive)
  (if 
      (eq window-system 'x)
      (shell-command
       (concat "kdialog --title \"" title 
	       "\" --passivepopup \""  msg
	       "\""))
    (message (concat title ": " msg))))

(defun kdialog-appt-display (min-to-appt new-time msg)
  (kdialog-popup (format "Appointment in %s minute(s)" min-to-appt) msg))
(setq appt-disp-window-function (function kdialog-appt-display))

(customize-set-variable
 'org-agenda-day-face-function
 (function
  jd:org-agenda-day-face-holidays-function))

;;http://lists.gnu.org/archive/html/emacs-orgmode/2010-11/msg00542.html
(defun my-org-agenda-day-face-holidays-function (date)
  "Compute DATE face for holidays."
  (unless (org-agenda-todayp date)
    (dolist (file (org-agenda-files nil 'ifmode))
      (let ((face
	     (dolist (entry (org-agenda-get-day-entries file date))
	       (let ((category (with-temp-buffer
				 (insert entry)
				 (org-get-category (point-min)))))
		 (when (or (string= "Holidays" category)
			   (string= "Vacation" category))
		   (return 'org-agenda-date-weekend))))))
	(when face (return face))))))

(customize-set-variable
 'holiday-other-holidays 
 (quote 
  (
   (holiday-float 5 1 -1 "Spring Bank Holiday")
   (holiday-float 5 1 1 "May Day Brank Holiday")
   (holiday-float 8 1 -1 "Late Summer Bank Holidays")
   )))

(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x 4" "C-x r" "C-x a" "C-x RET" "C-x ." "C-x @" "C-x v" "M-g" "C-c" "M-s"))
;(setq guide-key/guide-key-sequence t)
(setq guide-key-tip/enabled)
(guide-key-mode 1)  ; Enable guide-key-mode

(which-function-mode 't)
(smart-mode-line-enable)
(set-face-foreground 'which-func (face-foreground font-lock-variable-name-face))
(sml/apply-theme 'respectful)


(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (push '("<=" . ?≤) prettify-symbols-alist)
	    (push '("**2" . ?²) prettify-symbols-alist)))

(add-hook 'python-mode-hook
	  (lambda ()
	    (push '("<=" . ?≤) prettify-symbols-alist)
	    (push '(">=" . ?≥) prettify-symbols-alist)
	    (push '("!=" . ?≠) prettify-symbols-alist)
	    (push '("np.pi" . ?π) prettify-symbols-alist)
	    (push '("np.sum" . ?Σ) prettify-symbols-alist)
	    (push '("np.sqrt" . ?√) prettify-symbols-alist)
	    (push '("sqrt" . ?√) prettify-symbols-alist)
	    (push '("sum" . ?Σ) prettify-symbols-alist)
	    (push '("alpha" . ?α) prettify-symbols-alist)
	    (push '("sigma" . ?σ) prettify-symbols-alist)
	    (push '("lambda" . ?λ) prettify-symbols-alist)
	    (push '("**2" . ?²) prettify-symbols-alist)))

(add-hook
 'haskell-mode-hook
 (lambda ()
   (turn-on-haskell-indent)
   (push '("\\" . ?λ) prettify-symbols-alist)
   (push '("->" . ?→) prettify-symbols-alist)
   (push '("<-" . ?←) prettify-symbols-alist)
   (push '("=>" . ?⇒) prettify-symbols-alist)
   (push '("not" . ?¬) prettify-symbols-alist)
   (push '("==" . ?≟) prettify-symbols-alist)
   (push '("/=" . ?≠) prettify-symbols-alist)
   (push '("<=" . ?≤) prettify-symbols-alist)
   (push '(">=" . ?≥) prettify-symbols-alist)
   (push '("=" . ?≡) prettify-symbols-alist)
   (push '("pi" . ?π) prettify-symbols-alist)
   (push '(">>" . ?≫) prettify-symbols-alist)
   (push '("<<" . ?≪) prettify-symbols-alist)
   (push '("++" . ?⧺) prettify-symbols-alist)
   (push '("*" . ?⋅) prettify-symbols-alist)
   (push '(" . " . ?∘) prettify-symbols-alist)
   (push '("<*>" . ?⊛) prettify-symbols-alist)
   (push '("<+>" . ?⊕) prettify-symbols-alist)
   (push '("::" . ?⁝) prettify-symbols-alist)))

(require 'unbound)

(customize-save-variable
 'indent-tabs-mode
 nil)

(global-whitespace-cleanup-mode)
(global-prettify-symbols-mode t)

(global-set-key "\C-z" 'shell)
