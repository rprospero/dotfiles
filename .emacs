(package-initialize)
(eval-when-compile
  (require 'use-package))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  ;; :config
  ;; (color-theme-sanityinc-tomorrow-bright)
  )

(setq custom-file "~/dotfiles/.emacs-custom.el")
(load custom-file)

(setq w32-apps-modifier 'super)

(use-package multi-line
  :ensure t
  :bind (("C-c d" . multi-line)))

(use-package encourage-mode
  :ensure t
  :init (encourage-mode))

(use-package selectric-mode
  :ensure t)

(use-package jabber
  :ensure t
  :defer t
  :config
  (progn
   (let
    ((passwd (funcall (plist-get (car (auth-source-search :max 1 :host "talk.google.com")) :secret))))
    (customize-set-variable
     'jabber-account-list
     `(("rprospero@gmail.com"
        (:port . 5223)
        (:password . ,passwd)
        (:network-server . "talk.google.com")
        (:connection-type . ssl)))))
   (defun send-message-xmobar (msg)
          (if t
              (call-process-shell-command
               (format "echo \"%s\" > /tmp/jabber_notify" msg))))
   (defun jabber-notify-xmobar ()
          (if (equal "0" jabber-activity-count-string)
            (send-message-xmobar "")
            (send-message-xmobar (format "<fc=red,black><icon=/home/adam/dotfiles/pacman.xbm/>%s</fc>" jabber-activity-count-string))))
   (add-hook 'jabber-activity-update-hook 'jabber-notify-xmobar)))

(use-package emojify
  :ensure t
  :init
  (customize-set-variable 'emojify-display-style 'unicode) ; :-)
  (add-hook 'after-init-hook #'global-emojify-mode))

(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb))
  :config
  (progn
    (setq calendar-latitude 53.3836)
    (setq calendar-longitude 1.4669)

    (customize-set-variable 'org-agenda-include-diary nil)
    (customize-set-variable 'org-agenda-start-on-weekday nil)
    (customize-set-variable 'org-return-follows-link t)
    (add-hook 'org-mode-hook
              (lambda ()
                (variable-pitch-mode t)
                (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
                (set-face-attribute 'org-block-begin-line nil :inherit 'fixed-pitch)
                (set-face-attribute 'org-block-end-line nil :inherit 'fixed-pitch)
                (set-face-attribute 'org-verbatim nil :inherit 'fixed-pitch)))

    (defun adam-org-sunrise () 
      (concat
       (nth 1 (split-string (diary-sunrise-sunset)))
       " Sunrise"))
    (defun adam-org-sunset () 
      (concat
       (nth 4 (split-string (diary-sunrise-sunset)))
       " Sunset"))
    
    (customize-set-variable 'org-agenda-start-on-weekday nil)
    (customize-set-variable 'org-babel-load-languages (quote ((emacs-lisp . t) (python . t))))
    (customize-set-variable 'org-confirm-babel-evaluate nil)
    (customize-set-variable 'org-src-fontify-natively t)
    (customize-set-variable 'org-agenda-include-diary nil)

    (customize-set-variable
     'holiday-other-holidays 
     (quote 
      (
       (holiday-float 5 1 -1 "Spring Bank Holiday")
       (holiday-float 5 1 1 "May Day Brank Holiday")
       (holiday-float 8 1 -1 "Late Summer Bank Holidays")
       )))

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
     'org-agenda-day-face-function
     (function
      jd:org-agenda-day-face-holidays-function))
    (require 'org-notify)
    (setq org-agenda-custom-commands
          '(("c" . "My Custom Agendas")
            ("cu" "Unscheduled TODO"
             ((todo ""
                    ((org-agenda-overriding-header "\nUnscheduled TODO")
                     (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp)))))
             nil
             nil)))
    
    (add-hook 'org-mode-hook 'auto-fill-mode)
    (add-hook 'org-mode-hook 'flyspell-mode)))
  

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(customize-set-variable 'TeX-PDF-mode t)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'visual-line-mode)

(bind-key "C-x k" 'kill-this-buffer)

(customize-set-variable 'tab-always-indent 'complete)

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

(customize-set-variable 'ispell-dictionary nil)

(customize-set-variable
 'package-archives
 (quote
  (("gnu" . "http://elpa.gnu.org/packages/")
   ("marmalade" . "http://marmalade-repo.org/packages/")
   ("melpa" . "http://melpa.milkbox.net/packages/"))))
(customize-set-variable 'TeX-PDF-mode t)

(ido-mode)



(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-hook 'mail-mode-hook 'flyspell-mode)

(defun flymake-keys ()
  (local-set-key [(meta down)] 'flymake-goto-next-error)
  (local-set-key [(meta up)] 'flymake-goto-prev-error))

;;Python checking stuff
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(add-hook 'python-mode-hook 'flymake-keys)

;;Usenet stuff
;; Mail stuff
(use-package gnus
  :config
  (progn
    (customize-set-variable 'gnus-select-method '(nntp "news.gwene.org"))
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

    (defun gnus-keys ()
      (local-set-key ["S-delete"] 'gnus-summary-delete-article))

    (add-hook 'gnus-summary-mode-hook 'gnus-keys)))
    ;; (gnus-add-configuration
    ;;  '(article
    ;;    (horizontal 1.0
    ;;                (vertical 60 (group 1.0))
    ;;                (vertical 1.0
    ;;                          (summary 0.16 point)
    ;;                          (article 1.0)))))

    ;; (gnus-add-configuration
    ;;  '(summary
    ;;    (horizontal 1.0
    ;;                (vertical 60 (group 1.0))
    ;;                (vertical 1.0 (summary 1.0 point)))))))


;;csharp
(customize-set-variable
 'csharp-make-tool
 "mcs")

;; Haskell Stuff

(use-package haskell-mode
  :ensure t
  :config
  (setenv "PATH" (concat "~/.cabal/bin:" (getenv "PATH")))
  (add-to-list 'exec-path "~/.cabal/bin")
  (customize-set-variable 'haskell-tags-on-save t)

  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook
   'haskell-mode-hook
   (lambda ()
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
     (push '("::" . ?⁝) prettify-symbols-alist))))


;; Custom hot-keys

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(winner-mode)

;; Helm bindings
(use-package helm
  :bind (("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-c h" . helm-command-prefix)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("M-s SPC" . helm-swoop)
         ("C-x 8 RET" . helm-unicode)
         ("M-$" . helm-flyspell-correct))
  :config
  (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
  (bind-key "C-i" 'helm-execute-persistent-action helm-map)
  (bind-key "C-z" 'helm-select-action helm-map))


(set-fontset-font "fontset-default" nil 
                  (font-spec :size 12 :name "DejaVu Sans"))

(set-fontset-font "fontset-default" nil 
                  (font-spec :size 20 :name "DejaVu Sans"))

(customize-set-variable 'helm-split-window-in-side-p t)
(helm-mode 1)

;; Twitter Stuff
(use-package twittering-mode
             :bind (("C-c t" . twit))
             :config
             (setq twittering-use-master-password t))

(use-package rainbow-delimiters
             :ensure t
             :defer t
             :init
             (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
             :config
             (set-face-attribute 'rainbow-delimiters-depth-1-face nil
                                 :foreground (face-attribute 'default :foreground))
             (set-face-attribute 'rainbow-delimiters-depth-2-face nil
                                 :foreground (face-attribute 'outline-1 :foreground))
             (set-face-attribute 'rainbow-delimiters-depth-3-face nil
                                 :foreground (face-attribute 'outline-2 :foreground))
             (set-face-attribute 'rainbow-delimiters-depth-4-face nil
                                 :foreground (face-attribute 'outline-3 :foreground))
             (set-face-attribute 'rainbow-delimiters-depth-5-face nil
                                 :foreground (face-attribute 'outline-4 :foreground))
             (set-face-attribute 'rainbow-delimiters-depth-6-face nil
                                 :foreground (face-attribute 'outline-5 :foreground))
             (set-face-attribute 'rainbow-delimiters-depth-7-face nil
                                 :foreground (face-attribute 'outline-6 :foreground))
             (set-face-attribute 'rainbow-delimiters-depth-8-face nil
                                 :foreground (face-attribute 'outline-7 :foreground))
             (set-face-attribute 'rainbow-delimiters-depth-9-face nil
                                 :foreground (face-attribute 'outline-8 :foreground)))

(use-package company
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  :diminish company-mode)

(use-package god-mode
  :bind (("<Scroll_Lock>" . god-mode-all))
  :config
  (add-hook 'god-mode-enabled-hook 'my-update-cursor)
  (add-hook 'god-mode-disabled-hook 'my-update-cursor)
  (add-to-list 'god-exempt-major-modes 'magit-mode)
  (add-to-list 'god-exempt-major-modes 'Group)
  (add-to-list 'god-exempt-major-modes 'Messages)
  (define-minor-mode mortal-mode
    "Allow temporary departures from god-mode."
    :lighter " mortal"
    :keymap '(([return] . (lambda ()
                            "Exit mortal-mode and resume god mode." (interactive)
                            (god-local-mode-resume)
                            (mortal-mode 0))))
    (when mortal-mode
      (god-local-mode-pause)))

  (define-key god-local-mode-map (kbd "I") 'mortal-mode))

(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))
(setq cursor-type 'bar)

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

(use-package guide-key
  :ensure t
  :diminish guide-key-mode
  :config
  (customize-set-variable 'guide-key/recursive-key-sequence-flag t)
  (setq guide-key/guide-key-sequence '("C-x 4" "C-x r" "C-x a" "C-x RET" "C-x ." "C-x @" "C-x v" "M-g" "C-c" "M-s"))
  (setq guide-key-tip/enabled))
(guide-key-mode 1)  ; Enable guide-key-mode

(which-function-mode 't)
(set-face-foreground 'which-func (face-foreground font-lock-variable-name-face))

(use-package smart-mode-line
  :ensure t
  :init
  (smart-mode-line-enable)
  :config
  (sml/apply-theme 'respectful))

(add-hook 'prog-mode-hook 'hs-minor-mode)

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

(use-package unbound
  :ensure t)

(customize-save-variable
 'indent-tabs-mode
 nil)

(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode
  :init
  (global-whitespace-cleanup-mode))


(global-prettify-symbols-mode t)

(bind-key "C-z" 'shell)

(setq inhibit-startup-screen t)

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package zone-select
  :ensure t)

(use-package zone-rainbow
  :config
  (zone-select-add-program 'zone-pgm-rainbow))

(use-package zone-sl
  :config
  (zone-select-add-program 'zone-pgm-sl))

(use-package flycheck
  :config
  (flycheck-define-checker
   proselint
   "A linter for plain prose"
   :command ("proselint" source)
   :standard-input f
   :error-patterns
   ((warning line-start (file-name) ":" line ":" column ": " (message) line-end))
   :modes (markdown-mode text-mode org-mode)))

(use-package ace-isearch
  :config
  (global-ace-isearch-mode +1))

(use-package ace-window
  :ensure t
  :bind
  (("M-z" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package ledger-mode
  :ensure t)

;;nnreddit stuff.  Should eventually be turned into a package
(add-to-list 'load-path "~/Code/nnreddit")
(require 'nnreddit)
;; (add-to-list 'gnus-secondary-select-methods
;;              '(nnreddit ""))

(use-package ivy
  :ensure t)

(use-package counsel
  :bind   (("C-s" . swiper)
           ("C-c C-r" . ivy-resume)
           ("<f6>" . ivy-resume)
           ("M-x" . counsel-M-x)
           ("C-x C-f" . counsel-find-file)
           ("<f1> f" . counsel-describe-function)
           ("<f1> v" . counsel-describe-variable)
           ("<f1> l" . counsel-load-library)
           ("<f2> i" . counsel-info-lookup-symbol)
           ("<f2> u" . counsel-unicode-char)
           ("C-c g" . counsel-git)
           ("C-c j" . counsel-git-grep)
           ("C-c k" . counsel-ag)
           ("C-x l" . counsel-locate)
           ("C-S-o" . counsel-rhythmbox))
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t))
