(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t t)
 '(TeX-view-program-list (quote (("Okular" "okular --unique %o#src:%n%b"))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "Okular")
     (output-pdf "Evince")
     (output-html "xdg-open"))))
 '(browse-url-browser-function (quote eww-browse-url))
 '(csharp-make-tool "mcs" t)
 '(custom-safe-themes
   (quote
    ("f0d8af755039aa25cd0792ace9002ba885fd14ac8e8807388ab00ec84c9497d7" "8fed5e4b89cf69107d524c4b91b4a4c35bcf1b3563d5f306608f0c48f580fdf8" "90edd91338ebfdfcd52ecd4025f1c7f731aced4c9c49ed28cfbebb3a3654840b" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "cedd3b4295ac0a41ef48376e16b4745c25fa8e7b4f706173083f16d5792bb379" "f0a99f53cbf7b004ba0c1760aa14fd70f2eabafe4e62a2b3cf5cabae8203113b" default)))
 '(diff-switches "-u")
 '(emojify-display-style (quote unicode))
 '(flycheck-disabled-checkers (quote (haskell-stack-ghc)))
 '(flymake-python-pyflakes-executable "flake8")
 '(flymake-python-pyflakes-extra-arguments (quote (" --ignore=E302,E226")))
 '(gnus-secondary-select-methods
   (quote
    ((nnmaildir "Professional"
                (directory "~/Maildir/Professional"))
     (nnmaildir "Work"
                (directory "~/Maildir/Work"))
     (nnmaildir "Personal"
                (directory "~/Maildir/Personal")))))
 '(gnus-select-method (quote (nntp "news.gwene.org")))
 '(gnus-use-full-window nil)
 '(guide-key/recursive-key-sequence-flag t)
 '(haskell-tags-on-save t)
 '(helm-split-window-in-side-p t)
 '(holiday-other-holidays
   (quote
    ((holiday-float 5 1 -1 "Spring Bank Holiday")
     (holiday-float 5 1 1 "May Day Brank Holiday")
     (holiday-float 8 1 -1 "Late Summer Bank Holidays"))))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-dictionary nil)
 '(jabber-debug-keep-process-buffers t)
 '(jabber-debug-log-xml t)
 '(message-send-mail-function (quote message-send-mail-with-sendmail))
 '(message-sendmail-envelope-from (quote header))
 '(message-sendmail-extra-arguments (quote ("--read-envelope-from")))
 '(message-sendmail-f-is-evil t)
 '(org-agenda-day-face-function (quote jd:org-agenda-day-face-holidays-function))
 '(org-agenda-files
   (quote
    ("~/google.org" "~/agenda.org" "~/Dropbox/agenda.org")))
 '(org-agenda-include-diary nil)
 '(org-agenda-start-on-weekday nil)
 '(org-babel-load-languages (quote ((emacs-lisp . t) (python . t))))
 '(org-confirm-babel-evaluate nil)
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . system))))
 '(org-latex-listings (quote minted))
 '(org-latex-packages-alist (quote (("" "minted" nil))))
 '(org-latex-pdf-process
   (quote
    ("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))
 '(org-src-fontify-natively t)
 '(org-src-preserve-indentation t)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(package-enable-at-startup nil)
 '(paradox-automatically-star t)
 '(send-mail-function (quote smtpmail-send-it))
 '(sendmail-program "msmtp")
 '(tab-always-indent (quote complete))
 '(user-mail-address "a.washington@sheffield.ac.uk"))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
