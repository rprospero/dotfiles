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
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#3C3836" "#FB4934" "#B8BB26" "#FABD2F" "#83A598" "#D3869B" "#8EC07C" "#EBDBB2"])
 '(browse-url-browser-function (quote browse-url-firefox))
 '(column-number-mode t)
 '(company-dabbrev-code-modes
   (quote
    (prog-mode batch-file-mode csharp-mode css-mode erlang-mode haskell-mode jde-mode lua-mode python-mode purescript-mode)))
 '(compilation-message-face (quote default))
 '(counsel-find-file-at-point t)
 '(counsel-mode t)
 '(csharp-make-tool "mcs" t)
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("a800120841da457aa2f86b98fb9fd8df8ba682cebde033d7dbf8077c1b7d677a" "b0ab5c9172ea02fba36b974bbd93bc26e9d26f379c9a29b84903c666a5fde837" "71ecffba18621354a1be303687f33b84788e13f40141580fa81e7840752d31bf" "e56ee322c8907feab796a1fb808ceadaab5caba5494a50ee83a13091d5b1a10c" "c567c85efdb584afa78a1e45a6ca475f5b55f642dfcd6277050043a568d1ac6f" "345f8f92edc3508574c61850b98a2e0a7a3f5ba3bb9ed03a50f6e41546fe2de0" "e8a976fbc7710b60b069f27f5b2f1e216ec8d228fe5091f677717d6375d2669f" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "e97dbbb2b1c42b8588e16523824bc0cb3a21b91eefd6502879cf5baa1fa32e10" "cc60d17db31a53adf93ec6fad5a9cfff6e177664994a52346f81f62840fe8e23" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "c86f868347919095aa44d2a6129dd714cbcf8feaa88ba954f636295b14ceff8f" "f0d8af755039aa25cd0792ace9002ba885fd14ac8e8807388ab00ec84c9497d7" "8fed5e4b89cf69107d524c4b91b4a4c35bcf1b3563d5f306608f0c48f580fdf8" "90edd91338ebfdfcd52ecd4025f1c7f731aced4c9c49ed28cfbebb3a3654840b" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "cedd3b4295ac0a41ef48376e16b4745c25fa8e7b4f706173083f16d5792bb379" "f0a99f53cbf7b004ba0c1760aa14fd70f2eabafe4e62a2b3cf5cabae8203113b" default)))
 '(default-input-method "TeX")
 '(diff-switches "-u")
 '(dired-dwim-target t)
 '(display-buffer-alist
   (quote
    (("agenda.org"
      (display-buffer-reuse-window display-buffer-use-some-window)
      (nil)))))
 '(elfeed-feeds
   (quote
    ("http://www.xkcd.org/atom.xml" "https://www.reddit.com/r/emacs.rss" "https://www.reddit.com/r/haskell.rss" "http://www.merriam-webster.com/wotd/feed/rss2" "https://wordsmith.org/awad/rss1.xml")))
 '(emojify-display-style (quote unicode))
 '(fci-rule-color "#383838")
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
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#49483E" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#49483E" . 100))))
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
 '(magit-diff-use-overlays nil)
 '(message-send-mail-function (quote message-send-mail-with-sendmail))
 '(message-sendmail-envelope-from (quote header))
 '(message-sendmail-extra-arguments (quote ("--read-envelope-from")))
 '(message-sendmail-f-is-evil t)
 '(org-agenda-day-face-function (quote jd:org-agenda-day-face-holidays-function) t)
 '(notmuch-archive-tags (quote ("-inbox" "-unread")))
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "Today's mail" :query "date:0d.."))))
 '(org-agenda-files
   (quote
    ("~/org/google.org" "~/org/agenda.org" "~/Dropbox/agenda.org")))
 '(org-agenda-include-diary nil t)
 '(org-agenda-start-on-weekday nil t)
 '(org-agenda-window-setup (quote current-window))
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
 '(org-mode-hook
   (quote
    (flyspell-mode auto-fill-mode
                   #[nil "\300\301\302\303\304$\207"
                         [org-add-hook change-major-mode-hook org-show-block-all append local]
                         5]
                   #[nil "\300\301\302\303\304$\207"
                         [org-add-hook change-major-mode-hook org-babel-show-result-all append local]
                         5]
                   org-babel-result-hide-spec org-babel-hide-all-hashes)))
 '(org-return-follows-link t)
 '(org-src-fontify-natively t)
 '(org-src-preserve-indentation t)
 '(org-table-convert-region-max-lines 99999)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(package-enable-at-startup nil)
 '(package-selected-packages
   (quote
    (evil which-key counsel-projectile projectile color-theme-modern zone-sl zone-select zone-rainbow writegood-mode whitespace-cleanup-mode use-package unbound tronesque-theme tree-mode sx smart-mode-line selectric-mode rainbow-delimiters psci psc-ide pass paradox org multi-line monokai-theme magit link-hint ledger-mode keyfreq jedi jabber ivy-purpose intero helm-swoop guide-key grandshell-theme god-mode flyspell-correct-ivy fireplace encourage-mode emojify elm-mode elfeed counsel ace-window ace-jump-mode ace-jump-buffer)))
 '(paradox-automatically-star t)
 '(paradox-github-token t)
 '(pos-tip-background-color "#36473A")
 '(pos-tip-foreground-color "#FFFFC8")
 '(scheme-program-name "meep")
 '(send-mail-function (quote smtpmail-send-it))
 '(sendmail-program "msmtp")
 '(tab-always-indent (quote complete))
 '(twittering-timer-interval 30)
 '(user-mail-address "a.washington@sheffield.ac.uk")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#cc6666")
     (40 . "#de935f")
     (60 . "#f0c674")
     (80 . "#b5bd68")
     (100 . "#8abeb7")
     (120 . "#81a2be")
     (140 . "#b294bb")
     (160 . "#cc6666")
     (180 . "#de935f")
     (200 . "#f0c674")
     (220 . "#b5bd68")
     (240 . "#8abeb7")
     (260 . "#81a2be")
     (280 . "#b294bb")
     (300 . "#cc6666")
     (320 . "#de935f")
     (340 . "#f0c674")
     (360 . "#b5bd68"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(notmuch-search-unread-face ((t (:foreground "deep sky blue")))))
