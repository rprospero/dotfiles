(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-term-color-vector
   [unspecified "#081724" "#ff694d" "#68f6cb" "#fffe4e" "#bad6e2" "#afc0fd" "#d2f1ff" "#d3f9ee"] t)
 '(auto-image-file-mode t)
 '(browse-url-browser-function (quote browse-url-firefox))
 '(column-number-mode t)
 '(csharp-make-tool "mcs" t)
 '(custom-enabled-themes (quote (smart-mode-line-respectful)))
 '(custom-safe-themes
   (quote
    ("0973b33d2f15e6eaf88400eee3dc8357ad8ae83d2ca43c125339b25850773a70" "abd7719fd9255fcd64f631664390e2eb89768a290ee082a9f0520c5f12a660a8" "c51e302edfe6d2effca9f7c9a8a8cfc432727efcf86246002a3b45e290306c1f" "0f302165235625ca5a827ac2f963c102a635f27879637d9021c04d845a32c568" "ed92c27d2d086496b232617213a4e4a28110bdc0730a9457edf74f81b782c5cf" "1c10e946f9a22b28613196e4c02b6508970e3b34660282ec92d9a1c293ee81bb" "9a3c51c59edfefd53e5de64c9da248c24b628d4e78cc808611abd15b3e58858f" "5eb4b22e97ddb2db9ecce7d983fa45eb8367447f151c7e1b033af27820f43760" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "427fed191e7a766152e59ef0e2904283f436dbbe259b9ccc04989f3acde50a55" "42ba25fad91db26bfa0130161412d49804ff27e6a09bf45f1a8268511300d981" "7c0f19a98f44505031ceeba97a53500a5be3b435e77f5b68d7a4d0e24d26d409" "0820d191ae80dcadc1802b3499f84c07a09803f2cb90b343678bdb03d225b26b" "88ae008e9bf586a903dfb4e7ca6d9c06f6b1f8ce10d2ae89295a4114a6f2c3f3" "557c283f4f9d461f897b8cac5329f1f39fac785aa684b78949ff329c33f947ec" "cfa7053f155661faa33ef648f55d524eb97854f8f0ff9ff91a08b3ba47a9a25f" "74278d14b7d5cf691c4d846a4bbf6e62d32104986f104c1e61f718f9669ec04b" "1fc1fdf975c8c8c3767c29787a063eee50cbceef903644a0771fa66568ee8777" "b0ab5c9172ea02fba36b974bbd93bc26e9d26f379c9a29b84903c666a5fde837" "71ecffba18621354a1be303687f33b84788e13f40141580fa81e7840752d31bf" "e56ee322c8907feab796a1fb808ceadaab5caba5494a50ee83a13091d5b1a10c" "c567c85efdb584afa78a1e45a6ca475f5b55f642dfcd6277050043a568d1ac6f" "345f8f92edc3508574c61850b98a2e0a7a3f5ba3bb9ed03a50f6e41546fe2de0" "e8a976fbc7710b60b069f27f5b2f1e216ec8d228fe5091f677717d6375d2669f" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "e97dbbb2b1c42b8588e16523824bc0cb3a21b91eefd6502879cf5baa1fa32e10" "cc60d17db31a53adf93ec6fad5a9cfff6e177664994a52346f81f62840fe8e23" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "c86f868347919095aa44d2a6129dd714cbcf8feaa88ba954f636295b14ceff8f" "f0d8af755039aa25cd0792ace9002ba885fd14ac8e8807388ab00ec84c9497d7" "8fed5e4b89cf69107d524c4b91b4a4c35bcf1b3563d5f306608f0c48f580fdf8" "90edd91338ebfdfcd52ecd4025f1c7f731aced4c9c49ed28cfbebb3a3654840b" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "cedd3b4295ac0a41ef48376e16b4745c25fa8e7b4f706173083f16d5792bb379" "f0a99f53cbf7b004ba0c1760aa14fd70f2eabafe4e62a2b3cf5cabae8203113b" default)))
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
    ("http://www.xkcd.org/atom.xml" "https://www.reddit.com/r/emacs.rss" "https://www.reddit.com/r/haskell.rss" "http://www.merriam-webster.com/wotd/feed/rss2" "https://wordsmith.org/awad/rss1.xml" "http://us10.campaign-archive1.com/feed?u=49a6a2e17b12be2c5c4dcb232&id=ffbbbbd930")))
 '(emojify-display-style (quote unicode))
 '(flycheck-disabled-checkers (quote (haskell-stack-ghc)))
 '(flymake-python-pyflakes-executable "flake8")
 '(flymake-python-pyflakes-extra-arguments (quote (" --ignore=E302,E226")))
 '(gnus-secondary-select-methods
   (quote
    ("~/.cabal/bin" "/opt/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/MacPorts/Emacs.app/Contents/MacOS/libexec" "/Applications/MacPorts/Emacs.app/Contents/MacOS/bin" "/usr/local/bin/stack")))
 '(gnus-select-method (quote (nntp "news.gwene.org")))
 '(guide-key/recursive-key-sequence-flag t)
 '(haskell-tags-on-save t)
 '(helm-split-window-in-side-p t)
 '(hl-sexp-background-color "#1c1f26")
 '(holiday-other-holidays
   (quote
    ((holiday-float 5 1 -1 "Spring Bank Holiday")
     (holiday-float 5 1 1 "May Day Brank Holiday")
     (holiday-float 8 1 -1 "Late Summer Bank Holidays"))))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(mouse-autoselect-window 0.5)
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
 '(package-enable-at-startup nil)
 '(package-selected-packages
   (quote
    (evil-magit langtool smooth-scrolling evil-god-state evil-escape telepathy ten-hundred-mode evil-tutor flycheck-jshint flymake-jshint web-completion-data which-key counsel-projectile projectile color-theme-modern color-theme zone-sl zone-select zone-rainbow zone-nyan zone-matrix yasnippet yaml-mode writegood-mode wolfram-mode whitespace-cleanup-mode voca-builder use-package unicode-fonts undo-tree unbound twittering-mode tronesque-theme tron-theme tree-mode tle tabbar systemd sx switch-window suggest spray smart-mode-line shorten shm sexy-monochrome-theme selectric-mode rainbow-delimiters pylint purescript-mode pov-mode pcmpl-pip pcmpl-git pass ox-ioslide org-pandoc oauth2 nyan-mode notmuch multi-line monokai-theme moe-theme material-theme magit lush-theme lui link-hint ledger-mode lcs kv keyfreq jedi jabber ivy-purpose intero hi2 hackernews guide-key-tip grandshell-theme goto-chg god-mode gnuplot gitignore-mode git-timemachine flyspell-correct-ivy flymake-python-pyflakes flycheck-pyflakes flycheck-purescript flycheck-ledger flycheck-haskell fireplace esup esqlite encourage-mode emojify emoji-fontset elfeed-goodies ein-mumamo dracula-theme doom-themes docker darktooth-theme cyberpunk-theme cuda-mode counsel company-jedi company-ghc color-theme-sanityinc-tomorrow circe cherry-blossom-theme auctex android-mode ample-zen-theme ace-window ace-jump-buffer 2048-game)))
 '(paradox-automatically-star t)
 '(paradox-github-token t)
 '(revert-without-query (quote ("*.png")))
 '(scheme-program-name "meep")
 '(send-mail-function (quote smtpmail-send-it))
 '(sendmail-program "msmtp")
 '(tab-always-indent (quote complete))
 '(twittering-timer-interval 30)
 '(user-mail-address "a.washington@sheffield.ac.uk"))



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(notmuch-search-unread-face ((t (:foreground "deep sky blue")))))
