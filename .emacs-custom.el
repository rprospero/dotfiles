(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-image-file-mode t)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (smart-mode-line-respectful)))
 '(diff-switches "-u")
 '(display-buffer-alist
   (quote
    (("agenda.org"
      (display-buffer-reuse-window display-buffer-use-some-window)
      (nil)))))
 '(elfeed-feeds
   (quote
    ("http://www.xkcd.org/atom.xml" "https://www.reddit.com/r/emacs.rss" "https://www.reddit.com/r/haskell.rss" "http://www.merriam-webster.com/wotd/feed/rss2" "https://wordsmith.org/awad/rss1.xml" "http://us10.campaign-archive1.com/feed?u=49a6a2e17b12be2c5c4dcb232&id=ffbbbbd930")))
 '(flycheck-disabled-checkers (quote (haskell-stack-ghc)))
 '(flymake-python-pyflakes-executable "flake8")
 '(flymake-python-pyflakes-extra-arguments (quote (" --ignore=E302,E226")))
 '(hl-sexp-background-color "#1c1f26")
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
 '(package-enable-at-startup nil)
 '(package-selected-packages
   (quote
    (evil-magit langtool smooth-scrolling evil-god-state evil-escape telepathy ten-hundred-mode evil-tutor flycheck-jshint flymake-jshint web-completion-data which-key counsel-projectile projectile color-theme-modern color-theme zone-sl zone-select zone-rainbow zone-nyan zone-matrix yasnippet yaml-mode writegood-mode wolfram-mode whitespace-cleanup-mode voca-builder use-package unicode-fonts undo-tree unbound twittering-mode tronesque-theme tron-theme tree-mode tle tabbar systemd sx switch-window suggest spray smart-mode-line shorten shm sexy-monochrome-theme selectric-mode rainbow-delimiters pylint purescript-mode pov-mode pcmpl-pip pcmpl-git pass ox-ioslide org-pandoc oauth2 nyan-mode notmuch multi-line monokai-theme moe-theme material-theme magit lush-theme lui link-hint ledger-mode lcs kv keyfreq jedi jabber ivy-purpose intero hi2 hackernews guide-key-tip grandshell-theme goto-chg god-mode gnuplot gitignore-mode git-timemachine flyspell-correct-ivy flymake-python-pyflakes flycheck-pyflakes flycheck-purescript flycheck-ledger flycheck-haskell fireplace esup esqlite encourage-mode emojify emoji-fontset elfeed-goodies ein-mumamo dracula-theme doom-themes docker darktooth-theme cyberpunk-theme cuda-mode counsel company-jedi company-ghc color-theme-sanityinc-tomorrow circe cherry-blossom-theme auctex android-mode ample-zen-theme ace-window ace-jump-buffer 2048-game)))
 '(revert-without-query (quote ("*.png")))
 '(user-mail-address "a.washington@sheffield.ac.uk"))



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(notmuch-search-unread-face ((t (:foreground "deep sky blue")))))
