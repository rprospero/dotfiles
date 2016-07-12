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
 '(browse-url-browser-function (quote browse-url-firefox))
 '(csharp-make-tool "mcs" t)
 '(custom-enabled-themes (quote (sanityinc-tomorrow-bright)))
 '(custom-safe-themes
   (quote
    ("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(diff-switches "-u")
 '(display-buffer-alist
   (quote
    (("agenda.org"
      (display-buffer-reuse-window display-buffer-use-some-window)
      (nil)))))
 '(emojify-display-style (quote unicode))
 '(exec-path
   (quote
    ("~/.cabal/bin" "/opt/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/MacPorts/Emacs.app/Contents/MacOS/libexec" "/Applications/MacPorts/Emacs.app/Contents/MacOS/bin" "/usr/local/bin/stack")))
 '(gnus-select-method (quote (nntp "news.gwene.org")))
 '(guide-key/recursive-key-sequence-flag t)
 '(haskell-tags-on-save t)
 '(indent-tabs-mode nil)
 '(ispell-dictionary nil t)
 '(message-send-mail-function (quote message-send-mail-with-sendmail) t)
 '(message-sendmail-envelope-from (quote header) t)
 '(message-sendmail-extra-arguments (quote ("--read-envelope-from")) t)
 '(message-sendmail-f-is-evil t t)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(send-mail-function (quote smtpmail-send-it))
 '(sendmail-program "msmtp")
 '(tab-always-indent (quote complete)))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
