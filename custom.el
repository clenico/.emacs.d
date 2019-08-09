(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-auto-save t)
 '(TeX-byte-compile t)
 '(TeX-clean-confirm nil)
 '(TeX-master (quote dwim))
 '(TeX-parse-self t)
 '(TeX-source-correlate-mode t)
 '(TeX-view-program-selection (quote ((output-pdf "PDF Tools"))))
 '(aggressive-indent-comments-too nil)
 '(alert-default-style (quote libnotify))
 '(auto-revert-verbose nil)
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "firefox")
 '(ccls-args nil)
 '(ccls-executable "/snap/bin/ccls")
 '(css-indent-offset 2)
 '(elpy-rpc-backend "jedi" t)
 '(flycheck-display-errors-delay 1)
 '(flyspell-abbrev-p t)
 '(flyspell-default-dictionary "en_US")
 '(flyspell-issue-message-flag nil)
 '(flyspell-issue-welcome-flag nil)
 '(history-delete-duplicates t)
 '(ispell-dictionary "en_US")
 '(ispell-dictionary-alist
   (quote
    (("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil
      ("-d" "en_US")
      nil utf-8)
     ("fr_BE" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil
      ("-d" "fr_BE")
      nil utf-8))) t)
 '(ispell-program-name "/usr/bin/hunspell")
 '(ispell-really-hunspell t t)
 '(ispell-silently-savep t)
 '(lsp-prefer-flymake nil)
 '(math-additional-units
   (quote
    ((GiB "1024 * MiB" "Giga Byte")
     (MiB "1024 * KiB" "Mega Byte")
     (KiB "1024 * B" "Kilo Byte")
     (B nil "Byte")
     (Gib "1024 * Mib" "Giga Bit")
     (Mib "1024 * Kib" "Mega Bit")
     (Kib "1024 * b" "Kilo Bit")
     (b "B / 8" "Bit"))) t)
 '(math-units-table nil t)
 '(org-agenda-files
   (quote
    ("~/Google Drive/TFE/Rapport.org" "~/Org/agenda/organizer.org")))
 '(org-capture-templates
   (quote
    (("B" "Book" checkitem
      (file+headline "~/Org/other/books.org" "Books")
      "- [ ] %^{Book}" :immediate-finish t)
     ("L" "Learning" checkitem
      (file+headline "~/Org/other/learning.org" "Things")
      "- [ ] %^{Thing}" :immediate-finish t)
     ("M" "Movie" checkitem
      (file+headline "~/Org/other/movies.org" "Movies")
      "- [ ] %^{Movie}" :immediate-finish t)
     ("P" "Purchase" checkitem
      (file+headline "~/Org/other/purchases.org" "Purchases")
      "- [ ] %^{Item}" :immediate-finish t)
     ("l" "Ledger")
     ("lb" "Bank" plain
      (file "~/Org/ledger/ledger-2019.dat")
      "%(org-read-date) %^{Payee}
  Expenses:%^{Account}  €%^{Amount}
  Liabilities:CreditsCards:Belfius" :empty-lines 1 :immediate-finish t)
     ("lc" "Cash" plain
      (file "~/Org/ledger/ledger-2019.dat")
      "%(org-read-date) * %^{Payee}
  Expenses:%^{Account}  €%^{Amount}
  Assets:Cash:Wallet" :empty-lines 1 :immediate-finish t)
     ("f" "FindMyCat" entry
      (file+headline "~/Org/agenda/findmycat.org" "Tasks")
      "* TODO %^{Task}
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:
Captured %<%Y-%m-%d %H:%M>" :empty-lines 1)
     ("p" "People" entry
      (file+headline "~/Org/agenda/people.org" "Tasks")
      "* TODO %^{Task}
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:
Captured %<%Y-%m-%d %H:%M>" :empty-lines 1)
     ("s" "School" entry
      (file+headline "~/Org/agenda/school.org" "Tasks")
      "* TODO %^{Task}
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:
Captured %<%Y-%m-%d %H:%M>" :empty-lines 1)
     ("t" "Task" entry
      (file+headline "~/Org/agenda/organizer.org" "Tasks")
      "* TODO %^{Task}
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:
Captured %<%Y-%m-%d %H:%M>" :empty-lines 1)
     ("j" "Journal" entry
      (file+datetree "~/Org/Journal/journal.org")
      "" :empty-lines 1))) t)
 '(org-latex-compiler "xelatex")
 '(org-reveal-mathjax t t)
 '(org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/" t)
 '(org-todo-keyword-faces
   (quote
    (("TODO" :foreground "red" :weight bold)
     ("NEXT" :foreground "magenta" :weight bold)
     ("WAITING" :foreground "orange" :weight bold)
     ("DONE" :foreground "green" :weight bold)
     ("CANCELED" :foreground "gray" :weight bold)
     ("BUG" :foreground "yellow" :weight bold)
     ("KNOWNCAUSE" :foreground "light sea green" :weight bold)
     ("FIXED" :foreground "forest green" :weight bold))))
 '(package-selected-packages
   (quote
    (org-ref ox-bibtex eyebrowse flycheck-pos-tip org spaceline yasnippet-snippets yarn-mode yaml-mode xref-js2 wiki-summary which-key webpaste web-mode vue-mode virtualenvwrapper use-package-ensure-system-package try treemacs-projectile treemacs-icons-dired toc-org tide switch-window suggest sqlup-mode sql-indent smooth-scrolling smartparens scss-mode react-snippets ranger rainbow-mode rainbow-delimiters prettier-js popup-kill-ring plantuml-mode pdf-tools paredit paradox org-re-reveal org-plus-contrib org-journal org-bullets nov nord-theme mysql-to-org mu4e-alert move-text markdown-preview-mode major-mode-hydra lua-mode lsp-ui lsp-python-ms lsp-java lorem-ipsum ledger-mode langtool json-mode js2-refactor ivy-yasnippet ivy-rich ivy-pass ivy-hydra imgbb iedit ibuffer-vc ibuffer-projectile ibuffer-git hungry-delete highlight-numbers helpful gradle-mode google-c-style gnuplot-mode gnuplot git-timemachine git-gutter forge flyspell-correct-ivy flymd flycheck-ledger fancy-battery expand-region evil-escape evil-avy eros erc-image erc-hl-nicks engine-mode emmet-mode elpy elisp-demos electric-operator editorconfig dumb-jump dracula-theme doom-modeline dockerfile-mode dired-subtree dired-narrow delight dashboard dap-mode csv-mode counsel-projectile company-tern company-math company-lsp company-go company-box company-auctex cmake-ide cmake-font-lock ccls caps-lock auto-yasnippet atomic-chrome all-the-icons-ivy aggressive-indent ace-jump-mode ac-php)))
 '(paradox-execute-asynchronously t)
 '(pdf-view-display-size (quote fit-page))
 '(pdf-view-resize-factor 1.1)
 '(pdf-view-use-unicode-ligther nil)
 '(projectile-completion-system (quote ivy))
 '(projectile-enable-caching t)
 '(projectile-keymap-prefix "p")
 '(projectile-mode-line (quote (:eval (projectile-project-name))) t)
 '(projectile-project-root-files-top-down-recurring
   (quote
    ("compile_commands.json" ".ccls" "compile_commands.json" ".ccls" "compile_commands.json" ".ccls" "compile_commands.json" ".ccls" "compile_commands.json" ".ccls" "compile_commands.json" ".ccls" "compile_commands.json" ".ccls" ".svn" "CVS" "Makefile")))
 '(ranger-preview-file 1 t)
 '(recentf-exclude
   (quote
    ("COMMIT_EDITMSG" "~$" "/scp:" "/ssh:" "/sudo:" "/tmp/")))
 '(recentf-max-menu-items 15)
 '(recentf-max-saved-items 200)
 '(savehist-save-minibuffer-history 1)
 '(sp-escape-quotes-after-insert nil)
 '(undo-tree-visualizer-diff t)
 '(undo-tree-visualizer-timestamps t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-0-face ((t (:foreground "saddle brown"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "black")))))
