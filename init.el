(defun load-backup-config()
            "load the backup config to debug easily"
            (interactive)
            (load "~/.emacs.d/init_backup.el"))
(defun open_conf_org()
  "open config.org, need to delete this one i get this to work"
  (interactive)
  (find-file "~/.emacs.d/config.org")
  )
(require 'package)
(package-initialize)

;; Ensure that use-package is installed.
;;
;; If use-package isn't already installed, it's extremely likely that this is a
;; fresh installation! So we'll want to update the package repository and
;; install use-package before loading the literate configuration.

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; (load "~/.emacs.d/init_backup.el")
;; (load "~/.emacs.d/custom.el")
;; (load "~/Documents/Projects/.emacs.d/config.el")
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
;; (org-babel-load-file (expand-file-name "~/.emacs.d.old/config.org"))
;; (org-babel-load-file (expand-file-name "~/Documents/Tmp/.emacs.d/config.org"))
;; (org-babel-load-file (expand-file-name "~/Documents/Projects/.emacs.d/config.org"))
;; (org-babel-load-file (expand-file-name "~/Downloads/.emacs.d.previous/config.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-verbose nil t)
 '(package-selected-packages
   (quote
    (spaceline yasnippet-snippets yarn-mode yaml-mode xref-js2 wiki-summary which-key webpaste web-mode vue-mode virtualenvwrapper use-package-ensure-system-package try treemacs-projectile treemacs-icons-dired toc-org tide switch-window suggest sqlup-mode sql-indent smooth-scrolling smartparens scss-mode react-snippets ranger rainbow-mode rainbow-delimiters prettier-js popup-kill-ring plantuml-mode pdf-tools paredit paradox org-re-reveal org-plus-contrib org-journal org-bullets nov nord-theme mysql-to-org mu4e-alert move-text markdown-preview-mode major-mode-hydra lua-mode lsp-ui lsp-python-ms lsp-java lorem-ipsum ledger-mode langtool json-mode js2-refactor ivy-yasnippet ivy-rich ivy-pass ivy-hydra imgbb iedit ibuffer-vc ibuffer-projectile ibuffer-git hungry-delete highlight-numbers helpful gradle-mode google-c-style gnuplot-mode gnuplot git-timemachine git-gutter forge flyspell-correct-ivy flymd flycheck-ledger fancy-battery expand-region evil-escape evil-avy eros erc-image erc-hl-nicks engine-mode emmet-mode elpy elisp-demos electric-operator editorconfig dumb-jump dracula-theme doom-modeline dockerfile-mode dired-subtree dired-narrow delight dashboard dap-mode csv-mode counsel-projectile company-tern company-math company-lsp company-go company-box company-auctex cmake-ide cmake-font-lock ccls caps-lock auto-yasnippet atomic-chrome all-the-icons-ivy aggressive-indent ace-jump-mode ac-php)))
 '(sp-escape-quotes-after-insert nil t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
