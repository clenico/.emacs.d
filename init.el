(setq-default buffer-file-coding-system 'utf-8)
(setq-default default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(setq debug-on-error t)

(defun load-backup-config()
            "load the backup config to debug easily"
            (interactive)
            (load "~/.emacs.d/init_backup.el"))

(require 'package)
(package-initialize)

;; (load "~/.emacs.d/init_backup.el")

;; (load "~/.emacs.d/custom.el")
;; (load "~/Documents/Projects/.emacs.d/config.el")
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
;;(org-babel-load-file (expand-file-name "~/Documents/Projects/.emacs.d/config.org"))
