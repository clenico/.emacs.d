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
;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                            ("melpa" . "http://melpa.org/packages/")
;;                            ("org" . "http://orgmode.org/elpa/")))

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; (load "~/.emacs.d/init_backup.el")
;; (load "~/.emacs.d/custom.el")
;; (load "~/Documents/Projects/.emacs.d/config.el")


(if (equal (shell-command-to-string "~/.emacs.d/get_hostname.sh") "Virtualbox")
    (org-babel-load-file (expand-file-name "~/.emacs.d/config_light.org"))
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))


;; (org-babel-load-file (expand-file-name "~/.emacs.d.old/config.org"))
;; (org-babel-load-file (expand-file-name "~/Documents/Tmp/.emacs.d/config.org"))
;; (org-babel-load-file (expand-file-name "~/Documents/Projects/.emacs.d/config.org"))
;; (org-babel-load-file (expand-file-name "~/Downloads/.emacs.d.previous/config.org"))
(put 'narrow-to-region 'disabled nil)
