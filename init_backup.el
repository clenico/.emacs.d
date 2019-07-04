;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

(require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize)
(use-package delight :ensure t)
(use-package use-package-ensure-system-package :ensure t)


(package-initialize)

(use-package dracula-theme
  :config (load-theme 'dracula t)
  (set-face-background 'mode-line "#510370")
  (set-face-background 'mode-line-inactive "#212020"))

;;disable the tab binding for evil-mode
(setq evil-want-C-i-jump nil)



(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t) ; Org-mode's repository
(require 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)
(package-initialize)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(use-package swiper
  :ensure t
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-find-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
    ))
(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)))
(tool-bar-mode -1)

(require 'evil)
(evil-mode 1)
(require 'evil-escape)
(evil-escape-mode 1)
(global-set-key (kbd "M-A") 'caps-lock-mode)

(setq inhibit-startup-screen t)

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(provide 'move-text)


(global-set-key [M-p] 'move-text-up)
(global-set-key [M-n] 'move-text-down)

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))



(setq TeX-parse-self t) ; Enable parse on load.

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-auctex t)

;; Capitalize keywords in SQL mode
(add-hook 'sql-mode-hook 'sqlup-mode)
;; Capitalize keywords in an interactive session (e.g. psql)
(add-hook 'sql-interactive-mode-hook 'sqlup-mode)
;; Set a global keyword to use sqlup on a region
(global-set-key (kbd "C-c u") 'sqlup-capitalize-keywords-in-region)


;; (global-set-key
;;  (kbd "C-c n")
;;  (defhydra hydra-move
;;    (:body-pre (next-line))
;;    "move"
;;    ("n" next-line)
;;    ("p" previous-line)
;;    ("f" forward-char)
;;    ("b" backward-char)
;;    ("a" beginning-of-line)
;;    ("e" move-end-of-line)
;;    ("v" scroll-up-command)
;;    ;; Converting M-v to V here by analogy.
;;    ("V" scroll-down-command)
;;    ("l" recenter-top-bottom)))

(global-set-key
 (kbd "M-o")
 (defhydra hydra-window()
   "window"
   ("h" windmove-left)
   ("j" windmove-down)
   ("k" windmove-up)
   ("l" windmove-right)
   ("x" (lambda ()
	  (interactive)
	  (split-window-right)
	  (windmove-right))
    "vertical")
   ("v" (lambda ()
	  (interactive)
	  (split-window-below)
	  (windmove-down))
    "horizontal")
   ("t" transpose-frame "'")
   ;; ("o" delete-window "one" :color blue)
   ("D" delete-other-windows "one" :color blue)
   ;; ("o" delete-windows-on "one" :color blue)
   ("g" ace-window "go to")
   ("s" ace-swap-window "swap")
   ("d" (lambda ()
	  (interactive)
	  (delete-window)
	  ;; (hydra-window))
	  )
    "del")
   ("i" ace-maximize-window "maximise hint" :color blue)
   ("b" ido-switch-buffer "buffer")
   ;; ("m" headlong-bookmark-jump "bmk")
   ("q" nil "cancel")))

;;
;; ;; ace jump mode major function
;; ;;
(add-to-list 'load-path "/full/path/where/ace-jump-mode.el/in/")
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)



;;
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;;If you use evil
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

(set-face-attribute 'default nil :height 140)

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; (require 'use-package)
;; (setq use-package-always-ensure t)

;; ; then define packages you use
;; (use-package auto-complete)

;; (ac-config-default)

(global-set-key (kbd "C-x k") 'kill-this-buffer)

(global-set-key (kbd "C-c b") 'ranger)

(global-set-key
 (kbd "C-c sd")
(defhydra hydra-orgmode-source (:color blue)
    "
    ^
    ^test ^             ^Do^
    ^─────^─────────────^──^─────────
    ^^                  _o_ C
    ^^                  _r_ python
    ^^                  ^^
    "
    ("o" (insert "C"))
    ("r" (insert "python"))))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default help-window-select t) ;auto focus on help windows

(require 'dashboard)
(dashboard-setup-startup-hook)
;; Or if you use use-package
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(setq dashboard-set-footer nil)

(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))
; )

;; (ace-popup-menu-mode 1)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

;; Bind C-k to kill buffer from `ivy-switch-buffer'
(defun mu-ivy-kill-buffer ()
  (interactive)
  (ivy-set-action 'kill-buffer)
  (ivy-done))

(define-key ivy-switch-buffer-map (kbd "C-k") 'mu-ivy-kill-buffer)

;; (subword-mode 1) ; 1 for on, 0 for off : activation of CamelCase move
(global-subword-mode 1) ; 1 for on, 0 for off : activation of CamelCase move

(defun my-turn-spell-checking-on ()
  "Turn flyspell-mode on."
  (flyspell-mode 1)
  )

(add-hook 'text-mode-hook 'my-turn-spell-checking-on)


;; (require 'helm-config)
;; (helm-mode 1)

;; set specific browser to open links
(setq browse-url-browser-function 'browse-url-qutebrowser)

(use-package browse-url
  :ensure nil
  :custom
  (browse-url-browser-function 'browse-url-generic)
  (browse-url-generic-program "qutebrowser"))

  (show-paren-mode 1)                               ; Show the parent

(set-face-attribute 'default nil :font "Source Code Pro")
(set-fontset-font t 'latin "Noto Sans")

(defun mark-whole-word (&optional arg allow-extend)
  "Like `mark-word', but selects whole words and skips over whitespace.
If you use a negative prefix arg then select words backward.
Otherwise select them forward.

If cursor starts in the middle of word then select that whole word.

If there is whitespace between the initial cursor position and the
first word (in the selection direction), it is skipped (not selected).

If the command is repeated or the mark is active, select the next NUM
words, where NUM is the numeric prefix argument.  (Negative NUM
selects backward.)"
  (interactive "P\np")
  (let ((num  (prefix-numeric-value arg)))
    (unless (eq last-command this-command)
      (if (natnump num)
          (skip-syntax-forward "\\s-")
        (skip-syntax-backward "\\s-")))
    (unless (or (eq last-command this-command)
                (if (natnump num)
                    (looking-at "\\b")
                  (looking-back "\\b")))
      (if (natnump num)
          (left-word)
        (right-word)))
    (mark-word arg allow-extend)))

(use-package window
    :ensure nil
    :bind (("C-x 3" . hsplit-last-buffer)
           ("C-x 2" . vsplit-last-buffer))
    :preface
    (defun hsplit-last-buffer ()
      "Gives the focus to the last created horizontal window."
      (interactive)
      (split-window-horizontally)
      (other-window 1))

    (defun vsplit-last-buffer ()
      "Gives the focus to the last created vertical window."
      (interactive)
      (split-window-vertically)
      (other-window 1)))
   (setq-default
     use-package-always-ensure t                      ; Avoid the :ensure keyword for each package
  )
(blink-cursor-mode 0)
(toc-org-mode 1)
(server-start)
;; (org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))

(use-package ivy-rich
  :init
  (setq ivy-format-function 'ivy-format-function-line)
  :config
  (progn
    (defun ivy-rich-switch-buffer-icon (candidate)
      (with-current-buffer
          (get-buffer candidate)
        (let ((icon (all-the-icons-icon-for-mode major-mode)))
          (if (symbolp icon)
              (all-the-icons-icon-for-mode 'fundamental-mode)
            icon))))
    (setq
      ivy-rich--display-transformers-list
     '(ivy-switch-buffer
       (:columns
        ((ivy-rich-switch-buffer-icon :width 2)
         (ivy-rich-candidate (:width 30))
         (ivy-rich-switch-buffer-size (:width 7))
         (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
         (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
         (ivy-rich-switch-buffer-project (:width 15 :face success))
         (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
        :predicate
        (lambda (cand) (get-buffer cand))))))
  (setq ivy-format-function 'ivy-format-function-line))

(use-package ivy-rich
  :defines (all-the-icons-icon-alist
            all-the-icons-dir-icon-alist
            bookmark-alist)
  :functions (all-the-icons-icon-for-file
              all-the-icons-icon-for-mode
              all-the-icons-icon-family
              all-the-icons-match-to-alist
              all-the-icons-faicon
              all-the-icons-octicon
              all-the-icons-dir-is-submodule)
  :preface
  (defun ivy-rich-bookmark-name (candidate)
    (car (assoc candidate bookmark-alist)))

  (defun ivy-rich-buffer-icon (candidate)
    "Display buffer icons in `ivy-rich'."
    (when (display-graphic-p)
      (let* ((buffer (get-buffer candidate))
             (buffer-file-name (buffer-file-name buffer))
             (major-mode (buffer-local-value 'major-mode buffer))
             (icon (if (and buffer-file-name
                            (all-the-icons-match-to-alist buffer-file-name
                                                          all-the-icons-icon-alist))
                       (all-the-icons-icon-for-file (file-name-nondirectory buffer-file-name)
                                                    :height 0.9 :v-adjust -0.05)
                     (all-the-icons-icon-for-mode major-mode :height 0.9 :v-adjust -0.05))))
        (if (symbolp icon)
            (setq icon (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.9 :v-adjust -0.05))
          icon))))

  (defun ivy-rich-file-icon (candidate)
    "Display file icons in `ivy-rich'."
    (when (display-graphic-p)
      (let* ((path (concat ivy--directory candidate))
             (file (file-name-nondirectory path))
             (icon (cond ((file-directory-p path)
                          (cond
                           ((and (fboundp 'tramp-tramp-file-p)
                                 (tramp-tramp-file-p default-directory))
                            (all-the-icons-octicon "file-directory" :height 0.93 :v-adjust 0.01))
                           ((file-symlink-p path)
                            (all-the-icons-octicon "file-symlink-directory" :height 0.93 :v-adjust 0.01))
                           ((all-the-icons-dir-is-submodule path)
                            (all-the-icons-octicon "file-submodule" :height 0.93 :v-adjust 0.01))
                           ((file-exists-p (format "%s/.git" path))
                            (all-the-icons-octicon "repo" :height 1.0 :v-adjust -0.01))
                           (t (let ((matcher (all-the-icons-match-to-alist candidate all-the-icons-dir-icon-alist)))
                                (apply (car matcher) (list (cadr matcher) :height 0.93 :v-adjust 0.01))))))
                         ((string-match "^/.*:$" path)
                          (all-the-icons-material "settings_remote" :height 0.9 :v-adjust -0.2))
                         ((not (string-empty-p file))
                          (all-the-icons-icon-for-file file :height 0.9 :v-adjust -0.05)))))
        (if (symbolp icon)
            (setq icon (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.9 :v-adjust -0.05))
          icon))))
  :hook ((ivy-mode . ivy-rich-mode)
         (ivy-rich-mode . (lambda ()
                            (setq ivy-virtual-abbreviate
                                  (or (and ivy-rich-mode 'abbreviate) 'name)))))
  :init
  ;; For better performance
  (setq ivy-rich-parse-remote-buffer nil)

  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand)))
          ivy-switch-buffer-other-window
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand)))
          counsel-switch-buffer
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand)))
          persp-switch-to-buffer
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 50))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 50))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 50))
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
          counsel-find-file
          (:columns
           ((ivy-rich-file-icon)
            (ivy-read-file-transformer)))
          counsel-file-jump
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate)))
          counsel-dired
          (:columns
           ((ivy-rich-file-icon)
            (ivy-read-file-transformer)))
          counsel-dired-jump
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate)))
          counsel-git
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate)))
          counsel-recentf
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate (:width 0.8))
            (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))
          counsel-bookmark
          (:columns
           ((ivy-rich-bookmark-type)
            (ivy-rich-bookmark-name (:width 40))
            (ivy-rich-bookmark-info)))
          counsel-projectile-switch-project
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate)))
          counsel-projectile-find-file
          (:columns
           ((ivy-rich-file-icon)
            (counsel-projectile-find-file-transformer)))
          counsel-projectile-find-dir
          (:columns
           ((ivy-rich-file-icon)
            (counsel-projectile-find-dir-transformer)))
          treemacs-projectile
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))))))

(use-package ivy
  :hook (after-init . ivy-mode)
  :config
  (progn
    (setq ivy-use-virtual-buffers t)
    (setq ivy-initial-inputs-alist nil)
    (counsel-mode)
    (ivy-rich-mode)))

(use-package counsel
  :after ivy
  :config
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  :bind
  (("M-x" . counsel-M-x)
    ("C-x C-f" . counsel-find-file)
    ("C-c p f" . counsel-projectile-find-file)
    ("C-c p d" . counsel-projectile-find-dir)
    ("C-c p p" . counsel-projectile-switch-project)
    ("<f1> f" . counsel-describe-function)
    ("<f1> v" . counsel-describe-variable)
    ("<f1> l" . counsel-load-library)
    ("<f2> i" . counsel-info-lookup-symbol)
    ("<f2> u" . counsel-unicode-char)
    ("C-c k" . counsel-rg)))

(use-package treemacs
  :config
  (progn
    (setq treemacs-follow-after-init          t
          treemacs-width                      35
          treemacs-indentation                2
          treemacs-git-integration            t
          treemacs-collapse-dirs              3
          treemacs-silent-refresh             nil
          treemacs-change-root-without-asking nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-show-hidden-files          t
          treemacs-never-persist              nil
          treemacs-is-never-other-window      nil
          treemacs-goto-tag-strategy          'prefetch-index)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (setq treemacs-icons-hash (make-hash-table :size 200 :test #'equal)
          treemacs-icon-fallback (concat
                                  "  "
                                  (all-the-icons-faicon "file-o"
                                                        :face 'all-the-icons-dsilver
                                                        :height 0.9
                                                        :v-adjust -0.05)
                                  " ")
          treemacs-icon-text treemacs-icon-fallback)
    (dolist (item all-the-icons-icon-alist)
      (let* ((extension (car item))
             (func (cadr item))
             (args (append (list (caddr item))
                           '(:height 0.9 :v-adjust -0.05)
                           (cdddr item)))
             (icon (apply func args))
             (key (s-replace-all '(("^" . "") ("\\" . "") ("$" . "") ("." . "")) extension))
             (value (concat "  " icon " ")))
        (ht-set! treemacs-icons-hash (s-replace-regexp "\\?" "" key) value)
        (ht-set! treemacs-icons-hash (s-replace-regexp ".\\?" "" key) value))))
  :bind
  (:map global-map
        ([f8]        . treemacs-toggle)
        ("<C-M-tab>" . treemacs)
        ("M-0"       . treemacs-select-window)
        ("C-c 1"     . treemacs-delete-other-windows)))

(global-set-key (kbd "¬") 'treemacs)

;;https://github.com/abrochard/emacs-config/blob/master/configuration.org
(defun generate-scratch-buffer ()
  "Create and switch to a temporary scratch buffer with a random
     name."
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-")))

(defun sudo ()
  "Use TRAMP to `sudo' the current buffer"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "F" 'my-dired-find-file)
     (defun my-dired-find-file (&optional arg)
       "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
       (interactive "P")
       (let* ((fn-list (dired-get-marked-files nil arg)))
         (mapc 'find-file fn-list)))))

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "F" 'my-dired-find-file)
     (defun my-dired-find-file (&optional arg)
       "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
       (interactive "P")
       (let* ((fn-list (dired-get-marked-files nil arg)))
         (mapc 'find-file fn-list)))))

(defun copy-filename ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun rename-local-var (name)
  (interactive "sEnter new name: ")
  (let ((var (word-at-point)))
    (mark-defun)
    (replace-string var name nil (region-beginning) (region-end))))

(defun move-file ()
  "Write this file to a new location, and delete the old one."
  (interactive)
  (let ((old-location (buffer-file-name)))
    (call-interactively #'write-file)
    (when old-location
      (delete-file old-location))))

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
    current buffer's file. The eshell is renamed to match that
    directory to make multiple eshell windows easier."
  (interactive)
  (let* ((height (/ (window-total-height) 3)))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (insert (concat "ls"))
    (eshell-send-input)))
(bind-key "C-!" 'eshell-here)

(require 'color)
(if (display-graphic-p)
    (set-face-attribute 'org-block nil :background
                        (color-darken-name
                         (face-attribute 'default :background) 10)))
(which-key-mode t)

(setq org-catch-invisible-edits 'show-and-error)

;i should look into this
;; (use-package elfeed
;;   :bind ("C-x w" . elfeed))

;; (use-package elfeed-org
;;   :config
;;   (setq rmh-elfeed-org-files (list (concat config-load-path "elfeed.org")))
;;   (elfeed-org))

(setq initial-scratch-message nil)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq auto-window-vscroll nil)


(global-hl-line-mode)                             ; Hightlight current line

(setq require-final-newline t)

(setq-default buffer-file-coding-system 'utf-8)
(setq-default default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

(show-paren-mode 1)
(electric-pair-mode 1)

(use-package paradox
  :custom
  (paradox-execute-asynchronously t)
  :config
  (paradox-enable))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h F" . helpful-function)
         ("C-h o" . helpful-symbol)
         ("C-h C" . helpful-command)))
(use-package elisp-demos
  :config (advice-add 'helpful-update
                      :after #'elisp-demos-advice-helpful-update))
;; (use-package undo-tree
;;   :diminish undo-tree-mode
;;   :config
;;   (global-undo-tree-mode t)
;;   (setq undo-tree-visualizer-diff t))

;; (use-package volatile-highlights
;;   :diminish volatile-highlights-mode
;;   :config
;;   (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
;;   (vhl/install-extension 'undo-tree)
;;   (volatile-highlights-mode t))

(use-package electric-operator
  :config
  (electric-operator-add-rules-for-mode 'php-mode
                                        (cons " - >" "->"))
  (electric-operator-add-rules-for-mode 'php-mode
                                        (cons " / /" "// "))
  (electric-operator-add-rules-for-mode 'php-mode
                                        (cons " = > " " => "))
  (electric-operator-add-rules-for-mode 'php-mode
                                        (cons " < ?" "<?"))
  (electric-operator-add-rules-for-mode 'js2-mode
                                        (cons " = > " " => "))
  (electric-operator-add-rules-for-mode 'js2-jsx-mode
                                        (cons " = > " " => "))
  (electric-operator-add-rules-for-mode 'rjsx-mode
                                        (cons " = > " " => ")))

(use-package dumb-jump
  :config (setq dumb-jump-aggressive nil))

(use-package highlight-numbers
  :config (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package eldoc
  :diminish eldoc-mode
  :config (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(use-package rainbow-delimiters
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

;;learn how to use it
(use-package paredit
  :config
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package eros
  :config (add-hook 'emacs-lisp-mode-hook 'eros-mode))

(use-package suggest)

(require 'sql)
(sql-set-product "mysql")

(use-package sqlup-mode
  :config (add-hook 'sql-mode-hook 'sqlup-mode))

(use-package mysql-to-org
  :config
  (add-hook 'sql-mode-hook 'mysql-to-org-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
   (custom-set-faces
    '(rainbow-delimiters-depth-0-face ((t (:foreground "saddle brown"))))
    '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
    '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
    '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
    '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
    '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
    '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
    '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
    '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1"))))
    '(rainbow-delimiters-unmatched-face ((t (:foreground "black"))))))

(use-package git-gutter
    :defer 0.3
    :init (global-git-gutter-mode +1))

  (use-package company
    :diminish company-mode
    :config
    (add-hook 'after-init-hook 'global-company-mode)
    (setq company-minimum-prefix-length 2)
    (setq company-dabbrev-downcase nil)
    (setq company-begin-commands '(self-insert-command))
    (setq company-idle-delay .1)
    (setq company-minimum-prefix-length 2)
    (setq company-show-numbers t)
    (setq company-tooltip-align-annotations 't)
    (bind-keys :map company-active-map
           ("C-d" . company-show-doc-buffer)
           ("C-l" . company-show-location)
           ("C-n" . company-select-next)
           ("C-p" . company-select-previous)
           ("TAB" . company-complete)))

(use-package company-go)

(use-package company-box
    :after company
    :hook (company-mode . company-box-mode))

(global-company-mode t)

(toc-org-mode t)

(setq scroll-conservatively 10000
scroll-preserve-screen-position 1)

