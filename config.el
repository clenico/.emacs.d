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

(defun what-is-my-ip ()
  (interactive)
  (message "IP: %s"
           (with-current-buffer (url-retrieve-synchronously "https://api.ipify.org")
             (buffer-substring (+ 1 url-http-end-of-headers) (point-max)))))

(defun what-the-commit ()
  (interactive)
  (insert
   (with-current-buffer
       (url-retrieve-synchronously "http://whatthecommit.com")
     (re-search-backward "<p>\\([^<]+\\)\n<\/p>")
     (match-string 1))))

;; (package-refresh-contents)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; (use-package delight :ensure t)
(use-package use-package-ensure-system-package :ensure t)

(setq-default
 ad-redefinition-action 'accept                   ; Silence warnings for redefinition
 cursor-in-non-selected-windows t                 ; Hide the cursor in inactive windows
 display-time-default-load-average nil            ; Don't display load average
 fill-column 80                                   ; Set width for automatic line breaks
 help-window-select t                             ; Focus new help windows when opened
 indent-tabs-mode nil                             ; Prefers spaces over tabs
 inhibit-startup-screen t                         ; Disable start-up screen
 initial-scratch-message ""                       ; Empty the initial *scratch* buffer
 kill-ring-max 128                                ; Maximum length of kill ring
 load-prefer-newer t                              ; Prefers the newest version of a file
 mark-ring-max 128                                ; Maximum length of mark ring
 scroll-conservatively most-positive-fixnum       ; Always scroll by one line
 select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
 tab-width 4                                      ; Set width for tabs
 use-package-always-ensure t                      ; Avoid the :ensure keyword for each package
 user-full-name "NICOLAS Clement"                 ; Set the full name of the current user
 user-mail-address "niccle27@gmail.com"           ; Set the email address of the current user
 vc-follow-symlinks t                             ; Always follow the symlinks
 view-read-only t)                                ; Always open read-only buffers in view-mode
(cd "~/")                                         ; Move to the user directory
(column-number-mode 1)                            ; Show the column number
(set-fill-column 80)
(display-time-mode 0)                             ; Enable time in the mode-line
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(set-default-coding-systems 'utf-8)               ; Default to utf-8 encoding
(show-paren-mode 1)                               ; Show the parent
(global-display-line-numbers-mode)                ; Always display lines number
(blink-cursor-mode 0)                             ; Disable Cursor Blinking
(server-start)                                    ; Start emacs Server
(set-face-attribute 'default nil :height 140)     ; Set fonts size
(global-hl-line-mode)                             ; Hightlight current line
(when window-system
    (menu-bar-mode 1)                             ; Disable the menu bar
    (scroll-bar-mode 1)                           ; Enable the scroll bar
    (tool-bar-mode -1)                            ; Disable the tool bar
    (tooltip-mode 1))                             ; Enable tooltips

(use-package move-text
  :bind (("M-p" . move-text-up)
	 ("M-n" . move-text-down))
  :config (move-text-default-bindings))

(use-package evil
  :init
  (evil-mode 1)
  (setq evil-want-C-i-jump nil);disable the tab binding for evil-mode
)

(use-package evil-escape
  :after evil
  :init
  (evil-escape-mode 1)
  :bind
  (("M-A" . caps-lock-mode)))

(use-package ace-jump-mode)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

(defun my/smarter-move-beginning-of-line (arg)
  "Moves point back to indentation of beginning of line.

   Move point to the first non-whitespace character on this line.
   If point is already there, move to the beginning of the line.
   Effectively toggle between the first non-whitespace character and
   the beginning of the line.

   If ARG is not nil or 1, move forward ARG - 1 lines first. If
   point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key (kbd "C-a") 'my/smarter-move-beginning-of-line)

(use-package imenu
    :ensure nil
    :bind ("C-R" . imenu))

(use-package iedit
  :ensure t
:bind (("M-e" . iedit-mode)))

(use-package expand-region
  :bind (("C-+" . er/contract-region)
         ("C-=" . er/expand-region)))

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file t))

(use-package autorevert
  :ensure nil
  :bind ("C-x R" . revert-buffer)
  :custom (auto-revert-verbose nil)
  :config (global-auto-revert-mode 1))

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

(use-package smartparens
  :defer 1
  :custom (sp-escape-quotes-after-insert nil)
  :config (smartparens-global-mode 1))

(set-face-attribute 'default nil :font "Source Code Pro")
(set-fontset-font t 'latin "Noto Sans")

(use-package dracula-theme
  :config (load-theme 'dracula t)
  (set-face-background 'mode-line "#510370")
  (set-face-background 'mode-line-inactive "#212020"))

(require 'color)
(if (display-graphic-p)
    (set-face-attribute 'org-block nil :background
                        (color-darken-name
                         (face-attribute 'default :background) 10)))

(use-package all-the-icons :defer 0.5)

;; (use-package abbrev
;;   :hook (text-mode . abbrev-mode)
;;   :config
;;   (load "~/.emacs.d/lisp/my-abbrev.el"))

(use-package flyspell
  :hook ((markdown-mode org-mode text-mode) . flyspell-mode)
         (prog-mode . flyspell-prog-mode)
  :custom
  (flyspell-abbrev-p t)
  (flyspell-default-dictionary "en_US")
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil))

(use-package flyspell-correct-ivy
  :after (flyspell ivy)
  :init (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package ispell
:defer 2
:ensure-system-package (hunspell . "trizen -S hunspell")
:custom
(ispell-dictionary "en_US")
(ispell-dictionary-alist
 '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
   ("fr_BE" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "fr_BE") nil utf-8)))
(ispell-program-name (executable-find "hunspell"))
(ispell-really-hunspell t)
(ispell-silently-savep t)
:preface
(defun my/switch-language ()
  "Switches between the English and French language."
  (interactive)
  (let* ((current-dictionary ispell-current-dictionary)
         (new-dictionary (if (string= current-dictionary "fr_BE") "en_US" "fr_BE")))
    (ispell-change-dictionary new-dictionary)
    (if (string= new-dictionary "fr_BE")
        (langtool-switch-default-language "fr")
      (langtool-switch-default-language "en"))

    ;;Clears all these old errors after switching to the new language
    (if (and (boundp 'flyspell-mode) flyspell-mode)
        (flyspell-mode 0)
      (flyspell-mode 1))

  (message "Dictionary switched from %s to %s" current-dictionary new-dictionary))))

(use-package lorem-ipsum
  :bind (("C-c C-v l" . lorem-ipsum-insert-list)
         ("C-c C-v p" . lorem-ipsum-insert-paragraphs)
         ("C-c C-v s" . lorem-ipsum-insert-sentences)))

(use-package paradox
  :custom
  (paradox-execute-asynchronously t)
  :config
  (paradox-enable))

(use-package which-key
  :defer 0.2
  :config (which-key-mode)
  :bind
  ("<f5>" . which-key-show-top-level))

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
   ;; ("C-c p f" . counsel-projectile-find-file)
   ;; ("C-c p d" . counsel-projectile-find-dir)
   ;; ("C-c p p" . counsel-projectile-switch-project)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-load-library)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char)
   ("C-c k" . counsel-rg)))

;; (use-package ivy-pass
;;   :after ivy
;;   :commands ivy-pass)

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         :map swiper-map
         ("M-%" . swiper-query-replace)))

(use-package pdf-tools
  :defer 1
  :magic ("%PDF" . pdf-view-mode)
  :init (pdf-tools-install :no-query))

(use-package pdf-view
  :ensure nil
  :after pdf-tools
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward)
              ("d" . pdf-annot-delete)
              ("h" . pdf-annot-add-highlight-markup-annotation)
              ("t" . pdf-annot-add-text-annotation))
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-view-resize-factor 1.1)
  (pdf-view-use-unicode-ligther nil))

(use-package projectile
  :defer 1
  :preface
  (defun my/projectile-compilation-buffers (&optional project)
    "Get a list of a project's compilation buffers.
  If PROJECT is not specified the command acts on the current project."
    (let* ((project-root (or project (projectile-project-root)))
           (buffer-list (mapcar #'process-buffer compilation-in-progress))
           (all-buffers (cl-remove-if-not
                         (lambda (buffer)
                           (projectile-project-buffer-p buffer project-root))
                         buffer-list)))
      (if projectile-buffers-filter-function
          (funcall projectile-buffers-filter-function all-buffers)
        all-buffers)))
  :custom
  (projectile-completion-system 'ivy)
  (projectile-enable-caching t)
  (projectile-keymap-prefix (kbd "C-c C-p"))
  (projectile-mode-line '(:eval (projectile-project-name)))
  :config (projectile-global-mode))

(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode 1))

(use-package magit :defer 0.3)

;; (use-package git-commit
;;   :after magit
;;   :hook (git-commit-mode . my/git-commit-auto-fill-everywhere)
;;   :custom (git-commit-summary-max-length 50)
;;   :preface
;;   (defun my/git-commit-auto-fill-everywhere ()
;;     "Ensures that the commit body does not exceed 72 characters."
;;     (setq fill-column 72)
;;     (setq-local comment-auto-fill-only-comments nil)))

(use-package smerge-mode
    :after hydra
    :hook (magit-diff-visit-file . (lambda ()
                                     (when smerge-mode
                                       (hydra-merge/body)))))

(use-package forge)

(use-package git-gutter
   :defer 0.3
   :init (global-git-gutter-mode ))

(use-package git-timemachine
  :defer 1)

(use-package ranger
  :bind ("C-c b" . ranger)
  :custom
  (ranger-preview-file 1))

(use-package editorconfig
  :defer 0.3
  :config (editorconfig-mode 1))

(use-package aggressive-indent
  :hook ((css-mode . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode)
         (js-mode . aggressive-indent-mode)
         (lisp-mode . aggressive-indent-mode))
  :custom (aggressive-indent-comments-too))

(use-package electric-operator
  ;; :hook (python-mode . electric-operator-mode)
  :config (electric-operator-mode 1))

(use-package rainbow-mode
  :hook (prog-mode))

(use-package simple
  :ensure nil
  :hook (before-save . delete-trailing-whitespace))

(use-package hungry-delete
  :defer 0.7
  :config (global-hungry-delete-mode))

(use-package web-mode
  :mode "\\.phtml\\'"
  :mode "\\.volt\\'"
  :mode "\\.html\\'"
  :mode "\\.svelte\\'"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

(use-package emmet-mode
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  )

(use-package alert
  :defer 1
  :custom (alert-default-style 'libnotify))

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

(use-package browse-url
  :ensure nil
  :custom
  (browse-url-browser-function 'browse-url-generic)
  (browse-url-generic-program "qutebrowser"))

(use-package engine-mode
  :defer 3
  :config
  (defengine amazon
    "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s"
    :keybinding "a")

  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g")

  (defengine google-images
    "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s"
    :keybinding "i")

  (defengine google-maps
    "http://maps.google.com/maps?q=%s"
    :keybinding "m"
    :docstring "Mappin' it up.")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")

  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y")

  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w"
    :docstring "Searchin' the wikis.")
  (engine-mode t))

(use-package ibuffer
    :bind ("C-x C-b" . ibuffer))
(use-package ibuffer-vc
  :after ibuffer)
(use-package ibuffer-git
  :after ibuffer)

(when (get-buffer "*scratch*")
(with-current-buffer "*scratch*"
	  (emacs-lock-mode 'kill)))

(when (get-buffer "*dashboard*")
(with-current-buffer "*dashboard*"
	  (emacs-lock-mode 'kill)))

(when (get-buffer "*Backtrace*")
(with-current-buffer "*Backtrace*"
	  (emacs-lock-mode 'kill)))

(when (get-buffer "*Messages*")
(with-current-buffer "*Messages*"
	  (emacs-lock-mode 'kill)))

(use-package calc
  :defer t
  :custom
  (math-additional-units
   '((GiB "1024 * MiB" "Giga Byte")
     (MiB "1024 * KiB" "Mega Byte")
     (KiB "1024 * B" "Kilo Byte")
     (B nil "Byte")
     (Gib "1024 * Mib" "Giga Bit")
     (Mib "1024 * Kib" "Mega Bit")
     (Kib "1024 * b" "Kilo Bit")
     (b "B / 8" "Bit")))
  (math-units-table nil))

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

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "F" 'my-dired-find-file)
     (defun my-dired-find-file (&optional arg)
       "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
       (interactive "P")
       (let* ((fn-list (dired-get-marked-files nil arg)))
         (mapc 'find-file fn-list)))))

(use-package savehist
  :ensure nil
  :custom
  (history-delete-duplicates t)
  ;; (savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-save-minibuffer-history 1)
  :config (savehist-mode 1))

(use-package hydra
  :bind (("C-c I" . hydra-image/body)
         ("C-c L" . hydra-ledger/body)
         ("C-c M" . hydra-merge/body)
         ("C-c T" . hydra-tool/body)
         ("C-c b" . hydra-btoggle/body)
         ("C-c c" . hydra-clock/body)
         ("C-c e" . hydra-erc/body)
         ("C-c f" . hydra-flycheck/body)
         ("C-c g" . hydra-go-to-file/body)
         ("C-c m" . hydra-magit/body)
         ("C-c o" . hydra-org/body)
         ("C-c p" . hydra-projectile/body)
         ("C-c q" . hydra-query/body)
         ("C-c s" . hydra-spelling/body)
         ("C-c t" . hydra-tex/body)
         ("C-c u" . hydra-upload/body)
         ("C-c w" . hydra-windows/body)))

(use-package major-mode-hydra
  :after hydra
  :preface
  (defun with-alltheicon (icon str &optional height v-adjust)
    "Displays an icon from all-the-icon."
    (s-concat (all-the-icons-alltheicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-faicon (icon str &optional height v-adjust)
    "Displays an icon from Font Awesome icon."
    (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-fileicon (icon str &optional height v-adjust)
    "Displays an icon from the Atom File Icons package."
    (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-octicon (icon str &optional height v-adjust)
    "Displays an icon from the GitHub Octicons."
    (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str)))

(pretty-hydra-define hydra-btoggle
  (:hint nil :color amaranth :quit-key "q" :title (with-faicon "toggle-on" "Toggle" 1 -0.05))
  ("Basic"
   (("a" abbrev-mode "abbrev" :toggle t)
    ("h" global-hungry-delete-mode "hungry delete" :toggle t))
   "Coding"
   (("e" electric-operator-mode "electric operator" :toggle t)
    ("F" flyspell-mode "flyspell" :toggle t)
    ("f" flycheck-mode "flycheck" :toggle t)
    ("l" lsp-mode "lsp" :toggle t)
    ("k" subword-mode "CamelCase" :toggle t)
    ("s" smartparens-mode "smartparens" :toggle t))
   "UI"
   (("i" ivy-rich-mode "ivy-rich" :toggle t))))

(pretty-hydra-define hydra-clock
  (:hint nil :color teal :quit-key "q" :title (with-faicon "clock-o" "Clock" 1 -0.05))
  ("Action"
   (("c" org-clock-cancel "cancel")
    ("d" org-clock-display "display")
    ("e" org-clock-modify-effort-estimate "effort")
    ("i" org-clock-in "in")
    ("j" org-clock-goto "jump")
    ("o" org-clock-out "out")
    ("r" org-clock-report "report"))))

(pretty-hydra-define hydra-erc
  (:hint nil :color teal :quit-key "q" :title (with-faicon "comments-o" "ERC" 1 -0.05))
  ("Action"
   (("b" my/erc-browse-last-url "browse last url")
    ("c" my/erc-start-or-switch "connect")
    ("d" erc-quit-server "disconnect")
    ("j" erc-join-channel "join")
    ("n" erc-channel-names "names")
    ("o" my/erc-get-ops "ops")
    ("u" my/erc-count-users "users")
    ("r" my/erc-reset-track-mode "reset track mode"))))

(pretty-hydra-define hydra-flycheck
  (:hint nil :color teal :quit-key "q" :title (with-faicon "plane" "Flycheck" 1 -0.05))
  ("Checker"
   (("?" flycheck-describe-checker "describe")
    ("d" flycheck-disable-checker "disable")
    ("m" flycheck-mode "mode")
    ("s" flycheck-select-checker "select"))
   "Errors"
   (("<" flycheck-previous-error "previous" :color pink)
    (">" flycheck-next-error "next" :color pink)
    ("f" flycheck-buffer "check")
    ("l" flycheck-list-errors "list"))
   "Other"
   (("M" flycheck-manual "manual")
    ("v" flycheck-verify-setup "verify setup"))))

(pretty-hydra-define hydra-go-to-file
  (:hint nil :color teal :quit-key "q" :title (with-faicon "file-text-o" "Go To" 1 -0.05))
  ("Agenda"
   (("ac" (find-file "~/.personal/agenda/contacts.org") "contacts")
    ("af" (find-file "~/.personal/agenda/findmycat.org") "findmycat")
    ("ao" (find-file "~/.personal/agenda/organizer.org") "organizer")
    ("ap" (find-file "~/.personal/agenda/people.org") "people")
    ("ar" (find-file "~/.personal/agenda/routine.org") "routine")
    ("as" (find-file "~/.personal/agenda/school.org") "school"))
   "Config"
   (("ca" (find-file (format "%s/alacritty/alacritty.yml" xdg-config)) "alacritty")
    ("cA" (find-file (format "%s/sh/aliases" xdg-config)) "aliases")
    ("cd" (find-file (format "%s/dunst/dunstrc" xdg-config)))
    ("ce" (find-file "~/.emacs.d/config.org") "emacs")
    ("cE" (find-file (format "%s/sh/environ" xdg-config)) "environ")
    ("ci" (find-file (format "%s/i3/config" xdg-config))"i3")
    ("cn" (find-file (format "%s/neofetch/config.conf" xdg-config)) "neofetch")
    ("cp" (find-file (format "%s/polybar/config" xdg-config)) "polybar")
    ("cq" (find-file (format "%s/qutebrowser/config.py" xdg-config)) "qutebrowser")
    ("cR" (find-file (format "%s/rofi/config.rasi" xdg-config)) "rofi")
    ("cr" (find-file (format "%s/ranger/rc.conf" xdg-config)) "ranger")
    ("cs" (find-file (format "%s/sway/config" xdg-config)) "sway")
    ("ct" (find-file (format "%s/tmux/tmux.conf" xdg-config)) "tmux")
    ("cx" (find-file (format "%s/sh/xdg" xdg-config)) "xdg"))
   "Other"
   (("ob" (find-file "~/.personal/other/books.org") "book")
    ("ol" (find-file "~/.personal/other/learning.org") "learning")
    ("om" (find-file "~/.personal/other/movies.org"))
    ("op" (find-file "~/.personal/other/purchases.org") "purchase")
    ("ou" (find-file "~/.personal/other/usb.org") "usb"))))

(pretty-hydra-define hydra-image
  (:hint nil :color pink :quit-key "q" :title (with-faicon "file-image-o" "Images" 1 -0.05))
  ("Action"
   (("r" image-rotate "rotate")
    ("s" image-save "save" :color teal))
    "Zoom"
    (("-" image-decrease-size "out")
     ("+" image-increase-size "in")
     ("=" image-transform-reset "reset"))))

(pretty-hydra-define hydra-ledger
  (:hint nil :color teal :quit-key "q" :title (with-faicon "usd" "Ledger" 1 -0.05))
  ("Action"
   (("b" leadger-add-transaction "add")
    ("c" ledger-mode-clean-buffer "clear")
    ("i" ledger-copy-transaction-at-point "copy")
    ("s" ledger-delete-current-transaction "delete")
    ("r" ledger-report "report"))))

(pretty-hydra-define hydra-magit
  (:hint nil :color teal :quit-key "q" :title (with-alltheicon "git" "Magit" 1 -0.05))
  ("Action"
   (("b" magit-blame "blame")
    ("c" magit-clone "clone")
    ("i" magit-init "init")
    ("l" magit-log-buffer-file "commit log (current file)")
    ("L" magit-log-current "commit log (project)")
    ("s" magit-status "status"))))

(pretty-hydra-define hydra-merge
  (:hint nil :color pink :quit-key "q" :title (with-alltheicon "git" "Merge" 1 -0.05))
  ("Move"
   (("n" smerge-next "next")
    ("p" smerge-prev "previous"))
   "Keep"
   (("RET" smerge-keep-current "current")
    ("a" smerge-keep-all "all")
    ("b" smerge-keep-base "base")
    ("l" smerge-keep-lower "lower")
    ("u" smerge-keep-upper "upper"))
   "Diff"
   (("<" smerge-diff-base-upper "upper/base")
    ("=" smerge-diff-upper-lower "upper/lower")
    (">" smerge-diff-base-lower "base/lower")
    ("R" smerge-refine "redefine")
    ("E" smerge-ediff "ediff"))
   "Other"
   (("C" smerge-combine-with-next "combine")
    ("r" smerge-resolve "resolve")
    ("k" smerge-kill-current "kill current"))))

(pretty-hydra-define hydra-org
  (:hint nil :color teal :quit-key "q" :title (with-fileicon "org" "Org" 1 -0.05))
  ("Action"
   (("A" my/org-archive-done-tasks "archive")
    ("a" org-agenda "agenda")
    ("c" org-capture "capture")
    ("d" org-decrypt-entry "decrypt")
    ("i" org-insert-link-global "insert-link")
    ("j" my/org-jump "jump-task")
    ("k" org-cut-subtree "cut-subtree")
    ("o" org-open-at-point-global "open-link")
    ("r" org-refile "refile")
    ("s" org-store-link "store-link")
    ("t" org-show-todo-tree "todo-tree"))))

(pretty-hydra-define hydra-projectile
  (:hint nil :color teal :quit-key "q" :title (with-faicon "rocket" "Projectile" 1 -0.05))
  ("Buffers"
   (("b" counsel-projectile-switch-to-buffer "list")
    ("k" projectile-kill-buffers "kill all")
    ("S" projectile-save-project-buffers "save all"))
   "Find"
   (("d" counsel-projectile-find-dir "directory")
    ("D" projectile-dired "root")
    ("f" counsel-projectile-find-file "file")
    ("p" counsel-projectile-switch-project "project"))
   "Other"
   (("i" projectile-invalidate-cache "reset cache"))
   "Search"
   (("r" projectile-replace "replace")
    ("R" projectile-replace-regexp "regexp replace")
    ("s" counsel-rg "search"))))

(pretty-hydra-define hydra-query
  (:hint nil :color teal :quit-key "q" :title (with-faicon "search" "Engine-Mode" 1 -0.05))
  ("Query"
   (("a" engine/search-amazon "amazon")
    ("d" engine/search-duckduckgo "duckduckgo")
    ("g" engine/search-github "github")
    ("i" engine/search-google-images "google images")
    ("m" engine/search-google-maps "google maps")
    ("s" engine/search-stack-overflow "stack overflow")
    ("w" engine/search-wikipedia "wikipedia")
    ("y" engine/search-youtube "youtube"))))

(pretty-hydra-define hydra-spelling
  (:hint nil :color teal :quit-key "q" :title (with-faicon "magic" "Spelling" 1 -0.05))
  ("Checker"
   (("c" langtool-correct-buffer "correction")
    ("C" langtool-check-done "clear")
    ("d" ispell-change-dictionary "dictionary")
    ("l" (message "Current language: %s (%s)" langtool-default-language ispell-current-dictionary) "language")
    ("s" my/switch-language "switch")
    ("w" wiki-summary "wiki"))
   "Errors"
   (("<" flyspell-correct-previous "previous" :color pink)
    (">" flyspell-correct-next "next" :color pink)
    ("f" langtool-check "find"))))

(pretty-hydra-define hydra-tex
  (:hint nil :color teal :quit-key "q" :title (with-fileicon "tex" "LaTeX" 1 -0.05))
  ("Action"
   (("g" reftex-goto-label "goto")
    ("r" reftex-query-replace-document "replace")
    ("s" counsel-rg "search")
    ("t" reftex-toc "table of content"))))

(pretty-hydra-define hydra-tool
  (:hint nil :color teal :quit-key "q" :title (with-faicon "briefcase" "Tool" 1 -0.05))
  ("Network"
   (("c" ipcalc "subnet calculator")
    ("i" ipinfo "ip info"))))

(pretty-hydra-define hydra-typescript
  (:hint nil :color teal :quit-key "q" :title (with-faicon "briefcase" "Tool" 1 -0.05))
  ("Network"
   (("q" nil)
   ("b" tide-jump-back "back")
   ("e" tide-project-errors "errors")
   ("j" tide-jump-to-definition "jump")
   ("r" tide-references "references")
   ("R" tide-restart-server "restart"))))

(pretty-hydra-define hydra-upload
  (:hint nil :color teal :quit-key "q" :title (with-faicon "cloud-upload" "Upload" 1 -0.05))
  ("Action"
   (("b" webpaste-paste-buffe "buffer")
    ("i" imgbb-upload "image")
    ("r" webpaste-paste-region "region"))))

(pretty-hydra-define hydra-windows
  (:hint nil :forein-keys warn :quit-key "q" :title (with-faicon "windows" "Windows" 1 -0.05))
  ("Window"
   (("b" balance-windows "balance")
    ("i" enlarge-window "heighten")
    ("j" shrink-window-horizontally "narrow")
    ("k" shrink-window "lower")
    ("l" enlarge-window-horizontally "widen")
    ("s" switch-window-then-swap-buffer "swap" :color teal))
   "Zoom"
   (("-" text-scale-decrease "out")
    ("+" text-scale-increase "in")
    ("=" (text-scale-increase 0) "reset"))))

(use-package gnuplot
  :ensure-system-package gnuplot
  :defer 2)

(use-package gnuplot-mode
  :after gnuplot
  :mode "\\.gp\\'")

(use-package try :defer 5)

(use-package undo-tree
  :bind ("C--" . undo-tree-redo)
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t))

(use-package wiki-summary
  :defer 1
  :preface
  (defun my/format-summary-in-buffer (summary)
    "Given a summary, sticks it in the *wiki-summary* buffer and displays
     the buffer."
    (let ((buf (generate-new-buffer "*wiki-summary*")))
      (with-current-buffer buf
        (princ summary buf)
        (fill-paragraph)
        (goto-char (point-min))
        (view-mode))
      (pop-to-buffer buf))))

(advice-add 'wiki-summary/format-summary-in-buffer :override #'my/format-summary-in-buffer)

(use-package recentf
  :bind ("C-c r" . recentf-open-files)
  :init (recentf-mode)
  :custom
  (recentf-exclude (list "COMMIT_EDITMSG"
                         "~$"
                         "/scp:"
                         "/ssh:"
                         "/sudo:"
                         "/tmp/"))
  (recentf-max-menu-items 15)
  (recentf-max-saved-items 200)
  :config (run-at-time nil (* 5 60) 'recentf-save-list))

(use-package org-re-reveal
  :after org
  :custom
  (org-reveal-mathjax t)
  (org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/"))
