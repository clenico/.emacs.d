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
 fill-column 72                                   ; Set width for automatic line breaks
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
  (menu-bar-mode 1)                               ; Disable the menu bar
  (scroll-bar-mode 1)                             ; Enable the scroll bar
  (tool-bar-mode -1)                              ; Disable the tool bar
  (tooltip-mode 1))                               ; Enable tooltips
(delete-selection-mode 1)                         ; Enable de deletion of selected text
(toggle-frame-maximized)                          ; Toggle fullscreen by default
(setq visible-bell 1)                             ; Disable the bell on Windows
(setq save-silently 1)                            ; Disable minibuffer messageon saving
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

(global-unset-key "\C-z")
(global-unset-key "\C-x\C-z")

(define-prefix-command 'z-map)
(global-set-key (kbd "C-z") 'z-map) ;; was C-1

(defun backward-paragraph-bracket()
  (interactive)
  (backward-paragraph))
(defun forward-paragraph-bracket()
  (interactive)
  (forward-paragraph))
(global-set-key (kbd "M-[") 'backward-paragraph-bracket)
(global-set-key (kbd "M-]") 'forward-paragraph-bracket)

(use-package goto-last-change
  :bind(("C-s-h" . goto-last-change)
        ("C-s-l" . goto-last-change-reverse)))

(use-package caps-lock
  :bind(("þ" . caps-lock-mode)))

(defun window-half-height ()
  (max 1 (/ (1- (window-height (selected-window))) 2)))

(defun scroll-up-half ()
  (interactive)
  (scroll-up (window-half-height)))

(defun scroll-down-half ()
  (interactive)
  (scroll-down (window-half-height)))

(defun scroll-other-up-half ()
  (interactive)
  (scroll-other-window (window-half-height)))

(defun scroll-other-down-half ()
  (interactive)
  (scroll-other-window-down (window-half-height)))

(global-set-key (kbd "H-o") 'scroll-other-up-half)
(global-set-key (kbd "H-p") 'scroll-other-down-half)

(global-set-key (kbd "C-v") 'scroll-up-half)
(global-set-key (kbd "M-v") 'scroll-down-half)

(use-package move-text
  :bind (("M-p" . move-text-up)
	 ("M-n" . move-text-down))
  :config (move-text-default-bindings))

(use-package avy
  :ensure t
  :bind (("C-z SPC" . avy-goto-char)
         ("C-z l" . avy-goto-line)
         ("C-z w" . avy-goto-word-2))
  :config
  (setq avy-background t))

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(global-set-key (kbd "M-ù") 'goto-match-paren)

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

(use-package windmove
  :bind (("C-c h" . windmove-left)
         ("C-c j" . windmove-down)
         ("C-c k" . windmove-up)
         ("C-c l" . windmove-right)))

(use-package winner
  :defer 2
  :config (winner-mode 1))

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(defun daedreth/kill-inner-word ()
  "Kills the entire word your cursor is in. Equivalent to 'ciw' in vim."
  (interactive)
  (forward-char 1)
  (backward-word)
  (kill-word 1))
(global-set-key (kbd "C-c cw") 'daedreth/kill-inner-word)

(defun daedreth/copy-whole-word ()
  (interactive)
  (save-excursion
    (forward-char 1)
    (backward-word)
    (kill-word 1)
    (yank)))
(global-set-key (kbd "C-c yw") 'daedreth/copy-whole-word)

(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s"))
  (beginning-of-line))
(global-set-key (kbd "C-c yy") 'copy-line)

(global-set-key (kbd "C-c dd") 'kill-whole-line)

(defun kill-all-random-scratch-buffer()
  (interactive)
  (kill-matching-buffers "^scratch-" t t))

(global-set-key (kbd "H-K") 'kill-all-random-scratch-buffer)

(global-set-key (kbd "M-F") 'forward-whitespace)

(defun my/clear ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(global-set-key (kbd "C-c cl") 'my/clear)

(defun xah-toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Always cycle in this order: Init Caps, ALL CAPS, all lower.

URL `http://ergoemacs.org/emacs/modernization_upcase-word.html'
Version 2017-04-19"
  (interactive)
  (let (
        (deactivate-mark nil)
        $p1 $p2)
    (if (use-region-p)
        (setq $p1 (region-beginning)
              $p2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alnum:]-_")
        (setq $p1 (point))
        (skip-chars-forward "[:alnum:]-_")
        (setq $p2 (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region $p1 $p2)
      (put this-command 'state 1))
     ((equal 1  (get this-command 'state))
      (upcase-region $p1 $p2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region $p1 $p2)
      (put this-command 'state 0)))))

(global-set-key (kbd "C-x C-u") 'xah-toggle-letter-case)

(use-package iedit
  :ensure t
:bind (("M-e" . iedit-mode)))

(use-package expand-region
  :bind (("C-+" . er/contract-region)
         ("C-=" . er/expand-region)))

(use-package atomic-chrome
:ensure t
:config (atomic-chrome-start-server))
(setq atomic-chrome-buffer-open-style 'frame)

(use-package sudo-edit
  :ensure t
  :config (sudo-edit-indicator-mode t)
  :bind
  ("C-c su" . sudo-edit))

(use-package zzz-to-char
  :config
  (setq zzz-to-char-reach 800)
  :bind ("M-Z" . zzz-up-to-char))

(use-package ciel
  :bind (("C-c ci" . ciel-ci)
         ("C-c co" . ciel-co)))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("H-j" . mc/mark-next-like-this)
         ("H-k" . mc/mark-previous-like-this)
         ("H-A" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file t))

(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode))

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
  :config (smartparens-global-mode 1)
  :bind("C-c spd" . sp-splice-sexp))

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

;; (use-package lsp-mode
;;   :hook (prog-mode . lsp))

(use-package lsp-mode
  :hook ((c-mode c++-mode java-mode ) . lsp)
  :custom
  (lsp-prefer-flymake nil))

(use-package lsp-ui)
(use-package company-lsp)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

;; (setq py-python-command "python3")
;; (setq python-shell-interpreter "python3")

;; (use-package elpy
;;   :ensure t
;;   :custom (elpy-rpc-backend "jedi")
;;   :config
;;   (advice-add 'python-mode :before 'elpy-enable))

;; (use-package virtualenvwrapper
;;   :ensure t
;;   :config
;;   (venv-initialize-interactive-shells)
;;   (venv-initialize-eshell))

(use-package lsp-python-ms
  :ensure t
  :after projectile
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))  ; or lsp-deferred

(defun my/clear ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(use-package python
  :delight "π "
  :bind (:map python-mode-map
              ("M-[" . python-nav-backward-block)
              ("M-]" . python-nav-forward-block))
  :config
  (setq python-indent-guess-indent-offset t)
  (setq python-indent-guess-indent-offset-verbose nil))

(use-package ccls
  :after projectile
  :ensure-system-package ccls
  :custom
  (ccls-args nil)
  (ccls-executable (executable-find "ccls"))
  (projectile-project-root-files-top-down-recurring
   (append '("compile_commands.json" ".ccls")
           projectile-project-root-files-top-down-recurring))
  :config (push ".ccls-cache" projectile-globally-ignored-directories))

(use-package google-c-style
  :hook ((c-mode c++-mode) . google-set-c-style)
  (c-mode-common . google-make-newline-indent))

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
;; :ensure-system-package (hunspell . "sudo apt-get install  hunspell")
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

    (message "Dictionary switched from %s to %s" current-dictionary new-dictionary)))
)

(use-package lorem-ipsum
  :bind (("C-c v l" . lorem-ipsum-insert-list)
         ("C-c v p" . lorem-ipsum-insert-paragraphs)
         ("C-c v s" . lorem-ipsum-insert-sentences)))

(use-package paradox
  :custom
  (paradox-execute-asynchronously t)
  :config
  (paradox-enable))

(use-package which-key
  :defer 0.2
  :config (which-key-mode)
  :bind(
        ("<f5>" . which-key-show-top-level)
        ))

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
   ("C-c k" . counsel-rg)
   :map org-mode-map
   ("C-c C-f" . counsel-org-goto)))

(use-package swiper
  :after ivy
  :preface
  (defun swiper-region ()
    "If region is selected, `swiper' with the keyword selected in region.
If the region isn't selected, `swiper'."
    (interactive)
    (if (not (use-region-p))
        (swiper)
      (deactivate-mark)
      (swiper (buffer-substring-no-properties
               (region-beginning) (region-end)))))
  :bind (("C-s" . swiper-region)
         :map swiper-map
         ("M-r" . swiper-query-replace)))

(use-package smex)

;; (use-package ivy-pass
;;   :after ivy
;;   :commands ivy-pass)

(use-package pdf-tools
  :defer 1
  :magic ("%PDF" . pdf-view-mode)
  :init (pdf-tools-install :no-query)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (bind-keys :map pdf-view-mode-map
             ("\\" . hydra-pdftools/body)
             ("<s-spc>" .  pdf-view-scroll-down-or-next-page)
             ("g"  . pdf-view-first-page)
             ("G"  . pdf-view-last-page)
             ("l"  . image-forward-hscroll)
             ("h"  . image-backward-hscroll)
             ("j"  . pdf-view-next-page)
             ("k"  . pdf-view-previous-page)
             ("e"  . pdf-view-goto-page)
             ("u"  . pdf-view-revert-buffer)
             ("al" . pdf-annot-list-annotations)
             ("ad" . pdf-annot-delete)
             ("aa" . pdf-annot-attachment-dired)
             ("am" . pdf-annot-add-markup-annotation)
             ("at" . pdf-annot-add-text-annotation)
             ("y"  . pdf-view-kill-ring-save)
             ("i"  . pdf-misc-display-metadata)
             ("s"  . pdf-occur)
             ("b"  . pdf-view-set-slice-from-bounding-box)
             ("r"  . pdf-view-reset-slice)))

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

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if (executable-find "python3") 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

;; (use-package treemacs-magit
;;   :after treemacs magit
;;   :ensure t)

(defun close-all-buffers ()
  "Kill all buffers without regard for their origin."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-M-s-k") 'close-all-buffers)

(use-package popup-kill-ring
  :ensure t
  :bind ("M-y" . popup-kill-ring))

(use-package flycheck
  :defer 2
  :delight
  :init (global-flycheck-mode)
  :custom
  (flycheck-display-errors-delay 1)
  ;; (flycheck-pylintrc "~/.pylintrc")
  ;; (flycheck-python-pylint-executable "/usr/bin/pylint")
  ;; (flycheck-stylelintrc "~/.stylelintrc.json")
  ;; :config
  ;; (flycheck-add-mode 'javascript-eslint 'web-mode)
  ;; (flycheck-add-mode 'typescript-tslint 'web-mode)
  )

;; (use-package flycheck-popup-tip
;;   :after flycheck
;;   :hook global-flycheck-mode . flycheck-popup-tip)

(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree)
  (volatile-highlights-mode t))

(use-package hydra)

(use-package dash)  ;; need dash for major-mode-hydra

(use-package major-mode-hydra
  ;; :after dash hydra
  :hook hydra
  :config
  (setq major-mode-hydra-invisible-quit-key "q")
  :bind
  ("M-SPC" . major-mode-hydra)
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
(major-mode-hydra)

(pretty-hydra-define my/hydra-toggle
  (:hint nil :foreign-keys warn :title "toggle" :quit-key "q")
  ("Basic"
   (("a" abbrev-mode "abbrev" :toggle t)
    ("h" global-hungry-delete-mode "hungry delete" :toggle t)
    )
   "Coding"
   (("e" electric-operator-mode "electric operator" :toggle t)
    ("F" flyspell-mode "flyspell" :toggle t)
    ("f" flycheck-mode "flycheck" :toggle t)
    ("l" lsp-mode "lsp" :toggle t)
    ("s" smartparens-mode "smartparens" :toggle t))
   "UI"
   (("i" ivy-rich-mode "ivy-rich" :toggle t)
   ("T" treemacs "Treemacs")
   )))

(global-set-key (kbd "C-z b") 'my/hydra-toggle/body)

(pretty-hydra-define my/hydra-switch-mode
  (:hint nil :foreign-keys warn :title "switch-mode" :quit-key "q" :color blue)
  ("Programming"
   (("el" emacs-lisp-mode "emacs-lisp")
    ("py" python-mode "python")
    ("P" php-mode "php")
    ("cpp" c++-mode "C++")
    ("cc" func "C")
    ("md" markdown-mode "markdown")
    ("jn" json-mode "JSON")
    ("ltx" TeX-latex-mode "LaTeX")
    ("O" org-mode "Org-mode"))))

(global-set-key (kbd "H-m") 'my/hydra-switch-mode/body)

(pretty-hydra-define my/hydra-go-to-config
  (:hint nil :foreign-keys warn :title "go-to-config" :quit-key "q" :color blue)
  ("Subtitle"
   (("i" (find-file "~/.config/i3/config") "i3-config")
    ("I" (find-file "~/.config/i3status/config") "i3status")
    ("c" (find-file "~/.config/compton/compton.conf") "compton")
    ("z" (find-file "~/.zshrc") "zshrc")
    ("qq" (find-file "~/.config/qutebrowser") "qutebrowser")
    ("qb" (find-file "~/.config/qutebrowser/search_engines.py") "qute engine")
    ("e" (find-file "~/.emacs.d/config.org") "Emacs")
    )))
(global-set-key (kbd "C-z F") 'my/hydra-go-to-config/body)

(pretty-hydra-define my/hydra-go-to-folder
  (:hint nil :foreign-keys warn :title "go-to-folder" :quit-key "q" :color blue)
  ("Subtitle"
   (("t" (counsel-find-file "/tmp/") "Tmp")
    ("d" (counsel-find-file "~/Documents/") "Document")
    ("D" (counsel-find-file "~/Downloads") "Download")
    ("g" (counsel-find-file "~/Google Drive") "Google drive")
    ("p" (counsel-find-file "~/Pictures") "Pictures")
    ("n" (counsel-find-file "~/Google Drive/Notes") "Notes")
    ("O" (counsel-find-file "~/Google Drive/Org") "Org")
    ("T" (counsel-find-file "~/Google Drive/TFE") "TFE")
    ("." (counsel-find-file "~/Dotfiles") "Dotfiles")
    )))
(global-set-key (kbd "C-z f") 'my/hydra-go-to-folder/body)

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
  (projectile-keymap-prefix (kbd "C-c p"))
  (projectile-mode-line '(:eval (projectile-project-name)))
  :config (projectile-global-mode))

(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode 1))

(use-package magit
  :defer 0.3
  :bind(("C-x g". magit-status)))

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

(use-package git-gutter
  :defer 0.3
  :preface
  (defun git-gutter:popup-hunk-other-windows()
    (interactive)
    (git-gutter:popup-hunk)
    (other-window 1))
  :init (global-git-gutter-mode )
  :bind (
         ("C-:" . git-gutter:popup-hunk-other-windows)
         ))

(use-package git-timemachine
  :defer 1
  :bind(
        :map git-timemachine-mode-map
        ("M-SPC" . git-timemachine-help)))

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
         ;; (lisp-mode . aggressive-indent-mode)
)
  :custom (aggressive-indent-comments-too))

(use-package electric-operator
  :hook
  (python-mode . electric-operator-mode)
  (c-mode . electric-operator-mode)
  :config (electric-operator-mode 1))

(use-package rainbow-mode
  :hook (prog-mode))

(use-package simple
  :ensure nil
  :hook (before-save . delete-trailing-whitespace))

(use-package hungry-delete
  :defer 0.7
  :config (global-hungry-delete-mode))

(global-set-key [remap kill-buffer] #'kill-this-buffer)

(use-package prog-fill
  :bind ("M-Q" . prog-fill))

(use-package highlight-numbers
  :config (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package css-mode
  :custom (css-indent-offset 2))

(use-package less-css-mode
  :mode "\\.less\\'")

(use-package scss-mode
  :mode "\\.scss\\'")

(use-package csv-mode
  :bind(
        ("<f6>" . csv-menu)
        ))

(pretty-hydra-define my/hydra-emacs-lisp
  (:hint nil :foreign-keys warn :title "emacs-lisp" :quit-key "q" :color blue)
  ("Eval"
   (("b" eval-buffer "Buffer")
    ("r" eval-region "Region")
    ("l" load-file "Load File")
    ("o" org-babel-load-file "Org load file")
    ("L" (load-file "~/.emacs.d/config.el") "Load config.el")
    ("O" (org-babel-load-file "~/.emacs.d/config.org" ) "Load config.org"))))

(define-key emacs-lisp-mode-map (kbd "M-SPC") 'my/hydra-emacs-lisp/body)

(use-package lua-mode
  :delight "Λ "
  :mode "\\.lua\\'"
  :interpreter ("lua" . lua-mode))

(use-package sql-indent
  :hook sql-mode)

(use-package sqlup-mode
  :hook sql-mode)

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package eldoc
  :delight
  :hook (emacs-lisp-mode . eldoc-mode))

(use-package gradle-mode
  :mode ("\\.java\\'" "\\.gradle\\'")
  :bind (:map gradle-mode-map
              ("C-c C-c" . gradle-build)
              ("C-c C-t" . gradle-test))
  :preface
  (defun my/switch-to-compilation-window ()
    "Switches to the *compilation* buffer after compilation."
    (other-window 1))
  :config
  (advice-add 'gradle-build :after #'my/switch-to-compilation-window)
  (advice-add 'gradle-test :after #'my/switch-to-compilation-window))

;; define files for which to enable cmake mode
(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  :config
  (setq cmake-tab-width 4))
;; enable better syntax highlighting
(use-package cmake-font-lock
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))

;; set all the variables related to rtags, flycheck, company ...
(use-package cmake-ide
  :after projectile
  :hook (c++-mode . my/cmake-ide-find-project)
  :preface
  (defun my/cmake-ide-find-project ()
    "Finds the directory of the project for cmake-ide."
    (with-eval-after-load 'projectile
      (setq cmake-ide-project-dir (projectile-project-root))
      (setq cmake-ide-build-dir (concat cmake-ide-project-dir "build")))
    (setq cmake-ide-compile-command
          (concat "cd " cmake-ide-build-dir " && cmake .. && make"))
    (cmake-ide-load-db))

  (defun my/switch-to-compilation-window ()
    "Switches to the *compilation* buffer after compilation."
    (other-window 1))
  :bind ([remap comment-region] . cmake-ide-compile)
  :init (cmake-ide-setup)
  :config (advice-add 'cmake-ide-compile :after #'my/switch-to-compilation-window))

(use-package make-mode
  :bind(:map makefile-mode-map
             ("M-P" . move-text-up)
             ("M-N" . move-text-down)))

(define-generic-mode 'xmodmap-mode
  '(?!)
  '("add" "clear" "keycode" "keysym" "pointer" "remove")
  nil
  '("[xX]modmap\\(rc\\)?\\'")
  nil
  "Simple mode for xmodmap files.")

(use-package tex
  :ensure auctex
  :bind (:map TeX-mode-map
              ("C-c C-o" . TeX-recenter-output-buffer)
              ("C-c C-l" . TeX-next-error)
              ("M-[" . outline-previous-heading)
              ("M-]" . outline-next-heading))
  :hook ((LaTeX-mode . reftex-mode))
  :preface
  (defun my/switch-to-help-window (&optional ARG REPARSE)
    "Switches to the *TeX Help* buffer after compilation."
    (other-window 1))
  :custom
  (TeX-auto-save t)
  (TeX-byte-compile t)
  (TeX-clean-confirm nil)
  (TeX-master 'dwim)
  (TeX-parse-self t)
  (TeX-PDF-mode t)
  (TeX-source-correlate-mode t)
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  :config
  (advice-add 'TeX-next-error :after #'my/switch-to-help-window)
  (advice-add 'TeX-recenter-output-buffer :after #'my/switch-to-help-window)
  ;; the ":hook" doesn't work for this one... don't ask me why.
  (add-hook 'TeX-after-compilation-finished-functions 'TeX-revert-document-buffer)
  (setq reftex-plug-into-auctex t)
  (setq font-latex-fontify-script nil))

(defvar my-LaTeX-no-autofill-environments
  '("equation" "equation*" "align" "align*")
  "A list of LaTeX environment names in which `auto-fill-mode' should be inhibited.")

(defun my-LaTeX-auto-fill-function ()
  "This function checks whether point is currently inside one of
the LaTeX environments listed in
`my-LaTeX-no-autofill-environments'. If so, it inhibits automatic
filling of the current paragraph."
  (let ((do-auto-fill t)
        (current-environment "")
        (level 0))
    (while (and do-auto-fill (not (string= current-environment "document")))
      (setq level (1+ level)
            current-environment (LaTeX-current-environment level)
            do-auto-fill (not (member current-environment my-LaTeX-no-autofill-environments))))
    (when do-auto-fill
      (do-auto-fill))))

(defun my-LaTeX-setup-auto-fill ()
  "This function turns on auto-fill-mode and sets the function
used to fill a paragraph to `my-LaTeX-auto-fill-function'."
  (auto-fill-mode)
  (setq auto-fill-function 'my-LaTeX-auto-fill-function))

(add-hook 'LaTeX-mode-hook 'my-LaTeX-setup-auto-fill)

  (use-package bibtex
    :after auctex
    :hook
    (bibtex-mode . my/bibtex-fill-column)
    :preface
    (defun my/bibtex-fill-column ()
      "Ensures that each entry does not exceed 120 characters."
      (setq fill-column 120))
    :config
    (setq bibtex-maintain-sorted-entries t))

(use-package company-auctex
  :after (auctex company)
  :config (company-auctex-init))

(use-package company-math :after (auctex company))


(setq-default TeX-engine 'xetex)

(add-hook 'LaTeX-mode-hook
          (lambda()
            (add-to-list 'TeX-command-list
                         '("XeLaTeX" "xelatex  --shell-escape -synctex=1 -interaction=nonstopmode %s "
                           TeX-run-command t t :help "Run xelatex") t)
            (setq TeX-command-default "XeLaTeX")
            (setq TeX-save-query nil)
            (setq TeX-show-compilation t)))

(use-package json-mode
  :delight "J "
  :mode "\\.json\\'"
  :hook (before-save . my/json-mode-before-save-hook)
  :preface
  (defun my/json-mode-before-save-hook ()
    (when (eq major-mode 'json-mode)
      (json-pretty-print-buffer)))

  (defun my/json-array-of-numbers-on-one-line (encode array)
    "Prints the arrays of numbers in one line."
    (let* ((json-encoding-pretty-print
            (and json-encoding-pretty-print
                 (not (loop for x across array always (numberp x)))))
           (json-encoding-separator (if json-encoding-pretty-print "," ", ")))
      (funcall encode array)))
  :config
  (advice-add 'json-encode-array :around #'my/json-array-of-numbers-on-one-line)
  :mode-hydra
  ((:hint nil)
   ("help screen"
    (("f" json-mode-beautifyjson-reformat "Format region/buffer (C-c C-f)")
     ("p" json-mode-show-path "Display path to the object at point (C-c C-p:)")
     ("P" json-mode-kill-path "Copy path to the object to kill ring (C-c P)")
     ("t" json-toggle-boolean "Toggle true falce (C-c C-t)" :color red)
     ("k" json-nullify-sexp "Set current expression to null (C-c C-k)")
     ("i" json-increment-number-at-point "Increment the number at point (C-c C-i)" :color red)
     ("d" json-decrement-number-at-point "Decrement the number at point (C-c C-d)" :color red)))))

(use-package yaml-mode
   :config
   (add-hook 'yaml-mode-hook 'flycheck-mode)
   (add-hook 'yaml-mode-hook 'flyspell-mode))


(use-package flycheck-yamllint)


(use-package highlight-indentation
  :config
  (set-face-background 'highlight-indentation-face "#8B6090")
  (add-hook 'yaml-mode-hook 'highlight-indentation-mode))

(use-package web-mode
  :mode "\\.phtml\\'"
  :mode "\\.volt\\'"
  :mode "\\.html\\'"
  :mode "\\.svelte\\'"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

(use-package yasnippet-snippets
  :after yasnippet
  :config (yasnippet-snippets-initialize))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :bind("H-c" . ivy-yasnippet))

(use-package ivy-yasnippet :after yasnippet
  :bind ("H-e" . ivy-yasnippet))
(use-package react-snippets :after yasnippet)

;; used for creating local snippets or persistant snippets
(use-package auto-yasnippet )

(setq org-src-window-setup 'current-window)
(setq org-startup-folded nil)
(setq org-startup-with-latex-preview t)
(setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")
(setq org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
(setq org-startup-with-inline-images t)
(setq org-latex-toc-command "\\tableofcontents \\clearpage")

(setq org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                             (vm-imap . vm-visit-imap-folder-other-frame)
                             (gnus . org-gnus-no-new-news)
                             (file . find-file)
                             (wl . wl-other-frame)))
(setq org-latex-default-class "report")
(setq org-export-headline-levels 5)

(add-hook 'org-mode 'turn-on-auto-fill)

(defun my/org-use-speed-commands-for-headings-and-lists ()
  "Activates speed commands on list items too."
  (or (and (looking-at org-outline-regexp) (looking-back "^(\**)|^(\s*#)"))
      (save-excursion (and (looking-at (org-item-re)) (looking-back "^[ \t]*")))))

(setq org-use-speed-commands 'my/org-use-speed-commands-for-headings-and-lists)

(setq org-speed-commands-user '(("N" org-speed-move-safe 'org-forward-heading-same-level)
                                ("P" org-speed-move-safe 'org-backward-heading-same-level)
                                ("F" . org-next-block)
                                ("B" . org-previous-block)
                                ("u" org-speed-move-safe 'outline-up-heading)
                                ("j" . org-goto)
                                ("g" org-refile t)
                                ("Outline Visibility")
                                ("c" . org-cycle)
                                ("C" . org-shifttab)
                                (" " . org-display-outline-path)
                                ("s" . org-toggle-narrow-to-subtree)
                                ("k" . org-cut-subtree)
                                ("=" . org-columns)
                                ("Outline Structure Editing")
                                ("U" . org-metaup)
                                ("D" . org-metadown)
                                ("r" . org-metaright)
                                ("l" . org-metaleft)
                                ("R" . org-shiftmetaright)
                                ("L" . org-shiftmetaleft)
                                ("i" progn
                                 (forward-char 1)
                                 (call-interactively 'org-insert-heading-respect-content))
                                ("^" . org-sort)
                                ("w" . org-refile)
                                ("a" . org-archive-subtree-default-with-confirmation)
                                ("@" . org-mark-subtree)
                                ("#" . org-toggle-comment)
                                ("Clock Commands")
                                ("I" . org-clock-in)
                                ("O" . org-clock-out)
                                ("Meta Data Editing")
                                ("t" . org-todo)
                                ("," org-priority)
                                ("0" org-priority 32)
                                ("1" org-priority 65)
                                ("2" org-priority 66)
                                ("3" org-priority 67)
                                (":" . org-set-tags-command)
                                ("e" . org-set-effort)
                                ("E" . org-inc-effort)
                                ("W" lambda
                                 (m)
                                 (interactive "sMinutes before warning: ")
                                 (org-entry-put
                                  (point)
                                  "APPT_WARNTIME" m))
                                ("Agenda Views etc")
                                ("v" . org-agenda)
                                ("/" . org-sparse-tree)
                                ("Misc")
                                ("o" . org-open-at-point)
                                ("?" . org-speed-command-help)
                                ("<" org-agenda-set-restriction-lock 'subtree)
                                (">" org-agenda-remove-restriction-lock))
      )

(use-package toc-org
  :hook (org-mode . toc-org-mode)
  :config
  (toc-org-mode t))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org-faces
  :ensure nil
  :after org
  :custom
  (org-todo-keyword-faces
   '(
     ("TODO" . (:foreground "red" :weight bold))
     ("NEXT" . (:foreground "magenta" :weight bold))
     ("WAITING" . (:foreground "orange" :weight bold))
     ("DONE" . (:foreground "green" :weight bold))
     ("CANCELED" . (:foreground "gray" :weight bold))
     ("BUG" . (:foreground "yellow" :weight bold))
     ("KNOWNCAUSE" . (:foreground "light sea green" :weight bold))
     ("FIXED" . (:foreground "forest green" :weight bold))
     )))

(use-package org-capture
  :ensure nil
  :after org
  :preface
  (defvar my/org-basic-task-template "* TODO %^{Task}
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:
Captured %<%Y-%m-%d %H:%M>" "Template for basic task.")

;;   (defvar my/org-contacts-template "* %(org-contacts-template-name)
;; :PROPERTIES:
;; :ADDRESS: %^{289 Cleveland St. Brooklyn, 11206 NY, USA}
;; :BIRTHDAY: %^{yyyy-mm-dd}
;; :EMAIL: %(org-contacts-template-email)
;; :NOTE: %^{NOTE}
;; :END:" "Template for org-contacts.")

  (defvar my/org-ledger-card-template "%(org-read-date) %^{Payee}
  Expenses:%^{Account}  €%^{Amount}
  Liabilities:CreditsCards:Belfius" "Template for credit card transaction with ledger.")

  (defvar my/org-ledger-cash-template "%(org-read-date) * %^{Payee}
  Expenses:%^{Account}  €%^{Amount}
  Assets:Cash:Wallet" "Template for cash transaction with ledger.")
  :custom
  (org-capture-templates
   `(("B" "Book" checkitem (file+headline "~/Org/other/books.org" "Books")
      "- [ ] %^{Book}"
      :immediate-finish t)
     ("L" "Learning" checkitem (file+headline "~/Org/other/learning.org" "Things")
      "- [ ] %^{Thing}"
      :immediate-finish t)

     ("M" "Movie" checkitem (file+headline "~/Org/other/movies.org" "Movies")
      "- [ ] %^{Movie}"
      :immediate-finish t)

     ("P" "Purchase" checkitem (file+headline "~/Org/other/purchases.org" "Purchases")
      "- [ ] %^{Item}"
      :immediate-finish t)

     ;; ("c" "Contact" entry (file+headline "~/Org/agenda/contacts.org" "Friends"),
     ;;  my/org-contacts-template
     ;;  :empty-lines 1)

     ("l" "Ledger")

     ("lb" "Bank" plain (file ,(format "~/Org/ledger/ledger-%s.dat" (format-time-string "%Y"))),
      my/org-ledger-card-template
      :empty-lines 1
      :immediate-finish t)

     ("lc" "Cash" plain (file ,(format "~/Org/ledger/ledger-%s.dat" (format-time-string "%Y"))),
      my/org-ledger-cash-template
      :empty-lines 1
      :immediate-finish t)

     ("f" "FindMyCat" entry (file+headline "~/Org/agenda/findmycat.org" "Tasks"),
      my/org-basic-task-template
      :empty-lines 1)

     ("p" "People" entry (file+headline "~/Org/agenda/people.org" "Tasks"),
      my/org-basic-task-template
      :empty-lines 1)

     ("s" "School" entry (file+headline "~/Org/agenda/school.org" "Tasks"),
      my/org-basic-task-template
      :empty-lines 1)

     ("t" "Task" entry (file+headline "~/Org/agenda/organizer.org" "Tasks"),
      my/org-basic-task-template
      :empty-lines 1)
     ("j" "Journal" entry (file+datetree "~/Org/Journal/journal.org")
      ""
      :empty-lines 1)
     ))
  :bind
  ("H-c" . org-capture))

(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-agenda-files (quote ("~/Org/agenda/organizer.org")))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@/!)"  "|" "DONE(d)" "CANCELED(c@/!)")
        (sequence "BUG(b@/!)" "KNOWNCAUSE(k@)" "|" "FIXED(f)")))


(setq org-refile-targets (quote ((nil :maxlevel . 9)  ; Targets include this file and any file contributing to the agenda - up to 9 levels deep
                                 (org-agenda-files :maxlevel . 9))))

(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted" "natbib")))

(setq org-latex-pdf-process
      '("xelatex -synctex=1 -shell-escape -interaction nonstopmode %f"
        "bibtex %b"
        "xelatex -synctex=1 -shell-escape -interaction nonstopmode %f"
        "xelatex -synctex=1 -shell-escape -interaction nonstopmode %f")) ;; for multiple passes

(use-package org-ref)

(use-package ob-C :ensure nil :after org)
(use-package ob-css :ensure nil :after org)
(use-package ob-ditaa :ensure nil :after org)
(use-package ob-dot :ensure nil :after org)
(use-package ob-emacs-lisp :ensure nil :after org)
(use-package ob-gnuplot :ensure nil :after org)
(use-package ob-java :ensure nil :after org)
(use-package ob-js :ensure nil :after org)

(use-package ob-latex
  :ensure nil
  :after org
  :custom (org-latex-compiler "xelatex"))

(use-package ob-ledger :ensure nil :after org)
(use-package ob-makefile :ensure nil :after org)
(use-package ob-org :ensure nil :after org)

(use-package ob-python :ensure nil :after org)
(use-package ob-ruby :ensure nil :after org)
(use-package ob-shell :ensure nil :after org)
(use-package ob-sql :ensure nil :after org)

(defun org-copy-blocks ()
  (interactive)
  (let ((code ""))
    (save-restriction
      (org-narrow-to-subtree)
      (org-babel-map-src-blocks nil
        (setq code (concat code (org-no-properties body)))))
    (kill-new code)))

(require 'org-inlinetask)

(defun scimax/org-return (&optional ignore)
  "Add new list item, heading or table row with RET.
A double return on an empty element deletes it.
Use a prefix arg to get regular RET. "
  (interactive "P")
  (if ignore
      (org-return)
    (cond

     ((eq 'line-break (car (org-element-context)))
      (org-return-indent))

     ;; Open links like usual, unless point is at the end of a line.
     ;; and if at beginning of line, just press enter.
     ((or (and (eq 'link (car (org-element-context))) (not (eolp)))
          (bolp))
      (org-return))

     ;; It doesn't make sense to add headings in inline tasks. Thanks Anders
     ;; Johansson!
     ((org-inlinetask-in-task-p)
      (org-return))

     ;; checkboxes too
     ((org-at-item-checkbox-p)
      (org-insert-todo-heading nil))

     ;; lists end with two blank lines, so we need to make sure we are also not
     ;; at the beginning of a line to avoid a loop where a new entry gets
     ;; created with only one blank line.
     ((org-in-item-p)
      (if (save-excursion (beginning-of-line) (org-element-property :contents-begin (org-element-context)))
          (org-insert-heading)
        (beginning-of-line)
        (delete-region (line-beginning-position) (line-end-position))
        (org-return)))

     ;; org-heading
     ((org-at-heading-p)
      (if (not (string= "" (org-element-property :title (org-element-context))))
          (progn (org-end-of-meta-data)
                 (org-insert-heading-respect-content)
                 (outline-show-entry))
        (beginning-of-line)
        (setf (buffer-substring
               (line-beginning-position) (line-end-position)) "")))

     ;; tables
     ((org-at-table-p)
      (if (-any?
           (lambda (x) (not (string= "" x)))
           (nth
            (- (org-table-current-dline) 1)
            (org-table-to-lisp)))
          (org-return)
        ;; empty row
        (beginning-of-line)
        (setf (buffer-substring
               (line-beginning-position) (line-end-position)) "")
        (org-return)))

     ;; fall-through case
     (t
      (org-return)))))


(define-key org-mode-map (kbd "RET") 'scimax/org-return)
(define-key org-mode-map (kbd "C-j") 'org-return)

(use-package alert
  :defer 1
  :custom (alert-default-style 'libnotify))

(use-package company
  :diminish company-mode
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-dabbrev-downcase nil)
  (setq company-begin-commands '(self-insert-command))
  (setq company-idle-delay .1)
  (setq company-minimum-prefix-length 6)
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations 't)
  (bind-keys :map company-active-map
             ("C-d" . company-show-doc-buffer)
             ("C-l" . company-show-location)
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             ("ESC" . company-abort)
             ("TAB" . company-complete))
  :bind("H-SPC" . company-complete))

(use-package company-go)

(use-package company-box
    :after company
    :hook (company-mode . company-box-mode))

(use-package browse-url
  :ensure nil
  :custom
  ;; (browse-url-generic-program "qutebrowser")
  (browse-url-generic-program "firefox")
  (browse-url-browser-function 'browse-url-generic))

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

  (defengine search-code
    "https://searchcode.com/?q=%s"
    :keybinding "c")
  (engine-mode t))

(use-package ibuffer
    :bind ("C-x C-b" . ibuffer))
(use-package ibuffer-vc
  :after ibuffer)
(use-package ibuffer-git
  :after ibuffer)
(use-package ibuffer-projectile
  :after ibuffer
  :preface
  (defun my/ibuffer-projectile ()
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  :hook (ibuffer . my/ibuffer-projectile))

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

(setq dashboard-startup-banner "~/.emacs.d/assert/img/dashLogo.png")
(setq dashboard-banner-logo-title "Live as if you were to die tomorrow. Learn as if you were to live forever")
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(setq dashboard-set-footer nil)

(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))

(setq dired-listing-switches "-alh")

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "F" 'my-dired-find-file)
     (defun my-dired-find-file (&optional arg)
       "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
       (interactive "P")
       (let* ((fn-list (dired-get-marked-files nil arg)))
         (mapc 'find-file fn-list)))))

(use-package dired-subtree :ensure t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

(use-package savehist
  :ensure nil
  :custom
  (history-delete-duplicates t)
  ;; (savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-save-minibuffer-history 1)
  :config (savehist-mode 1))

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

(defun generate-scratch-buffer ()
  "Create and switch to a temporary scratch buffer with a random
     name."
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-")))
(global-set-key (kbd "H-s") 'generate-scratch-buffer)

(defun create-tmp-file ()
  "Prompt name then create a file in /tmp directory"
  (interactive)
  (find-file  (concat "/tmp/" (read-string "/tmp/"))))
(global-set-key (kbd "H-t") 'create-tmp-file)

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "F" 'my-dired-find-file)
     (defun my-dired-find-file (&optional arg)
       "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
       (interactive "P")
       (let* ((fn-list (dired-get-marked-files nil arg)))
         (mapc 'find-file fn-list)))))

(defun copy-path ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))
(global-set-key (kbd "C-c cp") 'copy-path)

(defun what-is-my-ip ()
  (interactive)
  (message "IP: %s"
           (with-current-buffer (url-retrieve-synchronously "https://api.ipify.org")
             (buffer-substring (+ 1 url-http-end-of-headers) (point-max)))))

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

(defun move-file ()
  "Write this file to a new location, and delete the old one."
  (interactive)
  (let ((old-location (buffer-file-name)))
    (call-interactively #'write-file)
    (when old-location
      (delete-file old-location))))

(defun format-function-parameters ()
  "Turn the list of function parameters into multiline."
  (interactive)
  (beginning-of-line)
  (search-forward "(" (line-end-position))
  (newline-and-indent)
  (while (search-forward "," (line-end-position) t)
    (newline-and-indent))
  (end-of-line)
  (c-hungry-delete-forward)
  (insert " ")
  (search-backward ")")
  (newline-and-indent))

(defun config-reload ()
  "Reloads ~/.emacs.d/config.org at runtime"
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

(defun copy-filename ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun create-CMakeLists.txt ()
  (interactive)
  (find-file "CMakeLists.txt"))

(defun bjm/align-whitespace (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))

(defun bjm/align-& (start end)
  "Align columns by ampersand"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)&" 1 1 t))

(setq plantuml-exec-mode "jar")
(setq plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
(setq plantuml-output-type "utxt")

(use-package pkg-info)

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
  (engine-mode t)
  :bind ("H-a" . engine/search-duckduckgo))

(use-package elfeed
  :bind ("C-x w" . elfeed))

(use-package elfeed-org
  :config
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))
  (elfeed-org))

(use-package mermaid-mode
  :config
  (setq mermaid-mmdc-location "/home/niccle27/node_modules/.bin/mmdc"))

(use-package eyebrowse
  :ensure t
  :init
  :config
  (eyebrowse-mode)
  (unbind-key "C-w" eyebrowse-mode-map)
  (eyebrowse-switch-to-window-config-0)
  (find-file "~/.emacs.d/config.org")
  (eyebrowse-switch-to-window-config-9)
  (elfeed)
  (eyebrowse-switch-to-window-config-1)
  :bind(
        ("H-&" . eyebrowse-switch-to-window-config-0)
        ("H-é" . eyebrowse-switch-to-window-config-1)
        ("H-\"" . eyebrowse-switch-to-window-config-2)
        ("H-'" . eyebrowse-switch-to-window-config-3)
        ("H-(" . eyebrowse-switch-to-window-config-4)
        ("H-§" . eyebrowse-switch-to-window-config-5)
        ("H-è" . eyebrowse-switch-to-window-config-6)
        ("H-!" . eyebrowse-switch-to-window-config-7)
        ("H-ç" . eyebrowse-switch-to-window-config-8)
        ("H-à" . eyebrowse-switch-to-window-config-9)
        )
  )
