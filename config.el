(defun copy-filename ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

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

(use-package caps-lock
  :bind
  ("M-A" . caps-lock-mode))

(use-package evil
  :init
  (evil-mode 1)
  (setq evil-want-C-i-jump nil);disable the tab binding for evil-mode
)

(use-package evil-escape
  :after evil
  :init
  (evil-escape-mode 1))

(use-package ace-jump-mode
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode))

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

(use-package iedit
  :ensure t
:bind (("M-E" . iedit-mode)))

(use-package expand-region
  :bind (("C-+" . er/contract-region)
         ("C-=" . er/expand-region)))

;; (when (file-exists-p custom-file)
    ;; (load custom-file t))
(setq custom-file "~/.emacs.d/custom.el")

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

(use-package color
  :after org
  :config
  (if (display-graphic-p)
      (set-face-attribute 'org-block nil :background
                          (color-darken-name
                           (face-attribute 'default :background) 10))))

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

(use-package editorconfig
  :defer 0.3
  :config (editorconfig-mode 1))

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

(with-current-buffer "*scratch*"
      (emacs-lock-mode 'kill))
;; (with-current-buffer "*dashboard*"
      ;; (emacs-lock-mode 'kill))
;; (with-current-buffer "*Backtrace*"
;;       (emacs-lock-mode 'kill))
(with-current-buffer "*Messages*"
      (emacs-lock-mode 'kill))

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
