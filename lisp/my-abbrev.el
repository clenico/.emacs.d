;;-*-coding: utf-8;-*-
(define-abbrev-table 'Buffer-menu-mode-abbrev-table '())

(define-abbrev-table 'Custom-mode-abbrev-table '())

(define-abbrev-table 'Info-edit-mode-abbrev-table '())

(define-abbrev-table 'TeX-error-overview-mode-abbrev-table '())

(define-abbrev-table 'TeX-output-mode-abbrev-table '())

(define-abbrev-table 'TeX-special-mode-abbrev-table '())

(define-abbrev-table 'awk-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'biblio-selection-mode-abbrev-table '())

(define-abbrev-table 'bibtex-mode-abbrev-table '())

(define-abbrev-table 'bookmark-bmenu-mode-abbrev-table '())

(define-abbrev-table 'bookmark-edit-annotation-mode-abbrev-table '())

(define-abbrev-table 'bui-info-mode-abbrev-table '())

(define-abbrev-table 'bui-list-mode-abbrev-table '())

(define-abbrev-table 'c++-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'c-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'calendar-mode-abbrev-table '())

(define-abbrev-table 'ccls-tree-mode-abbrev-table '())

(define-abbrev-table 'change-log-mode-abbrev-table '())

(define-abbrev-table 'checkdoc-output-mode-abbrev-table '())

(define-abbrev-table 'cmake-mode-abbrev-table
  '(
    ("archive" "ARCHIVE" nil 0)
    ("destination" "DESTINATION" nil 0)
    ("library" "LIBRARY" nil 0)
    ("runtime" "RUNTIME" nil 0)
    ("shared" "SHARED" nil 4)
    ("targets" "TARGETS" nil 0)
   ))

(define-abbrev-table 'comint-mode-abbrev-table '())

(define-abbrev-table 'completion-list-mode-abbrev-table '())

(define-abbrev-table 'conf-colon-mode-abbrev-table '())

(define-abbrev-table 'conf-desktop-mode-abbrev-table '())

(define-abbrev-table 'conf-javaprop-mode-abbrev-table '())

(define-abbrev-table 'conf-ppd-mode-abbrev-table '())

(define-abbrev-table 'conf-space-mode-abbrev-table '())

(define-abbrev-table 'conf-toml-mode-abbrev-table '())

(define-abbrev-table 'conf-unix-mode-abbrev-table '())

(define-abbrev-table 'conf-windows-mode-abbrev-table '())

(define-abbrev-table 'conf-xdefaults-mode-abbrev-table '())

(define-abbrev-table 'css-mode-abbrev-table '())

(define-abbrev-table 'dap-ui-breakpoints-ui-list-mode-abbrev-table '())

(define-abbrev-table 'debugger-mode-abbrev-table '())

(define-abbrev-table 'diff-mode-abbrev-table '())

(define-abbrev-table 'display-time-world-mode-abbrev-table '())

(define-abbrev-table 'dockerfile-mode-abbrev-table
  '(
    ("add" "ADD" nil 0)
    ("cmd" "CMD" nil 0)
    ("copy" "COPY" nil 0)
    ("expose" "EXPOSE" nil 0)
    ("from" "FROM" nil 0)
    ("maintainer" "MAINTAINER" nil 0)
    ("run" "RUN" nil 0)
    ("workdir" "WORKDIR" nil 0)
   ))

(define-abbrev-table 'doctex-mode-abbrev-table '())

(define-abbrev-table 'edit-abbrevs-mode-abbrev-table '())

(define-abbrev-table 'elisp-byte-code-mode-abbrev-table '())

(define-abbrev-table 'emacs-lisp-mode-abbrev-table '())

(define-abbrev-table 'epa-info-mode-abbrev-table '())

(define-abbrev-table 'epa-key-list-mode-abbrev-table '())

(define-abbrev-table 'epa-key-mode-abbrev-table '())

(define-abbrev-table 'eshell-mode-abbrev-table '())

(define-abbrev-table 'flycheck-error-list-mode-abbrev-table '())

(define-abbrev-table 'flymake-diagnostics-buffer-mode-abbrev-table '())

(define-abbrev-table 'fundamental-mode-abbrev-table '())

(define-abbrev-table 'gdb-breakpoints-mode-abbrev-table '())

(define-abbrev-table 'gdb-disassembly-mode-abbrev-table '())

(define-abbrev-table 'gdb-frames-mode-abbrev-table '())

(define-abbrev-table 'gdb-locals-mode-abbrev-table '())

(define-abbrev-table 'gdb-memory-mode-abbrev-table '())

(define-abbrev-table 'gdb-registers-mode-abbrev-table '())

(define-abbrev-table 'gdb-script-mode-abbrev-table '())

(define-abbrev-table 'gdb-threads-mode-abbrev-table '())

(define-abbrev-table 'gfm-mode-abbrev-table '())

(define-abbrev-table 'gfm-view-mode-abbrev-table '())

(define-abbrev-table 'git-commit-elisp-text-mode-abbrev-table '())

;; (define-abbrev-table 'global-abbrev-table
;;   '(
;;     ("afaik" "as far as i know" nil 0)
;;     ("ahk" "AutoHotkey" nil 0)
;;     ("ann" "announcement" nil 0)
;;     ("arg" "argument" nil 0)
;;     ("atm" "at the moment" nil 0)
;;     ("autom" "automatic" nil 0)
;;     ("bc" "because" nil 0)
;;     ("bg" "background" nil 0)
;;     ("bt" "between" nil 0)
;;     ("btw" "by the way" nil 0)
;;     ("bu" "•" nil 0)
;;     ("catface" "😸" nil 0)
;;     ("cfg" "context-free grammar" nil 0)
;;     ("cj" "Clojure" nil 0)
;;     ("cnt" "can't" nil 0)
;;     ("cs" "computer science" nil 0)
;;     ("db" "database" nil 0)
;;     ("ddnt" "didn't" nil 0)
;;     ("dfb" "difference between" nil 0)
;;     ("dnt" "don't" nil 0)
;;     ("eq" "==" nil 0)
;;     ("ev" "environment variable" nil 0)
;;     ("gc" "Google Chrome" nil 0)
;;     ("gm" "Google Map" nil 0)
;;     ("gui3" "graphical user interface" nil 0)
;;     ("hr" "--------------------------------------------------" nil 0)
;;     ("ipa" "IP address" nil 0)
;;     ("jvm" "Java Virtual Machine" nil 0)
;;     ("macos" "Mac OS" nil 0)
;;     ("math" "mathematics" nil 0)
;;     ("md" "—" nil 0)
;;     ("msw" "Microsoft Windows" nil 0)
;;     ("oop3" "object oriented programing" nil 0)
;;     ("os3" "operating system" nil 0)
;;     ("r" "return" nil 0)
;;     ("ra" "→" nil 0)
;;     ("rsi" "Repetitive Strain Injury" nil 0)
;;     ("subdir" "sub-directory" nil 0)
;;     ("ty" "thank you" nil 0)
;;     ("ui" "user interface" nil 6)
;;     ("uns" "understand" nil 0)
;;     ("ur" "you are" nil 0)
;;     ("urlemacs" "http://ergoemacs.org/" nil 0)
;;     ("utf8" "-*- coding: utf-8 -*-" nil 0)
;;     ("wd" "web development" nil 0)
;;     ("xaz" "\\([A-Za-z0-9]+\\)" nil 0)
;;    ))

(define-abbrev-table 'gnuplot-comint-mode-abbrev-table '())

(define-abbrev-table 'gnus-group-mode-abbrev-table '())

(define-abbrev-table 'go-mode-abbrev-table
  '(
    ("c" "const x = 3" nil 0)
    ("df" "x := 3" nil 0)
    ("ei" "else if x > 0 { 3 }" nil 0)
    ("else" "else { 3 }" nil 0)
    ("eq" "==" nil 0)
    ("f" "func ff(x int) int {
    return nil
}" nil 0)
    ("for" "for i := 0; i < 4; i++ { i }" nil 0)
    ("fr" "for k, v := range xxx {
▮
    }
" nil 0)
    ("go" "package main
import \"fmt\"
func main() {
        fmt.Println(\"3\")
}" nil 0)
    ("ie" " if err != nil { panic(err) }" nil 0)
    ("if" "if 4 { 3 }" nil 0)
    ("p" "fmt.Printf(\"%v\\n\", hh▮)" nil 0)
    ("pl" "fmt.Println(hh▮)" nil 0)
    ("r" "return" nil 0)
    ("st" "string" nil 0)
    ("v" "var x = 3" nil 0)
   ))

(define-abbrev-table 'godoc-mode-abbrev-table '())

(define-abbrev-table 'grep-mode-abbrev-table '())

(define-abbrev-table 'gud-mode-abbrev-table '())

(define-abbrev-table 'helm-major-mode-abbrev-table '())

(define-abbrev-table 'help-mode-abbrev-table '())

(define-abbrev-table 'html-mode-abbrev-table '())

(define-abbrev-table 'ibuffer-mode-abbrev-table '())

(define-abbrev-table 'idl-mode-abbrev-table '())

(define-abbrev-table 'inferior-python-mode-abbrev-table '())

(define-abbrev-table 'ivy-occur-grep-mode-abbrev-table '())

(define-abbrev-table 'ivy-occur-mode-abbrev-table '())

(define-abbrev-table 'java-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'latex-mode-abbrev-table '())

(define-abbrev-table 'lisp-mode-abbrev-table '())

(define-abbrev-table 'log-edit-mode-abbrev-table '())

(define-abbrev-table 'log4e-mode-abbrev-table '())

(define-abbrev-table 'lsp-browser-mode-abbrev-table '())

(define-abbrev-table 'lsp-log-io-mode-abbrev-table '())

(define-abbrev-table 'lsp-ui-flycheck-list-mode-abbrev-table '())

(define-abbrev-table 'lsp-ui-imenu-mode-abbrev-table '())

(define-abbrev-table 'magit-cherry-mode-abbrev-table '())

(define-abbrev-table 'magit-diff-mode-abbrev-table '())

(define-abbrev-table 'magit-log-mode-abbrev-table '())

(define-abbrev-table 'magit-log-select-mode-abbrev-table '())

(define-abbrev-table 'magit-merge-preview-mode-abbrev-table '())

(define-abbrev-table 'magit-mode-abbrev-table '())

(define-abbrev-table 'magit-process-mode-abbrev-table '())

(define-abbrev-table 'magit-reflog-mode-abbrev-table '())

(define-abbrev-table 'magit-refs-mode-abbrev-table '())

(define-abbrev-table 'magit-repolist-mode-abbrev-table '())

(define-abbrev-table 'magit-revision-mode-abbrev-table '())

(define-abbrev-table 'magit-stash-mode-abbrev-table '())

(define-abbrev-table 'magit-stashes-mode-abbrev-table '())

(define-abbrev-table 'magit-status-mode-abbrev-table '())

(define-abbrev-table 'magit-submodule-list-mode-abbrev-table '())

(define-abbrev-table 'makefile-automake-mode-abbrev-table '())

(define-abbrev-table 'makefile-bsdmake-mode-abbrev-table '())

(define-abbrev-table 'makefile-gmake-mode-abbrev-table '())

(define-abbrev-table 'makefile-imake-mode-abbrev-table '())

(define-abbrev-table 'makefile-makepp-mode-abbrev-table '())

(define-abbrev-table 'makefile-mode-abbrev-table '())

(define-abbrev-table 'markdown-mode-abbrev-table '())

(define-abbrev-table 'markdown-view-mode-abbrev-table '())

(define-abbrev-table 'message-mode-abbrev-table '())

(define-abbrev-table 'messages-buffer-mode-abbrev-table '())

(define-abbrev-table 'objc-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'occur-edit-mode-abbrev-table '())

(define-abbrev-table 'occur-mode-abbrev-table '())

(define-abbrev-table 'org-export-stack-mode-abbrev-table '())

(define-abbrev-table 'org-mode-abbrev-table
  '(
    ("cmakelist" "CMakeLists.txt" nil 3)
   ))

(define-abbrev-table 'outline-mode-abbrev-table '())

(define-abbrev-table 'package-menu-mode-abbrev-table '())

(define-abbrev-table 'paradox-commit-list-mode-abbrev-table '())

(define-abbrev-table 'paradox-menu-mode-abbrev-table '())

(define-abbrev-table 'pdf-annot-list-mode-abbrev-table '())

(define-abbrev-table 'pdf-occur-buffer-mode-abbrev-table '())

(define-abbrev-table 'pdf-outline-buffer-mode-abbrev-table '())

(define-abbrev-table 'pike-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'plain-tex-mode-abbrev-table '())

(define-abbrev-table 'process-menu-mode-abbrev-table '())

(define-abbrev-table 'prog-mode-abbrev-table '())

(define-abbrev-table 'python-mode-abbrev-table '())

(define-abbrev-table 'python-mode-skeleton-abbrev-table
  '(
   ))

(define-abbrev-table 'scss-mode-abbrev-table '())

(define-abbrev-table 'select-tags-table-mode-abbrev-table '())

(define-abbrev-table 'sgml-mode-abbrev-table '())

(define-abbrev-table 'sh-mode-abbrev-table '())

(define-abbrev-table 'shell-mode-abbrev-table '())

(define-abbrev-table 'slitex-mode-abbrev-table '())

(define-abbrev-table 'snippet-mode-abbrev-table '())

(define-abbrev-table 'special-mode-abbrev-table '())

(define-abbrev-table 'speedbar-mode-abbrev-table '())

(define-abbrev-table 'tablist-mode-abbrev-table '())

(define-abbrev-table 'tabulated-list-mode-abbrev-table '())

(define-abbrev-table 'term-mode-abbrev-table '())

(define-abbrev-table 'tex-mode-abbrev-table '())

(define-abbrev-table 'tex-shell-abbrev-table '())

(define-abbrev-table 'text-mode-abbrev-table '())

(define-abbrev-table 'tree-mode-abbrev-table '())

(define-abbrev-table 'url-cookie-mode-abbrev-table '())

(define-abbrev-table 'use-package-statistics-mode-abbrev-table '())

(define-abbrev-table 'vc-bzr-log-view-mode-abbrev-table '())

(define-abbrev-table 'vc-git-log-edit-mode-abbrev-table '())

(define-abbrev-table 'vc-git-log-view-mode-abbrev-table '())

(define-abbrev-table 'vc-git-region-history-mode-abbrev-table '())

(define-abbrev-table 'vc-hg-log-view-mode-abbrev-table '())

(define-abbrev-table 'vc-mtn-log-view-mode-abbrev-table '())

(define-abbrev-table 'vc-svn-log-view-mode-abbrev-table '())

(define-abbrev-table 'xref--xref-buffer-mode-abbrev-table '())

(define-abbrev-table 'yaml-mode-abbrev-table '())
