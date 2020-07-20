; vim: set foldmethod=marker foldlevel=0 nomodeline:
; gc {{{
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)
; }}}

; init {{{
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'cl) ; lexcial-let
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives
               (cons "melpa" (concat proto "://melpa.org/packages/")) t))
(setq package-enable-at-startup nil)
(package-initialize)
(require 'use-package)
(use-package diminish :ensure t :demand t)
; }}}

; themes {{{
; TODO: remove submodule
(add-to-list 'custom-theme-load-path "~/.emacs.d/submodules/zenburn")
(setq zenburn-override-colors-alist
        '(("zenburn-fg+1"     . "#FFFFEF")
          ("zenburn-fg"       . "#eaeae0")
          ("zenburn-fg-1"     . "#757565")
          ("zenburn-bg-2"     . "#000000")
          ("zenburn-bg"       . "#1c1c1c")
          ("zenburn-bg+05"    . "#2f2f2f")
          ("zenburn-bg+1"     . "#3F3F3F")
          ("zenburn-bg+2"     . "#4F4F4F")
          ("zenburn-bg+3"     . "#5F5F5F")
          ))
(load-theme 'zenburn)
; TODO: region should override lazy-highlight
; https://www.reddit.com/r/emacs/comments/345by9/having_the_background_face_for_selection_region/
; NOTE:
; * isearch, lazy-highlight: uses zenburn yellow if `:foreground nil` is not specified
; * line-number: match zenburn-fg-1 zenburn-bg

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'coq-response-mode-hook 'rainbow-delimiters-mode)
(add-hook 'coq-goals-mode-hook 'rainbow-delimiters-mode)
; }}}

; evil {{{
; https://www.emacswiki.org/emacs/Evil#toc6
; settings, loading, hooks {{{
(setq scroll-preserve-screen-position t
      scroll-margin 3 ; vim's `scrolloff`
      scroll-conservatively 101)
(setq evil-want-C-i-jump t)
(setq evil-want-C-u-scroll t) ; TODO: alternative map for original C-u
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-want-Y-yank-to-eol t)
(setq evil-cross-lines t)
; TODO search still doesn't work like vim e.g. \w\+
(setq evil-ex-search-vim-style-regexp t)
;; (setq evil-ex-search-persistent-highlight nil)
; https://github.com/syl20bnr/spacemacs/issues/8853
(setq evil-want-abbrev-expand-on-insert-exit nil)
(setq evil-emacs-state-cursor 'hbar)
(setq evil-vsplit-window-right t)
(setq evil-split-window-below t)
; `_` isn't word char in emacs NOTE: symbol vs word
(setq evil-symbol-word-search t)

(add-to-list 'load-path "~/.emacs.d/submodules/evil")
(require 'evil)
(evil-mode 1)

(add-hook 'after-change-major-mode-hook
          (lambda () (modify-syntax-entry ?_ "w")))
(evil-select-search-module 'evil-search-module 'evil-search) ; evil-ex-search- for gn
; }}}

; stuff that evil should've handled {{{
; TODO: make word motions symbol-wise
; NOTE: Do not (setq evil-want-minibuffer t)
; This option makes the minibuffer work like a normal vim window. ESC exits the
; insert mode of the minibuffer window instead of exiting the minibuffer.
; Solution: bind evil-paste-from-register in minibuffer-local-map.
(define-key minibuffer-local-map (kbd "C-r") 'evil-paste-from-register)
(define-key minibuffer-local-map (kbd "C-w") 'evil-delete-backward-word)
(define-key minibuffer-local-map (kbd "C-<SPC>") 'evil-insert-digraph)
(define-key minibuffer-local-map (kbd "C-@") 'evil-insert-digraph)
; TODO completion in minibuffer

(define-key evil-insert-state-map (kbd "C-u")
  (lambda () (interactive) (evil-delete (point-at-bol) (point))))

(defun my/break-undo (f)
  (progn (evil-end-undo-step) (funcall f) (evil-start-undo-step)))
; }}}

; basic {{{
(defun silence () (interactive))
(define-key evil-motion-state-map (kbd "C-=") 'global-scale-up)
(define-key evil-motion-state-map (kbd "C-+") 'global-scale-up)
(define-key evil-motion-state-map (kbd "C--") 'global-scale-down)
(define-key evil-motion-state-map (kbd "C-0") 'global-scale-default)
(define-key evil-motion-state-map [mouse-6] 'silence)
(define-key evil-motion-state-map [double-mouse-6] 'silence)
(define-key evil-motion-state-map [triple-mouse-6] 'silence)
(define-key evil-motion-state-map [mouse-7] 'silence)
(define-key evil-motion-state-map [double-mouse-7] 'silence)
(define-key evil-motion-state-map [triple-mouse-7] 'silence)
(define-key evil-motion-state-map (kbd "<down>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<up>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "J") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "K") 'evil-previous-visual-line)
(define-key evil-visual-state-map (kbd "J") 'evil-next-visual-line)
(define-key evil-visual-state-map (kbd "K") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "L") 'evil-forward-char)
(define-key evil-normal-state-map (kbd "H") 'evil-backward-char)
(define-key evil-motion-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-motion-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-motion-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-motion-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "<SPC>") 'evil-scroll-down)
(define-key evil-normal-state-map (kbd "C-<SPC>") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "C-@") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "H") 'evil-backward-char)
(define-key evil-visual-state-map (kbd "L") 'evil-forward-char)

(define-key evil-window-map "q" 'my/evil-quit)
(define-key evil-window-map "\C-q" 'my/evil-quit)

(define-key global-map (kbd "C-S-H") 'help-command)
(define-key global-map (kbd "M-h") 'help-command)

; esc and C-q to quit everything.
(defun minibuffer-keyboard-quit ()
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
    (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(define-key evil-normal-state-map (kbd "C-q") 'keyboard-quit)
(define-key evil-visual-state-map (kbd "C-q") 'keyboard-quit)
(define-key evil-insert-state-map (kbd "C-q") 'evil-normal-state)
(define-key minibuffer-local-map (kbd "C-q") 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map (kbd "C-q") 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map (kbd "C-q") 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map (kbd "C-q") 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map (kbd "C-q") 'minibuffer-keyboard-quit)
; }}}

; evil plugins {{{
(add-to-list 'load-path "~/.emacs.d/submodules/evil-leader")
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

(evil-leader/set-key
  "k" 'kill-buffer
  "J" 'evil-join
  "<RET>" 'evil-ex-nohighlight ; TODO: for all windows
  "q" 'my/evil-quit
  "t t" 'eyebrowse-create-window-config
  "`" 'eyebrowse-last-window-config
  "w" 'evil-write
  "f n" (lambda () (interactive) (message (buffer-file-name)))
  "f m" 'delete-trailing-whitespace
  "s w" 'toggle-truncate-lines
  "t i" 'electric-indent-local-mode
  )

(define-key evil-normal-state-map (kbd "g t") 'eyebrowse-next-window-config)
(define-key evil-normal-state-map (kbd "g T") 'eyebrowse-prev-window-config)

(add-to-list 'load-path "~/.emacs.d/submodules/evil-surround")
(require 'evil-surround)
(global-evil-surround-mode 1)

(add-to-list 'load-path "~/.emacs.d/submodules/evil-nerd-commenter")
(require 'evil-nerd-commenter)
; NOTE: M-;
(global-set-key (kbd "M-/") 'evilnc-comment-or-uncomment-lines)
(evil-leader/set-key
  "c <SPC>" 'evilnc-comment-or-uncomment-lines
  "c c" 'evilnc-copy-and-comment-lines)

(add-to-list 'load-path "~/.emacs.d/submodules/evil-visualstar")
(require 'evil-visualstar)
(global-evil-visualstar-mode)
(evil-define-motion my/star-keep-position (count &optional symbol)
  "need this to incorporate symbol search stuff"
  :type exclusive
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     evil-symbol-word-search))
  (evil-ex-start-word-search nil 'forward count symbol)
  (evil-ex-search-previous))
(defun my/visualstar-keep-position ()
  (interactive)
  (when (region-active-p)
    (evil-visualstar/begin-search (region-beginning) (region-end) t)
    (evil-ex-search-previous)))
(define-key evil-motion-state-map "*" 'my/star-keep-position)
(evil-define-key 'visual evil-visualstar-mode-map (kbd "*") 'my/visualstar-keep-position)

(add-to-list 'load-path "~/.emacs.d/submodules/evil-snipe")
(setq evil-snipe-scope 'buffer)
(setq evil-snipe-repeat-scope 'buffer)
(setq evil-snipe-smart-case t)
; TODO: `,`, mappping overrides <leader> in snipe state(?)
(require 'evil-snipe)
(evil-snipe-mode +1)
(evil-snipe-override-mode +1)
(diminish 'evil-snipe-local-mode)

(use-package avy :ensure t)
(add-to-list 'load-path "~/.emacs.d/submodules/evil-easymotion")
(require 'evil-easymotion)
(define-key evil-snipe-parent-transient-map (kbd ";")
  (evilem-create 'evil-snipe-repeat
                 :bind ((evil-snipe-scope 'buffer)
                        (evil-snipe-enable-highlight)
                        (evil-snipe-enable-incremental-highlight))))

(add-to-list 'load-path "~/.emacs.d/submodules/evil-collection")
(require 'evil-collection) ; this requires "annalist" package
(dolist (mode '(company))
  (setq evil-collection-mode-list (delq mode evil-collection-mode-list)))
(evil-collection-init)

(unless (display-graphic-p)
  (add-to-list 'load-path "~/.emacs.d/submodules/evil-terminal-cursor-changer")
  (require 'evil-terminal-cursor-changer)
  ;; https://github.com/7696122/evil-terminal-cursor-changer/issues/19
  (setq evil-motion-state-cursor 'box
        evil-visual-state-cursor 'box
        evil-normal-state-cursor 'box
        evil-insert-state-cursor 'bar)
  (evil-terminal-cursor-changer-activate))

; folding
; NOTE: doesn't support nest folded
(use-package vimish-fold :ensure t)
(add-to-list 'load-path "~/.emacs.d/submodules/evil-vimish-fold")
(setq evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode coq-mode))
(require 'evil-vimish-fold)
(global-evil-vimish-fold-mode 1)
(diminish 'evil-vimish-fold-mode)
; }}}

; misc {{{
; fine-grained word
; TODO: no camel case, more fine-grained control over special characters
(global-syntax-subword-mode)
(define-key evil-insert-state-map (kbd "C-j") 'syntax-subword-right)
(define-key evil-insert-state-map (kbd "C-k") 'syntax-subword-left)
(define-key evil-insert-state-map (kbd "C-<SPC>") 'evil-insert-digraph)
(define-key evil-insert-state-map (kbd "C-@") 'evil-insert-digraph)
(define-key evil-insert-state-map (kbd "C-v") 'yank)
(define-key evil-normal-state-map (kbd "M-o") 'evil-jump-backward)
(define-key evil-normal-state-map (kbd "M-i") 'evil-jump-forward)
(define-key evil-normal-state-map (kbd "M-0")
            (lambda () (interactive) (evil-first-non-blank) (evil-forward-word-begin)))

;}}}
;}}}

; undo {{{
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
(add-to-list 'load-path "~/.emacs.d/submodules/evil/lib")
(require 'undo-tree)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)
(define-key evil-normal-state-map (kbd "u") 'undo-tree-undo)
(define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)
; TODO: undo-tree-undo'ing in the PG locked region may break the proof
; https://github.com/ProofGeneral/PG/issues/430#issuecomment-511967635
; https://www.reddit.com/r/emacs/comments/6yzwic/how_emacs_undo_works/
; http://ergoemacs.org/emacs/emacs_undo_cult_problem.html
; what does undo-tree.el do in evil???
;}}}

; coq {{{
; mapping {{{
; Note: this can't be done with things like dolist because `-map` is variable
; C-c C-. proof-goto-end-of-locked
(evil-define-key 'normal coq-mode-map
  "=" 'my/evil-indent-coq
  ; folding: S-tab, C-c C-/, C-c C-\ (repeat to hide/show all)
  (kbd "C-c C-_") 'company-coq-fold ; C-/ is C-_ in terminal
  (kbd "M-l") 'proof-goto-point
  (kbd "M-k") 'proof-undo-last-successful-command
  (kbd "M-j") 'my/proof-assert-next-command
  (kbd "M-d") 'company-coq-doc
  (kbd "M-,") 'my/coq-Print-point
  (kbd "M-.") 'my/coq-Check-point
  (kbd "M-]") 'company-coq-jump-to-definition)
(evil-define-key 'normal coq-response-mode-map
  (kbd "M-k") 'proof-undo-last-successful-command
  (kbd "M-j") 'my/proof-assert-next-command
  (kbd "M-d") 'company-coq-doc
  (kbd "M-,") 'my/coq-Print-point
  (kbd "M-.") 'my/coq-Check-point
  (kbd "M-]") 'company-coq-jump-to-definition)
(evil-define-key 'normal coq-goals-mode-map
  (kbd "M-k") 'proof-undo-last-successful-command
  (kbd "M-j") 'my/proof-assert-next-command
  (kbd "M-d") 'company-coq-doc
  (kbd "M-,") 'my/coq-Print-point
  (kbd "M-.") 'my/coq-Check-point
  (kbd "M-]") 'company-coq-jump-to-definition)
(evil-define-key 'insert coq-mode-map
  (kbd "TAB") 'smie-indent-line
  (kbd "M-l") (lambda () (interactive) (my/break-undo 'proof-goto-point))
  (kbd "M-k") (lambda () (interactive) (my/break-undo 'proof-undo-last-successful-command))
  (kbd "M-j") (lambda () (interactive) (my/break-undo 'my/proof-assert-next-command))
  ;; Better than comment-indent-new-line as it uses indent-line-function, which
  ;; I set to indent-relative-first-indent-point
  (kbd "RET") 'newline-and-indent
  (kbd "M-i") 'indent-relative)
(dolist (mode '(coq-mode coq-goals-mode coq-response-mode))
  (evil-leader/set-key-for-mode mode
    "l c" 'coq-LocateConstant
    "l l" 'proof-layout-windows
    "l p" 'proof-prf
    "C-c" 'proof-interrupt-process
    "x" 'proof-shell-exit
    "S" 'proof-find-theorems
    "?" 'coq-Check
    "p" 'coq-Print
    "t p" 'my/coq-toggle-printing-level
    ";" 'pg-insert-last-output-as-comment
    "o" 'company-coq-occur))
;}}}

; functions {{{
(defun my/coq-mode-setup ()
  ;; allow some manual indentation
  (setq indent-line-function 'indent-relative-first-indent-point)
  (setq electric-indent-inhibit t)
  (setq evil-shift-width 2)
  (setq comment-style 'multi-line)
  ; TODO: don't add "   " in the empty line
  (setq comment-continue "   ") ; (* ...    style comment
                                ;    ... *) need modified comment-padright
  (diminish 'hs-minor-mode)
  (diminish 'outline-minor-mode)
  (load-file "~/.emacs.d/pg-ssr.el")
  (proof-definvisible my/coq-printing-1 "Set Printing Coercions. Set Printing Implicit")
  (proof-definvisible my/coq-printing-0 "Unset Printing Coercions. Unset Printing Implicit. Unset Printing All."))

(defun comment-padright (str &optional n)
  "Construct a string composed of STR plus `comment-padding'.
It also adds N copies of the last non-whitespace chars of STR.
If STR already contains padding, the corresponding amount is
ignored from `comment-padding'.
N defaults to 0.
If N is `re', a regexp is returned instead, that would match
the string for any N.
This modified version just returns str if it's a whitespace so that
comment-region works properly with whitespace comment-continue."
  (setq n (or n 0))
  (if (and (stringp str) (string-match "\\S-" str))
      ;; Separate the actual string from any leading/trailing padding
      (progn
        (string-match "\\`\\s-*\\(.*?\\)\\s-*\\'" str)
        (let ((s (match-string 1 str))    ;actual string
              (lpad (substring str 0 (match-beginning 1))) ;left padding
              (rpad (concat (substring str (match-end 1)) ;original right padding
                            (substring comment-padding ;additional right padding
                                       (min (- (match-end 0) (match-end 1))
                                            (length comment-padding)))))
              (multi (not (and comment-quote-nested
                               ;; comment-end is a single char
                               (string-match "\\`\\s-*\\S-\\s-*\\'" comment-end)))))
          (if (not (symbolp n))
              (concat lpad s (when multi (make-string n (aref str (1- (match-end 1))))) rpad)
            ;; construct a regexp that would match anything from just S
            ;; to any possible output of this function for any N.
            (concat (mapconcat (lambda (c) (concat (regexp-quote (string c)) "?"))
                               lpad "")    ;padding is not required
                    (regexp-quote s)
                    (when multi "+")    ;the last char of S might be repeated
                    (mapconcat (lambda (c) (concat (regexp-quote (string c)) "?"))
                               rpad ""))))) ;padding is not required
    str))

(defvar my/coq-printing-level 0)
(defun my/coq-toggle-printing-level ()
  (interactive)
  (cond
   ((= my/coq-printing-level 0) (my/coq-printing-1) (setq my/coq-printing-level 1))
   ((= my/coq-printing-level 1) (coq-set-printing-all) (setq my/coq-printing-level 2))
   (t (my/coq-printing-0) (setq my/coq-printing-level 0))))

(defun my/proof-assert-next-command ()
  "Don't go to the next line"
  (interactive)
  (proof-with-script-buffer
    (save-excursion
      (goto-char (proof-queue-or-locked-end))
      (skip-chars-forward " \t\n")
      (proof-assert-until-point))
    (proof-maybe-follow-locked-end)
    (skip-chars-backward "\n")))

(defun my/coq-command-point (cmd)
  (let* ((sb-pos (company-coq-symbol-at-point-with-pos)))
    (unless sb-pos (error "No symbol here"))
    (proof-shell-ready-prover)
    (proof-shell-invisible-command (format (concat cmd) (car sb-pos)))))

(defun my/coq-Check-point ()
  (interactive)
  (my/coq-command-point "Check %s . "))

(defun my/coq-Print-point ()
  (interactive)
  (my/coq-command-point "Print %s . "))

(evil-define-operator my/evil-indent-coq (beg end)
  :move-point nil :type line
  (let (func indent-line-function)
    (setq indent-line-function 'smie-indent-line)
    (evil-indent beg end)
    (setq indent-line-function func)))
; }}}

; hooks, settings {{{
(add-hook 'coq-mode-hook #'company-coq-mode)
(add-hook 'company-coq-mode-hook #'my/coq-mode-setup)

(put 'company-coq-fold 'disabled nil)

; interaction of jump-to-definition and evil jump lists (C-o, C-i)
(evil-add-command-properties #'company-coq-jump-to-definition :jump t)
(evil-add-command-properties #'proof-goto-end-of-locked :jump t)
; TODO: make M-j/k :jump considering proof-end-of-locked-visible-p or error
; don't repeat proof stuff with `.`. TODO: apply to all?
(evil-declare-not-repeat #'my/proof-assert-next-command)
(evil-declare-not-repeat #'proof-undo-last-successful-command)
(evil-declare-not-repeat #'proof-goto-point)

(setq-default proof-three-window-mode-policy 'hybrid)

(setq coq-smie-user-tokens
      '(("∗" . "/\\")
        ("-∗" . "->")
        ("==∗" . "->")
        ("=∗" . "->")))

(setq company-coq-prettify-symbols-alist
      '(
        ("|-" . ?⊢) ("||" . ?‖) ("/\\" . ?∧) ("\\/" . ?∨)
        ("->" . ?→) ("<-" . ?←) ("<->" . ?↔) ("=>" . ?⇒)
        ("<=" . ?≤) (">=" . ?≥) ; ("<>" . ?≠)
        ("True" . ?⊤) ("False" . ?⊥)
        ("fun" . ?λ) ("forall" . ?∀) ("exists" . ?∃)
        ("nat" . ?ℕ) ("Prop" . ?ℙ) ("Real" . ?ℝ) ("bool" . ?𝔹)

        ;; Extra symbols
        (">->" . ?↣)
        ("-->" . ?⟶) ("<--" . ?⟵) ("<-->" . ?⟷)
        ("==>" . ?⟹) ("<==" . ?⟸) ("~~>" . ?⟿) ("<~~" . ?⬳)))
; }}}
; }}}

; etc packages {{{
; TODO: use-package all non-submodule stuff
(use-package neotree :ensure t
  :init
  (setq neo-hidden-regexp-list '("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.o$" "\\.vo$" "\\.v\\.d$" "\\.glob$"))
  (setq neo-window-fixed-size nil)
  (setq neo-window-width 32)
  (setq neo-smart-open t)
  (setq neo-theme 'nerd)
  :config
  (evil-leader/set-key
    "n n" 'neotree-toggle
    "n h" 'neotree-hidden-file-toggle))

; https://leanpub.com/markdown-mode/read
(use-package markdown-mode :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :init
  (setq markdown-command
        (concat
         "pandoc"
         " --from=markdown --to=html"
         " --standalone --mathjax --ascii --highlight-style=kate"))
  (setq markdown-enable-math t)
  (setq markdown-use-pandoc-style-yaml-metadata t))

(use-package editorconfig :ensure t :diminish
  :config (editorconfig-mode 1))

(use-package hl-todo :ensure t
  :init
  (add-hook 'prog-mode-hook #'hl-todo-mode)
  (add-hook 'markdown-mode-hook #'hl-todo-mode))

; completion & snippets with supertab + ultisnips behavior
(use-package company :ensure t :diminish
  :config
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0.2)
  (setq company-transformers
    '(company-sort-by-backend-importance
      company-sort-prefer-same-case-prefix
      company-sort-by-occurrence))
  (company-tng-configure-default)
  (evil-collection-define-key nil 'company-active-map
    ; TODO: c-w doesn't work as expected
    (kbd "C-w") 'evil-delete-backward-word
    (kbd "<tab>") 'company-select-next
    ; '(lambda () (interactive) (company-complete-common-or-cycle -1))
    (kbd "<backtab>") 'company-select-previous-or-abort
    (kbd "<S-iso-lefttab>") 'company-select-previous-or-abort
    (kbd "<S-tab>") 'company-select-previous-or-abort)
  ; https://github.com/company-mode/company-mode/issues/881
  (advice-add 'company-tng--supress-post-completion :override #'ignore)
  :hook (after-init . global-company-mode))

(use-package yasnippet :ensure t :diminish yas-minor-mode
  :config
  (define-key yas-minor-mode-map "\C-l" 'yas-expand)
  (define-key yas-keymap "\C-j" 'yas-next-field-or-maybe-expand)
  (define-key yas-keymap "\C-k" 'yas-prev-field)
  (dolist (keymap (list yas-minor-mode-map yas-keymap))
    (define-key keymap (kbd "TAB") nil)
    (define-key keymap [(tab)] nil)))

; M-i to insert current entry M-o: ivy-dispatching-done
(use-package ivy :ensure t :diminish
  :init (ivy-mode 1)
  :config
  (define-key ivy-minibuffer-map (kbd "C-w") 'evil-delete-backward-word)
  (define-key ivy-minibuffer-map (kbd "C-<SPC>") 'evil-insert-digraph)
  (define-key ivy-minibuffer-map (kbd "C-@") 'evil-insert-digraph)
  (define-key ivy-minibuffer-map (kbd "C-<RET>") 'ivy-done)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  (define-key ivy-minibuffer-map (kbd "C-q") 'minibuffer-keyboard-quit)
  (define-key ivy-switch-buffer-map (kbd "C-k") 'ivy-previous-line)
  ;; ivy-reverse-isearch-map
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order))) ; ivy--regex-fuzzy
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-display-style 'fancy))

(use-package projectile :ensure t
  :config (projectile-mode +1))

(use-package counsel :ensure t
  :init (setq counsel-fzf-cmd "~/.fzf/bin/fzf -f \"%s\"")
  :bind ("M-x" . counsel-M-x))

(use-package swiper :ensure t :after ivy
  :config
  (setq swiper-include-line-number-in-search t)
  :bind (("C-s" . swiper-isearch)
         ("M-s w" . swiper-thing-at-point)))

(define-key evil-normal-state-map (kbd "C-f") 'counsel-fzf)
(evil-leader/set-key
  "e" 'counsel-find-file ; <BS> deletes each node
  "b" 'ivy-switch-buffer
  "h h" 'counsel-recentf
  "G" 'counsel-rg)

(use-package magit :ensure t)
;; TODO: if I use-package evil-magit, it also downloads evil. so smart
(add-to-list 'load-path "~/.emacs.d/submodules/evil-magit")
(require 'evil-magit)

(use-package eyebrowse :ensure t :demand t
  :config
  (setq eyebrowse-mode-line-separator " ")
  (setq eyebrowse-new-workspace t)
  (eyebrowse-mode t)
  (evil-define-command my/evil-quit (&optional force)
    "eyebrowse-aware evil-quit."
    :repeat nil
    (interactive "<!>")
    (condition-case nil
        (delete-window)
      (error
       (cond ((> (length (eyebrowse--get 'window-configs)) 1)
              (eyebrowse-close-window-config))
             ((and (boundp 'server-buffer-clients)
                   (fboundp 'server-edit)
                   (fboundp 'server-buffer-done)
                   server-buffer-clients)
              (if force
                  (server-buffer-done (current-buffer))
                (server-edit)))
             (t
              (condition-case nil
                  (delete-frame)
                (error
                 (if force
                     (kill-emacs)
                   (save-buffers-kill-emacs)))))))))
  (evil-ex-define-cmd "q[uit]" 'my/evil-quit))

;; TODO: separate "+ and "", connect "+ to ssh client clipboard (ForwardX11)
;; https://github.com/syl20bnr/spacemacs/issues/5750#issuecomment-281480406
(use-package xclip :ensure t
  :init (setq xclip-method 'xclip)
  :config (xclip-mode 1))
; }}}

; font {{{
(set-face-attribute 'default nil :height 110)
(dolist (ft (fontset-list))
  ; Main font
  (set-fontset-font ft 'unicode (font-spec :family "Source Code Pro" :foundry "ADBO"))
  ; Fallback font
  (set-fontset-font ft nil (font-spec :name "DejaVu Sans Mono")))
(set-fontset-font t nil (font-spec :name "Symbola"))
(set-fontset-font nil 'hangul (font-spec :family "D2Coding"))

(lexical-let*
  ((default (face-attribute 'default :height))
   (size default))
  (defun global-scale-default ()
    (interactive)
    (setq size default)
    (global-scale-internal size))
  (defun global-scale-up ()
    (interactive)
    (global-scale-internal (incf size 10)))
  (defun global-scale-down ()
    (interactive)
    (global-scale-internal (decf size 10)))
  (defun global-scale-internal (arg)
    (set-face-attribute 'default (selected-frame) :height arg)))
; }}}

; etc settings {{{
(use-package recentf
  :config
  (setq recentf-save-file "~/.emacs.d/recentf")
  (setq recentf-max-saved-items 200)
  :hook (after-init . recentf-mode))
(use-package savehist
  :config
  (setq savehist-file "~/.emacs.d/savehist")
  (savehist-mode 1))
(use-package eldoc :diminish)
(use-package autorevert :diminish auto-revert-mode)

(save-place-mode 1) ; cursor position
(setq display-line-numbers-width-start t) ; TODO: this isn't buffer-local?
(global-display-line-numbers-mode 1)
(electric-pair-mode 1)
(setq column-number-mode t)
(when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
  (tool-bar-mode -1))
(when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
  (menu-bar-mode -1))
(when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
  (scroll-bar-mode -1))
(setq mouse-wheel-scroll-amount '(3))
(setq mouse-wheel-progressive-speed nil)
(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
(add-hook 'text-mode-hook (lambda () (setq show-trailing-whitespace t)))
(setq recenter-redisplay nil); https://emacs.stackexchange.com/q/47091

; http://ergoemacs.org/emacs/emacs_auto_save.html
(setq auto-save-default nil)
(setq create-lockfiles nil)

; http://ergoemacs.org/emacs/emacs_set_backup_into_a_directory.html
(defun my-backup-file-name (fpath)
  (let* ((backupRootDir "~/.emacs.d/backup/")
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath))
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~"))))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setq make-backup-file-name-function 'my-backup-file-name)

(setq evil-digraphs-table-user
      '(((?_ ?l) . ?ₗ)
        ((?^ ?a) . ?ᵃ)
        ((?^ ?c) . ?ᶜ)
        ((?^ ?e) . ?ᵉ)
        ((?^ ?l) . ?ˡ)
        ((?^ ?r) . ?ʳ)
        ((?^ ?x) . ?ˣ)
        ((?O ?X) . ?☠)
        ((?t ?l) . ?⌜)
        ((?t ?r) . ?⌝)
        ((?l ?u) . ?⎡)
        ((?r ?u) . ?⎤)
        ((?m ?t) . ?↦)
        ((?| ?-) . ?⊢)
        ((?- ?|) . ?⊣)
        ((?* ?*) . ?∗)
        ((?o ?o) . ?●)
        ((?O ?O) . ?◯)
        ((?< ?\\) . ?≼)
        ((?t ?p) . ?⊤)
        ((?b ?t) . ?⊥)
        ; (⋅ cdot .P),
        ))
(push '(?* "[*∗]") evil-snipe-aliases)

(setq-default fill-column 80)

(defun my/command-error-function (data context caller)
  "Ignore some siginals"
  (when (not (memq (car data) '(text-read-only beginning-of-buffer end-of-buffer)))
    (command-error-default-function data context caller)))
(setq command-error-function #'my/command-error-function)

(set-language-environment "UTF-8")
; }}}

; examples: https://github.com/SkySkimmer/.emacs.d
; https://www.emacswiki.org/emacs/ELPA#toc5
; TODO: tabbar (emacs 27)
