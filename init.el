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
; }}}

; themes {{{
; NOTE: hardcoded colors for region(block) and lazy-highlight(search)
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
(setq evil-want-C-u-scroll t)
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-want-Y-yank-to-eol t)
(setq evil-cross-lines t)
; TODO search still doesn't work like vim e.g. \w\+
(setq evil-ex-search-vim-style-regexp t)
; https://github.com/syl20bnr/spacemacs/issues/8853
(setq evil-want-abbrev-expand-on-insert-exit nil)

(add-to-list 'load-path "~/.emacs.d/submodules/evil")
(require 'evil)
(evil-mode 1)

; `_` isn't word char in emacs
(add-hook 'after-change-major-mode-hook
          (lambda () (modify-syntax-entry ?_ "w")))
(evil-select-search-module 'evil-search-module 'evil-search)
; }}}

; stuff that evil should've handled {{{
; NOTE: Do not (setq evil-want-minibuffer t)
; This option makes the minibuffer work like a normal vim window. ESC exits the
; insert mode of the minibuffer window instead of exiting the minibuffer.
; Solution: bind evil-paste-from-register in minibuffer-local-map.
(define-key minibuffer-local-map (kbd "C-r") 'evil-paste-from-register)
(define-key minibuffer-local-map (kbd "C-w") 'evil-delete-backward-word)
(define-key minibuffer-local-map (kbd "C-<SPC>") 'evil-insert-digraph)
; TODO completion in minibuffer

(define-key evil-insert-state-map (kbd "C-u")
  (lambda () (interactive) (evil-delete (point-at-bol) (point))))

(defun my/break-undo (f)
  (progn (evil-end-undo-step) (funcall f) (evil-start-undo-step)))
; }}}

; basic {{{
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
(define-key evil-motion-state-map (kbd "j") 'evil-next-line)
(define-key evil-motion-state-map (kbd "k") 'evil-previous-line)
(define-key evil-normal-state-map (kbd "J") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "K") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "L") 'evil-forward-char)
(define-key evil-normal-state-map (kbd "H") 'evil-backward-char)
(define-key evil-motion-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-motion-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-motion-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-motion-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "<SPC>") 'evil-scroll-down)
(define-key evil-normal-state-map (kbd "C-<SPC>") 'evil-scroll-up)

; replace C-h with C-H
(define-key global-map (kbd "C-S-H") 'help-command)

; esc to quit everything.
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
; }}}

; evil plugins {{{
(add-to-list 'load-path "~/.emacs.d/submodules/evil-leader")
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

(evil-leader/set-key
  "e" 'find-file
  "b" 'switch-to-buffer
  "k" 'kill-buffer
  "J" 'evil-join
  "<RET>" 'evil-ex-nohighlight
  "q" 'evil-quit
  "w" 'evil-write
  "f n" (lambda () (interactive) (message (buffer-file-name)))
  "a f" 'delete-trailing-whitespace
  "s w" 'toggle-truncate-lines
  "t i" 'electric-indent-local-mode
  )

(add-to-list 'load-path "~/.emacs.d/submodules/evil-surround")
(require 'evil-surround)
(global-evil-surround-mode 1)

(add-to-list 'load-path "~/.emacs.d/submodules/evil-nerd-commenter")
(require 'evil-nerd-commenter)
(evil-leader/set-key
  "c <SPC>" 'evilnc-comment-or-uncomment-lines
  "c c" 'evilnc-copy-and-comment-lines
)

(add-to-list 'load-path "~/.emacs.d/submodules/evil-visualstar")
(require 'evil-visualstar)
(global-evil-visualstar-mode)
(defun my/star-keep-position ()
  (interactive)
  (evil-ex-search-word-forward)
  (evil-ex-search-previous)) ; use (evil-search-previous) if not using evil-search
(defun my/visualstar-keep-position ()
  (interactive)
  (when (region-active-p)
    (evil-visualstar/begin-search (region-beginning) (region-end) t)
    (evil-ex-search-previous)))
(evil-global-set-key 'normal (kbd "*") 'my/star-keep-position)
(evil-define-key 'visual evil-visualstar-mode-map (kbd "*") 'my/visualstar-keep-position)

(add-to-list 'load-path "~/.emacs.d/submodules/evil-snipe")
(setq evil-snipe-scope 'whole-buffer)
(setq evil-snipe-repeat-scope 'whole-buffer)
(setq evil-snipe-smart-case t)
; TODO: `,`, mappping overrides <leader> in snipe state(?)
; TODO: direction
(require 'evil-snipe)
(evil-snipe-mode +1)
(evil-snipe-override-mode +1)

(add-to-list 'load-path "~/.emacs.d/submodules/evil-collection")
(require 'evil-collection) ; this requires "annalist" package
(dolist (mode '(company))
  (setq evil-collection-mode-list (delq mode evil-collection-mode-list)))
(evil-collection-init)

(evil-collection-define-key nil 'company-active-map
  (kbd "C-w") 'evil-delete-backward-word
  (kbd "<tab>") 'company-select-next-or-abort
  (kbd "<backtab>") 'company-select-previous-or-abort
  (kbd "<S-iso-lefttab>") 'company-select-previous-or-abort
  (kbd "<S-tab>") 'company-select-previous-or-abort)
; }}}

; misc {{{
; fine-grained word
; TODO: no camel case, more fine-grained control over special characters
(global-syntax-subword-mode)
(define-key evil-insert-state-map (kbd "C-j") 'syntax-subword-right)
(define-key evil-insert-state-map (kbd "C-k") 'syntax-subword-left)
(define-key evil-insert-state-map (kbd "C-<SPC>") 'evil-insert-digraph)
(define-key evil-normal-state-map (kbd "M-o") 'evil-jump-backward)
(define-key evil-normal-state-map (kbd "M-i") 'evil-jump-forward)

;}}}
;}}}

; Neotree {{{
(setq neo-hidden-regexp-list '("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.o$" "\\.vo$" "\\.v\\.d$" "\\.glob$"))
(setq neo-window-fixed-size nil)
(setq neo-window-width 32)
(setq neo-smart-open t)
(setq neo-theme 'nerd)

(add-to-list 'load-path "~/.emacs.d/submodules/emacs-neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(evil-leader/set-key
  "n n" 'neotree-toggle)
; }}}

; undo {{{
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
(add-to-list 'load-path "~/.emacs.d/submodules/evil/lib")
(require 'undo-tree)
(global-undo-tree-mode)
(define-key evil-normal-state-map (kbd "u") 'undo-tree-undo)
(define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)
; TODO: undo-tree-undo'ing in proved(?) part may break something.
; https://github.com/ProofGeneral/PG/issues/430
; https://www.reddit.com/r/emacs/comments/6yzwic/how_emacs_undo_works/
; http://ergoemacs.org/emacs/emacs_undo_cult_problem.html
; what does undo-tree.el do in evil???
;}}}

; coq {{{
; https://github.com/cpitclaudel/company-coq/blob/master/company-coq.el#L3278
; Note: use <menu> for the definition of a symbol in the goal/response window
; Note: this can't be done with things like dolist because `-map` is variable
(evil-define-key 'normal coq-mode-map
  (kbd "M-l") 'proof-goto-point
  (kbd "M-k") 'proof-undo-last-successful-command
  (kbd "M-j") 'my/proof-assert-next-command
  (kbd "M-d") 'company-coq-doc
  (kbd "M-.") 'my/coq-Print-point
  (kbd "M-[") 'my/coq-Check-point
  (kbd "M-]") 'company-coq-jump-to-definition)
(evil-define-key 'normal coq-response-mode-map
  (kbd "M-k") 'proof-undo-last-successful-command
  (kbd "M-j") 'my/proof-assert-next-command
  (kbd "M-d") 'company-coq-doc
  (kbd "M-.") 'my/coq-Print-point
  (kbd "M-[") 'my/coq-Check-point
  (kbd "M-]") 'company-coq-jump-to-definition)
(evil-define-key 'normal coq-goals-mode-map
  (kbd "M-k") 'proof-undo-last-successful-command
  (kbd "M-j") 'my/proof-assert-next-command
  (kbd "M-d") 'company-coq-doc
  (kbd "M-.") 'my/coq-Print-point
  (kbd "M-[") 'my/coq-Check-point
  (kbd "M-]") 'company-coq-jump-to-definition)
(evil-define-key 'insert coq-mode-map
  (kbd "M-l") (lambda () (interactive) (my/break-undo 'proof-goto-point))
  (kbd "M-k") (lambda () (interactive) (my/break-undo 'proof-undo-last-successful-command))
  (kbd "M-j") (lambda () (interactive) (my/break-undo 'my/proof-assert-next-command))
  ; this was mapped to 'company-coq-features/code-folding-toggle-block
  (kbd "<backtab>") 'yas-prev-field
  (kbd "<S-iso-lefttab>") 'yas-prev-field
  (kbd "<S-tab>") 'yas-prev-field
  )
(global-unset-key (kbd "M-h"))
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
    ";" 'pg-insert-last-output-as-comment
    "o" 'company-coq-occur))

; etc  {{{
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

(setq proof-splash-enable nil)
(add-hook 'coq-mode-hook #'company-coq-mode)

; TODO: C-c C-/ folding also hides empty lines for no good reason
(put 'company-coq-fold 'disabled nil)

; interaction of jump-to-definition and evil jump lists (C-o, C-i)
(evil-add-command-properties #'company-coq-jump-to-definition :jump t)

(setq-default proof-three-window-mode-policy 'hybrid)
; }}}
; }}}

; etc packages {{{
; https://leanpub.com/markdown-mode/read
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :init
  (setq markdown-command
        (concat
         "pandoc"
         " --from=markdown --to=html"
         " --standalone --mathjax --ascii --highlight-style=kate"))
  (setq markdown-enable-math t)
  (setq markdown-use-pandoc-style-yaml-metadata t))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package hl-todo
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'hl-todo-mode))
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
; }}}

; etc settings {{{
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
      '(((?l ?s) . ?\x2097) ; ₗ subsript l
        ((?u ?l) . ?\x231c) ; ⌜ ulcorner
        ((?u ?r) . ?\x231d) ; ⌝ urcorner
        ((?m ?t) . ?\x21A6) ; ↦ mapsto
        ((?| ?-) . ?\x22A2) ; ⊢ vdash
        ((?- ?|) . ?\x22A3) ; ⊣ dashv
        ((?i ?n) . ?\x2208) ; ∈ in
        ((?* ?*) . ?\x2217) ; ∗ ast
        ((?t ?r) . ?\x25b7) ; ▷ Tr
        ((?o ?s) . ?\x25a1) ; □ OS
        ((?o ?o) . ?\x25cf) ; ● (0M)
        ((?O ?O) . ?\x25ef) ; ◯ (cf. ○ 0m)
        ((?< ?\\) . ?\x227c) ; ≼
        ; (⋅ cdot .P),
        ))

(setq-default fill-column 80)
; }}}

; TODO: use-package for language-specific stuff? `:command` looks good
; examples: https://github.com/SkySkimmer/.emacs.d
; https://www.emacswiki.org/emacs/ELPA#toc5
; TODO: remove submodules except zenburn?
; TODO: tabbar, CtrlP/fzf-like things, git gutter, MRU
; https://www.emacswiki.org/emacs/RecentFiles
; - https://github.com/bling/fzf.el
; - helm?
; https://www.emacswiki.org/emacs/RecentFiles
; TODO: general keyword completion
; - https://www.emacswiki.org/emacs/TabCompletion
; TODO evil jump list is severely broken
; - https://github.com/emacs-evil/evil/issues/1138
; TODO remove useless prompts like 'end of buffer', 'Text is read only', ...
