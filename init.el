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
(setq evil-cross-lines t)
(setq evil-want-minibuffer t)
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
(define-key evil-motion-state-map (kbd "<SPC>") 'evil-scroll-down)
(define-key evil-motion-state-map (kbd "C-<SPC>") 'evil-scroll-up)

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
; (setq evil-snipe-override-evil-repeat-keys nil)
(require 'evil-snipe)
(evil-snipe-mode +1)
(evil-snipe-override-mode +1)

; NOTE: DO NOT UPDATE THIS SUBMODULE. Use 0.0.2.
(add-to-list 'load-path "~/.emacs.d/submodules/evil-collection")
(require 'evil-collection)
(evil-collection-init)
; }}}
; }}}

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
(setq proof-splash-enable nil)
(add-hook 'coq-mode-hook #'company-coq-mode)
; TODO: more keybindings from company-coq
; https://github.com/cpitclaudel/company-coq/blob/master/company-coq.el#L3278
(evil-define-key 'normal coq-mode-map
  (kbd "M-l") 'proof-goto-point
  (kbd "M-k") 'proof-undo-last-successful-command
  (kbd "M-j") 'proof-assert-next-command-interactive
  (kbd "M-[") 'company-coq-doc
  (kbd "M-]") 'company-coq-jump-to-definition
  )
(evil-define-key 'insert coq-mode-map
  (kbd "M-l") 'proof-goto-point
  (kbd "M-k") 'proof-undo-last-successful-command
  (kbd "M-j") 'proof-assert-next-command-interactive)
(evil-leader/set-key-for-mode 'coq-mode
  "l c" 'coq-LocateConstant
  "l l" 'proof-layout-windows
  "l p" 'proof-prf
  "C-c" 'proof-interrupt-process
  "x" 'proof-shell-exit
  "s" 'proof-find-theorems
  "?" 'coq-Check
  "p" 'coq-Print
  ";" 'pg-insert-last-output-as-comment
  "o" 'company-coq-occur)
; interaction of jump-to-definition and evil jump lists (C-o, C-i)
(evil-add-command-properties #'company-coq-jump-to-definition :jump t)

(setq-default proof-three-window-mode-policy 'hybrid)
(custom-set-faces
  '(proof-eager-annotation-face ((t (:background "medium blue"))))
  '(proof-error-face ((t (:background "dark red"))))
  '(proof-warning-face ((t (:background "indianred3")))))
; }}}

; markdown https://leanpub.com/markdown-mode/read {{{
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
; }}}

; etc etc {{{
(save-place-mode 1) ; cursor position
(global-linum-mode 1)
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

; http://ergoemacs.org/emacs/emacs_auto_save.html
(setq auto-save-default nil)
(setq create-lockfiles nil)

; http://ergoemacs.org/emacs/emacs_set_backup_into_a_directory.html
(defun my-backup-file-name (fpath)
  (let* ((backupRootDir "~/.emacs.d/backup/")
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath))
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~"))))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath
  )
)
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
        ((?t ?r) . ?\x25b7) ; ▷
        ((?o ?s) . ?\x25a1) ; □
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
