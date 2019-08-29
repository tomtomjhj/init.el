; custom-set-variables, -face things
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

; requires
(require 'cl) ; lexcial-let
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives
               (cons "melpa" (concat proto "://melpa.org/packages/")) t))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; zenburn
(add-to-list 'custom-theme-load-path "~/.emacs.d/zenburn")
; TODO: search highlight color, more contrast
(setq zenburn-override-colors-alist
        '(("zenburn-fg+1"     . "#FFFFEF")
          ("zenburn-fg"       . "#eaeae0")
          ("zenburn-fg-1"     . "#757565")
          ("zenburn-bg-2"     . "#000000")
          ("zenburn-bg-1"     . "#0f0f0f")
          ("zenburn-bg-05"    . "#1f1f1f")
          ("zenburn-bg"       . "#242424")
          ("zenburn-bg+05"    . "#2f2f2f")
          ("zenburn-bg+1"     . "#3F3F3F")
          ("zenburn-bg+2"     . "#4F4F4F")
          ("zenburn-bg+3"     . "#5F5F5F")
          ))
(load-theme 'zenburn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; evil.
; https://www.emacswiki.org/emacs/Evil#toc6

; why is this option even a thing????????
(setq evil-want-fine-undo t)
(setq evil-want-C-i-jump t)
(setq evil-want-C-u-scroll t)
(setq evil-want-integration t)
(setq evil-want-keybinding nil)

(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

(modify-syntax-entry ?_ "w") ; `_` isn't word char in emacs
(evil-select-search-module 'evil-search-module 'evil-search)
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
(define-key evil-motion-state-map (kbd "C-=") 'global-scale-up)
(define-key evil-motion-state-map (kbd "C-+") 'global-scale-up)
(define-key evil-motion-state-map (kbd "C--") 'global-scale-down)
(define-key evil-motion-state-map (kbd "C-0") 'global-scale-default)
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

; replace C-h with C-H
(define-key global-map (kbd "C-S-H") 'help-command)

; TODO: many other <leader> commands
(add-to-list 'load-path "~/.emacs.d/evil-leader")
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

(evil-leader/set-key
  "e" 'find-file
  "b" 'switch-to-buffer
  "k" 'kill-buffer
  "J" 'evil-join
  "<RET>" 'evil-ex-nohighlight
  )

(add-to-list 'load-path "~/.emacs.d/evil-surround")
(require 'evil-surround)
(global-evil-surround-mode 1)

; esc to quit everything. TODO: not complete?
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


(add-to-list 'load-path "~/.emacs.d/evil-nerd-commenter")
(require 'evil-nerd-commenter)
(evil-leader/set-key
  "c <SPC>" 'evilnc-comment-or-uncomment-lines
  "c c" 'evilnc-copy-and-comment-lines
)

(add-to-list 'load-path "~/.emacs.d/evil-visualstar")
(require 'evil-visualstar)
(global-evil-visualstar-mode)


(add-to-list 'load-path "~/.emacs.d/evil-collection")
(require 'evil-collection)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; undo
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
; Why do I need to load this path again???
; No errors without this load-path, but won't work
(add-to-list 'load-path "~/.emacs.d/evil/lib")
(require 'undo-tree)
(global-undo-tree-mode)
(define-key evil-normal-state-map (kbd "u") 'undo-tree-undo)
(define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)

; https://www.reddit.com/r/emacs/comments/6yzwic/how_emacs_undo_works/
; http://ergoemacs.org/emacs/emacs_undo_cult_problem.html
; what does undo-tree.el do in evil???

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; - PG
(add-hook 'coq-mode-hook #'company-coq-mode)
(evil-define-key 'normal coq-mode-map
  (kbd "M-l") 'proof-goto-point
  (kbd "M-k") 'proof-undo-last-successful-command
  (kbd "M-j") 'proof-assert-next-command-interactive)
(evil-define-key 'insert coq-mode-map
  (kbd "M-l") 'proof-goto-point
  (kbd "M-k") 'proof-undo-last-successful-command
  (kbd "M-j") 'proof-assert-next-command-interactive)
(evil-leader/set-key-for-mode 'coq-mode
  "h" 'company-coq-doc
  "l c" 'coq-LocateConstant
  "l l" 'proof-layout-windows
  "l p" 'proof-prf
  "x" 'proof-shell-exit
  "s" 'proof-find-theorems
  "?" 'coq-Check
  "p" 'coq-Print
  ";" 'pg-insert-last-output-as-comment
  "o" 'company-coq-occur)
(setq-default proof-three-window-mode-policy 'hybrid)
;; TODO: ?? no splash screen
(setq proof-splash-seen t)
; TODO: color theme. this option is somehow copied into custom.el
(custom-set-faces
  '(proof-eager-annotation-face ((t (:background "medium blue"))))
  '(proof-error-face ((t (:background "dark red"))))
  '(proof-warning-face ((t (:background "indianred3")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Neotree
; TODO: disable scroll bars
(setq neo-hidden-regexp-list '("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.o$" "\\.vo$" "\\.v\\.d$" "\\.glob"))
(setq neo-window-fixed-size nil)
(setq neo-window-width 32)
(setq neo-smart-open t)
(setq neo-theme 'nerd)

(add-to-list 'load-path "~/.emacs.d/emacs-neotree")
(require 'neotree)
(evil-collection-init 'neotree)
(global-set-key [f8] 'neotree-toggle)
(define-key evil-normal-state-map (kbd ", n n") 'neotree-toggle)

(menu-bar-mode -1)
(tool-bar-mode -1)
(global-linum-mode 1)

; TODO: unicode input
; TODO: use mouse to adjust window size, split line style
; TODO: tabbar
; TODO: CtrlP, git gutter, MRU
; TODO: fix normal star
; TODO: completion
; TODO: '#file', 'file~', ....
; https://stackoverflow.com/questions/12031830/what-are-file-and-file-and-how-can-i-get-rid-of-them

; https://doingmyprogramming.wordpress.com/2015/12/17/getting-started-with-coq-and-proof-general/
; https://www.williamjbowman.com/blog/2012/07/26/using-evil-for-good/
; https://stackoverflow.com/questions/8483182/evil-mode-best-practice
; https://github.com/ProofGeneral/PG/issues/430
