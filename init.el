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

; TODO: persistent undo, which doesn't seem to work
; (setq undo-tree-auto-save-history t)
; (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; evil.
; https://www.emacswiki.org/emacs/Evil#toc6

; why is this option even a thing????????
(setq evil-want-C-i-jump t)
(setq evil-want-C-u-scroll t)

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

; https://doingmyprogramming.wordpress.com/2015/12/17/getting-started-with-coq-and-proof-general/
; https://www.williamjbowman.com/blog/2012/07/26/using-evil-for-good/
; https://stackoverflow.com/questions/8483182/evil-mode-best-practice
; https://github.com/ProofGeneral/PG/issues/430

; - PG
(add-hook 'coq-mode-hook #'company-coq-mode)
; (add-hook 'coq-mode-hook 'undo-tree-mode)
; (define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)
; (define-key evil-normal-state-map (kbd "u") 'undo-tree-undo)
; (evil-define-key 'normal coq-mode-map
;  (kbd "u") 'undo-tree-undo
;  (kbd "C-r") 'undo-tree-redo)
; TODO: PG keymaps
; TODO: unicode conversion/input

; - tabbar-mode
; - things from .spacemacs
;   - undo-tree mapping of coq ----> plain evil undo is _broken_ too.

; https://www.reddit.com/r/emacs/comments/6yzwic/how_emacs_undo_works/
; TODO: what does undo-tree.el do in evil/???

; TODO: Remove gui buttons
; TODO: something similar to NerdTree, CtrlP, git gutter, MRU
; TODO: evil Commentary
; TODO: completion
; TODO: my <leader> commands
