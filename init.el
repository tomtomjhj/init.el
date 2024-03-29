; -*- lexical-binding: t; -*-
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

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives
               (cons "melpa" (concat proto "://melpa.org/packages/")) t))
(setq package-enable-at-startup nil)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(use-package diminish :ensure t :demand t)
; }}}

; themes {{{
(use-package zenburn-theme :ensure t
  :init
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
  :config
  (load-theme 'zenburn t))
; TODO: region should override lazy-highlight
; https://www.reddit.com/r/emacs/comments/345by9/having_the_background_face_for_selection_region/
; NOTE:
; * isearch, lazy-highlight: uses zenburn yellow if `:foreground nil` is not specified
; * line-number: match zenburn-fg-1 zenburn-bg

(use-package rainbow-delimiters :ensure t)
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
(setq evil-want-C-u-scroll t) ; use "<SPC> u" for universal-argument
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
(setq evil-undo-system 'undo-tree)

(add-to-list 'load-path "~/.emacs.d/submodules/evil")
(require 'evil)
(evil-mode 1)

; NOTE: <leader> is completely broken https://github.com/emacs-evil/evil/issues/1383 just explicitly map ,
; (evil-set-leader nil (kbd "<SPC>"))

(add-hook 'after-change-major-mode-hook
          (lambda () (modify-syntax-entry ?_ "w")))
; TODO: search pattern is not global?? (but the highlight is global)
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
; cmap = evil-ex-completion-map?
(define-key evil-ex-completion-map (kbd "C-<SPC>") 'evil-insert-digraph)
(define-key evil-ex-completion-map (kbd "C-@") 'evil-insert-digraph)
; TODO completion in minibuffer

(define-key evil-insert-state-map (kbd "C-u")
  (lambda () (interactive) (evil-delete (point-at-bol) (point))))
(evil-define-key 'normal 'global (kbd "<SPC> u") 'universal-argument)

(defun my/break-undo (f)
  (progn (evil-end-undo-step) (call-interactively f) (evil-start-undo-step)))
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
(define-key evil-normal-state-map (kbd "C-<SPC>") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "C-@") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "H") 'evil-backward-char)
(define-key evil-visual-state-map (kbd "L") 'evil-forward-char)

(define-key evil-window-map "q" 'my/evil-quit)
(define-key evil-window-map "\C-q" 'my/evil-quit)

(define-key global-map (kbd "C-S-H") 'help-command)
(define-key global-map (kbd "M-h") 'help-command)

; TODO: evil-join (= join-line) doesn't insert space when joining line that
; start with closing paren e.g. .\n}. This is a feature.
(evil-define-key 'normal 'global
  (kbd "<SPC> k") 'kill-buffer
  (kbd "<SPC> J") 'evil-join
  (kbd "<SPC> <RET>") 'evil-ex-nohighlight
  (kbd "<SPC> q") 'my/evil-quit
  (kbd "<SPC> t t") 'eyebrowse-create-window-config
  (kbd "<SPC> `") 'eyebrowse-last-window-config
  (kbd "<SPC> w") 'evil-write
  (kbd "<SPC> f n") (lambda () (interactive) (message (buffer-file-name)))
  (kbd "<SPC> f m") 'delete-trailing-whitespace
  (kbd "<SPC> s w") 'toggle-truncate-lines
  (kbd "<SPC> t i") 'electric-indent-local-mode
  (kbd "<SPC> i c") 'my/toggle-evil-ex-search-case)
(evil-define-key 'visual 'global
  (kbd "<SPC> J") 'evil-join)

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
(define-key evil-ex-completion-map (kbd "C-q") 'minibuffer-keyboard-quit) ; https://emacs.stackexchange.com/a/14165
; }}}

; evil plugins {{{
(add-to-list 'load-path "~/.emacs.d/submodules/goto-chg")
(require 'goto-chg)

(define-key evil-normal-state-map (kbd "g t") 'eyebrowse-next-window-config)
(define-key evil-normal-state-map (kbd "g T") 'eyebrowse-prev-window-config)

(add-to-list 'load-path "~/.emacs.d/submodules/evil-surround")
(require 'evil-surround)
(global-evil-surround-mode 1)

(setq-default comment-column 0)
(add-to-list 'load-path "~/.emacs.d/submodules/evil-nerd-commenter")
(require 'evil-nerd-commenter)
; TODO: comment-dwim sometimes doesn't do what I mean, which is to simply
; insert the comment right at the point. It does weird things like alignment
; and moving cursor to the eol.
; ... (insert "(* "), (insert " *)")
(evil-define-key 'insert 'global (kbd "M-/") (lambda () (interactive) (my/break-undo 'comment-dwim)))
; TODO: evilnc text object doesn't understand coqdoc comment (** *) because comment-start includes a space..
(evil-define-key '(normal visual) 'global
  (kbd "<SPC> c <SPC>") 'evilnc-comment-or-uncomment-lines
  (kbd "<SPC> c c") 'evilnc-copy-and-comment-lines)
(define-key evil-inner-text-objects-map evilnc-comment-text-object 'evilnc-inner-commenter)
(define-key evil-outer-text-objects-map evilnc-comment-text-object 'evilnc-outer-commenter)

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
(setq evil-snipe-repeat-keys nil)
(setq evil-snipe-scope 'buffer)
(setq evil-snipe-repeat-scope 'buffer)
(setq evil-snipe-smart-case t)
(setq evil-snipe-use-vim-sneak-bindings t)
(require 'evil-snipe)
(evil-snipe-mode +1)
(evil-snipe-override-mode +1)
(diminish 'evil-snipe-local-mode)
; Use "," as a secondary leader, and use "M-;" for reverse repeat only for motion states.
; In other states, keep the default mapping M-; to comment-dwim.
(evil-define-key 'motion evil-snipe-override-local-mode-map
  (kbd ",") nil
  (kbd "M-;") 'evil-snipe-repeat-reverse)
(define-key evil-snipe-parent-transient-map
  (kbd ",") nil)

(use-package avy :ensure t)
(add-to-list 'load-path "~/.emacs.d/submodules/evil-easymotion")
(require 'evil-easymotion)
; TODO: enter label mode automatically, and make "," and "M-;" keep working
(define-key evil-snipe-parent-transient-map (kbd "<SPC> ;")
  (evilem-create 'evil-snipe-repeat
                 :bind ((evil-snipe-scope 'buffer)
                        (evil-snipe-enable-highlight)
                        (evil-snipe-enable-incremental-highlight))))
(define-key evil-snipe-parent-transient-map (kbd "<SPC> M-;")
  (evilem-create 'evil-snipe-repeat-reverse
                 :bind ((evil-snipe-scope 'buffer)
                        (evil-snipe-enable-highlight)
                        (evil-snipe-enable-incremental-highlight))))

(setq evil-collection-want-unimpaired-p nil)
(add-to-list 'load-path "~/.emacs.d/submodules/evil-collection")
(use-package annalist :ensure t) ; evil-collection dependency
(require 'evil-collection)
; Don't use the evil-collection's bindings for these modes
(dolist (mode '(outline))
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

; Understands `evil-set-jump` but doesn't use evil's jumplist.
(use-package better-jumper :ensure t
  :diminish better-jumper-local-mode
  :init
  (global-set-key [remap evil-jump-forward]  #'better-jumper-jump-forward)
  (global-set-key [remap evil-jump-backward] #'better-jumper-jump-backward)
  (global-set-key [remap xref-pop-marker-stack] #'better-jumper-jump-backward)
  (global-set-key [remap xref-go-back] #'better-jumper-jump-backward)
  (global-set-key [remap xref-go-forward] #'better-jumper-jump-forward)
  :config
  (better-jumper-mode +1)

  ; taken from doom
  (defun my/set-jump-a (orig-fn &rest args)
    "Set a jump point and ensure ORIG-FN doesn't set any new jump points."
    (better-jumper-set-jump (if (markerp (car args)) (car args)))
    (let ((evil--jumps-jumping t)
          (better-jumper--jumping t))
      (apply orig-fn args)))

  ;; Creates a jump point before killing a buffer. This allows you to undo
  ;; killing a buffer easily (only works with file buffers though; it's not
  ;; possible to resurrect special buffers).
  ;;
  ;; I'm not advising `kill-buffer' because I only want this to affect
  ;; interactively killed buffers.
  (advice-add #'kill-current-buffer :around #'my/set-jump-a)

  ;; Create a jump point before jumping with imenu.
  (advice-add #'imenu :around #'my/set-jump-a))


(use-package undo-tree :ensure t :diminish
  :init
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (setq undo-tree-visualizer-diff t)
  :config
  (global-undo-tree-mode))
; NOTE: emacs undo is weird.
; https://www.reddit.com/r/emacs/comments/6yzwic/how_emacs_undo_works/
; http://ergoemacs.org/emacs/emacs_undo_cult_problem.html
; NOTE: emacs 28 has use undo-redo stuff and evil can use it
; }}}

; misc {{{
; fine-grained word
; TODO: no camel case, more fine-grained control over special characters
(use-package syntax-subword :ensure t
  :config (global-syntax-subword-mode))
(define-key evil-insert-state-map (kbd "C-j") 'syntax-subword-right)
(define-key evil-insert-state-map (kbd "C-k") 'syntax-subword-left)
(define-key evil-insert-state-map (kbd "C-<SPC>") 'evil-insert-digraph)
(define-key evil-insert-state-map (kbd "C-@") 'evil-insert-digraph)
(define-key evil-insert-state-map (kbd "C-v") 'yank)
(define-key evil-visual-state-map (kbd "M-y") (kbd "\"+y"))
(define-key evil-normal-state-map (kbd "M-o") 'evil-jump-backward)
(define-key evil-normal-state-map (kbd "M-i") 'evil-jump-forward)
(define-key evil-normal-state-map (kbd "M-0")
            (lambda () (interactive) (evil-first-non-blank) (evil-forward-word-begin)))

(defun my/toggle-evil-ex-search-case ()
  (interactive)
  (cond
   ((eq evil-ex-search-case 'smart) (setq evil-ex-search-case 'sensitive))
   ((eq evil-ex-search-case 'sensitive) (setq evil-ex-search-case 'smart))))
;}}}
;}}}

; coq {{{
; TODO: https://proofgeneral.github.io/doc/master/userman/Coq-Proof-General/#Showing-Proof-Diffs
; mapping {{{
(define-minor-mode my/coq-mode
  "Minor mode for keys common to main/goal/resp buffers.
evil-define-key is very weird. dolist didn't work: https://emacs.stackexchange.com/a/61238"
  :keymap (make-sparse-keymap))
; TODO: prefer About to Check
; TODO: check/locate visual region
; TODO: "C-c ," to jump to the error point
(evil-define-key 'normal 'my/coq-mode
  (kbd "M-k") 'proof-undo-last-successful-command
  (kbd "M-j") 'my/proof-assert-next-command
  (kbd "C-M-k") 'proof-undo-last-successful-command
  (kbd "C-M-j") 'my/proof-assert-next-command
  (kbd "M-d") 'company-coq-doc
  (kbd "M-,") 'my/coq-Print-point
  (kbd "M-.") 'my/coq-Check-point
  (kbd "M-]") 'company-coq-jump-to-definition
  (kbd ", c s") 'coq-Search
  (kbd ", l c") 'coq-LocateConstant
  (kbd ", l l") 'proof-layout-windows
  (kbd ", l p") 'proof-prf
  (kbd ", C-c") 'proof-interrupt-process
  (kbd "C-c s") 'proof-shell-exit
  (kbd ", S") 'proof-find-theorems
  (kbd ", ?") 'coq-Check
  (kbd ", p") 'coq-Print
  (kbd ", t p") 'my/coq-toggle-printing-level
  (kbd ", ;") 'pg-insert-last-output-as-comment
  (kbd ", o") 'company-coq-occur)
(add-hook 'coq-mode-hook 'my/coq-mode)
(add-hook 'coq-goals-mode-hook 'my/coq-mode)
(add-hook 'coq-response-mode-hook 'my/coq-mode)
; keys specific to main buffer
(evil-define-key 'normal coq-mode-map
  (kbd "u") 'my/coq-undo
  (kbd "C-r") 'my/coq-redo
  ; folding: S-tab, C-c C-/, C-c C-\ (repeat to hide/show all)
  (kbd "C-c C-_") 'company-coq-fold ; C-/ is C-_ in terminal
  (kbd "M-l") 'proof-goto-point
  (kbd "C-M-l") 'proof-goto-point
  (kbd "C-w M-]") (lambda () (interactive) (evil-window-split) (company-coq-jump-to-definition (company-coq-symbol-at-point-with-error))))
(evil-define-key 'insert coq-mode-map
  (kbd "M-l") (lambda () (interactive) (my/break-undo 'proof-goto-point))
  (kbd "M-k") (lambda () (interactive) (my/break-undo 'proof-undo-last-successful-command))
  (kbd "M-j") (lambda () (interactive) (my/break-undo 'my/proof-assert-next-command))
  (kbd "C-M-l") (lambda () (interactive) (my/break-undo 'proof-goto-point))
  (kbd "C-M-k") (lambda () (interactive) (my/break-undo 'proof-undo-last-successful-command))
  (kbd "C-M-j") (lambda () (interactive) (my/break-undo 'my/proof-assert-next-command)))
; Indentation mappings
; Coq uses smie-indent-line for indent-line-function by default.
; It indents codes too eagerly and doesn't indent comments, which is quite annoying.
; So, set indent-line-function to my/indent-relative-first-indent-point, which works like vim's autoindent,
; and run smie indent manually when needed.
(evil-define-key 'insert coq-mode-map
  (kbd "TAB") 'tab-to-tab-stop ; NOTE: This uses tab-width, NOT evil-shift-width. Add a function for expandtab <tab> that uses evil-shift-width?
  (kbd "C-f") 'smie-indent-line
  (kbd "M-i") 'indent-relative
  ; Better than comment-indent-new-line because newline-and-indent uses indent-line-function
  ; NOTE: RET was mapped to company-coq-maybe-exit-snippet
  (kbd "RET") 'newline-and-indent) ; TODO: break undo at nonblank line
(evil-define-key 'normal coq-mode-map
  (kbd "=") 'my/evil-indent-coq) ; use smie-indent-line for =
;}}}

; functions {{{
(defun my/coq-mode-setup ()
  (setq indent-line-function 'my/indent-relative-first-indent-point)
  (setq electric-indent-inhibit t)
  (setq evil-shift-width 2
        tab-width 2)
  (setq comment-style 'multi-line)
  ; TODO: https://github.com/emacs-evil/evil/issues/606
  ; Rollback these and fix evil-join? Does gq work properly?
  ; These comment-continue styles require the modified comment-padright
  (setq comment-continue "")
  ; For this style, if the "   " is removed, then uncomment fails to un-indent the text below the empty line.
  ; (setq comment-continue "   ")
  (setq vimish-fold-marks '("<!--" . "-->"))
  (diminish 'hs-minor-mode)
  (diminish 'outline-minor-mode)
  (load-file "~/.emacs.d/pg-ssr.el")
  (proof-definvisible my/coq-printing-1 "Set Printing Coercions. Set Printing Implicit")
  (proof-definvisible my/coq-printing-0 "Unset Printing Coercions. Unset Printing Implicit. Unset Printing All."))

(defun my/indent-relative-first-indent-point ()
  "Equivalent to `indent-relative-first-indent-point', but with different name
so that `indent-according-to-mode' mode doesn't handle it specially."
  (interactive)
  (indent-relative t))

; Redefine some commment functions to allow whitespace comment-continue {{{
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
    (let ((s (match-string 1 str))	;actual string
	  (lpad (substring str 0 (match-beginning 1))) ;left padding
	  (rpad (concat (substring str (match-end 1)) ;original right padding
			(substring comment-padding ;additional right padding
				   (min (- (match-end 0) (match-end 1))
					(length comment-padding)))))
	  ;; We can only duplicate C if the comment-end has multiple chars
	  ;; or if comments can be nested, else the comment-end `}' would
	  ;; be turned into `}}' where only the first ends the comment
	  ;; and the rest becomes bogus junk.
	  (multi (not (and comment-quote-nested
			   ;; comment-end is a single char
			   (string-match "\\`\\s-*\\S-\\s-*\\'" comment-end)))))
      (if (not (symbolp n))
	  (concat lpad s (when multi (make-string n (aref str (1- (match-end 1))))) rpad)
	;; construct a regexp that would match anything from just S
	;; to any possible output of this function for any N.
	(concat (mapconcat (lambda (c) (concat (regexp-quote (string c)) "?"))
			   lpad "")	;padding is not required
		(regexp-quote s)
		(when multi "+")	;the last char of S might be repeated
		(mapconcat (lambda (c) (concat (regexp-quote (string c)) "?"))
			   rpad ""))))) ;padding is not required

    str)) ; MODIFIED return str if str is whitespace

(defun comment-region-internal (beg end cs ce
                                &optional ccs cce block lines indent)
  "Comment region BEG .. END.
CS and CE are the comment start string and comment end string,
respectively.  CCS and CCE are the comment continuation strings
for the start and end of lines, respectively (default to CS and CE).
BLOCK indicates that end of lines should be marked with either CCE,
CE or CS \(if CE is empty) and that those markers should be aligned.
LINES indicates that an extra lines will be used at the beginning
and end of the region for CE and CS.
INDENT indicates to put CS and CCS at the current indentation of
the region rather than at left margin.

This modified version does not mark the empty line if CCS is whitespace."
  ;;(assert (< beg end))
  (let ((no-empty (not (or (eq comment-empty-lines t)
			   (and comment-empty-lines (zerop (length ce))))))
	ce-sanitized)
    ;; Sanitize CE and CCE.
    (if (and (stringp ce) (string= "" ce)) (setq ce nil))
    (setq ce-sanitized ce)
    (if (and (stringp cce) (string= "" cce)) (setq cce nil))
    ;; If CE is empty, multiline cannot be used.
    (unless ce (setq ccs nil cce nil))
    ;; Should we mark empty lines as well ?
    (if (or ccs block lines) (setq no-empty nil))
    ; MODIFIED
    (setq no-empty (or no-empty (and (stringp ccs) (not (string-match "\\S-" ccs)))))
    ;; Make sure we have end-markers for BLOCK mode.
    (when block (unless ce (setq ce (comment-string-reverse cs))))
    ;; If BLOCK is not requested, we don't need CCE.
    (unless block (setq cce nil))
    ;; Continuation defaults to the same as CS and CE.
    (unless ccs (setq ccs cs cce ce))

    (save-excursion
      (goto-char end)
      ;; If the end is not at the end of a line and the comment-end
      ;; is implicit (i.e. a newline), explicitly insert a newline.
      (unless (or ce-sanitized (eolp)) (insert "\n") (indent-according-to-mode))
      (comment-with-narrowing beg end
	(let ((min-indent (point-max))
	      (max-indent 0))
	  (goto-char (point-min))
	  ;; Quote any nested comment marker
	  (comment-quote-nested comment-start comment-end nil)

	  ;; Loop over all lines to find the needed indentations.
	  (goto-char (point-min))
	  (while
	      (progn
		(unless (looking-at "[ \t]*$")
		  (setq min-indent (min min-indent (current-indentation))))
		(end-of-line)
		(setq max-indent (max max-indent (current-column)))
		(not (or (eobp) (progn (forward-line) nil)))))

	  (setq max-indent
		(+ max-indent (max (length cs) (length ccs))
                   ;; Inserting ccs can change max-indent by (1- tab-width)
                   ;; but only if there are TABs in the boxed text, of course.
                   (if (save-excursion (goto-char beg)
                                       (search-forward "\t" end t))
                       (1- tab-width) 0)))
	  (unless indent (setq min-indent 0))

	  ;; make the leading and trailing lines if requested
	  (when lines
            ;; Trim trailing whitespace from cs if there's some.
            (setq cs (string-trim-right cs))

	    (let ((csce
		   (comment-make-extra-lines
		    cs ce ccs cce min-indent max-indent block)))
	      (setq cs (car csce))
	      (setq ce (cdr csce))))

	  (goto-char (point-min))
	  ;; Loop over all lines from BEG to END.
	  (while
	      (progn
		(unless (and no-empty (looking-at "[ \t]*$"))
		  (move-to-column min-indent t)
		  (insert cs) (setq cs ccs) ;switch to CCS after the first line
		  (end-of-line)
		  (if (eobp) (setq cce ce))
		  (when cce
		    (when block (move-to-column max-indent t))
		    (insert cce)))
		(end-of-line)
		(not (or (eobp) (progn (forward-line) nil))))))))))
; }}}

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

;; TODO don't move cursor in goal/info window
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

; protected undo-tree {{{
; undo-tree is not enabled because PG has its own undo impl. Manually enable.
(add-hook 'coq-mode-hook #'undo-tree-mode)

; undo-tree-undo'ing in the PG locked region may break the proof
; Solution from https://github.com/ProofGeneral/PG/issues/430#issuecomment-604317650
(defun my/pg-in-protected-region-p ()
  ; TODO: this should be <=
  (< (point) (proof-queue-or-locked-end)))

(defmacro my/coq-wrap-edit (action)
  `(if (or (not proof-locked-span)
           (equal (proof-queue-or-locked-end) (point-min)))
       (,action)
     (,action)
     (when (my/pg-in-protected-region-p)
       ; TODO: Broken because proof-assert-until-point is too eager. It should
       ; not assert the sentence containing the point. Find the function used
       ; for retracting when editing inside the locked region.
       (proof-goto-point))))

(defun my/coq-redo ()
  (interactive)
  (my/coq-wrap-edit undo-tree-redo))

(defun my/coq-undo ()
  (interactive)
  (my/coq-wrap-edit undo-tree-undo))
; }}}

; hooks, settings {{{
(add-hook 'coq-mode-hook #'company-coq-mode)
(add-hook 'company-coq-mode-hook #'my/coq-mode-setup)

(put 'company-coq-fold 'disabled nil)

; interaction of jump-to-definition and evil jump lists (C-o, C-i)
(evil-add-command-properties #'company-coq-jump-to-definition :jump t)
(evil-add-command-properties #'proof-goto-end-of-locked :jump t)
(evil-add-command-properties #'proof-interrupt-process :jump t)

; TODO: make M-j/k :jump considering proof-end-of-locked-visible-p or error
; don't repeat proof stuff with `.`.
(evil-declare-not-repeat #'my/proof-assert-next-command)
(evil-declare-not-repeat #'proof-undo-last-successful-command)
(evil-declare-not-repeat #'proof-goto-point)

(setq-default proof-three-window-mode-policy 'smart)

(setq coq-compile-before-require nil)

(setq coq-smie-user-tokens
      '(("," . ":=")
        ("∗" . "->")
        ("-∗" . "->")
        ("∗-∗" . "->")
        ("==∗" . "->")
        ("=∗" . "->") ;; Hack to match ={E1,E2}=∗
        ("|==>" . ":=")
        ("⊢" . "->")
        ("⊣⊢" . "->")
        ("↔" . "->")
        ("←" . "<-")
        ("→" . "->")
        ("=" . "->")
        ("==" . "->")
        ("/\\" . "->")
        ("⋅" . "->")
        (":>" . ":=")
        ("by" . "now")
        ("forall" . "now") ;; NB: this breaks current ∀ indentation.
        ))


(setq company-coq-disabled-features '(hello smart-subscripts spinner)
      company-coq-features/prettify-symbols-in-terminals t)

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
        ;("-->" . ?⟶) ("<--" . ?⟵) ("<-->" . ?⟷)
        ;("==>" . ?⟹) ("<==" . ?⟸) ("~~>" . ?⟿) ("<~~" . ?⬳)
        ))

; TODO https://coq.zulipchat.com/#narrow/stream/304019-Proof-General.20users/topic/tactic.20completion.20scaling.20in.20company-coq/near/258439819
; }}}
; }}}

; etc packages {{{
(use-package neotree :ensure t
  :init
  (setq neo-hidden-regexp-list '("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.o$" "\\.vo[sk]?$" "\\.v\\.d$" "\\.glob$"))
  (setq neo-window-fixed-size nil)
  (setq neo-window-width 32)
  (setq neo-smart-open t)
  (setq neo-theme 'nerd)
  :config
  (evil-define-key 'normal 'global
    (kbd "<SPC> n n") 'neotree-toggle
    (kbd "<SPC> n h") 'neotree-hidden-file-toggle))

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
  (setq company-selection-wrap-around t)
  (setq company-dabbrev-downcase nil
        company-dabbrev-other-buffers nil
        company-dabbrev-ignore-case nil)
  (setq company-idle-delay 0.2)
  (setq company-transformers
    '(company-sort-by-backend-importance
      company-sort-prefer-same-case-prefix
      company-sort-by-occurrence))
  ; make company work like vim ins-completion
  (company-tng-mode)
  ; override default evil-collection maps
  (evil-collection-define-key nil 'company-active-map
    ; -or-abort variants fail if there's single candidate
    (kbd "C-n") 'company-select-next
    (kbd "C-p") 'company-select-previous
    (kbd "C-j") nil
    (kbd "C-k") nil
    (kbd "M-j") nil
    (kbd "M-k") nil
    (kbd "C-w") nil
    ; <S-tab> in terminal is <S-iso-leftab>
    (kbd "<S-iso-lefttab>") 'company-select-previous-or-abort
    (kbd "C-e") 'company-abort
    (kbd "C-y") 'company-complete-selection)
  (evil-collection-define-key nil 'company-search-map
    (kbd "M-j") nil
    (kbd "M-k") nil)
  (evil-define-key 'insert 'global
    (kbd "C-x C-f") 'company-files
    (kbd "C-x C-]") 'company-etags
    (kbd "C-x s")   'company-ispell
    (kbd "C-x C-s") 'company-yasnippet
    (kbd "C-x C-o") 'company-capf
    (kbd "C-n") '+company/dabbrev
    (kbd "C-p") '+company/dabbrev-previous)
  :hook (after-init . global-company-mode))

; taken from doom
(defun +company/dabbrev ()
  "Invokes `company-dabbrev-code' in prog-mode buffers and `company-dabbrev'
everywhere else."
  (interactive)
  (call-interactively
   (if (derived-mode-p 'prog-mode)
       #'company-dabbrev-code
     #'company-dabbrev)))

(defun +company/dabbrev-previous ()
  "Like `+company/dabbrev', but starts from the bottom of the list."
  (interactive)
  (let ((company-selection-wrap-around t))
    (call-interactively #'+company/dabbrev)
    (company-select-previous-or-abort)))

(use-package yasnippet :ensure t :diminish yas-minor-mode
  :config
  (define-key yas-minor-mode-map "\C-l" 'yas-expand) ; TODO: requires explitcit company-select-next even if the typed text exactly matches the snippet
  (define-key yas-keymap "\C-j" 'yas-next-field-or-maybe-expand)
  (define-key yas-keymap "\C-k" 'yas-prev-field)
  (dolist (keymap (list yas-minor-mode-map yas-keymap))
    (define-key keymap (kbd "TAB") nil)
    (define-key keymap [(tab)] nil)))

(use-package fzf
  :config
  ;; https://github.com/bling/fzf.el/issues/1
  (add-hook 'term-mode-hook 'evil-emacs-state)
  (add-hook 'term-mode-hook #'disable-scroll-margin)
  (defadvice fzf/start (after normalize-fzf-mode-line activate)
    (face-remap-add-relative 'mode-line '(:box nil)))
  (defun disable-scroll-margin ()
    (setq-local scroll-margin 0))
  (defadvice fzf/start (after normalize-fzf-mode-line activate)
    (setq mode-line-format nil))
    (evil-define-key 'normal 'global (kbd "<SPC> G") 'fzf-ripgrep))

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
  :init
  (setq counsel-fzf-cmd "~/.fzf/bin/fzf -f \"%s\"")
  (evil-add-command-properties #'counsel-find-file :jump t)
  :bind ("M-x" . counsel-M-x))

(use-package swiper :ensure t :after ivy
  :config
  (setq swiper-include-line-number-in-search t)
  :bind (("M-s M-s" . swiper-isearch)
         ("M-s w" . swiper-thing-at-point)))

(evil-define-key 'normal 'global
  (kbd "C-f") 'counsel-fzf
  (kbd "<SPC> e") 'counsel-find-file ; <BS> deletes each node
  (kbd "<SPC> b") 'ivy-switch-buffer
  (kbd "<SPC> h h") 'counsel-recentf)

(use-package magit :ensure t)

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
;; Current setup partially works but has some flaws:
;; * yanking to "+ also overrides ""
;; * Pasting a line from "+ which is yanked in visual-line mode is not linewise
;;   paste (pasting from "" (which is yanked as a side-effect) works fine)
;; * the content of "+ of :reg command output is not syntax highlighted
(use-package xclip :ensure t
  :init (setq xclip-method 'xclip)
  :config
  (xclip-mode 1)
  (setq select-enable-clipboard nil))

; TODO https://github.com/akermu/emacs-libvterm something's broken
; (use-package vterm :ensure t)
; }}}

; font {{{
(when (display-graphic-p)
  (set-face-attribute 'default nil :height 110)
  (dolist (ft (fontset-list))
    ; Main font
    (set-fontset-font ft 'unicode (font-spec :family "Source Code Pro" :foundry "ADBO"))
    ; Fallback font
    (set-fontset-font ft nil (font-spec :name "DejaVu Sans Mono")))
  (set-fontset-font t nil (font-spec :name "Symbola"))
  (set-fontset-font nil 'hangul (font-spec :family "D2Coding")))

(let*
  ((default (face-attribute 'default :height))
   (size default))
  (defun global-scale-default ()
    (interactive)
    (setq size default)
    (global-scale-internal size))
  (defun global-scale-up ()
    (interactive)
    (global-scale-internal (cl-incf size 10)))
  (defun global-scale-down ()
    (interactive)
    (global-scale-internal (cl-decf size 10)))
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
(setq display-line-numbers-width-start t)
(global-display-line-numbers-mode 1)
(electric-pair-mode 1)
(setq electric-pair-inhibit-predicate `(lambda (char) (minibufferp)))
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
        ((?[ ?U) . ?⊓)
        ((?] ?U) . ?⊔)
        ((?[ ?C) . ?⊏)
        ((?] ?C) . ?⊐)
        ((?[ ?_) . ?⊑)
        ((?] ?_) . ?⊒)
        ((?( ?+) . ?∉)
        ((?\\ ?\\) . ?∖)
        ))
(dolist (alias '((?a "[aα∀]") (?b "[bβ]") (?c "[cξ]") (?d "[dδ]") (?e "[eε∃]") (?f "[fφ]") (?g "[gγ]") (?h "[hθ]") (?i "[iι]") (?j "[jϊ]") (?k "[kκ]") (?l "[lλ]") (?m "[mμ]") (?n "[nν]") (?o "[oο]") (?p "[pπ]") (?q "[qψ]") (?r "[rρ]") (?s "[sσ]") (?t "[tτ]") (?u "[uυ]") (?v "[vϋ𝓥]") (?w "[wω]") (?x "[xχ]") (?y "[yη]") (?z "[zζ]") (?* "[*∗]") (?/ "[/∧]") (?\ "[\∨]") (?< "[<≼]") (?> "[>↦→⇒⇝]") (?[ "[[⌜⎡⊑⊓]") (?] "[\]⌝⎤⊒⊔]")))
  (push alias evil-snipe-aliases))


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
; vim: set foldmethod=marker foldlevel=0 sw=2:
