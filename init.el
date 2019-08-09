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



; evil.
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)
(modify-syntax-entry ?_ "w") ; `_` isn't word char in emacs
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

; - PG
; - tabbar-mode
; - things from .spacemacs
;   - undo-tree mapping of coq
