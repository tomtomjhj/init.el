
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(company-coq-disabled-features (quote (hello smart-subscripts)))
 '(company-coq-features/prettify-symbols-in-terminals t)
 '(counsel-find-file-ignore-regexp
   "\\(?:\\.\\(?:aux\\|b\\(?:bl\\|in\\|lg\\|zr/\\)\\|c\\(?:lass\\|ps?\\)\\|d\\(?:\\(?:64fs\\|fs\\|x\\(?:\\(?:32\\|64\\)fs\\)?\\)l\\)\\|elc\\|f\\(?:asl?\\|mt\\|ns?\\|\\(?:x\\(?:\\(?:32\\|64\\)f\\)\\)?sl\\)\\|g\\(?:it/\\|lob?\\|mo\\)\\|hg/\\|idx\\|kys?\\|l\\(?:bin\\|ib\\|o[ft]\\|x\\(?:\\(?:32\\|64\\)fsl\\)\\|[ano]\\)\\|m\\(?:em\\|o\\)\\|p\\(?:64fsl\\|fsl\\|gs?\\|h[io]\\|y[co]\\)\\|s\\(?:o\\|parcf\\|vn/\\|x\\(?:\\(?:32\\|64\\)fsl\\)\\)\\|t\\(?:fm\\|oc\\|ps?\\)\\|ufsl\\|v\\(?:rs\\|[or]\\)\\|wx\\(?:\\(?:32\\|64\\)fsl\\)\\|x86f\\|[ao]\\)\\|CVS/\\|_\\(?:\\(?:MTN\\|darcs\\)/\\)\\|~\\)")
 '(evil-goto-definition-functions (quote (evil-goto-definition-search)))
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#ffafd7")
     ("NOTE" . "#ffafd7")
     ("HACK" . "#ffafd7")
     ("FIXME" . "#ffafd7")
     ("XXX+" . "#ffafd7"))))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (zenburn-theme undo-tree better-jumper vimish-fold avy xclip projectile diminish eyebrowse neotree magit yasnippet company counsel hl-todo editorconfig annalist syntax-subword markdown-mode use-package rainbow-delimiters company-coq proof-general)))
 '(proof-splash-enable nil)
 '(proof-universal-keys
   (quote
    (([(control c)
       96]
      . proof-next-error)
     ([(control c)
       (control c)]
      . proof-interrupt-process)
     ([(control c)
       (control n)]
      . proof-assert-next-command-interactive)
     ([(control c)
       (control u)]
      . proof-undo-last-successful-command)
     ([(control c)
       (control p)]
      . proof-prf)
     ([(control c)
       (control l)]
      . proof-layout-windows)
     ([(control c)
       (control x)]
      . proof-shell-exit)
     ([(control c)
       (control v)]
      . proof-minibuffer-cmd)
     ([(control c)
       (control w)]
      . pg-response-clear-displays)
     ([(control c)
       (control 46)]
      . proof-goto-end-of-locked)
     ([(control c)
       46]
      . proof-goto-end-of-locked)
     ([(control c)
       (control f)]
      . proof-find-theorems)
     ([(control c)
       (control o)]
      . proof-display-some-buffers)
     ([(control shift mouse-1)]
      . pg-identifier-under-mouse-query))))
 '(safe-local-variable-values
   (quote
    ((eval setq coq-prog-name
           (expand-file-name
            (concat
             (locate-dominating-file buffer-file-name ".dir-locals.el")
             "_opam/bin/coqtop"))))))
 '(show-paren-mode t)
 '(word-wrap t)
 '(xterm-mouse-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-lead-face ((t (:foreground "#1c1c1c" :background "#f0e" :weight bold))))
 '(evil-snipe-first-match-face ((t (:foreground "#1c1c1c" :background "#ffffff" :inverse-video t :weight bold))))
 '(evil-snipe-matches-face ((t (:foreground "#1c1c1c" :background "#afff00" :weight bold))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#afd7af"))))
 '(font-lock-comment-face ((t (:foreground "#afd7af"))))
 '(font-lock-constant-face ((t (:foreground "#afd75f"))))
 '(hl-todo ((t (:inverse-video t :weight bold))))
 '(isearch ((t (:foreground nil :inverse-video t :weight bold :underline t))))
 '(lazy-highlight ((t (:foreground nil :background "#444444" :weight bold :underline t))))
 '(line-number ((t (:foreground "#757565" :background "#1c1c1c"))))
 '(markdown-code-face ((t (:inherit fixed-pitch :foreground "#ffd7d7"))))
 '(markdown-inline-code-face ((t (:inherit markdown-code-face))))
 '(markdown-pre-face ((t (:inherit markdown-code-face))))
 '(proof-eager-annotation-face ((t (:background "medium blue"))))
 '(proof-error-face ((t (:background "dark red"))))
 '(proof-locked-face ((t (:background "#3a3a3a"))))
 '(proof-queue-face ((t (:background "#304060"))))
 '(proof-warning-face ((t (:background "indianred3"))))
 '(region ((t (:background "#626262" :extend t))))
 '(vimish-fold-overlay ((t (:inherit highlight :inverse-video t :weight bold)))))
