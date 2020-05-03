
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
 '(custom-safe-themes
   (quote
    ("6b77fcf51237a5313bb0ea39ef4127e82e7d2b7196bc00b31a186235a3ce838e" default)))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (avy xclip projectile diminish eyebrowse neotree magit yasnippet company counsel hl-todo editorconfig annalist syntax-subword markdown-mode use-package rainbow-delimiters company-coq proof-general)))
 '(proof-splash-enable nil)
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
 '(proof-eager-annotation-face ((t (:background "medium blue"))))
 '(proof-error-face ((t (:background "dark red"))))
 '(proof-locked-face ((((type tty) (min-colors 256)) (:background "#3a3a3a")) (t (:background "#00283c"))))
 '(proof-queue-face ((t (:background "#304060"))))
 '(proof-warning-face ((t (:background "indianred3")))))
