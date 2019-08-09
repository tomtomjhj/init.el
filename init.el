; evil.
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)
(modify-syntax-entry ?_ "w") ; `_` isn't word char in emacs
; - undo-tree
; - PG
; - tabbar-mode
; - things from .spacemacs
;   - undo-tree mapping of coq
