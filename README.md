# install

```sh
git clone https://github.com/tomtomjhj/init.el ~/.emacs.d
cd ~/.emacs.d
git submodule update --init --recursive
```

then, in emacs,

```
M-x package-refresh-contents <RET>
M-x package-install-selected-packages <RET>
C-x C-c
```

# Note

* `C-z` to enter emacs mode
* don't do this or find a way to automatically manage this
```
C-u 0 M-x byte-recompile-directory <RET> ~/.emacs.d/submodules <RET>
M-x byte-recompile-directory
```
    * http://ergoemacs.org/emacs/emacs_byte_compile.html
    * https://emacs.stackexchange.com/questions/185/can-i-avoid-outdated-byte-compiled-elisp-files
    * https://github.com/emacscollective/auto-compile
* upgrade packages
```
M-x package-refresh-contents <RET>
M-x list-packages <RET>
U
x
```

# Tips
* Fixing "xclip error can't open display" without exiting emacs: Open another term and get `$DISPLAY`, then `M-x setenv`.
    * This probably happens when disconnecting from ssh session hangs.
* `previous-complete-history-element` doesn't put cursor on the last char because it gets history element which completes the minibuffer before the point. Just use `C-e`.
    * https://github.com/emacs-evil/evil/issues/1086
* `toggle-debug-on-error` → how to use this?

# TODO
* use-package works fine with submodules. Don't put `:ensure` and configure `load-path`.
* apropos?
* https://depp.brause.cc/dotemacs/
* company coq folding broken: `. ` inside comment is not ignored
  ```coq
  Definition a := (* asdf. *) 1.
  ```
* take stuff from https://github.com/hlissner/doom-emacs/tree/develop/modules/editor/evil
