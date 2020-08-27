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

# TODO
* rebase jumplist-squash onto 1.14.0
* apropos?
