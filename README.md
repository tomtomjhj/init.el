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
C-u 0 M-x byte-recompile-directory <RET> ~/.emacs.d/submodules <RET>
C-x C-c # quit emacs
```

# Note

* `C-z` to enter emacs mode
* `M-x byte-recompile-directory` after updating submodules
