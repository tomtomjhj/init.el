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
* don't do this
```
C-u 0 M-x byte-recompile-directory <RET> ~/.emacs.d/submodules <RET>
M-x byte-recompile-directory
```
* upgrade packages
```
M-x package-refresh-contents <RET>
M-x list-packages <RET>
U
x
```
