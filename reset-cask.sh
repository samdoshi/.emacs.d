#!/bin/bash
pushd ~/.emacs.d
rm -r .cask
rm Cask
echo "(source melpa)" > Cask
echo "(source gnu)" >> Cask
echo "(depends-on \"req-package\")" >> Cask
cask install
emacs --eval '(progn (pallet-init) (kill-emacs))'
popd
