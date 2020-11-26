#!/bin/sh

ln -s ~/.emacs.d/.emacs ~/.emacs

! mkdir -r ~/.config/Code/User/
ln -s ~/.emacs.d/keybindings.json ~/.config/Code/User/keybindings.json
