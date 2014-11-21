#!/bin/bash

xkbcomp ~/dotfiles/custom.xkb $DISPLAY

export PATH=$PATH:~/.gem/ruby/2.1.0/bin/:~/maple17/bin

emacs --daemon

numlockx &

devmon &

xset -b

xrdb ~/.Xresources

trayer --edge bottom --align right --SetDockType true --SetPartialStrut true --expand true --width 192 --widthtype pixel --height 13 --transparent true --alpha 0 --tint 0x000000 &

nm-applet &

#exec openbox
xsetroot -cursor_name left_ptr

#start the keyring daemon
eval $(gnome-keyring-daemon -s)
export GNOME_KEYRING_CONTROL
export SSH_AUTH_SOCK
###
