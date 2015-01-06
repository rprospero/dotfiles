#!/bin/bash

#export PATH=$PATH:~/.gem/ruby/2.1.0/bin/:~/maple17/bin

emacs --daemon

numlockx &

xset -b

xrdb ~/.Xresources

xsetroot -cursor_name left_ptr -solid black

#start the keyring daemon
#eval $(gnome-keyring-daemon -s)
#export GNOME_KEYRING_CONTROL
#export SSH_AUTH_SOCK
###
xmonad 
