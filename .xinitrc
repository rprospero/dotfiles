#!/bin/bash

xkbcomp ~/.Xkeymap $DISPLAY

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
exec xmonad
