#!/bin/bash

emacs --daemon

numlockx &

dropboxd &

xset -b

xrdb ~/.Xresources

trayer --edge bottom --align right --SetDockType true --SetPartialStrut true --expand true --width 192 --widthtype pixel --height 12 --transparent true --alpha 0 --tint 0x000000 &

#exec openbox
xsetroot -cursor_name left_ptr
exec xmonad
