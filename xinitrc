#!/bin/bash

export PATH=/home/adam/bin:/usr/local/texlive/2015/bin/x86_64-linux:/usr/local/MATLAB/MATLAB_Compiler_Runtime/v82/runtime/glnxa64:/usr/local/MATLAB/MATLAB_Compiler_Runtime/v82/bin/glnxa64:/usr/local/MATLAB/MATLAB_Compiler_Runtime/v82/sys/os/glnxa64:/usr/local/MATLAB/MATLAB_Compiler_Runtime/v82/sys/java/jre/glnxa64/jre/lib/amd64/native_threads:/usr/local/MATLAB/MATLAB_Compiler_Runtime/v82/sys/java/jre/glnxa64/jre/lib/amd64/server:/usr/local/MATLAB/MATLAB_Compiler_Runtime/v82/sys/java/jre/glnxa64/jre/lib/amd64:$HOME/.cabal/bin:/usr/local/bin:/home/adam/Science/LINUX64:/opt/maple18/bin:$PATH:/usr/local/cuda-7.0/bin

numlockx &
# xscreensaver &

xset -b

xrdb -cpp cpp ~/.Xresources

# compton &
# hsetroot -solid '#002B36'
xsetroot -solid '#002B36' # -tile ~/Pictures/tile.png

#start the keyring daemon
#eval $(gnome-keyring-daemon -s)
#export GNOME_KEYRING_CONTROL
#export SSH_AUTH_SOCK
###
# taffybar &!
~/.xmonad/xmonad-x86_64-linux
