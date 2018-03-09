
base16-builder -s ./$1.yaml -t ./Xresources.template -b dark > Xresources
base16-builder -s ./$1.yaml -t ./taffybar.rc.template -b dark > taffybar.rc
base16-builder -s ./$1.yaml -t ./zathurarc.template -b dark > zathurarc
base16-builder -s ./$1.yaml -t ./gtk-2.0.template -b dark > gtk-2.0
base16-builder -s ./$1.yaml -t ./gtk-3.0.template -b dark > gtk-3.0
