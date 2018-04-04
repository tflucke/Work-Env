[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

sh ~/.xsessionrc

exec /usr/bin/i3
