[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

exec /usr/bin/i3
