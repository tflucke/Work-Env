[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

source .xsessionrc

exec /usr/bin/i3
