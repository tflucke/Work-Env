[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

sh ~/.xsessionrc

if [ -f /usr/bin/i3 ]; then
    exec /usr/bin/i3
fi
