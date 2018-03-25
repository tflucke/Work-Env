[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

source ~/.config/env/*.sh

numlockx &

#exec xautolock -time 15 -locker '/usr/local/bin/suspend' &
exec /usr/bin/i3
