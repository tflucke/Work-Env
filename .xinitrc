[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

source ~/.config/env/*.sh

numlockx &

xrandr --output $mon1 --auto --primary --pos 1280x290 --output $mon2 --auto --left-of $mon1
#exec xautolock -time 15 -locker '/usr/local/bin/suspend' &
exec /usr/bin/i3
