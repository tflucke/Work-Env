#!/bin/bash
display_song() {
	status=
	color=
#	mpcOut = "$(mpc status | sed 1d | head -n1)"
#	case $(echo $mpcOut | awk '{ print $1 }') in
	case $(mpc status | sed 1d | head -n1 | awk '{ print $1 }') in
		'[playing]')
		status=♪
		color="#FFFFFF"
		;;
	'[paused]')
		status=X
		color="#FF0000"
		;;
	esac
#	time = $(mpc status | sed 1d | head -n1 | awk '{ print $3 }')
	echo '[{"name": "mpd", "instance": "now playing", "full_text": "'${status}' '$1'", "color": "'${color}'"}]'
}

(while :; do
    display_song "$(mpc current --wait)"
done) &

while :; do
    display_song "$(mpc current)"
    mpc idle player > /dev/null
done
