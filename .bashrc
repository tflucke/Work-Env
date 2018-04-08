#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

MUSIC_DIR="/media/music"
MTG_DIR="~/.local/share/data/Cockatrice/Cockatrice/decks"
SH_DIR="~/.config/env"
SERVER_IP="63.246.2.164"

export EDITOR="/usr/bin/emacs -nw"
export BROWSER=/usr/bin/firefox
export TERM=xterm-256color

shopt -s checkwinsize
shopt -s autocd
shopt -s dirspell
shopt -s xpg_echo

# Set up history search
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'
export HISTCONTROL=ignoredups

# Set up left/right word skipping
bind '"\e[1;5D": backward-word'
bind '"\e[1;5C": forward-word'

# List of directories to look for git completion
gitCompletion="/usr/share/git/completion/git-completion.bash /usr/share/bash-completion/completions/git"

for f in $gitCompletion; do
    if [ -f "$f" ]; then
	source "$f"
    fi
done

# TODO: Can't use SH_DIR, doesn't expand with *.
for script in ~/.config/env/*.sh; do
    source "$script"
done

getLyricalSongs()
{
  mpc list album | grep -v '\(Portal 2.*\)\|\(Avatar.*\)\|\(Frozen.*\)' | while read -r line; do
    printf "$(mpc find album "$line" | grep -v 'Instrumental')\n"
  done
}

# echo exec
# Print command and execute ala make
#if type -t ecex; then
ecex()
{
    echo "$@"
    $@
    
}

#alias xterm='echo "xterm -rv" && xterm -rv'
alias emacs='emacs -nw'
alias xterm='ecex xterm -rv'
alias decks='ecex "$MTG_DIR" && ecex mv /tmp/*.dec "$MTG_DIR"'
alias newMusic='ecex sudo chown media /tmp/*.mp3 && ecex sudo -u media mv -v /tmp/*.mp3 /media/music && ecex mpc update'
alias playMusic='ecex mpc clear && ecex getLyricalSongs | ecex mpc add && ecex mpc shuffle && ecex mpc play'
alias conServer='ecex ssh tflucke@63.246.2.164'
alias packageSize="pacman -Qi | gawk '/^Name/ { x = $3 }; /^Installed Size/ { sub(/Installed Size  *:/, ""); print x":" $0 }' | sort -k2,3nr"

#[ $exitCode -eq 0 ] && color=$FGRN || color=$FRED
PS1="[\u@\h->$BLD\w$RS]
$BLD$([ $? -ne 0 ] && echo '\[$FRED')(\$?)$RS \$ "

# Auto mount
# Mounts or unmounts intelligently.  Useful for keyboard bindings
amount () {
    if mount | grep -q "/dev/$1"; then
	udevil umount -l "/dev/$1"
    else
	udevil mount "/dev/$1"
    fi	
} 

mysqlpermissions()
{
  mysql -B -N $@ -e "SELECT DISTINCT CONCAT(
    'SHOW GRANTS FOR \'', user, '\'@\'', host, '\';'
    ) AS query FROM mysql.user" | \
  mysql $@ | \
  sed 's/\(GRANT .*\)/\1;/;s/^\(Grants for .*\)/## \1 ##/;/##/{x;p;x;}'
}

# Syncs music dir to dir.  Useful for syncing phone music.
syncMusic()
{
    ecex rsync -ruh --progress "$MUSIC_DIR" "$1"
}
