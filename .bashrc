# ~/.bashrc: executed by bash(1) for non-login shells.
# CSL default
# by kkowal 2005-06-21 and updated by various other people
# apence, 20120909: this is a very Sun centric set of options

# DO NOT MAKE CHANGES TO THIS FILE.  Put your changes in your .mybashrc
# file instead.

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

#
# `PS1' is run each time the bash prompt is drawn.  The following
# line noise draws a colorful prompt with your user name, which
# computer you're logged into, and what directory you're in.
#
#PS1="\@ \h \w\\$ " # non-colored
PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\] \$ '

#
# `PATH' determines the locations, in order, in which the shell will look for
# executable programs.
#
#PATH=/bin:/usr/bin:/usr/sbin
PATH=/bin:/usr/bin
# IF it exists, Look in my home bin first, then the other path
[ -d ~/bin ] && PATH=~/bin:$PATH
# IF it exists, look here after the normal path
[ -d /usr/local/bin ] && PATH=$PATH:/usr/local/bin
[ -d /usr/share/bin ] && PATH=$PATH:/usr/share/bin

### NO don't add PATH=$PATH:.
# the better option is to type ./command when the command is in cwd
# if you want to make a habbit of running other commands out of your path
# append them to your path in your .mybashrc
# YOU need to know what you are going to run

export PATH

#
# `MANPATH' determines the locations, in order, where `man' will look
# for manual pages.
#
MANPATH=/usr/share/man
[ -d ~/man ] && MANPATH=~/man:$MANPATH

# ML
[ -d /usr/local/sml/bin ] && PATH=$PATH:/usr/local/sml/bin

[ -d /usr/local/man ] && MANPATH=$MANPATH:/usr/local/man
[ -d /usr/local/share/man ] && MANPATH=$MANPATH:/usr/local/share/man

export MANPATH

#
# `PAGER' is the name of the program that many applications will use to
# limit their output to a page at a time.  `more' is the original pager.
# `less' additionally allows you to back up.
#
PAGER=less
#PAGER=more

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

#
# `EDITOR' is the name of the program that many applications will invoke
# to edit a text file.
#
#EDITOR=vim
#EDITOR=emacs
#EDITOR=pico # Warning: other users will be able to see
             #  you using this with the ps command ;-)

#
# common aliases
#
# the following is a list of commonly used aliases, to use them simply remove
# the '#' from the beginning of the line.  Read the man pages to find out 
# exactly what each command does.
#
#alias h="history | $PAGER"
#alias ls="ls -F1"
#alias l.="ls -FA1"
#alias ll="ls -slagFL"
#alias lf="ls -F"
#alias lr="ls -RF"
#alias l="ls -FLAsC"
#alias bye="logout"

#
# don't put duplicate lines in the history. See bash(1) for more options
#
export HISTCONTROL=ignoredups

#
# `umask' is an octal bit mask which constrains the permissions you
# will grant to new files by default.  077 completely denies other users
# access to your files.  022 denies all other users write access.
# The CSL mandates that you keep your umask set to 077, and only
# change some small known set of files so that others may have access.
#
umask 077

#
# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
#
shopt -s checkwinsize

#
# `TERM' determines what kind of terminal the system expects you are using.
# This affects whether and what kinds of control characters will be sent
# to your terminal client (like color codes and cursor positions).
#
#TERM=xtermc

#
# if this is an xterm set the title of the window to user@host:dir
#
case "$TERM" in
	xterm*|rxvt*)
		PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD}\007"'
		;;
	*)
    ;;
esac

#
# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc).
#
[ -f /etc/bash_completion ] && . /etc/bash_completion

#
# This runs a user defined script `.mybashrc', which by default does
# nothing.
#
#[ -f ~/.bashrc.`uname` ] && . ~/.bashrc.`uname`
#[ -f ~/.bashrc.`hostname` ] && . ~/.bashrc.`hostname`
[ -f ~/.mybashrc ] && . ~/.mybashrc
# end
