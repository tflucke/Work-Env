#!/bin/sh

cat $(ls $HOME/.i3/*.config | sort) > $HOME/.i3/config

if [ ! -z "$1" ]; then
    i3-msg "$1"
fi
