#!/bin/sh

cat $(ls $HOME/.i3/*.config | sort) > $HOME/.i3/config
