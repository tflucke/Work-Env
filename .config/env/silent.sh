#!/usr/bin/sh

if type -t silent > /dev/null; then
    return
fi

# Do not print stdout.  Removes ugly redirect syntax
silent()
{
    $@ > /dev/null
}

if type -t noError > /dev/null; then
    return
fi

# Do not print stderr.  Removes ugly redirect syntax
noError()
{
    $@ 2> /dev/null
}
