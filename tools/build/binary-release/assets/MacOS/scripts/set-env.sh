#!/bin/sh

# Sourced from https://stackoverflow.com/a/29835459/1975049
rreadlink() (
    target=$1 fname= targetDir= CDPATH=
    { \unalias command; \unset -f command; } >/dev/null 2>&1
    [ -n "$ZSH_VERSION" ] && options[POSIX_BUILTINS]=on
    while :; do
        [ -L "$target" ] || [ -e "$target" ] || { command printf '%s\n' "ERROR: '$target' does not exist." >&2; return 1; }
        command cd "$(command dirname -- "$target")" || exit 1
        fname=$(command basename -- "$target")
        [ "$fname" = '/' ] && fname=''
        if [ -L "$fname" ]; then
            target=$(command ls -l "$fname")
            target=${target#* -> }
            continue
        fi
        break
    done
    targetDir=$(command pwd -P)
    if [ "$fname" = '.' ]; then
        command printf '%s\n' "${targetDir%/}"
    elif  [ "$fname" = '..' ]; then
        command printf '%s\n' "$(command dirname -- "${targetDir}")"
    else
        command printf '%s\n' "${targetDir%/}/$fname"
    fi
)

EXEC=$(rreadlink "$0")
DIR=$(dirname $(dirname "$EXEC"))

while [ $# -gt 0 ]; do
    case $1 in
        "--fish")
            FISH=true
            ;;
        "--quiet")
            QUIET=true
            ;;
    esac
    shift
done

my_echo() {
    if [ "$QUIET" != true ]; then
        echo "$1"
    fi
}

my_echo "echo '                           Adding Rakudo to PATH';"
my_echo "echo '                          =======================';"
my_echo "echo '';"

NEW_PATH=$PATH
RAKUDO_PATH0="$DIR/bin"
RAKUDO_PATH1="$DIR/share/perl6/site/bin"
STUFF_DONE=false
for RPATH in $RAKUDO_PATH1 $RAKUDO_PATH0 ; do
    if echo "$NEW_PATH" | grep -vEq "(^|:)$RPATH($|:)" ; then
        NEW_PATH="$RPATH:$NEW_PATH"
        STUFF_DONE=true
    fi
done

if $STUFF_DONE ; then
    if [ "$FISH" = true ]; then
        NEW_PATH=$(echo "$NEW_PATH" | sed "s/:/ /g")
        echo "set -x PATH $NEW_PATH;"
    else
        echo "export PATH='$NEW_PATH';"
    fi
    my_echo "echo 'Paths successfully added.';"
else
    my_echo "echo 'Paths already set. Nothing to do.';"
fi

my_echo "echo '';
echo '================================================================================';
echo ' =========                                                             __   __';
echo '  ||_|_||                =============================                (  \,/  )';
echo '  || # ||                 Welcome to the Raku Console                  \_ O _/';
echo '  || # ||                =============================                 (_/ \_)';
echo '';
echo 'This console has all the tools available you need to get started using Raku.';
echo '';
echo 'Rakudo provides an interactive command line interpreter (a so called Read Eval';
echo 'Print Loop, REPL for short) you can use to quickly try out pieces of Raku code.';
echo 'Start it by typing:';
echo '';
echo '    raku';
echo '';
echo 'If you already have a Raku program in a file, you can run it by typing:';
echo '';
echo '    raku path/to/my/program.raku';
echo '';
echo 'To install additional modules you can use the Zef module manager:';
echo '';
echo '    zef install Some::Module';
echo '';
echo 'https://rakudo.org/           - The home of this implementation of Raku.';
echo 'https://raku.land/            - Go here to browse for Raku modules.';
echo 'https://docs.raku.org/        - The Raku documentation.';
echo 'https://web.libera.chat/#raku - The Raku user chat. Talk to us!';
echo '';
echo '                              Happy hacking!';
echo '';
echo '================================================================================';
echo '';
"
