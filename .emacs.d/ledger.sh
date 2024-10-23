#!/bin/sh

iargs=("$@")
oargs=()
j=0;
date=;
for((i=0; i<${#iargs[@]}; ++i)); do
    case ${iargs[i]} in
        --date-format)
            # drop --date-format and the next arg
            i=$((i+1));
            ;;
        xact)
            # convert "xact" to "print --match"
            oargs[j]=print; oargs[j+1]=--match; j=$((j+2));
            # drop xact argument and stash the date argument
            i=$((i+1));
            date=${iargs[i]};
            ;;
        *)
            # keep any other args:
            oargs[j]=${iargs[i]};
            j=$((j+1));
            ;;
    esac
done

if test "$date"
then
    # substitute the given date for the old date:
    hledger "${oargs[@]}" | sed "1s/....-..-../$date/"
else
    hledger "${oargs[@]}"
fi
