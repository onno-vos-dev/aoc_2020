#!/bin/bash
FILE=../apps/aoc/priv/inputs/day1.txt
IFS=$'\n' array=($(<$FILE))
cat $FILE | while read line;
do
    for int in "${array[@]}"; do
        case $((${array[$i]}+$int)) in
            2020)
                echo "Result: $((${array[$i]}*$int))"
                exit 0
                ;;
            *)
                ;;
        esac
    done
    i=$(($i+1))
done
