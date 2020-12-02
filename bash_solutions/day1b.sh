#!/bin/bash
FILE=../apps/aoc/priv/inputs/day1.txt
IFS=$'\n' array=($(<$FILE))
cat $FILE | while read line;
do
    for int1 in "${array[@]}"; do
        for int2 in "${array[@]}"; do
            case $((${array[$i]}+$int1+$int2)) in
                2020)
                    echo "Result: $((${array[$i]}*$int1*$int2))"
                    exit 0
                    ;;
                *)
                    ;;
            esac
        done
    done
    i=$(($i+1))
done
