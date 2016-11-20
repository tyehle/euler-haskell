#!/bin/bash

while true; do
    find -regextype posix-extended -regex "\./(src|test|app).*|.*\.cabal" |
        entr -cd sh -c "stack build && clear && stack exec euler"
done
