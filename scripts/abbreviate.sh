#!/bin/bash

targetDir=abbreviated

for dir in {Classes,Freer,Mtl}; do
    fullDir=$targetDir/$dir
    if [ ! -d $fullDir ]; then
        mkdir -p $fullDir
    fi
done

for file in src/{Freer,Classes,Mtl}/{Countdown,Cross,Exception,Reader,State,StateTH,Writer}.hs; do
    target=$(echo $file | sed 's_[^/]*_'$targetDir'_')
    cat $file | scripts/remove-up-to-last-import.sh > $target
done

