#!/usr/bin/env bash

set -e

#==============
# Remove old dot flies
#==============


ignoreFiles=(`basename "$0"` ".git")

# make sure we are on the dotfiles folder
cd ~/.dotfiles

for file in * .[^.]*; do
    if [[ " ${ignoreFiles[*]} " =~ " ${file} " ]]; then
        continue
    fi

    echo "Symlinking $file"
    # remove existing symlink
    rm -rf ~/."$file" > /dev/null 2>&1
    # create new link
    ln -sf ~/.dotfiles/"$file" ~/."$file"
done

# bin is not a dotfolder
mv ~/.bin ~/bin

echo -e "\n====== Done! ======\n"

source ~/.bashrc
