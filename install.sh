#!/bin/bash

echo "creating alacritty and nvim folders under ~/.config"

mkdir -p ~/.config/alacritty
mkdir -p ~/.config/nvim

echo "symlinking configs to ~/.config"

ln -s $(pwd)/alacritty ~/.config/alacritty
ln -s $(pwd)/nvim ~/.config/nvim
