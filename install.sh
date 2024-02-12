#!/bin/bash

# Create fig dir containig fig binary files
mkdir $HOME/.fig
mkdir $HOME/.fig/bin

# Get the latest fig release
wget -O $HOME/.fig/bin/fig https://github.com/fig-lang/fig/releases/download/0.1.0/fig

sudo chmod +x $HOME/.fig/bin/fig

echo 'export PATH="$HOME/.fig/bin:$PATH"' >> $HOME/.bashrc

