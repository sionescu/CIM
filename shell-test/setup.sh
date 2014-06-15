#!/bin/bash

if [ ! -e shunit2 ]
then
    dist=shunit2-2.1.6
    wget "https://shunit2.googlecode.com/files/$dist.tgz" -O $dist.tgz
    tar xf $dist.tgz
    ln -s $dist shunit2
fi

# make a shortcut to shunit2
ln -s shunit2/src/shunit2 loader

./install.sh
source ~/.bashrc
./easy-update.sh # ensures the latest lib/ is used
