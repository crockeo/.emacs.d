#!/usr/bin/env bash



if [ ! -d ./emacs ]; then
    git clone https://git.savannah.gnu.org/git/emacs.git
fi
cd emacs
git pull origin master

# make clean 2> /dev/null
./autogen.sh
./configure \
    --with-cairo \
    --with-json \
    --with-native-compilation \
    --with-x \
    --with-xwidgets \
    --without-mailutils \
    CFLAGS="-O3 -mtune=native -march=native -fomit-frame-pointer"

make
make install
