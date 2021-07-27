#!/usr/bin/env bash

set -e

fetch_sha() {
    # TODO: test that this nonsense works
    target_sha="$1"
    mkdir -p emacs
    cd emacs
    if [ ! -d .git ]; then
	git init
    fi
    git remote add origin https://github.com/emacs-mirror/emacs
    git fetch origin "$target_sha"
    git reset --hard FETCH_HEAD
    cd ..
}


# TODO: also test that this nonsense works
SHA="db106ea88b41e7b293f18a587cbe43685cb769a6"
if [ -d ./emacs ]; then
    cd emacs
    current_sha=$(git rev-parse HEAD)
    if [ "$SHA" ~= "$current_sha" ]; then
	fetch_sha "$SHA"
    fi
else
    fetch_sha "$SHA"
    cd emacs
fi

./autogen.sh
./configure \
    --with-cairo \
    --with-json \
    --with-native-compilation \
    --with-x \
    --without-mailutils \
    CFLAGS="-O3 -mtune=native -march=native -fomit-frame-pointer"

make
sudo make install
