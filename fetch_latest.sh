#!/usr/bin/env bash


fetch_sha() {
    # TODO: test that this nonsense works
    target_sha="$1"
    mkdir -p emacs
    cd emacs
    if [ ! -d .git ]; then
	git init
    fi
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
fi

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
