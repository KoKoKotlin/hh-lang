#!/bin/sh

set -x

cp hh-lang $HOME/.vscode/extensions -r

ls /usr/local/lib/hh-stdlib || mkdir -p /usr/local/lib/hh-stdlib
cp stdlib/* /usr/local/lib/hh-stdlib
