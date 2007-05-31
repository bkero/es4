#!/bin/bash

NAME=es4-ri

cd ..
cp -R build $NAME
tar --create --file="$NAME".tar --exclude=_MTN --exclude *heap* $NAME
gzip -f "$NAME".tar
rm -r $NAME
