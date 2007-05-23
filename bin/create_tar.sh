#!/bin/bash

NAME=es4-ri

cd ..
cp -R build $NAME
tar --create --file="$NAME".tar $NAME
gzip -f "$NAME".tar
rm -r $NAME
