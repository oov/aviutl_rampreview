#!/bin/bash

mkdir bin

# copy readme
sed 's/\r$//' README.md | sed 's/$/\r/' > bin/README.txt

# update version string
VERSION='v0.1.1alpha'
GITHASH=`git rev-parse --short HEAD`
cat << EOS | sed 's/\r$//' | sed 's/$/\r/' > 'src/lazarus/ver.pas'
unit Ver;

{\$mode objfpc}{\$H+}
{\$CODEPAGE UTF-8}

interface

const
  Version = '$VERSION ( $GITHASH )';

implementation

end.
EOS
cat << EOS | sed 's/\r$//' | sed 's/$/\r/' > 'src/go/ver.go'
package main

const version = "$VERSION ( $GITHASH )"
EOS

# build ZRamPreview.exe
pushd src/go
go.exe build -x -ldflags="-s" -o ../../bin/ZRamPreview.exe
popd

# build lazarus projects
cmd.exe /c C:/lazarus/lazbuild.exe --build-all src/lazarus/RamPreview.lpi

# install
# cp bin/ZRamPreview.auf aviutl/
# cp bin/ZRamPreview.exe aviutl/
