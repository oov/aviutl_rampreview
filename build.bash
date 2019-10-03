#!/bin/bash

mkdir -p bin/script

# copy readme
sed 's/\r$//' README.md | sed 's/$/\r/' > bin/ZRamPreview.txt

# update version string
VERSION='v0.3rc7'
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
pushd src/go > /dev/null
go.exe build -x -ldflags="-s" -o ../../bin/ZRamPreview.exe
popd > /dev/null

# build lazarus projects
cmd.exe /c C:/lazarus/lazbuild.exe --build-all src/lazarus/RamPreview.lpi
cmd.exe /c C:/lazarus/lazbuild.exe --build-all src/lazarus/Output.lpi
cmd.exe /c C:/lazarus/lazbuild.exe --build-all src/lazarus/Extram.lpi

# install
# mkdir -p aviutl/script
# cp bin/ZRamPreview.auf aviutl/
# cp bin/ZRamPreview.auo aviutl/
# cp bin/ZRamPreview.exe aviutl/
# cp bin/script/Extram.dll aviutl/script/
