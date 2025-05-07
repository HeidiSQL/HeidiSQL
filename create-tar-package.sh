#!/bin/sh

echo Creates a tar.xz file for release.
echo Note the binary and translation files need to be in place and compiled, which is both done by the .deb creator script.
echo

echo -n Enter full version number to use \(e.g. "12.10.1.133"\):
read fullVer
tarFilename="HeidiSQL_$fullVer.tar.xz"
cd out
tar -cvJf $tarFilename heidisql functions*.ini locale/heidisql.*.mo
mv $tarFilename ../
cd ..
