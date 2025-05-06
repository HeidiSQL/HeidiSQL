#!/bin/sh

# Exit immediately if a command exits with a non-zero status.
set -e

APPNAME=HeidiSQL

if [ ! -f "LICENSE" ]; then
  echo Error: Current dir is not the root: `pwd`. Please run in repository root directory.
  exit
fi

echo -n Enter major + minor version number to use \(e.g. "12.10"\):
read majorMinorVer

echo Reset local modifications and pull latest commits...
git clean -dfxq
git reset --hard
git pull

echo Detect full version...
buildRevision=`git log | grep -E "^commit\s" -c`
fullVer="$majorMinorVer.1.$buildRevision"
echo Full version: $fullVer

echo Pulling translations from Transifex...
extra/internationalization/tx pull -a
echo Compiling .po files to .mo files...
for f in $(find out -type f -iname '*.po'); do
    langCode=`echo $f | sed -nE 's/.*\/(\w+)\/LC_MESSAGES\/.*$/\1/p'`
    msgfmt -o "out/locale/heidisql.$langCode.mo" $f
done

echo Inject version string into .lpi file
# <BuildNr Value="0"/>
sed -i "s/<BuildNr Value=\"[0-9]\+\"/<BuildNr Value=\"$buildRevision\"/g" heidisql.lpi

echo Build executable...
lazbuild heidisql.lpi

# create directory and prepare files
packageDir="$APPNAME_$fullVer"
echo Creating package directory $packageDir
cp -R deb-package-skeleton $packageDir
rm $packageDir/usr/share/heidisql/locale/.gitkeep
rm $packageDir/usr/share/pixmaps/.gitkeep
cp out/*.ini $packageDir/usr/share/heidisql/
cp out/heidisql $packageDir/usr/share/heidisql/
cp out/locale/*.mo $packageDir/usr/share/heidisql/locale/
cp res/deb-package-icon.png $packageDir/usr/share/pixmaps/heidisql.png
cp LICENSE $packageDir/usr/share/doc/heidisql/

echo Inject version number in control file...
sed -i "s/%VERSION%/$majorMinorVer/g" $packageDir/DEBIAN/control

# create deb package
echo Creating package...
sudo chown root:root -R $packageDir
sudo chmod 0755 $packageDir/usr/bin/heidisql
sudo dpkg -b $packageDir

# remove folder structure
echo Remove temp folder structure
sudo rm -rf $packageDir
