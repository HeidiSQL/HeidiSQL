#!/bin/sh

APPNAME=HeidiSQL

if [ ! -f "LICENSE" ]; then
  echo Error: Current dir is not the root: `pwd`. Please run in repository root directory.
  exit
fi
if [ ! -f "out/heidisql" ]; then
  echo Error: compiled binary does not exist
  exit
fi

echo -n Enter version number to use \(e.g. "12.10"\): 
read versionStr

# create directory and prepare files
packageDir="$APPNAME-$versionStr"
echo Creating package directory $packageDir
cp -R deb-package-skeleton $packageDir
rm $packageDir/usr/share/heidisql/.gitkeep
rm $packageDir/usr/share/pixmaps/.gitkeep
cp out/*.ini $packageDir/usr/share/heidisql/
cp out/heidisql $packageDir/usr/share/heidisql/
cp res/deb-package-icon.png $packageDir/usr/share/pixmaps/heidisql.png
cp LICENSE $packageDir/usr/share/doc/heidisql/

echo Inject version number in control file...
sed -i "s/%VERSION%/$versionStr/g" $packageDir/DEBIAN/control

# create deb package
echo Creating package...
sudo chown root:root -R $packageDir
sudo chmod 0755 $packageDir/usr/bin/heidisql
sudo dpkg -b $packageDir

# remove folder structure
echo Remove temp folder structure
sudo rm -rf $packageDir
