jHeidi Alpha 3 Update 4 Release Notes
=====================================

New in this release is the Auto Update Check functionality
allowing you to check for updates and install newer versions
of jHeidi as they become available.

Change log:
338: timestamp value '0000-00-00 00:00:00' causes exception
333: database names are not properly escaped
fix download url
add user agent to http calls
Fix broken urls
create table field list broken
create table dlg: add error messages for adding a field
launch java download link when NSIS cant find Java
add popup error when NSIS cant launch java
fix null pointer when saving table with one field
make autoupdate check async
Add error handling for autoupdate check
ajl.cmd was not a variable
fix bug in bash scripts
fix error on initial run from source because classes dir doesnt exist
fix osx startup scripts
add prop files for src download
add autoupdater tasks
add setup-exe upload target
delete misplaced file
keep log.xml in log dir so svn downloaders can run jheidi as is
add required libraries to build
add innosetup to build process for windows setup.exe
add jheidi.exe make to build
fix windows cmd args
fix several build errors
remove uneccesary file extension
moved rel notes to etc
autoupdater moved to its own project
add osx java launcher
add jheidi jar launcher
add jheidi jar manifest



