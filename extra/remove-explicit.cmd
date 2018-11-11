@echo off

rem ensure current directory is where this file lives
set olddir=%CD%
cd %~dp0
php remove-explicit.php ..\source\
cd %olddir%
pause