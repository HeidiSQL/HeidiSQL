@echo off
setlocal enableextensions
setlocal enabledelayexpansion

cls

:test_dcc32
dcc32.exe --version >NUL: 2>NUL:
if %errorlevel% == 0 goto test_svn

:dcc32_not_found
echo Error: Delphi compiler 'dcc32.exe' was not found in PATH.
echo.
echo Please install Delphi.  When installing, Delphi should modify
echo your system path to include the location of this file.
echo.
echo See also:
echo http://www.codegear.com/downloads/free/delphi
echo.
pause > NUL:
goto :eof

:test_svn
svnversion.exe --version >NUL: 2>NUL:
if %errorlevel% == 0 goto test_libs

:svn_not_found
echo Error: Subversion executable 'svnversion.exe' was not found in PATH.
echo.
echo Please install Subversion.  When installing, Subversion should modify
echo your system path to include the location of this file.
echo.
echo See also:
echo http://subversion.tigris.org/servlets/ProjectDocumentList?folderID=91^&filter=setup.exe
echo.
pause > NUL:
goto :eof

:locate_dcc32
if not "%compiler_dir%" == "none" goto :eof
pushd %~f1
if exist dcc32.exe set compiler_dir=found
if "%compiler_dir%" == "found" cd ..
if "%compiler_dir%" == "found" set compiler_dir=%CD%
popd
goto :eof

:test_libs
SET compiler_dir=none
for %%p in ("%PATH:;=" "%") do call :locate_dcc32 %%p
if "%compiler_dir%" == "none" (goto dcc32_not_found) else (goto test_packages)

:test_packages
set package_dir=none
dcc32.exe --version | find "18.0" >NUL:
if %errorlevel% == 0 set package_dir=delphi10
dcc32.exe --version | find "18.5" >NUL:
if %errorlevel% == 0 set package_dir=delphi11
if "%package_dir%" == "none" (goto unknown_version) else (goto init)

:unknown_version
echo Error: Unknown version of Delphi compiler 'dcc32.exe'!
echo.
echo At present, only Delphi 10 (Borland Developer Studio 2006)
echo and Delphi 11 (CodeGear Delphi 2007).
echo.
echo Please install one of those...
echo.
echo If you think you are receiving this message in error,
echo please inform the developers at heidisql-devel@sourceforge.net.
echo.
pause >NUL:
goto :eof

:init
set start_time=%DATE% %TIME%
set start_dir=%CD%
cd ..
set base_dir=%CD%

set compiler=dcc32.exe
set params=--no-config
set params=%params% -aWinTypes=Windows;WinProcs=Windows;DbiProcs=BDE;DbiTypes=BDE;DbiErrs=BDE
set params=%params% -B
set params=%params% -u"%compiler_dir%\lib;%compiler_dir%\lib\obj;%base_dir%\components\zeosdbo\build;%base_dir%\components\virtualtreeview\build;%base_dir%\components\synedit\build;%base_dir%\components\smdbgrid\build;%base_dir%\components\heidisql\build;%base_dir%\components\edbimage\build"
set params=%params% -i"%base_dir%\components\compilerdetection\include;%base_dir%\components\heidisql\include"
set params=%params% -LE"%base_dir%\build"
set params=%params% -LN"%base_dir%\build"
set params=%params% -r"%base_dir%\components\smdbgrid\Resources;%base_dir%\components\synedit\resources;%base_dir%\components\virtualtreeview\Resources;%base_dir%\components\edbimage\resources"

echo.
echo Base directory:          %base_dir%
echo Compiler directory:      %compiler_dir%
echo.

:start
rem Delete old binaries
cd /d "%base_dir%"
del /S *.dcu
del /S *.dcp
del /S *.bpl
del /S out\heidisql.exe

:find_wcver
rem Unix tool, may not handle Windows paths well, so go to base directory and use dot.
cd /d "%base_dir%"
for /f "usebackq" %%s in (`svnversion.exe . ^|^| ECHO unknown`) DO SET wcver=WC %%s
if "%wcver%" == "WC unknown" (set wcver=unknown) else (goto insert_wcver)

:svnversion_failure
rem Non-fatal, continue if this happens.
echo Error: svnversion failed - run this step manually to see what went wrong?
echo.

:insert_wcver
rem Put WC version or "unknown" into main.pas
%base_dir%\extra\sed\sed.exe "s/\$Revision.*\$/\$Revision: %WCVER% \$/g" -i "%base_dir%\source\main.pas"
if not %errorlevel% == 0 goto sedfail
%base_dir%\extra\sed\sed.exe "s/\$Rev[^i].*\$/\$Rev: %WCVER% \$/g" -i "%base_dir%\source\main.pas"
if not %errorlevel% == 0 goto sedfail
goto after_wcver

:sedfail
echo Error: SED failure - run this step manually to see what went wrong?
echo.
goto end

:after_wcver
echo.

rem Build EDBImage
cd /d "%base_dir%\components\edbimage\packages\%package_dir%\"
"%compiler%" -N0"..\..\build" %params% VCLSer.dpk
if not %errorlevel% == 0 goto end
"%compiler%" -N0"..\..\build" %params% DCLSer.dpk
if not %errorlevel% == 0 goto end


rem Build SMDBGrid
cd /d "%base_dir%\components\smdbgrid\packages\%package_dir%\"
"%compiler%" -N0"..\..\build" %params% SMDBGridComponents.dpk
if not %errorlevel% == 0 goto end


rem Build SynEdit
cd /d "%base_dir%\components\synedit\packages\%package_dir%\"
rem -Q = Quiet compile
rem      This is a workaround for avoiding error D21153
rem      see here: http://qc.borland.com/wc/qcmain.aspx?d=44731
"%compiler%" -N0"..\..\build" -Q %params% SynEditR.dpk
if not %errorlevel% == 0 goto end


rem Build ZeosDBO
cd /d "%base_dir%\components\zeosdbo\packages\%package_dir%\"
"%compiler%" -N0"..\..\build" -Q %params% ZCore.dpk
if not %errorlevel% == 0 goto end
"%compiler%" -N0"..\..\build" -Q %params% ZPlain.dpk
if not %errorlevel% == 0 goto end
"%compiler%" -N0"..\..\build" -Q %params% ZParseSql.dpk
if not %errorlevel% == 0 goto end
"%compiler%" -N0"..\..\build" -Q %params% ZDbc.dpk
if not %errorlevel% == 0 goto end
"%compiler%" -N0"..\..\build" -Q %params% ZComponent.dpk
if not %errorlevel% == 0 goto end


rem Build HeidiComponents
cd /d "%base_dir%\components\heidisql\packages\%package_dir%\"
"%compiler%" -N0"..\..\build" %params% HeidiComponents.dpk
if not %errorlevel% == 0 goto end


rem Build VirtualTreeView
cd /d "%base_dir%\components\virtualtreeview\packages\%package_dir%\"
"%compiler%" -N0"..\..\build" %params% VirtualTreesR.dpk
if not %errorlevel% == 0 goto end


rem Build main executable
cd /d "%base_dir%\packages\%package_dir%\"
"%compiler%" -N0"..\..\build" -e"%base_dir%\out" %params% heidisql.dpr
if not %errorlevel% == 0 goto end


echo.
echo Started:   %start_time%
echo Finished:  %DATE% %TIME%
echo.


:end
cd /d "%start_dir%" 2>NUL:
