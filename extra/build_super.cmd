@echo off
setlocal enableextensions
setlocal enabledelayexpansion

CLS

REM =============================================
REM
REM Set compiler_dir and compiler_version appropriately via command line.
REM Example:
REM  build_super.cmd C:\path\to\delphi\bin\directory\ delphi11
REM

set compiler_dir=%1
set package_dir=%2

REM =============================================

IF %compiler_dir%. == . GOTO nostart
IF NOT %package_dir%. == . GOTO start

:nostart
ECHO =================== Note ===================
ECHO If you are being asked about paths, you can also enter them
ECHO directly into the build script, or provide them on the command line.
ECHO.
ECHO For help with building individual packages, see the developer readme.
ECHO ============================================
ECHO.
ECHO.

IF %compiler_dir%. == . SET /P compiler_dir=Please enter path to delphi bin folder: 
IF %package_dir%. == . SET /P package_dir=Please enter compiler version (eg delphi11): 

set compiler_dir=%compiler_dir:"=%
set package_dir=%package_dir:"=%

IF "%compiler_dir%" == "" GOTO usage
IF "%package_dir%" == "" GOTO usage
GOTO start

:usage
ECHO.
ECHO Please pass compiler_dir AND package_dir via
ECHO command line or supply them when prompted.
ECHO.
PAUSE
ECHO.
GOTO end

:start
set start_time=%DATE% %TIME%
set start_dir=%CD%
cd ..
set base_dir=%CD%
cd %compiler_dir%
set compiler_bin_dir=%CD%
cd ..
set compiler_dir=%CD%
set compiler=%compiler_bin_dir%\dcc32.exe

set params=-aWinTypes=Windows;WinProcs=Windows;DbiProcs=BDE;DbiTypes=BDE;DbiErrs=BDE
set params=%params% -B
set params=%params% -u"%compiler_dir%\lib;%compiler_dir%\lib\Obj;%base_dir%\components\zeosdbo\build;%base_dir%\components\virtualtreeview\build;%base_dir\%components\synedit\build;%base_dir%\components\smdbgrid\build;%base_dir%\components\heidisql\build;%base_dir%\components\edbimage\build;"
set params=%params% -i"%base_dir%\components\compilerdetection;%base_dir%source"
set params=%params% -LE"%base_dir%\build"
set params=%params% -LN"%base_dir%\build"
set params=%params% -r"%base_dir%\source;%base_dir%\components\smdbgrid\Resources;%base_dir%\components\synedit\resources;%base_dir%\components\synedit\Source;%base_dir%\components\virtualtreeview\Resources;%base_dir%\components\edbimage\resources"

echo.
echo Base directory:          %base_dir%
echo Compiler directory:      %compiler_dir%
echo Compiler bin directory:  %compiler_bin_dir%
echo Compiler binary:         %compiler%
echo.

rem Switch to base directory
cd %base_dir%

rem Delete old binaries
del /S *.dcu
del /S *.dcp
del /S *.bpl
echo.

SET wcver=unknown
REM Check that svnversion exists
set svndir=%ProgramFiles%\Subversion\bin
"%svndir%\svnversion" --version > NUL:
IF %errorlevel% == 0 GOTO svn_good
set svndir=%ProgramFiles(x86)%\Subversion\bin
"%svndir%\svnversion" --version > NUL:
IF %errorlevel% == 0 GOTO svn_good

ECHO Please install subversion from http://subversion.tigris.org/
ECHO and make sure that svnversion.exe is in:
ECHO "%ProgramFiles%\Subversion\bin\"
ECHO.
GOTO after_wcver

:svn_good
rem Unix tool, may not handle Windows paths well, so go to directory and use dot.
CD "%base_dir%"
FOR /F "usebackq" %%s IN (`"%svndir%\svnversion" . ^|^| ECHO unknown`) DO SET wcver=WC %%s
IF NOT "%wcver%" == "WC unknown" GOTO go_wcver
SET wcver=unknown
ECHO svnversion failure - run this step manually to see what went wrong?
ECHO.
GOTO after_wcver

:go_wcver
rem Put WC version or "unknown" into main.pas
%base_dir%\extra\sed\sed.exe "s/\$Revision.*\$/\$Revision: %WCVER% \$/g" -i "%base_dir%\source\main.pas"
IF NOT %errorlevel% == 0 GOTO sedfail
%base_dir%\extra\sed\sed.exe "s/\$Rev[^i].*\$/\$Rev: %WCVER% \$/g" -i "%base_dir%\source\main.pas"
IF NOT %errorlevel% == 0 GOTO sedfail
GOTO wc_ver_good

:sedfail
ECHO SED failure - run this step manually to see what went wrong?
GOTO end

:after_wcver
echo.

rem Build EDBImage
cd "%base_dir%\components\edbimage\packages\%package_dir%\"
"%compiler%" -N0"..\..\build" %params% VCLSer.dpk
if not %errorlevel% == 0 goto end
"%compiler%" -N0"..\..\build" %params% DCLSer.dpk
if not %errorlevel% == 0 goto end


rem Build SMDBGrid
cd "%base_dir%\components\smdbgrid\packages\%package_dir%\"
"%compiler%" -N0"..\..\build" %params% SMDBGridComponents.dpk
if not %errorlevel% == 0 goto end


rem Build SynEdit
cd "%base_dir%\components\synedit\packages\%package_dir%\"
rem -Q = Quiet compile
rem      This is a workaround for avoiding error D21153
rem      see here: http://qc.borland.com/wc/qcmain.aspx?d=44731
"%compiler%" -N0"..\..\build" -Q %params% SynEditR.dpk
if not %errorlevel% == 0 goto end


rem Build ZeosDBO
cd "%base_dir%\components\zeosdbo\packages\%package_dir%\"
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
cd "%base_dir%\components\heidisql\packages\%package_dir%\"
"%compiler%" -N0"..\..\build" %params% HeidiComponents.dpk
if not %errorlevel% == 0 goto end


rem Build VirtualTreeView
cd "%base_dir%\components\virtualtreeview\packages\%package_dir%\"
"%compiler%" -N0"..\..\build" %params% VirtualTreesR.dpk
if not %errorlevel% == 0 goto end


rem Build main executable
cd "%base_dir%\packages\%package_dir%\"
"%compiler%" -N0"..\..\build" -e"%base_dir%\out" %params% heidisql.dpr
if not %errorlevel% == 0 goto end

echo.
echo Started:   %start_time%
echo Finished:  %DATE% %TIME%

:end
cd "%base_dir%\extra" 2>NUL:
