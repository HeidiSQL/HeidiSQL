@echo off

REM =============================================
REM
REM Set compiler_dir and package_dir appropriately via command line.
REM Example:
REM  build_super.cmd C:\path\to\delphi\bin\directory\ delphi11
REM

set compiler_dir=%1
set package_dir=%2

REM =============================================


IF %compiler_dir%. == . GOTO usage
IF %package_dir%. == . GOTO usage
GOTO start

:usage
ECHO Please pass compiler_dir AND package_dir via command line.
ECHO base_dir is automatically derived from current working directory.
ECHO.
PAUSE
GOTO end

:start
set compiler=%compiler_dir%dcc32.exe
set start_dir=%CD%
set base_dir=%start_dir%\..\

rem Switch to base directory
cd %base_dir%

rem Delete old binaries
del /S *.dcu

set params=-aWinTypes=Windows;WinProcs=Windows;DbiProcs=BDE;DbiTypes=BDE;DbiErrs=BDE
set params=%params% -B
set params=%params% -u"%compiler_dir%..\lib;%compiler_dir%..\lib\Obj;%base_dir%components\zeosdbo\build;%base_dir%components\virtualtreeview\build;%base_dir%components\synedit\build;%base_dir%components\smdbgrid\build;%base_dir%components\heidisql\build;%base_dir%components\edbimage\build;"
set params=%params% -i"%base_dir%components\compilerdetection;%base_dir%source"
set params=%params% -LE"%base_dir%build"
set params=%params% -LN"%base_dir%build"
set params=%params% -r"%base_dir%source;%base_dir%components\smdbgrid\Resources;%base_dir%components\synedit\resources;%base_dir%components\synedit\Source;%base_dir%components\virtualtreeview\Resources;%base_dir%components\edbimage\resources"

REM Check that svnversion exists
svnversion --version > NUL:
IF %errorlevel% == 0 GOTO svn_good

ECHO Please install subversion from http://subversion.tigris.org/
ECHO and make sure that svnversion.exe is in:
ECHO "%ProgramFiles%\Subversion\bin\"
GOTO end

:svn_good
rem Put WC version into main.pas
SET WCVER=
%base_dir%extra\EnvPipe\EnvPipe.exe WCVER "%ProgramFiles%\Subversion\bin\svnversion.exe" "%base_dir%"
IF NOT %errorlevel% == 0 GOTO pipefail

%base_dir%extra\sed\sed.exe "s/\$Revision.*\$/\$Revision: WC %WCVER% \$/g" -i "%base_dir%\source\main.pas"
IF NOT %errorlevel% == 0 GOTO sedfail
%base_dir%extra\sed\sed.exe "s/\$Rev[^i].*\$/\$Rev: WC %WCVER% \$/g" -i "%base_dir%\source\main.pas"

IF NOT %errorlevel% == 0 GOTO sedfail
GOTO wc_ver_good

:pipefail
ECHO EnvPipe or svnversion failure - run this step manually to see what went wrong?
GOTO end

:sedfail
ECHO SED failure - run this step manually to see what went wrong?
GOTO end

:wc_ver_good

rem Build EDBImage
cd %base_dir%components\edbimage\packages\%package_dir%\
%compiler% -N0"..\..\build" %params% VCLSer.dpk
%compiler% -N0"..\..\build" %params% DCLSer.dpk


rem Build SMDBGrid
cd %base_dir%components\smdbgrid\packages\%package_dir%\
%compiler% -N0"..\..\build" %params% SMDBGridComponents.dpk


rem Build SynEdit
cd %base_dir%components\synedit\packages\%package_dir%\
rem -Q = Quiet compile
rem      This is a workaround for avoiding error D21153
rem      see here: http://qc.borland.com/wc/qcmain.aspx?d=44731
%compiler% -N0"..\..\build" -Q %params% SynEditR.dpk


rem Build ZeosDBO
cd %base_dir%components\zeosdbo\packages\%package_dir%\
%compiler% -N0"..\..\build" -Q %params% ZCore.dpk
%compiler% -N0"..\..\build" -Q %params% ZPlain.dpk
%compiler% -N0"..\..\build" -Q %params% ZParseSql.dpk
%compiler% -N0"..\..\build" -Q %params% ZDbc.dpk
%compiler% -N0"..\..\build" -Q %params% ZComponent.dpk


rem Build HeidiComponents
cd %base_dir%components\heidisql\packages\%package_dir%\
%compiler% -N0"..\..\build" %params% HeidiComponents.dpk


rem Build VirtualTreeView
cd %base_dir%components\virtualtreeview\packages\%package_dir%\
%compiler% -N0"..\..\build" %params% VirtualTreesR.dpk


rem Build main executable
cd %base_dir%\packages\%package_dir%\
%compiler% -N0"..\..\build" -e"%base_dir%out" %params% heidisql.dpr

cd %start_dir%


:end
CD extra
