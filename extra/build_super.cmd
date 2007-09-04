@echo off

REM =============================================
REM
REM Set compiler_dir and package_dir appropriately.
REM Example:
REM  set compiler_dir=C:\path\to\delphi\bin\directory\
REM  set package_dir=delphi11
REM

set compiler_dir=
set package_dir=

REM =============================================


IF "%compiler_dir%" == "" GOTO usage
IF "%package_dir%" == "" GOTO usage
GOTO start

:usage
ECHO Please modify path and version settings in build_super.cmd.
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
%compiler% -N0"..\..\build" -Q %params% SynEditD.dpk


rem Build ZeosDBO
cd %base_dir%components\zeosdbo\packages\%package_dir%\
%compiler% -N0"..\..\build" -Q %params% ZCore.dpk
%compiler% -N0"..\..\build" -Q %params% ZPlain.dpk
%compiler% -N0"..\..\build" -Q %params% ZParseSql.dpk
%compiler% -N0"..\..\build" -Q %params% ZDbc.dpk
%compiler% -N0"..\..\build" -Q %params% ZComponent.dpk
%compiler% -N0"..\..\build" -Q %params% ZComponentDesign.dpk


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
