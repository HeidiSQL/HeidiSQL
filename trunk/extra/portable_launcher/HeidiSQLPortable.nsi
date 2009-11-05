; **************************************************************************
; Name: HeidiSQL Portable
;
; Website: http://www.heidisql.com/
;
; Written for:
;	NSIS 2.45 or higher
;	Required plugins: Registry, FindProcDLL
;
; License:
;	Copyright© 2009 by HeidiSQL Team
;
; Attention: When doing backups of existing directories or files, this could be a lengthy
;	operation depending on the speed of your USB device. Keep this in mind!
;	The status window showing the copy is enabled
;
; Portable application template created 2006 by Karl Loncarek, version 1.4.1 - 2006/11/30,
; modified by Bruno Soares Pasin and Francisco Ernesto Teixeira for heidisql.com
; License of this template see the respective readme.txt.
; **************************************************************************

; **************************************************************************
; * Define constants 
; **************************************************************************
!define AUTHOR "HeidiSQL Team"
!define APP "HeidiSQL"
!define VER "1.0.0.0"
!define APPVER "4.0"
!define EXE "heidisql.exe"
!define PNAME "${App}Portable"
!define ICON "..\..\res\mainicon.ico"
!define SPLASHIMAGE "${PNAME}.bmp"
!define REGKEYS "HKEY_CURRENT_USER\Software\HeidiSQL"
!define INI "${PNAME}.ini"

; **************************************************************************
; *  Includes
; **************************************************************************
!include "Registry.nsh"
!include "WordFunc.nsh"
!insertmacro "WordFind"
!include "FileFunc.nsh"
!insertmacro "GetParameters"

; **************************************************************************
; *  Runtime Switches
; **************************************************************************
CRCCheck On
WindowIcon Off
SilentInstall Silent
AutoCloseWindow True

; **************************************************************************
; *  Define variables
; **************************************************************************
Var ACTION
Var ACTIONDIRECTORY
Var ACTIONPARAMETERS
Var SPLASHSCREEN
Var PROGRAMEXE
Var PROGRAMDIR
Var PROGRAMPARMS
Var DATADIR
Var INIFILE
Var SECONDLAUNCH

; **************************************************************************
; *  Set basic information
; **************************************************************************
Name "${APP} Portable"
!ifdef ICON
	Icon "${ICON}"
!endif
Caption "${APP} Portable - ${VER}"
OutFile "${PNAME}.exe"

; **************************************************************************
; *  Set version information
; **************************************************************************
LoadLanguageFile "${NSISDIR}\Contrib\Language files\English.nlf"
VIProductVersion "${Ver}"
VIAddVersionKey /LANG=${LANG_ENGLISH} "ProductName" "${APP} ${APPVER} Portable"
VIAddVersionKey /LANG=${LANG_ENGLISH} "Comments" "Allow ${APP} to be run from a removeable drive."
VIAddVersionKey /LANG=${LANG_ENGLISH} "LegalCopyright" "© ${AUTHOR}"
VIAddVersionKey /LANG=${LANG_ENGLISH} "CompanyName" "by ${AUTHOR}"
VIAddVersionKey /LANG=${LANG_ENGLISH} "FileDescription" "${APP} ${APPVER} Portable"
VIAddVersionKey /LANG=${LANG_ENGLISH} "FileVersion" "${VER}"
VIAddVersionKey /LANG=${LANG_ENGLISH} "OriginalFilename" "${PNAME}.exe"

; **************************************************************************
; *  Function: Other initialisations before launch of program
; **************************************************************************
Function Init
FunctionEnd

; **************************************************************************
; *  Function: Other things that have to be cleaned up after end of the program
; **************************************************************************
Function CleanUp
FunctionEnd

; **************************************************************************
; *  Function: Initialize working variables
; **************************************************************************
Function InitVariables
	; --------------------------------------------------------------------------
	; Check whether an INI file exists, set variable pointing to it
	; --------------------------------------------------------------------------
	IfFileExists "$EXEDIR\${INI}" "" CheckPortableINI
		StrCpy "$INIFILE" "$EXEDIR\${INI}"
		Goto ReadINIFile
	CheckPortableINI:
		IfFileExists "$EXEDIR\${PNAME}\${INI}" "" CheckPortableAppsINI
			StrCpy "$INIFILE" "$EXEDIR\${PNAME}\${INI}"
			Goto ReadINIFile
	CheckPortableAppsINI:
		IfFileExists "$EXEDIR\PortableApps\${PNAME}\${INI}" "" CheckAppsINI
			StrCpy "$INIFILE" "$EXEDIR\PortableApps\${PNAME}\${INI}"
			Goto ReadINIFile
	CheckAppsINI:
		IfFileExists "$EXEDIR\Apps\${PNAME}\${INI}" "" CheckDataINI
			StrCpy "$INIFILE" "$EXEDIR\Apps\${PNAME}\${INI}"
			Goto ReadINIFile
	CheckDataINI:
		IfFileExists "$EXEDIR\Data\${PNAME}\${INI}" "" NoINIFile
			StrCpy "$INIFILE" "$EXEDIR\Data\${PNAME}\${INI}"
			Goto ReadINIFile	
	Goto NoINIFile
	; --------------------------------------------------------------------------
	; Read content of the INI file
	; --------------------------------------------------------------------------
	ReadINIFile:
		ReadINIStr $0 "$INIFILE" "${PNAME}" "ProgramDirectory"
		StrCmp $0 "" NoINIFile ; if emtpy retrieve correct setting
		StrCpy "$PROGRAMDIR" "$EXEDIR\$0" ; save program directory
		ReadINIStr $0 "$INIFILE" "${PNAME}" "DataDirectory"
		StrCmp $0 "" NoINIFile ; if empty retrieve correct setting
		StrCpy "$DATADIR" "$EXEDIR\$0" ; save data directory
		ReadINIStr $0 "$INIFILE" "${PNAME}" "ProgramExecutable"
		StrCpy "$PROGRAMEXE" "$0" ; save .exe name
		StrCmp $0 "" "" Splash ; if emtpy use default
		StrCpy "$PROGRAMEXE" "${EXE}" ; set default value
		Splash:
			ReadINIStr $0 "$INIFILE" "${PNAME}" "SplashScreen"
			StrCpy "$SPLASHSCREEN" "$0" ; save state of splashscreen display
			StrCmp "$SPLASHSCREEN" "" "" ProgramParameters ; check whether variable splashscreen was empty
			StrCpy "$SPLASHSCREEN" "enabled" ; set to default
		ProgramParameters:
			ReadINIStr $0 "$INIFILE" "${PNAME}" "ProgramParameters"
			StrCpy "$PROGRAMPARMS" "$0" ; save additional program parameters
		Goto InitVarEnd ; finished reading the INI file
	NoINIFile: ; default settings for all the variables
		; --------------------------------------------------------------------------
		; Set default values for variables
		; --------------------------------------------------------------------------
		StrCpy "$PROGRAMEXE" "${EXE}" ; use default setting
		StrCpy "$PROGRAMPARMS" ""
		StrCpy "$SPLASHSCREEN" "enabled" ; enable splashscreen
		; --------------------------------------------------------------------------
		; Check which directory configuration is used and set variables accordingly
		; --------------------------------------------------------------------------
		IfFileExists "$EXEDIR\App\${APP}\$PROGRAMEXE" "" CheckPortableDIR
			StrCpy "$PROGRAMDIR" "$EXEDIR\App\${APP}"
			StrCpy "$DATADIR" "$EXEDIR\Data"
			Goto InitVarEnd
		CheckPortableDIR:
		IfFileExists "$EXEDIR\${PNAME}\App\${APP}\$PROGRAMEXE" "" CheckPortableAppsDIR
			StrCpy "$PROGRAMDIR" "$EXEDIR\${PNAME}\App\${APP}"
			StrCpy "$DATADIR" "$EXEDIR\${PNAME}\Data"
			Goto InitVarEnd
		CheckPortableAppsDIR:
		IfFileExists "$EXEDIR\PortableApps\${PNAME}\App\${APP}\$PROGRAMEXE" "" CheckAppsDIR
			StrCpy "$PROGRAMDIR" "$EXEDIR\PortableApps\${PNAME}\App\${APP}"
			StrCpy "$DATADIR" "$EXEDIR\PortableApps\${PNAME}\Data"
			Goto InitVarEnd
		CheckAppsDIR:
		IfFileExists "$EXEDIR\Apps\${PNAME}\${APP}\$PROGRAMEXE" "" NoDIR
			StrCpy "$PROGRAMDIR" "$EXEDIR\Apps\${PNAME}\${APP}"
			StrCpy "$DATADIR" "$EXEDIR\Data\${PNAME}"
			Goto InitVarEnd
	NoDIR:
		; --------------------------------------------------------------------------
		; No allowed pathconfiguration was found
		; --------------------------------------------------------------------------
		MessageBox MB_OK|MB_ICONEXCLAMATION `$PROGRAMEXE was not found.  Please check your configuration`
		Abort ; terminate launcher
	InitVarEnd: ;simly the end of the function
FunctionEnd

; **************************************************************************
; *  Function: Work with registry keys or trees (backup, restore, delete)
; **************************************************************************
Function DoAction
	StrCmp "$ACTIONPARAMETERS" "" AfterLoop ; if no parameters provided leave immediately
	StrCpy "$R8" "0" ; reset counter
	StrCpy "$R2" "$ACTION" 6 # ; get only first 6 characters of the required action
	StrCmp "$R2" "Backup" "" GoOn ; backup requires existing directory
	IfFileExists "$ACTIONDIRECTORY\*.*" GoOn ; create backup directory if not existant
		CreateDirectory $ACTIONDIRECTORY
	GoOn:
	StrCpy "$R0" "$ACTIONPARAMETERS" ; copy constant to working register
	Loop:
		; --------------------------------------------------------------------------
		; Get single parameter out of list
		; --------------------------------------------------------------------------
		StrCmp "$R0" "" AfterLoop ; do not do registry parsing, when no keys given anymore
		IntOp $R8 $R8 + 1 ; increase counter
		${WordFind} "$R0" "||" "+01" $R9 ; save first parameter to register
		${WordFind} "$R0" "||" "+02*}"  $R0 ; remove first part from saved value 
		StrCmp "$R0" "$R9" LastLoop ; if register values are identical -> no more delimiters
		Goto DoWork ; do not delete value list
		LastLoop:
		StrCpy "$R0" "" ; if no delimiter available anymore empty working variable
		; --------------------------------------------------------------------------
		;Decide what to do with parameter
		; --------------------------------------------------------------------------
		DoWork:
		StrCpy "$R2" "$ACTION" "" -4 # ; get last 7 characters of action variable
		!ifdef REGKEYS
			StrCmp "$R2" "Keys" DoWorkRegistry ; actions should be performed on the registry
		!endif
		!ifdef SETTINGSFILES
			StrCmp "$R2" "File" DoWorkFiles ; actions should be performed on files
		!endif
		!ifdef SETTINGSDIRS
			StrCmp "$R2" "Dirs" DoWorkDirectory ; actions should be performed on directories
		!endif
		Goto Continue ; something else undefined
		; --------------------------------------------------------------------------
		; Do action on registry keys
		; --------------------------------------------------------------------------
		!ifdef REGKEYS
			DoWorkRegistry:
				StrCmp "$ACTION" "BackupKeys" BackupRegistryKey ; do backup of registry key
				StrCmp "$ACTION" "RestoreKeys" RestoreRegistryKey ; restore registry key, i.e. write them to the registry
				StrCmp "$ACTION" "DeleteKeys" DeleteRegistryKey ; delete registry key from registry
				Goto Continue ; something else undefined
			BackupRegistryKey:
				${registry::KeyExists} "$R9" $R7 ; check whether registry key exists
				StrCmp "$R7" "0" 0 Continue ; registry key does not exist, do not save anything
				${registry::SaveKey} "$R9" "$ACTIONDIRECTORY\RegKey$R8.reg" "/G=1" $R7 ; Backup registry key
				Sleep 50
				Goto Continue
			RestoreRegistryKey:
				IfFileExists "$ACTIONDIRECTORY\RegKey$R8.reg" 0 Continue ; only restore if a registry file exists
				${registry::RestoreKey} "$ACTIONDIRECTORY\RegKey$R8.reg" $R7 ; Restore saved registry key
				Sleep 50
				Goto Continue
			DeleteRegistryKey:
				${registry::DeleteKey} "$R9" $R7 ; Delete registry key
				Sleep 50
				Goto Continue
		!endif
		; --------------------------------------------------------------------------
		; Do action on files
		; --------------------------------------------------------------------------
		!ifdef SETTINGSFILES
			DoWorkFiles:
				StrCmp "$ACTION" "BackupFile" BackupFile ; do backup of file
				StrCmp "$ACTION" "RestoreFile" RestoreFile ; restores saved file, i.e. write it to host computer
				StrCmp "$ACTION" "DeleteFile" DeleteFile ; delete file from hostcomputer
			BackupFile:
				IfFileExists "$R9" 0 Continue ; check whether file exists
				CopyFiles "$R9" "$ACTIONDIRECTORY\File$R8.dat" ; backup file
				Goto Continue
			RestoreFile:
				IfFileExists "$ACTIONDIRECTORY\File$R8.dat" 0 Continue ; only restore when available
				CopyFiles "$ACTIONDIRECTORY\File$R8.dat" "$R9" ; restore file
				Goto Continue
			DeleteFile:
				Delete "$R9" ; delete file
				Goto Continue
		!endif
		; --------------------------------------------------------------------------
		; Do action on directories
		; --------------------------------------------------------------------------
		!ifdef SETTINGSDIRS
			DoWorkDirectory:
				StrCmp "$ACTION" "BackupDirs" BackupDirectory ; do backup of directory
				StrCmp "$ACTION" "RestoreDirs" RestoreDirectory ; restores saved directory, i.e. write it to host computer
				StrCmp "$ACTION" "DeleteDirs" DeleteDirectory ; delete directory from hostcomputer
			BackupDirectory:
				IfFileExists "$R9\*.*" 0 Continue ; check whether source directory exists
				IfFileExists "$ACTIONDIRECTORY\Dir$R8.dat" +2 0 ; does target directory exist?
				CreateDirectory "$ACTIONDIRECTORY\Dir$R8.dat" ; create target directory
				CopyFiles "$R9\*.*" "$ACTIONDIRECTORY\Dir$R8.dat" ; backup directory
				Goto Continue
			RestoreDirectory:
				IfFileExists "$ACTIONDIRECTORY\Dir$R8.dat\*.*" 0 Continue ; check whether backup exists
				IfFileExists "$R9\*.*" +2 0 ; does target directory exist
				CreateDirectory "$R9" ; create target directory
				CopyFiles "$ACTIONDIRECTORY\Dir$R8.dat\*.*" "$R9" ; restore directories
				Goto Continue
			DeleteDirectory:
				RMDir "/r" "$R9" ; delete directory
				Goto Continue
		!endif
		; --------------------------------------------------------------------------
		; End of Loop
		; --------------------------------------------------------------------------
		Continue:
		Goto Loop
	AfterLoop:
FunctionEnd

; **************************************************************************
; * Function: Check whether EXE exists is launched a second time etc.
; **************************************************************************
Function CheckEXE
	IfFileExists "$PROGRAMDIR\$PROGRAMEXE" FoundEXE
		; Program executable not where expected
		MessageBox MB_OK|MB_ICONEXCLAMATION `$PROGRAMEXE was not found. Please check your configuration!`
		Abort
	FoundEXE: ; Check if already running and set variable
		FindProcDLL::FindProc "$PROGRAMEXE"                 
		StrCmp $R0 "1" "" EndEXE
			StrCpy $SECONDLAUNCH "true"
	EndEXE:
FunctionEnd

; **************************************************************************
; *  Main section
; **************************************************************************
Section "Main"
	; --------------------------------------------------------------------------
	; Initialize variables, read INI etc.
	; --------------------------------------------------------------------------
	Call InitVariables
	; --------------------------------------------------------------------------
	; Do some EXE checking
	; --------------------------------------------------------------------------
	Call CheckEXE
	; --------------------------------------------------------------------------
	; Display Splashscreen when available
	; --------------------------------------------------------------------------
	!ifdef SPLASHIMAGE
		StrCmp "$SPLASHSCREEN" "enabled" 0 NoSplash
		InitPluginsDir
		File /oname=$PLUGINSDIR\splash.bmp "${SPLASHIMAGE}"	
    advsplash::show 1000 600 400 -1 $PLUGINSDIR\splash
		NoSplash:
	!endif
	StrCmp $SECONDLAUNCH "true" SimplyStartProgram
	; --------------------------------------------------------------------------
	; Backup existing, replace with saved registry keys
	; --------------------------------------------------------------------------
	!ifdef REGKEYS
		StrCpy "$ACTIONPARAMETERS" "${REGKEYS}" ; hand over parameters list
		StrCpy "$ACTIONDIRECTORY" "$DATADIR\RegistryBackup" ; define where to backup keys
		StrCpy "$ACTION" "BackupKeys" ; backup existing registry keys
		Call DoAction ; just do it
;		StrCpy "$ACTION" "DeleteKeys" ; delete existing registry keys
;		Call DoAction
		StrCpy "$ACTIONDIRECTORY" "$DATADIR\Registry" ; define where local keys are
		StrCpy "$ACTION" "RestoreKeys" ;restore saved keys
		Call DoAction ; just do it
	!endif
	; --------------------------------------------------------------------------
	; Backup existing, replace with saved settings files
	; --------------------------------------------------------------------------
	!ifdef SETTINGSFILES
		StrCpy "$ACTIONPARAMETERS" "${SETTINGSFILES}" ; hand over parameters list
		StrCpy "$ACTIONDIRECTORY" "$DATADIR\SettingsBackup" ; define where to save files
		StrCpy "$ACTION" "BackupFile" ; define action
		Call DoAction ; just do it
		StrCpy "$ACTIONDIRECTORY" "$DATADIR\Settings" ; define where local files are
		StrCpy "$ACTION" "RestoreFile" ; define action
		Call DoAction ; just do it
	!endif
	; --------------------------------------------------------------------------
	; Backup existing settings directories
	; --------------------------------------------------------------------------
	!ifdef SETTINGSDIRS
		StrCpy "$ACTIONPARAMETERS" "${SETTINGSDIRS}" ; hand over parameters list
		StrCpy "$ACTIONDIRECTORY" "$DATADIR\SettingsBackup" ; define where to save directories
		StrCpy "$ACTION" "BackupDirs" ; define action
		Call DoAction ; just do it
		StrCpy "$ACTIONDIRECTORY" "$DATADIR\Settings" ; define where local directories are
		StrCpy "$ACTION" "RestoreDirs" ; define action
		Call DoAction ; just do it
	!endif
	; --------------------------------------------------------------------------
	; Other changes before start
	; --------------------------------------------------------------------------
	Call Init
	; --------------------------------------------------------------------------
	; Start program
	; --------------------------------------------------------------------------
	${GetParameters} $R0 ; obtain eventually provided commandline parameters
	ExecWait "$PROGRAMDIR\$PROGRAMEXE $PROGRAMPARMS $R0" ; run program
	; --------------------------------------------------------------------------
	; Other clean up action after program has run
	; --------------------------------------------------------------------------
	Call CleanUp
	; --------------------------------------------------------------------------
	; backup and restore or clean up settings directories
	; --------------------------------------------------------------------------
	!ifdef SETTINGSDIRS
		StrCpy "$ACTIONPARAMETERS" "${SETTINGSDIRS}" ; hand over parameters list
		StrCpy "$ACTIONDIRECTORY" "$DATADIR\Settings" ; define where local directories are
		StrCpy "$ACTION" "BackupDirs" ; define action
		Call DoAction ; just do it
		StrCpy "$ACTION" "DeleteDirs" ; delete directories on host computer
		Call DoAction ; just do it
		StrCpy "$ACTIONDIRECTORY" "$DATADIR\SettingsBackup" ; define where to save directories
		StrCpy "$ACTION" "RestoreDirs" ; define action
		Call DoAction ; just do it
	!endif
	; --------------------------------------------------------------------------
	; backup and restore or clean up settings files
	; --------------------------------------------------------------------------
	!ifdef SETTINGSFILES
		StrCpy "$ACTIONPARAMETERS" "${SETTINGSFILES}" ; hand over parameters list
		StrCpy "$ACTIONDIRECTORY" "$DATADIR\Settings" ; define where local files are
		StrCpy "$ACTION" "BackupFile" ; define action
		Call DoAction ; just do it
		StrCpy "$ACTION" "DeleteFile" ; delete files on host computer
		Call DoAction ; just do it
		StrCpy "$ACTIONDIRECTORY" "$DATADIR\SettingsBackup" ; define where to save directories
		StrCpy "$ACTION" "RestoreFile" ; define action
		Call DoAction ; just do it
	!endif
	; --------------------------------------------------------------------------
	; backup and restore or clean up registry keys
	; --------------------------------------------------------------------------
	!ifdef REGKEYS
		StrCpy "$ACTIONPARAMETERS" "${REGKEYS}" ; hand over parameters list
		StrCpy "$ACTIONDIRECTORY" "$DATADIR\Registry" ; define where local keys are
		StrCpy "$ACTION" "BackupKeys" ; backup existing registry keys
		Call DoAction ; just do it
		StrCpy "$ACTION" "DeleteKeys" ; delete registry keys on host computer
		Call DoAction ; just do it
		sleep 500 ; let deletion of registry keys finish
		StrCpy "$ACTIONDIRECTORY" "$DATADIR\RegistryBackup" ; define where to backup keys
		StrCpy "$ACTION" "RestoreKeys" ;restore saved keys
		Call DoAction ; just do it
		sleep 1000 ; let registry writing finish (necessary before deleting backup file)
	!endif
	${registry::Unload}
	; --------------------------------------------------------------------------
	; remove saved backups
	; --------------------------------------------------------------------------
	RMDir "/r" "$DATADIR\SettingsBackup"
	RMDir "/r" "$DATADIR\RegistryBackup"
	Goto EndOfCode
	; --------------------------------------------------------------------------
	; run application directly without doing anything else, as it is already running
	; --------------------------------------------------------------------------
	SimplyStartProgram:
		${GetParameters} $R0 ; obtain eventually provided commandline parameters
		Exec "$PROGRAMDIR\$PROGRAMEXE $PROGRAMPARMS $R0" ; run program
		${registry::Unload}
	EndOfCode:
SectionEnd
