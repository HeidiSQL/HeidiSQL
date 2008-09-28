; Java Launcher
;--------------
 
Name "jHeidi"
Caption "jHeidi"
Icon "heidi.ico"
OutFile "jheidi.exe"
 
SilentInstall silent
AutoCloseWindow true
ShowInstDetails nevershow
 
!define CLASSPATH "jheidi.jar;lib/ajl.jar;lib/swing-worker-1.1.jar;lib/editor.jar;lib/log4j-1.2.13.jar;lib/mysql-connector-java-5.0.7-bin.jar"
!define CLASS "LaunchJHeidi"
 
Section ""
  Call GetJRE
  Pop $R0
 
  ; change for your purpose (-jar etc.)
  StrCpy $0 '"$R0" -classpath "${CLASSPATH}" ${CLASS}'
 
  SetOutPath $EXEDIR

  ClearErrors
  Exec $0

  IfErrors 0 +2
  messageBox MB_OK "The Java Runtime Environment has not been installed.  Please visit javasoft.com and download the latest version."
   
SectionEnd
 
Function GetJRE
;
;  Find JRE (Java.exe)
;  1 - in .\jre directory (JRE Installed with application)
;  2 - in JAVA_HOME environment variable
;  3 - in the registry
;  4 - assume java.exe in current dir or PATH
 
  Push $R0
  Push $R1
 
  ClearErrors
  StrCpy $R0 "$EXEDIR\jre\bin\javaw.exe"
  IfFileExists $R0 JreFound
  StrCpy $R0 ""
 
  ClearErrors
  ReadEnvStr $R0 "JAVA_HOME"
  StrCpy $R0 "$R0\bin\javaw.exe"
  IfErrors 0 JreFound
 
  ClearErrors
  ReadRegStr $R1 HKLM "SOFTWARE\JavaSoft\Java Runtime Environment" "CurrentVersion"
  ReadRegStr $R0 HKLM "SOFTWARE\JavaSoft\Java Runtime Environment\$R1" "JavaHome"
  StrCpy $R0 "$R0\bin\javaw.exe"
 
  IfErrors 0 JreFound
  StrCpy $R0 "javaw.exe"
        
 JreFound:
  Pop $R1
  Exch $R0
FunctionEnd