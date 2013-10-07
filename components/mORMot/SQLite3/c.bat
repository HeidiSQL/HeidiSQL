@echo off
del sqlite3fts3.obj
del sqlite3.obj
\dev\bcc\bin\bcc32 -6 -O2 -c -d -DSQLITE_ENABLE_FTS3 -u- sqlite3.c
copy sqlite3.obj sqlite3fts3.obj
\dev\bcc\bin\bcc32 -6 -O2 -c -d -u- sqlite3.c
rem \dev\bcc\bin\bcc32 -6 -O2 -c -d -u- -S sqlite3.c
 pause
