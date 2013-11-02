Some infos around HeidiSQL

Project website: http://www.heidisql.com/
Google Code: http://code.google.com/p/heidisql/


*** What is HeidiSQL?

HeidiSQL is an easy-to-use interface and a "working-horse" for web-developers
using the popular MySQL-Database. It allows you to manage and browse your
databases and tables from an intuitive Windows® interface.

With HeidiSQL you will also be able to 
* Manage Microsoft SQL Servers, with a reduced feature set
* Generate nice SQL-exports
* Synchronize tables between two databases
* Manage user-privileges
* Import text-files
* Export grid-data as CSV, HTML, XML, SQL, LaTeX, Wiki and PHP Array style
* Browse and edit table-data using a comfortable grid
* Create and edit tables, views, stored routines, triggers and scheduled events
* Bulk edit tables (move to db, change engine, collation etc.)
* Batch-insert ascii or binary files into tables
* Write queries with customizable syntax-highlighting and code-completion
* Profile your queries
* Pretty reformat disordered SQL
* Monitor and kill client-processes
* Connect to servers via commandline
* Find specific text in all tables of all databases of one server
* Optimize and repair tables in a batch manner
* And much more 


*** Requirements:

HeidiSQL runs fine on Windows XP, Vista and 7. Running HeidiSQL on Wine/Linux
also works fine.

On Windows 2000 you might run into some "illegal function call into KERNEL.DLL",
which is caused by the newer libmysql.dll which dropped Win2k support. You can fix
that by placing an older one into the HeidiSQL directory, overwriting the original:
http://heidisql.googlecode.com/svn-history/r3106/trunk/out/libmysql.dll

HeidiSQL does not run on Windows 95/98 or ME, as the Unicode extensions are not
available on these systems.


*** Command line switches

Although HeidiSQL is a pure GUI application, it can be automated for connecting
and opening files via command line parameters. Parameter names are case sensitive
and are based on those used by the MySQL command line applications, e.g. mysqldump.

-d, -description    Session name
-h, -host           Host name
-u, -user           User name
-p, -password       Password
-P, -port           Port (defaults to 3306 if not given)
-S, -socket         Socket name (for connecting via named pipe)
--psettings         Custom filename for portable settings. Ignored if file does not exist.
                    Default filename is "portable_settings.txt"

Examples:

* Start over using stored settings from session "xyz":
    c:\path\to\heidisql.exe -d=xyz
    c:\path\to\heidisql.exe -description=xyz

* Connect with different username or port:
    c:\path\to\heidisql.exe -d=xyz -u=OtherUser
    c:\path\to\heidisql.exe -d=xyz -P=3307

* Connect to a non-stored session:
    c:\path\to\heidisql.exe -h=localhost -u=root -p=Mypass -P=3307

* Open multiple .sql files in query tabs:
    c:\path\to\heidisql.exe fileA.sql path\to\fileB.sql fileC.sql ...

* Use custom portable settings file:
    c:\path\to\heidisql.exe --psettings=c:\temp\p.txt


*** HeidiSQL portable:

If HeidiSQL finds a "portable_settings.txt" (or the custom filename as noted above)
it starts in portable mode. Which means basically that all settings are restored
from that file and when exiting stored again into that file.

When you download the portable zip, that "portable_settings.txt" needs to be manually
copied from your old directory, overwriting the empty default file.


*** License:

HeidiSQL is OpenSource and released under GPL (GNU GENERAL PUBLIC LICENSE).

Probably HeidiSQL saved you a lot of time and you like it. In this case you may
make a donation here: http://www.heidisql.com/donate.php


*** Credits:

Author: Ansgar Becker

Former development contributors: David Dindorp, Francisco Ernesto Teixeira

Sven Lorenz (graphics), Mike Lischke + Timo Tegtmeier (VirtualTreeView, GLPL),
Mathias Rauen (madExcept), Maël Hörz (SynEdit, MPL), Serhiy Perevoznyk (JumpList),
Jordan Russell (Inno Setup), Iztok Kacin (Cromis.DirectoryWatch, BSD License),
Mark James (Silk icons, Creative Commons 2.5)

Thanks to Transifex.com for a free translation account, and the following translators:
David Rodrigues (initiator and Portuguese), Richard van Laak (Dutch),
Michele Locati (Italian), viskubov (Russian), Kirill (Russian), Francisco Alvarado
(Spanish), TiGeR (Chinese), Matej Szendi (Czech), jeff.tu (Chinese/Taiwan), Rastislav
Janosik (Slovak), renaud (French), Hichem BOUKSANI (French), s.d.w. (Korean), Alican
Çubukçuoğlu (Turkish), MichalDCK (Polish), Florin Chis (Romanian), udariza
(Indonesian), Januar (Indonesian), Seiji Ogawa (Japanese), Микола Ковбаса
(Ukrainian), mivzakim (Hebrew), Martin Bedrač (Slovenian), sith (Hungarian),
Xtreme Power (Norwegian), Sasa Kostic (Serbian), Morne Gouws (Afrikaans)

1000 thanks to MySQL and MariaDB for great database software

