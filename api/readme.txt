====================================================================

mysql.pas Readme File, Version 1.0

Copyright (c) 1999-2002 Matthias Fichtner. All rights reserved.

====================================================================

mysql.pas is a literal translation of relevant parts of MySQL AB's
C header files, mysql.h, mysql_com.h, and mysql_version.h. It serves
as a Pascal interface unit for libmySQL.dll, the client library that
ships with the MySQL database server.

Using mysql.pas, applications developed in Borland Delphi (version
4 and above) can connect to MySQL servers and utilize the full range
of functions defined in the server's C API (Application Programming
Interface).

Like other application programming interfaces (e.g. windows.pas, the
"Windows API", or shellapi.pas, the "Shell API"), mysql.pas does not
contain any visual components; nothing you can place on a project's
form and configure using Object Inspector. It is not installed in
the Delphi IDE.

To use the MySQL API, copy mysql.pas and libmySQL.dll into a Delphi
project's directory and add "MySQL" to its USES clause. Then write
source code that calls the appropriate API functions to connect to a
MySQL server, create databases and tables, insert and modify data,
submit queries, retrieve results, etc.

As mysql.pas is completely compatible with the original C API, there
is no need for separate documentation. For a detailed discussion of
the data types and functions defined in the API, simply refer to the
official documentation:

http://mysql.com/documentation/mysql/bychapter/manual_Clients.html#C

The only aspect not covered in the official documentation is the
dynamic loading of MySQL's client library, libmySQL.dll. Any Delphi
application that uses mysql.pas must load this DLL before it can
call any of the API's functions. The unit's default behaviour is to
try loading the DLL automatically. If it succeeds, the byte value
LIBMYSQL_READY is stored in a variable called libmysql_status. Make
sure you check that variable before using the MySQL API.

If loading libmySQL.dll fails, libmysql_status contains one of two
possible values: LIBMYSQL_MISSING indicates that no suitable library
could be located; LIBMYSQL_INCOMPATIBLE means that the application
tried loading a version of the DLL that is not compatible with the
version of mysql.pas being used. It is then up to the application to
either terminate or find a compatible version of the DLL. In order
to load it, call libmysql_load(), supplying a null-terminated string
containing the library's full path as the function's only parameter.
The function's return value is identical to libmysql_status -- make
sure you check it before using any of the API's functions.

If you do not want mysql.pas to automatically try loading the DLL,
set a compiler conditional called DONT_LOAD_DLL. To do that, select
Project/Options from Delphi's main menu, switch to the Directories/
Conditionals tab, add the symbol to the "Conditional defines" field,
and rebuild your project. It is then up to the application to call
libmysql_load(), supplying either nil or a null-terminated string
containing the library's full path as the function's only parameter.
Again: Don't try using the MySQL API unless the function's return
value is LIBMYSQL_READY.

====================================================================

Latest releases of mysql.pas are made available through the
distribution site at: http://www.fichtner.net/delphi/mysql/

Please send questions, bug reports, and suggestions regarding
mysql.pas to Matthias Fichtner <mfichtner@fichtner-meyer.com>

See license.txt for licensing information and disclaimer.

====================================================================
