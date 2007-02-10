// Common constants
const

  // Carriage return / Line feed
  CRLF = #13#10;

  // Names of the system tables and system databases
  PRIVTABLE_USERS           = 'user';
  PRIVTABLE_DB              = 'db';
  PRIVTABLE_TABLES          = 'tables_priv';
  PRIVTABLE_COLUMNS         = 'columns_priv';
  DBNAME_INFORMATION_SCHEMA = 'information_schema';
  DBNAME_MYSQL              = 'mysql';
  LOCAL_HOST                = '127.0.0.1';
  MYSQL_PORT                = 3306;

  // Related field things
  TBLTYPE_AUTOMATIC: String = '<Automatic>';
  TEMPFIELDNAME             = 'temp_fieldname';

  // General things
  APPNAME          = 'HeidiSQL';
  REGPATH          = 'Software\' + APPNAME;
  STATUS_MSG_READY = 'Ready.';