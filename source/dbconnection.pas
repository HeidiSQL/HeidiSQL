unit dbconnection;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Generics.Defaults;

type
  TDBConnection = class;

  { TDBConnection }

  TDBLogCategory = (lcInfo, lcSQL, lcUserFiredSQL, lcError, lcDebug, lcScript);
  TDBLogItem = class(TObject)
    public
      Category: TDBLogCategory;
      LineText: String;
      Connection: TDBConnection;
  end;
  TDBLogItems = TObjectList<TDBLogItem>;
  TDBLogEvent = procedure(Msg: String; Category: TDBLogCategory=lcInfo; Connection: TDBConnection=nil) of object;
  TDBEvent = procedure(Connection: TDBConnection; Database: String) of object;

  TDBConnection = class(TComponent)
    private
      FActive: Boolean;
    public
  end;
  TDBConnectionList = TObjectList<TDBConnection>;


implementation

end.

