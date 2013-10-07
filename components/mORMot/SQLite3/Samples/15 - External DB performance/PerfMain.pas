unit PerfMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Contnrs, XPMan, ShellApi,
  SynCommons, SQLite3Commons, SynSQLite3, SQLite3, SQLite3DB,
  SynDB, SynDBSQLite3, SynDBOracle, SynOleDB, SynDBODBC;

type
  TMainForm = class(TForm)
    LogMemo: TMemo;
    OraTNSName: TEdit;
    OraUser: TEdit;
    OraPass: TEdit;
    Label1: TLabel;
    BtnRunTests: TButton;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure BtnRunTestsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    Ini: RawUTF8;
    Stats: TObjectList;
    procedure SaveStats;
  public
    procedure Test(PropsClass: TSQLDBConnectionPropertiesClass;
      const aServerName, aDatabaseName, aUserID, aPassWord, aTrailDesc: RawUTF8;
      Mode: TSQLSynchronousMode=smNormal);
  end;

var
  MainForm: TMainForm;

implementation

uses DateUtils;

{$R *.dfm}

// if defined, will create two "stored false" properties, to test UNIQUE columns
{.$define UNIK}

type
  TStat = class(TPersistent)
  private
    fCreateTable: RawUTF8;
    fNumberOfElements: integer;
    fInsertTime: RawUTF8;
    fEngine: RawUTF8;
    fClientCloseTime: RawUTF8;
    fInsertRate: integer;
    fReadOneByOneTime: RawUTF8;
    fReadOneByOneRate: integer;
    fInsertBatchTransactionRate: integer;
    fInsertTransactionRate: integer;
    fInsertBatchRate: integer;
    fInsertBatchTransactionTime: RawUTF8;
    fInsertTransactionTime: RawUTF8;
    fInsertBatchTime: RawUTF8;
    fReadAllVirtualRate: integer;
    fReadAllDirectRate: integer;
    fReadAllDirectTime: RawUTF8;
    fReadAllVirtualTime: RawUTF8;
    {$ifdef UNIK}
    fReadOneByNameRate: integer;
    fReadOneByNameTime: RawUTF8;
    {$endif}
  published
    property Engine: RawUTF8 read fEngine;
    property CreateTableTime: RawUTF8 read fCreateTable;
    property NumberOfElements: integer read fNumberOfElements;
    property InsertTime: RawUTF8 read fInsertTime;
    property InsertRate: integer read fInsertRate;
    property InsertBatchTime: RawUTF8 read fInsertBatchTime;
    property InsertBatchRate: integer read fInsertBatchRate;
    property InsertTransactionTime: RawUTF8 read fInsertTransactionTime;
    property InsertTransactionRate: integer read fInsertTransactionRate;
    property InsertBatchTransactionTime: RawUTF8 read fInsertBatchTransactionTime;
    property InsertBatchTransactionRate: integer read fInsertBatchTransactionRate;
    property ReadOneByOneTime: RawUTF8 read fReadOneByOneTime;
    property ReadOneByOneRate: integer read fReadOneByOneRate;
    {$ifdef UNIK}
    property ReadOneByNameTime: RawUTF8 read fReadOneByNameTime;
    property ReadOneByNameRate: integer read fReadOneByNameRate;
    {$endif}
    property ReadAllVirtualTime: RawUTF8 read fReadAllVirtualTime;
    property ReadAllVirtualRate: integer read fReadAllVirtualRate;
    property ReadAllDirectTime: RawUTF8 read fReadAllDirectTime;
    property ReadAllDirectRate: integer read fReadAllDirectRate;
    property ClientCloseTime: RawUTF8 read fClientCloseTime;
  end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Ini := StringFromFile(ChangeFileExt(paramstr(0),'.ini'));
  OraTNSName.Text := UTF8ToString(FindIniEntry(Ini,'Oracle','TNSName'));
  OraUser.Text := UTF8ToString(FindIniEntry(Ini,'Oracle','User'));
  OraPass.Text := UTF8ToString(FindIniEntry(Ini,'Oracle','Password'));
  Stats := TObjectList.Create;
end;

procedure TMainForm.BtnRunTestsClick(Sender: TObject);
var T,U,P: RawUTF8;
begin
  //SynDBLog.Family.Level := LOG_VERBOSE;  // for debugging
  T := StringToUTF8(OraTNSName.Text);
  U := StringToUTF8(OraUser.Text);
  P := StringToUTF8(OraPass.Text);
  UpdateIniEntry(Ini,'Oracle','TNSName',T);
  UpdateIniEntry(Ini,'Oracle','User',U);
  UpdateIniEntry(Ini,'Oracle','Password',P);
  FileFromString(Ini,ChangeFileExt(paramstr(0),'.ini'));
  LogMemo.Clear;
  try
    Test(nil,'','','','','SQLite3 (file full)',smFull);
    Test(nil,'','','','','SQLite3 (file off)',smOff);
    Test(nil,':memory:','','','','SQLite3 (mem)');
    Test(nil,'static','','','','TObjectList (static)');
    Test(nil,'SQL','','','','TObjectList (virtual)');
    Test(TSQLDBSQLite3ConnectionProperties,'','','','',' (ext file full)',smFull);
    Test(TSQLDBSQLite3ConnectionProperties,'','','','',' (ext file off)',smOff);
    Test(TSQLDBSQLite3ConnectionProperties,':memory:','','','',' (ext mem)'); 
    if T<>'' then begin
      Test(TSQLDBOracleConnectionProperties,T,'',U,P,'');
      Test(TODBCConnectionProperties,T,'',U,P,' Oracle');
    end;
    Test(TOleDBJetConnectionProperties,'','','','','');
  except
    on E: Exception do
      LogMemo.Lines.Add(E.Message);
  end;
  Label3.Caption := '';
  T := ObjectToJSON(Stats,true);
  FileFromString(T,ChangeFileExt(paramstr(0),'.stats'));
  FileFromString(T,Ansi7ToString(NowToString(false))+'.log');
  SaveStats;
end;

type
  TSQLRecordSample = class(TSQLRecord)
  private
    fFirstName: RawUTF8;
    fLastName: RawUTF8;
    fAmount: currency;
    fBirthDate: TDateTime;
    fLastChange: TModTime;
    fCreatedAt: TCreateTime;
  published
    property FirstName: RawUTF8 index 40 read fFirstName write fFirstName
      {$ifdef UNIK}stored false{$endif};
    property LastName: RawUTF8 index 40 read fLastName write fLastName
      {$ifdef UNIK}stored false{$endif};
    property Amount: currency read fAmount write fAmount;
    property BirthDate: TDateTime read fBirthDate write fBirthDate;
    property LastChange: TModTime read fLastChange;
    property CreatedAt: TCreateTime read fCreatedAt write fCreatedAt;
  end;

procedure TMainForm.Test(PropsClass: TSQLDBConnectionPropertiesClass;
  const aServerName, aDatabaseName, aUserID, aPassWord, aTrailDesc: RawUTF8;
  Mode: TSQLSynchronousMode);
var aUseBatch, aUseTransactions, aUseDirect: boolean;
    Props: TSQLDBConnectionProperties;
    Model: TSQLModel;
    Client: TSQLRestClientDB;
    Value: TSQLRecordSample;
    Stat: TStat;
    Start: TTimeLog;
    Timer: TPrecisionTimer;
    Res: TIntegerDynArray;
    U, Server, MainDBName, Num, Time: RawUTF8;
    Rate, i: integer;
begin
  U := 'Namee ';
  U[4] := #$c3;
  U[5] := #$a9;
  Stat := TStat.Create;
  Stat.fEngine := PropsClass.EngineName;
  if aTrailDesc<>'' then
    Stat.fEngine := Stat.fEngine+aTrailDesc;
  Model := TSQLModel.Create([TSQLRecordSample]);
  Value := TSQLRecordSample.Create;
  Num := '1';
  for aUseTransactions := false to true do
  for aUseBatch := false to true do begin
    // open connection and initialize mORMot Client-Server instance
    Label3.Caption := Format('Running tests phase #%s on %s...',[Num,Stat.fEngine]);
    Application.ProcessMessages;
    if aServerName='' then begin
      Server := Num+'.'+LowerCaseU(Stat.Engine);
      DeleteFile(UTF8ToString(Server));
    end else
      Server := aServerName;
    if PropsClass<>nil then begin
      MainDBName := ':memory:';
      Props := PropsClass.Create(Server,aDatabaseName,aUserID,aPassWord);
    end else begin
      MainDBName := Server;
      Props := nil;
    end;
    try
      if Server='SQL' then
      begin
        MainDBName := ':memory:';
        Model.VirtualTableRegister(TSQLRecordSample,TSQLVirtualTableBinary);
      end else
        VirtualTableExternalRegister(Model,TSQLRecordSample,Props,'SampleRecord');
      Client := TSQLRestClientDB.Create(Model,nil,MainDBName,TSQLRestServerDB,false,'');
      if Server='static' then begin
        DeleteFile('static.data');
        Client.Server.StaticDataCreate(TSQLRecordSample,'static.data',true);
      end;
      Client.Server.DB.Synchronous := Mode;
      if PropsClass=TSQLDBSQLite3ConnectionProperties then
        TSQLDBSQLite3Connection(Props.MainConnection).Synchronous := Mode;
      try
        // huge insertion in virtual table, with 4 kinds of process
        Timer.Start;
        Client.Server.CreateMissingTables;
        Start := Client.ServerTimeStamp;
        if Stat.CreateTableTime='' then
          Stat.fCreateTable := Timer.Stop;
        if (Mode=smFull) and not aUseTransactions then
          Stat.fNumberOfElements := 50 else // SQLite3 is dead slow without transactions
        if (PropsClass=TOleDBJetConnectionProperties) then
          Stat.fNumberOfElements := 500 else // JET is slow, slow, slow
        if (PropsClass=TODBCConnectionProperties) then
          Stat.fNumberOfElements := 1000 else
          Stat.fNumberOfElements := 5000;
        //Stat.fNumberOfElements := 5;
        Timer.Start;
        if aUseTransactions then
          Client.TransactionBegin(TSQLRecordSample);
        if aUseBatch then
          Client.BatchStart(TSQLRecordSample) else
          SetLength(Res,Stat.fNumberOfElements);
        //Value.BirthDate := EncodeDate(1900,1,1);
        Value.BirthDate := 0;
        for i := 1 to Stat.fNumberOfElements do begin
          Value.Amount := i*0.01;
          Value.LastName := Int32ToUtf8(i);
          Value.FirstName := U+Value.LastName;
          if aUseBatch then
            Client.BatchAdd(Value,true) else
            Res[i-1] := Client.Add(Value,true);
          Value.BirthDate := Value.BirthDate+1;
        end;
        if aUseBatch then
          Client.BatchSend(Res);
        if aUseTransactions then
          Client.Commit;
        Time := Timer.Stop;
        Rate := Timer.PerSec(Stat.fNumberOfElements);
        case Num[1] of
        '1': begin
          Stat.fInsertTime := Time;
          Stat.fInsertRate := Rate;
        end;
        '2': begin
          Stat.fInsertBatchTime := Time;
          Stat.fInsertBatchRate := Rate;
        end;
        '3': begin
          Stat.fInsertTransactionTime := Time;
          Stat.fInsertTransactionRate := Rate;
        end;
        '4': begin
          Stat.fInsertBatchTransactionTime := Time;
          Stat.fInsertBatchTransactionRate := Rate;
          Label3.Caption := Format('Running reading tests on %s...',[Stat.fEngine]);
          Application.ProcessMessages;
          // one by one retrieve values from server
          Timer.Start;
          for i := 0 to Stat.fNumberOfElements-1 do begin
            Client.Retrieve(Res[i],Value);
            assert((Value.fID=Res[i])and(PInt64(@Value.Amount)^=(i+1)*100)and(Value.LastChange>=Start));
          end;
          Stat.fReadOneByOneTime := Timer.Stop;
          Stat.fReadOneByOneRate := Timer.PerSec(Stat.fNumberOfElements);
          {$ifdef UNIK}
          // one by one retrieve values using Name property
          Timer.Start;
          for i := 0 to Stat.fNumberOfElements-1 do begin
            Client.Retrieve('LastName=?',[],[Int32ToUTF8(Res[i])],Value);
            assert((Value.fID=Res[i])and(PInt64(@Value.Amount)^=(i+1)*100)and(Value.LastChange>=Start));
          end;
          Stat.fReadOneByNameTime := Timer.Stop;
          Stat.fReadOneByNameRate := Timer.PerSec(Stat.fNumberOfElements);
          {$endif}
          // retrieve all rows with or without the virtual module
          for aUseDirect := false to true do begin
            with Client.Server do begin
              Cache.Flush; // fair benchmark
              DB.CacheFlush; // fair benchmark (16100 rows/s->456000 with cache!)
              StaticVirtualTableDirect := aUseDirect;
            end;
            Timer.Start;
            Value.ClearProperties;
            if Server='SQL' then
              Value.FillPrepare(Client,'') else
              Value.FillPrepare(Client,'order by RowId');
            i := 0;
            while Value.FillOne do begin
              assert((Value.fID=Res[i])and(PInt64(@Value.Amount)^=(i+1)*100)and(Value.LastChange>=Start));
              inc(i);
            end;
            assert(i=Stat.fNumberOfElements);
            if aUseDirect then begin
              Stat.fReadAllDirectTime := Timer.Stop;
              Stat.fReadAllDirectRate := Timer.PerSec(Stat.fNumberOfElements);
            end else begin
              Stat.fReadAllVirtualTime := Timer.Stop;
              Stat.fReadAllVirtualRate := Timer.PerSec(Stat.fNumberOfElements);
            end;
          end;
{          // backup (for testing purposes)
          if MainDBName<>':memory:' then
            Client.Server.BackupGZ(MainDBName+'.gz'); } 
        end;
        end;
      finally
        Timer.Start;
        try
          if (aServerName<>'') and (aServerName[1]<>':') and // only files remain
             (Server<>'static') and (Server<>'SQL') then
              Client.Server.EngineExecuteAll('drop table '+Value.SQLTableName);
        finally
          Client.Free;
        end;
        Stat.fClientCloseTime := Timer.Stop;
      end;
      inc(Num[1]);
    finally
      Props.Free;
    end;
  end;
  Stats.Add(Stat);
  Model.Free;
  Value.Free;
  LogMemo.Lines.Add(UTF8ToString(ObjectToJSON(Stat,true)));
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Stats.Free;
end;


procedure TMainForm.SaveStats;
type TStatArray = array[0..1000] of TStat;
var Stat: ^TStatArray;
    mode,s,txt: RawUTF8;
    m,nCat: integer;
    max,Cat1,Cat2,Eng1,Eng2: RawUTF8;
    Rows: array[0..4] of RawUTF8;
procedure SetCategories(const Title: RawUTF8; const Cat: array of RawUTF8);
var i: integer;
begin
  mode := UrlEncode(Title);
  s := s+'<h1>'+copy(Title,1,pos(' (',Title)-1)+'</h1>'#13#10;
  max := Int32ToUtf8(m);
  nCat := length(Cat);
  Cat1 := '';
  Cat2 := '';
  Rows[0] := '<td>&nbsp;</td>';
  for i := 0 to high(Cat) do begin
    Rows[i+1] := '<td><b>'+Cat[i]+'</b></td>';
    Cat1 := Cat1+UrlEncode(Cat[i])+'|';
    Cat2 := Cat2+UrlEncode(Cat[high(Cat)-i])+'|';
  end;
  SetLength(Cat1,length(Cat1)-1);
  SetLength(Cat2,length(Cat2)-1);
  Eng1 := '';
  Eng2 := '';
  for i := 0 to Stats.Count-1 do begin
    Rows[0] := Rows[0]+'<td><b>'+
      StringReplace(Stat[i].Engine,' (','<br>(',[])+'</b></td>';
    Eng1 := Eng1+UrlEncode(Stat[i].Engine)+'|';
    Eng2 := Eng2+UrlEncode(Stat[Stats.Count-1-i].Engine)+'|';
  end;
  SetLength(Eng1,length(Eng1)-1);
  SetLength(Eng2,length(Eng2)-1);
end;
procedure Pic1(const Leg: RawUTF8; n: integer);
var i: integer;
begin
  txt := 'http://chart.apis.google.com/chart?chtt='+mode+'&chxl=1:|'+Leg+
    '&chxt=x,y&chbh=a&chs=600x300&cht=bhg&chco='+
    '3D7930,3D8930,309F30,6070F0,5070E0,40C355,65D055,80C1A2,F05050,F0A280'+
    '&chxr=0,0,'+max+'&chds=';
  for i := 1 to n do
    txt := txt+'0,'+max+',';
  txt[length(txt)] := '&';
  txt := txt+'chd=t:';
end;
procedure PicEnd(const Legend: RawUTF8);
begin
  txt[length(txt)] := '&';
  s := s+'<p><img src='+txt+'chdl='+Legend+'></p>'#13#10;
  txt := '';
end;
procedure Table;
var i: integer;
begin
  s := s+'<p><table>';
  for i := 0 to nCat do
    s := s+'<tr align=center>'+Rows[i]+'</tr>'#13#10;
  s := s+'</table></p>';
end;
var i,j: integer;
begin
  Stat := pointer(Stats.List);

  m := 0;
  for i := 0 to Stats.Count-1 do
    with Stat[i] do begin
      if InsertRate>m then m := InsertRate;
      if InsertBatchRate>m then m := InsertBatchRate;
      if InsertTransactionRate>m then m := InsertTransactionRate;
      if InsertBatchTransactionRate>m then m := InsertBatchTransactionRate;
    end;
  SetCategories('Insertion speed (rows/second)',['Direct','Batch','Trans','Batch Trans']);
  Pic1(Cat2,5);
  for i := 0 to Stats.Count-1 do
  with Stat[i] do begin
    txt := FormatUTF8('%%,%,%,%|',
      [txt,InsertRate,InsertBatchRate,InsertTransactionRate,InsertBatchTransactionRate]);
    Rows[1] := FormatUTF8('%<td>%</td>',[Rows[1],InsertRate]);
    Rows[2] := FormatUTF8('%<td>%</td>',[Rows[2],InsertBatchRate]);
    Rows[3] := FormatUTF8('%<td>%</td>',[Rows[3],InsertTransactionRate]);
    Rows[4] := FormatUTF8('%<td>%</td>',[Rows[4],InsertBatchTransactionRate]);
  end;
  Table;
  PicEnd(Eng1);

  Pic1(Eng2,Stats.Count);
  for i := 0 to Stats.Count-1 do
    txt := txt+Int32ToUtf8(Stat[i].InsertRate)+',';
  txt[length(txt)] := '|';
  for i := 0 to Stats.Count-1 do
    txt := txt+Int32ToUtf8(Stat[i].InsertBatchRate)+',';
  txt[length(txt)] := '|';
  for i := 0 to Stats.Count-1 do
    txt := txt+Int32ToUtf8(Stat[i].InsertTransactionRate)+',';
  txt[length(txt)] := '|';
  for i := 0 to Stats.Count-1 do
    txt := txt+Int32ToUtf8(Stat[i].InsertBatchTransactionRate)+',';
  PicEnd(Cat1);

  m := 0;
  for i := 0 to Stats.Count-1 do
    with Stat[i] do begin
      if ReadOneByOneRate>m then m := ReadOneByOneRate;
      {$ifdef UNIK}
      if ReadOneByNameRate>m then m := ReadOneByNameRate;
      {$endif}
      if ReadAllVirtualRate>m then m := ReadAllVirtualRate;
      if ReadAllDirectRate>m then m := ReadAllDirectRate;
    end;
  SetCategories('Read speed (rows/second)',['By one',
    {$ifdef UNIK}'By name',{$endif}'All Virtual','All Direct']);
  Pic1(Cat2,{$ifdef UNIK}4{$else}3{$endif});
  for i := 0 to Stats.Count-1 do
  with Stat[i] do begin
    txt := FormatUTF8({$ifdef UNIK}'%%,%,%,%|'{$else}'%%,%,%|'{$endif},
      [txt,ReadOneByOneRate,{$ifdef UNIK}ReadOneByNameRate,{$endif}
        ReadAllVirtualRate,ReadAllDirectRate]);
    Rows[1] := FormatUTF8('%<td>%</td>',[Rows[1],ReadOneByOneRate]);
    {$ifdef UNIK}
    Rows[2] := FormatUTF8('%<td>%</td>',[Rows[2],ReadOneByNameRate]);
    j := 3;
    {$else}
    j := 2;
    {$endif}
    Rows[j] := FormatUTF8('%<td>%</td>',[Rows[j],ReadAllVirtualRate]);
    Rows[j+1] := FormatUTF8('%<td>%</td>',[Rows[j+1],ReadAllDirectRate]);
  end;
  Table;
  PicEnd(Eng1);

  Pic1(Eng2,Stats.Count);
  for i := 0 to Stats.Count-1 do
    txt := txt+Int32ToUtf8(Stat[i].ReadOneByOneRate)+',';
  txt[length(txt)] := '|';
  {$ifdef UNIK}
  for i := 0 to Stats.Count-1 do
    txt := txt+Int32ToUtf8(Stat[i].ReadOneByNameRate)+',';
  txt[length(txt)] := '|';
  {$endif}
  for i := 0 to Stats.Count-1 do
    txt := txt+Int32ToUtf8(Stat[i].ReadAllVirtualRate)+',';
  txt[length(txt)] := '|';
  for i := 0 to Stats.Count-1 do
    txt := txt+Int32ToUtf8(Stat[i].ReadAllDirectRate)+',';
  PicEnd(Cat1);

  FileFromString('<html><body>'#13#10+s,ChangeFileExt(paramstr(0),'.htm'));
end;

procedure TMainForm.FormShow(Sender: TObject);
var Valid: boolean;
    S: RawUTF8;
begin
  exit;
  S := StringFromFile(ChangeFileExt(paramstr(0),'.stats'));
  JSONToObject(Stats,pointer(S),Valid,TStat);
  if Valid then
    SaveStats;
  Close;
end;

end.
