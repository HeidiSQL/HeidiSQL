unit Sequal.Suggest;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, SynEdit, SynMemo, extra_controls, apphelpers,
  IdHTTP, IdSSLOpenSSL, System.JSON, dbconnection;

type
  TSequalSuggestForm = class(TExtForm)
    memoPrompt: TMemo;
    btnGenerateSQL: TButton;
    memoGeneratedSQL: TSynMemo;
    btnExecute: TButton;
    btnClose: TButton;
    Label1: TLabel;
    btnHelp: TButton;
    procedure btnHelpClick(Sender: TObject);
    procedure btnGenerateSQLClick(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FConnection: TDBConnection;
    function EngineType: String;
    function JsonEncode(aValue: String): String;
  public
    { Public declarations }
  end;

var
  SequalSuggestForm: TSequalSuggestForm;

implementation

uses main;

{$R *.dfm}


function TSequalSuggestForm.EngineType: String;
begin
  // Supported engine types, see https://sequal.dev/docs#suggest
  if FConnection.Parameters.IsMariaDB then
    Result := 'mariadb'
  else if FConnection.Parameters.IsAnyPostgreSQL then
    Result := 'postgres'
  else
    Result := 'mysql';
end;

function TSequalSuggestForm.JsonEncode(aValue: String): String;
var
  JsonText: TJSONString;
begin
  JsonText := TJSONString.Create(aValue);
  Result := JsonText.ToJSON;
  JsonText.Free;
end;

procedure TSequalSuggestForm.btnGenerateSQLClick(Sender: TObject);
var
  HttpReq: TIdHTTP;
  SSLio: TIdSSLIOHandlerSocketOpenSSL;
  JsonBodyStr, JsonResponseStr: String;
  JsonBodyStream: TStringStream;
  JsonTmp: TJSONValue;
begin
  // Call suggest API
  HttpReq := TIdHTTP.Create;
  SSLio := TIdSSLIOHandlerSocketOpenSSL.Create;
  HttpReq.IOHandler := SSLio;
  SSLio.SSLOptions.SSLVersions := [sslvTLSv1_1, sslvTLSv1_2];
  HttpReq.Request.ContentType := 'application/json';
  HttpReq.Request.CharSet := 'utf-8';

  JsonBodyStr := '{"prompt": '+JsonEncode(memoPrompt.Text)+', "type":'+JsonEncode(EngineType)+'}';
  //showmessage(jsonbodystr);
  JsonBodyStream := TStringStream.Create(JsonBodyStr, TEncoding.UTF8);

  try
    Screen.Cursor := crHourGlass;
    JsonResponseStr := HttpReq.Post('https://api.sequal.dev:2000/simple-suggest', JsonBodyStream);
    JsonTmp := TJSONObject.ParseJSONValue(JsonResponseStr);
    memoGeneratedSQL.Text := JsonTmp.FindValue('data').Value;
    btnExecute.Enabled := Length(Trim(memoGeneratedSQL.Text)) > 0;
  except
    on E:Exception do begin
      Screen.Cursor := crDefault;
      ErrorDialog(E.ClassName + ': ' + E.Message);
    end;
  end;
  HttpReq.Free;
  JsonBodyStream.Free;
  Screen.Cursor := crDefault;
end;

procedure TSequalSuggestForm.btnHelpClick(Sender: TObject);
begin
  // Help button
  ShellExec('https://sequal.dev/our-plans');
end;

procedure TSequalSuggestForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  // Store GUI setup
  AppSettings.WriteIntDpiAware(asSequalSuggestWindowWidth, Self, Width);
  AppSettings.WriteIntDpiAware(asSequalSuggestWindowHeight, Self, Height);
end;

procedure TSequalSuggestForm.FormCreate(Sender: TObject);
begin
  HasSizeGrip := True;
end;

procedure TSequalSuggestForm.FormShow(Sender: TObject);
begin
  // Restore GUI setup
  Width := AppSettings.ReadIntDpiAware(asSequalSuggestWindowWidth, Self);
  Height := AppSettings.ReadIntDpiAware(asSequalSuggestWindowHeight, Self);
  FConnection := MainForm.ActiveConnection;
  memoPrompt.Text := 'Write me a '+EngineType+' query to select column a, b and c from table foobar';
end;

procedure TSequalSuggestForm.btnExecuteClick(Sender: TObject);
var
  Tab: TQueryTab;
begin
  // Pass to query tab and run (!)
  if MainForm.actNewQueryTab.Execute then begin
    Tab := MainForm.QueryTabs[MainForm.QueryTabs.Count-1];
    Tab.Memo.Text := memoGeneratedSQL.Text;
    Tab.TabSheet.Show;
    MainForm.actExecuteQueryExecute(Sender);
  end;
end;


end.
