unit reformatter;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, IdHTTP, IdSSLOpenSSL,
  apphelpers, extra_controls;

type
  TfrmReformatter = class(TExtForm)
    grpReformatter: TRadioGroup;
    btnCancel: TButton;
    btnOk: TButton;
    procedure btnOkClick(Sender: TObject);
  private
    { Private declarations }
    FInputCode, FOutputCode: String;
  public
    { Public declarations }
    property InputCode: String read FInputCode write FInputCode;
    property OutputCode: String read FOutputCode;
  end;

var
  frmReformatter: TfrmReformatter;

implementation

{$R *.dfm}


procedure TfrmReformatter.btnOkClick(Sender: TObject);
var
  HttpReq: TIdHTTP;
  SSLio: TIdSSLIOHandlerSocketOpenSSL;
  Parameters: TStringList;
begin
  Screen.Cursor := crHourGlass;
  try
    case grpReformatter.ItemIndex of
      0: begin
        // Internal
        FOutputCode := apphelpers.ReformatSQL(FInputCode);
      end;
      1: begin
        // Online
        HttpReq := TIdHTTP.Create;
        SSLio := TIdSSLIOHandlerSocketOpenSSL.Create;
        HttpReq.IOHandler := SSLio;
        SSLio.SSLOptions.SSLVersions := [sslvTLSv1_1, sslvTLSv1_2];
        //HttpReq.Request.ContentType := 'application/json';
        HttpReq.Request.CharSet := 'utf-8';
        HttpReq.Request.UserAgent := apphelpers.UserAgent(Self);
        Parameters := TStringList.Create;
        if AppSettings.ReadBool(asTabsToSpaces) then
          Parameters.AddPair('indent', StringOfChar(' ', AppSettings.ReadInt(asTabWidth)))
        else
          Parameters.AddPair('indent', #9);
        Parameters.AddPair('input', FInputCode);
        FOutputCode := HttpReq.Post(APPDOMAIN + 'sql-formatter.php', Parameters);
        if FOutputCode.IsEmpty then
          raise Exception.Create('Empty result from online reformatter');
        HttpReq.Free;
      end;
    end;
  except
    on E:Exception do begin
      ErrorDialog(E.Message);
      ModalResult := mrNone;
    end;

  end;
  Screen.Cursor := crDefault;
end;

end.
