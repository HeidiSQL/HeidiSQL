unit ReleaseForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ShellAPI;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
{$R Vista.res}

procedure TForm1.Button1Click(Sender: TObject);
var SearchVersion, Text, Head: string;

  procedure ProcessFile(const FN: TFileName);
  var SL: TStringList;
      s,tag,title: string;
      i,j: integer;
      first,titledone: boolean;
  begin
    SL := TStringList.Create;
    try
      SL.LoadFromFile(FN);
      first := false;
      titledone := false;
      for i := 0 to SL.Count-1 do begin
        s := trim(SL[i]);
        if s='' then continue;
        if not titledone and (s[1]='/') then begin
          while (s<>'') and (s[1] in [' ','/']) do delete(s,1,1);
          if s='' then continue;
          if s[1]='-' then
            titledone := true else
            title := title+s+' ';
        end;
        if SameText(s,'interface') then
          exit else
        if IdemPChar(pointer(s),'UNIT ') then
          first := true else
        if first and
           (copy(s,length(s)-length(SearchVersion)+1,length(SearchVersion))=SearchVersion) then begin
          s := ExtractFileName(FN);
          tag := copy(s,1,pos('.',s)-1);
          Head := Head+#13#10'<li><b><a href=#'+tag+'>'+s+'</a></b> - '+trim(title)+';</li>';
          Text := Text+#13#10'<a name='+tag+'><h3>Unit '+s+'</h3></a>'#13#10;
          for j := i+1 to SL.Count-1 do begin
            s := trim(SL[j]);
            if s='' then
              break;
            if s[1]='-' then begin
              delete(s,1,1);
              if first then begin
                first := false;
                Text := Text+'<ul>';
              end else
                Text := Text+';</li>';
              s := trim(s);
              s[1] := UpCase(s[1]);
              s := #13#10'<li>'+s;
            end else
              if not first then
                s := ' '+s;
            Text := Text+s;
          end;
          Text := Text+'.</li>'#13#10'</ul>'#13#10;
          exit;
        end;
      end;
    finally
      SL.Free;
    end;
  end;
  procedure Search(const Folder: TFileName);
  var SR: TSearchRec;
  begin
    if FindFirst(Folder+'*.pas',faAnyFile,SR)<>0 then
      exit;
    repeat
      if IdemPChar(pointer(SR.Name),'SYN') or
         IdemPChar(pointer(SR.Name),'SQLITE3') then
        ProcessFile(Folder+SR.Name);
    until FindNext(SR)<>0;
    FindClose(SR);
  end;
begin
  SearchVersion := Edit1.Text;
  Search(ExtractFilePath(paramstr(0))+'..\..\..\'); // D:\Dev\Lib
  Search(ExtractFilePath(paramstr(0))+'..\..\');    // D:\Dev\Lib\SQLite3
  Head[length(Head)-5] := '.';
  Text := '<p>Our Open Source mORMot framework is now available in revision '+
    SearchVersion+'.</p>'#13#10#13#10+
    '<p>The main new features are the following:<ul><li>...</li></ul></p>'#13#10+
    '<p>Go down to the <a href=#Download>download and forum links</a>.'#13#10+
    '<h2>Synopse mORMot '+SearchVersion+' fixes and enhancements</h2>'#13#10+
    '<p>This is a per-unit list of changes for the '+SearchVersion+
    ' release of mORMot:</p>'#13#10'<ul>'+
    Head+'</ul><p>Changes in details:<br />'#13#10+Text+#13#10'<p><br />'+
    '<a name=Download><h2>Synopse mORMot download and forum</h2></a>'+
    'To get it, go to <a href="http://synopse.info/fossil/wiki?name=Downloads">this '+
    'download page</a>, or <a href="http://synopse.info/fossil">use the '+
    'source</a>... Do not forget to get and read the '+
    'full reference documentation available there (mainly the "SAD" - Software Architecture Design - document).</p>'#13#10+
    '<p>Feedback and questions are <a href="http://synopse.info/forum/viewtopic.php?id=449">welcome in our forum</a>, just '+
    'as usual.</p>';
  Memo1.Text := Text;
end;

procedure TForm1.Button2Click(Sender: TObject);
var FN: TFileName;
    version, content: string;
begin
  version := Edit1.Text;
  content := Memo1.Text;
  if content='' then begin
    Button1Click(nil);
    content := Memo1.Text;
  end;
  FN := ChangeFileExt(paramstr(0),'_'+Version+'.htm');
  with TFileStream.Create(FN,fmCreate) do
  try
    content := '<html><head><title>'+Caption+' '+Version+'</title></head>'+
      '<body>'+Content+'</body></html>';
    Write(content[1],length(content));
  finally
    Free;
  end;
  if DebugHook=0 then
    ShellExecute(0,nil,pointer(FN),nil,nil,SW_SHOWNORMAL);
end;

end.
