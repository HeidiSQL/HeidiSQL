/// SynFile client handling
unit FileClient;

interface

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  SynCommons,
  SynGdiPlus,
  SQLite3Commons,
  SQLite3HttpClient,
  SQLite3i18n,
  SQLite3ToolBar,
  SQLite3Pages,
  FileTables;

type
  /// a HTTP/1.1 client to access SynFile
  TFileClient = class(TSQLite3HttpClient)
  public
    /// initialize the Client for a specified network Server name
    constructor Create(const aServer: AnsiString); reintroduce;
	/// used internaly to retrieve a given action
    function OnSetAction(TableIndex, ToolbarIndex: integer; TestEnabled: boolean;
      var Action): string;
	/// client-side access to the remote RESTful service
    procedure AddAuditTrail(aEvent: TFileEvent; aAssociatedRecord: TSQLRecord);
  end;

  /// class used to create the User interface
  TFileRibbon = class(TSQLRibbon)
  public
    /// overriden method used customize the report content
    procedure CreateReport(aTable: TSQLRecordClass; aID: integer; aReport: TGDIPages;
      AlreadyBegan: boolean=false); override;
  end;


implementation

uses Forms;


{ TFileClient }

procedure TFileClient.AddAuditTrail(aEvent: TFileEvent;
  aAssociatedRecord: TSQLRecord);
begin
  if aAssociatedRecord=nil then
    CallBackGetResult('Event',['event',ord(aEvent)]) else
    with aAssociatedRecord do
      CallBackGetResult('Event',['event',ord(aEvent)],RecordClass,ID);
end;

constructor TFileClient.Create(const aServer: AnsiString);
begin
  inherited Create(aServer,SERVER_HTTP_PORT,CreateFileModel(self));
  ForceBlobTransfert := true;
end;

function TFileClient.OnSetAction(TableIndex, ToolbarIndex: integer;
  TestEnabled: boolean; var Action): string;
var A: TFileActions;
begin
  Result := '';
  if ToolBarIndex<0 then
    ToolbarIndex := FileActionsToolbar_MARKINDEX;
  A := FileTabs[TableIndex].Actions*FileActionsToolBar[ToolBarIndex];
  move(A,Action,sizeof(A));
end;


{ TFileRibbon }

resourcestring
  sCreated = 'Created';
  sModified = 'Modified';
  sKeyWords = 'KeyWords';
  sContent = 'Content';
  sNone = 'None';
  sPageN = 'Page %d / %d';
  sSizeN = 'Size: %s';
  sContentTypeN = 'Content Type: %s';
  sSafeMemoContent = 'This memo is password protected.'#13+
    'Please click on the "Edit" button to show its content.';
  sDataContent = 'Please click on the "Extract" button to get its content.';
  sSignedN = 'Signed,By %s on %s';
  sPictureN = '%s Picture';

procedure TFileRibbon.CreateReport(aTable: TSQLRecordClass; aID: integer; aReport: TGDIPages;
  AlreadyBegan: boolean=false);
var Rec: TSQLFile;
    Pic: TBitmap;
    s: string;
    PC: PWideChar;
    P: TSQLRibbonTab;
begin
  with aReport do begin
    // initialize report
    Clear;
    BeginDoc;
    Font.Size := 10;
    if not aTable.InheritsFrom(TSQLFile) then
      P := nil else
      P := GetActivePage;
    if (P=nil) or (P.CurrentRecord.ID<>aID) or (P.Table<>aTable) then begin
      inherited; // default handler
      exit;
    end;
    Rec := TSQLFile(P.CurrentRecord);
    Caption := U2S(Rec.fName);
    // prepare page footer
    SaveLayout;
    Font.Size := 9;
    AddPagesToFooterAt(sPageN,LeftMargin);
    TextAlign := taRight;
    AddTextToFooterAt('SynFile  http://synopse.info - '+Caption,RightMarginPos);
    RestoreSavedLayout;
    // write global header at the beginning of the report
    DrawTitle(P.Table.CaptionName+' : '+Caption,true);
    NewHalfLine;
    AddColumns([6,40]);
    SetColumnBold(0);
    if Rec.SignatureTime<>0 then begin
      PC := Pointer(StringToSynUnicode(Format(sSignedN,[Rec.SignedBy,Iso2S(Rec.SignatureTime)])));
      DrawTextAcrossColsFromCSV(PC,$C0C0FF);
    end;
    if Rec.fCreated<>0 then
      DrawTextAcrossCols([sCreated,Iso2S(Rec.fCreated)]);
    if Rec.fModified<>0 then
      DrawTextAcrossCols([sModified,Iso2S(Rec.fModified)]);
    if Rec.fKeyWords='' then
      s := sNone else begin
      s := U2S(Rec.fKeyWords);
      ExportPDFKeywords := s;
    end;
    DrawTextAcrossCols([sKeyWords,s]);
    NewLine;
    Pic := LoadFromRawByteString(Rec.fPicture);
    if Pic<>nil then
    try
      DrawBMP(Pic,0,Pic.Width div 3);
    finally
      Pic.Free;
    end;
    // write report content
    DrawTitle(sContent,true);
    SaveLayout;
    Font.Name := 'Courier New';
    if Rec.InheritsFrom(TSQLSafeMemo) then
      DrawText(sSafeMemoContent) else
    if Rec.InheritsFrom(TSQLMemo) then
      DrawTextU(TSQLMemo(Rec).Content) else
    if Rec.InheritsFrom(TSQLData) then
    with TSQLData(Rec) do begin
      DrawTextU(Rec.fName);
      s := PictureName(TSynPicture.IsPicture(TFileName(Rec.fName)));
      if s<>'' then
        s := format(sPictureN,[s]) else
        if not Rec.InheritsFrom(TSQLSafeData) then
          s := U2S(GetMimeContentType(Pointer(Data),Length(Data),TFileName(Rec.fName)));
      if s<>'' then
        DrawTextFmt(sContentTypeN,[s]);
      DrawTextFmt(sSizeN,[U2S(KB(Length(Data)))]);
      NewHalfLine;
      DrawText(sDataContent);
    end;
    RestoreSavedLayout;
    // set custom report parameters
    ExportPDFApplication := 'SynFile  http://synopse.info';
    ExportPDFForceJPEGCompression := 80;
  end;
end;

end.
