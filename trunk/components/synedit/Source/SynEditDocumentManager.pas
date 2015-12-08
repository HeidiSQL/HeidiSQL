unit SynEditDocumentManager;

interface

uses
  classes,
  messages,
  ExtCtrls,
  SynEditTypes,
  SynEdit,
  SynMemo,
  SynEditTextBuffer,
  SynEditHighlighter;

type
  ISynDocument = interface
  ['{DC80C7CF-FC56-4FDE-9E3E-6A1C53D6EFCD}']
    procedure SetCaretXY(const value : TBufferCoord);
    function GetCaretXY : TBufferCoord;
    procedure SetLines(const value : TStrings);
    function GetLines : TStrings;
    function GetUndoList : TSynEditUndoList;
    function GetRedoList : TSynEditUndoList;
    function GetTopLine : integer;
    procedure SetTopLine(const value : integer);
    procedure SetModified(const value : boolean);
    function GetModified : boolean;
    function GetName : string;
    function GetHighLighter : TSynCustomHighlighter;
    procedure SetHighlighter(const value : TSynCustomHighlighter);
    function GetDataIntf : IInterface;
    procedure SetDataIntf(const value : IInterface);
    function GetMarks   : TSynEditMarkList; 

    property CaretXY      : TBufferCoord read GetCaretXY write SetCaretXY;
    property Lines        : TStrings read GetLines write SetLines;
    property UndoList     : TSynEditUndoList read GetUndoList;
    property RedoList     : TSynEditUndoList read GetRedoList;
    property TopLine      : integer read GetTopLine write SetTopLine;
    property Modified     : boolean read GetModified write SetModified;
    property Name         : string read GetName;
    property Highlighter  : TSynCustomHighlighter read GetHighlighter write SetHighLighter;
    property DataIntf     : IInterface read GetDataIntf write SetDataIntf;
    property Marks        : TSynEditMarkList read GetMarks;
    //Line info allows us to store stuff like gutter icons, breakpoints etc.
  end;

  TSynEditDocumentManager = class(TCOmponent)
  private
    FDocuments : IInterfaceList;
    FCurrentDocumentIndex : integer;
    FMemo : TSynMemo;
    FMemoWndProc : TWndMethod;
    FUpdateTimer : TTimer;
    function GetCount: integer;
    function GetCurrentDocument: ISynDocument;
  protected
    procedure MemoWndProc(var Msg: TMessage);
    procedure SetMemo(const Value: TSynMemo);
    function GetDocument(index: integer): ISynDocument;
    function GetDocumentByName(index: string): ISynDocument;
    procedure SetCurrentDocumentIndex(const Value: integer);
    procedure UpdateTimerEvent(Sender : TObject);

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner : TComponent);override;
    destructor Destroy;override;
    procedure UpdateCurrentDocument; //saves editor to document
    procedure ApplyCurrentDocument; //applies document to editor
    function AddDocument(const AName : string; const ALines : TStrings; const AHighlighter : TSynCustomHighlighter) : ISynDocument;
    procedure RemoveDocument(const index : integer);overload;
    procedure RemoveDocument(const AName : string);overload;
    procedure RemoveDocument(const ADocument : ISynDocument);overload;
    procedure RemoveAll;
    property Documents[index : integer] : ISynDocument read GetDocument;
    property CurrentDocument : ISynDocument read GetCurrentDocument;
    property DocumentsByName[index : string] : ISynDocument read GetDocumentByName;
    property CurrentDocumentIndex : integer read FCurrentDocumentIndex write SetCurrentDocumentIndex;
    property Count : integer read GetCount;
  published
    property Memo : TSynMemo read FMemo write SetMemo;
  end;

implementation

uses
  windows,SysUtils;
{ TSynEditDocumentManager }

type
  TSynDocument = class(TInterfacedObject,ISynDocument)
  private
    FName         : string;
    FLines        : TStringList;
    FCaretXY      : TBufferCoord;
    FModified     : Boolean;
    FRedoList     : TSynEditUndoList;
    FUndoList     : TSynEditUndoList;
    FTopLine      : Integer;
    FHighLighter  : TSynCustomHighlighter;
    FDataIntf     : IInterface;
    FMarks        : TSynEditMarkList;
  protected
    function GetCaretXY : TBufferCoord;
    function GetLines : TStrings;
    function GetModified  : Boolean;
    function GetName  : String;
    function GetRedoList  : TSynEditUndoList;
    function GetTopLine : Integer;
    function GetUndoList  : TSynEditUndoList;
    procedure SetCaretXY(const value: TBufferCoord);
    procedure SetLines(const value: TStrings);
    procedure SetModified(const value: Boolean);
    procedure SetTopLine(const value: Integer);
    function GetHighLighter : TSynCustomHighlighter;
    procedure SetHighlighter(const value : TSynCustomHighlighter);
    function GetDataIntf : IInterface;
    procedure SetDataIntf(const value : IInterface);
    function GetMarks   : TSynEditMarkList;
  public
    constructor Create(const AName : string; ALines : TStrings);
    destructor Destroy;override;
  end;


function TSynEditDocumentManager.AddDocument(const AName: string; const ALines: TStrings; const AHighlighter : TSynCustomHighlighter): ISynDocument;
begin
  result := GetDocumentByName(AName);
  if result <> nil then
  begin
    result.Lines.Assign(ALines);
    result.Highlighter := AHighlighter;
  end
  else
  begin
    result := TSynDocument.Create(AName,ALines);
    result.Highlighter := AHighlighter;
    FDocuments.Add(Result);
{    if CurrentDocumentIndex = -1 then
      CurrentDocumentIndex := 0;}
  end;

end;

constructor TSynEditDocumentManager.Create(AOwner: TComponent);
begin
  inherited;
  FDocuments            := TInterfaceList.Create;
  FCurrentDocumentIndex := -1;
  FUpdateTimer          := TTimer.Create(Self);
  FUpdateTimer.enabled  := False;
  FUpdateTimer.Interval := 200;
  FUpdateTimer.OnTimer  := UpdateTimerEvent;
end;

function TSynEditDocumentManager.GetDocument(index: integer): ISynDocument;
begin
  if (index >= 0) and (index < FDocuments.Count) then
    result := FDocuments.Items[index] as ISynDocument
  else
    result := nil;
end;

function TSynEditDocumentManager.GetDocumentByName(index: string): ISynDocument;
var
  i : integer;
begin
  result := nil;
  for i := 0 to FDocuments.Count -1 do
  begin
    result := GetDocument(i);
    if CompareText(result.Name,index) = 0 then
      break
    else
      result := nil;
  end;
end;

procedure TSynEditDocumentManager.RemoveDocument(const index: integer);
begin
  FDocuments.Delete(index);
  if FDocuments.Count = 0 then
    FCurrentDocumentIndex := -1;
end;

procedure TSynEditDocumentManager.RemoveDocument(const AName: string);
var
  doc : ISynDocument;
begin
  doc := GetDocumentByName(AName);
  if doc <> nil then
    FDocuments.Remove(doc);
  if FDocuments.Count = 0 then
    FCurrentDocumentIndex := -1;

end;

procedure TSynEditDocumentManager.MemoWndProc(var Msg: TMessage);
begin
  if (Msg.Msg = WM_CHAR) then
  begin
    FUpdateTimer.Enabled := False;
    FUpdateTimer.Enabled := True;
  end;
  if Assigned(FMemoWndProc) then
    FMemoWndProc(Msg);
end;

procedure TSynEditDocumentManager.RemoveDocument(const ADocument: ISynDocument);
begin
  FDocuments.Remove(ADocument);
  if FDocuments.Count = 0 then
    FCurrentDocumentIndex := -1;
end;

procedure TSynEditDocumentManager.SetCurrentDocumentIndex(const Value: integer);
begin
  if FCurrentDocumentIndex <> Value then
  begin
    UpdateCurrentDocument;
    if (Value >= 0) and (Value < FDocuments.Count) then
    begin
      FCurrentDocumentIndex := Value;
      ApplyCurrentDocument;
    end;
  end;
end;

procedure TSynEditDocumentManager.SetMemo(const Value: TSynMemo);
begin
  if FMemo <> Value then
  begin
    if FMemo <> nil then
    begin
      FMemo.RemoveFreeNotification(Self);
      if not (csDesigning in ComponentState) then
      begin
        if Assigned(FMemoWndProc) then
          FMemo.WindowProc := FMemoWndProc;
      end;
    end;
    FMemo := Value;
    if FMemo <> nil then
    begin
      FMemo.FreeNotification(Self);
      if not (csDesigning in ComponentState) then
      begin
        FMemoWndProc := FMemo.WindowProc;
        FMemo.WindowProc := Self.MemoWndProc;
      end;
    end;
  end;
end;

procedure TSynEditDocumentManager.RemoveAll;
begin
  FDocuments.Clear;
  FCurrentDocumentIndex := -1;
end;

function TSynEditDocumentManager.GetCount: integer;
begin
  result := FDocuments.Count;
end;

function TSynEditDocumentManager.GetCurrentDocument: ISynDocument;
begin
  if FCurrentDocumentIndex <> -1 then
    result := GetDocument(FCurrentDocumentIndex)
  else
    result := nil;
end;

function CloneMark(const AOwner : TCustomSynEdit; const source : TSynEditMark) : TSynEditMark;
begin
  result := TSynEditMark.Create(AOwner);
  result.Line := source.Line;
  Result.Char := source.Char;
  result.ImageIndex := source.ImageIndex;
  result.BookmarkNumber := source.BookmarkNumber;
  result.InternalImage := source.InternalImage;
  result.Visible := source.Visible;
end;

procedure TSynEditDocumentManager.ApplyCurrentDocument;
var
  doc : ISynDocument;
  I: Integer;
begin
  if FCurrentDocumentIndex <> -1 then
  begin
    if FMemo <> nil then
    begin
      doc := GetDocument(FCurrentDocumentIndex);
      if doc <> nil then
      begin
        FMemo.Lines.Assign(doc.Lines);
        FMemo.TopLine := doc.TopLine;
        FMemo.CaretXY := doc.CaretXY;
        FMemo.UndoList.Assign(doc.UndoList);
        FMemo.RedoList.Assign(doc.RedoList);
        FMemo.Highlighter := doc.Highlighter;
        //can't do this because it av's now???
//        FMemo.Marks.Assign(doc.Marks);
        FMemo.Marks.Clear;
        for i := 0 to doc.Marks.Count - 1 do
        begin
          FMemo.Marks.Place(CloneMark(FMemo,doc.Marks.Items[i]));
        end;
        FMemo.Modified := doc.Modified;
        FMemo.Refresh;
      end;
    end;
  end;
end;

{ TSynDocument }

constructor TSynDocument.Create(const AName: string; ALines: TStrings);
begin
  inherited Create;
  FLines    := TStringList.Create;
  FRedoList := TSynEditUndoList.Create;
  FUndoList := TSynEditUndoList.Create;
  FName := AName;
  FLines.Assign(ALines);
  FModified := False;
  FTopLine := 0;
  FMarks   := TSynEditMarkList.Create(nil);
end;

destructor TSynDocument.Destroy;
begin
  FLines.Free;
  FRedoList.Free;
  FUndoList.Free;
  FMarks.Free;
  inherited;
end;

function TSynDocument.GetCaretXY: TBufferCoord;
begin
  result := FCaretXY;
end;

function TSynDocument.GetDataIntf: IInterface;
begin
  result := FDataIntf;
end;

function TSynDocument.GetHighLighter: TSynCustomHighlighter;
begin
  result := FHighLighter;
end;


function TSynDocument.GetLines: TStrings;
begin
  result := FLines;
end;

function TSynDocument.GetMarks: TSynEditMarkList;
begin
  result := FMarks;
end;

function TSynDocument.GetModified: Boolean;
begin
  result := FModified;
end;

function TSynDocument.GetName: String;
begin
  result := FName;
end;

function TSynDocument.GetRedoList: TSynEditUndoList;
begin
  result := FRedoList;
end;

function TSynDocument.GetTopLine: Integer;
begin
  result := FTopLine;
end;

function TSynDocument.GetUndoList: TSynEditUndoList;
begin
  result := FUndoList;
end;

procedure TSynDocument.SetCaretXY(const value: TBufferCoord);
begin
  FCaretXY := value;
end;

procedure TSynDocument.SetDataIntf(const value: IInterface);
begin
  FDataIntf := Value;
end;

procedure TSynDocument.SetHighlighter(const value: TSynCustomHighlighter);
begin
  FHighLighter := value;
end;


procedure TSynDocument.SetLines(const value: TStrings);
begin
  FLines.Assign(value);
end;

procedure TSynDocument.SetModified(const value: Boolean);
begin
  FModified := Value;
end;

procedure TSynDocument.SetTopLine(const value: Integer);
begin
  FTopLine := Value;
end;

procedure TSynEditDocumentManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FMemo) and (Operation = opRemove) then
  begin
    SetMemo(nil);
  end;
end;

destructor TSynEditDocumentManager.Destroy;
begin
  FUpdateTimer.Free;
  inherited;
end;

procedure TSynEditDocumentManager.UpdateTimerEvent(Sender: TObject);
var
  doc : ISynDocument;
begin
  FUpdateTimer.Enabled := False;
  if FCurrentDocumentIndex <> -1 then
  begin
    if FMemo <> nil then
    begin
      doc := GetDocument(FCurrentDocumentIndex);
      if doc <> nil then
      begin
        doc.Modified := FMemo.Modified;
        doc.Lines.Assign(FMemo.Lines);
        doc.TopLine := FMemo.TopLine;
        doc.CaretXY := FMemo.CaretXY;
        doc.UndoList.Assign(FMemo.UndoList);
        doc.RedoList.Assign(FMemo.RedoList);
      end;
    end;
  end;
end;


procedure TSynEditDocumentManager.UpdateCurrentDocument;
var
  doc : ISynDocument;
  i: Integer;
begin
  if FCurrentDocumentIndex <> -1 then
  begin
    //save the state of the current document
    if FMemo <> nil then
    begin
      doc := GetDocument(FCurrentDocumentIndex);
      if doc <> nil then
      begin
        doc.Modified := FMemo.Modified;
        doc.Lines.Assign(FMemo.Lines);
        doc.TopLine := FMemo.TopLine;
        doc.CaretXY := FMemo.CaretXY;
        doc.UndoList.Assign(FMemo.UndoList);
        doc.RedoList.Assign(FMemo.RedoList);
        doc.Marks.Clear;
        for i := 0 to FMemo.Marks.Count - 1 do
          doc.Marks.Place(CloneMark(nil,FMemo.Marks.Items[i]));
        FMemo.Highlighter := doc.Highlighter;
      end;
    end;
  end;
end;
end.
