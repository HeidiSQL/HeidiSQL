unit FormMain_ctCode;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SynCompletionProposal, StdCtrls, SynEdit, ComCtrls;

const
  cCaseSensitive = 1;
  cAnsiStrings   = 2;
  cPrettyText    = 3;
  cInsertList    = 4;
  cMatchedText   = 5;

type
  TForm1 = class(TForm)
    scpDemo: TSynCompletionProposal;
    PageControl1: TPageControl;
    CodeCompletion: TTabSheet;
    TabSheet2: TTabSheet;
    mmoInsert: TMemo;
    mmoItem: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    SynTest: TSynEdit;
    edBiggestWord: TEdit;
    Label3: TLabel;
    cbCase: TCheckBox;
    cbAnsi: TCheckBox;
    cbPrettyText: TCheckBox;
    cbUseInsertList: TCheckBox;
    cbLimitToMatchedText: TCheckBox;
    SynEdit1: TSynEdit;
    edTitle: TEdit;
    Label4: TLabel;
    Button3: TButton;
    Button4: TButton;
    FontDialog1: TFontDialog;
    procedure FormCreate(Sender: TObject);
    procedure CheckBoxClick(Sender: TObject);
    procedure edBiggestWordChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure edTitleChange(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  with mmoInsert.Lines do
  begin
    Clear;
    Add('Create');
    Add('Destroy');
    Add('Add');
    Add('ClearLine');
    Add('Delete');
    Add('First');
    Add('GetMarksForLine');
    Add('Insert');
    Add('Last');
    Add('Place');
    Add('Remove');
    Add('WMCaptureChanged');
    Add('WMCopy');
    Add('WMCut');
    Add('WMDropFiles');
    Add('WMEraseBkgnd');
    Add('WMGetDlgCode');
    Add('WMHScroll');
    Add('WMPaste');
  end;

  with mmoItem.Lines do
  begin
    Clear;              
    Add('constructor \column{}\style{+B}Create\style{-B}(AOwner: TCustomSynEdit)');
    Add('destructor \column{}\style{+B}Destroy\style{-B}');
    Add('function \column{}\style{+B}Add\style{-B}(Item: TSynEditMark): Integer');
    Add('procedure \column{}\style{+B}ClearLine\style{-B}(line: integer)');
    Add('procedure \column{}\style{+B}Delete\style{-B}(Index: Integer)');
    Add('function \column{}\style{+B}First\style{-B}: TSynEditMark');
    Add('procedure \column{}\style{+B}GetMarksForLine\style{-B}(line: integer; var Marks: TSynEditMarks)');
    Add('procedure \column{}\style{+B}Insert\style{-B}(Index: Integer; Item: TSynEditMark)');
    Add('function \column{}\style{+B}Last\style{-B}: TSynEditMark');
    Add('procedure \column{}\style{+B}Place\style{-B}(mark: TSynEditMark)');
    Add('function \column{}\style{+B}Remove\style{-B}(Item: TSynEditMark): Integer');
    Add('procedure \column{}\style{+B}WMCaptureChanged\style{-B}(var Msg: TMessage); message WM_CAPTURECHANGED');
    Add('procedure \column{}\style{+B}WMCopy\style{-B}(var Message: TMessage); message WM_COPY');
    Add('procedure \column{}\style{+B}WMCut\style{-B}(var Message: TMessage); message WM_CUT');
    Add('procedure \column{}\style{+B}WMDropFiles\style{-B}(var Msg: TMessage); message WM_DROPFILES');
    Add('procedure \column{}\style{+B}WMEraseBkgnd\style{-B}(var Msg: TMessage); message WM_ERASEBKGND');
    Add('procedure \column{}\style{+B}WMGetDlgCode\style{-B}(var Msg: TWMGetDlgCode); message WM_GETDLGCODE');
    Add('procedure \column{}\style{+B}WMHScroll\style{-B}(var Msg: TWMScroll); message WM_HSCROLL');
    Add('procedure \column{}\style{+B}WMPaste\style{-B}(var Message: TMessage); message WM_PASTE');
  end;
  scpDemo.InsertList.AddStrings(mmoInsert.Lines);
  scpDemo.ItemList.AddStrings(mmoItem.Lines);
end;

procedure TForm1.CheckBoxClick(Sender: TObject);
begin
  if Sender is TCheckBox then
  begin
    if TCheckBox(Sender).Checked then
    begin
      Case TCheckBox(Sender).Tag of
        cCaseSensitive : scpDemo.Options := scpDemo.Options + [scoCaseSensitive];
        cAnsiStrings   : scpDemo.Options := scpDemo.Options + [scoAnsiStrings];
        cPrettyText    : scpDemo.Options := scpDemo.Options + [scoUsePrettyText];
        cInsertList    : scpDemo.Options := scpDemo.Options + [scoUseInsertList];
        cMatchedText   : scpDemo.Options := scpDemo.Options + [scoLimitToMatchedText];
      end;
    end else begin
      Case TCheckBox(Sender).Tag of
        cCaseSensitive : scpDemo.Options := scpDemo.Options - [scoCaseSensitive];
        cAnsiStrings   : scpDemo.Options := scpDemo.Options - [scoAnsiStrings];
        cPrettyText    : scpDemo.Options := scpDemo.Options - [scoUsePrettyText];
        cInsertList    : scpDemo.Options := scpDemo.Options - [scoUseInsertList];
        cMatchedText   : scpDemo.Options := scpDemo.Options - [scoLimitToMatchedText];
      end;
    end;
  end;
end;

procedure TForm1.edBiggestWordChange(Sender: TObject);
begin
  scpDemo.Columns[0].BiggestWord := edBiggestWord.Text;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  scpDemo.InsertList.Clear;
  scpDemo.InsertList.AddStrings(mmoInsert.Lines);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  scpDemo.ItemList.Clear;
  scpDemo.ItemList.AddStrings(mmoItem.Lines);
  scpDemo.ResetAssignedList;
end;

procedure TForm1.edTitleChange(Sender: TObject);
begin
  scpDemo.Title := edTitle.Text;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  FontDialog1.Font.Assign(scpDemo.Font);
  if FontDialog1.Execute then
    scpDemo.Font.Assign(FontDialog1.Font);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  FontDialog1.Font.Assign(scpDemo.TitleFont);
  if FontDialog1.Execute then
    scpDemo.TitleFont.Assign(FontDialog1.Font);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  SynEdit1.SetFocus;
end;

end.
