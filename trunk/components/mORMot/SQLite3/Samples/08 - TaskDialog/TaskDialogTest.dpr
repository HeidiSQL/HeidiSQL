program TaskDialogTest;

uses
  SysUtils,
  SynTaskDialog,
  SQLite3Commons,
  SQLite3UILogin,
  Forms;

{$R *.res}

{$R Vista.res} // to enable XP/Vista/Seven theming

procedure Test;
var aUserName, aPassWord: string;
    Task: TTaskDialog;
    res: boolean;
    Enum: TTaskDialogIcon;
begin
  ShowMessage('This is just a message');
  Task.Inst := 'Do you want to see the new dialog?';
  Task.Content := 'This is the content';
  Task.Buttons := 'Exit application without saving\nThis is not a good idea'#10+
    'Exit application with saving';
  Task.Footer := 'Made with Synopse SynTaskDialog unit';
  case Task.Execute([],100,[tdfUseCommandLinks],tiQuestion,tfiInformation) of
    101: ShowMessage('Saving settings','Blabla',false);
    100: ShowMessage('You should better save your data. You should better save your data.'+
      'You should better save your data.','Please RTFM instructions',true);
    else assert(false);
  end;
  Task.Footer := '';
  aUserName := 'user';
  res := InputQuery('Prompt','Edit the user name',aUserName);
  ShowMessage(aUserName,'You entered:',not res);
  Enum := tiQuestion;
  if InputSelectEnum('Enumeration selection','Please pick up one',
     TypeInfo(TTaskDialogIcon),Enum) then
    ShowMessage(GetEnumCaption(TypeInfo(TTaskDialogIcon),Enum),'You selected:') else
    ShowMessage('You pressed Cancel',true);
  ShowMessage('You selected Item #'+IntToStr(
    Choose('','Select one item,First,Second,Last one')+1));
  Task.Inst := 'Save file to disk ?';
  Task.Content := 'If you do not save changes, these will be lost';
  Task.Buttons := 'Save'#10'Don''t Save';
  Task.Execute([],100,[],tiQuestion);
  Task.Inst := 'Saving application settings';
  Task.Content := 'This is the content';
  Task.Buttons := '';
  Task.Radios := 'Store settings in registry'#10'Store settings in XML file';
  Task.Verify := 'Do no ask for this setting next time';
  Task.VerifyChecked := true;
  Task.Footer := 'XML file is perhaps a better choice';
  Task.Execute([],0,[],tiBlank,tfiInformation,200);
  ShowMessage(IntToStr(Task.RadioRes));
  if Task.VerifyChecked then
    ShowMessage(Task.Verify);
  ShowMessage(Format('User=%s Password=%s',[aUserName,aPassword]),
    not TLoginForm.Login('Title','Please login',aUserName,aPassWord,true,''));
  ShowMessage(Format('User=%s Password=%s',[aUserName,aPassword]),
    not TLoginForm.Login('Title','Please login again',aUserName,aPassWord,true,''));
end;

begin
  Application.Initialize;
  Application.Run;
  Test;
  if @TaskDialogIndirect<>nil then begin
    ShowMessage('Now displaying the dialogs using Delphi emulation');
    @TaskDialogIndirect := nil;
    Test;
  end;
end.
