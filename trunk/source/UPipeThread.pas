unit UPipeThread;

interface

uses
  Windows, Classes;

type
  TPipeThread = class(TThread)
  private
    FReadPipeData: TThreadMethod;
    FProcessHandle: THandle;
    FWillSuspend : Boolean;
  protected
    procedure Execute; override;
  public
    procedure Resume; reintroduce;
    property ProcessHandle : THandle
      read FProcessHandle write FProcessHandle;
    property ReadPipeData: TThreadMethod
      read FReadPipeData write FReadPipeData;
    property WillSuspend : Boolean
      read FWillSuspend write FWillSuspend;
  end;

implementation

{ Wichtig: Methoden und Eigenschaften von Objekten in visuellen Komponenten dürfen
  nur in einer Methode namens Synchronize aufgerufen werden, z.B.

      Synchronize(UpdateCaption);

  und UpdateCaption könnte folgendermaßen aussehen:

    procedure TPipeThread.UpdateCaption;
    begin
      Form1.Caption := 'Aktualisiert in einem Thread';
    end; }

{ TPipeThread }

procedure TPipeThread.Execute;
begin
  FWillSuspend := false;
  while     (WaitForSingleObject(ProcessHandle,1) <> WAIT_OBJECT_0)
        and not Terminated do
  begin
    if Assigned(FReadPipeData) then
      Synchronize(FReadPipeData);
    if FWillSuspend then
      Suspend
    else
      Sleep(1);
  end;
end;

procedure TPipeThread.Resume;
begin
  FWillSuspend := false;
  inherited;
end;

end.
