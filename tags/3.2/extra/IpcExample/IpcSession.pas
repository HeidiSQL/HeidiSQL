unit IpcSession;

interface

uses Classes, Windows, communication;

type
  TIpcSessionState = (issInactive,issActive);

  TIpcSession = class
    private
      FOwner : TComponent;
      FOnOpened : TNotifyEvent;
      FOnClosed : TNotifyEvent;
      FState : TIpcSessionState;
      FHandle : THandle;
      procedure SetConnectedFlag(const Value: Boolean);
      procedure SetName(const Value: String);
      function GetIsActive: Boolean;
    public
      constructor Create(AOwner : TComponent; AHandle : THandle);
      function Open(AConnected : Boolean; AName : String) : Boolean;
      function Close() : Boolean;
      property IsActive : Boolean read GetIsActive;
      property OnOpened : TNotifyEvent read FOnOpened write FOnOpened;
      property OnClosed : TNotifyEvent read FOnClosed write FOnClosed;
      property Name : String write SetName;
      property ConnectedFlag : Boolean write SetConnectedFlag;
  end;

implementation

uses synchronization;

{ TIpcSession }

function TIpcSession.Close: Boolean;
begin
  try
    DeInitializeSync();
    FState := issInactive;
    if Assigned (FOnClosed) then FOnClosed(Self);
  except
  end;
end;

constructor TIpcSession.Create(AOwner: TComponent; AHandle: THandle);
begin
  Inherited Create ();
  FOwner := AOwner;
  FHandle := AHandle;
end;

function TIpcSession.GetIsActive: Boolean;
begin
  Result := (FState=issActive);
end;

function TIpcSession.Open(AConnected : Boolean; AName : String): Boolean;
begin
  if not IsActive then
    begin
      try
        InitializeSync(FHandle);
        FState := issActive;
        ConnectedFlag := AConnected;
        Name := AName;
        if Assigned (FOnOpened) then FOnOpened(Self);
      except
      end;

    end;
end;

procedure TIpcSession.SetConnectedFlag(const Value: Boolean);
begin
  if FState=issActive then
    SetWindowConnected (Value);
end;

procedure TIpcSession.SetName(const Value: String);
begin
  if FState=issActive then
    SetWindowName (Value);
end;

end.
