unit RMC.Store.Connection;

interface

type
{$SCOPEDENUMS ON}
  TConnectionStatus = (Disconnected, Connecting, Connected, ConnectionError, Standby);
  TLoginStatus = (LoggedOut, LoggingIn, LoggedIn, LoginError, LoginLockedLogin, LoginRequire2FA, Standby);
  TLoginType = (None, Credentials, Token);
{$SCOPEDENUMS OFF}

  IStoreAgentConnection = interface
    ['{11111111-2222-3333-4444-555555555555}']
    function CanCallMethods: Boolean;
    function GetConnectionErrorMsg: string;
    function GetConnectionStatus: TConnectionStatus;
    function GetLoginErrorMsg: string;
    function GetLoginStatus: TLoginStatus;
    function GetLoginType: TLoginType;
    function GetLockedLoginSeconds: Integer;
  end;

function GetStoreAgentConnection: IStoreAgentConnection;

implementation

uses
  RMC.Store.Events,
  DDP.Actions.Consts,
  Flux.Actions,
  Flux.Store.Base,
  Grijjy.Data.Bson,
  Grijjy.System.Messaging,
  System.SyncObjs;

type
  TStoreAgentConnection = class(TStoreBase, IStoreAgentConnection)
  private
    FConnectionErrorMsg: string;
    FConnectionStatus: TConnectionStatus;
    FLoginErrorMsg: string;
    FLoginStatus: TLoginStatus;
    FLoginType: TLoginType;
    FLockedLoginSeconds: Integer;
  protected
    procedure EmitStoreChange; override;
  protected
    { IStoreAgentConnection implementation }
    function CanCallMethods: Boolean;
    function GetConnectionErrorMsg: string;
    function GetConnectionStatus: TConnectionStatus;
    function GetLoginErrorMsg: string;
    function GetLoginStatus: TLoginStatus;
    function GetLoginType: TLoginType;
    function GetLockedLoginSeconds: Integer;
  public
    procedure OnAction(const ASender: TObject; const AAction: TgrMessage); override;
  end;

var
  _Lock: TCriticalSection;
  _StoreAgentConnection: IStoreAgentConnection;

{ TStoreAgentConnection }

procedure TStoreAgentConnection.EmitStoreChange;
begin
  FDispatcher.DoDispatch(TStoreRMCConnectionChangedMessage.Create);
end;

function TStoreAgentConnection.CanCallMethods: Boolean;
begin
  // Websocket connected and logged in
  Result := (FConnectionStatus = TConnectionStatus.Connected) and (FLoginStatus = TLoginStatus.LoggedIn);

  // REST API when connection in standby
  if not Result then
    Result := (FConnectionStatus = TConnectionStatus.Standby) or (FLoginStatus = TLoginStatus.Standby);
end;

function TStoreAgentConnection.GetConnectionErrorMsg: string;
begin
  Result := FConnectionErrorMsg;
end;

function TStoreAgentConnection.GetConnectionStatus: TConnectionStatus;
begin
  Result := FConnectionStatus;
end;

function TStoreAgentConnection.GetLoginErrorMsg: string;
begin
  Result := FLoginErrorMsg;
end;

function TStoreAgentConnection.GetLoginStatus: TLoginStatus;
begin
  Result := FLoginStatus;
end;

function TStoreAgentConnection.GetLoginType: TLoginType;
begin
  Result := FLoginType;
end;

function TStoreAgentConnection.GetLockedLoginSeconds: Integer;
begin
  Result := FLockedLoginSeconds;
  FLockedLoginSeconds := 0; // Reset after read
end;

procedure TStoreAgentConnection.OnAction(const ASender: TObject; const AAction: TgrMessage);
var
  LAction: TFluxAction absolute AAction;
  LActionType: string;
begin
  Assert(AAction is TFluxAction);

  LActionType := LAction.&Type;
  if LActionType = ACTION_DDP_USING_CREDENTIALS then
  begin
    FLoginType := TLoginType.Credentials;
  end
  else if LActionType = ACTION_DDP_USING_TOKEN then
  begin
    FLoginType := TLoginType.Token;
  end
  else if LActionType = ACTION_DDP_CONNECTING then
  begin
    // Do not switch to Connecting status when in standby
    if FConnectionStatus <> TConnectionStatus.Standby then
      FConnectionStatus := TConnectionStatus.Connecting;
  end
  else if LActionType = ACTION_DDP_CONNECTED then
  begin
    FConnectionStatus := TConnectionStatus.Connected;
  end
  else if LActionType = ACTION_DDP_DISCONNECTED then
  begin
    // prevent status change from error to disconnect
    if FConnectionStatus = TConnectionStatus.ConnectionError then
      Exit;
    FConnectionStatus := TConnectionStatus.Disconnected;
  end
  else if LActionType = ACTION_DDP_STANDBY then
  begin
    FConnectionStatus := TConnectionStatus.Standby;
    FLoginStatus := TLoginStatus.Standby;
  end
  else if LActionType = ACTION_DDP_CONNECTION_ERROR then
  begin
    if (not LAction.Data.IsNil) and LAction.Data['msg'].IsString then
    begin
      FConnectionErrorMsg := LAction.Data['msg'].AsString;
    end;
    FConnectionStatus := TConnectionStatus.ConnectionError;
  end
  else if LActionType = ACTION_DDP_CONNECTION_EXCEPTION then
  begin
    if (not LAction.Data.IsNil) and LAction.Data['msg'].IsString then
    begin
      FConnectionErrorMsg := LAction.Data['msg'].AsString;
    end;
    FConnectionStatus := TConnectionStatus.ConnectionError;
  end
  else if LActionType = ACTION_DDP_LOGGING_IN then
  begin
    // Do not switch to LoggingIn status when in standby
    if FLoginStatus <> TLoginStatus.Standby then
      FLoginStatus := TLoginStatus.LoggingIn;
  end
  else if LActionType = ACTION_DDP_LOGGED_IN then
  begin
    FLoginStatus := TLoginStatus.LoggedIn;
  end
  else if LActionType = ACTION_DDP_LOGGED_OUT then
  begin
    FLoginStatus := TLoginStatus.LoggedOut;
    FLoginType := TLoginType.None;
  end
  else if LActionType = ACTION_DDP_LOGIN_ERROR then
  begin
    FLoginStatus := TLoginStatus.LoginError;
  end
  else if LActionType = ACTION_DDP_LOGIN_LOCKED_LOGIN then
  begin
    FLoginStatus := TLoginStatus.LoginLockedLogin;
    if (not LAction.Data.IsNil) and LAction.Data['seconds'].IsInt32 then
      FLockedLoginSeconds := LAction.Data['seconds'].AsInteger;
  end
  else if LActionType = ACTION_DDP_LOGIN_2FA_REQUIRED then
  begin
    FLoginStatus := TLoginStatus.LoginRequire2FA;
  end
  else
  begin
    Exit;
  end;
  EmitStoreChange;
end;

// Factory function
function GetStoreAgentConnection: IStoreAgentConnection;
begin
  if not Assigned(_StoreAgentConnection) then
  begin
    _Lock.Enter;
    try
      if not Assigned(_StoreAgentConnection) then
      begin
        _StoreAgentConnection := TStoreAgentConnection.Create;
      end;
    finally
      _Lock.Leave;
    end;
  end;
  Result := _StoreAgentConnection;
end;

initialization
  _Lock := TCriticalSection.Create;

finalization
  _Lock.Free;

end.
