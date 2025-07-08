// Aggiungi queste interfacce a RMC.ActionCreator.pas

unit RMC.ActionCreator;

interface

uses
  Grijjy.Data.Bson,
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  RMC.Store.Agent,
  RMC.Connection,
  RMC.Data,
  System.JSON, Factories, AgentServiceI;

type
  // Interfaccia per l'ActionCreator dell'Agent
  IActionCreatorAgent = interface
    ['{33333333-4444-5555-6666-777777777777}']
    // Eventi di connessione
    procedure OnConnected;
    procedure OnDisconnected;
    procedure OnDisconnect(const ACode: Integer);
    procedure OnError(const AError: string);
    procedure OnException(const AClassName, AMessage: string);

    // Eventi per comandi
    procedure OnCommandReceived(const ACommandId, ASessionId, ACommandLine: string;
      const APriority: Integer = 5; const ATimeout: Integer = 0);
    procedure OnCommandExecuting(const ACommandId, ASessionId: string);
    procedure OnCommandCompleted(const ACommandId, ASessionId: string; const AExitCode: Integer);
    procedure OnCommandError(const ACommandId, ASessionId, AError: string);

    // Eventi per sessioni
    procedure OnSessionOpenRequested(const ASessionId, AUserId, AShellType: string;
      const AWorkingDir: string = '');
    procedure OnSessionOpened(const ASessionId: string);
    procedure OnSessionClosed(const ASessionId: string);
    procedure OnSessionError(const ASessionId, AError: string);

    // Eventi di controllo
    procedure OnShutdownRequested;
    procedure OnRestartRequested;
    procedure OnStatusRequested;

    procedure Login(const AUsername, APassword: string); overload;
    procedure Login(const AToken: string); overload;
    procedure Login(const AUsername, APassword, A2FACode: string); overload;
    procedure Logout;
    procedure Standby;
    procedure Resume;

    procedure RegisterAgent(const AAgentInfo: TJSONObject);
    procedure SetServices;
    procedure SetShellRunnerFactory(const ShellRunnerFactory: IShellRunnerFactory);
    procedure SetSessionManager(const SetSessionManager: ISessionManager);
    procedure ping(const ASessionId: string);

    procedure SetAgentConnection(const AgentConnection: IAgentConnection);
    procedure SetAgentData(const AgentData: IAgentData);

    // *** AGGIUNGI QUESTI METODI PER I TEST ***
    function TestInsertSession: string;
    function TestInsertCommand(const ASessionId: string): string;
    function IsConnected: Boolean;
    function GetConnectionStatus: string;
    function GetAgentStore: IStoreAgent;




  end;

// Factory per ottenere l'ActionCreator per un agent specifico
function GetActionCreatorAgentWithId(const AAgentId: string): IActionCreatorAgent;

implementation

uses
  RMC.Actions.Consts,
  Flux.Actions,
  Flux.ActionCreator.Base,
  System.Generics.Collections,

  BCrypt,
  DDP.Actions.Consts,
  DDP.Factories,
  DDP.Interfaces,
  DDP.NetLib.Factory,
  DDP.RequestGenerator,
  DDP.Exception,
  Nanosystems.Logging;

type
  TActionCreatorAgent = class(TActionCreatorBase, IActionCreatorAgent)
  private
    FAgentId: string;
    FLock: TCriticalSection;
    FDDPRequestGenerator: IDDPRequestGenerator;
    FAgentConnection: IAgentConnection;
    FAgentData: IAgentData;
    FAutoReconnect: Boolean;
    FToken: string;
    FStandby: Boolean;
    FShellRunnerFactory: IShellRunnerFactory;
    FSessionManager:ISessionManager;

    FDDPNetLib: IDDPNetLib;     // <- Mantieni riferimento
    FDDPClient: IDDPClient;     // <- Mantieni riferimento
    FDDPLogin: IDDPLogin;
    FAgentStore: IStoreAgent;
    function GetAutoReconnect: Boolean;
    function GetToken: string;
    procedure SetAutoReconnect(const AValue: Boolean);
    procedure SetStandby(const AValue: Boolean);
    procedure SetToken(const AValue: string);
    procedure Build;
    function DDPConnect: Boolean;
    procedure DDPDisconnect;
    function DDPLogin(const AUsername, APassword: string): Boolean; overload;
    function DDPLogin(const AToken: string): Boolean; overload;
    function DDPLogin(const AUsername, APassword, A2FACode: string): Boolean; overload;
    procedure DDPLogout;
    procedure DDPSubscribe;
    procedure StartAutoReconnect;
  protected
    { IActionCreatorAgent implementation }
    procedure OnConnected;
    procedure OnDisconnected;
    procedure OnDisconnect(const ACode: Integer);
    procedure OnError(const AError: string);
    procedure OnException(const AClassName, AMessage: string);

    procedure OnCommandReceived(const ACommandId, ASessionId, ACommandLine: string;
      const APriority: Integer = 5; const ATimeout: Integer = 0);
    procedure OnCommandExecuting(const ACommandId, ASessionId: string);
    procedure OnCommandCompleted(const ACommandId, ASessionId: string; const AExitCode: Integer);
    procedure OnCommandError(const ACommandId, ASessionId, AError: string);

    procedure OnSessionOpenRequested(const ASessionId, AUserId, AShellType: string;
      const AWorkingDir: string = '');
    procedure OnSessionOpened(const ASessionId: string);
    procedure OnSessionClosed(const ASessionId: string);
    procedure OnSessionError(const ASessionId, AError: string);

    procedure OnShutdownRequested;
    procedure OnRestartRequested;
    procedure OnStatusRequested;

    procedure Login(const AUsername, APassword: string); overload;
    procedure Login(const AToken: string); overload;
    procedure Login(const AUsername, APassword, A2FACode: string); overload;
    procedure Logout;
    procedure Standby;
    procedure Resume;
    procedure RegisterAgent(const AAgentInfo: TJSONObject);
    procedure SetServices;
    procedure SetShellRunnerFactory(const ShellRunnerFactory: IShellRunnerFactory);
    procedure SetSessionManager(const SetSessionManager: ISessionManager);
    procedure ping(const ASessionId: string);

    procedure SetAgentConnection(const AgentConnection: IAgentConnection);
    procedure SetAgentData(const AgentData: IAgentData);

    // *** AGGIUNGI QUESTI METODI PER I TEST ***
    function TestInsertSession: string;
    function TestInsertCommand(const ASessionId: string): string;

    function IsConnected: Boolean;
    function GetConnectionStatus: string;
    function GetAgentStore: IStoreAgent;


  public
    constructor Create(const AAgentId: string);
  end;

var
  _ActionCreators: TDictionary<string, IActionCreatorAgent>;

{ TActionCreatorAgent }

constructor TActionCreatorAgent.Create(const AAgentId: string);
begin
  inherited Create;
  FAgentId := AAgentId;
  FLock := TCriticalSection.Create;
  FDDPRequestGenerator := TDDPRequestGenerator.Create;


end;


procedure TActionCreatorAgent.SetAgentConnection(const AgentConnection: IAgentConnection);
begin
  FAgentConnection:=AgentConnection;
end;

procedure TActionCreatorAgent.SetAgentData(const AgentData: IAgentData);
begin
  FAgentData := AgentData;
end;

function TActionCreatorAgent.TestInsertSession: string;
begin
  Result := '';
  try
    if Assigned(FAgentData) then
    begin
      WriteLn('[ActionCreator] Inserting test session...');
      Result := FAgentData.TestInsertSessionForAgent(FAgentId);
      WriteLn('[ActionCreator] Test session inserted: ' + Result);
    end
    else
    begin
      WriteLn('[ActionCreator] Error: FAgentData not assigned');
    end;
  except
    on E: Exception do
      WriteLn('[ActionCreator] Error inserting test session: ' + E.Message);
  end;
end;

function TActionCreatorAgent.TestInsertCommand(const ASessionId: string): string;
begin
  Result := '';
  try
    if Assigned(FAgentData) then
    begin
      WriteLn('[ActionCreator] Inserting test command...');
      Result := FAgentData.TestInsertCommandForAgent(FAgentId, ASessionId,
        'echo "Test from ActionCreator at ' + FormatDateTime('hh:nn:ss', Now) + '"');
      WriteLn('[ActionCreator] Test command inserted: ' + Result);
    end
    else
    begin
      WriteLn('[ActionCreator] Error: FAgentData not assigned');
    end;
  except
    on E: Exception do
      WriteLn('[ActionCreator] Error inserting test command: ' + E.Message);
  end;
end;


function TActionCreatorAgent.IsConnected: Boolean;
begin
  Result := Assigned(FAgentConnection) and Assigned(FAgentData);
end;

function TActionCreatorAgent.GetConnectionStatus: string;
begin
  if Assigned(FAgentConnection) and Assigned(FAgentData) then
    Result := 'Connected and ready'
  else if Assigned(FAgentConnection) then
    Result := 'Connected but data not ready'
  else
    Result := 'Not connected';
end;

function TActionCreatorAgent.GetAutoReconnect: Boolean;
begin
  FLock.Enter;
  try
    Result := FAutoReconnect;
  finally
    FLock.Leave;
  end;
end;

function TActionCreatorAgent.GetToken: string;
begin
  FLock.Enter;
  try
    Result := FToken;
  finally
    FLock.Leave;
  end;
end;

procedure TActionCreatorAgent.SetAutoReconnect(const AValue: Boolean);
begin
  FLock.Enter;
  try
    FAutoReconnect := AValue;
  finally
    FLock.Leave;
  end;
end;

procedure TActionCreatorAgent.SetStandby(const AValue: Boolean);
begin
  FStandby := AValue;
  if AValue then
  begin
    if Assigned(FAgentData) then
    begin
      { Set token before enabling standby status on FAddressBookData }
      FAgentData.SetToken(GetToken);
      FAgentData.SetStandby(AValue);
    end;
  end
  else
  begin
    if Assigned(FAgentData) then
    begin
      { Empty token after disabling standby status on FAddressBookData }
      FAgentData.SetStandby(AValue);
      FAgentData.SetToken('');
    end;
  end;
end;

procedure TActionCreatorAgent.SetToken(const AValue: string);
begin
  FLock.Enter;
  try
    FToken := AValue;
  finally
    FLock.Leave;
  end;
end;

procedure TActionCreatorAgent.Build;
//var
  //LDDPNetLib: IDDPNetLib;
  //LDDPClient: IDDPClient;
  //LDDPLogin: IDDPLogin;
begin
  {FAgentData := nil;
  FAgentConnection := nil;
  FDDPNetLib := TDDPNetLibFactory.CreateNew;
  FDDPClient := GetDDPClient(FDDPNetLib, FDDPRequestGenerator);
  FDDPLogin := GetDDPLogin(FDDPClient);
  FAgentConnection := GetAgentConnection(FDDPClient, FDDPLogin);
  FAgentData := GetAgentData(FDDPClient);
  }
  FAgentStore := GetStoreAgent(FAgentId);
  //FAgentStore := GetStoreAgent(FAgentId);
  // Configura i servizi nello store agent
  SetServices;
end;
// AGGIUNGI metodo per esporre lo Store al main program:
function TActionCreatorAgent.GetAgentStore: IStoreAgent;
begin
  Result := FAgentStore;
end;


function TActionCreatorAgent.DDPConnect: Boolean;
begin
  Result := False;
  FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_CONNECTING));
  try
    FAgentConnection.Connect;
    FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_CONNECTED));
    Result := True;
  except
    on Ex: Exception do
    begin
      FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_CONNECTION_EXCEPTION, TgrBsonDocument.Create('name',
        Ex.ClassName).Add('msg', Ex.Message)));
    end;
  end;
end;

procedure TActionCreatorAgent.DDPDisconnect;
begin
  FAgentConnection.Disconnect;
  FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_DISCONNECTED));
end;

function TActionCreatorAgent.DDPLogin(const AUsername, APassword: string): Boolean;
var
  LToken: string;
begin
  Result := False;
  FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_LOGGING_IN));
  try
    LToken := FAgentConnection.Login(AUsername, APassword);
    SetToken(LToken); // save as field for autoreconnect
    FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_LOGGED_IN, TgrBsonDocument.Create('token', LToken)));
    FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_USING_TOKEN)); // now we have the token
    Result := True;
  except
    on E: ELockedLoginException do
    begin
      FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_LOGIN_LOCKED_LOGIN, TgrBsonDocument.Create('seconds', E.Seconds)));
    end;
    on E: E2FAuthenticationRequired do
    begin
//      FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_LOGIN_ERROR));
      FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_LOGIN_2FA_REQUIRED));
    end;
    on Ex: Exception do
    begin
      FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_LOGIN_ERROR));
    end;
  end;
end;

function TActionCreatorAgent.DDPLogin(const AToken: string): Boolean;
begin
  Result := False;
  FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_LOGGING_IN));
  try
    FAgentConnection.Login(AToken);
    FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_LOGGED_IN));
    Result := True;
  except
    on Ex: Exception do
    begin
      FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_LOGIN_ERROR));
    end;
  end;
end;

function TActionCreatorAgent.DDPLogin(const AUsername, APassword, A2FACode: string): Boolean;
var
  LToken: string;
begin
  Result := False;
  FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_LOGGING_IN));
  try
    LToken := FAgentConnection.Login(AUsername, APassword, A2FACode);
    SetToken(LToken); // save as field for autoreconnect
    FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_LOGGED_IN, TgrBsonDocument.Create('token', LToken)));
    FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_USING_TOKEN)); // now we have the token
    Result := True;
  except
    on Ex: Exception do
    begin
      FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_LOGIN_ERROR));
    end;
  end;
end;

procedure TActionCreatorAgent.DDPLogout;
begin
  FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_LOGGING_OUT));
  FAgentConnection.Logout;
  FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_LOGGED_OUT));
end;

procedure TActionCreatorAgent.DDPSubscribe;
begin
  FDispatcher.DoDispatch(TFluxAction.Create(ACTION_AGENT_SUBSCRIBING));
  try
    FAgentData.SubscribeToSessions(FAgentId);
    FAgentData.SubscribeToCommands(FAgentId);
    FAgentData.SubscribeToAgentControl;
    FDispatcher.DoDispatch(TFluxAction.Create(ACTION_AGENT_SUBSCRIBED));
  except
    on Ex: Exception do
    begin
      FDispatcher.DoDispatch(TFluxAction.Create(ACTION_AGENT_SUBSCRIBE_ERROR));
    end;
  end;
end;

procedure TActionCreatorAgent.StartAutoReconnect;
var
  LToken: string;
begin
  FLock.Enter;
  try
    if GetAutoReconnect then
      Exit;

    LToken := GetToken;
    if LToken <> '' then
    begin
      SetAutoReconnect(True);
      DoInBackground(
        procedure
        begin
          Sleep(10000);
          FLock.Enter;
          try
            if not GetAutoReconnect then
              Exit;
            SetAutoReconnect(False);
          finally
            FLock.Leave;
          end;
          Log.Info('Reconnecting to AddressBook', TAG_CLIENT);
          Login(LToken);
        end);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TActionCreatorAgent.Login(const AUsername, APassword: string);
var
  LUsername: string;
  LPassword: string;
begin
  Log.Info('Logging in to AddressBook using username and password (%s)', [AUsername], TAG_CLIENT);

  LUsername := AUsername;
  LPassword := APassword;
  DoInBackground(
    procedure
    begin
      Build;
      FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_USING_CREDENTIALS));
      if not DDPConnect then
        Exit;
      if not DDPLogin(LUsername, LPassword) then
      begin
        DDPDisconnect;
        Exit;
      end;
      { TODO -oAC -cGeneral : handle subscription loading/error }
      DDPSubscribe;
    end);
end;

procedure TActionCreatorAgent.Login(const AToken: string);
var
  LToken: string;
begin
  Log.Info('Logging in to AddressBook using token', TAG_CLIENT);

  SetAutoReconnect(False);
  SetToken(AToken); // save as field for autoreconnect

  LToken := AToken;
  //DoInBackground(
  //  procedure
    //begin
      Build;
      FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_USING_TOKEN));
      if not DDPConnect then
        Exit;
      if not DDPLogin(LToken) then
      begin
        DDPDisconnect;
        Exit;
      end;
      { Always set standby to false when connected }
      SetStandby(False);
      { TODO -oAC -cGeneral : handle subscription loading/error }
      DDPSubscribe;
   // end);
end;

procedure TActionCreatorAgent.Login(const AUsername, APassword, A2FACode: string);
var
  LUsername: string;
  LPassword: string;
begin
  Log.Info('Logging in to AddressBook using 2-Step verification (%s)', [AUsername], TAG_CLIENT);

  LUsername := AUsername;
  LPassword := APassword;
  DoInBackground(
    procedure
    begin
      Build;
      FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_USING_CREDENTIALS));
      if not DDPConnect then
        Exit;
      if not DDPLogin(LUsername, LPassword, A2FACode) then
      begin
        DDPDisconnect;
        Exit;
      end;
      { TODO -oAC -cGeneral : handle subscription loading/error }
      DDPSubscribe;
    end);
end;

procedure TActionCreatorAgent.Logout;
var
  LToken: string;
begin
  Log.Info('Logging out from AddressBook', TAG_CLIENT);

  LToken := GetToken;

  DoInBackground(
    procedure
    begin
      if FStandby then
      begin
        Build;
        if not DDPConnect then
          Exit;
        if not DDPLogin(LToken) then
        begin
          DDPDisconnect;
          Exit;
        end;
      end;

      DDPLogout;
      DDPDisconnect;
      FAgentData := nil;
      FAgentConnection := nil;

      SetToken('');
    end);
end;

procedure TActionCreatorAgent.Standby;
begin
  if FStandby then
    Exit;

  if not Assigned(FAgentConnection) then
    Exit;

  Log.Info('AddressBook is going into standby', TAG_CLIENT);

  SetStandby(True);

  DoInBackground(
    procedure
    begin
      { Notify that standby is active }
      FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_STANDBY));
      { Disconnect the websocket }
      FAgentConnection.Disconnect;
    end);
end;

procedure TActionCreatorAgent.Resume;
var
  LToken: string;
begin
  if not FStandby then
    Exit;

  if not Assigned(FAgentConnection) then
    Exit;

  Log.Info('AddressBook is resuming from standby', TAG_CLIENT);

  LToken := GetToken;

  if LToken <> '' then
  begin
    DoInBackground(
      procedure
      begin
        { Login again using the same token }
        Login(LToken);
      end);
  end;
end;


// Eventi di connessione
procedure TActionCreatorAgent.OnConnected;
begin
  WriteLn('[ActionCreator] Agent connected: ' + FAgentId);
  FDispatcher.DoDispatch(TFluxAction.Create(ACTION_AGENT_SUBSCRIBED,
    TgrBsonDocument.Create.Add('agent_id', FAgentId)));
end;

procedure TActionCreatorAgent.OnDisconnected;
begin
  WriteLn('[ActionCreator] Agent disconnected: ' + FAgentId);
  FDispatcher.DoDispatch(TFluxAction.Create(ACTION_AGENT_SUBSCRIBE_ERROR,
    TgrBsonDocument.Create.Add('agent_id', FAgentId)));
end;

procedure TActionCreatorAgent.OnDisconnect(const ACode: Integer);
begin
  WriteLn(Format('[ActionCreator] Agent disconnect code %d: %s', [ACode, FAgentId]));
  FDispatcher.DoDispatch(TFluxAction.Create(ACTION_AGENT_SUBSCRIBE_ERROR,
    TgrBsonDocument.Create.Add('agent_id', FAgentId).Add('code', ACode)));
end;

procedure TActionCreatorAgent.OnError(const AError: string);
begin
  WriteLn('[ActionCreator] Agent error: ' + AError);
  FDispatcher.DoDispatch(TFluxAction.Create(ACTION_AGENT_SUBSCRIBE_ERROR,
    TgrBsonDocument.Create.Add('agent_id', FAgentId).Add('error', AError)));
end;

procedure TActionCreatorAgent.OnException(const AClassName, AMessage: string);
begin
  WriteLn(Format('[ActionCreator] Agent exception %s: %s', [AClassName, AMessage]));
  FDispatcher.DoDispatch(TFluxAction.Create(ACTION_AGENT_SUBSCRIBE_ERROR,
    TgrBsonDocument.Create.Add('agent_id', FAgentId).Add('exception', AClassName + ': ' + AMessage)));
end;

// Eventi per comandi
procedure TActionCreatorAgent.OnCommandReceived(const ACommandId, ASessionId, ACommandLine: string;
  const APriority: Integer; const ATimeout: Integer);
begin
  WriteLn(Format('[ActionCreator] Command received: %s -> %s', [ASessionId, ACommandLine]));
  FDispatcher.DoDispatch(TFluxAction.Create(ACTION_COMMAND_EXECUTING,
    TgrBsonDocument.Create
      .Add('agent_id', FAgentId)
      .Add('command_id', ACommandId)
      .Add('session_id', ASessionId)
      .Add('command_line', ACommandLine)
      .Add('priority', APriority)
      .Add('timeout', ATimeout)));
end;

procedure TActionCreatorAgent.OnCommandExecuting(const ACommandId, ASessionId: string);
begin
  WriteLn(Format('[ActionCreator] Command executing: %s', [ACommandId]));
  FDispatcher.DoDispatch(TFluxAction.Create(ACTION_COMMAND_EXECUTING,
    TgrBsonDocument.Create
      .Add('agent_id', FAgentId)
      .Add('command_id', ACommandId)
      .Add('session_id', ASessionId)));
end;

procedure TActionCreatorAgent.OnCommandCompleted(const ACommandId, ASessionId: string; const AExitCode: Integer);
begin
  WriteLn(Format('[ActionCreator] Command completed: %s (exit: %d)', [ACommandId, AExitCode]));
  FDispatcher.DoDispatch(TFluxAction.Create(ACTION_COMMAND_COMPLETED,
    TgrBsonDocument.Create
      .Add('agent_id', FAgentId)
      .Add('command_id', ACommandId)
      .Add('session_id', ASessionId)
      .Add('exit_code', AExitCode)));
end;

procedure TActionCreatorAgent.OnCommandError(const ACommandId, ASessionId, AError: string);
begin
  WriteLn(Format('[ActionCreator] Command error: %s -> %s', [ACommandId, AError]));
  FDispatcher.DoDispatch(TFluxAction.Create(ACTION_COMMAND_ERROR,
    TgrBsonDocument.Create
      .Add('agent_id', FAgentId)
      .Add('command_id', ACommandId)
      .Add('session_id', ASessionId)
      .Add('error', AError)));
end;

// Eventi per sessioni
procedure TActionCreatorAgent.OnSessionOpenRequested(const ASessionId, AUserId, AShellType: string;
  const AWorkingDir: string);
begin
  WriteLn(Format('[ActionCreator] Session open requested: %s (%s)', [ASessionId, AShellType]));
  FSessionManager.CreateSession(AShellType, ASessionId);
  {FDispatcher.DoDispatch(TFluxAction.Create(ACTION_SESSION_OPENING,
    TgrBsonDocument.Create
      .Add('agent_id', FAgentId)
      .Add('session_id', ASessionId)
      .Add('user_id', AUserId)
      .Add('shell_type', AShellType)
      .Add('working_dir', AWorkingDir)));
      }
end;

procedure TActionCreatorAgent.OnSessionOpened(const ASessionId: string);
begin
  WriteLn(Format('[ActionCreator] Session opened: %s', [ASessionId]));

  FDispatcher.DoDispatch(TFluxAction.Create(ACTION_SESSION_OPENED,
    TgrBsonDocument.Create
      .Add('agent_id', FAgentId)
      .Add('session_id', ASessionId)));
end;

procedure TActionCreatorAgent.OnSessionClosed(const ASessionId: string);
begin
  WriteLn(Format('[ActionCreator] Session closed: %s', [ASessionId]));
  FDispatcher.DoDispatch(TFluxAction.Create(ACTION_SESSION_CLOSED,
    TgrBsonDocument.Create
      .Add('agent_id', FAgentId)
      .Add('session_id', ASessionId)));
end;

procedure TActionCreatorAgent.OnSessionError(const ASessionId, AError: string);
begin
  WriteLn(Format('[ActionCreator] Session error: %s -> %s', [ASessionId, AError]));
  FDispatcher.DoDispatch(TFluxAction.Create(ACTION_SESSION_ERROR,
    TgrBsonDocument.Create
      .Add('agent_id', FAgentId)
      .Add('session_id', ASessionId)
      .Add('error', AError)));
end;

// Eventi di controllo
procedure TActionCreatorAgent.OnShutdownRequested;
begin
  WriteLn('[ActionCreator] Shutdown requested for agent: ' + FAgentId);
  // Qui potresti implementare la logica di shutdown
end;

procedure TActionCreatorAgent.OnRestartRequested;
begin
  WriteLn('[ActionCreator] Restart requested for agent: ' + FAgentId);
  // Qui potresti implementare la logica di restart
end;

procedure TActionCreatorAgent.OnStatusRequested;
begin
  WriteLn('[ActionCreator] Status requested for agent: ' + FAgentId);
  // Qui potresti implementare l'invio dello status
end;

procedure TActionCreatorAgent.RegisterAgent(const AAgentInfo: TJSONObject);
begin
  FDispatcher.DoDispatch(TFluxAction.Create(ACTION_AGENT_REGISTERING));
  try
    if Assigned(FAgentConnection) then
    begin
      FAgentData.RegisterAgent(FAgentId, AAgentInfo);
      FDispatcher.DoDispatch(TFluxAction.Create(ACTION_AGENT_REGISTERED));
    end
    else
    begin
      FDispatcher.DoDispatch(TFluxAction.Create(ACTION_AGENT_REGISTER_ERROR));
    end;
  except
    on Ex: Exception do
    begin
      FDispatcher.DoDispatch(TFluxAction.Create(ACTION_AGENT_REGISTER_ERROR));
    end;
  end;
end;

procedure TActionCreatorAgent.ping(const ASessionId: string);
begin
  FAgentData.ping(ASessionId);
end;

procedure TActionCreatorAgent.SetShellRunnerFactory(const ShellRunnerFactory: IShellRunnerFactory);
begin
  FShellRunnerFactory:= ShellRunnerFactory;
end;
procedure TActionCreatorAgent.SetSessionManager(const SetSessionManager: ISessionManager);
begin
  FSessionManager := SetSessionManager;
end;

procedure TActionCreatorAgent.SetServices;
begin
  //FAgentStore := GetStoreAgent(FAgentId);
  //FAgentStore.SetServices(FAgentData, FShellRunnerFactory);
end;

// Factory function
function GetActionCreatorAgentWithId(const AAgentId: string): IActionCreatorAgent;
begin
  if not _ActionCreators.ContainsKey(AAgentId) then
  begin
    _ActionCreators.Add(AAgentId, TActionCreatorAgent.Create(AAgentId));
  end;
  Result := _ActionCreators[AAgentId];
end;

initialization
  _ActionCreators := TDictionary<string, IActionCreatorAgent>.Create;

finalization
  _ActionCreators.Free;

end.
