unit RMC.ActionCreator;

interface

uses
  System.JSON, System.SysUtils, System.DateUtils,
  RMC.Data, AgentServiceI, RMC.Connection, Grijjy.Data.Bson;

type
  IActionCreatorAgent = interface
    ['{B8F3E2A5-1234-4567-89AB-123456789ABC}']
    // Connection events

        // Login/Registration
    procedure Login(const AUsername, APassword: string); overload;
    procedure Login(const AToken: string); overload;
    procedure Login(const AUsername, APassword, A2FACode: string); overload;
    procedure Logout;
    procedure Standby;
    procedure Resume;

    procedure OnDisconnect(const ACode: Integer);
    procedure OnError(const AMessage: string);
    procedure OnException(const AClassName, AMessage: string);

    // Command events - chiamati quando arrivano comandi via DDP
    procedure OnCommandReceived(const AAgentId, ASessionId, AAction, ACommand: string);
    procedure OnSessionRequest(const AAgentId, ASessionId, AShellType, AUrl: string);
    procedure OnSessionClose(const AAgentId, ASessionId: string);



    procedure RegisterAgent(const AAgentInfo: TJSONObject);

    // Helper methods
    procedure SetCommandCallback(const ACallback: TCommandProc);

    // NUOVO: Accesso all'AgentConnection
    function GetRMCAgentConnection: IAgentConnection;
    function IsLoginCompleted: Boolean;
    function GetACAgentData: IAgentData;
    procedure ForceMarkLoginCompleted;


  end;

function GetActionCreatorAgentWithId(const AAgentId: string): IActionCreatorAgent;
procedure Clear;

implementation

uses
  DDP.Interfaces,
  DDP.Actions.Consts,
  DDP.Factories,
  DDP.NetLib.Factory,
  DDP.RequestGenerator,
  DDP.Exception,
  Flux.Actions,
  Flux.ActionCreator.Base,
  System.Classes, System.SyncObjs, RMC.Actions.Consts;

type
  TActionCreatorAgent = class(TActionCreatorBase, IActionCreatorAgent)
  private
    FLock: TCriticalSection;
    FDDPRequestGenerator: IDDPRequestGenerator;
    FAgentConnection: IAgentConnection;
    FAgentData: IAgentData;
    FAgentId: string;
    FAutoReconnect: Boolean;
    FToken: string;
    FCommandCallback: TCommandProc;
    FLoginCompleted: Boolean;
    FConnected: Boolean;
    FStandby: Boolean;
    function GetAutoReconnect: Boolean;
    function GetToken: string;
    procedure SetAutoReconnect(const AValue: Boolean);
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
    procedure SetStandby(const AValue: Boolean);
    procedure StartCleanupTimer;

  protected
    { IActionCreatorAgent implementation }
    procedure OnDisconnect(const ACode: Integer);
    procedure OnError(const AMessage: string);
    procedure OnException(const AClassName, AMessage: string);
    procedure OnCommandReceived(const AAgentId, ASessionId, AAction, ACommand: string);
    procedure OnSessionRequest(const AAgentId, ASessionId, AShellType, AUrl: string);
    procedure OnSessionClose(const AAgentId, ASessionId: string);

    procedure Login(const AUsername, APassword: string); overload;
    procedure Login(const AToken: string); overload;
    procedure Login(const AUsername, APassword, A2FACode: string); overload;
    procedure Logout;
    procedure Standby;
    procedure Resume;
    procedure RegisterAgent(const AAgentInfo: TJSONObject);
    procedure SetCommandCallback(const ACallback: TCommandProc);
    function GetRMCAgentConnection: IAgentConnection;
    function IsLoginCompleted: Boolean;

  public
    constructor Create(const AAgentId: string);
    destructor Destroy; override;
    function GetACAgentData: IAgentData;
    procedure ForceMarkLoginCompleted;
  end;

var
  _Lock: TCriticalSection;
  _ActionCreatorAgent: IActionCreatorAgent;

{ TActionCreatorAgent }

constructor TActionCreatorAgent.Create(const AAgentId: string);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FDDPRequestGenerator := TDDPRequestGenerator.Create;
  FAgentId := AAgentId;
  FAutoReconnect := False;
  FCommandCallback := nil;
  FLoginCompleted := False;
  FConnected := False;
end;

destructor TActionCreatorAgent.Destroy;
begin
  // *** LIBERA AGENT DATA PRIMA DI CONNECTION (come AddressBook) ***
  FAgentData := nil;
  FAgentConnection := nil;
  //
  FLock.Free;
  inherited;
end;

  procedure TActionCreatorAgent.ForceMarkLoginCompleted;
begin
  FLock.Enter;
  try
    FLoginCompleted := True;
    FConnected := True;
    Writeln('[ACTIONCREATOR] *** LOGIN FORZATO COME COMPLETATO ***');
  finally
    FLock.Leave;
  end;
end;

function TActionCreatorAgent.GetACAgentData: IAgentData;
begin
    Result := FAgentData;
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
      FAgentData.SetToken(GetToken);
      FAgentData.SetStandby(AValue);
    end;
  end
  else
  begin
    if Assigned(FAgentData) then
    begin
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

procedure TActionCreatorAgent.SetCommandCallback(const ACallback: TCommandProc);
begin
  FCommandCallback := ACallback;
end;

function TActionCreatorAgent.GetRMCAgentConnection: IAgentConnection;
begin
  Result := FAgentConnection;
end;

function TActionCreatorAgent.IsLoginCompleted: Boolean;
begin
  FLock.Enter;
  try
    Result := FLoginCompleted and FConnected and Assigned(FAgentConnection);
  finally
    FLock.Leave;
  end;
end;

procedure TActionCreatorAgent.Build;
var
  LDDPNetLib: IDDPNetLib;
  LDDPClient: IDDPClient;
  LDDPLogin: IDDPLogin;
begin
  FAgentData := nil;
  FAgentConnection := nil;
  LDDPNetLib := TDDPNetLibFactory.CreateNew;
  LDDPClient := GetDDPClient(LDDPNetLib, FDDPRequestGenerator);
  LDDPLogin := GetDDPLogin(LDDPClient);
  FAgentConnection := GetAgentConnection(LDDPClient, LDDPLogin);
  FAgentData := GetAgentData(LDDPClient);
end;


function TActionCreatorAgent.DDPConnect: Boolean;
begin
  Result := False;

  Writeln('[DDPCONNECT] === INIZIO DDPConnect ===');
  FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_CONNECTING));

  try
    if not Assigned(FAgentConnection) then
    begin
      Writeln('[DDPCONNECT] AgentConnection non assegnata');
      Exit;
    end;

    Writeln('[DDPCONNECT] Chiamata FAgentConnection.Connect...');
    FAgentConnection.Connect;
    Writeln('[DDPCONNECT] FAgentConnection.Connect completato');

    // Non impostiamo ancora FConnected qui - arriverà dall'evento
    FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_CONNECTED));
    Writeln('[DDPCONNECT] Action CONNECTED inviata');

    Result := True;

  except
    on Ex: Exception do
    begin
      Writeln(Format('[DDPCONNECT] ECCEZIONE: %s - %s', [Ex.ClassName, Ex.Message]));
      FConnected := False;
      FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_CONNECTION_EXCEPTION,
        TgrBsonDocument.Create('name', Ex.ClassName).Add('msg', Ex.Message)));
      Result := False;
    end;
  end;

  Writeln(Format('[DDPCONNECT] === FINE DDPConnect (Result: %s) ===', [BoolToStr(Result, True)]));
end;

procedure TActionCreatorAgent.DDPDisconnect;
begin
  FAgentConnection.Disconnect;
  FConnected := False;
  FLoginCompleted := False;
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
    SetToken(LToken);
    FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_LOGGED_IN,
      TgrBsonDocument.Create('token', LToken)));
    FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_USING_TOKEN));
    Result := True;
  except
    on E: ELockedLoginException do
    begin
      FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_LOGIN_LOCKED_LOGIN,
        TgrBsonDocument.Create('seconds', E.Seconds)));
    end;
    on E: E2FAuthenticationRequired do
    begin
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
    SetToken(LToken);
    FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_LOGGED_IN,
      TgrBsonDocument.Create('token', LToken)));
    FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_USING_TOKEN));
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
    // Sottoscrivi alla collezione comandi per questo agent
    FAgentData.SubscribeToCommands(FAgentId);
    FAgentData.SubscribeToSessions(FAgentId);
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
    if GetAutoReconnect then Exit;

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
            if not GetAutoReconnect then Exit;
            SetAutoReconnect(False);
          finally
            FLock.Leave;
          end;
          Login(LToken);
        end);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TActionCreatorAgent.StartCleanupTimer;
begin
  // Start background cleanup for expired/idle sessions
  DoInBackground(
    procedure
    begin
      while not FStandby do
      begin
        Sleep(60000); // Check every minute
        try
          //if Assigned(FSessionManager) then
          begin
          //  FSessionManager.CleanupExpiredSessions;
          //  FSessionManager.CleanupIdleSessions;
          end;
        except
          on E: Exception do
            Writeln('Session cleanup error: %s', E.Message);
        end;
      end;
    end);
end;

// Event Handlers - chiamati dal NetLib quando arrivano eventi DDP



procedure TActionCreatorAgent.OnDisconnect(const ACode: Integer);
begin
   Writeln('Agent disconnected with code %d', ACode);
  FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_DISCONNECTED,
    TgrBsonDocument.Create('code', ACode)));
  if ACode = 1000 then // Normal Closure by RFC 6455
    StartAutoReconnect;
end;

procedure TActionCreatorAgent.OnError(const AMessage: string);
begin
Writeln('Agent connection error: %s', AMessage);
  FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_CONNECTION_ERROR,
    TgrBsonDocument.Create('msg', AMessage)));
  StartAutoReconnect;
end;

procedure TActionCreatorAgent.OnException(const AClassName, AMessage: string);
begin
Writeln('Agent raised %s: %s', AClassName, AMessage);
  FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_CONNECTION_EXCEPTION,
    TgrBsonDocument.Create('name', AClassName).Add('msg', AMessage)));
  StartAutoReconnect;
end;


// Public Interface Methods

procedure TActionCreatorAgent.Login(const AUsername, APassword: string);
var
  LUsername, LPassword: string;
begin
  LUsername := AUsername;
  LPassword := APassword;

  // *** USA IL PATTERN DOBACKGROUND COME ADDRESSBOOK ***
  DoInBackground(
    procedure
    begin
      Build;
      FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_USING_CREDENTIALS));
      if not DDPConnect then Exit;
      if not DDPLogin(LUsername, LPassword) then
      begin
        DDPDisconnect;
        Exit;
      end;
      DDPSubscribe;
      StartCleanupTimer;
    end);
end;

procedure TActionCreatorAgent.Login(const AToken: string);
var
  LToken: string;
begin
  SetAutoReconnect(False);
  SetToken(AToken);

  LToken := AToken;
  DoInBackground(
    procedure
    begin
      Build;
      FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_USING_TOKEN));
      if not DDPConnect then Exit;
      if not DDPLogin(LToken) then
      begin
        DDPDisconnect;
        Exit;
      end;
      SetStandby(False);
      DDPSubscribe;
      StartCleanupTimer;
    end);
end;

procedure TActionCreatorAgent.Login(const AUsername, APassword, A2FACode: string);
var
  LUsername, LPassword: string;
begin
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
      DDPSubscribe;
      StartCleanupTimer;
    end);
end;

procedure TActionCreatorAgent.Logout;
var
  LToken: string;
begin

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

  SetStandby(True);
  DoInBackground(
    procedure
    begin
      FDispatcher.DoDispatch(TFluxAction.Create(ACTION_DDP_STANDBY));
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

  LToken := GetToken;
  if LToken <> '' then
  begin
    DoInBackground(
      procedure
      begin
        Login(LToken);
      end);
  end;
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


{TODO Spostare fuori il setup}


procedure TActionCreatorAgent.OnCommandReceived(const AAgentId, ASessionId, AAction, ACommand: string);
var
  LSessionId, LCommandLine: string;
  LTimeout, LPriority: Integer;
begin
  LSessionId := ASessionId;
  LCommandLine := ACommand;

  DoInBackground(
    procedure
    begin
      FAgentData.ExecuteCommand(LSessionId, LCommandLine, LTimeout, LPriority);
    end);
end;
 {TODO uniformare passando AAction}
procedure TActionCreatorAgent.OnSessionRequest(const AAgentId, ASessionId, AShellType, AUrl: string);
var
  LUserId: string;
  LShellType: string;
  LWorkingDir, LEnvironment: string;
begin

  LShellType := AShellType;
  //LWorkingDir := AWorkingDir;
  //LEnvironment := AEnvironment;

  DoInBackground(
    procedure
    begin
      FAgentData.OpenSession(LUserId, LShellType, LWorkingDir, LEnvironment);
    end);
end;

procedure TActionCreatorAgent.OnSessionClose(const AAgentId, ASessionId: string);
var
  LSessionId: string;
begin
  LSessionId := ASessionId;
  DoInBackground(
    procedure
    begin
      FAgentData.CloseSession(LSessionId);
    end);
end;

// Global Functions

function GetActionCreatorAgentWithId(const AAgentId: string): IActionCreatorAgent;
begin
  if not Assigned(_ActionCreatorAgent) then
  begin
    _Lock.Enter;
    try
      if not Assigned(_ActionCreatorAgent) then
      begin
        _ActionCreatorAgent := TActionCreatorAgent.Create(AAgentId);
      end;
    finally
      _Lock.Leave;
    end;
  end;
  Result := _ActionCreatorAgent;
end;

procedure Clear;
begin
  _ActionCreatorAgent := nil;
end;

initialization

_Lock := TCriticalSection.Create;

finalization

_Lock.Free;

end.
