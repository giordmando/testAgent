// RMC.Data.pas - Versione aggiornata senza callback errati

unit RMC.Data;

interface

uses
  DDP.Interfaces,
  System.JSON,
  System.SysUtils,
  System.Net.HttpClient,
  Flux.Dispatcher,
  System.Threading;  // AGGIUNTO;

type
  IAgentData = interface
    ['{F1E2D3C4-5B6A-7890-ABCD-EF1234567890}']

    // Gestione standby (come AddressBook)
    function GetToken: string;
    procedure SetStandby(const AValue: Boolean);
    procedure SetToken(const AValue: string);

    // Sottoscrizioni DDP
    procedure SubscribeToCommands(const AAgentId: string);
    procedure SubscribeToSessions(const AAgentId: string);
    procedure SubscribeToAgentControl;

    // Agent registration and status
    procedure RegisterAgent(const AAgentId: string; const AAgentInfo: TJSONObject);
    procedure SendAgentStatus(const AAgentId, AStatus: string);
    procedure UpdateAgentSessions(const AAgentId: string; const ASessions: array of string);

    // Session management
    procedure OpenSession(const AUserId: string; const AShellType: string;
      const AWorkingDir: string = ''; const AEnvironment: string = '');
    procedure CloseSession(const ASessionId: string);
    procedure CloseAllSessions(const AUserId: string = '');
    procedure SetSessionLimits(const AMaxSessions, AMaxSessionsPerUser, AMaxIdleTime: Integer);
    procedure SessionHeartbeat(const ASessionId: string);

    // Metodi per output
    procedure SendOutput(const AAgentId, ASessionId, AStream, AData: string);
    procedure SendCommandComplete(const AAgentId, ASessionId: string; const AExitCode: Integer);
    procedure SendError(const AAgentId, ASessionId, AError: string);
    procedure SendSessionStatus(const AAgentId, ASessionId, AStatus: string);

    procedure ExecuteCommand(const ASessionId: string; const ACommandLine: string;
      const ATimeout: Integer = 0; const APriority: Integer = 5);

    // Metodo per avviare le sottoscrizioni quando DDP è pronto
    procedure StartSubscriptions(const AAgentId: string);
    procedure ping(const ASessionId: string);

    function TestInsertSessionForAgent(const AAgentId: string): string;
    function TestInsertCommandForAgent(const AAgentId, ASessionId, ACommand: string): string;
  end;

function GetAgentData(const ADDPClient: IDDPClient): IAgentData;

implementation

uses
  Grijjy.Data.Bson,
  System.SyncObjs,
  System.DateUtils,
  DDP.RequestGenerator,
  Nanosystems.Http,
  Nanosystems.Http.Factory;

const
  // Subscriptions
  SESSIONS_SUBSCRIPTION = 'agent.sessions';
  COMMANDS_SUBSCRIPTION = 'agent.commands';
  AGENT_CONTROL_SUBSCRIPTION = 'agent.control';

  // Session methods
  SESSION_OPEN = 'sessions.open';
  SESSION_CLOSE = 'sessions.close';
  SESSION_CLOSE_ALL = 'sessions.closeAll';
  SESSION_SET_LIMITS = 'sessions.setLimits';
  SESSION_HEARTBEAT = 'sessions.heartbeat';
  SESSION_STATUS = 'session.sessionStatus';

  // Command methods
  COMMAND_EXECUTE = 'commands.execute';
  COMMAND_INPUT = 'commands.sendInput';
  COMMAND_CANCEL = 'commands.cancel';
  COMMAND_OUTPUT = 'commands.sendOutput';
  COMMAND_COMPLETED = 'commands.completed';
  COMMAND_SEND_ERROR = 'commands.sendError';

  // Agent methods
  AGENT_STATUS = 'agent.updateStatus';
  AGENT_SYSTEM_INFO = 'agent.sendSystemInfo';
  AGENT_REQUEST_SHELLS = 'agent.requestShells';
  AGENT_REGISTER = 'agent.register';
  AGENT_HEARTBEAT = 'agent.heartbeat';
  AGENT_UPDATE_SESSIONS = 'agent.updateSessions';
  AGENT_UNREGISTER = 'agent.unregister';
  TEST_PING = 'test.ping';
type
  TAgentData = class(TInterfacedObject, IAgentData)
  private
    FDDPClient: IDDPClient;
    FLock: TCriticalSection;
    FStandby: Boolean;
    FToken: string;
    FRestUrl: string;

    procedure CallMethod(const AMethodName: string; const AParams: TgrBsonArray);
    procedure DDPMethod(const AMethodName: string; const AParams: TgrBsonArray);
    procedure HttpMethod(const AMethodName: string; const AParams: TgrBsonArray);

  protected
    { IAgentData implementation }

    function GetToken: string;
    procedure SetStandby(const AValue: Boolean);
    procedure SetToken(const AValue: string);

    procedure SubscribeToCommands(const AAgentId: string);
    procedure SubscribeToSessions(const AAgentId: string);
    procedure SubscribeToAgentControl;

    procedure OpenSession(const AUserId: string; const AShellType: string;
      const AWorkingDir: string = ''; const AEnvironment: string = '');
    procedure CloseSession(const ASessionId: string);
    procedure CloseAllSessions(const AUserId: string = '');
    procedure SetSessionLimits(const AMaxSessions, AMaxSessionsPerUser, AMaxIdleTime: Integer);
    procedure SessionHeartbeat(const ASessionId: string);

    procedure RegisterAgent(const AAgentId: string; const AAgentInfo: TJSONObject);
    procedure UpdateAgentStatus(const AAgentId, AStatus: string);
    procedure UpdateAgentSessions(const AAgentId: string; const ASessions: array of string);
    procedure UnregisterAgent(const AAgentId: string);
    procedure SendAgentStatus(const AAgentId, AStatus: string);

    procedure SendOutput(const AAgentId, ASessionId, AStream, AData: string);
    procedure SendCommandComplete(const AAgentId, ASessionId: string; const AExitCode: Integer);
    procedure SendError(const AAgentId, ASessionId, AError: string);
    procedure SendSessionStatus(const AAgentId, ASessionId, AStatus: string);
    procedure ExecuteCommand(const ASessionId: string; const ACommandLine: string;
      const ATimeout: Integer = 0; const APriority: Integer = 5);

    procedure StartSubscriptions(const AAgentId: string);
    procedure ping(const ASessionId: string);

    function TestInsertSessionForAgent(const AAgentId: string): string;
    function TestInsertCommandForAgent(const AAgentId, ASessionId, ACommand: string): string;
  public
    constructor Create(const ADDPClient: IDDPClient);
    destructor Destroy; override;
  end;

{ TAgentData }

constructor TAgentData.Create(const ADDPClient: IDDPClient);
begin
  Assert(Assigned(ADDPClient));
  inherited Create;
  FDDPClient := ADDPClient;
  FLock := TCriticalSection.Create;

  // Configure your REST URL here
  FRestUrl := 'http://localhost:3000';
end;

destructor TAgentData.Destroy;
begin
  FLock.Free;
  inherited;
end;


function TAgentData.TestInsertSessionForAgent(const AAgentId: string): string;
begin
  try
    WriteLn('[AgentData] Calling test.insertSessionForAgent...');
    var SessionResult := FDDPClient.Method('test.insertSessionForAgent',
      TgrBsonArray.Create([AAgentId]));

    if not SessionResult.IsNil then
      Result := SessionResult.AsString
    else
      Result := '';

    WriteLn('[AgentData] Session inserted: ' + Result);
  except
    on E: Exception do
    begin
      WriteLn('[AgentData] Error in TestInsertSessionForAgent: ' + E.Message);
      Result := '';
    end;
  end;
end;

function TAgentData.TestInsertCommandForAgent(const AAgentId, ASessionId, ACommand: string): string;
begin
  try
    WriteLn('[AgentData] Calling test.insertCommandForAgent...');
    var CommandResult := FDDPClient.Method('test.insertCommandForAgent', TgrBsonArray.Create([
      TgrBsonDocument.Create
        .Add('agent_id', AAgentId)
        .Add('session_id', ASessionId)
        .Add('command', ACommand)
        .Add('priority', 5)
        .Add('status', 'pending')
    ]));

    if not CommandResult.IsNil then
      Result := CommandResult.AsString
    else
      Result := '';

    WriteLn('[AgentData] Command inserted: ' + Result);
  except
    on E: Exception do
    begin
      WriteLn('[AgentData] Error in TestInsertCommandForAgent: ' + E.Message);
      Result := '';
    end;
  end;
end;


procedure TAgentData.CallMethod(const AMethodName: string; const AParams: TgrBsonArray);
begin
  if FStandby then
    HttpMethod(AMethodName, AParams)
  else
    DDPMethod(AMethodName, AParams);
end;

procedure TAgentData.DDPMethod(const AMethodName: string; const AParams: TgrBsonArray);
begin
  try
    // Chiamata asincrona - non aspetta la risposta
    TTask.Run(
      procedure
      begin
        try
          var Result := FDDPClient.Method(AMethodName, AParams);
          WriteLn(Format('[Data] DDP method %s completed successfully', [AMethodName]));
          WriteLn('%s response: %s', AMethodName, Result.AsString);
        except
          on E: Exception do
            WriteLn(Format('[Data] DDP method %s failed: %s', [AMethodName, E.Message]));
        end;
      end);

    WriteLn(Format('[Data] DDP method %s called asynchronously', [AMethodName]));
  except
    on E: Exception do
      WriteLn(Format('[Data] Error calling DDP method %s: %s', [AMethodName, E.Message]));
  end;
end;


procedure TAgentData.HttpMethod(const AMethodName: string; const AParams: TgrBsonArray);
var
  LHttpClient: InsHttpClient;
  LHttpClientRequest: InsHttpClientRequest;
begin
  LHttpClient := TnsHttpFactory.GetHttpClient;

  LHttpClientRequest := TnsHttpClientRequest.Create;
  LHttpClientRequest.Authorization := 'Bearer ' + GetToken;
  LHttpClientRequest.Body := TEncoding.UTF8.GetBytes(AParams.ToJson);
  LHttpClientRequest.ContentType := 'application/json';

  LHttpClient.Post(FRestUrl + '/methods/' + AMethodName, LHttpClientRequest);
end;

function TAgentData.GetToken: string;
begin
  FLock.Enter;
  try
    Result := FToken;
  finally
    FLock.Leave;
  end;
end;

procedure TAgentData.SetStandby(const AValue: Boolean);
begin
  FStandby := AValue;
end;

procedure TAgentData.SetToken(const AValue: string);
begin
  FLock.Enter;
  try
    FToken := AValue;
  finally
    FLock.Leave;
  end;
end;

// Metodo per avviare le sottoscrizioni quando DDP è pronto
procedure TAgentData.StartSubscriptions(const AAgentId: string);
begin
  WriteLn('[Data] Starting subscriptions for agent: ' + AAgentId);

  try
    SubscribeToSessions(AAgentId);
    SubscribeToCommands(AAgentId);
    SubscribeToAgentControl;

    WriteLn('[Data] All subscriptions started successfully');
  except
    on E: Exception do
      WriteLn('[Data] Error starting subscriptions: ' + E.Message);
  end;
end;

// ========================================================================
// SOTTOSCRIZIONI DDP
// ========================================================================

procedure TAgentData.SubscribeToCommands(const AAgentId: string);
begin
  try
    // CORRETTO: Passa un oggetto con agent_id come si aspetta la pubblicazione
    FDDPClient.Subscribe(COMMANDS_SUBSCRIPTION,
      TgrBsonArray.Create([
        TgrBsonDocument.Create.Add('agent_id', AAgentId)
      ]));
    WriteLn('[Data] Subscribed to commands for agent: ' + AAgentId);
  except
    on E: Exception do
      WriteLn('[Data] Error subscribing to commands: ' + E.Message);
  end;
end;

procedure TAgentData.SubscribeToSessions(const AAgentId: string);
begin
  try
    // CORRETTO: Passa un oggetto con agent_id come si aspetta la pubblicazione
    FDDPClient.Subscribe(SESSIONS_SUBSCRIPTION,
      TgrBsonArray.Create([
        TgrBsonDocument.Create.Add('agent_id', AAgentId)
      ]));
    WriteLn('[Data] Subscribed to sessions for agent: ' + AAgentId);
  except
    on E: Exception do
      WriteLn('[Data] Error subscribing to sessions: ' + E.Message);
  end;
end;

procedure TAgentData.SubscribeToAgentControl;
begin
  try
    // Per agent control, non servono parametri se è globale
    FDDPClient.Subscribe(AGENT_CONTROL_SUBSCRIPTION, TgrBsonArray.Create);
    WriteLn('[Data] Subscribed to agent control');
  except
    on E: Exception do
      WriteLn('[Data] Error subscribing to agent control: ' + E.Message);
  end;
end;

// ========================================================================
// METODI DDP PER REGISTRAZIONE AGENT
// ========================================================================

procedure TAgentData.RegisterAgent(const AAgentId: string; const AAgentInfo: TJSONObject);
var
  InfoDoc: TgrBsonDocument;
begin
  try
    InfoDoc := TgrBsonDocument.Parse(AAgentInfo.ToJSON);

    CallMethod(AGENT_REGISTER,
      TgrBsonArray.Create([
        TgrBsonDocument.Create
          .Add('agent_id', AAgentId)
          .Add('info', InfoDoc)
          .Add('status', 'online')
          .Add('registered_at', DateToISO8601(Now, True))
      ]));

    WriteLn('[Data] Agent registered: ' + AAgentId);
  except
    on E: Exception do
      WriteLn('[Data] Error registering agent: ' + E.Message);
  end;
end;

procedure TAgentData.UpdateAgentStatus(const AAgentId, AStatus: string);
begin
  CallMethod(AGENT_STATUS,
    TgrBsonArray.Create([
      TgrBsonDocument.Create
        .Add('agent_id', AAgentId)
        .Add('status', AStatus)
        .Add('updated_at', DateToISO8601(Now, True))
    ]));
end;

procedure TAgentData.UpdateAgentSessions(const AAgentId: string; const ASessions: array of string);
var
  SessionsArray: TgrBsonArray;
  i: Integer;
begin
  SessionsArray := TgrBsonArray.Create;
  for i := 0 to High(ASessions) do
    SessionsArray.Add(ASessions[i]);

  CallMethod(AGENT_UPDATE_SESSIONS,
    TgrBsonArray.Create([
      TgrBsonDocument.Create
        .Add('agent_id', AAgentId)
        .Add('sessions', SessionsArray)
        .Add('session_count', Length(ASessions))
        .Add('updated_at', DateToISO8601(Now, True))
    ]));
end;

procedure TAgentData.UnregisterAgent(const AAgentId: string);
begin
  CallMethod(AGENT_UNREGISTER,
    TgrBsonArray.Create([
      TgrBsonDocument.Create
        .Add('agent_id', AAgentId)
        .Add('unregistered_at', DateToISO8601(Now, True))
    ]));
end;

procedure TAgentData.SendAgentStatus(const AAgentId, AStatus: string);
var
  LBsonDoc: TgrBsonDocument;
begin
  LBsonDoc := TgrBsonDocument.Create;
  LBsonDoc['agentId'] := AAgentId;
  LBsonDoc['status'] := AStatus;
  LBsonDoc['timestamp'] := Now;

  CallMethod(AGENT_STATUS, TgrBsonArray.Create([LBsonDoc]));
end;

// Session management
procedure TAgentData.OpenSession(const AUserId: string; const AShellType: string;
  const AWorkingDir: string; const AEnvironment: string);
var
  LBsonDoc: TgrBsonDocument;
begin
  LBsonDoc := TgrBsonDocument.Create;
  LBsonDoc['userId'] := AUserId;
  LBsonDoc['shellType'] := AShellType;
  LBsonDoc['workingDir'] := AWorkingDir;
  LBsonDoc['environment'] := AEnvironment;
  LBsonDoc['timestamp'] := Now;

  CallMethod(SESSION_OPEN, TgrBsonArray.Create([LBsonDoc]));
end;

procedure TAgentData.CloseSession(const ASessionId: string);
begin
  CallMethod(SESSION_CLOSE, TgrBsonArray.Create([ASessionId]));
end;

procedure TAgentData.CloseAllSessions(const AUserId: string);
begin
  if AUserId <> '' then
    CallMethod(SESSION_CLOSE_ALL, TgrBsonArray.Create([AUserId]))
  else
    CallMethod(SESSION_CLOSE_ALL, TgrBsonArray.Create([]));
end;

procedure TAgentData.SetSessionLimits(const AMaxSessions, AMaxSessionsPerUser, AMaxIdleTime: Integer);
var
  LLimits: TgrBsonDocument;
begin
  LLimits := TgrBsonDocument.Create;
  LLimits['maxSessions'] := AMaxSessions;
  LLimits['maxSessionsPerUser'] := AMaxSessionsPerUser;
  LLimits['maxIdleTime'] := AMaxIdleTime;

  CallMethod(SESSION_SET_LIMITS, TgrBsonArray.Create([LLimits]));
end;

procedure TAgentData.SessionHeartbeat(const ASessionId: string);
begin
  CallMethod(SESSION_HEARTBEAT, TgrBsonArray.Create([ASessionId, Now]));
end;

// ========================================================================
// METODI DDP PER OUTPUT
// ========================================================================

procedure TAgentData.SendOutput(const AAgentId, ASessionId, AStream, AData: string);
begin
  // Usa la versione asincrona
  TTask.Run(
    procedure
    begin
      CallMethod(COMMAND_OUTPUT,
        TgrBsonArray.Create([
          TgrBsonDocument.Create
            .Add('agent_id', AAgentId)
            .Add('session_id', ASessionId)
            .Add('stream', AStream)
            .Add('data', AData)
            .Add('timestamp', DateToISO8601(Now, True))
        ]));
    end);
end;

procedure TAgentData.SendCommandComplete(const AAgentId, ASessionId: string; const AExitCode: Integer);
begin
  // Usa la versione asincrona
  TTask.Run(
    procedure
    begin
      CallMethod(COMMAND_COMPLETED,
        TgrBsonArray.Create([
          TgrBsonDocument.Create
            .Add('agent_id', AAgentId)
            .Add('session_id', ASessionId)
            .Add('exit_code', AExitCode)
            .Add('timestamp', DateToISO8601(Now, True))
        ]));
    end);
end;

procedure TAgentData.SendError(const AAgentId, ASessionId, AError: string);
begin
  // Usa la versione asincrona
  TTask.Run(
    procedure
    begin
      CallMethod(COMMAND_SEND_ERROR,
        TgrBsonArray.Create([
          TgrBsonDocument.Create
            .Add('agent_id', AAgentId)
            .Add('session_id', ASessionId)
            .Add('error', AError)
            .Add('timestamp', DateToISO8601(Now, True))
        ]));
    end);
end;

procedure TAgentData.SendSessionStatus(const AAgentId, ASessionId, AStatus: string);
begin
  CallMethod(SESSION_STATUS,
    TgrBsonArray.Create([
      TgrBsonDocument.Create
        .Add('agent_id', AAgentId)
        .Add('session_id', ASessionId)
        .Add('status', AStatus)
        .Add('timestamp', DateToISO8601(Now, True))
    ]));
end;

// Command execution
procedure TAgentData.ExecuteCommand(const ASessionId: string; const ACommandLine: string;
  const ATimeout: Integer; const APriority: Integer);
var
  LBsonDoc: TgrBsonDocument;
begin
  LBsonDoc := TgrBsonDocument.Create;
  LBsonDoc['sessionId'] := ASessionId;
  LBsonDoc['commandLine'] := ACommandLine;
  LBsonDoc['timeout'] := ATimeout;
  LBsonDoc['priority'] := APriority;
  LBsonDoc['timestamp'] := Now;

  CallMethod(COMMAND_EXECUTE, TgrBsonArray.Create([LBsonDoc]));
end;

// ========================================================================
// TEST FUNCTION
// ========================================================================


// Command execution
procedure TAgentData.ping(const ASessionId: string);
var
  LBsonDoc: TgrBsonDocument;
begin
  LBsonDoc := TgrBsonDocument.Create;
  LBsonDoc['sessionId'] := ASessionId;
  LBsonDoc['timestamp'] := Now;

  CallMethod(TEST_PING, TgrBsonArray.Create([LBsonDoc]));
end;
// ========================================================================
// FACTORY FUNCTION
// ========================================================================

function GetAgentData(const ADDPClient: IDDPClient): IAgentData;
begin
  Result := TAgentData.Create(ADDPClient);
end;

end.
