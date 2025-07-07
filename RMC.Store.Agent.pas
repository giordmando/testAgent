unit RMC.Store.Agent;

interface

uses
  System.Generics.Collections;

type
  IStoreAgentData = interface
    // Filters
    //procedure AddOrSetSessionFilter(const AName: string; const AFilter: TSessionFilter);
    //procedure AddOrSetCommandFilter(const AName: string; const AFilter: TCommandFilter);
    //procedure ApplyFilters;
    //procedure ClearFilters;
    //procedure RemoveSessionFilter(const AName: string);
    //procedure RemoveCommandFilter(const AName: string);

    // Data access
    //function GetSessions: TSessions;
    //function GetSession(const AId: string): TSession;
    //function GetCommands: TCommands;
    //function GetCommand(const AId: string): TCommand;
    //function GetFilteredSessions: TSessions;
    //function GetFilteredCommands: TCommands;
    //function GetLoggedInUserId: string;
    function GetAgentId: string;
    function GetSubscriptionReady: Boolean;

    // Session statistics
    //function GetSessionCount: Integer;
    //function GetActiveSessionCount: Integer;
    //function GetUserSessionCount(const AUserId: string): Integer;
    //function GetSessionsByUser(const AUserId: string): TSessions;
    //function GetSessionsByShellType(const AShellType: string): TSessions;

    // Command statistics
    //function GetCommandCount: Integer;
    //function GetExecutingCommandCount: Integer;
    //function GetCompletedCommandCount: Integer;
    //function GetFailedCommandCount: Integer;
    //function GetCommandsBySession(const ASessionId: string): TCommands;
    //function GetCommandsByStatus(const AStatus: TCommandStatus): TCommands;

    // Session management helpers
    //function CanCreateSession(const AUserId: string): Boolean;
    //function GetSessionLimits: string; // JSON with current limits
    //function GetSessionStatistics: string; // JSON with statistics

    property SubscriptionReady: Boolean read GetSubscriptionReady;
  end;

function GetStoreAgentData(const AAgentId: string): IStoreAgentData;

implementation

uses
  DDP.Consts,
  System.SyncObjs,
  RMC.Actions.Consts,
  RMC.Store.Events,
  DDP.Actions.Consts,
  DDP.Interfaces,
  Flux.Actions,
  Flux.Store.Base,
  Grijjy.Data.Bson,
  Grijjy.System.Messaging,
  System.SysUtils,
  System.DateUtils;

const
  // Nomi delle collezioni sul server Meteor
  COMMANDS_COLLECTION = 'commands';
  SESSIONS_COLLECTION = 'sessions';
  AGENTS_COLLECTION = 'agents';
  OUTPUTS_COLLECTION = 'outputs';


  type
  TStoreAgentData = class(TStoreBase, IStoreAgentData)
  private

    // Filters
    //FSessionFilters: TDictionary<string, TSessionFilter>;
    //FCommandFilters: TDictionary<string, TCommandFilter>;

    // Data
    //FSessions: TSessions;
    //FCommands: TCommands;
    //FFilteredSessions: TSessions; // Filtered list of sessions
    //FFilteredCommands: TCommands; // Filtered list of commands

    // Search dictionaries for fast lookup
    //FSessionsDict: TObjectDictionary<string, TSession>;
    //FCommandsDict: TObjectDictionary<string, TCommand>;

    FSessionsSorted: Boolean;
    FCommandsSorted: Boolean;
    FSubscriptionReady: Boolean;
    FLoggedInUserId: string;
    FAgentId: string;
    FSessionLimits: TgrBsonDocument;

    procedure OnAdded(const ACollection, AId: string; const AFields: TgrBsonDocument);
    procedure OnChanged(const ACollection, AId: string; const AFields: TgrBsonDocument); overload;
    procedure OnChanged(const ACollection, AId: string; const ACleared: TgrBsonArray); overload;
    procedure OnRemoved(const ACollection, AId: string);
  protected
    procedure EmitStoreChange; override;
    procedure OnAction(const ASender: TObject; const AAction: TgrMessage); override;
  protected
    { IStoreAgentData implementation }
    {procedure AddOrSetSessionFilter(const AName: string; const AFilter: TSessionFilter);
    procedure AddOrSetCommandFilter(const AName: string; const AFilter: TCommandFilter);
    procedure ApplyFilters;
    procedure ClearFilters;
    procedure RemoveSessionFilter(const AName: string);
    procedure RemoveCommandFilter(const AName: string);
    function GetSessions: TSessions;
    function GetSession(const AId: string): TSession;
    function GetCommands: TCommands;
    function GetCommand(const AId: string): TCommand;
    function GetFilteredSessions: TSessions;
    function GetFilteredCommands: TCommands;
    function GetLoggedInUserId: string;
    function GetAgentId: string;
    function GetSubscriptionReady: Boolean;
    function GetSessionCount: Integer;
    function GetActiveSessionCount: Integer;
    function GetUserSessionCount(const AUserId: string): Integer;
    function GetSessionsByUser(const AUserId: string): TSessions;
    function GetSessionsByShellType(const AShellType: TShellType): TSessions;
    function GetCommandCount: Integer;
    function GetExecutingCommandCount: Integer;
    function GetCompletedCommandCount: Integer;
    function GetFailedCommandCount: Integer;
    function GetCommandsBySession(const ASessionId: string): TCommands;
    function GetCommandsByStatus(const AStatus: TCommandStatus): TCommands;
    function CanCreateSession(const AUserId: string): Boolean;
    function GetSessionLimits: string;
    function GetSessionStatistics: string;
    }
    function GetSubscriptionReady: Boolean;
    function GetAgentId: string;
  public
    constructor Create(const AgentId: string);
    destructor Destroy; override;
  end;

var
  _Lock: TCriticalSection;
  _StoreAgentData: IStoreAgentData;

{ TStoreAgent }


constructor TStoreAgentData.Create(const AgentId: string);
begin
  inherited Create;
  FAgentId := AgentId;
  // Filters
  //FSessionFilters := TDictionary<string, TSessionFilter>.Create;
  //FCommandFilters := TDictionary<string, TCommandFilter>.Create;

  // Data
  //FSessions := TSessions.Create;
  //FCommands := TCommands.Create;

  //FFilteredSessions := TSessions.Create(False); // Reference
  //FFilteredCommands := TCommands.Create(False); // Reference

  // Search
  //FSessionsDict := TObjectDictionary<string, TSession>.Create;
  //FCommandsDict := TObjectDictionary<string, TCommand>.Create;

  FSessionLimits := TgrBsonDocument.Create;
end;

destructor TStoreAgentData.Destroy;
begin
  Writeln('[STORE_AGENT] Store Agent distrutto');

  inherited;
end;

function TStoreAgentData.GetAgentId: string;
begin
  Result := FAgentId;
end;

function TStoreAgentData.GetSubscriptionReady: Boolean;
begin
  Result := FSubscriptionReady;
end;

procedure TStoreAgentData.OnChanged(const ACollection, AId: string; const AFields: TgrBsonDocument);

begin
  if ACollection = SESSIONS_COLLECTION then
  begin

  end
  else if ACollection = COMMANDS_COLLECTION then
  begin

  end;
end;

procedure TStoreAgentData.OnChanged(const ACollection, AId: string; const ACleared: TgrBsonArray);

begin
  if ACollection = SESSIONS_COLLECTION then
  begin


  end
  else if ACollection = COMMANDS_COLLECTION then
  begin


  end;
end;

procedure TStoreAgentData.OnRemoved(const ACollection, AId: string);

begin
  if ACollection = SESSIONS_COLLECTION then
  begin

  end
  else if ACollection = COMMANDS_COLLECTION then
  begin

  end;
end;

procedure TStoreAgentData.EmitStoreChange;
begin
  if FSubscriptionReady then
    FDispatcher.DoDispatch(TStoreRMCDataChangedMessage.Create);
end;

// In RMC.Store.Agent.pas - modifica OnAction per aggiungere debug

procedure TStoreAgentData.OnAction(const ASender: TObject; const AAction: TgrMessage);
var
  LAction: TFluxAction absolute AAction;
  LActionType: string;
begin
  Assert(AAction is TFluxAction);

  LActionType := LAction.&Type;

  // *** AGGIUNGI DEBUG LOGGING ***
  Writeln(Format('[STORE_AGENT] Ricevuto evento: %s', [LActionType]));
  if LActionType = ACTION_DDP_CONNECTED then
  begin
    FSubscriptionReady := False;
     {FSessions.Clear;
    FSessionsDict.Clear;
    FCommands.Clear;
    FCommandsDict.Clear;}
    FLoggedInUserId := '';
    FAgentId := '';
  end
  else if LActionType = ACTION_AGENT_SUBSCRIBED then
  begin
    Writeln('[STORE_AGENT] AGENT_SUBSCRIBED ricevuto');
    FSubscriptionReady := True;
  end
  else if LActionType = ACTION_AGENT_SUBSCRIBING then
  begin
    FSubscriptionReady := False;
  end
  // *** INTERCETTA EVENTI DDP DOCUMENTI ***
  else if LActionType = ACTION_DDP_DOCUMENT_ADDED then
  begin
    Writeln('[STORE_AGENT] EVENTO DDP_DOCUMENT_ADDED ricevuto!');

    if LAction.Data[DDP_COLLECTION].IsString and
       LAction.Data[DDP_ID].IsString and
       LAction.Data[DDP_FIELDS].IsBsonDocument then
    begin
      Writeln('[STORE_AGENT] Dati validi, chiamando OnDocumentAdded...');
      //OnDocumentAdded(
      OnAdded(
        LAction.Data[DDP_COLLECTION].AsString,
        LAction.Data[DDP_ID].AsString,
        LAction.Data[DDP_FIELDS].AsBsonDocument
      );
    end
    else
    begin
      Writeln('[STORE_AGENT] Dati evento non validi');
      if not LAction.Data[DDP_COLLECTION].IsString then
        Writeln('[STORE_AGENT] - Collection non è string');
      if not LAction.Data[DDP_ID].IsString then
        Writeln('[STORE_AGENT] - ID non è string');
      if not LAction.Data[DDP_FIELDS].IsBsonDocument then
        Writeln('[STORE_AGENT] - Fields non è BsonDocument');
    end;
  end

  else if LActionType = ACTION_DDP_DOCUMENT_CHANGED then
  begin
    Writeln('[STORE_AGENT] DDP_DOCUMENT_CHANGED ricevuto');
    if LAction.Data[DDP_COLLECTION].IsString and
       LAction.Data[DDP_ID].IsString and
       LAction.Data[DDP_FIELDS].IsBsonDocument then
    begin
      OnChanged(
        LAction.Data[DDP_COLLECTION].AsString,
        LAction.Data[DDP_ID].AsString,
        LAction.Data[DDP_FIELDS].AsBsonDocument
      );
    end;
  end
  else if LActionType = ACTION_DDP_DOCUMENT_REMOVED then
  begin
    Writeln('[STORE_AGENT] DDP_DOCUMENT_REMOVED ricevuto');
    if LAction.Data[DDP_COLLECTION].IsString and
       LAction.Data[DDP_ID].IsString then
    begin
      OnRemoved(
        LAction.Data[DDP_COLLECTION].AsString,
        LAction.Data[DDP_ID].AsString
      );
    end;
  end
  else if LActionType = 'TEST_EVENT' then
  begin
    Writeln('[STORE_AGENT] TEST_EVENT ricevuto - Dispatcher funziona!');
  end
  else
  begin
    Writeln(Format('[STORE_AGENT] Evento non gestito: %s', [LActionType]));
  end;

  EmitStoreChange;
end;

procedure TStoreAgentData.OnAdded(const ACollection, AId: string; const AFields: TgrBsonDocument);
begin
  Writeln(Format('[STORE_AGENT] *** DOCUMENTO AGGIUNTO *** Collection: %s, ID: %s', [ACollection, AId]));
  Writeln(Format('[STORE_AGENT] Fields JSON: %s', [AFields.ToJson]));

  try
    // *** PROCESSA COMANDI ***
    if SameText(ACollection, COMMANDS_COLLECTION) then
    begin
      var AgentId := '';
      var SessionId := '';
      var Action := '';
      var Command := '';

      // Estrai i campi dal documento
      if AFields.Contains('agent_id') then
        AgentId := AFields['agent_id'].AsString;
      if AFields.Contains('session_id') then
        SessionId := AFields['session_id'].AsString;
      if AFields.Contains('action') then
        Action := AFields['action'].AsString;
      if AFields.Contains('command') then
        Command := AFields['command'].AsString;

      Writeln(Format('[STORE_AGENT] COMANDO: Agent=%s, Session=%s, Action=%s, Command=%s',
        [AgentId, SessionId, Action, Command]));


    end

    // *** PROCESSA RICHIESTE SESSIONE ***
    else if SameText(ACollection, SESSIONS_COLLECTION) then
    begin
      var AgentId := '';
      var SessionId := '';
      var ShellType := '';
      var Url := '';
      var Status := '';

      if AFields.Contains('agent_id') then
        AgentId := AFields['agent_id'].AsString;
      if AFields.Contains('session_id') then
        SessionId := AFields['session_id'].AsString;
      if AFields.Contains('shell_type') then
        ShellType := AFields['shell_type'].AsString;
      if AFields.Contains('url') then
        Url := AFields['url'].AsString;
      if AFields.Contains('status') then
        Status := AFields['status'].AsString;

      Writeln(Format('[STORE_AGENT] SESSIONE: Agent=%s, Session=%s, Shell=%s, Status=%s',
        [AgentId, SessionId, ShellType, Status]));
    end

    else
    begin
      Writeln(Format('[STORE_AGENT] Collezione non gestita: %s', [ACollection]));
    end;


  except
    on E: Exception do
    begin
      Writeln(Format('[STORE_AGENT] ERRORE processing documento: %s', [E.Message]));
    end;
  end;
end;

// Factory Functions

function GetStoreAgentData(const AAgentId: string): IStoreAgentData;
begin
  if not Assigned(_StoreAgentData) then
  begin
    _Lock.Enter;
    try
      if not Assigned(_StoreAgentData) then
      begin
        _StoreAgentData := TStoreAgentData.Create(AAgentId);
        Writeln('[STORE_AGENT] Store Agent singleton creato');
      end;
    finally
      _Lock.Leave;
    end;
  end;
  Result := _StoreAgentData;
end;

procedure ClearStoreAgent;
begin
  _StoreAgentData := nil;
end;

initialization
  _Lock := TCriticalSection.Create;

finalization
  _Lock.Free;

end.
