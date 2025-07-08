// Aggiornamento di RMC.Store.Agent.pas per includere il CommandExecutor

unit RMC.Store.Agent;

interface

uses
  DDP.Actions.Consts,
  RMC.Actions.Consts,
  RMC.Store.Events,
  Flux.Actions,
  Flux.Store.Base,
  Grijjy.Data.Bson,
  Grijjy.System.Messaging,
  System.SysUtils,
  System.JSON,
  CommandExecutor,  // AGGIUNTO
  RMC.Data,         // AGGIUNTO
  AgentServiceI;    // AGGIUNTO

type
  IStoreAgent = interface
    ['{22222222-3333-4444-5555-666666666666}']
    // Metodi per accedere ai dati dell'agent
    function GetLastCommand: TJSONObject;
    function GetLastSession: TJSONObject;
    function GetAgentStatus: string;

    // AGGIUNTO: Metodi per inizializzare i servizi
    procedure SetServices(const AAgentData: IAgentData; const AShellRunnerFactory: IShellRunnerFactory);
  end;

  TStoreAgent = class(TStoreBase, IStoreAgent)
  private
    FLastCommand: TJSONObject;
    FLastSession: TJSONObject;
    FAgentStatus: string;
    FAgentId: string;

    // AGGIUNTO: Servizi per l'esecuzione dei comandi
    FAgentData: IAgentData;
    FCommandExecutor: ICommandExecutor;

    // Gestori per i diversi tipi di collection
    procedure HandleCommandAdded(const ADoc: TgrBsonDocument);
    procedure HandleCommandChanged(const ADoc: TgrBsonDocument);
    procedure HandleCommandRemoved(const ADoc: TgrBsonDocument);

    procedure HandleSessionAdded(const ADoc: TgrBsonDocument);
    procedure HandleSessionChanged(const ADoc: TgrBsonDocument);
    procedure HandleSessionRemoved(const ADoc: TgrBsonDocument);

    procedure HandleAgentControlAdded(const ADoc: TgrBsonDocument);

  protected
    procedure EmitStoreChange; override;

    { IStoreAgent implementation }
    function GetLastCommand: TJSONObject;
    function GetLastSession: TJSONObject;
    function GetAgentStatus: string;
    procedure SetServices(const AAgentData: IAgentData; const AShellRunnerFactory: IShellRunnerFactory);

  public
    constructor Create(const AAgentId: string);
    destructor Destroy; override;
    procedure OnAction(const ASender: TObject; const AAction: TgrMessage); override;
  end;

function GetStoreAgent(const AAgentId: string): IStoreAgent;

implementation

uses
  System.SyncObjs;

var
  _Lock: TCriticalSection;
  _StoreAgent: IStoreAgent;

{ TStoreAgent }

constructor TStoreAgent.Create(const AAgentId: string);
begin
  inherited Create;
  FAgentId := AAgentId;
  FAgentStatus := 'initializing';
end;

destructor TStoreAgent.Destroy;
begin
  if Assigned(FLastCommand) then
    FLastCommand.Free;
  if Assigned(FLastSession) then
    FLastSession.Free;
  inherited;
end;

procedure TStoreAgent.SetServices(const AAgentData: IAgentData; const AShellRunnerFactory: IShellRunnerFactory);
begin
  WriteLn('[StoreAgent] Setting up services...');
  FAgentData := AAgentData;
  FCommandExecutor := GetCommandExecutor(FAgentId, AAgentData, AShellRunnerFactory);
  WriteLn('[StoreAgent] Services configured successfully');
end;

procedure TStoreAgent.EmitStoreChange;
begin
  FDispatcher.DoDispatch(TStoreRMCDataChangedMessage.Create);
end;

procedure TStoreAgent.OnAction(const ASender: TObject; const AAction: TgrMessage);
var
  LAction: TFluxAction absolute AAction;
  LActionType: string;
  LDoc: TgrBsonDocument;
  LCollection: string;
begin

  WriteLn('[DEBUG TStoreAgent] OnAction chiamato');
  WriteLn('[DEBUG TStoreAgent] Dispatcher ID: ' + IntToHex(Integer(FDispatcher), 8));


  Assert(AAction is TFluxAction);

  LActionType := LAction.&Type;
  WriteLn('[DEBUG TStoreAgent] Action type: ' + LActionType);
  // Gestisci solo le azioni DDP per le collection
  if (LActionType = ACTION_DDP_DOCUMENT_ADDED) or
     (LActionType = ACTION_DDP_DOCUMENT_CHANGED) or
     (LActionType = ACTION_DDP_DOCUMENT_REMOVED) then
  begin
    if not LAction.Data.IsNil then
    begin
      LDoc := LAction.Data;

      // Estrai il nome della collection dal documento DDP
      if LDoc.Contains('collection') then
      begin
        LCollection := LDoc['collection'].AsString;

        WriteLn(Format('[StoreAgent] DDP Action: %s for collection: %s', [LActionType, LCollection]));

        // Gestisci in base alla collection e al tipo di azione
        if LCollection = 'commands' then
        begin
          if LActionType = ACTION_DDP_DOCUMENT_ADDED then
            HandleCommandAdded(LDoc)
          else if LActionType = ACTION_DDP_DOCUMENT_CHANGED then
            HandleCommandChanged(LDoc)
          else if LActionType = ACTION_DDP_DOCUMENT_REMOVED then
            HandleCommandRemoved(LDoc);
        end
        else if LCollection = 'sessions' then
        begin
          if LActionType = ACTION_DDP_DOCUMENT_ADDED then
            HandleSessionAdded(LDoc)
          else if LActionType = ACTION_DDP_DOCUMENT_CHANGED then
            HandleSessionChanged(LDoc)
          else if LActionType = ACTION_DDP_DOCUMENT_REMOVED then
            HandleSessionRemoved(LDoc);
        end
        else if LCollection = 'agent_control' then
        begin
          if LActionType = ACTION_DDP_DOCUMENT_ADDED then
            HandleAgentControlAdded(LDoc);
        end;
      end;
    end;
  end
  else if LActionType = ACTION_DDP_CONNECTED then
  begin
    FAgentStatus := 'connected';
    EmitStoreChange;
  end
  else if LActionType = ACTION_DDP_DISCONNECTED then
  begin
    FAgentStatus := 'disconnected';
    EmitStoreChange;
  end;
end;

// Gestori per i comandi - AGGIORNATI
procedure TStoreAgent.HandleCommandAdded(const ADoc: TgrBsonDocument);
var
  CommandId, SessionId, CommandLine: string;
  AgentId: string;
  Priority, Timeout: Integer;
begin
  WriteLn('[StoreAgent] *** NEW COMMAND RECEIVED ***');

  // Estrai i dati del comando dal documento DDP
  if ADoc.Contains('id') then
    CommandId := ADoc['id'].AsString;

  if ADoc.Contains('fields') then
  begin
    var Fields := ADoc['fields'].AsBsonDocument;

    if Fields.Contains('agent_id') then
      AgentId := Fields['agent_id'].AsString;

    // Verifica che sia per questo agent
    if AgentId <> FAgentId then
    begin
      WriteLn(Format('[StoreAgent] Command ignored - wrong agent_id: %s (expected: %s)', [AgentId, FAgentId]));
      Exit;
    end;

    if Fields.Contains('session_id') then
      SessionId := Fields['session_id'].AsString;

    if Fields.Contains('commandLine') then
      CommandLine := Fields['commandLine'].AsString;

    if Fields.Contains('priority') then
      Priority := Fields['priority'].AsInteger
    else
      Priority := 5;

    if Fields.Contains('timeout') then
      Timeout := Fields['timeout'].AsInteger
    else
      Timeout := 30000;

    WriteLn('[StoreAgent] Command for this agent:');
    WriteLn(Format('[StoreAgent]   Command ID: %s', [CommandId]));
    WriteLn(Format('[StoreAgent]   Session ID: %s', [SessionId]));
    WriteLn(Format('[StoreAgent]   Command: %s', [CommandLine]));
    WriteLn(Format('[StoreAgent]   Priority: %d, Timeout: %d', [Priority, Timeout]));

    // Salva l'ultimo comando
    if Assigned(FLastCommand) then
      FLastCommand.Free;

    FLastCommand := TJSONObject.Create;
    FLastCommand.AddPair('id', CommandId);
    FLastCommand.AddPair('session_id', SessionId);
    FLastCommand.AddPair('command_line', CommandLine);
    FLastCommand.AddPair('priority', TJSONNumber.Create(Priority));
    FLastCommand.AddPair('timeout', TJSONNumber.Create(Timeout));

    // ESEGUI IL COMANDO se il CommandExecutor è disponibile
    if Assigned(FCommandExecutor) then
    begin
      WriteLn('[StoreAgent] Executing command...');
      FCommandExecutor.ExecuteCommand(CommandId, SessionId, CommandLine, Priority, Timeout);
    end
    else
    begin
      WriteLn('[StoreAgent] CommandExecutor not available - command not executed');
    end;

    EmitStoreChange;
  end;
end;

procedure TStoreAgent.HandleCommandChanged(const ADoc: TgrBsonDocument);
begin
  WriteLn('[StoreAgent] Command changed');
  // Gestisci l'aggiornamento del comando se necessario
  EmitStoreChange;
end;

procedure TStoreAgent.HandleCommandRemoved(const ADoc: TgrBsonDocument);
begin
  WriteLn('[StoreAgent] Command removed');
  EmitStoreChange;
end;

// Gestori per le sessioni
procedure TStoreAgent.HandleSessionAdded(const ADoc: TgrBsonDocument);
var
  SessionId, UserId, ShellType: string;
  AgentId: string;
begin
  WriteLn('[StoreAgent] *** NEW SESSION RECEIVED ***');

  if ADoc.Contains('id') then
    SessionId := ADoc['id'].AsString;

  if ADoc.Contains('fields') then
  begin
    var Fields := ADoc['fields'].AsBsonDocument;

    if Fields.Contains('agent_id') then
      AgentId := Fields['agent_id'].AsString;

    // Verifica che sia per questo agent
    if AgentId <> FAgentId then
    begin
      WriteLn(Format('[StoreAgent] Session ignored - wrong agent_id: %s (expected: %s)', [AgentId, FAgentId]));
      Exit;
    end;

    if Fields.Contains('client_id') then
      UserId := Fields['client_id'].AsString;

    if Fields.Contains('shell_type') then
      ShellType := Fields['shell_type'].AsString;

    WriteLn('[StoreAgent] Session for this agent:');
    WriteLn(Format('[StoreAgent]   Session ID: %s', [SessionId]));
    WriteLn(Format('[StoreAgent]   User ID: %s', [UserId]));
    WriteLn(Format('[StoreAgent]   Shell Type: %s', [ShellType]));

    // Salva l'ultima sessione
    if Assigned(FLastSession) then
      FLastSession.Free;

    FLastSession := TJSONObject.Create;
    FLastSession.AddPair('id', SessionId);
    FLastSession.AddPair('user_id', UserId);
    FLastSession.AddPair('shell_type', ShellType);

    EmitStoreChange;
  end;
end;

procedure TStoreAgent.HandleSessionChanged(const ADoc: TgrBsonDocument);
begin
  WriteLn('[StoreAgent] Session changed');
  EmitStoreChange;
end;

procedure TStoreAgent.HandleSessionRemoved(const ADoc: TgrBsonDocument);
begin
  WriteLn('[StoreAgent] Session removed');
  EmitStoreChange;
end;

// Gestori per il controllo agent
procedure TStoreAgent.HandleAgentControlAdded(const ADoc: TgrBsonDocument);
var
  Action: string;

begin
  WriteLn('[StoreAgent] *** AGENT CONTROL RECEIVED ***');

  if ADoc.Contains('fields') then
  begin
    var Fields := ADoc['fields'].AsBsonDocument;

    if Fields.Contains('action') then
    begin
      Action := Fields['action'].AsString;

      WriteLn(Format('[StoreAgent] Agent control action: %s', [Action]));

      EmitStoreChange;
    end;
  end;
end;

// Metodi di accesso ai dati
function TStoreAgent.GetLastCommand: TJSONObject;
begin
  Result := FLastCommand;
end;

function TStoreAgent.GetLastSession: TJSONObject;
begin
  Result := FLastSession;
end;

function TStoreAgent.GetAgentStatus: string;
begin
  Result := FAgentStatus;
end;

// Factory function
function GetStoreAgent(const AAgentId: string): IStoreAgent;
begin
  if not Assigned(_StoreAgent) then
  begin
    _Lock.Enter;
    try
      if not Assigned(_StoreAgent) then
      begin
        _StoreAgent := TStoreAgent.Create(AAgentId);
      end;
    finally
      _Lock.Leave;
    end;
  end;
  Result := _StoreAgent;
end;

initialization
  _Lock := TCriticalSection.Create;

finalization
  _Lock.Free;

end.
