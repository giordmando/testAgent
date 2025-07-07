// Aggiungi queste interfacce a RMC.ActionCreator.pas

unit RMC.ActionCreator;

interface

uses
  Grijjy.Data.Bson,
  System.SysUtils;

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
  end;

// Factory per ottenere l'ActionCreator per un agent specifico
function GetActionCreatorAgentWithId(const AAgentId: string): IActionCreatorAgent;

implementation

uses
  RMC.Actions.Consts,
  Flux.ActionCreator.Base,
  Flux.Actions,
  System.Generics.Collections;

type
  TActionCreatorAgent = class(TActionCreatorBase, IActionCreatorAgent)
  private
    FAgentId: string;
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
  FDispatcher.DoDispatch(TFluxAction.Create(ACTION_SESSION_OPENING,
    TgrBsonDocument.Create
      .Add('agent_id', FAgentId)
      .Add('session_id', ASessionId)
      .Add('user_id', AUserId)
      .Add('shell_type', AShellType)
      .Add('working_dir', AWorkingDir)));
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
