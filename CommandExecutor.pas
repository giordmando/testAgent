// CommandExecutor.pas - Versione corretta per l'interfaccia asincrona

unit CommandExecutor;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Threading,
  System.Generics.Collections,
  System.SyncObjs,
  RMC.Data,
  RMC.ActionCreator,
  AgentServiceI,
  System.DateUtils;

type
  ICommandExecutor = interface
    ['{44444444-5555-6666-7777-888888888888}']
    procedure ExecuteCommand(const ACommandId, ASessionId, ACommandLine: string;
      const APriority: Integer = 5; const ATimeout: Integer = 0);
    procedure CancelCommand(const ACommandId: string);
  end;

  // Classe per tenere traccia di un comando in esecuzione
  TRunningCommand = class
  private
    FCommandId: string;
    FSessionId: string;
    FCommandLine: string;
    FShellRunner: IShellRunner;
    FStartTime: TDateTime;
    FOutputBuffer: TStringList;
    FCompleted: Boolean;
    FExitCode: Integer;
  public
    constructor Create(const ACommandId, ASessionId, ACommandLine: string;
      const AShellRunner: IShellRunner);
    destructor Destroy; override;

    property CommandId: string read FCommandId;
    property SessionId: string read FSessionId;
    property CommandLine: string read FCommandLine;
    property ShellRunner: IShellRunner read FShellRunner;
    property StartTime: TDateTime read FStartTime;
    property OutputBuffer: TStringList read FOutputBuffer;
    property Completed: Boolean read FCompleted write FCompleted;
    property ExitCode: Integer read FExitCode write FExitCode;
  end;

  TCommandExecutor = class(TInterfacedObject, ICommandExecutor)
  private
    FAgentId: string;
    FAgentData: IAgentData;
    FActionCreator: IActionCreatorAgent;
    FShellRunnerFactory: IShellRunnerFactory;
    FRunningCommands: TDictionary<string, TRunningCommand>;
    FLock: TCriticalSection;

    // Callback per gestire l'output del comando
    procedure OnCommandOutput(const ACommandId: string; const ALine: string);
    procedure OnCommandError(const ACommandId: string; const ALine: string);
    procedure OnCommandExit(const ACommandId: string; const AExitCode: Integer);

    // Metodi di gestione
    procedure CompleteCommand(const ACommandId: string; const AExitCode: Integer);
    procedure RemoveCommand(const ACommandId: string);

  protected
    { ICommandExecutor implementation }
    procedure ExecuteCommand(const ACommandId, ASessionId, ACommandLine: string;
      const APriority: Integer = 5; const ATimeout: Integer = 0);
    procedure CancelCommand(const ACommandId: string);
  public
    constructor Create(const AAgentId: string; const AAgentData: IAgentData;
      const AShellRunnerFactory: IShellRunnerFactory);
    destructor Destroy; override;
  end;

function GetCommandExecutor(const AAgentId: string; const AAgentData: IAgentData;
  const AShellRunnerFactory: IShellRunnerFactory): ICommandExecutor;

implementation

{ TRunningCommand }

constructor TRunningCommand.Create(const ACommandId, ASessionId, ACommandLine: string;
  const AShellRunner: IShellRunner);
begin
  inherited Create;
  FCommandId := ACommandId;
  FSessionId := ASessionId;
  FCommandLine := ACommandLine;
  FShellRunner := AShellRunner;
  FStartTime := Now;
  FOutputBuffer := TStringList.Create;
  FCompleted := False;
  FExitCode := -1;
end;

destructor TRunningCommand.Destroy;
begin
  FOutputBuffer.Free;
  inherited;
end;

{ TCommandExecutor }

constructor TCommandExecutor.Create(const AAgentId: string; const AAgentData: IAgentData;
  const AShellRunnerFactory: IShellRunnerFactory);
begin
  inherited Create;
  FAgentId := AAgentId;
  FAgentData := AAgentData;
  FShellRunnerFactory := AShellRunnerFactory;
  FActionCreator := GetActionCreatorAgentWithId(AAgentId);
  FRunningCommands := TDictionary<string, TRunningCommand>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TCommandExecutor.Destroy;
var
  Command: TRunningCommand;
begin
  FLock.Enter;
  try
    // Ferma tutti i comandi in esecuzione
    for Command in FRunningCommands.Values do
    begin
      if Assigned(Command.ShellRunner) and Command.ShellRunner.IsRunning then
        Command.ShellRunner.StopExecution;
      Command.Free;
    end;
    FRunningCommands.Clear;
  finally
    FLock.Leave;
  end;

  FRunningCommands.Free;
  FLock.Free;
  inherited;
end;

procedure TCommandExecutor.ExecuteCommand(const ACommandId, ASessionId, ACommandLine: string;
  const APriority: Integer; const ATimeout: Integer);
var
  ShellRunner: IShellRunner;
  RunningCommand: TRunningCommand;
begin
  WriteLn(Format('[CommandExecutor] Executing command %s: %s', [ACommandId, ACommandLine]));

  FLock.Enter;
  try
    // Verifica se il comando è già in esecuzione
    if FRunningCommands.ContainsKey(ACommandId) then
    begin
      WriteLn(Format('[CommandExecutor] Command %s is already running', [ACommandId]));
      Exit;
    end;

    // Crea il shell runner
    try
      ShellRunner := FShellRunnerFactory.CreateRunner('cmd_persistent');
      WriteLn(Format('[CommandExecutor] Created shell runner for session: %s', [ASessionId]));
    except
      on E: Exception do
      begin
        WriteLn(Format('[CommandExecutor] Error creating shell runner: %s', [E.Message]));
        FAgentData.SendError(FAgentId, ASessionId, 'Failed to create shell runner: ' + E.Message);
        FActionCreator.OnCommandError(ACommandId, ASessionId, 'Failed to create shell runner: ' + E.Message);
        Exit;
      end;
    end;

    // Crea l'oggetto comando
    RunningCommand := TRunningCommand.Create(ACommandId, ASessionId, ACommandLine, ShellRunner);
    FRunningCommands.Add(ACommandId, RunningCommand);

  finally
    FLock.Leave;
  end;

  // Notifica che il comando sta per essere eseguito
  FAgentData.SendOutput(FAgentId, ASessionId, 'status',
    Format('Starting execution of command: %s', [ACommandLine]));
  FActionCreator.OnCommandExecuting(ACommandId, ASessionId);

  // Esegui il comando in modo asincrono usando i callback
  try
    ShellRunner.ExecuteCommand(
      ACommandLine,
      // OnOutput callback
      procedure(const Line: string)
      begin
        OnCommandOutput(ACommandId, Line);
      end,
      // OnError callback
      procedure(const Line: string)
      begin
        OnCommandError(ACommandId, Line);
      end,
      // OnExit callback
      procedure(const ExitCode: Integer)
      begin
        OnCommandExit(ACommandId, ExitCode);
      end
    );

    WriteLn(Format('[CommandExecutor] Command %s started successfully', [ACommandId]));

  except
    on E: Exception do
    begin
      WriteLn(Format('[CommandExecutor] Error starting command %s: %s', [ACommandId, E.Message]));

      // Rimuovi il comando dalla lista e invia errore
      RemoveCommand(ACommandId);
      FAgentData.SendError(FAgentId, ASessionId, 'Error starting command: ' + E.Message);
      FActionCreator.OnCommandError(ACommandId, ASessionId, 'Error starting command: ' + E.Message);
    end;
  end;
end;

procedure TCommandExecutor.OnCommandOutput(const ACommandId: string; const ALine: string);
var
  RunningCommand: TRunningCommand;
begin
  FLock.Enter;
  try
    if not FRunningCommands.TryGetValue(ACommandId, RunningCommand) then
      Exit;
  finally
    FLock.Leave;
  end;

  WriteLn(Format('[CommandExecutor] Output from %s: %s', [ACommandId, ALine]));

  // Aggiungi alla buffer di output
  RunningCommand.OutputBuffer.Add(ALine);

  // Invia l'output al server
  FAgentData.SendOutput(FAgentId, RunningCommand.SessionId, 'stdout', ALine);
end;

procedure TCommandExecutor.OnCommandError(const ACommandId: string; const ALine: string);
var
  RunningCommand: TRunningCommand;
begin
  FLock.Enter;
  try
    if not FRunningCommands.TryGetValue(ACommandId, RunningCommand) then
      Exit;
  finally
    FLock.Leave;
  end;

  WriteLn(Format('[CommandExecutor] Error from %s: %s', [ACommandId, ALine]));

  // Aggiungi alla buffer di output
  RunningCommand.OutputBuffer.Add('ERROR: ' + ALine);

  // Invia l'errore al server
  FAgentData.SendOutput(FAgentId, RunningCommand.SessionId, 'stderr', ALine);
end;

procedure TCommandExecutor.OnCommandExit(const ACommandId: string; const AExitCode: Integer);
begin
  WriteLn(Format('[CommandExecutor] Command %s completed with exit code: %d', [ACommandId, AExitCode]));
  CompleteCommand(ACommandId, AExitCode);
end;

procedure TCommandExecutor.CompleteCommand(const ACommandId: string; const AExitCode: Integer);
var
  RunningCommand: TRunningCommand;
  Duration: Integer;
begin
  FLock.Enter;
  try
    if not FRunningCommands.TryGetValue(ACommandId, RunningCommand) then
      Exit;

    RunningCommand.Completed := True;
    RunningCommand.ExitCode := AExitCode;
  finally
    FLock.Leave;
  end;

  Duration := MilliSecondsBetween(Now, RunningCommand.StartTime);

  WriteLn(Format('[CommandExecutor] Command %s completed in %d ms with exit code %d',
    [ACommandId, Duration, AExitCode]));

  // Invia la notifica di completamento
  FAgentData.SendCommandComplete(FAgentId, RunningCommand.SessionId, AExitCode);
  FActionCreator.OnCommandCompleted(ACommandId, RunningCommand.SessionId, AExitCode);

  // Invia un riepilogo dell'output se necessario
  if RunningCommand.OutputBuffer.Count > 0 then
  begin
    var Summary := Format('Command completed. Total output lines: %d', [RunningCommand.OutputBuffer.Count]);
    FAgentData.SendOutput(FAgentId, RunningCommand.SessionId, 'status', Summary);
  end;

  // Rimuovi il comando dalla lista
  RemoveCommand(ACommandId);
end;

procedure TCommandExecutor.CancelCommand(const ACommandId: string);
var
  RunningCommand: TRunningCommand;
begin
  FLock.Enter;
  try
    if not FRunningCommands.TryGetValue(ACommandId, RunningCommand) then
    begin
      WriteLn(Format('[CommandExecutor] Command not found for cancellation: %s', [ACommandId]));
      Exit;
    end;
  finally
    FLock.Leave;
  end;

  WriteLn(Format('[CommandExecutor] Cancelling command: %s', [ACommandId]));

  try
    // Ferma l'esecuzione del comando
    if Assigned(RunningCommand.ShellRunner) and RunningCommand.ShellRunner.IsRunning then
      RunningCommand.ShellRunner.StopExecution;

    // Notifica la cancellazione
    FAgentData.SendOutput(FAgentId, RunningCommand.SessionId, 'status',
      Format('Command %s was cancelled', [ACommandId]));
    FActionCreator.OnCommandError(ACommandId, RunningCommand.SessionId, 'Command cancelled by user');

  except
    on E: Exception do
      WriteLn(Format('[CommandExecutor] Error cancelling command %s: %s', [ACommandId, E.Message]));
  end;

  // Rimuovi il comando dalla lista
  RemoveCommand(ACommandId);
end;

procedure TCommandExecutor.RemoveCommand(const ACommandId: string);
var
  RunningCommand: TRunningCommand;
begin
  FLock.Enter;
  try
    if FRunningCommands.TryGetValue(ACommandId, RunningCommand) then
    begin
      FRunningCommands.Remove(ACommandId);
      RunningCommand.Free;
      WriteLn(Format('[CommandExecutor] Removed command from tracking: %s', [ACommandId]));
    end;
  finally
    FLock.Leave;
  end;
end;

// Factory function
function GetCommandExecutor(const AAgentId: string; const AAgentData: IAgentData;
  const AShellRunnerFactory: IShellRunnerFactory): ICommandExecutor;
begin
  Result := TCommandExecutor.Create(AAgentId, AAgentData, AShellRunnerFactory);
end;

end.
