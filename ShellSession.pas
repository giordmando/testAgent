unit ShellSession;

interface

uses
  Classes, SysUtils, System.Threading, System.SyncObjs, System.DateUtils,
  System.Generics.Collections, AgentServiceI, System.JSON, Vcl.ExtCtrls;

type
  TCommandInfo = record
    Command: string;
    StartTime: TDateTime;
    EndTime: TDateTime;
    ExitCode: Integer;
    OutputBytes: Integer;
    ErrorBytes: Integer;
  end;

  TShellSession = class(TInterfacedObject, IShellSession)
  private
    FSessionId: string;
    FShellType: string;
    FShellRunner: IShellRunner;
    FOnOutput: TOutputProc;
    FOnError: TOutputProc;
    FOnExit: TExitCodeProc;

    // Stato
    FCreatedAt: TDateTime;
    FLastActivity: TDateTime;
    FIsActive: Boolean;
    FCurrentCommand: string;
    FCommandHistory: TList<TCommandInfo>;
    FCurrentCommandInfo: TCommandInfo;

    // FIX: Add separate flag for command execution state
    FCommandExecuting: Boolean;  // <-- NEW FLAG

    // Thread safety
    FLock: TCriticalSection;

    // Buffer per output
    FOutputBuffer: TStringList;
    FErrorBuffer: TStringList;
    FMaxBufferSize: Integer;
    FBufferFlushInterval: Integer;

    // Metodi privati
    procedure HandleRunnerOutput(const Line: string);
    procedure HandleRunnerError(const Line: string);
    procedure HandleRunnerExit(const ExitCode: Integer);
    procedure FlushBuffers;

    function GetOnOutput: TOutputProc;
    procedure SetOnOutput(const Value: TOutputProc);
    function GetOnError: TOutputProc;
    procedure SetOnError(const Value: TOutputProc);
    function GetOnExit: TExitCodeProc;
    procedure SetOnExit(const Value: TExitCodeProc);
    function GetSessionId: string;
    function GetShellType: string;
    function GetCreatedAt: TDateTime;
    function GetLastActivity: TDateTime;
    function IsActive: Boolean;
  public
    constructor Create(const SessionId, ShellType: string;
      const ShellFactory: IShellRunnerFactory;
      MaxBufferSize: Integer = 100;
      BufferFlushIntervalMs: Integer = 500);
    destructor Destroy; override;

    // IShellSession
    procedure ExecuteCommand(const Command: string);
    procedure StopSession;

    // Proprietà
    property SessionId: string read GetSessionId;
    property ShellType: string read GetShellType;
    property CreatedAt: TDateTime read GetCreatedAt;
    property LastActivity: TDateTime read GetLastActivity;
    property OnOutput: TOutputProc read GetOnOutput write SetOnOutput;
    property OnError: TOutputProc read GetOnError write SetOnError;
    property OnExit: TExitCodeProc read GetOnExit write SetOnExit;

    // Metodi aggiuntivi
    function GetCommandHistory: TArray<TCommandInfo>;
    function GetCurrentCommand: string;
    function GetSessionStats: TJSONObject;
    procedure ClearBuffers;
  end;

implementation

uses
  Winapi.Windows;

{ TShellSession }

constructor TShellSession.Create(const SessionId, ShellType: string;
  const ShellFactory: IShellRunnerFactory; MaxBufferSize: Integer;
  BufferFlushIntervalMs: Integer);
begin
  inherited Create;

  if SessionId.Trim.IsEmpty then
    raise Exception.Create('SessionId non può essere vuoto');
  if not Assigned(ShellFactory) then
    raise Exception.Create('ShellFactory non assegnata');

  FSessionId := SessionId;
  FShellType := ShellType;
  FCreatedAt := Now;
  FLastActivity := Now;
  FIsActive := True;
  FCurrentCommand := '';
  FCommandExecuting := False;  // <-- INITIALIZE NEW FLAG

  // Crea shell runner
  FShellRunner := ShellFactory.CreateRunner(ShellType);
  if not Assigned(FShellRunner) then
    raise Exception.CreateFmt('Runner non disponibile per shell type: %s', [ShellType]);

  // Inizializza strutture
  FLock := TCriticalSection.Create;
  FCommandHistory := TList<TCommandInfo>.Create;
  FOutputBuffer := TStringList.Create;
  FErrorBuffer := TStringList.Create;

  // Configura buffer
  FMaxBufferSize := MaxBufferSize;
  FBufferFlushInterval := BufferFlushIntervalMs;

  // Callbacks iniziali
  FOnOutput := nil;
  FOnError := nil;
  FOnExit := nil;
end;

destructor TShellSession.Destroy;
begin
  try
    StopSession;
  except
    // Ignora errori in distruzione
  end;

  FlushBuffers;

  FOutputBuffer.Free;
  FErrorBuffer.Free;
  FCommandHistory.Free;
  FLock.Free;
  FShellRunner := nil;

  inherited;
end;

procedure TShellSession.ExecuteCommand(const Command: string);
begin
  if Command.Trim.IsEmpty then
    raise Exception.Create('Comando vuoto');

  FLock.Enter;
  try
    if not FIsActive then
      raise Exception.Create('Sessione non attiva');

    // FIX: Check our internal command execution flag instead of shell runner state
    if FCommandExecuting then
      raise Exception.Create('Comando già in esecuzione');

    FLastActivity := Now;
    FCurrentCommand := Command;
    FCommandExecuting := True;  // <-- SET FLAG WHEN STARTING COMMAND

    // Inizializza info comando
    FCurrentCommandInfo.Command := Command;
    FCurrentCommandInfo.StartTime := Now;
    FCurrentCommandInfo.EndTime := 0;
    FCurrentCommandInfo.ExitCode := -1;
    FCurrentCommandInfo.OutputBytes := 0;
    FCurrentCommandInfo.ErrorBytes := 0;
  finally
    FLock.Leave;
  end;

  // Esegui comando
  FShellRunner.ExecuteCommand(
    Command,
    HandleRunnerOutput,
    HandleRunnerError,
    HandleRunnerExit
  );
end;

procedure TShellSession.StopSession;
begin
  FLock.Enter;
  try
    if not FIsActive then
      Exit;

    FIsActive := False;
    FCommandExecuting := False;  // <-- CLEAR FLAG WHEN STOPPING

    // Ferma runner se in esecuzione
    if Assigned(FShellRunner) and FShellRunner.IsRunning then
      FShellRunner.StopExecution;

    // Flush finale
    FlushBuffers;
  finally
    FLock.Leave;
  end;
end;

procedure TShellSession.HandleRunnerOutput(const Line: string);
begin
  FLock.Enter;
  try
    FLastActivity := Now;
    FCurrentCommandInfo.OutputBytes := FCurrentCommandInfo.OutputBytes + Length(Line);

    // Aggiungi a buffer
    FOutputBuffer.Add(Line);

    // Flush immediato o basato su dimensione
    if FOutputBuffer.Count >= FMaxBufferSize then
      FlushBuffers;
  finally
    FLock.Leave;
  end;
  // Per garantire che l'output arrivi, potremmo fare sempre il flush
  FlushBuffers; // Valuta se metterlo qui o basarti solo sulla dimensione.
end;

procedure TShellSession.HandleRunnerError(const Line: string);
begin
  FLock.Enter;
  try
    FLastActivity := Now;
    FCurrentCommandInfo.ErrorBytes := FCurrentCommandInfo.ErrorBytes + Length(Line);

    // Aggiungi a buffer
    FErrorBuffer.Add(Line);

    // Flush se buffer pieno
    if FErrorBuffer.Count >= FMaxBufferSize then
      FlushBuffers;
  finally
    FLock.Leave;
  end;
end;

procedure TShellSession.HandleRunnerExit(const ExitCode: Integer);
begin
  FLock.Enter;
  try
    FLastActivity := Now;
    FCurrentCommandInfo.EndTime := Now;
    FCurrentCommandInfo.ExitCode := ExitCode;

    // Aggiungi a storia
    FCommandHistory.Add(FCurrentCommandInfo);

    // Reset comando corrente
    FCurrentCommand := '';
    FCommandExecuting := False;  // <-- CLEAR FLAG WHEN COMMAND COMPLETES

    // Flush finale
    FlushBuffers;
  finally
    FLock.Leave;
  end;

  // Notifica exit (fuori dal lock)
  if Assigned(FOnExit) then
  begin
    try
      FOnExit(ExitCode);
    except
      // Ignora eccezioni nel callback
    end;
  end;
end;

procedure TShellSession.FlushBuffers;
var
  OutputLines: TStringList;
  ErrorLines: TStringList;
  Line: string;
begin
  // Chiamato con lock già acquisito o in contesto thread-safe

  // Copia buffer locali
  OutputLines := TStringList.Create;
  ErrorLines := TStringList.Create;
  try
    OutputLines.Assign(FOutputBuffer);
    ErrorLines.Assign(FErrorBuffer);

    // Pulisci buffer originali
    FOutputBuffer.Clear;
    FErrorBuffer.Clear;

    // Invia output (fuori dal lock principale)
    if Assigned(FOnOutput) then
    begin
      for Line in OutputLines do
      begin
        try
          FOnOutput(Line);
        except
          // Ignora eccezioni nel callback
        end;
      end;
    end;

    // Invia errori
    if Assigned(FOnError) then
    begin
      for Line in ErrorLines do
      begin
        try
          FOnError(Line);
        except
          // Ignora eccezioni nel callback
        end;
      end;
    end;
  finally
    OutputLines.Free;
    ErrorLines.Free;
  end;
end;

procedure TShellSession.ClearBuffers;
begin
  FLock.Enter;
  try
    FOutputBuffer.Clear;
    FErrorBuffer.Clear;
  finally
    FLock.Leave;
  end;
end;

function TShellSession.GetCommandHistory: TArray<TCommandInfo>;
begin
  FLock.Enter;
  try
    Result := FCommandHistory.ToArray;
  finally
    FLock.Leave;
  end;
end;

function TShellSession.GetCurrentCommand: string;
begin
  FLock.Enter;
  try
    Result := FCurrentCommand;
  finally
    FLock.Leave;
  end;
end;

function TShellSession.GetSessionStats: TJSONObject;
var
  TotalCommands, TotalOutput, TotalError: Integer;
  TotalTime: Double;
  Cmd: TCommandInfo;
  HistArray: TJSONArray;
  CmdObj: TJSONObject;
begin
  Result := TJSONObject.Create;

  FLock.Enter;
  try
    // Statistiche base
    Result.AddPair('sessionId', FSessionId);
    Result.AddPair('shellType', FShellType);
    Result.AddPair('createdAt', DateToISO8601(FCreatedAt));
    Result.AddPair('lastActivity', DateToISO8601(FLastActivity));
    Result.AddPair('isActive', TJSONBool.Create(FIsActive));
    Result.AddPair('currentCommand', FCurrentCommand);
    Result.AddPair('commandExecuting', TJSONBool.Create(FCommandExecuting));  // <-- ADD TO STATS

    // Calcola totali
    TotalCommands := FCommandHistory.Count;
    TotalOutput := 0;
    TotalError := 0;
    TotalTime := 0;

    HistArray := TJSONArray.Create;

    for Cmd in FCommandHistory do
    begin
      TotalOutput := TotalOutput + Cmd.OutputBytes;
      TotalError := TotalError + Cmd.ErrorBytes;
      if Cmd.EndTime > 0 then
        TotalTime := TotalTime + (Cmd.EndTime - Cmd.StartTime) * 24 * 60 * 60; // secondi

      // Aggiungi a history
      CmdObj := TJSONObject.Create;
      CmdObj.AddPair('command', Cmd.Command);
      CmdObj.AddPair('startTime', DateToISO8601(Cmd.StartTime));
      if Cmd.EndTime > 0 then
        CmdObj.AddPair('endTime', DateToISO8601(Cmd.EndTime));
      CmdObj.AddPair('exitCode', TJSONNumber.Create(Cmd.ExitCode));
      CmdObj.AddPair('outputBytes', TJSONNumber.Create(Cmd.OutputBytes));
      CmdObj.AddPair('errorBytes', TJSONNumber.Create(Cmd.ErrorBytes));
      HistArray.Add(CmdObj);
    end;

    // Aggiungi statistiche
    Result.AddPair('totalCommands', TJSONNumber.Create(TotalCommands));
    Result.AddPair('totalOutputBytes', TJSONNumber.Create(TotalOutput));
    Result.AddPair('totalErrorBytes', TJSONNumber.Create(TotalError));
    Result.AddPair('totalExecutionTime', TJSONNumber.Create(TotalTime));
    Result.AddPair('commandHistory', HistArray);

    // Info buffer
    Result.AddPair('outputBufferSize', TJSONNumber.Create(FOutputBuffer.Count));
    Result.AddPair('errorBufferSize', TJSONNumber.Create(FErrorBuffer.Count));
  finally
    FLock.Leave;
  end;
end;

// Getter/Setter implementations

function TShellSession.GetOnOutput: TOutputProc;
begin
  Result := FOnOutput;
end;

procedure TShellSession.SetOnOutput(const Value: TOutputProc);
begin
  FOnOutput := Value;
end;

function TShellSession.GetOnError: TOutputProc;
begin
  Result := FOnError;
end;

procedure TShellSession.SetOnError(const Value: TOutputProc);
begin
  FOnError := Value;
end;

function TShellSession.GetOnExit: TExitCodeProc;
begin
  Result := FOnExit;
end;

procedure TShellSession.SetOnExit(const Value: TExitCodeProc);
begin
  FOnExit := Value;
end;

function TShellSession.GetSessionId: string;
begin
  Result := FSessionId;
end;

function TShellSession.GetShellType: string;
begin
  Result := FShellType;
end;

function TShellSession.GetCreatedAt: TDateTime;
begin
  Result := FCreatedAt;
end;

function TShellSession.GetLastActivity: TDateTime;
begin
  FLock.Enter;
  try
    Result := FLastActivity;
  finally
    FLock.Leave;
  end;
end;

function TShellSession.IsActive: Boolean;
begin
  FLock.Enter;
  try
    Result := FIsActive;
  finally
    FLock.Leave;
  end;
end;

end.
