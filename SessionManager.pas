unit SessionManager;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.SyncObjs, System.DateUtils, AgentServiceI, ShellSession, System.JSON;

type
  TSessionInfo = record
    Session: IShellSession;
    CreatedAt: TDateTime;
    LastActivity: TDateTime;
    CommandCount: Integer;
    BytesOutput: Int64;
    State: string;
  end;

  TSessionManager = class(TInterfacedObject, ISessionManager)
  private
    FSessions: TDictionary<string, TSessionInfo>;
    FShellFactory: IShellRunnerFactory;
    FLock: TCriticalSection;
    FOnSessionCreated: TSessionEvent;
    FOnSessionClosed: TSessionEvent;
    FMaxSessions: Integer;
    FSessionTimeout: Integer; // minuti

    function GetOnSessionCreated: TSessionEvent;
    procedure SetOnSessionCreated(const Value: TSessionEvent);
    function GetOnSessionClosed: TSessionEvent;
    procedure SetOnSessionClosed(const Value: TSessionEvent);

    procedure CleanupInactiveSessions;
    function GenerateSessionId: string;
  public
    constructor Create(const ShellFactory: IShellRunnerFactory;
      MaxSessions: Integer = 10; SessionTimeoutMinutes: Integer = 30);
    destructor Destroy; override;

    // ISessionManager
    function CreateSession(const ShellType: string; const SessionId: string = ''): IShellSession;
    function GetSession(const SessionId: string): IShellSession;
    function ListSessions: TArray<string>;
    procedure CloseSession(const SessionId: string);
    procedure CloseAllSessions;
    function CheckLimitSession(const LimitSession: Integer): Boolean;
    function GetActiveSessionCount: Integer;

    // Metodi aggiuntivi
    function GetSessionInfo(const SessionId: string): TSessionInfo;
    procedure UpdateSessionActivity(const SessionId: string);
    procedure UpdateSessionStats(const SessionId: string; BytesOutput: Integer);
    function GetSessionStats: TJSONObject;

    property OnSessionCreated: TSessionEvent read GetOnSessionCreated write SetOnSessionCreated;
    property OnSessionClosed: TSessionEvent read GetOnSessionClosed write SetOnSessionClosed;
  end;

implementation


{ TSessionManager }

constructor TSessionManager.Create(const ShellFactory: IShellRunnerFactory;
  MaxSessions: Integer; SessionTimeoutMinutes: Integer);
begin
  inherited Create;

  if not Assigned(ShellFactory) then
    raise Exception.Create('TSessionManager.Create: ShellFactory non assegnata');

  FShellFactory := ShellFactory;
  FSessions := TDictionary<string, TSessionInfo>.Create;
  FLock := TCriticalSection.Create;
  FMaxSessions := MaxSessions;
  FSessionTimeout := SessionTimeoutMinutes;
  FOnSessionCreated := nil;
  FOnSessionClosed := nil;
end;

destructor TSessionManager.Destroy;
begin
  CloseAllSessions;
  FSessions.Free;
  FLock.Free;
  inherited;
end;

function TSessionManager.GenerateSessionId: string;
var
  GUID: TGUID;
begin
  CreateGUID(GUID);
  Result := GUIDToString(GUID).Replace('{', '').Replace('}', '').Replace('-', '').ToLower;
  Result := 'session_' + Result.Substring(0, 16);
end;

function TSessionManager.CreateSession(const ShellType, SessionId: string): IShellSession;
var
  Id: string;
  Session: IShellSession;
  Info: TSessionInfo;
  CurrentCount: Integer;
begin
  FLock.Enter;
  try
    // Pulizia sessioni inattive prima di crearne una nuova
    CleanupInactiveSessions;

    // Verifica limite
    CurrentCount := FSessions.Count;
    if CurrentCount >= FMaxSessions then
      raise Exception.CreateFmt('Limite sessioni raggiunto (%d/%d)', [CurrentCount, FMaxSessions]);

    // Genera o usa ID fornito
    if SessionId.Trim.IsEmpty then
      Id := GenerateSessionId
    else
      Id := SessionId;

    // Verifica duplicati
    if FSessions.ContainsKey(Id) then
      raise Exception.CreateFmt('Sessione già esistente: %s', [Id]);

    // Crea nuova sessione
    Session := TShellSession.Create(Id, ShellType, FShellFactory);

    // Prepara info sessione
    Info.Session := Session;
    Info.CreatedAt := Now;
    Info.LastActivity := Now;
    Info.CommandCount := 0;
    Info.BytesOutput := 0;
    Info.State := 'active';

    // Aggiungi a dizionario
    FSessions.Add(Id, Info);

    // Notifica evento
    if Assigned(FOnSessionCreated) then
      FOnSessionCreated(Id);

    Result := Session;
  finally
    FLock.Leave;
  end;
end;

function TSessionManager.GetSession(const SessionId: string): IShellSession;
var
  Info: TSessionInfo;
begin
  FLock.Enter;
  try
    if FSessions.TryGetValue(SessionId, Info) then
    begin
      Result := Info.Session;
      // Aggiorna ultima attività
      Info.LastActivity := Now;
      FSessions.AddOrSetValue(SessionId, Info);
    end
    else
      Result := nil;
  finally
    FLock.Leave;
  end;
end;

function TSessionManager.GetSessionInfo(const SessionId: string): TSessionInfo;
begin
  FLock.Enter;
  try
    if not FSessions.TryGetValue(SessionId, Result) then
      raise Exception.CreateFmt('Sessione non trovata: %s', [SessionId]);
  finally
    FLock.Leave;
  end;
end;

function TSessionManager.ListSessions: TArray<string>;
begin
  FLock.Enter;
  try
    Result := FSessions.Keys.ToArray;
  finally
    FLock.Leave;
  end;
end;

procedure TSessionManager.CloseSession(const SessionId: string);
var
  Info: TSessionInfo;
begin
  FLock.Enter;
  try
    if not FSessions.TryGetValue(SessionId, Info) then
      Exit;

    // Ferma esecuzione
    try
      Info.Session.StopSession;
    except
      // Ignora errori durante chiusura
    end;

    // Rimuovi da dizionario
    FSessions.Remove(SessionId);

    // Notifica evento
    if Assigned(FOnSessionClosed) then
      FOnSessionClosed(SessionId);
  finally
    FLock.Leave;
  end;
end;

procedure TSessionManager.CloseAllSessions;
var
  SessionIds: TArray<string>;
  Id: string;
begin
  FLock.Enter;
  try
    SessionIds := FSessions.Keys.ToArray;
  finally
    FLock.Leave;
  end;

  // Chiudi ogni sessione fuori dal lock per evitare deadlock
  for Id in SessionIds do
    CloseSession(Id);
end;

function TSessionManager.CheckLimitSession(const LimitSession: Integer): Boolean;
begin
  FLock.Enter;
  try
    Result := FSessions.Count < LimitSession;
  finally
    FLock.Leave;
  end;
end;

function TSessionManager.GetActiveSessionCount: Integer;
begin
  FLock.Enter;
  try
    Result := FSessions.Count;
  finally
    FLock.Leave;
  end;
end;

procedure TSessionManager.UpdateSessionActivity(const SessionId: string);
var
  Info: TSessionInfo;
begin
  FLock.Enter;
  try
    if FSessions.TryGetValue(SessionId, Info) then
    begin
      Info.LastActivity := Now;
      Info.CommandCount := Info.CommandCount + 1;
      FSessions.AddOrSetValue(SessionId, Info);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TSessionManager.UpdateSessionStats(const SessionId: string; BytesOutput: Integer);
var
  Info: TSessionInfo;
begin
  FLock.Enter;
  try
    if FSessions.TryGetValue(SessionId, Info) then
    begin
      Info.LastActivity := Now;
      Info.BytesOutput := Info.BytesOutput + BytesOutput;
      FSessions.AddOrSetValue(SessionId, Info);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TSessionManager.CleanupInactiveSessions;
var
  ToRemove: TList<string>;
  Pair: TPair<string, TSessionInfo>;
  TimeoutTime: TDateTime;
begin
  // Chiamato con lock già acquisito
  ToRemove := TList<string>.Create;
  try
    TimeoutTime := IncMinute(Now, -FSessionTimeout);

    for Pair in FSessions do
    begin
      if Pair.Value.LastActivity < TimeoutTime then
        ToRemove.Add(Pair.Key);
    end;

    // Rimuovi sessioni inattive
    for var Id in ToRemove do
    begin
      var Info := FSessions[Id];
      Info.State := 'timeout';
      FSessions.AddOrSetValue(Id, Info);
      // La rimozione effettiva avviene fuori dal lock
    end;
  finally
    ToRemove.Free;
  end;
end;

function TSessionManager.GetSessionStats: TJSONObject;
var
  SessionArray: TJSONArray;
  SessionObj: TJSONObject;
  Pair: TPair<string, TSessionInfo>;
  TotalBytes: Int64;
  TotalCommands: Integer;
begin
  Result := TJSONObject.Create;
  SessionArray := TJSONArray.Create;
  TotalBytes := 0;
  TotalCommands := 0;

  FLock.Enter;
  try
    for Pair in FSessions do
    begin
      SessionObj := TJSONObject.Create;
      SessionObj.AddPair('sessionId', Pair.Key);
      SessionObj.AddPair('shellType', Pair.Value.Session.ShellType);
      SessionObj.AddPair('createdAt', DateToISO8601(Pair.Value.CreatedAt));
      SessionObj.AddPair('lastActivity', DateToISO8601(Pair.Value.LastActivity));
      SessionObj.AddPair('commandCount', TJSONNumber.Create(Pair.Value.CommandCount));
      SessionObj.AddPair('bytesOutput', TJSONNumber.Create(Pair.Value.BytesOutput));
      SessionObj.AddPair('state', Pair.Value.State);
      SessionObj.AddPair('isActive', TJSONBool.Create(Pair.Value.Session.IsActive));

      SessionArray.Add(SessionObj);

      TotalBytes := TotalBytes + Pair.Value.BytesOutput;
      TotalCommands := TotalCommands + Pair.Value.CommandCount;
    end;

    Result.AddPair('sessions', SessionArray);
    Result.AddPair('totalSessions', TJSONNumber.Create(FSessions.Count));
    Result.AddPair('totalCommands', TJSONNumber.Create(TotalCommands));
    Result.AddPair('totalBytesOutput', TJSONNumber.Create(TotalBytes));
    Result.AddPair('maxSessions', TJSONNumber.Create(FMaxSessions));
    Result.AddPair('sessionTimeoutMinutes', TJSONNumber.Create(FSessionTimeout));
  finally
    FLock.Leave;
  end;
end;

function TSessionManager.GetOnSessionCreated: TSessionEvent;
begin
  Result := FOnSessionCreated;
end;

procedure TSessionManager.SetOnSessionCreated(const Value: TSessionEvent);
begin
  FOnSessionCreated := Value;
end;

function TSessionManager.GetOnSessionClosed: TSessionEvent;
begin
  Result := FOnSessionClosed;
end;

procedure TSessionManager.SetOnSessionClosed(const Value: TSessionEvent);
begin
  FOnSessionClosed := Value;
end;

end.
