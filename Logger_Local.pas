unit Logger_Local;

interface

uses
  AgentServiceI, System.SysUtils, System.Classes, System.Generics.Collections,
  System.SyncObjs; // Aggiunta per TCriticalSection

type
  TLogger_Local = class(TInterfacedObject, ILogger)
  private
    FLogLines: TThreadList<string>;
    FLogFile: string;
    FFileLock: TCriticalSection; // Per la scrittura thread-safe su file
    procedure AppendToFile(const Msg: string);
    procedure InternalLog(const Level, Msg: string);
  public
    constructor Create(const LogFile: string = '');
    destructor Destroy; override;
    // Implementazione completa di ILogger
    procedure Info(const Msg: string);
    procedure Warn(const Msg: string);
    procedure Error(const Msg: string);
    procedure Debug(const Msg: string); // Metodo che mancava
    function GetLogs: TArray<string>;
  end;

implementation

uses
  System.IOUtils; // Aggiunta per TPath e TStreamWriter

{ TLogger_Local }

constructor TLogger_Local.Create(const LogFile: string);
var
  DefaultLogPath: string;
begin
  inherited Create;
  FLogLines := TThreadList<string>.Create;
  FFileLock := TCriticalSection.Create;

  if LogFile <> '' then
    FLogFile := LogFile
  else
  begin
    // È più robusto mettere il log vicino all'eseguibile invece che nella CWD
    DefaultLogPath := TPath.GetDirectoryName(ParamStr(0));
    FLogFile := TPath.Combine(DefaultLogPath, 'agent.log');
  end;
end;

destructor TLogger_Local.Destroy;
begin
  FreeAndNil(FLogLines);
  FreeAndNil(FFileLock);
  inherited;
end;

procedure TLogger_Local.AppendToFile(const Msg: string);
var
  LStreamWriter: TStreamWriter;
begin
  // Blocca l'accesso al file da altri thread
  FFileLock.Enter;
  try
    // Usa TStreamWriter per una scrittura più semplice e pulita
    // 'True' come secondo parametro significa "append" (aggiungi in fondo)
    LStreamWriter := TStreamWriter.Create(FLogFile, True, TEncoding.UTF8);
    try
      LStreamWriter.WriteLine(Msg);
    finally
      LStreamWriter.Free;
    end;
  finally
    // Rilascia il lock SEMPRE, anche in caso di errore
    FFileLock.Leave;
  end;
end;

procedure TLogger_Local.InternalLog(const Level, Msg: string);
var
  Line: string;
begin
  // Formatta la linea di log una sola volta
  Line := Format('[%s] %s: %s', [Level, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now), Msg]);
  // Aggiunge alla lista in memoria (TThreadList è già thread-safe)
  FLogLines.Add(Line);
  // Scrive su file (usando il nostro metodo protetto dal lock)
  AppendToFile(Line);
end;

procedure TLogger_Local.Info(const Msg: string);
begin
  InternalLog('INFO', Msg);
end;

procedure TLogger_Local.Error(const Msg: string);
begin
  InternalLog('ERROR', Msg);
end;

procedure TLogger_Local.Warn(const Msg: string);
begin
  InternalLog('WARN', Msg);
end;

// Implementazione del metodo mancante
procedure TLogger_Local.Debug(const Msg: string);
begin
  // Per ora, lo trattiamo come un log di tipo INFO.
  // In futuro si potrebbe decidere di scriverlo su un file diverso
  // o di non scriverlo affatto in base a un livello di verbosità.
  {$IFDEF DEBUG}
  InternalLog('DEBUG', Msg);
  {$ENDIF}
end;

function TLogger_Local.GetLogs: TArray<string>;
var
  LList: TList<string>;
begin
  // Il modo corretto e sicuro di accedere a TThreadList
  LList := FLogLines.LockList;
  try
    Result := LList.ToArray;
  finally
    // Sblocca la lista SEMPRE, anche se ToArray fallisce (es. per memoria)
    FLogLines.UnlockList;
  end;
end;

end.
