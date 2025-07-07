unit AuthManagerImpl;

interface

uses
  AgentServiceI, System.SysUtils, System.Classes, System.Masks,
  System.SyncObjs; // Aggiunta per TCriticalSection

type
  TAuthManager = class(TInterfacedObject, IAuthManager)
  private
    FWhitelist: TStrings;
    FBlacklist: TStrings;
    FLock: TCriticalSection; // Per thread-safety
    function CheckList(const Command: string; const List: TStrings): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    // Implementazione completa di IAuthManager
    procedure AddToWhitelist(const CommandPattern: string);
    procedure AddToBlacklist(const CommandPattern: string);
    function IsAuthorized(const SessionId, Command: string): Boolean;
    procedure LoadRules(const Source: string); // Metodo che mancava
    procedure Clear;
  end;

implementation

{ TAuthManager }

constructor TAuthManager.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FWhitelist := TStringList.Create;
  FBlacklist := TStringList.Create;
  // Per default, le liste sono vuote. Caricare da file avviene tramite LoadRules.
end;

destructor TAuthManager.Destroy;
begin
  FreeAndNil(FWhitelist);
  FreeAndNil(FBlacklist);
  FreeAndNil(FLock);
  inherited;
end;

// Funzione helper privata per controllare una lista (whitelist o blacklist)
function TAuthManager.CheckList(const Command: string; const List: TStrings): Boolean;
var
  i: Integer;
  Pattern: string;
begin
  Result := False;
  for i := 0 to List.Count - 1 do
  begin
    Pattern := List[i];
    // SimpleMatch è una funzione robusta per i pattern con '*' e '?'
    // Il flag 'True' alla fine indica che il confronto deve essere case-insensitive.
    if MatchesMask(Command, Pattern) then
    begin
      Result := True;
      Exit; // Trovato un match, non serve continuare
    end;
  end;
end;

procedure TAuthManager.AddToWhitelist(const CommandPattern: string);
begin
  FLock.Enter;
  try
    FWhitelist.Add(CommandPattern);
  finally
    FLock.Leave;
  end;
end;

procedure TAuthManager.AddToBlacklist(const CommandPattern: string);
begin
  FLock.Enter;
  try
    FBlacklist.Add(CommandPattern);
  finally
    FLock.Leave;
  end;
end;

procedure TAuthManager.Clear;
begin
  FLock.Enter;
  try
    FWhitelist.Clear;
    FBlacklist.Clear;
  finally
    FLock.Leave;
  end;
end;

// Implementazione del metodo mancante
procedure TAuthManager.LoadRules(const Source: string);
begin
  // Qui si implementerebbe la logica per caricare le regole.
  // L'origine (Source) potrebbe essere un nome di file, una stringa JSON, etc.
  // Per ora, implementiamo un esempio semplice che carica da un file di testo.
  if FileExists(Source) then
  begin
    var Rules := TStringList.Create;
    try
      Rules.LoadFromFile(Source);
      FLock.Enter;
      try
        // Pulisce le regole vecchie prima di caricarne di nuove
        FWhitelist.Clear;
        FBlacklist.Clear;
        for var Line in Rules do
        begin
          if Line.StartsWith('+') then // Una regola di whitelist inizia con '+'
            FWhitelist.Add(Line.Substring(1).Trim)
          else if Line.StartsWith('-') then // Una regola di blacklist inizia con '-'
            FBlacklist.Add(Line.Substring(1).Trim);
        end;
      finally
        FLock.Leave;
      end;
    finally
      Rules.Free;
    end;
  end
  else
  begin
    // Potremmo lanciare un'eccezione o semplicemente loggare un errore.
    // raise EFileNotFoundException.CreateFmt('File di regole non trovato: %s', [Source]);
  end;
end;

function TAuthManager.IsAuthorized(const SessionId, Command: string): Boolean;
begin
  // La logica di autorizzazione deve essere thread-safe
  FLock.Enter;
  try
    // Regola 1: Se il comando è nella blacklist, è sempre vietato.
    // La blacklist ha la precedenza assoluta.
    if CheckList(Command, FBlacklist) then
    begin
      Result := False;
      Exit;
    end;

    // Regola 2: Se la whitelist è vuota, tutti i comandi non bloccati
    // dalla blacklist sono permessi.
    if FWhitelist.Count = 0 then
    begin
      Result := True;
      Exit;
    end;

    // Regola 3: Se la whitelist non è vuota, il comando deve essere
    // esplicitamente permesso.
    Result := CheckList(Command, FWhitelist);

  finally
    FLock.Leave;
  end;
end;

end.
