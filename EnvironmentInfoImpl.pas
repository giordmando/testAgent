unit EnvironmentInfoImpl;

interface

uses
  System.SysUtils,
  AgentServiceI;  // contiene IEnvironmentInfo

/// <summary>Factory che crea l’implementazione corretta di IEnvironmentInfo a runtime.</summary>
function CreateEnvironmentInfo: IEnvironmentInfo;

type
  /// <summary>Classe base con le implementazioni comuni o mancanti.</summary>
  TBaseEnvironmentInfo = class(TInterfacedObject, IEnvironmentInfo)
  public
    // Metodi comuni a tutte le piattaforme
    function GetTempDir: string;
    function GetHostname: string;
    function GetOSVersion: string;
    // Metodi astratti che le classi figlie devono implementare
    function GetHomeDir: string; virtual; abstract;
    function GetInstallPath: string; virtual; abstract;
    function GetConfigDir: string; virtual; abstract;
    function IsWindows: Boolean; virtual; abstract;
    function IsLinux: Boolean; virtual; abstract;
  end;

  /// <summary>Informazioni di ambiente su Windows.</summary>
  TWindowsEnvironmentInfo = class(TBaseEnvironmentInfo)
  public
    function GetHomeDir: string; override;
    function GetInstallPath: string; override;
    function GetConfigDir: string; override;
    function IsWindows: Boolean; override;
    function IsLinux: Boolean; override;
  end;


implementation

uses
  System.IOUtils,
  System.Types,
  System.Classes,
  Winapi.Windows; // Aggiunta per le funzioni specifiche di Windows

function CreateEnvironmentInfo: IEnvironmentInfo;
begin
  Result := TWindowsEnvironmentInfo.Create;
end;

// Restituisce il nome base dell’app (eseguibile senza estensione)
function GetAppBaseName: string;
begin
  Result := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
  if Result.IsEmpty then
    Result := 'App';
end;

{ TBaseEnvironmentInfo - Implementazioni Comuni }

function TBaseEnvironmentInfo.GetTempDir: string;
begin
  // TPath.GetTempPath è cross-platform
  Result := IncludeTrailingPathDelimiter(TPath.GetTempPath);
end;

function TBaseEnvironmentInfo.GetHostname: string;
var
  Buffer: array[0..MAX_COMPUTERNAME_LENGTH] of Char;
  Size: DWORD;
begin
  Size := MAX_COMPUTERNAME_LENGTH + 1;
  {$IFDEF MSWINDOWS}
  if GetComputerName(Buffer, Size) then
    Result := string(Buffer)
  else
    Result := 'localhost';
  {$ELSE}
  // Su Linux, questo richiederebbe una chiamata a gethostname() dalla unit Libc
  // Per semplicità, usiamo una variabile d'ambiente comune se disponibile.
  Result := GetEnvironmentVariable('HOSTNAME');
  if Result.IsEmpty then
    Result := 'localhost';
  {$ENDIF}
end;

function TBaseEnvironmentInfo.GetOSVersion: string;
begin
  {$IF CompilerVersion >= 33} // Delphi 10.3+
    Result := TOSVersion.ToString;
  {$ELSE}
    // Implementazione legacy per versioni più vecchie di Delphi
    {$IFDEF MSWINDOWS}
      // Questa è una semplificazione, ottenere la versione completa richiede più API
      Result := 'Windows (Legacy)';
    {$ELSE}
      Result := 'Linux (Legacy)';
    {$ENDIF}
  {$ENDIF}
end;

{ TWindowsEnvironmentInfo }

function TWindowsEnvironmentInfo.GetHomeDir: string;
begin
  Result := TPath.GetHomePath; // Più moderno e robusto
  if Result.IsEmpty then
    Result := GetInstallPath;
end;

function TWindowsEnvironmentInfo.GetInstallPath: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
end;

function TWindowsEnvironmentInfo.GetConfigDir: string;
var
  AppData: string;
  Dir: string;
begin
  // Recupera il percorso della cartella AppData locale tramite variabile d'ambiente
  AppData := GetEnvironmentVariable('LOCALAPPDATA');
  if AppData = '' then
    // fallback se la variabile non è definita (es. Windows XP)
    AppData := TPath.Combine(TPath.GetHomePath, 'AppData\Local');

  Dir := TPath.Combine(AppData, GetAppBaseName);

  if not TDirectory.Exists(Dir) then
    ForceDirectories(Dir);

  Result := IncludeTrailingPathDelimiter(Dir);
end;

function TWindowsEnvironmentInfo.IsWindows: Boolean;
begin
  Result := True;
end;

function TWindowsEnvironmentInfo.IsLinux: Boolean;
begin
  Result := False;
end;

end.
