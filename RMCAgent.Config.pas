unit RMCAgent.Config;

interface

uses
  Agent.DDP.NetLib.Factory,
  System.SysUtils,
  System.IniFiles;

type
  TAgentConfig = class
  private
    FConfigFile: string;
    FServerURL: string;
    FUsername: string;
    FPassword: string;
    FToken: string;
    FNetLibType: TAgentNetLibType;
    FAutoReconnect: Boolean;
    FReconnectDelay: Integer;
    FUseSSL: Boolean;
    FTimeout: Integer;
    FLogLevel: string;
    FAgentId: string;
    FAgentName: string;
    FWorkingDirectory: string;

    procedure SetDefaults;
  public
    constructor Create(const AConfigFile: string = '');
    destructor Destroy; override;

    procedure LoadFromFile;
    procedure SaveToFile;
    procedure LoadFromCommandLine;

    // Properties
    property ServerURL: string read FServerURL write FServerURL;
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property Token: string read FToken write FToken;
    property NetLibType: TAgentNetLibType read FNetLibType write FNetLibType;
    property AutoReconnect: Boolean read FAutoReconnect write FAutoReconnect;
    property ReconnectDelay: Integer read FReconnectDelay write FReconnectDelay;
    property UseSSL: Boolean read FUseSSL write FUseSSL;
    property Timeout: Integer read FTimeout write FTimeout;
    property LogLevel: string read FLogLevel write FLogLevel;
    property AgentId: string read FAgentId write FAgentId;
    property AgentName: string read FAgentName write FAgentName;
    property WorkingDirectory: string read FWorkingDirectory write FWorkingDirectory;
  end;

implementation

uses
  System.IOUtils;

{ TAgentConfig }

constructor TAgentConfig.Create(const AConfigFile: string);
begin
  inherited Create;

  if AConfigFile <> '' then
    FConfigFile := AConfigFile
  else
    FConfigFile := ChangeFileExt(ParamStr(0), '.ini');

  SetDefaults;

  if TFile.Exists(FConfigFile) then
    LoadFromFile;

  LoadFromCommandLine;
end;

destructor TAgentConfig.Destroy;
begin
  inherited;
end;

procedure TAgentConfig.SetDefaults;
begin
  FServerURL := 'ws://localhost:3000/websocket';
  FUsername := '';
  FPassword := '';
  FToken := '';
  FNetLibType := TAgentNetLibType.Grijjy;
  FAutoReconnect := True;
  FReconnectDelay := 10000; // 10 seconds
  FUseSSL := False;
  FTimeout := 30000; // 30 seconds
  FLogLevel := 'INFO';
  FAgentId := '';
  FAgentName := 'Agent-' + FormatDateTime('yyyymmdd-hhnnss', Now);
  FWorkingDirectory := GetCurrentDir;
end;

procedure TAgentConfig.LoadFromFile;
var
  LIni: TIniFile;
begin
  LIni := TIniFile.Create(FConfigFile);
  try
    // Connection settings
    FServerURL := LIni.ReadString('Connection', 'ServerURL', FServerURL);
    FUseSSL := LIni.ReadBool('Connection', 'UseSSL', FUseSSL);
    FTimeout := LIni.ReadInteger('Connection', 'Timeout', FTimeout);

    case LIni.ReadInteger('Connection', 'NetLibType', Ord(FNetLibType)) of
      0: FNetLibType := TAgentNetLibType.Grijjy;
      1: FNetLibType := TAgentNetLibType.SGC;
    end;

    // Authentication
    FUsername := LIni.ReadString('Auth', 'Username', FUsername);
    FPassword := LIni.ReadString('Auth', 'Password', FPassword);
    FToken := LIni.ReadString('Auth', 'Token', FToken);

    // Agent settings
    FAgentId := LIni.ReadString('Agent', 'AgentId', FAgentId);
    FAgentName := LIni.ReadString('Agent', 'AgentName', FAgentName);
    FWorkingDirectory := LIni.ReadString('Agent', 'WorkingDirectory', FWorkingDirectory);

    // Reconnection
    FAutoReconnect := LIni.ReadBool('Reconnect', 'AutoReconnect', FAutoReconnect);
    FReconnectDelay := LIni.ReadInteger('Reconnect', 'ReconnectDelay', FReconnectDelay);

    // Logging
    FLogLevel := LIni.ReadString('Logging', 'LogLevel', FLogLevel);
  finally
    LIni.Free;
  end;
end;

procedure TAgentConfig.SaveToFile;
var
  LIni: TIniFile;
begin
  LIni := TIniFile.Create(FConfigFile);
  try
    // Connection settings
    LIni.WriteString('Connection', 'ServerURL', FServerURL);
    LIni.WriteBool('Connection', 'UseSSL', FUseSSL);
    LIni.WriteInteger('Connection', 'Timeout', FTimeout);
    LIni.WriteInteger('Connection', 'NetLibType', Ord(FNetLibType));

    // Authentication
    LIni.WriteString('Auth', 'Username', FUsername);
    LIni.WriteString('Auth', 'Password', FPassword);
    LIni.WriteString('Auth', 'Token', FToken);

    // Agent settings
    LIni.WriteString('Agent', 'AgentId', FAgentId);
    LIni.WriteString('Agent', 'AgentName', FAgentName);
    LIni.WriteString('Agent', 'WorkingDirectory', FWorkingDirectory);

    // Reconnection
    LIni.WriteBool('Reconnect', 'AutoReconnect', FAutoReconnect);
    LIni.WriteInteger('Reconnect', 'ReconnectDelay', FReconnectDelay);

    // Logging
    LIni.WriteString('Logging', 'LogLevel', FLogLevel);
  finally
    LIni.Free;
  end;
end;

procedure TAgentConfig.LoadFromCommandLine;
var
  i: Integer;
  LParam, LValue: string;
begin
  for i := 1 to ParamCount do
  begin
    LParam := ParamStr(i);

    if (LParam.StartsWith('-') or LParam.StartsWith('/')) and (i < ParamCount) then
    begin
      LValue := ParamStr(i + 1);
      LParam := LParam.Substring(1).ToLower;

      if LParam = 'server' then
        FServerURL := LValue
      else if LParam = 'username' then
        FUsername := LValue
      else if LParam = 'password' then
        FPassword := LValue
      else if LParam = 'token' then
        FToken := LValue
      else if LParam = 'agentid' then
        FAgentId := LValue
      else if LParam = 'agentname' then
        FAgentName := LValue
      else if LParam = 'workdir' then
        FWorkingDirectory := LValue
      else if LParam = 'loglevel' then
        FLogLevel := LValue
      else if LParam = 'netlib' then
      begin
        if SameText(LValue, 'grijjy') then
          FNetLibType := TAgentNetLibType.Grijjy
        else if SameText(LValue, 'sgc') then
          FNetLibType := TAgentNetLibType.SGC;
      end
      else if LParam = 'timeout' then
        FTimeout := StrToIntDef(LValue, FTimeout)
      else if LParam = 'reconnectdelay' then
        FReconnectDelay := StrToIntDef(LValue, FReconnectDelay);
    end
    else if SameText(LParam, '-ssl') or SameText(LParam, '/ssl') then
      FUseSSL := True
    else if SameText(LParam, '-noautoreconnect') or SameText(LParam, '/noautoreconnect') then
      FAutoReconnect := False;
  end;
end;

end.
