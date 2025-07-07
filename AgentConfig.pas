unit AgentConfig;

interface

uses
  System.SysUtils, System.JSON, System.IOUtils, System.Classes;

type
  TChannelType = (ctHTTP, ctDDP);

  TAgentConfig = class
  private
    FAgentId: string;
    FShellType: string;
    FSessionLimit: Integer;
    FChannelType: TChannelType;

    // HTTP Configuration
    FHttpUrl: string;
    FHttpTimeout: Integer;
    FHttpRetries: Integer;

    // DDP Configuration - NEW
    FDDPServerUrl: string;
    FDDPUsername: string;
    FDDPPassword: string;
    FDDPToken: string;
    FDDPReconnectAttempts: Integer;
    FDDPHeartbeatInterval: Integer;

    // Logging
    FLogLevel: string;
    FLogFile: string;

    function ChannelTypeToString(const ChannelType: TChannelType): string;
    function StringToChannelType(const ChannelStr: string): TChannelType;

  public
    constructor Create;

    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    procedure LoadFromJSON(const JSON: string);
    function ToJSON: string;

    // Properties
    property AgentId: string read FAgentId write FAgentId;
    property ShellType: string read FShellType write FShellType;
    property SessionLimit: Integer read FSessionLimit write FSessionLimit;
    property ChannelType: TChannelType read FChannelType write FChannelType;

    // HTTP Properties
    property HttpUrl: string read FHttpUrl write FHttpUrl;
    property HttpTimeout: Integer read FHttpTimeout write FHttpTimeout;
    property HttpRetries: Integer read FHttpRetries write FHttpRetries;

    // DDP Properties
    property DDPServerUrl: string read FDDPServerUrl write FDDPServerUrl;
    property DDPUsername: string read FDDPUsername write FDDPUsername;
    property DDPPassword: string read FDDPPassword write FDDPPassword;
    property DDPToken: string read FDDPToken write FDDPToken;
    property DDPReconnectAttempts: Integer read FDDPReconnectAttempts write FDDPReconnectAttempts;
    property DDPHeartbeatInterval: Integer read FDDPHeartbeatInterval write FDDPHeartbeatInterval;

    // Logging Properties
    property LogLevel: string read FLogLevel write FLogLevel;
    property LogFile: string read FLogFile write FLogFile;

    // Helper methods
    function GetChannelTypeString: string;
    procedure SetDefaultValues;
    function Validate: string; // Returns error message if invalid, empty if OK
  end;

implementation

{ TAgentConfig }

constructor TAgentConfig.Create;
begin
  inherited Create;
  SetDefaultValues;
end;

procedure TAgentConfig.SetDefaultValues;
var
  GUID: TGUID;
begin
  // Generate unique agent ID
  CreateGUID(GUID);
  FAgentId := 'agent_' + GUIDToString(GUID).Replace('{', '').Replace('}', '').Replace('-', '').ToLower.Substring(0, 16);

  FShellType := 'cmd';
  FSessionLimit := 10;
  FChannelType := ctDDP; // Default to DDP now

  // HTTP defaults
  FHttpUrl := 'http://localhost:3000';
  FHttpTimeout := 30000;
  FHttpRetries := 3;


  // DDP defaults
  FDDPServerUrl := 'ws://localhost:3000/websocket';
  FDDPUsername := 'aa';
  FDDPPassword := 'aa';
  FDDPToken := 'ccc';
  FDDPReconnectAttempts := 5;
  FDDPHeartbeatInterval := 30000;

  // Logging defaults
  FLogLevel := 'INFO';
  FLogFile := 'agent.log';
end;

function TAgentConfig.ChannelTypeToString(const ChannelType: TChannelType): string;
begin
  case ChannelType of
    ctHTTP: Result := 'http';
    ctDDP: Result := 'ddp';
  else
    Result := 'ddp'; // Default
  end;
end;

function TAgentConfig.StringToChannelType(const ChannelStr: string): TChannelType;
begin
  if SameText(ChannelStr, 'http') then
    Result := ctHTTP
  else if SameText(ChannelStr, 'ddp') then
    Result := ctDDP
  else
    Result := ctDDP; // Default
end;

function TAgentConfig.GetChannelTypeString: string;
begin
  Result := ChannelTypeToString(FChannelType);
end;

procedure TAgentConfig.LoadFromFile(const FileName: string);
var
  JSONContent: string;
begin
  if not TFile.Exists(FileName) then
    raise Exception.CreateFmt('Configuration file not found: %s', [FileName]);

  JSONContent := TFile.ReadAllText(FileName, TEncoding.UTF8);
  LoadFromJSON(JSONContent);
end;

procedure TAgentConfig.SaveToFile(const FileName: string);
var
  JSONContent: string;
begin
  JSONContent := ToJSON;
  TFile.WriteAllText(FileName, JSONContent, TEncoding.UTF8);
end;

procedure TAgentConfig.LoadFromJSON(const JSON: string);
var
  JSONObj: TJSONObject;
  HTTPObj, DDPObj, LogObj: TJSONObject;
begin
  JSONObj := TJSONObject.ParseJSONValue(JSON) as TJSONObject;
  if not Assigned(JSONObj) then
    raise Exception.Create('Invalid JSON configuration');

  try
    // Basic settings
    FAgentId := JSONObj.GetValue<string>('agent_id', FAgentId);
    FShellType := JSONObj.GetValue<string>('shell_type', FShellType);
    FSessionLimit := JSONObj.GetValue<Integer>('session_limit', FSessionLimit);
    FChannelType := StringToChannelType(JSONObj.GetValue<string>('channel_type', 'ddp'));

    // HTTP configuration
    if JSONObj.TryGetValue<TJSONObject>('http', HTTPObj) then
    begin
      FHttpUrl := HTTPObj.GetValue<string>('url', FHttpUrl);
      FHttpTimeout := HTTPObj.GetValue<Integer>('timeout', FHttpTimeout);
      FHttpRetries := HTTPObj.GetValue<Integer>('retries', FHttpRetries);
    end;

    // DDP configuration
    if JSONObj.TryGetValue<TJSONObject>('ddp', DDPObj) then
    begin
      FDDPServerUrl := DDPObj.GetValue<string>('server_url', FDDPServerUrl);
      FDDPUsername := DDPObj.GetValue<string>('username', FDDPUsername);
      FDDPPassword := DDPObj.GetValue<string>('password', FDDPPassword);
      FDDPToken := DDPObj.GetValue<string>('token', FDDPToken);
      FDDPReconnectAttempts := DDPObj.GetValue<Integer>('reconnect_attempts', FDDPReconnectAttempts);
      FDDPHeartbeatInterval := DDPObj.GetValue<Integer>('heartbeat_interval', FDDPHeartbeatInterval);
    end;

    // Logging configuration
    if JSONObj.TryGetValue<TJSONObject>('logging', LogObj) then
    begin
      FLogLevel := LogObj.GetValue<string>('level', FLogLevel);
      FLogFile := LogObj.GetValue<string>('file', FLogFile);
    end;

  finally
    JSONObj.Free;
  end;
end;

function TAgentConfig.ToJSON: string;
var
  JSONObj, HTTPObj, DDPObj, LogObj: TJSONObject;
begin
  JSONObj := TJSONObject.Create;
  try
    // Basic settings
    JSONObj.AddPair('agent_id', FAgentId);
    JSONObj.AddPair('shell_type', FShellType);
    JSONObj.AddPair('session_limit', TJSONNumber.Create(FSessionLimit));
    JSONObj.AddPair('channel_type', GetChannelTypeString);

    // HTTP configuration
    HTTPObj := TJSONObject.Create;
    HTTPObj.AddPair('url', FHttpUrl);
    HTTPObj.AddPair('timeout', TJSONNumber.Create(FHttpTimeout));
    HTTPObj.AddPair('retries', TJSONNumber.Create(FHttpRetries));
    JSONObj.AddPair('http', HTTPObj);

    // DDP configuration
    DDPObj := TJSONObject.Create;
    DDPObj.AddPair('server_url', FDDPServerUrl);
    DDPObj.AddPair('username', FDDPUsername);
    DDPObj.AddPair('password', FDDPPassword);
    DDPObj.AddPair('token', FDDPToken);
    DDPObj.AddPair('reconnect_attempts', TJSONNumber.Create(FDDPReconnectAttempts));
    DDPObj.AddPair('heartbeat_interval', TJSONNumber.Create(FDDPHeartbeatInterval));
    JSONObj.AddPair('ddp', DDPObj);

    // Logging configuration
    LogObj := TJSONObject.Create;
    LogObj.AddPair('level', FLogLevel);
    LogObj.AddPair('file', FLogFile);
    JSONObj.AddPair('logging', LogObj);

    Result := JSONObj.ToString;
  finally
    JSONObj.Free;
  end;
end;

function TAgentConfig.Validate: string;
begin
  Result := '';

  if FAgentId.Trim.IsEmpty then
    Result := Result + 'Agent ID cannot be empty. ';

  if not (FSessionLimit > 0) then
    Result := Result + 'Session limit must be positive. ';

  case FChannelType of
    ctHTTP:
      begin
        if FHttpUrl.Trim.IsEmpty then
          Result := Result + 'HTTP URL cannot be empty. ';
        if FHttpTimeout <= 0 then
          Result := Result + 'HTTP timeout must be positive. ';
      end;

    ctDDP:
      begin
        if FDDPServerUrl.Trim.IsEmpty then
          Result := Result + 'DDP server URL cannot be empty. ';
        if FDDPReconnectAttempts < 0 then
          Result := Result + 'DDP reconnect attempts cannot be negative. ';
        if FDDPHeartbeatInterval <= 0 then
          Result := Result + 'DDP heartbeat interval must be positive. ';
      end;
  end;

  Result := Result.Trim;
end;

end.
