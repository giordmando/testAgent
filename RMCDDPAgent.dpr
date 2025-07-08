// MAIN PROGRAM FINALE con debug completo per identificare il problema

program RMCDDPAgent;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.DateUtils,
  System.Classes,
  System.JSON,
  AgentConfig,
  RMC.ActionCreator,
  RMC.DDP.NetLib.SGC,
  DDP.NetLib.Factory,
  Factories,
  DDP.Interfaces,
  EnvironmentInfoImpl,
  AgentServiceI,
  SessionManager,
  DDP.Actions.Consts,
  DDP.Factories,
  DDP.RequestGenerator,
  RMC.Connection,
  RMC.Data,
  DDP.Exception,
  RMC.Store.Agent;

var
  Config: TAgentConfig;
  ActionCreator: IActionCreatorAgent;
  EnvironmentInfo: IEnvironmentInfo;
  ShellRunnerFactory: IShellRunnerFactory;
  SessionMgr: ISessionManager;

  FAgentId: string;

    FDDPRequestGenerator: IDDPRequestGenerator;
    FAgentConnection: IAgentConnection;
    FAgentData: IAgentData;

    FDDPNetLib: IDDPNetLib;     // <- Mantieni riferimento
    FDDPClient: IDDPClient;     // <- Mantieni riferimento
    FDDPLogin: IDDPLogin;
    FAgentStore: IStoreAgent;

procedure InitializeConfig;
begin
  WriteLn('=== RMC DDP Agent v1.0 - PRODUCTION VERSION ===');
  WriteLn('Loading configuration...');

  Config := TAgentConfig.Create;
  Config.LoadFromFile('agent.conf.json');

  WriteLn('Agent ID: ' + Config.AgentId);
  WriteLn('Server URL: ' + Config.DDPServerUrl);
end;

procedure InitializeServices;
begin
  WriteLn('Initializing services...');
  EnvironmentInfo := CreateEnvironmentInfo;
  ShellRunnerFactory := TShellRunnerFactory.Create(EnvironmentInfo);
  SessionMgr := TSessionManager.Create(ShellRunnerFactory);
  WriteLn('Services initialized');
end;

procedure RegisterNetworkFactory;
begin
  WriteLn('Registering network factory...');
  TDDPNetLibFactory.Register(
    function: IDDPNetLib
    begin
      Result := TAgentDDPNetLibSGC.Create(Config.DDPServerUrl, Config.AgentId);
    end
  );
  WriteLn('Network factory registered');
end;

procedure InitializeActionCreator;
begin
  WriteLn('Initializing ActionCreator...');

  FDDPNetLib := TDDPNetLibFactory.CreateNew;
  FDDPClient := GetDDPClient(FDDPNetLib, TDDPRequestGenerator.Create);
  FDDPLogin := GetDDPLogin(FDDPClient);
  FAgentConnection := GetAgentConnection(FDDPClient, FDDPLogin);
  //FDDPClient.Connect;
  FAgentData := GetAgentData(FDDPClient);
  //FAgentStore := GetStoreAgent(FAgentId);
  FDDPClient.Connect;
  ActionCreator := GetActionCreatorAgentWithId(Config.AgentId);
  ActionCreator.SetShellRunnerFactory(ShellRunnerFactory);
  ActionCreator.SetSessionManager(SessionMgr);
  ActionCreator.SetAgentConnection(FAgentConnection);
  ActionCreator.SetAgentData(FAgentData);
  WriteLn('ActionCreator initialized');
end;

procedure ConnectAndLogin;
begin
  WriteLn('Connecting and logging in...');

  if Config.DDPToken <> '' then
  begin
    WriteLn('Using token authentication...');
    ActionCreator.Login(Config.DDPToken);  // <- Build() chiamato qui

    // *** ASPETTA che la connessione sia stabilita ***
    WriteLn('Waiting for connection to establish...');
    Sleep(15000);
     if    ActionCreator.IsConnected   then
      WriteLn('? Connection established');

  end
  else if (Config.DDPUsername <> '') and (Config.DDPPassword <> '') then
  begin
    WriteLn('Using username/password authentication...');
    ActionCreator.Login(Config.DDPUsername, Config.DDPPassword);
    Sleep(15000);
    if    ActionCreator.IsConnected   then
      WriteLn('? Connection established');

  end
  else
  begin
    WriteLn('Error: No authentication credentials configured');
    Exit;
  end;

  WriteLn('Final connection status: ' + ActionCreator.GetConnectionStatus);

end;

procedure TestFluxConnection;
begin
  WriteLn('=== TESTING FLUX CONNECTION ===');

  if not ActionCreator.IsConnected then
  begin
    WriteLn('? ActionCreator not connected - cannot test');
    Exit;
  end;

  WriteLn('? ActionCreator connected - testing message flow');
  WriteLn('');
  WriteLn('STEP 1: Testing ping (should show TReadThread activity)');
  ActionCreator.ping(Config.AgentId);

  WriteLn('');
  WriteLn('STEP 2: Inserting session (should trigger DDP ? Flux flow)');
  WriteLn('Watch for this sequence:');
  WriteLn('  [TReadThread] Received ? [TAddedHandler] ? [ActionCreatorDDP] ? [StoreAgent]');

  var SessionId := ActionCreator.TestInsertSession;
  if SessionId <> '' then
  begin
    WriteLn('Session inserted: ' + SessionId);
    WriteLn('Waiting 3 seconds for DDP processing...');
    Sleep(3000);

    WriteLn('');
    WriteLn('STEP 3: Inserting command (should trigger command execution)');
    var CommandId := ActionCreator.TestInsertCommand(SessionId);
    if CommandId <> '' then
    begin
      WriteLn('Command inserted: ' + CommandId);
      WriteLn('Waiting 5 seconds for command execution...');
      Sleep(5000);
    end;
  end;

  WriteLn('');
  WriteLn('*** DIAGNOSIS ***');
  WriteLn('If you saw [StoreAgent] DDP Action messages:');
  WriteLn('  ? DDP ? Flux connection is working');
  WriteLn('If you did NOT see [StoreAgent] messages:');
  WriteLn('  ? DDP ? Flux connection is broken');
  WriteLn('  Check: Handler registration, Dispatcher connection, Store registration');

  WriteLn('=== TEST COMPLETE ===');
end;

procedure RunDiagnosticMode;
begin
  WriteLn('=== DIAGNOSTIC MODE ===');
  WriteLn('Running continuous diagnostics...');
  WriteLn('');

  var TestCount := 0;
  while True do
  begin
    Inc(TestCount);
    WriteLn(Format('[%s] Diagnostic Test #%d', [FormatDateTime('hh:nn:ss', Now), TestCount]));

    if ActionCreator.IsConnected then
    begin
      WriteLn('  Connection: OK');


      // Test ogni 30 secondi
      if TestCount mod 6 = 1 then  // Ogni 6° iterazione (30 secondi)
      begin
        WriteLn('  Running full test...');
        TestFluxConnection;
      end;
    end
    else
    begin
      WriteLn('  Connection: LOST - attempting reconnect...');
      ConnectAndLogin;
    end;

    Sleep(5000); // Test ogni 5 secondi
  end;
end;

procedure RunInteractiveMode;
var
  Input: string;
begin
  WriteLn('=== INTERACTIVE MODE ===');
  WriteLn('Commands: test, ping, status, diagnostic, quit');
  WriteLn('');

  repeat
    Write('Agent> ');
    ReadLn(Input);
    Input := LowerCase(Trim(Input));

    if Input = 'test' then
    begin
      TestFluxConnection;
    end
    else if Input = 'ping' then
    begin
      if ActionCreator.IsConnected then
      begin
        ActionCreator.ping('');
      end
      else
        WriteLn('Not connected');
    end
    else if Input = 'status' then
    begin
      WriteLn('=== AGENT STATUS ===');
      WriteLn('Agent ID: ' + Config.AgentId);
      WriteLn('Connection: ' + ActionCreator.GetConnectionStatus);
      WriteLn('Connected: ' + BoolToStr(ActionCreator.IsConnected, True));
      //WriteLn('DDP Client: ' + ActionCreator.GetDDPClientStatus);

      var AgentStore := ActionCreator.GetAgentStore;
      if Assigned(AgentStore) then
        WriteLn('Agent Store: Active')
      else
        WriteLn('Agent Store: Not Available');
      WriteLn('===================');
    end
    else if Input = 'diagnostic' then
    begin
      WriteLn('Switching to diagnostic mode...');
      RunDiagnosticMode;
    end
    else if (Input = 'quit') or (Input = 'exit') then
      Break
    else
      WriteLn('Commands: test, ping, status, diagnostic, quit');
  until Input = 'quit';
end;


procedure Cleanup;
begin
  try
    WriteLn('Shutting down agent...');
    if Assigned(ActionCreator) then
    begin
      ActionCreator.Logout;
      ActionCreator := nil;
    end;
    if Assigned(Config) then
    begin
      Config.Free;
      Config := nil;
    end;
    WriteLn('Agent shutdown complete');
  except
    on E: Exception do
      WriteLn('Error during cleanup: ' + E.Message);
  end;
end;

begin
  try
    // Inizializzazione
    InitializeConfig;
    InitializeServices;
    RegisterNetworkFactory;
    InitializeActionCreator;

    // Connessione
    ConnectAndLogin;

    // Verifica connessione
    if not ActionCreator.IsConnected then
    begin
      WriteLn('Failed to connect. Check configuration and server.');
      WriteLn('Press Enter to exit...');
      ReadLn;
      Exit;
    end;

    WriteLn('');
    WriteLn('? Agent is ready and connected!');
    WriteLn('? TReadThread should be active');
    WriteLn('? Handlers should be connected to Flux');
    WriteLn('');

    // Test iniziale automatico
    TestFluxConnection;

    WriteLn('');
    WriteLn('--- Ready for commands ---');

    // Modalità di esecuzione
    case ParamCount of
      0:
        RunInteractiveMode;
      1:
        begin
          var Mode := LowerCase(ParamStr(1));
          if Mode = 'diagnostic' then
            RunDiagnosticMode
          else
            RunInteractiveMode;
        end;
    else
      RunInteractiveMode;
    end;

  except
    on E: Exception do
      WriteLn('Fatal error: ' + E.Message);
  end;

  Cleanup;

  WriteLn('Press Enter to exit...');
  ReadLn;
end.
