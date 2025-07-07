// Test per identificare esattamente il problema

program RMCDDPAgent;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  RMC.DDP.NetLib.SGC,
  DDP.Interfaces,
  DDP.Factories,
  DDP.RequestGenerator,
  Grijjy.Data.Bson,
  System.Threading,
  AgentConfig,
  RMC.Connection,
  RMC.Data,
  RMC.Store.Agent,
  RMC.Store.Connection,
  RMC.ActionCreator,
  DDP.Actions.Consts,
  Factories,           // AGGIUNTO per ShellRunnerFactory
  EnvironmentInfoImpl,
  AgentServiceI;

procedure TestDDPFlow;
var
  DDPNetLib: IDDPNetLib;
  DDPClient: IDDPClient;
  DDPRequestGenerator: IDDPRequestGenerator;
  MessageCount: Integer;
begin
  WriteLn('=== DDP FLOW DIAGNOSTIC ===');

  MessageCount := 0;

  try
    // 1. Test raw WebSocket communication
    WriteLn('1. Testing raw WebSocket...');
    DDPNetLib := TAgentDDPNetLibSGC.Create('ws://localhost:3000/websocket', 'flow_test');

    if DDPNetLib.Connect then
    begin
      WriteLn('   ? WebSocket connected');

      // Send raw DDP connect message
      DDPNetLib.SendData('{"msg":"connect","version":"1","support":["1"]}');
      WriteLn('   ? Sent DDP connect');

      // Wait for response
      Sleep(2000);

      var Response := DDPNetLib.ReceiveData;
      while Response <> '' do
      begin
        Inc(MessageCount);
        WriteLn(Format('   ? Raw message %d: %s', [MessageCount, Response.Substring(0, 100)]));
        Response := DDPNetLib.ReceiveData;
      end;

      WriteLn(Format('   Raw WebSocket: %d messages received', [MessageCount]));
    end
    else
    begin
      WriteLn('   ? WebSocket connection failed');
      Exit;
    end;

    // 2. Test DDP Client processing
    WriteLn('');
    WriteLn('2. Testing DDP Client...');

    DDPRequestGenerator := TDDPRequestGenerator.Create;
    DDPClient := GetDDPClient(DDPNetLib, DDPRequestGenerator);

    try
      // This should handle the DDP protocol
      var SessionId := DDPClient.Connect;
      WriteLn(Format('   ? DDP connected with session: %s', [SessionId]));

      // Test method call
      var PingResult := DDPClient.Method('test.ping', TgrBsonArray.Create);
      WriteLn(Format('   ? Method call result: %s', [PingResult.AsString]));

    except
      on E: Exception do
        WriteLn('   ? DDP Client error: ' + E.Message);
    end;

    // 3. Test subscription
    WriteLn('');
    WriteLn('3. Testing DDP Subscription...');

    try
      var SubId := DDPClient.Subscribe('agent.commands',
        TgrBsonArray.Create([
          TgrBsonDocument.Create.Add('agent_id', 'flow_test')
        ]));
      WriteLn(Format('   ? Subscribed with ID: %s', [SubId]));

      // Wait for subscription data
      Sleep(3000);

      // Check for more messages
      var SubResponse := DDPNetLib.ReceiveData;
      var SubMessageCount := 0;
      while SubResponse <> '' do
      begin
        Inc(SubMessageCount);
        WriteLn(Format('   ? Subscription message %d: %s', [SubMessageCount, SubResponse.Substring(0, 100)]));
        SubResponse := DDPNetLib.ReceiveData;
      end;

      WriteLn(Format('   Subscription: %d messages received', [SubMessageCount]));

    except
      on E: Exception do
        WriteLn('   ? Subscription error: ' + E.Message);
    end;

    DDPNetLib.Disconnect;

  except
    on E: Exception do
      WriteLn('General error: ' + E.Message);
  end;

  WriteLn('');
  WriteLn('=== DDP FLOW DIAGNOSTIC COMPLETE ===');
end;

var
  ProcessingTask: ITask;

 procedure StartDDPProcessing;
begin
  WriteLn('[Fix] Starting DDP processing task...');

  ProcessingTask := TTask.Run(
    procedure
    begin
      while True do
      begin
        Sleep(0); // Yield CPU to other threads
        Sleep(50); // 50ms intervals
      end;
    end);

  WriteLn('[Fix] DDP processing task started');
end;

procedure StopDDPProcessing;
begin
  if Assigned(ProcessingTask) then
  begin
    WriteLn('[Fix] Stopping DDP processing task...');
    ProcessingTask.Cancel;
    ProcessingTask := nil;
  end;
end;
 procedure RunAgentFixed;
var
  Input: string;
begin
  WriteLn('=== RMC DDP Agent Running (FIXED) ===');
  WriteLn('The agent is now listening for commands from the server.');
  WriteLn('Commands will be executed in real-time.');
  WriteLn('');

  repeat
    Write('> ');
    ReadLn(Input);

    // *** AGGIUNGI SOLO QUESTA LINEA DOPO OGNI ReadLn ***
    Sleep(0); // Force thread context switch

    Input := LowerCase(Trim(Input));

    if Input = 'ping' then
    begin
      // ... tuo codice esistente ...
    end
    else if Input = 'test' then
    begin
      // ... tuo codice esistente ...

      // *** AGGIUNGI ANCHE QUI ***
      WriteLn('Test data inserted. Processing...');
      Sleep(100); // Give time for DDP processing
    end
    else if Input = 'status' then
    begin
      // ... tuo codice esistente ...
    end
    else if Input = 'quit' then
      Break;

  until Input = 'quit';
end;
 var
  Config: TAgentConfig;
  DDPNetLib: IDDPNetLib;
  DDPClient: IDDPClient;
  DDPLogin: IDDPLogin;
  DDPRequestGenerator: IDDPRequestGenerator;
  AgentData: IAgentData;
  AgentStore: IStoreAgent;
  ConnectionStore: IStoreAgentConnection;
  AgentConnection: IAgentConnection;
   // AGGIUNTO: Servizi per esecuzione comandi
  EnvironmentInfo: IEnvironmentInfo;
  ShellRunnerFactory: IShellRunnerFactory;

  procedure InitializeServices;
begin
  WriteLn('Initializing agent services...');

  // Crea i servizi per l'esecuzione dei comandi
  EnvironmentInfo := CreateEnvironmentInfo;
  ShellRunnerFactory := TShellRunnerFactory.Create(EnvironmentInfo);

  // Configura i servizi nello store agent
  AgentStore.SetServices(AgentData, ShellRunnerFactory);

  WriteLn('Agent services initialized successfully');
end;

procedure InitializeAgent;

begin
  try
    WriteLn('Initializing RMC DDP Agent...');

    Config := TAgentConfig.Create;
    Config.LoadFromFile('agent.conf.json');

    WriteLn('Agent ID: ' + Config.AgentId);
    WriteLn('Server URL: ' + Config.DDPServerUrl);

    // Inizializza gli store Flux
    AgentStore := GetStoreAgent(Config.AgentId);
    ConnectionStore := GetStoreAgentConnection;

    // Crea i componenti DDP
    DDPRequestGenerator := TDDPRequestGenerator.Create;
    DDPNetLib := TAgentDDPNetLibSGC.Create(Config.DDPServerUrl, Config.AgentId);
    DDPClient := GetDDPClient(DDPNetLib, DDPRequestGenerator);
    DDPLogin := GetDDPLogin(DDPClient);

    AgentConnection := GetAgentConnection(DDPClient, DDPLogin);
    AgentData := GetAgentData(DDPClient);

    // Inizializza i servizi per l'esecuzione dei comandi
    InitializeServices;

    WriteLn('Agent initialized successfully');

  except
    on E: Exception do
    begin
      WriteLn('Error initializing agent: ' + E.Message);
      raise;
    end;
  end;
end;


procedure StartSubscriptions;
begin
  try
    WriteLn('Starting DDP subscriptions...');
    AgentData.StartSubscriptions(Config.AgentId);
    WriteLn('Subscriptions started successfully');

    // Aspetta un po' per assicurarsi che le sottoscrizioni siano attive
    Sleep(3000);

  except
    on E: Exception do
      WriteLn('Error starting subscriptions: ' + E.Message);
  end;
end;

begin
  //TestDDPFlow;
  InitializeAgent;
  StartSubscriptions;

    // *** AGGIUNGI QUESTA LINEA ***
    StartDDPProcessing;

    // ... resto del codice esistente ...

     RunAgentFixed;

    // *** AGGIUNGI QUESTA LINEA ***
    StopDDPProcessing;

  WriteLn('Press Enter to exit...');
  ReadLn;
  Sleep(0);
end.
