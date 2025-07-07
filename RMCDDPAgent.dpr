program RMCDDPAgent;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Threading,
  System.DateUtils,
  Grijjy.System.Messaging,
  AgentConfig in 'AgentConfig.pas',
  AgentServiceI in 'AgentServiceI.pas',
  AuthManagerImpl in 'AuthManagerImpl.pas',
  Crypto_Passthrough in 'Crypto_Passthrough.pas',
  EnvironmentInfoImpl in 'EnvironmentInfoImpl.pas',
  Exceptions in 'Exceptions.pas',
  Factories in 'Factories.pas',
  Logger_Local in 'Logger_Local.pas',
  RMC.ActionCreator in 'RMC.ActionCreator.pas',
  RMC.Actions.Consts in 'RMC.Actions.Consts.pas',
  RMC.Connection in 'RMC.Connection.pas',
  RMC.Data in 'RMC.Data.pas',
  RMC.DDP.NetLib.Grijjy in 'RMC.DDP.NetLib.Grijjy.pas',
  RMC.DDP.NetLib.SGC in 'RMC.DDP.NetLib.SGC.pas',
  RMC.Store.Agent in 'RMC.Store.Agent.pas',
  RMC.Store.Connection in 'RMC.Store.Connection.pas',
  RMC.Store.Events in 'RMC.Store.Events.pas',
  SessionManager in 'SessionManager.pas',
  ShellRunner_PersistentCMD in 'ShellRunner_PersistentCMD.pas',
  ShellSession in 'ShellSession.pas',
  DDP.NetLib.Factory,
  DDP.Interfaces;

var
  Config: TAgentConfig;

  // Servizi comuni
  Logger: ILogger;
  EnvInfo: IEnvironmentInfo;
  AuthMgr: IAuthManager;

  Crypto: ICryptoProvider;
  SessionMgr: ISessionManager;
  ShellFactory: IShellRunnerFactory;
  TimeProv: ITimeProvider;

  // Servizi di comunicazione
  Registrar: IAgentRegistrar;
  CommandReceiver: ICommandReceiver;
  OutputSender: IOutputSender;


procedure CreateCommonServices;
begin
  // Crea servizi comuni a tutte le modalità
  Logger := TLogger_Local.Create('agent.log');
  Logger.Info('=== Agent RMM Starting ===');

  EnvInfo := CreateEnvironmentInfo;
  AuthMgr := TAuthManager.Create;
  Crypto := TCryptoProvider_Passthrough.Create; // O altra implementazione

  // Configura autorizzazioni
  AuthMgr.AddToWhitelist('dir*');
  AuthMgr.AddToWhitelist('cd*');
  AuthMgr.AddToWhitelist('ping*');
  AuthMgr.AddToWhitelist('echo*');
  AuthMgr.AddToBlacklist('format*');
  AuthMgr.AddToBlacklist('del /s*');

  // Session management
  ShellFactory := TShellRunnerFactory.Create(EnvInfo);
  SessionMgr := TSessionManager.Create(ShellFactory);
end;



procedure ForceLoginCompleted;
var
  ActionCreator: IActionCreatorAgent;
begin
  Writeln('=== FORZANDO LOGIN COMPLETATO ===');

  ActionCreator := GetActionCreatorAgentWithId(Config.AgentId);
  if Assigned(ActionCreator) then
  begin
    // *** ACCESS DIRETTO AL PRIVATE FIELD (Hack temporaneo) ***
    // Nel TActionCreatorAgent, aggiungi un metodo pubblico:
    ActionCreator.ForceMarkLoginCompleted;

    Writeln('Login forzato come completato');

    if ActionCreator.IsLoginCompleted then
      Writeln('Verifica: Login ora risulta completato')
    else
      Writeln('Errore: Login ancora non completato');
  end;
end;


procedure CreateDDPServices;
var
  ActionCreator: IActionCreatorAgent;
  StoreAgent: IStoreAgentData;
begin
  Logger.Info('Modalità DDP (Distributed Data Protocol)');

  try

  // Initialize stores and action creator
 // *** STEP 2: VERIFICA CHE DDP.ACTIONCREATOR SIA COLLEGATO AL DISPATCHER ***
    Writeln('[MAIN] ?? Verificando DDP ActionCreator...');
    var DDPActionCreator := GetActionCreatorAgentWithId(Config.AgentId);
    if not Assigned(DDPActionCreator) then
    begin
      Writeln('[MAIN] ? ERRORE: DDP ActionCreator non disponibile!');
      raise Exception.Create('DDP ActionCreator non inizializzato');
    end;
    Writeln('[MAIN] ? DDP ActionCreator OK');

  var FStoreConnection := GetStoreAgentConnection;


  // Subscribe to store changes
//  GetFluxDispatcher.Register(TStoreRMCConnectionChangedMessage, OnConnectionChanged);//<TStoreAgentConnectionChangedMessage>(OnConnectionChanged);
//  GetFluxDispatcher.Subscribe<TStoreAgentDataChangedMessage>(OnDataChanged);


    // *** STEP 0: ASSICURATI CHE IL DISPATCHER FLUX SIA INIZIALIZZATO ***
    Writeln('[MAIN] ?? Inizializzando Flux Dispatcher...');

    // *** STEP 1: CREA E REGISTRA STORE AGENT AL DISPATCHER ***
    Writeln('[MAIN] ?? Creando Store Agent...');
    StoreAgent := GetStoreAgentData(Config.AgentId);
    if not Assigned(StoreAgent) then
    begin
      Writeln('[MAIN] ? ERRORE: Store Agent non creato!');
      raise Exception.Create('Store Agent non creato');
    end;
    Writeln('[MAIN] ? Store Agent creato e registrato al Dispatcher');


    // *** STEP 4: REGISTRA NETLIB FACTORY (ora il sistema Flux è pronto) ***
    Writeln('[MAIN] ?? Registrando NetLib Factory...');
    TDDPNetLibFactory.Register(
    function: IDDPNetLib
    begin
//      Result := TAgentDDPNetLibSGC.Create(Config.DDPServerUrl, Config.AgentId);
      Result := TRMCDDPNetLibGrijjy.Create(Config.DDPServerUrl, Config.AgentId);
    end
    );
    Writeln('[MAIN] ? NetLib Factory registrata');

    // *** STEP 5: CREA ADAPTER FACTORY E LOGIN ***
    Writeln('[MAIN] ?? Creando DDP Factory e Login...');

    DDPActionCreator.Login(Config.DDPUsername, Config.DDPPassword);
    Writeln('[MAIN] ? Login completato');

    Writeln('[MAIN] Sistema DDP completamente configurato');
    Writeln('[MAIN] Flusso: NetLib ? DDPClient ? DDP.ActionCreator ? Dispatcher ? Store.Agent ? ActionCreator.Agent');

  except
    on E: Exception do
    begin
      Logger.Error('Errore creazione servizi DDP: ' + E.Message);
      Writeln('[MAIN] Errore DDP: ' + E.Message);
      raise;
    end;
  end;
end;



procedure LoadConfiguration;
begin
  Config := TAgentConfig.Create;

  // Carica configurazione
  if FileExists('agent.conf.json') then
  begin
    Config.LoadFromFile('agent.conf.json');
    Writeln('Configurazione caricata da agent.conf.json');
  end
  else
  begin
    // Usa valori di default (ora DDP per default)
    Writeln('File configurazione non trovato, uso valori di default');
    Config.SaveToFile('agent.conf.json');
  end;

  // Valida configurazione
  var ValidationError := Config.Validate;
  if not ValidationError.IsEmpty then
  begin
    Writeln('Errore configurazione: ' + ValidationError);
    raise Exception.Create('Configurazione non valida: ' + ValidationError);
  end;

  Writeln('Agent ID: ' + Config.AgentId);
  Writeln('Modalità: ' + Config.GetChannelTypeString);
  Writeln('Shell Type: ' + Config.ShellType);
  Writeln('Session Limit: ' + IntToStr(Config.SessionLimit));

end;

procedure ShowBanner;
begin
  Writeln('================================================');
  Writeln('  Remote Command Line Agent v2.0');
  Writeln('  SOLID Architecture Edition');
  Writeln('================================================');
  Writeln;
end;


procedure Cleanup;
begin
  try

    // Cleanup comune
    if Assigned(Config) then
      Config.Free;

  except
    on E: Exception do
      Writeln('Errore durante cleanup: ' + E.Message);
  end;
end;


begin
  try
    ShowBanner;

    // 1. Carica configurazione
    LoadConfiguration;

    // 2. Crea servizi comuni
    CreateCommonServices;

    CreateDDPServices;

    Writeln(Format('Thread ID AGENTSERVICE: %d', [TThread.CurrentThread.ThreadID]));
    // 5. Avvia agent
    //RunAgent;

    Logger.Info('=== Agent RMM Terminated ===');
    Writeln('Agent terminato correttamente.');

  except
    on E: Exception do
    begin
      Writeln('Errore critico: ', E.ClassName, ' - ', E.Message);
      ExitCode := 1;
    end;
  end;

  // Cleanup
  Cleanup;

  {$IFDEF DEBUG}
  Writeln;
  Writeln('Premi INVIO per chiudere...');
  Readln;
  {$ENDIF}
end.
