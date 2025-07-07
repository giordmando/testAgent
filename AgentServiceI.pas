unit AgentServiceI;

interface
uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.JSON,
  System.Threading;

type
  // Tipi base
  //TChannelDataProc = reference to procedure(const Data: TBytes);

  TOutputProc = reference to procedure(const Line: string);
  TExitCodeProc = reference to procedure(const ExitCode: Integer);
  TCommandProc = reference to procedure(const Command: TJSONObject);
  TSessionEvent = reference to procedure(const SessionId: string);

  ICommChannel = interface;
  TChannelNotifyEvent = reference to procedure(const Sender: ICommChannel);
  TChannelDataProc = reference to procedure(const Sender: ICommChannel; const Data: TBytes);
  // ===== INTERFACCE DI COMUNICAZIONE =====

  // Interfaccia base per comunicazione sincrona request/response
  ICommunicator = interface
    function Communicate(const Data: string): string;
  end;

  // Interfaccia per ricezione comandi (push o pull)
  ICommandReceiver = interface
    // Modalità pull (polling)
    function PollCommand: string;
    // Modalità push (callback)
    procedure StartReceiving(const OnCommand: TCommandProc);
    procedure StopReceiving;
    function IsReceiving: Boolean;
  end;

  // Interfaccia per invio output e risultati
  IOutputSender = interface
    procedure SendOutput(const SessionId, Stream, Data: string);
    procedure SendCommandComplete(const SessionId: string; const ExitCode: Integer);
    procedure SendError(const SessionId, Error: string);
    procedure SendSessionStatus(const SessionId, Status: string);
  end;

  // Interfaccia per registrazione agent
  IAgentRegistrar = interface
    procedure Register(const AgentInfo: TJSONObject);
    procedure UpdateStatus(const Status: string);
    procedure UpdateSessions(const Sessions: TArray<string>);
    procedure UpdateMetrics(const Metrics: TJSONObject);
    procedure Unregister;
  end;

  // ===== INTERFACCE DI SESSIONE =====

  // Interfaccia per shell runner
  IShellRunner = interface
    procedure ExecuteCommand(
      const Command: string;
      const OnOutput: TOutputProc;
      const OnError: TOutputProc;
      const OnExit: TExitCodeProc
    );
    procedure StopExecution;
    function IsRunning: Boolean;
    function GetWorkingDirectory: string;
    procedure SetWorkingDirectory(const Dir: string);
  end;

  // Interfaccia per sessione shell
  IShellSession = interface
    procedure ExecuteCommand(const Command: string);
    procedure StopSession;

    function GetSessionId: string;
    function GetShellType: string;
    function GetCreatedAt: TDateTime;
    function GetLastActivity: TDateTime;
    function IsActive: Boolean;

    function GetOnOutput: TOutputProc;
    procedure SetOnOutput(const Value: TOutputProc);
    function GetOnError: TOutputProc;
    procedure SetOnError(const Value: TOutputProc);
    function GetOnExit: TExitCodeProc;
    procedure SetOnExit(const Value: TExitCodeProc);

    property SessionId: string read GetSessionId;
    property ShellType: string read GetShellType;
    property CreatedAt: TDateTime read GetCreatedAt;
    property LastActivity: TDateTime read GetLastActivity;
    property OnOutput: TOutputProc read GetOnOutput write SetOnOutput;
    property OnError: TOutputProc read GetOnError write SetOnError;
    property OnExit: TExitCodeProc read GetOnExit write SetOnExit;
  end;

  // Interfaccia per gestione sessioni
  ISessionManager = interface
    function CreateSession(const ShellType: string; const SessionId: string = ''): IShellSession;
    function GetSession(const SessionId: string): IShellSession;
    function ListSessions: TArray<string>;
    procedure CloseSession(const SessionId: string);
    procedure CloseAllSessions;
    function CheckLimitSession(const LimitSession: Integer): Boolean;
    function GetActiveSessionCount: Integer;

    function GetOnSessionCreated: TSessionEvent;
    procedure SetOnSessionCreated(const Value: TSessionEvent);
    function GetOnSessionClosed: TSessionEvent;
    procedure SetOnSessionClosed(const Value: TSessionEvent);

    property OnSessionCreated: TSessionEvent read GetOnSessionCreated write SetOnSessionCreated;
    property OnSessionClosed: TSessionEvent read GetOnSessionClosed write SetOnSessionClosed;
  end;

  ISessionPoller = interface
    function PollForSession: string; // polling sincrono
    procedure StartWatching(const OnSession: TProc<string>); // push asincrono
    procedure StopWatching;
  end;

  // ===== INTERFACCE DI SUPPORTO =====

  // Interfaccia per canale di comunicazione (WebSocket, HTTP, etc)
  ICommChannel = interface
    procedure Connect;
    procedure SendData(const Data: TBytes);
    // RIMUOVI: procedure ReceiveData(const Callback: TChannelDataProc);
    function IsConnected: Boolean;
    procedure Disconnect;

    function GetOnConnect: TChannelNotifyEvent;
    procedure SetOnConnect(const Value: TChannelNotifyEvent);
    function GetOnDisconnect: TChannelNotifyEvent;
    procedure SetOnDisconnect(const Value: TChannelNotifyEvent);
    // AGGIUNGI: L'evento per la ricezione dati
    function GetOnDataReceived: TChannelDataProc;
    procedure SetOnDataReceived(const Value: TChannelDataProc);

    property OnConnect: TChannelNotifyEvent read GetOnConnect write SetOnConnect;
    property OnDisconnect: TChannelNotifyEvent read GetOnDisconnect write SetOnDisconnect;
    property OnDataReceived: TChannelDataProc read GetOnDataReceived write SetOnDataReceived; // <-- NUOVA PROPERTY
  end;

  // Factory per shell runner
  IShellRunnerFactory = interface
    function CreateRunner(const ShellType: string): IShellRunner;
  end;

  // Interfaccia crittografia
  ICryptoProvider = interface
    function Encrypt(const Data: TBytes): TBytes;
    function Decrypt(const Data: TBytes): TBytes;
  end;

  // Interfaccia logging
  ILogger = interface
    procedure Info(const Msg: string);
    procedure Warn(const Msg: string);
    procedure Error(const Msg: string);
    procedure Debug(const Msg: string);
    function GetLogs: TArray<string>;
  end;

  // Interfaccia autorizzazione
  IAuthManager = interface
    function IsAuthorized(const SessionId, Command: string): Boolean;
    procedure AddToWhitelist(const CommandPattern: string);
    procedure AddToBlacklist(const CommandPattern: string);
    procedure LoadRules(const Source: string);
  end;

  // Interfaccia tempo
  ITimeProvider = interface
    function Now: TDateTime;
    function NowUTC: TDateTime;
    function NowISO: string;
  end;

  // Interfaccia informazioni ambiente
  IEnvironmentInfo = interface
    function GetHomeDir: string;
    function GetInstallPath: string;
    function GetConfigDir: string;
    function GetTempDir: string;
    function GetHostname: string;
    function GetOSVersion: string;
    function IsWindows: Boolean;
    function IsLinux: Boolean;
  end;

  // Interfaccia message loop
  IMessageLoop = interface
    procedure RunLoop(const OnTick: TProc);
    procedure StopLoop;
    function IsRunning: Boolean;
    procedure EnqueueData(const Data: string);
  end;

  // ===== INTERFACCIA PRINCIPALE =====

  // Interfaccia servizio agent
  IAgentService = interface
    procedure Run;
    procedure Stop;
    function IsRunning: Boolean;
    function GetAgentId: string;
    function GetVersion: string;
    procedure ProcessCommand(const Cmd: TJSONObject);
    procedure HandleOpenSession(const JSONObj: TJSONObject);
    procedure HandleCloseSession(const SessionId: string);
    procedure HandleClearSessions(const JSONObj: TJSONObject);
    procedure HandleExecuteCommand(const JSONObj: TJSONObject);
  end;

implementation

end.
