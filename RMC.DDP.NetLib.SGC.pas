unit RMC.DDP.NetLib.SGC;

interface

uses
  RMC.ActionCreator,
  DDP.NetLib.SGC,
  sgcWebSocket_Classes,
  System.SysUtils;

type
  TAgentDDPNetLibSGC = class(TDDPNetLibSGC)
  private
    [Weak]
    FActionCreatorAgent: IActionCreatorAgent;
    FAgentId: string;
    procedure InitializeFluxDependencies;
  protected
    procedure OnDisconnect(AConnection: TsgcWSConnection; ACode: Integer); override;
    procedure OnError(AConnection: TsgcWSConnection; const AError: string); override;
    procedure OnException(AConnection: TsgcWSConnection; AException: Exception); override;
  public
    constructor Create(const AURL: string; const AgentId: string);
  end;

implementation

{ TAgentDDPNetLibSGC }

constructor TAgentDDPNetLibSGC.Create(const AURL: string; const AgentId: string);
begin
  inherited Create(AURL);
  FAgentId:= AgentId;
  InitializeFluxDependencies;
end;

procedure TAgentDDPNetLibSGC.InitializeFluxDependencies;
begin
  FActionCreatorAgent := GetActionCreatorAgentWithId(FAgentId);
end;

procedure TAgentDDPNetLibSGC.OnDisconnect(AConnection: TsgcWSConnection; ACode: Integer);
begin
  inherited;
  FActionCreatorAgent.OnDisconnect(ACode);
end;

procedure TAgentDDPNetLibSGC.OnError(AConnection: TsgcWSConnection; const AError: string);
begin
  inherited;
  FActionCreatorAgent.OnError(AError);
end;

procedure TAgentDDPNetLibSGC.OnException(AConnection: TsgcWSConnection; AException: Exception);
begin
  inherited;
  FActionCreatorAgent.OnException(AException.ClassName, AException.Message);
end;

end.
