unit RMC.DDP.NetLib.Grijjy;

interface

uses
  RMC.ActionCreator,
  DDP.NetLib.Grijjy,
  Grijjy.Socket.Base,
  System.SysUtils;

type
  TRMCDDPNetLibGrijjy = class(TDDPNetLibGrijjy)
  private
    [Weak]
    FActionCreatorAgent: IActionCreatorAgent;
    FClosing: Boolean;
    FAgentId: string;
    procedure InitializeFluxDependencies;
  protected
    procedure HandleDisconnected(const ASender: TObject; const AConnection: IgrConnection); override;
    procedure HandleIoException(const AException: Exception); override;
  public
    constructor Create(const AURL: string; AgentId: string);
    destructor Destroy; override;
  end;

implementation

uses
  System.Classes,
  System.DateUtils;

{ TAgentDDPNetLibGrijjy }

constructor TRMCDDPNetLibGrijjy.Create(const AURL: string; AgentId: string);
begin
  inherited Create('http://localhost:3000/websocket') ;//Create(AURL);
  FAgentId := AgentId;
  InitializeFluxDependencies;
end;

destructor TRMCDDPNetLibGrijjy.Destroy;
begin
  FClosing := True;
  inherited;
end;

procedure TRMCDDPNetLibGrijjy.InitializeFluxDependencies;
begin
  FActionCreatorAgent := GetActionCreatorAgentWithId(FAgentId);
end;

procedure TRMCDDPNetLibGrijjy.HandleDisconnected(const ASender: TObject; const AConnection: IgrConnection);
var
  LActionCreatorAgent: IActionCreatorAgent;
begin
  inherited;
  LActionCreatorAgent := FActionCreatorAgent; { AddRef }

  { If we are destroying then exit }
  if FClosing then
    Exit;

  { The disconnected method could be triggered as the result of either a graceful or
    abrupt disconnect or a connect timeout }
  TThread.CreateAnonymousThread(
    procedure
    var
      Start: TDateTime;
    begin
      Start := Now;
      while Now.SecondsBetween(Start) < 5 do
      begin
        Sleep(100); { Wait }

        { If we are destroying then exit }
        if FClosing then
          Exit;
      end;

      if Assigned(LActionCreatorAgent) then
        LActionCreatorAgent.OnDisconnect(0); { TODO: status code? }
    end).Start;
end;

procedure TRMCDDPNetLibGrijjy.HandleIoException(const AException: Exception);
begin
  inherited;
  FActionCreatorAgent.OnException(AException.ClassName, AException.Message);
end;

end.
