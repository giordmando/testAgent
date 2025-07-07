unit RMCAgent.DDP.NetLib.Factory;

interface

uses
  DDP.Interfaces;

type
  TAgentNetLibType = (Grijjy, SGC);

function CreateAgentDDPNetLib(const AURL: string; const ANetLibType: TAgentNetLibType = TAgentNetLibType.Grijjy): IDDPNetLib;

implementation

uses
  Agent.DDP.NetLib.Grijjy,
  Agent.DDP.NetLib.SGC;

function CreateAgentDDPNetLib(const AURL: string; const ANetLibType: TAgentNetLibType): IDDPNetLib;
begin
  case ANetLibType of
    TAgentNetLibType.Grijjy:
      Result := TAgentDDPNetLibGrijjy.Create(AURL);
    TAgentNetLibType.SGC:
      Result := TAgentDDPNetLibSGC.Create(AURL);
  else
    raise Exception.Create('Unknown NetLib type');
  end;
end;

end.
