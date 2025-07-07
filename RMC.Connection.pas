unit RMC.Connection;

interface

uses
  DDP.Interfaces;

type
  IAgentConnection = interface
    ['{87654321-4321-4321-4321-210987654321}']
    procedure Connect;
    procedure Disconnect;
    function Login(const AUsername, APassword: string): string; overload;
    procedure Login(const AToken: string); overload;
    function Login(const AUsername, APassword, A2FACode: string): string; overload;
    procedure Logout;
  end;

function GetAgentConnection(const ADDPClient: IDDPClient; const ADDPLogin: IDDPLogin): IAgentConnection;

implementation

type
  TAgentConnection = class(TInterfacedObject, IAgentConnection)
  private
    FDDPClient: IDDPClient;
    FDDPLogin: IDDPLogin;
  protected
    { IAgentConnection implementation }
    procedure Connect;
    procedure Disconnect;
    function Login(const AUsername, APassword: string): string; overload;
    procedure Login(const AToken: string); overload;
    function Login(const AUsername, APassword, A2FACode: string): string; overload;
    procedure Logout;
  public
    constructor Create(const ADDPClient: IDDPClient; const ADDPLogin: IDDPLogin);
  end;

{ TAgentConnection }

constructor TAgentConnection.Create(const ADDPClient: IDDPClient; const ADDPLogin: IDDPLogin);
begin
  Assert(Assigned(ADDPClient));
  Assert(Assigned(ADDPLogin));
  inherited Create;
  FDDPClient := ADDPClient;
  FDDPLogin := ADDPLogin;
end;

procedure TAgentConnection.Connect;
begin
  FDDPClient.Connect;
end;

procedure TAgentConnection.Disconnect;
begin
  FDDPClient.Disconnect;
end;

function TAgentConnection.Login(const AUsername, APassword: string): string;
begin
  Result := FDDPLogin.Login(AUsername, APassword).Token;
end;

procedure TAgentConnection.Login(const AToken: string);
begin
  FDDPLogin.Login(AToken);
end;

function TAgentConnection.Login(const AUsername, APassword, A2FACode: string): string;
begin
  Result := FDDPLogin.Login(AUsername, APassword, A2FACode).Token;
end;

procedure TAgentConnection.Logout;
begin
  FDDPLogin.Logout;
end;

// Factory function
function GetAgentConnection(const ADDPClient: IDDPClient; const ADDPLogin: IDDPLogin): IAgentConnection;
begin
  Result := TAgentConnection.Create(ADDPClient, ADDPLogin);
end;

end.
