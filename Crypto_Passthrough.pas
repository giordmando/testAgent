unit Crypto_Passthrough;

interface

uses
  AgentServiceI, System.Classes, System.SysUtils;

type
  TCryptoProvider_Passthrough = class(TInterfacedObject, ICryptoProvider)
  public
    function Encrypt(const Data: TBytes): TBytes;
    function Decrypt(const Data: TBytes): TBytes;
  end;

implementation

function TCryptoProvider_Passthrough.Encrypt(const Data: TBytes): TBytes;
begin
  Result := Data; // nessuna cifratura, solo schema di test
end;

function TCryptoProvider_Passthrough.Decrypt(const Data: TBytes): TBytes;
begin
  Result := Data;
end;

end.
