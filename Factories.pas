unit Factories;

interface

uses
  AgentServiceI,
  ShellRunner_PersistentCMD,
  System.Threading, System.SysUtils,
  Winapi.Windows, Winapi.Messages, System.SyncObjs,
  System.DateUtils, System.Classes, Exceptions;

type

  TShellRunnerFactory = class(TInterfacedObject, IShellRunnerFactory)
  private
    FEnvInfo: IEnvironmentInfo;
  public
    constructor Create(const envinfo: IEnvironmentInfo);
    function CreateRunner(const ShellType: string): IShellRunner;
  end;

implementation

constructor TShellRunnerFactory.Create(const envinfo: IEnvironmentInfo);
begin
  FEnvInfo := envinfo;
end;

function TShellRunnerFactory.CreateRunner(const ShellType: string): IShellRunner;
begin
  if SameText(ShellType, 'cmd') then
    Result := TShellRunnerPersistentCMD.Create(FEnvInfo)
  else if SameText(ShellType, 'cmd_persistent') then
    Result := TShellRunnerPersistentCMD.Create(FEnvInfo)
  else
    raise EInvalidStrategyType.CreateFmt('Tipo di shell non supportata: %s', [ShellType]);
end;

end.
