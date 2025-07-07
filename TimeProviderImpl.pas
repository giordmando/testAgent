unit TimeProviderImpl;

interface

uses
  AgentServiceI, System.SysUtils, System.DateUtils;

type
  // Questa classe fornisce implementazioni concrete per l'interfaccia ITimeProvider.
  TTimeProvider = class(TInterfacedObject, ITimeProvider)
  public
    // Implementazione dei metodi richiesti dall'interfaccia.
    function Now: TDateTime;
    function NowUTC: TDateTime;
    function NowISO: string;
  end;

implementation

{ TTimeProvider }

// Ritorna la data e l'ora correnti del sistema locale.
function TTimeProvider.Now: TDateTime;
begin
  Result := System.SysUtils.Now;
end;

// Ritorna la data e l'ora correnti in formato Coordinated Universal Time (UTC).
function TTimeProvider.NowUTC: TDateTime;
begin
  // TTimeZone.Local.ToUniversalTime converte un TDateTime locale in UTC.
  // System.SysUtils.Now restituisce l'ora locale.
  Result := TTimeZone.Local.ToUniversalTime(System.SysUtils.Now);
end;

// Ritorna la data e l'ora correnti in formato stringa ISO 8601 UTC.
function TTimeProvider.NowISO: string;
begin
  // Esempio di output: "2025-06-12T14:23:00Z"
  // Il secondo parametro 'True' in DateToISO8601 aggiunge l'indicatore 'Z' per UTC.
  // Nota: È più corretto e diretto usare System.DateUtils.NowUTC invece di ricalcolarlo.
  Result := DateToISO8601(NowUTC, True);
end;

end.
