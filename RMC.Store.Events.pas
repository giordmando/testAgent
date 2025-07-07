unit RMC.Store.Events;

interface

uses
  Grijjy.System.Messaging;

type
  { Event fired when AddressBookConnection Store was changed }
  TStoreRMCConnectionChangedMessage = class(TgrMessage);

  { Event fired when AddressBookData Store was changed }
  TStoreRMCDataChangedMessage = class(TgrMessage);

implementation

end.
