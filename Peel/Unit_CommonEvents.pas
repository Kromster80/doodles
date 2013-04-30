unit Unit_CommonEvents;
interface
uses Classes;

//Common Event types
type
  TEvent = procedure of object;
  TSingleEvent = procedure(aValue: Single) of object;
  TCardinalEvent = procedure(aIndex: Cardinal) of object;
  TNotifyCardinalEvent = procedure(Sender: TObject; aIndex: Cardinal) of object;
  TNotifyIntegerEvent = procedure(Sender: TObject; aIndex: Integer) of object;
  TNotifyStringEvent = procedure(Sender: TObject; const S: string) of object;


implementation


end.
