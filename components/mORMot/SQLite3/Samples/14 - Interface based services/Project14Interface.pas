/// some common definitions shared by both client and server side 
unit Project14Interface;

interface

type
  ICalculator = interface(IInvokable)
    ['{9A60C8ED-CEB2-4E09-87D4-4A16F496E5FE}']
    function Add(n1,n2: integer): integer;
  end;

const
  ROOT_NAME = 'root';
  APPLICATION_NAME = 'RestService';

implementation

end.
