program Project2;

uses
  Vcl.Forms,
  Unit2 in 'Unit2.pas' {Factorial};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFactorial, Factorial);
  Application.Run;
end.
