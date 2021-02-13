unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFactorial = class(TForm)
    Label1: TLabel;
    NumberEdit: TEdit;
    ResultLabel: TLabel;
    procedure NumberEditChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Factorial: TFactorial;

implementation

{$R *.dfm}

Function CalculateFactorial(number : Integer) : Integer;
begin
  if number > 1 then
    Result := number * CalculateFactorial(number - 1)
  else
    Result := 1;
end;

procedure TFactorial.NumberEditChange(Sender: TObject);
begin
  var number : Integer;
  var error: Integer;

  val(NumberEdit.Text, number, error);

  if error <> 0 then
  begin
    ResultLabel.Caption := '''' + NumberEdit.Text + ''' is not an integer';
  end
  else
  begin
    if number >= 0 then
    begin
      ResultLabel.Caption := NumberEdit.Text + '! = ' + CalculateFactorial(number).ToString();
    end
    else
    begin
      ResultLabel.Caption := '''' + NumberEdit.Text + ''' is negativ';
    end;
  end;

end;

end.
