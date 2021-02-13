unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    AddNumbersButton: TButton;
    ResultMemo: TMemo;
    procedure AddNumbersButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.AddNumbersButtonClick(Sender: TObject);
begin
  var addedNumbersOutput : String;
  var sum : Double;
  var numbersCount : Integer;
  var input : String;
  var error : Integer;
  var lastInput : Integer;

  numbersCount := 0;

  repeat
    input := InputBox('Add number', 'Give an integer: ', '0');
    val(input, lastInput, error);

    if error <> 0 then
    begin
      ShowMessage(input + ' is not an integer');
    end
    else
    begin
      if lastInput <> 0 then
      begin
        sum := sum + lastInput;
        numbersCount := numbersCount + 1;
        addedNumbersOutput := addedNumbersOutput + lastInput.ToString() + '; ';
      end;
    end;

  until lastInput = 0;

  addedNumbersOutput := Copy(addedNumbersOutput, 0, Length(addedNumbersOutput) - 2);

  var average : Double;
  average := sum / numbersCount;
  if average >= 0 then
  begin
    ResultMemo.Text := Format('The average (%.2f) is positive. The numbers were: ', [average]);
  end
  else
  begin
    ResultMemo.Text := Format('The average (%.2f) is negative. The numbers were: ', [average]);
  end;

  ResultMemo.Text := ResultMemo.Text + addedNumbersOutput;
end;

end.
