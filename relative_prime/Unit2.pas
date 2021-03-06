unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    SecondNumberEdit: TEdit;
    FirstNumberEdit: TEdit;
    CalculateButton: TButton;
    ResultLabel: TLabel;
    procedure CalculateButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.CalculateButtonClick(Sender: TObject);
begin
  var firstNumber : Integer;
  var secondNumber : Integer;
  var temporary : Integer;
  var error : Integer;

  val(FirstNumberEdit.Text, firstNumber, error);
  if error <> 0 then
  begin
    ShowMessage(FirstNumberEdit.Text + ' is not an integer');
  end
  else
  begin
    val(SecondNumberEdit.Text, secondNumber, error);

    if error <> 0 then
    begin
      ShowMessage(SecondNumberEdit.Text + ' is not an integer');
    end
    else
    begin
      while secondNumber <> 0 do
      begin
        if firstNumber < SecondNumber then
        begin
          temporary := firstNumber;
          firstNumber := secondNumber;
          secondNumber := temporary;
        end;

        temporary := secondNumber;
        secondNumber := firstNumber mod secondNumber;
        firstNumber := temporary;
      end;

      if firstNumber = 1 then
      begin
        ResultLabel.Caption := FirstNumberEdit.Text + ' and ' + secondNumberEdit.Text + ' are relative primes';
      end
      else
      begin
        ResultLabel.Caption := FirstNumberEdit.Text + ' and ' + secondNumberEdit.Text + ' are not relative primes';
      end;

    end;

  end;

end;

end.
