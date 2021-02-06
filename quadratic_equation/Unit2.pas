unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    aEdit: TEdit;
    Label1: TLabel;
    bEdit: TEdit;
    Label2: TLabel;
    cEdit: TEdit;
    Label3: TLabel;
    x1Label: TLabel;
    Button1: TButton;
    x2Label: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
  var
    a: Double;
  var
    b: Double;
  var
    c: Double;
  var
    discriminant: Double;
  var
    inputGood: Boolean;
  var
    x1: Double;
  var
    x2: Double;

  inputGood := true;

  if Length(aEdit.Text) = 0 then
  begin
    inputGood := false;
    ShowMessage('You have to give a value for ''a''!');
  end
  else if not TryStrToFloat(aEdit.Text, a) then
  begin
    inputGood := false;
    ShowMessage('Wrong value (' + aEdit.Text + ') for ''a''');
  end;

  if Length(bEdit.Text) = 0 then
  begin
    inputGood := false;
    ShowMessage('You have to give a value for ''b''!');
  end
  else if not TryStrToFloat(bEdit.Text, b) then
  begin
    inputGood := false;
    ShowMessage('Wrong value (' + bEdit.Text + ') for ''b''');
  end;

  if Length(cEdit.Text) = 0 then
  begin
    inputGood := false;
    ShowMessage('You have to give a value for ''c''!');
  end
  else if not TryStrToFloat(cEdit.Text, c) then
  begin
    inputGood := false;
    ShowMessage('Wrong value (' + cEdit.Text + ') for ''c''');
  end;

  if inputGood then
  begin
    discriminant := b * b - 4 * a * c;

    if discriminant = 0 then
    begin
      x1 := -b / 2 * a;

      x1Label.Caption := Format('x1= %.4f', [x1]);
      x2Label.Caption := 'x2= -';
    end
    else if discriminant < 0 then
    begin
      ShowMessage('The result has complex root');
    end
    else
    begin
      x1 := (-b + Sqrt(discriminant)) / 2 * a;
      x2 := (-b - Sqrt(discriminant)) / 2 * a;

      x1Label.Caption := Format('x1= %.4f', [x1]);
      x2Label.Caption := Format('x2= %.4f', [x2]);
    end;

  end;

end;

end.
