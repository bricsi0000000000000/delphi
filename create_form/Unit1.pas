unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Unit2;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var otherForm : TForm2;
var items : TStringList;
begin
  items := TStringList.Create;
  items.Add('elso');
  items.Add('masodik');
  items.Add('harmadik');

  otherForm:=TForm2.Create(Self, items);
  if otherForm.ShowModal=mrOk then
  begin

  end;
  items.Free;
  otherForm.free;
end;

end.
