unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    ListBox1: TListBox;
  private
    { Private declarations }
  public
    { Public declarations }
     constructor Create(AOwner:Tcomponent;Valasztek:TStrings);
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

constructor TForm2.Create(AOwner: TComponent; Valasztek:TStrings);
begin
  inherited Create(Owner);
  Listbox1.items.Assign(valasztek);
end;

end.
