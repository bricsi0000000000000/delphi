unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    ComboBox1: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ComboBox1Select(Sender: TObject);
begin
  MessageDlg(ComboBox1.Text + ' ' + IntToStr(combobox1.ItemIndex), mtInformation, [mbOK], 0);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Combobox1.Items.Add('yo');
  Combobox1.Items.Add('ez');
end;

end.
