unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.NumberBox;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    ComboBox1: TComboBox;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    Ora : TTimer;
    procedure LetsGo(Sender:TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

var timerCount : integer;

implementation

{$R *.dfm}

procedure TForm1.LetsGo(Sender:TObject);
begin
  Inc(timerCount);
  Label1.Caption := IntToStr(timerCount);
  Ora.enabled := true;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if Ora.Enabled then
  begin
    Button1.Caption := 'Start';
    Ora.Enabled := false;
    ComboBox1.Enabled := true;
  end
  else
  begin
    Button1.Caption := 'Stop';
    Ora.Interval := StrToInt(ComboBox1.Text);
    Ora.Enabled := true;
    ComboBox1.Enabled := false;
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
var i : integer;
begin
  timerCount := 0;
  Label1.Caption := IntToStr(timerCount);

  i := 0;
  while i < 10000 do
  begin
    Inc(i, 100);
    ComboBox1.Items.Add(IntToStr(i));
  end;

  ComboBox1.Text := ComboBox1.Items[0];

  Ora := TTimer.Create(Self);
  Ora.Interval := StrToInt(ComboBox1.Items[0]);
  Ora.OnTimer := LetsGo;
  Ora.Enabled := false;

end;

end.
