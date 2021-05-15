unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    SaveButton: TButton;
    ImportButton: TButton;
    procedure SaveButtonClick(Sender: TObject);
    procedure ImportButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ImportButtonClick(Sender: TObject);
var myFile : TextFile;
var text : string;
begin
  AssignFile(myFile, 'Test.txt');
  Reset(myFile);

  while not Eof(myFile) do
  begin
    ReadLn(myFile, text);
  end;

  Edit1.Text := text;

  CloseFile(myFile);
end;

procedure TForm1.SaveButtonClick(Sender: TObject);
var myFile : TextFile;
begin
  AssignFile(myFile, 'Test.txt');
  ReWrite(myFile);
  WriteLn(myFile, Edit1.Text);
  CloseFile(myFile);
end;

end.
