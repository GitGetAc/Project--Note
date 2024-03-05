unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    AddBtn: TButton;
    AddChildBtn: TButton;
    TreeView1: TTreeView;
    procedure AddBtnClick(Sender: TObject);
    procedure AddChildBtnClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.AddBtnClick(Sender: TObject);

var
  Node : TTreeNode;

begin
  Node := TreeView1.Items.Add(TreeView1.Selected, 'New Item');
  Node.Selected := True;
  Node.EditText;

end;

procedure TForm1.AddChildBtnClick(Sender: TObject);

var
  Node : TTreeNode;

begin
  Node := TreeView1.Items.AddChild(TreeView1.Selected, 'New Item');
  Node.Selected := True;
  Node.EditText;
end;

end.

