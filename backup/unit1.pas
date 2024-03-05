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
    DelBtn: TButton;
    OutBtn: TButton;
    InBtn: TButton;
    TreeView1: TTreeView;
    procedure AddBtnClick(Sender: TObject);
    procedure AddChildBtnClick(Sender: TObject);
    procedure DelBtnClick(Sender: TObject);
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

procedure TForm1.DelBtnClick(Sender: TObject);
//Delete selected node
var
  index : integer;
  ok : Boolean;
begin
  ok := True;
  //Make sure there is something to delete
  if TreeView1.Items.Count = 0 then
     MessageDlg('There are no items in the list!', mtInformation, [mbOk], 0)
  else
  begin
  if TreeView1.Selected.HasChildren then
     if MessageDlg('There are items beneath the selected item.'#10'Delete them all?', mtInformation, [mbYes, mbNo], 0) = mrNo then
        ok := False;
  if ok then
     begin
       //Set index to 1 above the node being deleted
       index  := TreeView1.Selected.AbsoluteIndex - 1;
       //Delete the node
       TreeView1.Selected.Delete;
       //Then, if there is still any items in the TreeView, select the item at position 'index'
       if index >= 0 then
          TreeView1.Items[index].Selected := True;
       //And set the fokus back to TreeView
     end;
     TreeView1.SetFocus;
  end;
end;

end.



