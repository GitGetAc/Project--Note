unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  Menus;

type

  { TForm1 }

  TForm1 = class(TForm)
    AddBtn: TButton;
    AddChildBtn: TButton;
    DelBtn: TButton;
    InsertBtn: TButton;
    Memo1: TMemo;
    OutBtn: TButton;
    InBtn: TButton;
    TreeView1: TTreeView;
    procedure AddBtnClick(Sender: TObject);
    procedure AddChildBtnClick(Sender: TObject);
    procedure DelBtnClick(Sender: TObject);
    procedure InBtnClick(Sender: TObject);
    procedure InsertBtnClick(Sender: TObject);
    procedure OutBtnClick(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure TreeView1Changing(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
    procedure TreeView1Click(Sender: TObject);
    procedure TreeView1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TreeView1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
  private

  public

  end;

//NoteOb class
type
  NoteOb = class
    txt : String;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.AddBtnClick(Sender: TObject);

var
  Node : TTreeNode;
  note : NoteOb;
begin
  note := NoteOb.Create;
  note.txt := Memo1.Text;
  Node := TreeView1.Items.AddObject(TreeView1.Selected, 'NewItem', note);
  Node.Selected := True;
  Node.EditText;
end;

procedure TForm1.AddChildBtnClick(Sender: TObject);
//Add an item indented 1 level in below current item
var
  Node : TTreeNode;
  note : NoteOb;
begin
  if Treeview1.Items.Count = 0 then //If treeview is empty, just Add item
     AddBtnClick(Sender)
  else
    begin   //Save any edit-changes in current node
      if TreeView1.Selected.EditText then
         Treeview1.Selected.EndEdit(false);
      //Then add a child
      note := NoteOb.Create;
      note.txt := Memo1.Text;
      Node := TreeView1.Items.AddChildObject(TreeView1.Selected, 'NewItem', note);
      //Expand to show new node if necessary
      if Node.Level > 0 then
         Node.Parent.Expanded := True;
      Node.Selected := True;
      Node.EditText;
    end;
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

procedure TForm1.InBtnClick(Sender: TObject);
//Indent 1 level
var
  Node : TTreeNode;
  sibIndex : Integer;
begin
  if not (Treeview1.Items.Count = 0) then //Make sure it is not empty
     begin
       Node := TreeView1.Selected;
       sibIndex := Node.GetPrevSibling.AbsoluteIndex;
       if sibIndex = -1 then
          sibIndex := Node.GetNextSibling.AbsoluteIndex;
       if sibIndex = -1 then
          MessageDlg('There is nothing to indent this item beneath!', mtInformation, [mbOk], 0)
       else
         TreeView1.Selected.MoveTo(TreeView1.Items.Item[sibIndex], naAddChild);
     end;
  Treeview1.SetFocus;
end;

procedure TForm1.InsertBtnClick(Sender: TObject);
// insert a new item at same level above current item
var
  Node : TTreeNode;
begin
  Node := TreeView1.Items.Insert(TreeView1.Selected, 'NewItem');
  Node.Selected := True;
  Node.EditText;
end;

procedure TForm1.OutBtnClick(Sender: TObject);
//Outdent selected item
var
  Node : TTreeNode;
  parentIndex : Integer;
begin
  if not (TreeView1.Items.Count = 0) then //Make sure it is not empty
     begin
       Node := TreeView1.Selected;
       parentIndex := Node.Parent.AbsoluteIndex;
       if parentIndex = -1 then //!This does not run (Acces violation instead)
          MessageDlg('This item is already fully outdented!', mtInformation, [mbOk], 0)
       else
         TreeView1.Selected.MoveTo(TreeView1.Items.Item[parentIndex], naAdd);
     end;
     TreeView1.SetFocus;
end;

procedure TForm1.TreeView1Click(Sender: TObject);
begin
  if TreeView1.Selected <> nil then
     Memo1.Text := NoteOb(TreeView1.Selected.Data).txt;
end;

procedure TForm1.TreeView1DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Node : TTreeNode;
  Attachmode : TNodeAttachMode;
  HT : THitTests;
begin
  if TreeView1.Selected = nil then Exit;
  HT := TreeView1.GetHitTestInfoAt(X, Y);
  Node := TreeView1.GetNodeAt(X, Y);
  if  (HT - [htOnItem, htOnIcon, htNowhere, htOnIndent] <> HT) then
     begin
       if (htOnItem in HT) or (htOnIcon in HT) then
          AttachMode := naAddChild
       else if htNowhere in HT then
          AttachMode := naAdd
       else if htOnIndent in HT then
          AttachMode := naInsert;
       TreeView1.Selected.MoveTo(Node, AttachMode);
     end;
end;

procedure TForm1.TreeView1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
//Accept item that is been dragged
begin
  Accept := True;
end;
//Load text on note load
procedure TForm1.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  if Treeview1.Selected <> nil then Memo1.Text := NoteOb(Treeview1.Selected.Data).txt;
end;

//Saving note before opening a new node
procedure TForm1.TreeView1Changing(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
begin
  if Treeview1.Selected <> nil then NoteOb(Treeview1.Selected.Data).txt := Memo1.Text;
end;
end.



