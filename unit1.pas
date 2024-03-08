unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Menus;

type

  { TForm1 }

  TForm1 = class(TForm)
    AddBtn: TButton;
    AddChildBtn: TButton;
    DelBtn: TButton;
    InsertBtn: TButton;
    MainMenu: TMainMenu;
    Memo1: TMemo;
    MenuItemOptions: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemNew: TMenuItem;
    MenuItemOpen: TMenuItem;
    MenuItemSave: TMenuItem;
    MenuItemSaveAs: TMenuItem;
    OutBtn: TButton;
    InBtn: TButton;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
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
    function ValidSelection: Boolean;
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

function TForm1.ValidSelection: Boolean;
// !! This tests that a valid node selection has been made.
begin
    if TreeView1.Items.Count = 0 then
    begin
        MessageDlg('There are no items in the list!', mtInformation, [mbOk], 0);
        result := false;
    end
    else if TreeView1.Selected = nil then
    begin
        MessageDlg('You must select an item!', mtInformation, [mbOk], 0);
        result := false;
    end
    else
        result := True;
end;

procedure TForm1.AddBtnClick(Sender: TObject);

var
    Node: TTreeNode;
    note: NoteOb;
begin
    note := NoteOb.create;
    note.txt := Memo1.Text;
    Node := TreeView1.Items.AddObject(TreeView1.Selected, 'NewItem', note);
    Node.Selected := True;
    Node.EditText;
    SetFocusedControl(Memo1);
end;

procedure TForm1.AddChildBtnClick(Sender: TObject);
//Add an item indented 1 level in below current item
var
    Node: TTreeNode;
    note: NoteOb;
begin
    if TreeView1.Items.Count = 0 then // if TreeView is empty, just Add item
        AddBtnClick(Sender)
    else if ValidSelection then
    begin // else add child item
        note := NoteOb.create;
        note.txt := Memo1.Text;
        Node := TreeView1.Items.AddChildObject(TreeView1.Selected,
          'NewItem', note);
        // expand to show new node if necessary
        if Node.Level > 0 then
            Node.Parent.Expanded := True;
        Node.Selected := True;
        Node.EditText;
        SetFocusedControl(Memo1);
    end;
end;

procedure TForm1.DelBtnClick(Sender: TObject);
//Delete selected node
var
    index: Integer;
    ok: Boolean;
begin
    ok := True;
    if ValidSelection then
    begin
        if TreeView1.Selected.HasChildren then
            if MessageDlg
              ('There are items beneath the selected item.'#10'Delete them all?',
              mtInformation, [mbYes, mbNo], 0) = mrNo then
                ok := false;
        if ok then
        begin
            // set index to 1 above the node being deleted
            index := TreeView1.Selected.AbsoluteIndex - 1;
            // delete the node
            TreeView1.Selected.Delete;
            // then, if there is still any items in the TreeView,
            // select the item at position 'index'
            if index >= 0 then
                TreeView1.Items[index].Selected := True;
            // and set the focus back to the TreeView
        end; { if ok }
        TreeView1.SetFocus;
    end;
end;

procedure TForm1.InBtnClick(Sender: TObject);
//Indent 1 level
var
    Node: TTreeNode;
    sibindex: Integer;
begin
    if ValidSelection then // make sure it's not empty
    begin
        Node := TreeView1.Selected;
        sibindex := Node.GetPrevSibling.AbsoluteIndex;
        if sibindex = -1 then
            sibindex := Node.GetNextSibling.AbsoluteIndex;
        if sibindex = -1 then
            MessageDlg('There is nothing to indent this item beneath!',
              mtInformation, [mbOk], 0)
        else
            TreeView1.Selected.MoveTo(TreeView1.Items.Item[sibindex],
              naAddChild);
    end;
    TreeView1.SetFocus;
end;

procedure TForm1.InsertBtnClick(Sender: TObject);
// Insert a new item at same level above current item
var
    Node: TTreeNode;
    note: NoteOb;
begin
    if TreeView1.Items.Count = 0 then // if TreeView is empty, just Add item
        AddBtnClick(Sender)
    else if ValidSelection then
    begin
        note := NoteOb.create;
        note.txt := Memo1.Text;
        Node := TreeView1.Items.InsertObject(TreeView1.Selected, 'NewItem', note);
        Node.Selected := True;
        Node.EditText;
        SetFocusedControl(Memo1);
    end;
end;

procedure TForm1.OutBtnClick(Sender: TObject);
//Outdent selected item
var
    Node: TTreeNode;
    parentindex: Integer;
begin
    if ValidSelection then // make sure it's not empty
    begin
        Node := TreeView1.Selected;
        parentindex := Node.Parent.AbsoluteIndex;
        if parentindex = -1 then
            MessageDlg('This item is already fully outdented!', mtInformation,
              [mbOk], 0)
        else
            TreeView1.Selected.MoveTo(TreeView1.Items.Item[parentindex], naAdd);
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
  tv    : TTreeView;
  Node  : TTreeNode;
begin
  tv := Sender as TTreeView;    { Sender is TreeView where the data is being dropped  }
  Node := tv.GetNodeAt(x,y);   	{ x,y are drop coordinates (relative to the Sender)   }
                                {   since Sender is TreeView we can evaluate          }
                                {   a tree at the X,Y coordinates                     }

  if Source = Sender then begin         { drop is happening within a TreeView   }
    if Assigned(tv.Selected) and             {  check if any node has been selected  }
      (Node <> tv.Selected) then            {   and we're dropping to another node  }
    begin
      if Node <> nil then
        tv.Selected.MoveTo(Node, naAddChild) { complete the drop operation, by moving the selectede node }
      else
        tv.Selected.MoveTo(Node, naAdd); { complete the drop operation, by moving in root of a TreeView }
    end;
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
