unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Menus;

type
  //NoteOb class
  NoteOb = class
    txt : String;
  end;

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
    OpenDialog1: TOpenDialog;
    OutBtn: TButton;
    InBtn: TButton;
    SaveDialog1: TSaveDialog;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    TreeView1: TTreeView;
    procedure AddBtnClick(Sender: TObject);
    procedure AddChildBtnClick(Sender: TObject);
    procedure DelBtnClick(Sender: TObject);
    procedure InBtnClick(Sender: TObject);
    procedure InsertBtnClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemNewClick(Sender: TObject);
    procedure MenuItemOpenClick(Sender: TObject);
    procedure MenuItemSaveClick(Sender: TObject);
    procedure MenuItemSaveAsClick(Sender: TObject);
    procedure OutBtnClick(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure TreeView1Changing(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
    procedure TreeView1Click(Sender: TObject);
    procedure TreeView1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TreeView1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
  private
    function ReadChars(numchars: Integer; fs: TFileStream): string;
    procedure AddNode(LastNode: TTreeNode; nodelevel: Integer; lbl: string; note: NoteOb);
    function ValidSelection: Boolean;
    procedure DoSave(fn: string);
    procedure DoLoad(fn: string);
  public
    function ConfirmFileSave(FileName: string): Boolean;
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

procedure TForm1.DoSave(fn: string);
var
    fs: TFileStream;
    lbl: string[255];
    s: string;
    i, stri, slen, NumOfNodes, nodelevel: Integer;
begin
    fs := TFileStream.Create(fn, fmCreate);
    try // ...finally
        try // ...except
            // WRITE: Num of nodes in TreeView
            NumOfNodes := TreeView1.Items.Count;
            fs.WriteBuffer(NumOfNodes, sizeof(NumOfNodes));
            // WRITE: the labels and data of the nodes
            for i := 0 to TreeView1.Items.Count - 1 do
            begin
                // WRITE: node label
                lbl := TreeView1.Items[i].Text;
                // !! NEED to supply LEVEL too
                fs.WriteBuffer(lbl, sizeof(lbl));

                // WRITE: Node level
                nodelevel := TreeView1.Items[i].Level;
                fs.WriteBuffer(nodelevel, sizeof(nodelevel));

                // assign txt data to String S
                s := NoteOb(TreeView1.Items[i].Data).txt;
                slen := length(s);
                // WRITE: Length of String S
                fs.WriteBuffer(slen, sizeof(slen));
                // WRITE: String S one char at a time.

                for stri := 1 to slen do
                    fs.WriteBuffer(s[stri], sizeof(Char));
            end;
        except
            on Exception do
                ShowMessage('Stream could not be written! ');
        end;
    finally
        fs.Free;
    end;
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

procedure TForm1.MenuItemExitClick(Sender: TObject);
begin
    Close;
end;

procedure TForm1.AddNode(LastNode: TTreeNode; nodelevel: Integer; lbl: string;
  note: NoteOb);
begin
    if (TreeView1.Items.Count = 0) then
        { NewNode := } TreeView1.Items.AddChildObject(nil, lbl, note)
    else if (nodelevel > LastNode.Level) then
        { NewNode:= } TreeView1.Items.AddChildObject(LastNode, lbl, note)
    else
    begin // if this node is 'outdented' (it has a lower nodelevel than the
        // node above it) find the first existing node at its nodelevel
        // and add the NewNode to it.
        while nodelevel < LastNode.Level do
            LastNode := LastNode.Parent;

        { NewNode := } TreeView1.Items.AddObject(LastNode, lbl, note);
    end;
end;

function TForm1.ReadChars(numchars: Integer; fs: TFileStream): String;
// construct a string by reading in chars from a stream
var
    i: Integer;
    c: Char;
    s: String;
begin
    for i := 1 to numchars do
    begin
        fs.Read(c, sizeof(c));
        s := s + c;
    end;
    result := s;
end;

///////////////////////////////////////////////////////////////////////////

procedure TForm1.MenuItemNewClick(Sender: TObject);
begin
    TreeView1.Items.Clear;
    TreeView1.Repaint;
    Memo1.Text := '';
    OpenDialog1.FileName := '*.nb';
    Caption := '[ Untitled ]'
end;

procedure TForm1.MenuItemOpenClick(Sender: TObject);
begin
    with OpenDialog1 do
        if Execute then
            if FileExists(FileName) Then
                DoLoad(FileName)
            else
                MessageDlg('Error: Cannot find the file: ' + FileName, mtInformation, [mbOk], 0);
end;

procedure TForm1.MenuItemSaveClick(Sender: TObject);
begin
    if ((OpenDialog1.FileName = '') or (OpenDialog1.FileName = '*.nb')) then
        MenuItemSaveAsClick(Sender)
    else
        DoSave(OpenDialog1.FileName);
end;

procedure TForm1.MenuItemSaveAsClick(Sender: TObject);
var
    SaveFile: Boolean;
begin
    SaveFile := True;
    with SaveDialog1 do
        if Execute then
        begin
            if FileExists(FileName) then
                SaveFile := ConfirmFileSave(FileName);
            if SaveFile then
            begin
                DoSave(FileName);
                OpenDialog1.FileName := FileName;
                Caption := ExtractFileName(FileName);
            end;
        end;
end;

procedure TForm1.DoLoad(fn: string);
// File Open (reconstruct tree with notes from saved data)
var
    fs: TFileStream;
    i, slen, NumOfNodes, nodelevel: Integer;
    note: NoteOb;
    lbl: String[255];
    s: string;
    LastNode: TTreeNode;
begin
    LastNode := nil;
    fs := TFileStream.Create(fn, fmOpenRead);
    TreeView1.Items.Clear;
    TreeView1.Repaint;

    try // ...finally
        try // ...except
            // READ: Number of Nodes
            fs.Read(NumOfNodes, sizeof(NumOfNodes));
            for i := 0 to NumOfNodes - 1 do
            begin
                // READ: Node Label
                fs.Read(lbl, sizeof(lbl));
                // READ: Node level
                fs.Read(nodelevel, sizeof(nodelevel));
                // READ: Length of String data
                fs.Read(slen, sizeof(slen));
                s := '';
                // READ: and Construct String S from them
                s := ReadChars(slen, fs);
                // CREATE noteob, assign String S to its text field
                note := NoteOb.Create;
                note.txt := s;
                AddNode(LastNode, nodelevel, lbl, note);
                LastNode := TreeView1.Items[TreeView1.Items.Count - 1];
                TreeView1.FullExpand;
            end;
        except
            on E: Exception do
                ShowMessage(' Couldn''t load from stream: ' + E.message);
        end;
    finally
        fs.Free;
    end;
    Caption := ExtractFileName(fn);
end;
///////////////////////////////////////////////////////////////////////////
function TForm1.ConfirmFileSave(FileName: string): Boolean;
begin
    if MessageDlg(FileName + ' already exists. Save anyway?', mtConfirmation, mbYesNoCancel, 0) = mrYes then
        ConfirmFileSave := True
    else
        ConfirmFileSave := false;
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
