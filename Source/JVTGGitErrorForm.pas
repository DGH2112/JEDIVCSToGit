(**
  
  This module contains a form class to displays a GIT error message and ask the user what to do.

  @Author  David Hoyle
  @Version 1.0
  @Date    22 Feb 2018
  
**)
Unit JVTGGitErrorForm;

Interface

Uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Buttons;

Type
  (** A form class t0 display a GIT error message. **)
  TfrmGITError = Class(TForm)
    mmoMessage: TMemo;
    btnIgnore: TBitBtn;
    btnAbort: TBitBtn;
  Strict Private
  Strict Protected
  Public
    Class Function Execute(Const strMsg : String) : TModalResult;
  End;

Implementation

{$R *.dfm}

(**

  This method displays the dialogue and shows the message.

  @precon  None.
  @postcon The message is displayed.

  @param   strMsg as a String as a constant
  @return  a TModalResult

**)
Class Function TfrmGITError.Execute(Const strMsg: String): TModalResult;

Var
  F : TfrmGITError;
  
Begin
  F := TfrmGITError.Create(Application.MainForm);
  Try
    F.mmoMessage.Lines.Text := strMsg;
    Result := F.ShowModal;
  Finally
    F.Free;
  End;
End;

End.
