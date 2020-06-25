unit dlgProgress;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, OptionClass;

type

  { TProgressReturnCancel }
  TProgressReturnCancel = procedure(retValue: boolean);

  { TProgressDialog }

  TProgressDialog = class(TForm)
    bbt_Cancel    : TBitBtn;
    ico_Progress  : TImage;
    lab_Quelle    : TLabel;
    lab_Funktion  : TLabel;
    pan_Message   : TPanel;
    pan_Buttons   : TPanel;
    procedure bbt_CancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FQuelle       : string;
    FFunktion     : string;
  public
    // the callback procedure in the calling unit
    progCallBack  : TProgressReturnCancel;
    procedure SetzQuelle(AValue: string);
    procedure SetzFunktion(AValue: string);
  end;

var
  ProgressDialog: TProgressDialog;


implementation

{$R *.lfm}

const
  TITEL_ERGAENZUNG : String = ' — Fortschritt';

{ TProgressDialog }

//
//
//
procedure TProgressDialog.FormCreate(Sender: TObject);
begin
  inherited;
  PixelsPerInch := Screen.PixelsPerInch;
  Caption := optclass.ProgTitel + TITEL_ERGAENZUNG;
end;

//
// cancel button was clicked
// we have to send a break message to the calling unit
//
procedure TProgressDialog.bbt_CancelClick(Sender: TObject);
begin
  // set the global cancel to true
  If Assigned(progCallBack) Then progCallBack(True);
  // hide the dialog
  hide;
end;

//
// set the name of the source file to the dialog
//
procedure TProgressDialog.SetzQuelle(AValue: string);
begin
  If (FQuelle <> AValue)
  Then begin
    FQuelle := AValue;
    lab_Quelle.Caption := FQuelle;
  end;
end;

//
// set the name of the function to the dialog
//
procedure TProgressDialog.SetzFunktion(AValue: string);
begin
  If (FFunktion <> AValue)
  Then begin
    FFunktion := AValue;
    lab_Funktion.Caption := FFunktion;
  end;
end;

end.

