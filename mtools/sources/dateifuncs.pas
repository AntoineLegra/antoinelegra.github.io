unit dateifuncs;

{ ---------------------------------------------------------------------------

  dateifuncs -- the file functions copy and delete
  ------------- for Windows and Linux

  Version 1.0 2020 May 07, assembled by Deems

  This unit contains file functions for Windows and Linux,
  calling in your main form with one and same call.

  Under Windows we are using the system call SHFileOperationW --
  it will show automatically -- if there's time enough -- the
  Windows ProgressDialog.

  Under Linux we have to use an self programmed progress dialog
  that sets an boolean to true, if we want to cancel the function.

  For MacOS I can't program such a function because I don't
  have access to these computers.

  For the delete files function under Linux (MacOS) we
  are using the unit t4a_trashcan.pas by Hans Luijten
  (www.tweaking4all.com), Version 1.0, 2020-01-09.
  Thanks for the great idea, Hans!!!

  Usage:

  function filesKopieren(srcList, dstList: TStringList): Boolean;
  function filesLoeschen(srcList: TStringList): Boolean;

  The TStringLists contains all the complete path and file names,
  we want to copy or to delete.

  You're free to use this code in any of your projects, commercial or not.

----------------------------------------------------------------------------- }

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX}BaseUnix,{$ENDIF}
  Classes, SysUtils, Forms
  {$IFDEF MSWINDOWS}
  , ShellApi
  {$ENDIF}

  {$IFDEF LINUX}
  , DateUtils, LCLProc, LazFileUtils, FileUtil, dlgProgress, OptionClass
  , t4a_trashcan
  {$ENDIF}
  ;

  function filesKopieren(srcList, dstList: TStringList): Boolean;
  function filesLoeschen(srcList: TStringList): Boolean;

implementation

{$IFNDEF MSWINDOWS}
// ------------------------------------------------------------------------ //
// the callback procedure for the ProgressDialog
// to cancel copy and delete procedures -- this is needed only for Linux
// ------------------------------------------------------------------------ //
var
  FGlobalCancel: boolean;

procedure SetGlobalCancel(AValue: boolean);
begin
  FGlobalCancel := True;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
//
// the copy file function for Windows
//
function filesKopieren(srcList, dstList: TStringList): Boolean;
var
  R: TSHFILEOPSTRUCTW;
  pSrc, pDst: UnicodeString;
  iRes, i: Integer;
  bRes : Boolean;
begin
  bRes := False;
  try
    iRes := srcList.Count;
    pSrc := '';
    pDst := '';
    // walk through the StringLists
    For i := 0 to iRes-1 Do
    begin
       pSrc := pSrc + UnicodeString(srcList[i] + #0);
       pDst := pDst + UnicodeString(dstList[i] + #0);
    end;
    // fill the Windows TSHFILEOPSTRUCTW structure
    FillChar(R, SizeOf(R), 0);
    R.wFunc := FO_COPY;
    R.pFrom := PWideChar(pSrc + #0);
    R.pTo   := PWideChar(pDst + #0);
    R.fFlags := FOF_ALLOWUNDO OR FOF_MULTIDESTFILES
             OR FOF_NOCONFIRMATION OR FOF_NOCONFIRMMKDIR;
    // and call the Windows SHFileOperationW function
    bRes := (SHFileOperationW(@R) = 0);
  finally
    Result := bRes;
  end;
end;

//
// the delete file function for Windows
//
function filesLoeschen(srcList: TStringList): Boolean;
var
  R: TSHFILEOPSTRUCTW;
  pSrc: UnicodeString;
  iRes, i: Integer;
  bRes : Boolean;
begin
  bRes := False;
  try
    iRes := srcList.Count;
    pSrc := '';
    // walk through the StringList
    For i := 0 to iRes-1 Do
        pSrc := pSrc + UnicodeString(srcList[i] + #0);
    // fill the Windows TSHFILEOPSTRUCTW structure
    FillChar(R, SizeOf(R), 0);
    R.wFunc := FO_DELETE;
    R.pFrom := PWideChar(pSrc + #0);
    R.fFlags := FOF_ALLOWUNDO or FOF_NOCONFIRMATION;
    // and call the Windows SHFileOperationW function
    bRes := (SHFileOperationW(@R) = 0);
  finally
    Result := bRes;
  end;
end;
{$ENDIF}

{$IFDEF LINUX}
//
// the copy file function for Linux
//
// The unit optionclass holds some global properties
// for using in the main and all other forms if needed.
// Here these are a boolean AllowOverwriteWP
// and a String constant with the message at the ProgressDialog
// and another String constant if creation of directory failed.
//
// TODO: Get the remaining String constants from the optclass too.
//
function filesKopieren(srcList, dstList: TStringList): Boolean;
var
  iRes, i : Integer;
  bRes    : Boolean;
  dstPath : String;
  fehler  : String;
  datAttr : stat;
begin
  bRes := False;
  iRes := srcList.Count;
  FGlobalCancel := false;
  try
    ProgressDialog.SetzFunktion(optclass.MesgCopy);
    ProgressDialog.progCallBack := @SetGlobalCancel;
    ProgressDialog.Show;

    // walk through the StringLists
    i := 0;
    While (i < iRes) AND NOT(FGlobalCancel)
    Do begin
       // actualize the file name in the dialog
       ProgressDialog.SetzQuelle(srcList[i]);
       Application.ProcessMessages;

       // get the destination directory
       dstPath  := ExtractFilePath(dstList[i]);
       // create the directory if needed
       If NOT(LazFileUtils.DirPathExists(dstPath)) Then
          If NOT(ForceDirectories(dstPath))
          Then begin
             fehler := optclass.ErrMakeDir + sLineBreak + dstPath;
             raise Exception.Create(fehler);
          end;
       // check first if the user wants to override write protected files
       If optclass.AllowOverwriteWP Then
          // check then if the write bit isn't set
          If (fpaccess(dstList[i], W_OK) <> 0)
          Then begin
             // set the write bit for the user
             fpstat(dstList[i], datAttr);
             datAttr.st_mode := datAttr.st_mode OR S_IWUSR;
             iRes := fpchmod(dstList[i], datAttr.st_mode);
          end;
       // copy one file
       If NOT(CopyFile(srcList[i], dstList[i],
                  [cffPreserveTime, cffOverwriteFile]))
       Then begin
          fehler := 'Fehler beim Kopieren von' + sLineBreak + srcList[i];
          raise Exception.Create(fehler);
       end;
       inc(i);
    end; {while}
    bRes := True;
  finally
    // hide the ProgressDialog
    If NOT(FGlobalCancel) Then ProgressDialog.Hide;
    Result := bRes;
  end;
end;

//
// the delete file function for Linux
//
// The unit optionclass holds some global properties
// for using in the main and all other forms if needed.
// Here these are a boolean AllowOverwriteWP
// and a String with the message at the ProgressDialog.
//
function filesLoeschen(srcList: TStringList): Boolean;
var
  iRes, i: Integer;
  bRes   : Boolean;
  fehler : String;
begin
  bRes := False;
  iRes := srcList.Count;
  i := 0;
  FGlobalCancel := false;
  try
    ProgressDialog.SetzFunktion(optclass.MesgDelete);
    ProgressDialog.progCallBack := @SetGlobalCancel;
    ProgressDialog.Show;
    While (i < iRes) AND NOT(FGlobalCancel)
    Do begin
       // actualize the file name in the dialog and delete the file
       ProgressDialog.SetzQuelle(srcList[i]);
       Application.ProcessMessages;
       If NOT(MoveToTrash(srcList[i]))
       Then begin
          fehler := 'Fehler beim Löschen von' + sLineBreak + srcList[i];
          raise Exception.Create(fehler);
       end;
       inc(i);
    end; {while}
    bRes := True;
  finally
    // hide the ProgressDialog
    If NOT(FGlobalCancel) Then ProgressDialog.Hide;
    Result := bRes;
  end;
end;
{$ENDIF}

end.

