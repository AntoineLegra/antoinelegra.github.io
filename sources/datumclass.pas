unit datumclass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

// ------------------------------------------------------------------------- //
// TDatumClass, TFeiertag und TWochentag                                     //
// Developed in 2016 by Meister                                              //
// Version: 1.0 vom 2016-09-07                                               //
// Version: 1.1 vom 2022-07-05                                               //
// ------------------------------------------------------------------------- //
// Die eigene Klasse für Datumsberechnungen.                                 //
// Berechnung der beweglichen Feiertage wie Ostern, Beginn und               //
// Ende der Sommerzeit.                                                      //
// Die Berechnungen gelten in Deutschland erst ab dem 15.10.1582, der        //
// Einführung des Gregorianischen Kalenders. Dieser wurde aber in vielen     //
// Ländern erst viel später eingeführt. -- Das umfangreiche Programmpaket    //
// GNU gcal (https://www.gnu.org/software/gcal/) z.B. berücksichtigt die     //
// länderabhängigen Reformen und bietet so auch eine durchgängige Datums-    //
// Berechnung ab 1 n. Chr.                                                   //
// ------------------------------------------------------------------------- //

type
  // ------------------------------------------------- Die Feiertage in Berlin
  TFeiertag =  (TNeujahr,              // die festen
                THeiligeDreiKings,
                TValentinstag,
                TFrauentag,
                TErsterMai,
                TMariaeHimmelfahrt,
                TSchutzengelfest,
                TTagDerDtEinheit,
                TReformationstag,
                TAllerheiligen,
                TNikolaus,
                TMariaeEmpfaengnis,
                THeiligAbend,
                TErsterWeihnacht,
                TZweiterWeihnacht,
                TSilvester,
                TRosenmontag,          // die von Ostern abhängigen
                TAschermittwoch,
                TGruendonnerstag,
                TKarfreitag,
                TOstersonntag,
                TOstermontag,
                THimmelfahrt,
                TPfingstsonntag,
                TPfingstmontag,
                TFronleichnam,
                TVierterAdvent,        // die von Weihnachten abhängigen
                TDritterAdvent,
                TZweiterAdvent,
                TErsterAdvent,
                TVolkstrauertag,
                TBussUndBettag,
                TTotensonntag,
                TMuttertag,            // weitere besondere Tage
                TSommerzeit,
                TStandardzeit);
  // ---------------------------------------------------------- Die Wochentage
  TWochentag = (TSonntag,
                TMontag,
                TDienstag,
                TMittwoch,
                TDonnerstag,
                TFreitag,
                TSamstag);
  // -------------------------------------------------- Die Datumsberechnungen
  TDatumClass = Class
    private

    public
      // ----------------------------------------------- allgemeine Funktionen
      function FixFeiertag(fTag: TFeiertag): String;     // Bezeichner holen
      function VarFeiertag(fTag: TFeiertag): String;     // Bezeichner holen
      function Schaltjahr(jahr: Integer): Integer;
      function Tagesnummer(jahr, mon, dag: Integer): Integer;
      function WochentagAusTagNum(jahr, tnum: Integer): Integer;
      function MonatAusTagNum(jahr, tnum: Integer): Integer;
      function TagAusTagNum(jahr, tnum: Integer): Integer;
      function addWochentag(jahr, tnum: Integer; wTag: TWochentag): Integer;
      function subWochentag(jahr, tnum: Integer; wTag: TWochentag): Integer;
      function GregDateToJulianDay(kal: TDateTime): Longint;
      function JulianDayToGregDate(julTag: Longint): TDateTime;
      function TageImMonat(jahr, mon: Integer): Integer;
      function Wochennummer(kal: TDateTime): Integer;
      function Ostersonntag(jahr: Integer): Integer;
      // --------------------------------------------- die Feiertage berechnen
      function OsterAbhaengig(jahr: Integer; fTag: TFeiertag): TDateTime;
      function XmasAbhaengig(jahr: Integer; fTag: TFeiertag): TDateTime;
      function FesteUnabhaeng(jahr: Integer; fTag: TFeiertag): TDateTime;
      // ----------------------------------------------- andere besondere Tage
      function Muttertag(jahr: Integer): TDateTime;
      function Sommerzeit(jahr: Integer; fTag: TFeiertag): TDateTime;
      function calcFeiertag(jahr: Integer; fTag: TFeiertag): TDateTime;
    published

  end;

var
  datclass: TDatumClass;

implementation

ResourceString
  rsUnbekannterF = 'Unbekannter Feiertag.';
  rs0101Feiertag = '01-01, Feiertag: Neujahr';
  rs0106HeiligeD = '01-06, Heilige Drei Könige (nicht Berlin)';
  rs0214Valentin = '02-14, Valentinstag';
  rs0308Internat = '03-08, Internationaler Frauentag';
  rs0501Feiertag = '05-01, Feiertag: Erster Mai';
  rs0815MariaeHi = '08-15, Mariae Himmelfahrt (nicht Berlin)';
  rs1002TagDesSc = '10-02, Tag des Schutzengels.';
  rs1003Feiertag = '10-03, Feiertag: Tag der Deutschen Einheit';
  rs1031Reformat = '10-31, Reformationstag (nicht Berlin)';
  rs1101Allerhei = '11-01, Allerheiligen (nicht Berlin)';
  rs1206Nikolaus = '12-06, Nikolaustag';
  rs1208MariaeEm = '12-08, Mariae Empfängnis (nicht Berlin)';
  rs1224Heiligab = '12-24, Heiligabend';
  rs1225Feiertag = '12-25, Feiertag: Erster Weihnachtstag';
  rs1226Feiertag = '12-26, Feiertag: Zweiter Weihnachtstag';
  rs1231Silveste = '12-31, Silvester';
  rsRosenmontagN = 'Rosenmontag (nicht Berlin)';
  rsAschermittwo = 'Aschermittwoch (nicht Berlin)';
  rsGrNdonnersta = 'Gründonnerstag (nicht Berlin)';
  rsFeiertagKarf = 'Feiertag: Karfreitag';
  rsFeiertagOste = 'Feiertag: Ostersonntag';
  rsFeiertagOste2 = 'Feiertag: Ostermontag';
  rsFeiertagHimm = 'Feiertag: Himmelfahrt';
  rsFeiertagPfin = 'Feiertag: Pfingstsonntag';
  rsFeiertagPfin2 = 'Feiertag: Pfingstmontag';
  rsFronleichnam = 'Fronleichnam (nicht Berlin)';
  rsFeiertagVier = 'Feiertag: Vierter Advent';
  rsFeiertagDrit = 'Feiertag: Dritter Advent';
  rsFeiertagZwei = 'Feiertag: Zweiter Advent';
  rsFeiertagErst = 'Feiertag: Erster Advent';
  rsVolkstrauert = 'Volkstrauertag';
  rsBuUndBettagN = 'Buß- und Bettag (nur Sachsen)';
  rsFeiertagTote = 'Feiertag: Totensonntag';
  rsMuttertag = 'Muttertag';
  rsBeginnDerSom = 'Beginn der Sommerzeit (Uhren vorstellen)';
  rsEndeDerSomme = 'Ende der Sommerzeit (Uhren zurückstellen)';

const
  ERR_UNKNOWN_FTAG: String = rsUnbekannterF;
  FStrFeiertag: Array[0..35] of String = (
            rs0101Feiertag,
            rs0106HeiligeD,
            rs0214Valentin,
            rs0308Internat,
            rs0501Feiertag,
            rs0815MariaeHi,
            rs1002TagDesSc,
            rs1003Feiertag,
            rs1031Reformat,
            rs1101Allerhei,
            rs1206Nikolaus,
            rs1208MariaeEm,
            rs1224Heiligab,
            rs1225Feiertag,
            rs1226Feiertag,
            rs1231Silveste,
            rsRosenmontagN,
            rsAschermittwo,
            rsGrNdonnersta,
            rsFeiertagKarf,
            rsFeiertagOste,
            rsFeiertagOste2,
            rsFeiertagHimm,
            rsFeiertagPfin,
            rsFeiertagPfin2,
            rsFronleichnam,
            rsFeiertagVier,
            rsFeiertagDrit,
            rsFeiertagZwei,
            rsFeiertagErst,
            rsVolkstrauert,
            rsBuUndBettagN,
            rsFeiertagTote,
            rsMuttertag,
            rsBeginnDerSom,
            rsEndeDerSomme);

//
// Bezeichnung des jeweiligen festen Feiertages holen
//
function TDatumClass.FixFeiertag(fTag: TFeiertag): String;
begin
  if (fTag >= TNeujahr) AND (fTag <= TSilvester) then
      Result := FStrFeiertag[Ord(fTag)]
  else
      Result := ERR_UNKNOWN_FTAG;
end;

//
// Bezeichnung des jeweiligen variablen Feiertages holen
//
function TDatumClass.VarFeiertag(fTag: TFeiertag): String;
begin
  if (fTag >= TRosenmontag) AND (fTag <= TStandardzeit) then
      Result := FStrFeiertag[Ord(fTag)]
  else
      Result := ERR_UNKNOWN_FTAG;
end;

// ------------------------------------------------------------------------ //
// Kalender Algorithmen                                                     //
// ------------------------------------------------------------------------ //

//
// Schaltjahr: 0 = nein, 1 = ja
//
function TDatumClass.Schaltjahr(jahr: Integer): Integer;
begin
  if (((jahr MOD 4 = 0) AND (jahr MOD 100 <> 0)) OR (jahr MOD 400 = 0)) then
      Result := 1
  else
      Result := 0;
end;

//
// berechnet die Nummer des Tages im Jahr
//
function TDatumClass.Tagesnummer(jahr, mon, dag: Integer): Integer;
var
  a, b: Integer;

begin
  a := (mon + 10) DIV 13;
  b := dag + (611 * (mon + 2)) DIV 20 - 2 * a - 91;
  Result := b + Schaltjahr(jahr) * a;
end;

//
// berechnet den Wochentag aus Jahr und Tagesnummer
//
function TDatumClass.WochentagAusTagNum(jahr, tnum: Integer): Integer;
var
  a, b: Integer;

begin
  a := (jahr - 1) MOD 100;
  b := (jahr - 1) DIV 100;
  Result := (28 + a + tnum + (a DIV 4) + (b DIV 4) + 5 * b) MOD 7;
end;

//
// berechnet den Monat aus Jahr und Tagesnummer
//
function TDatumClass.MonatAusTagNum(jahr, tnum: Integer): Integer;
var
  a: Integer;

begin
  a := Schaltjahr(jahr);
  If (tnum > (59 + a)) Then
      tnum := tnum + (2 - a);
  tnum := tnum + 91;
  Result := (20 * tnum) DIV 611 - 2;
end;

//
// berechnet den Tag des Monats aus Jahr und Tagesnummer
//
function TDatumClass.TagAusTagNum(jahr, tnum: Integer): Integer;
var
  a, b: Integer;

begin
  a := Schaltjahr(jahr);
  if (tnum > (59 + a)) Then
      tnum := tnum + (2 - a);
  tnum := tnum + 91;
  b := (20 * tnum) DIV 611;
  Result := tnum - (611 * b) DIV 20;
end;

//
// berechnet den Wochentag gleich oder nach der übergebenen Tagesnummer
//
function TDatumClass.AddWochentag(jahr, tnum: Integer; wTag: TWochentag): Integer;
var
  wotag: Integer;

begin
  // Wochentag des Ausgangstages berechnen
  wotag := WochentagAusTagNum(jahr, tnum);
  // Wochentag _nach_ oder _gleich_ dem Datum
  If (wotag > Ord(wTag)) Then
      Result := tnum - wotag + 7 + Ord(wTag)
  else
      Result := tnum - wotag + Ord(wTag);
end;

//
// berechnet den Wochentag vor der übergebenen Tagesnummer
//
function TDatumClass.subWochentag(jahr, tnum: Integer; wTag: TWochentag): Integer;
var
  wotag: Integer;

begin
  // Wochentag des Ausgangstages berechnen
  wotag := WochentagAusTagNum(jahr, tnum);
  // Wochentag _vor_ dem angegeben Datum ermitteln}
  If (wotag > Ord(wTag)) Then
      Result := tnum - wotag + Ord(WTag)
  else
      Result := tnum - wotag - 7 + Ord(WTag);
end;

//
// JulianDays sind die Tage seit dem 1.1.4713 v.u.Z.
// und ist eine wichtige Rechengröße für die Datumsroutinen
// dieser Algorithmus gilt erst ab dem Jahr 1 n.u.Z.
//
function TDatumClass.GregDateToJulianDay(kal: TDateTime): Longint;
var
  a, b: Longint;
  j, m, t: Word;

begin
  DecodeDate(kal, j, m, t);
  if (m > 2) Then
      m := m - 3
  else
  begin
      m := m + 9;
      j := J - 1;
  end;
  t := t + (153 * m + 2) DIV 5;
  a := (146097 * (j DIV 100)) DIV 4;
  b := (1461 * (j MOD 100)) DIV 4;
  Result := a + b + t + 1721119;
end;

//
// berechnet aus dem JulianDay das Gregorianische Datum
// dieser Algorithmus gilt erst ab dem Jahr 1 n.u.Z.
//
function TDatumClass.JulianDayToGregDate(julTag: Longint): TDateTime;
var
  ju, j, m, t: Longint;
  kal: TDateTime;

begin
  ju := julTag - 1721119;
  j  := (4 * ju - 1) DIV 146097;
  ju := (4 * ju - 1) MOD 146097;
  t  := ju DIV 4;
  ju := (4 * t + 3) DIV 1461;
  t  := (4 * t + 3) MOD 1461;
  t  := (t + 4) DIV 4;
  m  := (5 * t - 3) DIV 153;
  t  := (5 * t - 3) MOD 153;
  t  := (t + 5) DIV 5;
  j  := 100 * j + ju;
  If (m < 10) Then
      m := m + 3
  else
  begin
      m := m + 9;
      j := j + 1;
  end;
  kal := EncodeDate(j, m , t);
  Result := kal;
end;

//
// gibt die korrekte Anzahl Tage im Monat zurück, inklusive Schaltjahr
//
function TDatumClass.TageImMonat(jahr, mon: Integer): Integer;
var
  FTageMonat: Array[0..11] OF Integer = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
  tMon: Integer;

begin
  // Schaltjahr berücksichtigen
  tMon := FTageMonat[mon - 1];
  if (mon = 2) Then
      tMon := tMon + Schaltjahr(jahr);
  Result := tMon;
end;

//
// berechnet die Wochennummer
// ist der 1.1. ein Tag bis Donnerstag, so ist es die erste Woche des
// neuen, sonst die letzte des alten Jahres
//
function TDatumClass.Wochennummer(kal: TDateTime): Integer;
var
  zugJahr, wonum, wotag: Word;

begin
  // FreePascal: berechnet aus TDateTime die Wochennummer in wonum,
  // das zugehörige Jahr in zugJahr sowie den Wochentag in wotag
  DecodeDateWeek(kal, zugJahr, wonum, wotag);
  Result := wonum;
end;

//
// berechnet den Ostersonntag
//
function TDatumClass.Ostersonntag(jahr: Integer): Integer;
var
  a, b, c, d, f, g, h: Integer;

begin
  a := (jahr MOD  19) + 1;
  b := (jahr DIV 100) + 1;
  c := (3 * b) DIV 4 - 12;
  d := (8 * b + 5) DIV 25 - 5;
  f := (5 * jahr) DIV 4 - c - 10;
  g := (11 * a + 20 + d - c) MOD 30;
  If ((g = 25) OR (a > 11) AND (g = 24)) Then
      g := g + 1;
  h := 44 - g;
  if (h < 21) Then
      h := h + 30;
  h := h + 7 - (f + h) MOD 7;
  h := h + Schaltjahr(jahr);
  Result := h + 59;
end;

// ----------------------------------------- von Ostern abhängige Feiertage //
// Rosenmontag         = Ostersonntag - 48                                  //
// Aschermittwoch      = Ostersonntag - 46                                  //
// Gründonnerstag      = Ostersonntag -  3                                  //
// Karfreitag          = Ostersonntag -  2                                  //
// Ostersonntag                                                             //
// Ostermontag         = Ostersonntag +  1                                  //
// Christi Himmelfahrt = Ostersonntag + 39                                  //
// Pfingstsonntag      = Ostersonntag + 49                                  //
// Pfingstmontag       = Ostersonntag + 50                                  //
// Fronleichnam        = Ostersonntag + 60                                  /

function TDatumClass.OsterAbhaengig(jahr: Integer; fTag: TFeiertag): TDateTime;
var
  tNum, m, t: Integer;
  kal: TDateTime;

begin
  // die Parameter-Prüfung muss in der rufenden Routine stattfinden
  // Tagesnummer des Ostersonntags berechnen, darauf basieren alle anderen
  tNum := Ostersonntag(jahr);
  case fTag OF
      TRosenmontag    : tNum := tNum - 48;
      TAschermittwoch : tNum := tNum - 46;
      TGruendonnerstag: tNum := tNum -  3;
      TKarfreitag     : tNum := tNum -  2;
      TOstermontag    : tNum := tNum +  1;
      THimmelfahrt    : tNum := tNum + 39;
      TPfingstsonntag : tNum := tNum + 49;
      TPfingstmontag  : tNum := tNum + 50;
      TFronleichnam   : tNum := tNum + 60;
  end;
  // Monat und Tag mit dem Jahr zusammenbasteln
  m      := MonatAusTagNum(jahr, tNum);
  t      := TagAusTagNum(jahr, tNum);
  kal    := EncodeDate(jahr, m, t);
  Result := kal;
end;

// ----------------------------------------- von Weihnachten abhängige Tage //
// 4. Advent       = 25.12 -So                                              //
// 3. Advent       = 25.12 -So-7                                            //
// 2. Advent       = 25.12 -So-14                                           //
// 1. Advent       = 25.12 -So-21                                           //
// Totensonntag    = 25.12 -So-28                                           //
// Buß- und Bettag = 25.12 -So-32                                           //
// Volkstrauertag  = 25.12 -So-35                                           //
function TDatumClass.XmasAbhaengig(jahr: Integer; fTag: TFeiertag): TDateTime;
var
  tNum, m, t: Integer;
  kal: TDateTime;

begin
  // die Parameter-Prüfung muss in der rufenden Routine stattfinden
  // 4. Advent berechnen: kleiner oder gleich dem 24.12.
  tNum := Tagesnummer(jahr, 12, 25);
  tNum := subWochentag(jahr, tNum, TWochentag.TSonntag);
  case fTag OF
      TDritterAdvent : tNum := tNum -  7;
      TZweiterAdvent : tNum := tNum - 14;
      TErsterAdvent  : tNum := tNum - 21;
      TTotensonntag  : tNum := tNum - 28;
      TBussUndBettag : tNum := tNum - 32;
      TVolkstrauertag: tNum := tNum - 35;
  end;
  // Monat und Tag mit dem Jahr zusammenbasteln
  m      := MonatAusTagNum(jahr, tNum);
  t      := TagAusTagNum(jahr, tNum);
  kal    := EncodeDate(jahr, m, t);
  Result := kal;
end;

//
// holt einen Feiertag mit festem Datum
// kann von außen in Schleife abgearbeitet werden
//
function TDatumClass.FesteUnabhaeng(jahr: Integer; fTag: TFeiertag): TDateTime;
var
  dstring: String;
  kal: TDateTime;
  t, m: Longint;

begin
  // die Parameter-Prüfung muss in der rufenden Routine stattfinden
  dstring := FixFeiertag(fTag);        // Datum (mm-dd) liegt als String vor
  m := StrToInt(LeftStr(dstring, 2));
  t := StrToInt(Copy(dstring, 4, 2));
  // Monat und Tag mit dem Jahr zusammenbasteln
  kal := EncodeDate(jahr, m , t);
  Result := kal;
end;

//
// berechnet den Muttertag, 2. Sonntag im Mai: 01.05 - Sonntag + 14
//
function TDatumClass.Muttertag(jahr: Integer): TDateTime;
var
  m, t, tNum: Integer;
  kal: TDateTime;

begin
  // Muttertag berechnen...
  tNum := Tagesnummer(jahr, 5, 1);
  tNum := subWochentag(jahr, tNum, TSonntag) + 14;
  // ...darf nicht auf Pfingsten fallen, dann eine Woche vorher ansetzen
  if (tNum = Ostersonntag(jahr) + 49) Then
      tNum := tNum - 7;
  // Monat und Tag mit dem Jahr zusammenbasteln
  m      := MonatAusTagNum(jahr, tNum);
  t      := TagAusTagNum(jahr, tNum);
  kal    := EncodeDate(jahr, m, t);
  Result := kal;
end;

// ---------------- Berechnung des Tages der Umstellung Winter-Sommer-Zeit: //
// letzter Sonntag im März   : Beginn der Sommerzeit (1 Stunde vordrehen)   //
// Letzter Sonntag im Oktober: Ende der Sommerzeit (1 Stunde zurückdrehen)  //
//
function TDatumClass.Sommerzeit(jahr: Integer; fTag: TFeiertag): TDateTime;
var
  m, t, tNum: Integer;
  kal: TDateTime;

begin
  // jeweilige Tagesnummer holen
  if (fTag = TSommerzeit) Then
      tNum := Tagesnummer(jahr, 4, 1)
  else
      tNum := Tagesnummer(jahr, 11, 1);

  // jeweils der letzte Sonntag im Vormonat
  tNum := subWochentag(jahr, tNum, TSonntag);

  // Monat und Tag holen
  m      := MonatAusTagNum(jahr, tNum);
  t      := TagAusTagNum(jahr, tNum);
  kal    := EncodeDate(jahr, m, t);
  Result := kal;
end;

//
// berechnet rinrn Feiertag
// kann von außen in Schleife abgearbeitet werden
//
function TDatumClass.calcFeiertag(jahr: Integer; fTag: TFeiertag): TDateTime;
begin
  Result := 0;
  try
    // prüfen, ob fTag im korrekten Rahmen ist. Das Jahr
    // muss in der rufenden Funktion geprüft werden
    if (fTag >= TRosenmontag) AND (fTag <= TFronleichnam) Then
        Result := OsterAbhaengig(jahr, fTag)
    else if (fTag >= TVierterAdvent) AND (fTag <= TTotensonntag) Then
        Result := XmasAbhaengig(jahr, fTag)
    else if (fTag >= TNeujahr) AND (fTag <= TSilvester) Then
        Result := FesteUnabhaeng(jahr, fTag)
    else if (fTag = TMuttertag) Then
        Result := Muttertag(jahr)
    else if (fTag >= TSommerzeit) AND (fTag <= TStandardzeit) Then
        Result := Sommerzeit(jahr, fTag)
    // Exception-Objekt erstellen und Fehler auslösen
    else
      raise Exception.Create(ERR_UNKNOWN_FTAG);
  except
    // die eben erzeugte Exception abfangen und weiter nach oben reichen
    raise;
  end;

end;

end.

