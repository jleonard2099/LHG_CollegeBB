'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Courtside College Basketball - Stats Viewer"
''$ExeIcon:'./lhg_ccb.ico'
'_Icon
_AllowFullScreen

DECLARE SUB TITLE ()

Dim O%(100), O$(100), T%(100), P%(100), Q$(500), teamItems$(500)
'Dim Z2(14, 14)
Dim Z(15), Z1(15), VV%(500)
Dim A$(14), D$(1), L%(4)
Dim A(14, 24)
Dim H$(100), CP$(25)
Dim W0(14), W1(14), AP%(100, 1), CRD(100), SITE$(100)
CP% = 1

Data NAME,G,MIN,MIN/G,FGM,FGA,FG PCT,3FGM,3FGA,3FG PCT,FTM,FTA,FT PCT
Data NAME,PTS,PTG/G,HI,OFF REB,DEF REB,TOT REB,REB/G,PF,AST,BLK,STL,TO
For I = 0 To 25: Read CP$(I): Next

Open "DEV.BAS" For Input As #1: Input #1, D$(0): Input #1, D$(1): Close 1

45 Color 15, 0: Cls
Call TITLE
GoSub 3056
GoSub 6585
Z$ = Mid$(B$, 4, QQ) + "." + Left$(B$, 2)

90 If UCase$(Z$) = "END" Then Run "HELLO.EXE 4"
'*** READ IN TEAM STATS ***
If Not _FileExists(D$(1) + Z$) Then 5025
PX = 0
P%(0) = 0: P%(1) = 0: L% = 0: W% = 0: X% = 0
HW% = 0: VW% = 0: NW% = 0: HL% = 0: VL% = 0: NL% = 0
Open D$(1) + Z$ + "S" For Input As #2
Input #2, Y%
For I = 1 To 100: Input #2, O%(I): Next I
For I = 1 To 100: Input #2, O$(I): Next I
For I = 1 To 100: Input #2, T%(I): Next I
For I = 1 To 100: Input #2, H$(I): Next I
For I = 1 To 100
For I1 = 0 To 1: Input #2, AP%(I, I1): Next: Next
For I = 1 To 100: Input #2, CRD(I): Next I
For I = 1 To 100: Input #2, SITE$(I): Next I
Close #2
For I = 1 To Y%
    If O%(I) > T%(I) Then W% = W% + 1
    If O%(I) > T%(I) And Left$(H$(I), 1) = "H" Then HW% = HW% + 1
    If O%(I) > T%(I) And Left$(H$(I), 1) = "V" Then VW% = VW% + 1
    If O%(I) > T%(I) And Left$(H$(I), 1) = "N" Then NW% = NW% + 1
    If O%(I) < T%(I) Then L% = L% + 1
    If O%(I) < T%(I) And Left$(H$(I), 1) = "H" Then HL% = HL% + 1
    If O%(I) < T%(I) And Left$(H$(I), 1) = "V" Then VL% = VL% + 1
    If O%(I) < T%(I) And Left$(H$(I), 1) = "N" Then NL% = NL% + 1
    If O%(I) = T%(I) Then X% = X% + 1
    P%(0) = P%(0) + O%(I): P%(1) = P%(1) + T%(I)
Next
Open D$(1) + Z$ For Input As #1
Input #1, B$
For I = 0 To 13: Input #1, A$(I)
For J = 0 To 13: Input #1, A(I, J): Next: Input #1, W0(I): Input #1, W1(I): Next
For L = 0 To 15: Input #1, Z(L): Next
For L = 0 To 15: Input #1, Z1(L): Next
Close #1
For I = 0 To 13
    If A(I, 0) <> 0 Then A(I, 14) = A(I, 10) / A(I, 0): A(I, 20) = (A(I, 2) * 2 + A(I, 4) + W0(I)) / A(I, 0): A(I, 21) = A(I, 5) / A(I, 0)
    A(I, 22) = A(I, 2) * 2 + A(I, 4) + W0(I)
    A(I, 23) = A(I, 5) - A(I, 12)
    If A(I, 1) <> 0 Then A(I, 15) = A(I, 2) / A(I, 1)
    A(I, 18) = W0(I): A(I, 19) = W1(I)
    If A(I, 19) <> 0 Then A(I, 16) = A(I, 18) / A(I, 19)
    If A(I, 3) <> 0 Then A(I, 17) = A(I, 4) / A(I, 3)
Next
GoSub 815
125 I$ = GetKeyPress$
Return

142 Call TITLE
Color , 1
Locate 24, 1: Print Tab(80);: Locate , 1
Color 15, 1: Print "SCHEDULE ";
Color 14: Print "[Pg UP]  ";
Color 15: Print "MORE STATS ";
Color 14: Print "[Pg DN]  ";
Color 15: Print "NEW TEAM ";
Color 14: Print "[ESC]  ";
Color 15: Print "SORT ";
Color 14: Print "<-,-> and [ENTER] ";
Locate 25, 1
Color 15: Print "PRINT ";: Color 14: Print "[P]  ";
Color 15: Print "DELETE STATS ";: Color 14: Print "[D]                    ";
Color 12, 0: Locate 2, 1: Print "SORT BY: "; CP$(CP%); Tab(40);
Locate 3, 1: Color 2, 0
Color 14: Print B$
Print "NAME"; Tab(15);
Print " G  MIN AVG   FGM  FGA  PCT  3FGM 3FGA  PCT   FTM  FTA  PCT"
For I = 0 To 13
    If A$(I) = "XXX" Then 165
    Color 7
    If I / 2 = Int(I / 2) Then Color 15
    Print Left$(A$(I), 13); Tab(15);
    Print Using "## "; A(I, 0);
    If A(I, 10) > 0 Then Locate , 18: Print Using "####"; Int((A(I, 10) + 30) / 60);: Locate , 24: Print Using "##"; Int(((A(I, 10) / 60) / A(I, 0)) + .5);
    Locate , 28: Print Using "#### ####"; A(I, 2); A(I, 1);: Locate , 38
    If A(I, 1) = 0 Then Print ".---";: GoTo 160
    If A(I, 2) = A(I, 1) Then Print "1.00";: GoTo 160
    If A(I, 2) = 0 Then Print ".000";: GoTo 160
    Print Using ".###"; A(I, 2) / A(I, 1);

    160 Locate , 44: Print Using "#### ####"; W0(I); W1(I);: Locate , 54
    If W1(I) = 0 Then Print ".---";: GoTo 161
    If W1(I) = W0(I) Then Print "1.00";: GoTo 161
    If W0(I) = 0 Then Print ".000";: GoTo 161
    Print Using ".###"; W0(I) / W1(I);

    161 Locate , 60: Print Using "#### ####"; A(I, 4); A(I, 3);: Locate , 70
    If A(I, 3) = 0 Then Print ".---": GoTo 165
    If A(I, 3) = A(I, 4) Then Print "1.00": GoTo 165
    If A(I, 4) = 0 Then Print ".000": GoTo 165
    Print Using ".###"; A(I, 4) / A(I, 3)

165 Next
Color 12: Print: Print "TOTALS";: Locate , 15
Print Using "##"; Y%;
Locate , 28: Print Using "#### ####"; Z(2); Z(1);
If Z(1) <> 0 Then Locate , 38: Print Using ".###"; Z(2) / Z(1);
Locate , 44: Print Using "#### ####"; Z(13); Z(14);
If Z(14) <> 0 Then Locate , 54: Print Using ".###"; Z(13) / Z(14);
Locate , 60: Print Using "#### ####"; Z(4); Z(3);
If Z(3) <> 0 Then Locate , 70: Print Using ".###"; Z(4) / Z(3)
Color 12: Locate , 15: Print Using "##"; Y%;
Locate , 28: Print Using "#### ####"; Z1(2); Z1(1);
If Z1(1) <> 0 Then Locate , 38: Print Using ".###"; Z1(2) / Z1(1);
Locate , 44: Print Using "#### ####"; Z1(13); Z1(14);
If Z1(14) <> 0 Then Locate , 54: Print Using ".###"; Z1(13) / Z1(14);
Locate , 60: Print Using "#### ####"; Z1(4); Z1(3);
If Z1(3) <> 0 Then Locate , 70: Print Using ".###"; Z1(4) / Z1(3)

230 GoSub 125
If I$ = Chr$(0) + "Q" Then CP% = 14: GoTo 235
If I$ = Chr$(0) + "I" Then Cls: GoTo 915
If UCase$(I$) = "D" Then GoSub 485: GoTo 230
If I$ = Chr$(0) + "M" Then GoSub 650: GoTo 230: 'RIGHT
If I$ = Chr$(0) + "K" Then GoSub 660: GoTo 230: 'LEFT
If UCase$(I$) = "P" Then GoSub 875: GoSub 530: GoTo 230
If I$ = Chr$(13) Then GoSub 600: GoTo 142
If I$ = Chr$(27) Then 45
GoTo 230

650 CP% = CP% + 1: If CP% > 12 Then CP% = 1
Locate 2, 1: Print "SORT BY: "; CP$(CP%); Tab(40);
Return

660 CP% = CP% - 1: If CP% < 1 Then CP% = 12
Locate 2, 1: Print "SORT BY: "; CP$(CP%); Tab(40);
Return

235 Cls
Call TITLE
Color , 1
Locate 24, 1: Print Tab(80);: Locate , 1
Locate 24, 1: Color 14, 1
Color 15: Print "MORE STATS ";
Color 14: Print "[Pg UP]  ";
Color 15: Print "SELECT NEW TEAM ";
Color 14: Print "[ESC]  ";
Color 15: Print "SORT ";
Color 14: Print "<-,-> and [ENTER]  ";
Color 15: Print "PRINT ";
Color 14: Print "[P]";
Locate 25, 1
Color 15: Print "DELETE STATS ";: Color 14: Print "[D]                     ";
Color 12, 0: Locate 2, 1: Print "SORT BY: "; CP$(CP%); Tab(40);
Locate 3, 1: Color 2, 0
Color 14: Print B$;: Locate , 15
Print "               OFF  DEF  TOT": Print "NAME"; Tab(15);
Print " PTS   AVG HI  REB  REB  REB   AVG   PF  AST  BLK  STL   TO"
For I = 0 To 13
    If A$(I) = "XXX" Then 266
    Color 7
    If I / 2 = Int(I / 2) Then Color 15
    Print Left$(A$(I), 13);: Locate , 15
    Print Using "####"; A(I, 2) * 2 + A(I, 4) + W0(I);
    If A(I, 0) > 0 Then Locate , 21: Print Using "##.#"; (A(I, 2) * 2 + A(I, 4) + W0(I)) / A(I, 0);
    Locate , 26: Print Using "## #### #### ####"; A(I, 13); A(I, 12); A(I, 5) - A(I, 12); A(I, 5);
    If A(I, 0) > 0 Then Locate , 45: Print Using "##.#"; A(I, 5) / A(I, 0);
    Locate , 51: Print Using "### ####  ###  ###  ###"; A(I, 6); A(I, 8); A(I, 9); A(I, 7); A(I, 11)

266 Next I
Print "TEAM";: Locate , 39: Print Using "####"; Z(15)
Color 12: Print "TOTALS";
Locate , 14: Print Using "##### ###.#"; Z(10); Z(10) / Y%;
Locate , 29: Print Using "#### #### ####  ##.#"; Z(12); Z(5) - Z(12) - Z(15); Z(5); Z(5) / Y%;
Print Using " #### #### #### #### ####"; Z(6); Z(8); Z(9); Z(7); Z(11)
Locate , 14: Print Using "##### ###.#"; Z1(10); Z1(10) / Y%;
Locate , 29: Print Using "#### #### ####  ##.#"; Z1(12); Z1(5) - Z1(12) - Z1(15); Z1(5); Z1(5) / Y%;
Print Using " #### #### #### #### ####"; Z1(6); Z1(8); Z1(9); Z1(7); Z1(11): Color 3
370 I$ = GetKeyPress$
If I$ = Chr$(0) + "I" Then Cls: CP% = 1: GoTo 142
If I$ = Chr$(0) + "M" Then GoSub 655: GoTo 370: 'RIGHT
If UCase$(I$) = "D" Then GoSub 485: GoTo 370
If I$ = Chr$(0) + "K" Then GoSub 665: GoTo 370: 'LEFT
If I$ = Chr$(13) Then GoSub 600: GoTo 235
If I$ = Chr$(27) Then 45
If UCase$(I$) = "P" Then GoSub 875: GoSub 530: GoTo 370
GoTo 370

655 CP% = CP% + 1: If CP% > 25 Then CP% = 14
Locate 2, 1: Print "SORT BY: "; CP$(CP%); Tab(40);
Return

665 CP% = CP% - 1: If CP% < 14 Then CP% = 25
Locate 2, 1: Print "SORT BY: "; CP$(CP%); Tab(40);
Return
Cls: Call TITLE: Print "RESET STATS TO 0 ? (YN)";:
455 I$ = GetKeyPress$
If UCase$(I$) <> "Y" Then 45
Print: Print "ARE YOU SURE ? (YN) ";
470 I$ = GetKeyPress$
If UCase$(I$) = "N" Then Run 'X = FRE(0)

485 Kill D$(1) + Z$
Kill D$(1) + Z$ + "S"
Return
Cls
Run

530 LPrint "STATISTICS SUMMARY FOR "; B$
LPrint "NAME"; Tab(15);
LPrint " G  MIN AVG   FGM  FGA  PCT  3FGM 3FGA  PCT   FTM  FTA  PCT"
For I = 0 To 13
    LPrint Left$(A$(I), 13); Tab(15);
    LPrint Using "## "; A(I, 0);
    If A(I, 10) > 0 Then LPrint Tab(18); Using "####"; Int((A(I, 10) + 30) / 60);: LPrint Tab(24); Using "##"; Int(((A(I, 10) / 60) / A(I, 0)) + .5);
    LPrint Tab(28); Using "#### ####"; A(I, 2); A(I, 1);
    LPrint Tab(38);
    If A(I, 1) = 0 Then LPrint ".---";: GoTo 531
    If A(I, 2) = A(I, 1) Then LPrint "1.00";: GoTo 531
    If A(I, 2) = 0 Then LPrint ".000";: GoTo 531
    LPrint Using ".###"; A(I, 2) / A(I, 1);

    531 LPrint Tab(44); Using "#### ####"; W0(I); W1(I);: LPrint Tab(54);
    If W1(I) = 0 Then LPrint ".---";: GoTo 532
    If W1(I) = W0(I) Then LPrint "1.00";: GoTo 532
    If W0(I) = 0 Then LPrint ".000";: GoTo 532
    LPrint Using ".###"; W0(I) / W1(I);

    532 LPrint Tab(60); Using "#### ####"; A(I, 4); A(I, 3);: LPrint Tab(70);
    If A(I, 3) = 0 Then LPrint ".---": GoTo 533
    If A(I, 3) = A(I, 4) Then LPrint "1.00": GoTo 533
    If A(I, 4) = 0 Then LPrint ".000": GoTo 533
    LPrint Using ".###"; A(I, 4) / A(I, 3)

533 Next
LPrint: LPrint "TOTALS"; Tab(15);
LPrint Using "##"; Y%;
LPrint Tab(28); Using "#### ####"; Z(2); Z(1);
If Z(1) <> 0 Then LPrint Tab(38); Using ".###"; Z(2) / Z(1);
LPrint Tab(44); Using "#### ####"; Z(13); Z(14);
If Z(14) <> 0 Then LPrint Tab(54); Using ".###"; Z(13) / Z(14);
LPrint Tab(60); Using "#### ####"; Z(4); Z(3);
If Z(3) <> 0 Then LPrint Tab(70); Using ".###"; Z(4) / Z(3)
LPrint Tab(15); Using "##"; Y%;
LPrint Tab(28); Using "#### ####"; Z1(2); Z1(1);
If Z1(1) <> 0 Then LPrint Tab(38); Using ".###"; Z1(2) / Z1(1);
LPrint Tab(44); Using "#### ####"; Z1(13); Z1(14);
If Z1(14) <> 0 Then LPrint Tab(54); Using ".###"; Z1(13) / Z1(14);
LPrint Tab(60); Using "#### ####"; Z1(4); Z1(3);
If Z1(3) <> 0 Then LPrint Tab(70); Using ".###"; Z1(4) / Z1(3)
LPrint
LPrint Tab(15);
LPrint "               OFF  DEF  TOT": LPrint Tab(15);
LPrint " PTS   AVG HI  REB  REB  REB   AVG   PF  AST  BLK  STL   TO"
For I = 0 To 13
    LPrint Left$(A$(I), 13);: LPrint Tab(15);
    LPrint Using "####"; A(I, 2) * 2 + A(I, 4) + W0(I);
    If A(I, 0) > 0 Then LPrint Tab(21); Using "##.#"; (A(I, 2) * 2 + A(I, 4) + W0(I)) / A(I, 0);
    LPrint Tab(26); Using "## #### #### ####"; A(I, 13); A(I, 12); A(I, 5) - A(I, 12); A(I, 5);
    If A(I, 0) > 0 Then LPrint Tab(45); Using "##.#"; A(I, 5) / A(I, 0);
    LPrint Tab(51); Using "### ####  ###  ###  ###"; A(I, 6); A(I, 8); A(I, 9); A(I, 7); A(I, 11)
Next I
LPrint "TEAM";: LPrint Tab(39); Using "####"; Z(15)
LPrint: LPrint "TOTALS";
LPrint Tab(14); Using "##### ###.#"; Z(10); Z(10) / Y%;
LPrint Tab(29); Using "#### #### ####  ##.#"; Z(12); Z(5) - Z(12) - Z(15); Z(5); Z(5) / Y%;
LPrint Using " #### #### #### #### ####"; Z(6); Z(8); Z(9); Z(7); Z(11)
LPrint Tab(14); Using "##### ###.#"; Z1(10); Z1(10) / Y%;
LPrint Tab(29); Using "#### #### ####  ##.#"; Z1(12); Z1(5) - Z1(12) - Z1(15); Z1(5); Z1(5) / Y%;
LPrint Using " #### #### #### #### ####"; Z1(6); Z1(8); Z1(9); Z1(7); Z1(11)
LPrint Chr$(12)
Return

600 'SORT BASED ON CATEGORY
If CP% = 1 Then SG% = 0
If CP% = 2 Then SG% = 10
If CP% = 3 Then SG% = 14
If CP% = 4 Then SG% = 2
If CP% = 5 Then SG% = 1
If CP% = 6 Then SG% = 15
If CP% = 7 Then SG% = 18
If CP% = 8 Then SG% = 19
If CP% = 9 Then SG% = 16
If CP% = 10 Then SG% = 4
If CP% = 11 Then SG% = 3
If CP% = 12 Then SG% = 17
If CP% = 14 Then SG% = 22
If CP% = 15 Then SG% = 20
If CP% = 17 Then SG% = 12
If CP% = 16 Then SG% = 13
If CP% = 18 Then SG% = 23
If CP% = 19 Then SG% = 5
If CP% = 20 Then SG% = 21
If CP% = 21 Then SG% = 6
If CP% = 22 Then SG% = 8
If CP% = 23 Then SG% = 9
If CP% = 24 Then SG% = 7
If CP% = 25 Then SG% = 11
For Z = 0 To 12
    For Z1 = Z + 1 To 13
        If A(Z, SG%) >= A(Z1, SG%) Then 605
        For I = 0 To 24: Swap A(Z, I), A(Z1, I): Next
        Swap W0(Z), W0(Z1)
        Swap W1(Z), W1(Z1)
        Swap A$(Z), A$(Z1)

605 Next Z1: Next Z
Return

815 Call TITLE: Color , 1: Locate 1, 1: Print Tab(80);: Locate , 31: Color 14
Y = Len(B$): Y = Int((80 - Y) / 2): Locate , Y: Print B$
Locate 25, 1: Print Tab(80);: Locate , 1
Locate 2, 1
Color , 0
For I = 1 To Y%
    Color 7
    If I / 2 = Int(I / 2) Then Color 15
    If I > 54 Then 860
    If I > 36 Then 827
    If I > 18 Then 826
    Print Using "###"; O%(I);
    Print Tab(5); Left$(O$(I), 14);
    Print Tab(20); Using "###"; T%(I);
    Print Tab(24); H$(I)
    GoTo 830

    826 Locate I - 17, 27: Print Using "###"; O%(I);
    Print Tab(31); Left$(O$(I), 14);
    Print Tab(46); Using "###"; T%(I);
    Print Tab(50); H$(I)
    GoTo 830

    827 Locate I - 35, 53: Print Using "###"; O%(I);
    Print Tab(57); Left$(O$(I), 14);
    Print Tab(72); Using "###"; T%(I);
    Print Tab(76); H$(I)

830 Next I
Print

860 Locate 21, 27
Color 14: Print "OVERALL RECORD ";: Color 15: Print "("; W%; "-"; L%; ")"
Locate , 27: Color 14: Print "HOME RECORD    ";: Color 15: Print "("; HW%; "-"; HL%; ")"
Locate , 27: Color 14: Print "ROAD RECORD    ";: Color 15: Print "("; VW%; "-"; VL%; ")"
Locate , 27: Color 14: Print "NEUTRAL RECORD ";: Color 15: Print "("; NW%; "-"; NL%; ")";
Color 3
Locate 25, 1
Color 15, 1: Print "INDIVIDUAL STATS ";
Color 14: Print "[Pg DN]  ";
Color 15: Print "SELECT ANOTHER TEAM ";
Color 14: Print "[ESC]  ";
Color 15: Print "PRINT STATS ";
Color 14: Print "[P]";
Locate 3, 1
870 I$ = GetKeyPress$
If I$ = Chr$(0) + "Q" Then Color , 0: Cls: GoTo 915
If UCase$(I$) = "D" Then GoSub 485: GoTo 870
If I$ = Chr$(27) Then 45
If UCase$(I$) = "P" Then GoSub 875: GoSub 530: GoTo 870
GoTo 870

915 Color , 1: Locate 1, 1: Print Tab(80);: Locate , 31: Color 14
Y = Len(B$): Y = Int((80 - Y) / 2): Locate , Y: Print B$
Locate 25, 1: Print Tab(80);: Locate , 1
Locate 2, 1
Color , 0
For I = 1 To Y%
    Color 7
    If I / 2 = Int(I / 2) Then Color 15
    If I < 55 Then 930
    If I > 72 Then 926
    Print Using "###"; O%(I);
    Print Tab(5); Left$(O$(I), 14);
    Print Tab(20); Using "###"; T%(I);
    Print Tab(24); H$(I)
    GoTo 930

    926 Locate I - 71, 27: Print Using "###"; O%(I);
    Print Tab(31); Left$(O$(I), 14);
    Print Tab(46); Using "###"; T%(I);
    Print Tab(50); H$(I)
    GoTo 930

930 Next I
Print
Locate 21, 27
Color 14: Print "OVERALL RECORD ";: Color 15: Print "("; W%; "-"; L%; ")"
Locate , 27: Color 14: Print "HOME RECORD    ";: Color 15: Print "("; HW%; "-"; HL%; ")"
Locate , 27: Color 14: Print "ROAD RECORD    ";: Color 15: Print "("; VW%; "-"; VL%; ")"
Locate , 27: Color 14: Print "NEUTRAL RECORD ";: Color 15: Print "("; NW%; "-"; NL%; ")";
Color 3
Locate 25, 1
Color 15, 1: Print "INDIVIDUAL STATS ";
Color 14: Print "[Pg DN]  ";
Color 15: Print "SELECT ANOTHER TEAM ";
Color 14: Print "[ESC]  ";
Color 15: Print "PRINT STATS ";
Color 14: Print "[P]";
Locate 3, 1
970 I$ = GetKeyPress$
If I$ = Chr$(0) + "Q" Then Color , 0: Cls: GoTo 142
If I$ = Chr$(0) + "I" Then Color , 0: Cls: GoTo 815
If UCase$(I$) = "D" Then GoSub 485: GoTo 970
If I$ = Chr$(27) Then 45
If UCase$(I$) = "P" Then GoSub 875: GoSub 530: GoTo 970
GoTo 970

875 LPrint "SCHEDULE RESULTS FOR "; B$
LPrint
For I = 1 To 28
    LPrint Using "###"; O%(I);
    LPrint Tab(5); Left$(O$(I), 14);
    LPrint Tab(20); Using "###"; T%(I);
    LPrint Tab(24); H$(I); Tab(27);
    If I + 28 > Y% Then LPrint: GoTo 880
    LPrint Using "###"; O%(I + 28);
    LPrint Tab(31); Left$(O$(I + 28), 14);
    LPrint Tab(46); Using "###"; T%(I + 28);
    LPrint Tab(50); H$(I + 28); Tab(53);
    If I + 56 > Y% Then LPrint: GoTo 880
    LPrint Using "###"; O%(I + 56);
    LPrint Tab(57); Left$(O$(I + 56), 14);
    LPrint Tab(72); Using "###"; T%(I + 56);
    LPrint Tab(76); H$(I + 56)

880 Next I
LPrint
LPrint Tab(27); "OVERALL RECORD ("; W%; "-"; L%; ")"
LPrint Tab(27); "HOME RECORD    ("; HW%; "-"; HL%; ")"
LPrint Tab(27); "ROAD RECORD    ("; VW%; "-"; VL%; ")"
LPrint Tab(27); "NEUTRAL RECORD ("; NW%; "-"; NL%; ")"
LPrint Chr$(12)
Return

3056 Color 15, 0
JJ = 6
SN = 1: Locate 6, 8: Color 15, 4
Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
Locate 7 + JJ, 8
Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";: Locate 7, 32
Locate , 12: Print "ENTER DISK ID: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Print
Locate , 12: Print "ESC TO RETURN TO MAIN MENU"
3057 I$ = GetKeyPress$
If I$ = Chr$(27) Then Z$ = "END": GoTo 90
If I$ = Chr$(13) Then GoSub 1102: GoTo 3060
Locate 7, 27: Print I$
Locate 7, 28: Line Input NY$
NY$ = I$ + NY$
Color 15, 0

If NY$ = "" Then GoSub 1102: GoTo 3060
YN$ = NY$
ERASE teamItems$
GoSub 1100

3060 If BO% = 1 Then BO% = 0: GoTo 3056

3058 Locate 2, 33: Color T%(0), T%(1): Print A$
Color 15, 4: Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 3056
Color 15, 0
Return

1100 Open D$(0) + "BASK." + YN$ For Random As #1 Len = 655
Y = LOF(1)
Field #1, 15 As Q$(0), 640 As X$
For I1 = 1 To Y / 655
    Get #1, I1
    teamItems$(I1) = Q$(0): VV%(I1) = I1
Next
Close 1
NTMS = Y / 655

1102 SELECTION% = 1
Count% = NTMS
Start% = 1
FileFGColr% = 15: FileBGColr% = 4
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Rows% = 20
Columns% = 4
Spacing% = 6
Row% = 4
Column% = 1
Color , 4: Cls: Locate , 33: Color 14
Print "TEAM SELECTION": Color 15
Locate 25, 1: Color 14: Print "ESC";: Color 15
Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
Print "ENTER";: Color 15: Print "-SELECTS TEAM";
    
_MouseShow "DEFAULT"
20 MAMenu teamItems$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then BO% = 1: Return
If EXITCODE% = -71 Then Start% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 20
If EXITCODE% = -79 Then Start% = NTMS: EXITCODE% = 3: SELECTION% = NTMS: GoTo 20
If EXITCODE% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If EXITCODE% = -81 Then Start% = Start% + 80: If Start% > NTMS Then Start% = NTMS
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = Start%: GoTo 20
T1 = VV%(SELECTION%)

9000 Open D$(0) + "BASK." + YN$ For Random As #1 Len = 655
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 2 AS Q$(16), 2 AS Q$(17), 2 AS Q$(18), 2 _
AS Q$(19), 602 AS X$
For I = 0 To 13
FIELD #1, 53 + I * 43 AS X$, 15 AS Q$(I * 15 + 20), 2 AS Q$(I * 15 + 21), 2 AS Q$(I * 15 + 22), 2 AS Q$(I * 15 + 23), 2 AS Q$(I * 15 + 24), 2 AS Q$(I * 15 + 25), 2 AS Q$(I * 15 + 26), 2 AS Q$(I * 15 + 27), 2 AS Q$(I * 15 + 28), 2 AS Q$(I * 15 + 29) _
, 2 AS Q$(I * 15 + 30), 2 AS Q$(I * 15 + 31), 2 AS Q$(I * 15 + 32), 2 AS Q$(I * 15 + 33), 2 AS Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 AS X$
Next
Get #1, T1
A$ = Q$(0)

9005 If Right$(A$, 1) = " " Then A$ = Left$(A$, Len(A$) - 1): GoTo 9005
B$ = A$
SP = CVI(Q$(1)): DA = CVI(Q$(2)): V1% = CVI(Q$(3)): V2% = CVI(Q$(4))
For I = 0 To 4: L%(I) = CVI(Q$(I + 5)): Next
For I = 0 To 9: T%(I) = CVI(Q$(I + 10)): Next
Close 1
B$ = A$
Return

6585 For XX = 4 To 11: If Mid$(B$, XX, 1) = " " Or Mid$(B$, XX, 1) = "." Then Mid$(B$, XX, 1) = "_"
Next XX
For XX = 12 To 14: If Mid$(B$, XX, 2) = "ST" Then Mid$(B$, 9, 1) = "_": Mid$(B$, 10, 2) = "ST"
Next XX
QQ = 8: For XX = 11 To 4 Step -1: If Mid$(B$, XX, 1) = "_" Then QQ = XX - 4: GoTo 6615
    GoTo 6620

6615 Next XX

6620 Return

4500 Color 15, 0: Locate 24, 1: Print "PRESS ANY KEY TO CONTINUE";
4501 I$ = GetKeyPress$
Return

5025 Cls: Print "ERROR::: UNABLE TO LOCATE COMPILED STATS FILE"
GoTo 45

895 Color 7: Locate 24, 1: Print "ANY KEY TO CONTINUE";
896 I$ = GetKeyPress$
Return


'------------------------------
'      TITLE Subroutine
'------------------------------
' Displays window title / game copyright
Sub TITLE ()
    Color , 0: KEY Off: Cls
    Color , 1: Locate 1, 1: Print Tab(80);: Locate , 23: Color 12
    Print "COURTSIDE COLLEGE BASKETBALL V4.0"
    Locate 3, 1: Color 2, 0
End Sub

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
