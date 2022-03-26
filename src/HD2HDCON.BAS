'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

DECLARE SUB TITLE ()

Cls
Dim O%(100), O$(100), TR%(100), H$(100), Z1$(40), HW(50), AW(50), HL(50), AL(50), R1(50), R2(50), R3(50), R4(50)
Dim Q$(377), B%(14, 19), B$(14), A$(14, 1), VV$(500), VV%(500), Z2$(40)
Dim TM1$(40), OPP$(100), ZA$(40), Z1%(40), Z3$(40)
Dim TAW(40), DV$(4), THW(40), item$(2), THL(40), TAL(40), TR3(40), TR4(40), D$(1)
Dim TR1(40), TR2(40), QQ$(1), T%(9), L%(4)
Open "DEV.BAS" For Input As #2
For I = 0 To 1: Input #2, D$(I): Next
Close #2

600 Call TITLE
item$(1) = "VIEW TEAM VS CONFERENCE"
item$(2) = "RETURN TO MAIN  MENU   "
SELECTION% = 1
Count% = 2
Start% = 1
SCANCODE% = 2
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 25

Call box0(7, 24, 10, 48, 2, 31)
    
_MouseShow "DEFAULT"
MMenuVert item$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then SELECTION% = 2
If SELECTION% = 1 Then 630
Run "HELLO.EXE 3"

630 RDD = 0

39 ER = 0

40 GoSub 3056
B$ = QQ$(1): GoSub 6585
Z$ = Mid$(B$, 4, QQ) + "." + Left$(B$, 2)
If Not _FileExists(D$(1) + Z$ + "S") Then 4500
Open D$(1) + Z$ + "S" For Input As #4
Input #4, Y
For J = 1 To 100: Input #4, O%(J): Next
For J = 1 To 100: Input #4, O$(J): Next
For J = 1 To 100: Input #4, TR%(J): Next
For J = 1 To 100: Input #4, H$(J): Next
Close #4
GoSub 1440
LG$ = F$

49 Cls: J = 5: GoSub 2075: Color 7
Locate , 30: Print "STATS TO ";: Color 15: Print "S";: Color 7: Print "CREEN"
Locate , 30: Print "STATS TO ";: Color 15: Print "P";: Color 7: Print "RINTER"
VPP = 0
8651 I$ = GetKeyPress$
Color 14, 0
If UCase$(I$) = "P" Then X1 = 1
If UCase$(I$) = "S" Then VPP = 1

78 W$ = LG$ + ".LIB"
Open D$(1) + W$ For Input As #1
Input #1, X
Input #1, LG$
Input #1, D1$: Input #1, D2$
For X = 1 To 30
    Input #1, Z1$(X): Input #1, Z1%(X): Input #1, Z2$(X)
Next
NT = 30
Close 1
Cls
Color 15: Locate 18, 18: Print "COMPUTING W/L/T RECORDS . . . PLEASE WAIT"
For Q = 1 To 30
    AW(Q) = 0: HW(Q) = 0
    AL(Q) = 0: HL(Q) = 0
    R1(Q) = 0: R2(Q) = 0
    R3(Q) = 0: R4(Q) = 0
Next
For K = 1 To 2
    THW(K) = 0: THL(K) = 0
    TAW(K) = 0: TAL(K) = 0
    TR1(K) = 0: TR2(K) = 0
    TR3(K) = 0: TR4(K) = 0
Next
For K = 1 To 30
    If Z1$(K) = "XXX" Then 181
    For I = 1 To Y
        If O%(I) > TR%(I) And Left$(H$(I), 1) = "V" And O$(I) = Z1$(K) Then AW(K) = AW(K) + 1
        If O%(I) > TR%(I) And (Left$(H$(I), 1) = "H" Or Left$(H$(I), 1) = "N") And O$(I) = Z1$(K) Then HW(K) = HW(K) + 1
        If O%(I) < TR%(I) And Left$(H$(I), 1) = "V" And O$(I) = Z1$(K) Then AL(K) = AL(K) + 1
        If O%(I) < TR%(I) And (Left$(H$(I), 1) = "H" Or Left$(H$(I), 1) = "N") And O$(I) = Z1$(K) Then HL(K) = HL(K) + 1
        If O$(I) = Z1$(K) And (Left$(H$(I), 1) = "H" Or Left$(H$(I), 1) = "N") Then R2(K) = R2(K) + TR%(I)
        If O$(I) = Z1$(K) And (Left$(H$(I), 1) = "H" Or Left$(H$(I), 1) = "N") Then R1(K) = R1(K) + O%(I)
        If O$(I) = Z1$(K) And Left$(H$(I), 1) = "V" Then R4(K) = R4(K) + TR%(I)
        If O$(I) = Z1$(K) And Left$(H$(I), 1) = "V" Then R3(K) = R3(K) + O%(I)

181 Next I: Next K
If VPP <> 1 Then 1200

190 DV$(1) = D1$
DV$(2) = D2$

200 Cls: Color 12
Locate 2, 35: Color T%(0), T%(1)
Print Z$: Color 0, 0
Color 3: Locate 4
Print Tab(15); String$(7, 196); " HOME "; String$(8, 196); Tab(36); Chr$(194); String$(7, 196); " AWAY "; String$(8, 196); Tab(58); Chr$(194); String$(6, 196); " TOTALS "; String$(7, 196)
    LOCATE 5: PRINT TAB(15); "W"; TAB(17); "L"; TAB(21); "PCT."; TAB(26); "PF/G"; TAB(32); "PA/G"; TAB(36); CHR$(179); TAB(37); "W"; TAB(39); "L"; TAB(43); "PCT."; TAB(48); "PF/G"; TAB(54); "PA/G"; TAB(58); CHR$(179); TAB(59); "W"; TAB(61); "L"; TAB _
(65); "PCT."; TAB(70); "PF/G"; TAB(76); "PA/G"
RDD = RDD + 1
If RDD = 1 Then 210
If RDD = 2 Then 211

210 DD = 1: EE = 15: If DV$(RDD) = "XXX" Or DV$(RDD) = "" Then 400 Else GoTo 215

211 DD = 16: EE = 30: If DV$(RDD) = "XXX" Or DV$(RDD) = "" Then 400 Else GoTo 215

215 Color 11: Print DV$(RDD);: Color 3: Print Tab(36); Chr$(179); Tab(58); Chr$(179)
For K = DD To EE
    If Z1$(K) = "XXX" Or Z1$(K) = TM$ Then 250
    Color 15: Locate , 1: Print Left$(Z1$(K), 13);
    Color 7: Locate , 15: Print Using "#"; HW(K);
    Locate , 17: Print Using "#"; HL(K);
    If HW(K) = HW(K) + HL(K) And HW(K) <> 0 Then Locate , 19: Print "1.000";
    If HW(K) = 0 Then Locate , 20: Print ".000";
    If HW(K) > 0 And HW(K) < HW(K) + HL(K) Then Locate , 20: Print Using ".###"; (HW(K) / (HW(K) + HL(K)));
    If R1(K) = 0 Then Locate , 25: Print "  0.0";
    If R1(K) > 0 Then Locate , 25: Print Using "###.#"; R1(K) / (HW(K) + HL(K));
    If R2(K) = 0 Then Locate , 31: Print "  0.0";
    If R2(K) > 0 Then Locate , 31: Print Using "###.#"; R2(K) / (HW(K) + HL(K));
    Color 3: Print Tab(36); Chr$(179);
    Color 7: Locate , 37: Print Using "#"; AW(K);
    Locate , 39: Print Using "#"; AL(K);
    If AW(K) = AW(K) + AL(K) And AW(K) <> 0 Then Locate , 41: Print "1.000";
    If AW(K) = 0 Then Locate , 42: Print ".000";
    If AW(K) > 0 And AW(K) < AW(K) + AL(K) Then Locate , 42: Print Using ".###"; (AW(K) / (AW(K) + AL(K)));
    If R3(K) = 0 Then Locate , 47: Print "  0.0";
    If R3(K) > 0 Then Locate , 47: Print Using "###.#"; R3(K) / (AW(K) + AL(K));
    If R4(K) = 0 Then Locate , 53: Print "  0.0";
    If R4(K) > 0 Then Locate , 53: Print Using "###.#"; R4(K) / (AW(K) + AL(K));
    Color 3: Print Tab(58); Chr$(179);
    Color 7: Locate , 59: Print Using "##"; HW(K) + AW(K);
    Locate , 61: Print Using "##"; HL(K) + AL(K);
    If (HW(K) + AW(K)) = (HW(K) + AW(K) + HL(K) + AL(K)) And (HW(K) + AW(K)) <> 0 Then Locate , 63: Print "1.000";
    If (HW(K) + AW(K)) = 0 Then Locate , 64: Print ".000";
    If (HW(K) + AW(K)) > 0 And (HW(K) + AW(K)) < (HW(K) + AW(K) + HL(K) + AL(K)) Then Locate , 64: Print Using ".###"; ((HW(K) + AW(K)) / (HW(K) + AW(K) + HL(K) + AL(K)));
    If R1(K) + R3(K) = 0 Then Locate , 69: Print "  0.0";
    If R1(K) + R3(K) > 0 Then Locate , 69: Print Using "###.#"; (R1(K) + R3(K)) / (HW(K) + HL(K) + AW(K) + AL(K));
    If R2(K) + R4(K) = 0 Then Locate , 75: Print "  0.0";
    If R2(K) + R4(K) > 0 Then Locate , 75: Print Using "###.#"; (R2(K) + R4(K)) / (HW(K) + HL(K) + AW(K) + AL(K))
    Color 3: Print Tab(36); Chr$(179); Tab(58); Chr$(179);
    THW(RDD) = THW(RDD) + HW(K)
    THL(RDD) = THL(RDD) + HL(K)
    TAW(RDD) = TAW(RDD) + AW(K)
    TAL(RDD) = TAL(RDD) + AL(K)
    TR1(RDD) = TR1(RDD) + R1(K)
    TR2(RDD) = TR2(RDD) + R2(K)
    TR3(RDD) = TR3(RDD) + R3(K)
    TR4(RDD) = TR4(RDD) + R4(K)

250 Next K
Print
Color 11: Locate , 14: Print Using "##"; THW(RDD);
If THW(RDD) = THW(RDD) + THL(RDD) And THW(RDD) <> 0 Then Locate , 19: Print "1.000";
If THW(RDD) = 0 Then Locate , 20: Print ".000";
If THW(RDD) > 0 And THW(RDD) < THW(RDD) + THL(RDD) Then Locate , 20: Print Using ".###"; (THW(RDD) / (THW(RDD) + THL(RDD)));
If TR2(RDD) < 1 Then Locate , 32: Print " 0.0";
If TR2(RDD) > 0 Then Locate , 31: Print Using "###.#"; TR2(RDD) / (THW(RDD) + THL(RDD));
Locate , 38: Print Using "##"; TAL(RDD);
If TR3(RDD) < 1 Then Locate , 48: Print " 0.0";
If TR3(RDD) > 0 Then Locate , 47: Print Using "###.#"; TR3(RDD) / (TAW(RDD) + TAL(RDD));
Locate , 57: Print Using "###"; THW(RDD) + TAW(RDD);
Color 11: If THW(RDD) + TAW(RDD) = THW(RDD) + THL(RDD) + TAW(RDD) + TAL(RDD) And THW(RDD) + TAW(RDD) <> 0 Then Locate , 63: Print "1.000";
If THW(RDD) + TAW(RDD) = 0 Then Locate , 64: Print ".000";
If THW(RDD) + TAW(RDD) > 0 And THW(RDD) + TAW(RDD) < THW(RDD) + TAW(RDD) + THL(RDD) + TAL(RDD) Then Locate , 64: Print Using ".###"; (THW(RDD) + TAW(RDD)) / (THW(RDD) + TAW(RDD) + THL(RDD) + TAL(RDD));
If TR2(RDD) + TR4(RDD) < 1 Then Locate , 76: Print " 0.0"
If TR2(RDD) + TR4(RDD) > 0 Then Locate , 75: Print Using "###.#"; (TR2(RDD) + TR4(RDD)) / (THW(RDD) + THL(RDD) + TAW(RDD) + TAL(RDD))
Locate , 5: Print "TOTALS";
If TR1(RDD) < 1 Then Locate , 26: Print " 0.0";
If TR1(RDD) > 0 Then Locate , 25: Print Using "###.#"; TR1(RDD) / (THW(RDD) + THL(RDD));
Color 11: If TAW(RDD) = TAW(RDD) + TAL(RDD) And TAW(RDD) <> 0 Then Locate , 41: Print "1.000";
If TAW(RDD) = 0 Then Locate , 42: Print ".000";
If TAW(RDD) > 0 And TAW(RDD) < TAW(RDD) + TAL(RDD) Then Locate , 42: Print Using ".###"; (TAW(RDD) / (TAW(RDD) + TAL(RDD)));
If TR4(RDD) < 1 Then Locate , 54: Print " 0.0";
If TR4(RDD) > 0 Then Locate , 53: Print Using "###.#"; TR4(RDD) / (TAW(RDD) + TAL(RDD));
If TR1(RDD) + TR3(RDD) = 0 Then Locate , 70: Print " 0.0";
If TR1(RDD) + TR3(RDD) > 0 Then Locate , 69: Print Using "###.#"; (TR1(RDD) + TR3(RDD)) / (THW(RDD) + THL(RDD) + TAW(RDD) + TAL(RDD));
Locate , 16: Print Using "##"; THL(RDD);
Locate , 36: Print Using "##"; TAW(RDD);
Locate , 59: Print Using "###"; THL(RDD) + TAL(RDD)
Locate 20, 5: Color 14: Input "HIT ENTER TO CONTINUE"; DUMMY$
If RDD = 4 Then 400 Else GoTo 200

400 Cls: Color 12: Locate 2, 35: Color T%(0), T%(1): Print Z$: Color 0, 0
Color 3: Locate 4: Print Tab(9); String$(8, 196); " HOME "; String$(9, 196); Tab(32); Chr$(194); String$(8, 196); " AWAY "; String$(9, 196); Tab(56); Chr$(194); String$(7, 196); " TOTALS "; String$(8, 196)
    LOCATE 5: PRINT TAB(10); "W"; TAB(13); "L"; TAB(17); "PCT."; TAB(22); "PF/G"; TAB(28); "PA/G"; TAB(32); CHR$(179); TAB(34); "W"; TAB(37); "L"; TAB(41); "PCT."; TAB(46); "PF/G"; TAB(52); "PA/G"; TAB(56); CHR$(179); TAB(58); "W"; TAB(61); "L"; TAB _
(65); "PCT."; TAB(70); "PF/G"; TAB(76); "PA/G"
Color 3: Print Tab(32); Chr$(179); Tab(56); Chr$(179)
For W = 1 To 2
    If DV$(W) = "XXX" Or DV$(W) = "" Then 490 Else GoTo 461

    461 Color 15: Locate , 1: Print Left$(DV$(W), 10);
    Color 7: Locate , 9: Print Using "##"; THW(W);
    Locate , 12: Print Using "##"; THL(W);
    If THW(W) = THW(W) + THL(W) And THW(W) <> 0 Then Locate , 15: Print "1.000";
    If THW(W) = 0 Then Locate , 16: Print ".000";
    If THW(W) > 0 And THW(W) < THW(W) + THL(W) Then Locate , 16: Print Using ".###"; (THW(W) / (THW(W) + THL(W)));
    If TR1(W) = 0 Then Locate , 21: Print "  0.0";
    If TR1(W) > 0 Then Locate , 21: Print Using "###.#"; TR1(W) / (THW(W) + THL(W));
    If TR2(W) = 0 Then Locate , 27: Print "  0.0";
    If TR2(W) > 0 Then Locate , 27: Print Using "###.#"; TR2(W) / (THW(W) + THL(W));
    Color 3: Print Tab(32); Chr$(179);
    Color 7: Locate , 33: Print Using "##"; TAW(W);
    Locate , 36: Print Using "##"; TAL(W);
    If TAW(W) = TAW(W) + TAL(W) And TAW(W) <> 0 Then Locate , 39: Print "1.000";
    If TAW(W) = 0 Then Locate , 40: Print ".000";
    If TAW(W) > 0 And TAW(W) < TAW(W) + TAL(W) Then Locate , 40: Print Using ".###"; (TAW(W) / (TAW(W) + TAL(W)));
    If TR3(W) = 0 Then Locate , 45: Print "  0.0";
    If TR3(W) > 0 Then Locate , 45: Print Using "###.#"; TR3(W) / (TAW(W) + TAL(W));
    If TR4(W) = 0 Then Locate , 51: Print "  0.0";
    If TR4(W) > 0 Then Locate , 51: Print Using "###.#"; TR4(W) / (TAW(W) + TAL(W));
    Color 3: Print Tab(56); Chr$(179);
    Color 7: Locate , 57: Print Using "##"; THW(W) + TAW(W);
    Locate , 60: Print Using "##"; THL(W) + TAL(W);
    If THW(W) + TAW(W) = THW(W) + TAW(W) + THL(W) + TAL(W) And THW(W) + TAW(W) <> 0 Then Locate , 63: Print "1.000";
    If (THW(W) + TAW(W)) = 0 Then Locate , 64: Print ".000";
    If (THW(W) + TAW(W)) > 0 And THW(W) + TAW(W) < THW(W) + TAW(W) + THL(W) + TAL(W) Then Locate , 64: Print Using ".###"; ((THW(W) + TAW(W)) / (THW(W) + TAW(W) + THL(W) + TAL(W)));
    If TR1(W) + TR3(W) = 0 Then Locate , 69: Print "  0.0";
    If TR1(W) + TR3(W) > 0 Then Locate , 69: Print Using "###.#"; (TR1(W) + TR3(W)) / (THW(W) + THL(W) + TAW(W) + TAL(W));
    If TR2(W) + TR4(W) = 0 Then Locate , 75: Print "  0.0";
    If TR2(W) + TR4(W) > 0 Then Locate , 75: Print Using "###.#"; (TR2(W) + TR4(W)) / (THW(W) + THL(W) + TAW(W) + TAL(W))
    Color 3: Print Tab(32); Chr$(179); Tab(56); Chr$(179);

490 Next W
THWW = 0: THLL = 0: TAWW = 0: TALL = 0
For I = 1 To 4
    THWW = THWW + THW(I): THLL = THLL + THL(I): TAWW = TAWW + TAW(I): TALL = TALL + TAL(I)
Next
TR11 = 0: TR22 = 0: TR33 = 0: TR44 = 0
For I = 1 To 4
    TR11 = TR11 + TR1(I): TR22 = TR22 + TR2(I): TR33 = TR33 + TR3(I): TR44 = TR44 + TR4(I)
Next
Print
Color 11: Print "TOTALS";
Locate , 9: Print Using "##"; THWW;
Locate , 12: Print Using "##"; THLL;
If THWW = THWW + THLL And THWW <> 0 Then Locate , 15: Print "1.000";
If THWW = 0 Then Locate , 16: Print ".000";
If THWW > 0 And THWW < THWW + THLL Then Locate , 16: Print Using ".###"; THWW / (THWW + THLL);
If TR11 = 0 Then Locate , 21: Print "  0.0";
If TR11 > 0 Then Locate , 21: Print Using "###.#"; TR11 / (THWW + THLL);
If TR22 = 0 Then Locate , 27: Print "  0.0";
If TR22 > 0 Then Locate , 27: Print Using "###.#"; TR22 / (THWW + THLL);
Color 3: Print Tab(32); Chr$(179);
Color 11: Locate , 33: Print Using "##"; TAWW;
Locate , 36: Print Using "##"; TALL;
If TAWW = TAWW + TALL And TAWW <> 0 Then Locate , 39: Print "1.000";
If TAWW = 0 Then Locate , 40: Print ".000";
If TAWW > 0 And TAWW < TAWW + TALL Then Locate , 40: Print Using ".###"; TAWW / (TAWW + TALL);
If TR33 = 0 Then Locate , 45: Print "  0.0";
If TR33 > 0 Then Locate , 45: Print Using "###.#"; TR33 / (TAWW + TALL);
If TR44 = 0 Then Locate , 51: Print "  0.0";
If TR44 > 0 Then Locate , 51: Print Using "###.#"; TR44 / (TAWW + TALL);
Color 3: Print Tab(56); Chr$(179);
Color 11: Locate , 57: Print Using "##"; THWW + TAWW;
Locate , 60: Print Using "##"; THLL + TALL;
If THWW + TAWW = THWW + TAWW + THLL + TALL And THWW + TAWW <> 0 Then Locate , 63: Print "1.000";
If THWW + TAWW = 0 Then Locate , 64: Print ".000";
If THWW + TAWW > 0 And THWW + TAWW < THWW + TAWW + THLL + TALL Then Locate , 64: Print Using ".###"; (THWW + TAWW) / (THWW + TAWW + THLL + TALL);
If TR11 + TR33 = 0 Then Locate , 69: Print "  0.0";
If TR11 + TR33 > 0 Then Locate , 69: Print Using "###.#"; (TR11 + TR33) / (THWW + THLL + TAWW + TALL);
If TR22 + TR44 = 0 Then Locate , 75: Print "  0.0";
If TR22 + TR44 > 0 Then Locate , 75: Print Using "###.#"; (TR22 + TR44) / (THWW + THLL + TAWW + TALL)
I$ = GetKeyPress
GoTo 600
    

1200 LPrint Chr$(15)
'This is not supported in QB64
'WIDTH "LPT1:1", 130
DV$(1) = D1$
DV$(2) = D2$
LPrint Tab(55); Z$: LPrint
LPrint Tab(22); "--------- HOME ----------"; Tab(54); "--------- AWAY ----------"; Tab(86); "---------- TOTALS ----------"
LPrint Tab(22); "W"; Tab(26); "L"; Tab(30); "PCT."; Tab(36); "PF/G"; Tab(43); "PA/G"; Tab(54); "W"; Tab(58); "L"; Tab(62); "PCT."; Tab(68); "PF/G"; Tab(75); "PA/G"; Tab(86); "W"; Tab(91); "L"; Tab(97); "PCT."; Tab(103); "PF/G"; Tab(110); "PA/G"

1204 RDD = RDD + 1
If RDD = 1 Then 1210
If RDD = 2 Then 1211

1210 DD = 1: EE = 15: If DV$(RDD) = "XXX" Or DV$(RDD) = "" Then 1401 Else GoTo 1215

1211 DD = 16: EE = 30: If DV$(RDD) = "XXX" Or DV$(RDD) = "" Then 1401 Else GoTo 1215

1215 LPrint Tab(2); DV$(RDD)
For K = DD To EE
    If Z1$(K) = "XXX" Or Z1$(K) = TM$ Then 1250
    LPrint Z1$(K);
    LPrint Tab(22); Using "#"; HW(K);
    LPrint Tab(26); Using "#"; HL(K);
    If HW(K) = HW(K) + HL(K) And HW(K) <> 0 Then LPrint Tab(28); "1.000";
    If HW(K) = 0 Then LPrint Tab(29); ".000";
    If HW(K) > 0 And HW(K) < HW(K) + HL(K) Then LPrint Tab(29); Using ".###"; (HW(K) / (HW(K) + HL(K)));
    If R1(K) = 0 Then LPrint Tab(35); "  0.0";
    If R1(K) > 0 Then LPrint Tab(35); Using "###.#"; R1(K) / (HW(K) + HL(K));
    If R2(K) = 0 Then LPrint Tab(42); "  0.0";
    If R2(K) > 0 Then LPrint Tab(42); Using "###.#"; R2(K) / (HW(K) + HL(K));
    LPrint Tab(54); Using "#"; AW(K);
    LPrint Tab(58); Using "#"; AL(K);
    If AW(K) = AW(K) + AL(K) And AW(K) <> 0 Then LPrint Tab(60); "1.000";
    If AW(K) = 0 Then LPrint Tab(61); ".000";
    If AW(K) > 0 And AW(K) < AW(K) + AL(K) Then LPrint Tab(61); Using ".###"; (AW(K) / (AW(K) + AL(K)));
    If R3(K) = 0 Then LPrint Tab(67); "  0.0";
    If R3(K) > 0 Then LPrint Tab(67); Using "###.#"; R3(K) / (AW(K) + AL(K));
    If R4(K) = 0 Then LPrint Tab(74); "  0.0";
    If R4(K) > 0 Then LPrint Tab(74); Using "###.#"; R4(K) / (AW(K) + AL(K));
    LPrint Tab(86); Using "##"; HW(K) + AW(K);
    LPrint Tab(91); Using "##"; HL(K) + AL(K);
    If HW(K) + AW(K) = HW(K) + AW(K) + HL(K) + AL(K) And HW(K) + AW(K) <> 0 Then LPrint Tab(95); "1.000";
    If (HW(K) + AW(K)) = 0 Then LPrint Tab(96); ".000";
    If (HW(K) + AW(K)) > 0 And (HW(K) + AW(K)) < (HW(K) + AW(K) + HL(K) + AL(K)) Then LPrint Tab(96); Using ".###"; ((HW(K) + AW(K)) / (HW(K) + AW(K) + HL(K) + AL(K)));
    If R1(K) + R3(K) = 0 Then LPrint Tab(102); "  0.0";
    If R1(K) + R3(K) > 0 Then LPrint Tab(102); Using "###.#"; (R1(K) + R3(K)) / (HW(K) + HL(K) + AW(K) + AL(K));
    If R2(K) + R4(K) = 0 Then LPrint Tab(109); "  0.0"
    If R2(K) + R4(K) > 0 Then LPrint Tab(109); Using "###.#"; (R2(K) + R4(K)) / (HW(K) + HL(K) + AW(K) + AL(K))
    THW(RDD) = THW(RDD) + HW(K)
    THL(RDD) = THL(RDD) + HL(K)
    TAW(RDD) = TAW(RDD) + AW(K)
    TAL(RDD) = TAL(RDD) + AL(K)
    TR1(RDD) = TR1(RDD) + R1(K)
    TR2(RDD) = TR2(RDD) + R2(K)
    TR3(RDD) = TR3(RDD) + R3(K)
    TR4(RDD) = TR4(RDD) + R4(K)

1250 Next K
LPrint Tab(5); "TOTALS";
LPrint Tab(21); Using "##"; THW(RDD);
LPrint Tab(25); Using "##"; THL(RDD);
If THW(RDD) = THW(RDD) + THL(RDD) And THW(RDD) <> 0 Then LPrint Tab(28); "1.000";
If THW(RDD) = 0 Then LPrint Tab(29); ".000";
If THW(RDD) > 0 And THW(RDD) < THW(RDD) + THL(RDD) Then LPrint Tab(29); Using ".###"; (THW(RDD) / (THW(RDD) + THL(RDD)));
If TR1(RDD) = 0 Then LPrint Tab(35); "  0.0";
If TR1(RDD) > 0 Then LPrint Tab(35); Using "###.#"; TR1(RDD) / (THW(RDD) + THL(RDD));
If TR2(RDD) = 0 Then LPrint Tab(42); "  0.0";
If TR2(RDD) > 0 Then LPrint Tab(42); Using "###.#"; TR2(RDD) / (THW(RDD) + THL(RDD));
LPrint Tab(53); Using "##"; TAW(RDD);
LPrint Tab(57); Using "##"; TAL(RDD);
If TAW(RDD) = TAW(RDD) + TAL(RDD) And TAW(RDD) <> 0 Then LPrint Tab(60); "1.000";
If TAW(RDD) = 0 Then LPrint Tab(61); ".000";
If TAW(RDD) > 0 And TAW(RDD) < TAW(RDD) + TAL(RDD) Then LPrint Tab(61); Using ".###"; (TAW(RDD) / (TAW(RDD) + TAL(RDD)));
If TR3(RDD) = 0 Then LPrint Tab(67); "  0.0";
If TR3(RDD) > 0 Then LPrint Tab(67); Using "###.#"; TR3(RDD) / (TAW(RDD) + TAL(RDD));
If TR4(RDD) = 0 Then LPrint Tab(74); "  0.0";
If TR4(RDD) > 0 Then LPrint Tab(74); Using "###.#"; TR4(RDD) / (TAW(RDD) + TAL(RDD));
LPrint Tab(84); Using "###"; THW(RDD) + TAW(RDD);
LPrint Tab(89); Using "###"; THL(RDD) + TAL(RDD);
If THW(RDD) + TAW(RDD) = THW(RDD) + TAW(RDD) + THL(RDD) + TAL(RDD) And THW(RDD) + TAW(RDD) <> 0 Then LPrint Tab(95); "1.000";
If THW(RDD) + TAW(RDD) = 0 Then LPrint Tab(96); ".000";
If THW(RDD) + TAW(RDD) > 0 And THW(RDD) + TAW(RDD) < THW(RDD) + TAW(RDD) + THL(RDD) + TAL(RDD) Then LPrint Tab(96); Using ".###"; ((THW(RDD) + TAW(RDD)) / (THW(RDD) + TAW(RDD) + THL(RDD) + TAL(RDD)));
If TR1(RDD) + TR3(RDD) = 0 Then LPrint Tab(102); "  0.0";
If TR1(RDD) + TR3(RDD) > 0 Then LPrint Tab(102); Using "###.#"; (TR1(RDD) + TR3(RDD)) / (THW(RDD) + THL(RDD) + TAW(RDD) + TAL(RDD));
If TR2(RDD) + TR4(RDD) = 0 Then LPrint Tab(109); "  0.0"
If TR2(RDD) + TR4(RDD) > 0 Then LPrint Tab(109); Using "###.#"; (TR2(RDD) + TR4(RDD)) / (THW(RDD) + THL(RDD) + TAW(RDD) + TAL(RDD))
LPrint
If RDD = 2 Then 1401 Else GoTo 1204

1401 LPrint: LPrint: LPrint Tab(22); "--------- HOME ----------"; Tab(54); "--------- AWAY ----------"; Tab(86); "---------- TOTALS ----------"
LPrint Tab(22); "W"; Tab(26); "L"; Tab(30); "PCT."; Tab(36); "PF/G"; Tab(43); "PA/G"; Tab(54); "W"; Tab(58); "L"; Tab(62); "PCT."; Tab(68); "PF/G"; Tab(75); "PA/G"; Tab(86); "W"; Tab(91); "L"; Tab(97); "PCT."; Tab(103); "PF/G"; Tab(110); "PA/G"
LPrint
For W = 1 To 2
    If DV$(W) = "XXX" Or DV$(W) = "" Then 1490 Else GoTo 1461

    1461 LPrint Tab(2); DV$(W);
    LPrint Tab(21); Using "##"; THW(W);
    LPrint Tab(25); Using "##"; THL(W);
    If THW(W) = THW(W) + THL(W) And THW(W) <> 0 Then LPrint Tab(28); "1.000";
    If THW(W) = 0 Then LPrint Tab(29); ".000";
    If THW(W) > 0 And THW(W) < THW(W) + THL(W) Then LPrint Tab(29); Using ".###"; (THW(W) / (THW(W) + THL(W)));
    If TR1(W) = 0 Then LPrint Tab(35); "  0.0";
    If TR1(W) > 0 Then LPrint Tab(35); Using "###.#"; TR1(W) / (THW(W) + THL(W));
    If TR2(W) = 0 Then LPrint Tab(42); "  0.0";
    If TR2(W) > 0 Then LPrint Tab(42); Using "###.#"; TR2(W) / (THW(W) + THL(W));
    LPrint Tab(53); Using "##"; TAW(W);
    LPrint Tab(57); Using "##"; TAL(W);
    If TAW(W) = TAW(W) + TAL(W) And TAW(W) <> 0 Then LPrint Tab(60); "1.000";
    If TAW(W) = 0 Then LPrint Tab(61); ".000";
    If TAW(W) > 0 And TAW(W) < TAW(W) + TAL(W) Then LPrint Tab(61); Using ".###"; (TAW(W) / (TAW(W) + TAL(W)));
    If TR3(W) = 0 Then LPrint Tab(67); "  0.0";
    If TR3(W) > 0 Then LPrint Tab(67); Using "###.#"; TR3(W) / (TAW(W) + TAL(W));
    If TR4(W) = 0 Then LPrint Tab(74); "  0.0";
    If TR4(W) > 0 Then LPrint Tab(74); Using "###.#"; TR4(W) / (TAW(W) + TAL(W));
    LPrint Tab(84); Using "###"; THW(W) + TAW(W);
    LPrint Tab(89); Using "###"; THL(W) + TAL(W);
    If THW(W) + TAW(W) = THW(W) + TAW(W) + THL(W) + TAL(W) And THW(W) + TAW(W) <> 0 Then LPrint Tab(95); "1.000";
    If (THW(W) + TAW(W)) = 0 Then LPrint Tab(96); ".000";
    If (THW(W) + TAW(W)) > 0 And THW(W) + TAW(W) < THW(W) + TAW(W) + THL(W) + TAL(W) Then LPrint Tab(96); Using ".###"; ((THW(W) + TAW(W)) / (THW(W) + TAW(W) + THL(W) + TAL(W)));
    If TR1(W) + TR3(W) = 0 Then LPrint Tab(102); "  0.0";
    If TR1(W) + TR3(W) > 0 Then LPrint Tab(102); Using "###.#"; (TR1(W) + TR3(W)) / (THW(W) + THL(W) + TAW(W) + TAL(W));
    If TR2(W) + TR4(W) = 0 Then LPrint Tab(109); "  0.0";
    If TR2(W) + TR4(W) > 0 Then LPrint Tab(109); Using "###.#"; (TR2(W) + TR4(W)) / (THW(W) + THL(W) + TAW(W) + TAL(W))

1490 Next W
THWW = 0: THLL = 0: TAWW = 0: TALL = 0
For I = 1 To 4
    THWW = THWW + THW(I): THLL = THLL + THL(I): TAWW = TAWW + TAW(I): TALL = TALL + TAL(I)
Next
TR11 = 0: TR22 = 0: TR33 = 0: TR44 = 0
For I = 1 To 4
    TR11 = TR11 + TR1(I): TR22 = TR22 + TR2(I): TR33 = TR33 + TR3(I): TR44 = TR44 + TR4(I)
Next
LPrint Tab(5); "TOTALS";
LPrint Tab(20); Using "###"; THWW;
LPrint Tab(24); Using "###"; THLL;
If THWW = THWW + THLL And THWW <> 0 Then LPrint Tab(28); "1.000";
If THWW = 0 Then LPrint Tab(29); ".000";
If THWW > 0 And THWW < THWW + THLL Then LPrint Tab(29); Using ".###"; THWW / (THWW + THLL);
If TR11 = 0 Then LPrint Tab(35); "  0.0";
If TR11 > 0 Then LPrint Tab(35); Using "###.#"; TR11 / (THWW + THLL);
If TR22 = 0 Then LPrint Tab(42); "  0.0";
If TR22 > 0 Then LPrint Tab(42); Using "###.#"; TR22 / (THWW + THLL);
LPrint Tab(52); Using "###"; TAWW;
LPrint Tab(56); Using "###"; TALL;
If TAWW = TAWW + TALL And TAWW <> 0 Then LPrint Tab(60); "1.000";
If TAWW = 0 Then LPrint Tab(61); ".000";
If TAWW > 0 And TAWW < TAWW + TALL Then LPrint Tab(61); Using ".###"; TAWW / (TAWW + TALL);
If TR33 = 0 Then LPrint Tab(67); "  0.0";
If TR33 > 0 Then LPrint Tab(67); Using "###.#"; TR33 / (TAWW + TALL);
If TR44 = 0 Then LPrint Tab(74); "  0.0";
If TR44 > 0 Then LPrint Tab(74); Using "###.#"; TR44 / (TAWW + TALL);
LPrint Tab(84); Using "###"; THWW + TAWW;
LPrint Tab(89); Using "###"; THLL + TALL;
If THWW + TAWW = THWW + TAWW + THLL + TALL And THWW + TAWW <> 0 Then LPrint Tab(95); "1.000";
If THWW + TAWW = 0 Then LPrint Tab(96); ".000";
If THWW + TAWW > 0 And THWW + TAWW < THWW + TAWW + THLL + TALL Then LPrint Tab(96); Using ".###"; (THWW + TAWW) / (THWW + TAWW + THLL + TALL);
If TR11 + TR33 = 0 Then LPrint Tab(102); "  0.0";
If TR11 + TR33 > 0 Then LPrint Tab(102); Using "###.#"; (TR11 + TR33) / (THWW + THLL + TAWW + TALL);
If TR22 + TR44 = 0 Then LPrint Tab(109); "  0.0";
If TR22 + TR44 > 0 Then LPrint Tab(109); Using "###.#"; (TR22 + TR44) / (THWW + THLL + TAWW + TALL)
LPrint Chr$(18)
'This is not supported in QB64
'WIDTH "LPT1:1", 80
LPrint Chr$(12)
GoTo 600

3056 Color 15, 0: Cls
JJ = 6
SN = 1: Locate 6, 8: Color 15, 4
Print " Ö"; String$(61, 196); "· ";: For II = 1 To JJ: Locate 6 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 7 + JJ, 8
Print " Ó"; String$(61, 196); "½ ";: Locate 7, 32
Locate , 12: Print "ENTER DISK ID: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Print
Locate , 12: Print "ESC TO RETURN TO MAIN MENU"
3057 I$ = GetKeyPress$
If I$ = Chr$(27) Then 600
If I$ = Chr$(13) Then GoSub 1102: GoTo 3060
Locate 7, 27: Print I$
Locate 7, 28: Line Input NY$
NY$ = I$ + NY$
Color 15, 0

If NY$ = "" Then GoSub 1102: GoTo 3060
YN$ = NY$
For I = 1 To 500: VV$(I) = "": Next
GoSub 1101

3060 If BO% = 1 Then BO% = 0: GoTo 3056

3058 Locate 2, 33: Color T%(0), T%(1): Print QQ$(1)
Color 15, 4: Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 3056
Color 15, 0
Return

1101 Open D$(0) + "BASK." + YN$ For Random As #1 Len = 655
Y = LOF(1)
Field #1, 15 As Q$(0), 640 As X$
For I1 = 1 To Y / 655
    Get #1, I1
    VV$(I1) = Q$(0): VV%(I1) = I1
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
20 MAMenu VV$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If ExitCode% = 27 Then BO% = 1: Return
If ExitCode% = -71 Then Start% = 1: ExitCode% = 3: SELECTION% = 1: GoTo 20
If ExitCode% = -79 Then Start% = NTMS: ExitCode% = 3: SELECTION% = NTMS: GoTo 20
If ExitCode% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If ExitCode% = -81 Then Start% = Start% + 80: If Start% > NTMS Then Start% = NTMS
If ExitCode% = -81 Or ExitCode% = -73 Then ExitCode% = 3: SELECTION% = Start%: GoTo 20
T1 = VV%(SELECTION%)

9000 Open D$(0) + "BASK." + YN$ For Random As #1 Len = 655
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 2 AS Q$(16), 2 AS Q$(17), 2 AS Q$(18), 2 _
AS Q$(19), 602 AS X$
For I = 0 To 13
FIELD #1, 53 + I * 43 AS X$, 15 AS Q$(I * 15 + 20), 2 AS Q$(I * 15 + 21), 2 AS Q$(I * 15 + 22), 2 AS Q$(I * 15 + 23), 2 AS Q$(I * 15 + 24), 2 AS Q$(I * 15 + 25), 2 AS Q$(I * 15 + 26), 2 AS Q$(I * 15 + 27), 2 AS Q$(I * 15 + 28), 2 AS Q$(I * 15 + 29) _
, 2 AS Q$(I * 15 + 30), 2 AS Q$(I * 15 + 31), 2 AS Q$(I * 15 + 32), 2 AS Q$(I * 15 + 33), 2 AS Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 AS X$
Next
Get #1, T1
TM$ = Q$(0)

9005 If Right$(TM$, 1) = " " Then TM$ = Left$(TM$, Len(TM$) - 1): GoTo 9005
QQ$(1) = TM$
SP = CVI(Q$(1)): DA = CVI(Q$(2)): V1% = CVI(Q$(3)): V2% = CVI(Q$(4))
For I = 0 To 4: L%(I) = CVI(Q$(I + 5)): Next
For I = 0 To 9: T%(I) = CVI(Q$(I + 10)): Next
Close 1
Return

6585 For XX = 4 To 11: If Mid$(B$, XX, 1) = " " Or Mid$(B$, XX, 1) = "." Then Mid$(B$, XX, 1) = "_"
Next XX
For XX = 12 To 14: If Mid$(B$, XX, 2) = "ST" Then Mid$(B$, 9, 1) = "_": Mid$(B$, 10, 2) = "ST"
Next XX
QQ = 8: For XX = 11 To 4 Step -1: If Mid$(B$, XX, 1) = "_" Then QQ = XX - 4: GoTo 6615
    GoTo 6620

6615 Next XX

6620 Return

4500 Cls: J = 4: GoSub 2075: Locate 10, 18
Print "COMPILED STATS WERE NOT FOUND"
Locate , 18: Print "PRESS ANY KEY TO RETURN TO MAIN MENU"
I$ = GetKeyPress
Color , 0
GoTo 600

2075 '
Locate 8, 8: Color 15, 1: Print " Ö"; String$(61, 196); "· ";
For II = 1 To J: Locate 8 + II, 8: Print " º"; String$(61, 32); "º ";: Next
Locate 9 + J, 8: Print " Ó"; String$(61, 196); "½ ";: Locate 10, 15: Return

1440 Spec$ = D$(1) + "*.LIB"
null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then 4600
ReDim F$(0 To Count%)
For X = 1 To Count%
    F$(X) = Space$(12)
Next
F$(0) = Spec$
Call ReadFile(F$())
'CALL SortStr(VARPTR(F$(1)), Count%, 0)

1445 Cls
Color , 1
Cls
Locate , 28
Color 14
Print "SELECT LEAGUE": Color 15
SELECTION% = 1
Start% = 1
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Rows% = 20
Columns% = 4
Spacing% = 6
Row% = 4
Column% = 1
Locate 25, 1
Color 14
Print "ENTER";: Color 15
Print "-SELECTS LEAGUE";

_MouseShow "DEFAULT"
MAMenu F$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If ExitCode% = 27 Then 600
F$ = F$(SELECTION%)
Locate 2, 33: Print F$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3912 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1445

3913 F$ = RTrim$(F$)
F$ = Left$(F$, Len(F$) - 4)
Return

4600 Locate 10, 8
Print "NO LEAGUE FILES WERE FOUND"
Locate , 8: Print "PRESS ANY KEY TO RETURN TO MAIN MENU"
3806 I$ = GetKeyPress$
GoTo 600

Sub TITLE ()
    Color , 0: KEY Off: Cls
    Color , 1: Locate 1, 1: Print Tab(80);: Locate , 23: Color 12
    Print "COURTSIDE COLLEGE BASKETBALL V4.0"
    Locate 24, 1: Print Tab(80);: Locate , 1
    Print "(C) LANCE HAFFNER/SHANNON LYNN 1987-1994"; Tab(70); Left$(Time$, 5);
    Locate 3, 1: Color 2, 0
    Color 15: Print "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
    Print "     FOR TECHNICAL SUPPORT, CALL (615) 366-8088 OR SportGames@AOL.COM"
    Color 10
End Sub

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
