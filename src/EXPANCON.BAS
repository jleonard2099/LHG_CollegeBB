'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Courtside College Basketball - Expanded Leaders"
''$ExeIcon:'./lhg_ccb.ico'
'_Icon
_AllowFullScreen

DECLARE SUB TITLE ()
DECLARE SUB BUILDSTATS ()

Dim TP$(31), TP1$(39)
Dim menuItems$(4)
Dim Shared A$(15), AL$(600), D$(1), TMA$(600), TT$(40, 15), TT1$(40, 15), Z1$(40)

Dim Shared GM(40), GM1(40), TT(40, 15), TT1(40, 15)
Dim Shared W0(15), W1(15), W0L(600), W1L(600), Z(15), Z1(15)

Dim Shared JB, TS, TTS, TMR

ReDim TMM(600) As String * 15, TPP(600) As String * 15
ReDim AL(600, 14), TYP(600)
ReDim Shared GMA(600)

Cls

K = 0: K1 = 0: PK = 0: PK1 = 0: JB = 0: JP = 0: TBN = 0: TPN = 0: YY = 0: TMR = 1280: TS = 0: PV = 0

Open "DEV.BAS" For Input As #2: For I = 0 To 1: Input #2, D$(I): Next: Close 2

Data GAMES,MINUTES,FIELD GOALS MADE,FIELD GOAL ATTEMPS,F.G. PCT. (3.65 FGM/GAME),THREE POINTS MADE,THREE POINT ATTEMPTS,3FG (.30 3FGM/GAME),FREE THROWS MADE,FREE THROWS ATTEMPTED,F.T. PCT. (1.52 FTM/GAME)
Data POINTS,SCORING AVERAGE,HI-GAME,OFFENSIVE REBOUNDS,DEFENSIVE REBOUNDS,TOTAL REBOUNDS,AVERAGE REBOUNDS PER GAME,FOULS,ASSISTS,BLOCKED SHOTS,STEALS,TURNOVERS
Data POINTS/48 MINUTES,REBOUNDS/48 MINUTES,FOULS/48 MINUTES,ASSISTS/48 MINUTES,BLOCKS/48 MINUTES,STEALS/48 MINUTES,TURNOVERS/48 MINUTES,MINUTES/GAME,GO TO MAIN MENU
For I = 0 To 31: Read TP$(I): Next

Data FIELD GOAL ATTEMPTS,FIELD GOALS MADE,F.G. PCT.,THREE POINT ATTEMPTS,THREE POINTS MADE,THREE POINT PERCENTAGE,FREE THROWS ATTEMPTED,FREE THROWS MADE,FREE THROW PERCENTAGE,OFFENSIVE REBOUNDS
Data DEFENSIVE REBOUNDS,TOTAL REBOUNDS,REBOUNDS PER GAME,FOULS,ASSISTS,BLOCKS,STEALS,TURNOVERS,POINTS PER GAME
Data OPP. FIELD GOAL ATTEMPTS,OPP. FIELD GOALS MADE,OPP. FIELD GOAL PERCENTAGE,OPP. THREE POINTERS ATTEMPTED,OPP. THREE POINTERS MADE,OPP. THREE POINT PERCENTAGE,OPP. FREE THROWS ATTEMPTED,OPP. FREE THROWS MADE,OPP. FREE THROW PERCENTAGE
Data OPP. OFFENSIVE REBOUNDS,OPP. DEFENSIVE REBOUNDS,OPP. TOTAL REBOUNDS,OPP. REBOUNDS PER GAME,OPP. FOULS,OPP. ASSISTS,OPP. BLOCKS,OPP. STEALS,OPP. TURNOVERS,OPP. POINTS PER GAME,RETURN TO MAIN MENU
For I = 0 To 38: Read TP1$(I): Next

5 Call TITLE
Erase menuItems$
menuItems$(1) = "VIEW EXPANDED LEAGUE STATISTICS "
menuItems$(2) = "RETURN TO LEAGUE LEADER MENU    "
SELECTION% = 1
Count% = 2
Start% = 1
SCANCODE% = 2
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 25

CALL box0(7, 24, 10, 57, 2,  15, 1)
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Or SELECTION% = 2 Then Run "HELLO.EXE 3"

2500 For X = 1 To 40
    Z1$(X) = "XXX"
Next
Spec$ = D$(1) + "*.TMS": GoSub 1440
LG$ = targetFile$
Open D$(1) + targetFile$ + ".TMS" For Input As #2
Input #2, LG$
Input #2, D1$: Input #2, D2$
For X = 1 To 30
    Input #2, Z1$(X)
Next
Close 2
Cls
Call BUILDSTATS

70 Call TITLE
Erase menuItems$
menuItems$(1) = "VIEW INDIVIDUAL PLAYER LEADERS  "
menuItems$(2) = "VIEW TEAM TOTALS                "
menuItems$(3) = "RETURN TO LEAGUE LEADER MENU    "
SELECTION% = 1
Count% = 3
Start% = 1
SCANCODE% = 3
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 25

CALL box0(7, 24, 11, 57, 2,  15, 1)

_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Or SELECTION% = 3 Then 5
If SELECTION% = 1 Then 80
If SELECTION% = 2 Then 2880

80 Color , 0: Cls: Print: Print: Print: Print: J = 20: ZZ = 1
'
Locate 2, 4: Color 15, 4
Print " " + Chr$(214); String$(66, 196); "· ";
For II = 1 To J: Locate 2 + II, 4: Print " " + Chr$(186); String$(66, 32); Chr$(186) + " ";: Next
Locate 3 + J, 4: Print " " + Chr$(211); String$(66, 196); Chr$(189) + " ";
Locate 10, 15

200 For I = 0 To 15: Color 14: Locate I + 3, 8: Print Using "##"; I;: Print " ";: Color 0: Print TP$(I);: Color 14: Print Tab(40); Using "##"; I + 16;: Print " ";: Color 0: Print TP$(I + 16): Next
Print: Locate , 23: Input "SELECT NUMBER TO VIEW"; B9
If B9 > 31 Then 200
If B9 = 0 Then X = 1: stpl = 0: PPP = 0: GoSub 5000: GoTo 400
If B9 = 1 Then X = 11: stpl = 0: PPP = 0: GoSub 5000: GoTo 420
If B9 = 2 Then X = 3: stpl = 0: PPP = 0: GoSub 5000: GoTo 400
If B9 = 3 Then X = 2: stpl = 0: PPP = 0: GoSub 5000: GoTo 400
If B9 = 4 Then X = 3: X1 = 2: stpl = 0: PPP = 0: GoSub 5000: GoTo 450
If B9 = 5 Then stpl = 0: PPP = 0: GoSub 5000: GoTo 660
If B9 = 6 Then stpl = 0: PPP = 0: GoSub 5000: GoTo 680
If B9 = 7 Then stpl = 0: PPP = 0: GoSub 5000: GoTo 480
If B9 = 8 Then X = 5: stpl = 0: PPP = 0: GoSub 5000: GoTo 400
If B9 = 9 Then X = 4: stpl = 0: PPP = 0: GoSub 5000: GoTo 400
If B9 = 10 Then X = 5: X1 = 4: stpl = 0: PPP = 0: GoSub 5000: GoTo 950
If B9 = 11 Then X = 3: X1 = 5: stpl = 0: PPP = 0: GoSub 5000: GoTo 500
If B9 = 12 Then X = 3: X1 = 5: X2 = 1: stpl = 0: PPP = 0: GoSub 5000: GoTo 580
If B9 = 13 Then X = 14: stpl = 0: PPP = 0: GoSub 5000: GoTo 400
If B9 = 14 Then X = 13: stpl = 0: PPP = 0: GoSub 5000: GoTo 400
If B9 = 15 Then X = 6: X1 = 13: stpl = 0: PPP = 0: GoSub 5000: GoTo 620
If B9 = 16 Then X = 6: stpl = 0: PPP = 0: GoSub 5000: GoTo 400
If B9 = 17 Then X = 6: X1 = 1: stpl = 0: PPP = 0: GoSub 5000: GoTo 640
If B9 = 18 Then X = 7: stpl = 0: PPP = 0: GoSub 5000: GoTo 400
If B9 = 19 Then X = 9: stpl = 0: PPP = 0: GoSub 5000: GoTo 400
If B9 = 20 Then X = 10: stpl = 0: PPP = 0: GoSub 5000: GoTo 400
If B9 = 21 Then X = 8: stpl = 0: PPP = 0: GoSub 5000: GoTo 400
If B9 = 22 Then X = 12: stpl = 0: PPP = 0: GoSub 5000: GoTo 400
If B9 = 23 Then X = 3: X1 = 5: X2 = 11: stpl = 0: PPP = 0: GoSub 5000: GoTo 520
If B9 = 24 Then X = 6: X1 = 11: stpl = 0: PPP = 0: GoSub 5000: GoTo 540
If B9 = 25 Then X = 7: X1 = 11: stpl = 0: PPP = 0: GoSub 5000: GoTo 540
If B9 = 26 Then X = 9: X1 = 11: stpl = 0: PPP = 0: GoSub 5000: GoTo 540
If B9 = 27 Then X = 10: X1 = 11: stpl = 0: PPP = 0: GoSub 5000: GoTo 540
If B9 = 28 Then X = 8: X1 = 11: stpl = 0: PPP = 0: GoSub 5000: GoTo 540
If B9 = 29 Then X = 12: X1 = 11: stpl = 0: PPP = 0: GoSub 5000: GoTo 540
If B9 = 30 Then X = 11: X1 = 1: stpl = 0: PPP = 0: GoSub 5000: GoTo 560
If B9 = 31 Then 70

400 Cls: PPP = 0
For I = 1 To JB
    If AL(I, X) = 0 Then 409 Else GoTo 405

    405 PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = AL(I, X)

409 Next I
GoSub 5200
TVN = 1: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

420 Cls: PPP = 0
For I = 1 To JB
    If AL(I, X) = 0 Then 429 Else GoTo 425

    425 PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = (AL(I, X) + 30) / 60

429 Next I
GoSub 5200
TVN = 1: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

450 Cls: PPP = 0
For I = 1 To JB
    If AL(I, X) < GMA(I) * 3.65 Then 459 Else GoTo 455

    455 PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = AL(I, X) / AL(I, X1): GoTo 459

459 Next I
GoSub 5200
TVN = 2: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

480 Cls: PPP = 0
For I = 1 To JB
    If W0L(I) < GMA(I) * .3 Then 489 Else GoTo 485

    485 PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = W0L(I) / W1L(I): GoTo 489

489 Next I
GoSub 5200
TVN = 2: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

500 Cls: PPP = 0
For I = 1 To JB
    If (AL(I, X) * 2) + AL(I, X1) + W0L(I) = 0 Then 509 Else GoTo 505

    505 PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = (AL(I, X) * 2) + AL(I, X1) + W0L(I): GoTo 509
    TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = 0

509 Next I
GoSub 5200
TVN = 1: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

520 Cls: PPP = 0
For I = 1 To JB
    If (AL(I, X) * 2) + AL(I, X1) + W0L(I) = 0 Or AL(I, X2) = 0 Then 529 Else GoTo 525

    525 PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = (((AL(I, X) * 2) + AL(I, X1) + W0L(I))) * 48 / ((AL(I, X2) + 30) / 60)

529 Next I
GoSub 5200
TVN = 3: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

540 Cls: PPP = 0
For I = 1 To JB
    If AL(I, X) = 0 Or AL(I, X1) = 0 Then 549 Else GoTo 545

    545 PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = (AL(I, X)) * 48 / ((AL(I, X1) + 30) / 60)

549 Next I
GoSub 5200
TVN = 3: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

560 Cls: PPP = 0
For I = 1 To JB
    If AL(I, X) = 0 Or AL(I, X1) = 0 Then 569 Else GoTo 565

    565 PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = ((AL(I, X) + 30) / 60) / AL(I, X1)

569 Next I
GoSub 5200
TVN = 3: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

580 Cls: PPP = 0
For I = 1 To JB
    If (AL(I, X) * 2) + AL(I, X1) + W0L(I) = 0 Or AL(I, X2) = 0 Then 589 Else GoTo 585

    585 PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = ((AL(I, X) * 2) + AL(I, X1) + W0L(I)) / AL(I, X2)

589 Next I
GoSub 5200
TVN = 3: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

620 Cls: PPP = 0
For I = 1 To JB
    If AL(I, X) - AL(I, X1) = 0 Then 629 Else GoTo 625

    625 PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = AL(I, X) - AL(I, X1)

629 Next I
GoSub 5200
TVN = 1: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

640 Cls: PPP = 0
For I = 1 To JB
    If AL(I, X) = 0 Then 644 Else GoTo 642

    642 PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = AL(I, X) / AL(I, X1)

644 Next I
GoSub 5200
TVN = 3: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

660 Cls: PPP = 0
For I = 1 To JB
    If W0L(I) = 0 Then 669 Else GoTo 665

    665 PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = W0L(I)

669 Next I
GoSub 5200
TVN = 1: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

680 Cls: PPP = 0
For I = 1 To JB
    If W1L(I) = 0 Then 689 Else GoTo 685

    685 PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = W1L(I)

689 Next I
GoSub 5200
TVN = 1: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

700 Color 14, 0: Cls: Print "     PLAYER                  TEAM                     "; TP$(B9)
For W = 1 To QW
    If W = 20 Or W = 40 Or W = 60 Or W = 80 Or W = 100 Or W = 120 Or W = 140 Or W = 160 Or W = 180 Or W = 200 Then GoSub 731: GoSub 743
    If W = 220 Or W = 240 Or W = 260 Or W = 280 Or W = 300 Or W = 320 Or W = 340 Or W = 360 Or W = 380 Or W = 400 Then GoSub 731: GoSub 743
    If W = 420 Or W = 440 Or W = 460 Or W = 480 Or W = 500 Or W = 520 Or W = 540 Or W = 560 Or W = 580 Or W = 600 Then GoSub 731: GoSub 743
    If W = 620 Or W = 640 Or W = 660 Then GoSub 731: GoSub 743
    If TVN = 1 Then 709
    If TVN = 2 Then 708
    If TVN = 3 Then 707

    707 Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TPP$(W);: Color 15: Print Tab(25); TMM$(W);: Color 15: Print Tab(58); Using "###.#"; TYP(W): GoTo 710

    708 Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TPP$(W);: Color 15: Print Tab(25); TMM$(W);: Color 15: Print Tab(58); Using ".###"; TYP(W): GoTo 710

    709 Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TPP$(W);: Color 15: Print Tab(25); TMM$(W);: Color 15: Print Tab(58); Using "#####"; TYP(W)

710 Next W
Locate 22: Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
712 I$ = GetKeyPress$
If I$ = Chr$(27) Then 80
Locate 22: Print "                                                                "
Locate 22: Color 14: Print "VIEW AGAIN (Y/N)"
716 I$ = GetKeyPress$
If I$ = "Y" Then 700
If I$ = "N" Then 719

719 GoTo 80

731 Locate 22: Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
732 I$ = GetKeyPress$
If I$ = Chr$(27) Then 80
Return

743 Cls: Color 14: Print "     PLAYER                  TEAM                     "; TP$(B9)
Return

850 LPrint Chr$(15)
'This is not supported by QB64
'WIDTH "LPT1:1", 130
Color 14, 0: Cls: LPrint "     PLAYER                  TEAM                     "; TP$(B9)
For W = 1 To LG
    If W = 50 Or W = 100 Or W = 150 Or W = 200 Then GoSub 943
    If W = 250 Or W = 300 Or W = 350 Or W = 400 Then GoSub 943
    If W = 450 Or W = 500 Or W = 550 Or W = 600 Then GoSub 943
    If W = 650 Or W = 700 Or W = 750 Or W = 800 Then GoSub 943
    If W = 850 Or W = 900 Then GoSub 943
    If TVN = 1 Then 909
    If TVN = 2 Then 908
    If TVN = 3 Then 907

    907 LPrint Using "###"; W;: LPrint " ";: LPrint TPP$(W);: LPrint Tab(25); TMM$(W);: LPrint Tab(58); Using "###.#"; TYP(W): GoTo 910

    908 LPrint Using "###"; W;: LPrint " ";: LPrint TPP$(W);: LPrint Tab(25); TMM$(W);: LPrint Tab(58); Using ".###"; TYP(W): GoTo 910

    909 LPrint Using "###"; W;: LPrint " ";: LPrint TPP$(W);: LPrint Tab(25); TMM$(W);: LPrint Tab(58); Using "#####"; TYP(W)

910 Next W
LPrint Chr$(18)
'This is not supported in QB64
'WIDTH "LPT1:1", 80
GoTo 70

943 LPrint Chr$(12)
LPrint "     PLAYER                  TEAM                     "; TP$(XX)
Return

950 Cls: PPP = 0
For I = 1 To JB
    If AL(I, X) < GMA(I) * 1.52 Then 959 Else GoTo 955

    955 PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = AL(I, X) / AL(I, X1): GoTo 959

959 Next I
GoSub 5200
TVN = 2: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

2880 Color , 0: Cls: Print: Print: Print: Print: J = 21: ZZ = 3
'
Locate 1, 4: Color 15, 6: Print " " + Chr$(214); String$(66, 196); "· ";
For II = 1 To J: Locate 1 + II, 4: Print " " + Chr$(186); String$(66, 32); Chr$(186) + " ";: Next
Locate 2 + J, 4: Print " " + Chr$(211); String$(66, 196); Chr$(189) + " ";: Locate 10, 15
For I = 0 To 19: Locate I + 2, 8: Color 11: Print Using "##"; I;: Print " ";: Color 0: Print TP1$(I): Next
For I = 20 To 38: Locate I - 18, 38: Color 11: Print Using "##"; I;: Print " ";: Color 0: Print TP1$(I): Next

3050 Color 14: Locate 22, 23: Input "SELECT NUMBER TO VIEW"; B9
If B9 > 38 Then 3050
If B9 = 0 Then X = 1: stpl = 0: GoSub 5000: GoTo 3500
If B9 = 1 Then X = 2: stpl = 0: GoSub 5000: GoTo 3500
If B9 = 2 Then X = 2: X1 = 1: stpl = 0: GoSub 5000: GoTo 3510
If B9 = 3 Then X = 14: stpl = 0: GoSub 5000: GoTo 3500
If B9 = 4 Then X = 13: stpl = 0: GoSub 5000: GoTo 3500
If B9 = 5 Then X = 13: X1 = 14: stpl = 0: GoSub 5000: GoTo 3510
If B9 = 6 Then X = 3: stpl = 0: GoSub 5000: GoTo 3500
If B9 = 7 Then X = 4: stpl = 0: GoSub 5000: GoTo 3500
If B9 = 8 Then X = 4: X1 = 3: stpl = 0: GoSub 5000: GoTo 3510
If B9 = 9 Then X = 12: stpl = 0: GoSub 5000: GoTo 3500
If B9 = 10 Then X = 5: X1 = 12: stpl = 0: GoSub 5000: GoTo 3520
If B9 = 11 Then X = 5: stpl = 0: GoSub 5000: GoTo 3500
If B9 = 12 Then X = 5: stpl = 0: GoSub 5000: GoTo 3530
If B9 = 13 Then X = 6: stpl = 0: GoSub 5000: GoTo 3500
If B9 = 14 Then X = 8: stpl = 0: GoSub 5000: GoTo 3500
If B9 = 15 Then X = 9: stpl = 0: GoSub 5000: GoTo 3500
If B9 = 16 Then X = 7: stpl = 0: GoSub 5000: GoTo 3500
If B9 = 17 Then X = 11: stpl = 0: GoSub 5000: GoTo 3500
If B9 = 18 Then X = 2: X1 = 13: X2 = 4: stpl = 0: GoSub 5000: GoTo 3540
If B9 = 19 Then X = 1: stpl = 0: GoSub 5000: GoTo 3600
If B9 = 20 Then X = 2: stpl = 0: GoSub 5000: GoTo 3600
If B9 = 21 Then X = 2: X1 = 1: stpl = 0: GoSub 5000: GoTo 3610
If B9 = 22 Then X = 14: stpl = 0: GoSub 5000: GoTo 3600
If B9 = 23 Then X = 13: stpl = 0: GoSub 5000: GoTo 3600
If B9 = 24 Then X = 13: X1 = 14: stpl = 0: GoSub 5000: GoTo 3610
If B9 = 25 Then X = 3: stpl = 0: GoSub 5000: GoTo 3600
If B9 = 26 Then X = 4: stpl = 0: GoSub 5000: GoTo 3600
If B9 = 27 Then X = 4: X1 = 3: stpl = 0: GoSub 5000: GoTo 3610
If B9 = 28 Then X = 12: stpl = 0: GoSub 5000: GoTo 3600
If B9 = 29 Then X = 5: X1 = 12: stpl = 0: GoSub 5000: GoTo 3620
If B9 = 30 Then X = 5: stpl = 0: GoSub 5000: GoTo 3600
If B9 = 31 Then X = 5: stpl = 0: GoSub 5000: GoTo 3630
If B9 = 32 Then X = 6: stpl = 0: GoSub 5000: GoTo 3600
If B9 = 33 Then X = 8: stpl = 0: GoSub 5000: GoTo 3600
If B9 = 34 Then X = 9: stpl = 0: GoSub 5000: GoTo 3600
If B9 = 35 Then X = 7: stpl = 0: GoSub 5000: GoTo 3600
If B9 = 36 Then X = 11: stpl = 0: GoSub 5000: GoTo 3600
If B9 = 37 Then X = 2: X1 = 13: X2 = 4: stpl = 0: GoSub 5000: GoTo 3640
If B9 = 38 Then GoTo 70

3500 Cls: For I = 1 To TS
TMM$(I) = TT$(I, X): TYP(I) = TT(I, X): Next
GoSub 5500
TVN = 1: QW = TS
If PV = 1 Then 3650
If PV = 2 Then 3850

3510 Cls: For I = 1 To TS
    If TT(I, X1) = 0 Then 3513 Else GoTo 3512

    3512 TMM$(I) = TT$(I, X): TYP(I) = TT(I, X) / TT(I, X1): GoTo 3514

    3513 TMM$(I) = TT$(I, X): TYP(I) = 0

3514 Next
GoSub 5500
TVN = 2: QW = TS
If PV = 1 Then 3650
If PV = 2 Then 3850

3520 Cls: For I = 1 To TS
TMM$(I) = TT$(I, X): TYP(I) = TT(I, X) - TT(I, X1): Next
GoSub 5500
TVN = 1: QW = TS
If PV = 1 Then 3650
If PV = 2 Then 3850

3530 Cls: For I = 1 To TS
TMM$(I) = TT$(I, X): TYP(I) = TT(I, X) / GM(I): Next
GoSub 5500
TVN = 3: QW = TS
If PV = 1 Then 3650
If PV = 2 Then 3850

3540 Cls: For I = 1 To TS
TMM$(I) = TT$(I, X): TYP(I) = ((TT(I, X) * 2) + TT(I, X1) + TT(I, X2)) / GM(I): Next
GoSub 5500
TVN = 3: QW = TS
If PV = 1 Then 3650
If PV = 2 Then 3850

3600 Cls: For I = 1 To TTS
TMM$(I) = TT1$(I, X): TYP(I) = TT1(I, X): Next
GoSub 5600
TVN = 1: QW = TTS
If PV = 1 Then 4650
If PV = 2 Then 4850

3610 Cls: For I = 1 To TTS
    If TT1(I, X1) = 0 Then 3613 Else GoTo 3612

    3612 TMM$(I) = TT1$(I, X): TYP(I) = TT1(I, X) / TT1(I, X1): GoTo 3614

    3613 TMM$(I) = TT1$(I, X): TYP(I) = 0

3614 Next
GoSub 5600
TVN = 2: QW = TTS
If PV = 1 Then 4650
If PV = 2 Then 4850

3620 Cls: For I = 1 To TTS
TMM$(I) = TT1$(I, X): TYP(I) = TT1(I, X) - TT1(I, X1): Next
GoSub 5600
TVN = 1: QW = TTS
If PV = 1 Then 4650
If PV = 2 Then 4850

3630 Cls: For I = 1 To TTS
TMM$(I) = TT1$(I, X): TYP(I) = TT1(I, X) / GM1(I): Next
GoSub 5600
TVN = 3: QW = TTS
If PV = 1 Then 4650
If PV = 2 Then 4850

3640 Cls: For I = 1 To TTS
TMM$(I) = TT1$(I, X): TYP(I) = ((TT1(I, X) * 2) + TT1(I, X1) + TT1(I, X2)) / GM1(I): Next
GoSub 5600
TVN = 3: QW = TTS
If PV = 1 Then 4650
If PV = 2 Then 4850

3650 Color 14, 0: Cls:: Print "     TEAM                     "; TP1$(B9)
For W = 1 To QW
    If W = 20 Or W = 40 Then GoSub 3731: GoSub 3743
    If TVN = 1 Then 3709
    If TVN = 2 Then 3708
    If TVN = 3 Then 3707

    3707 Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TMM$(W);: Color 15: Print Tab(33); Using "###.#"; TYP(W): GoTo 3710

    3708 Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TMM$(W);: Color 15: Print Tab(33); Using ".###"; TYP(W): GoTo 3710

    3709 Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TMM$(W);: Color 15: Print Tab(32); Using "#####"; TYP(W)

3710 Next W
Locate 22: Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
3712 I$ = GetKeyPress$
If I$ = Chr$(27) Then 2880
Locate 22: Print "                                                                "
Locate 22: Color 14: Print "VIEW AGAIN (Y/N)"
3716 I$ = GetKeyPress$
If I$ = "Y" Then 3650
If I$ = "N" Then 3719

3719 GoTo 2880

3731 Locate 22: Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
3732 I$ = GetKeyPress$
If I$ = Chr$(27) Then 2880
Return

3743 Cls: Color 14: Print "     TEAM                     "; TP1$(B9)
Return

3850 LPrint Chr$(15)
'This is not supported in QB64
'WIDTH "LPT1:1", 130
LPrint "     TEAM                     "; TP1$(B9)
For W = 1 To QW
    If TVN = 1 Then 3909
    If TVN = 2 Then 3908
    If TVN = 3 Then 3907

    3907 Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TMM$(W);: Color 15: Print Tab(33); Using "###.#"; TYP(W): GoTo 3710

    3908 Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TMM$(W);: Color 15: Print Tab(33); Using ".###"; TYP(W): GoTo 3710

    3909 Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TMM$(W);: Color 15: Print Tab(32); Using "#####"; TYP(W)
Next W
LPrint Chr$(12)
LPrint Chr$(18)
'This is not supported in QB64
'WIDTH "LPT1:1", 80
GoTo 70

4650 Color 14, 0: Cls: Print "     TEAM                     "; TP1$(B9)
For W = 1 To QW
    If W = 20 Or W = 40 Then GoSub 4731: GoSub 4743
    If TVN = 1 Then 4709
    If TVN = 2 Then 4708
    If TVN = 3 Then 4707

    4707 Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TMM$(W);: Color 15: Print Tab(33); Using "###.#"; TYP(W): GoTo 4710

    4708 Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TMM$(W);: Color 15: Print Tab(33); Using ".###"; TYP(W): GoTo 4710

    4709 Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TMM$(W);: Color 15: Print Tab(32); Using "#####"; TYP(W)

4710 Next W
Locate 22: Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
4712 I$ = GetKeyPress$
If I$ = Chr$(27) Then 2880
Locate 22: Print "                                                                "
Locate 22: Color 14: Print "VIEW AGAIN (Y/N)"
4716 I$ = GetKeyPress$
If I$ = "Y" Then 4650
If I$ = "N" Then 4719

4719 GoTo 2880

4731 Locate 22: Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
4732 I$ = GetKeyPress$
If I$ = Chr$(27) Then 2880
Return

4743 Cls: Color 14: Print "     TEAM                     "; TP1$(B9)
Return

4850 LPrint Chr$(15)
'WIDTH "LPT1:1", 130
LPrint "     TEAM                     "; TP1$(B9)
For W = 1 To QW
    If TVN = 1 Then 4909
    If TVN = 2 Then 4908
    If TVN = 3 Then 4907

    4907 Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TMM$(W);: Color 15: Print Tab(33); Using "###.#"; TYP(W): GoTo 4710

    4908 Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TMM$(W);: Color 15: Print Tab(33); Using ".###"; TYP(W): GoTo 4710

    4909 Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TMM$(W);: Color 15: Print Tab(32); Using "#####"; TYP(W)
Next W
LPrint Chr$(12)
LPrint Chr$(18)
'This is not supported in QB64
'WIDTH "LPT1:1", 80
GoTo 70

5000 J = 6: GoSub 5007: Color 3: Locate , 30: Print "SELECT OPTION:"
Locate , 30: Color 15: Print "H";: Color 7: Print "IGHEST TO LOWEST"
Locate , 30: Color 15: Print "L";: Color 7: Print "OWEST TO HIGHEST"
Locate , 30: Color 15: Print "R";: Color 7: Print "E-SELECT CATEGORY"
5002 I$ = GetKeyPress$
If I$ = "H" Then GoSub 7000: GoTo 5015
If I$ = "L" Then GoSub 7000: GoTo 5016
If I$ = "R" Then GoTo 5017
If I$ <> "H" And I$ <> "L" And I$ <> "R" Then 5002

5007 '
Locate 8, 8: Color 15, 1: Print " " + Chr$(214); String$(57, 196); "· ";
For II = 1 To J: Locate 8 + II, 8: Print " " + Chr$(186); String$(57, 32); Chr$(186) + " ";: Next
Locate 9 + J, 8: Print " " + Chr$(211); String$(57, 196); Chr$(189) + " ";: Locate 10, 15: Return

5015 JX = 1: GoTo 5030

5016 JX = 2: GoTo 5030

5017 If ZZ = 1 GoTo 80
If ZZ = 3 GoTo 2880

5030 Return

5200 For I = 2 To PPP: stpl = stpl + 1: Locate 5, 5: Print "sorting player "; stpl
    For J = PPP To I Step -1
        If JX = 1 Then 5264
        If JX = 2 Then 5265

        5264 If TYP(J - 1) < TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
        GoTo 5266

        5265 If TYP(J - 1) > TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)

    5266 Next J
Next I
Return
For I = 2 To PPP: stpl = stpl + 1: Locate 5, 5: Print "sorting player "; stpl
    For J = PPP To I Step -1
        If JX = 1 Then 5414
        If JX = 2 Then 5415

        5414 If TYP(J - 1) < TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
        GoTo 5416

        5415 If TYP(J - 1) > TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)

    5416 Next J
Next I
Return

5500 For I = 2 To TS: stpl = stpl + 1: Locate 5, 5: Print "sorting team "; stpl
    For J = TS To I Step -1
        If JX = 1 Then 5505
        If JX = 2 Then 5506

        5505 If TYP(J - 1) < TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
        GoTo 5507

        5506 If TYP(J - 1) > TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)

    5507 Next J
Next I
Return

5600 For I = 2 To TTS: stpl = stpl + 1: Locate 5, 5: Print "sorting team "; stpl
    For J = TTS To I Step -1
        If JX = 1 Then 5615
        If JX = 2 Then 5616

        5615 If TYP(J - 1) < TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
        GoTo 5617

        5616 If TYP(J - 1) > TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)

    5617 Next J
Next I
Return

7000 Print: Print: Print: Print: Print: J = 4: GoSub 7007: Color 12: Locate , 30: Print "SELECT OPTION:"
Locate , 30: Color 14: Print "P";: Color 7: Print "RINT"
Locate , 30: Color 14: Print "V";: Color 7: Print "IEW"
Locate , 30: Color 14: Print "R";: Color 7: Print "E-SELECT OPTION"
7002 I$ = GetKeyPress$
If I$ = "P" Then PV = 2: GoTo 7015
If I$ = "V" Then PV = 1: GoTo 7015
If I$ = "R" Then GoTo 5017
If I$ <> "P" And I$ <> "V" And I$ <> "R" Then 7002

7007 '
Locate 9, 15: Color 15, 0: Print " " + Chr$(214); String$(40, 196); "· ";
For II = 1 To J: Locate 9 + II, 15: Print " " + Chr$(186); String$(40, 32); Chr$(186) + " ";: Next
Locate 10 + J, 15: Print " " + Chr$(211); String$(40, 196); "½ ": Locate 10, 10

7015 Return

8000 Color , 0: Cls: If PV = 1 Then 8015
Print: Print: Print: Print: Print: J = 2: GoSub 8007
Locate , 25: Color 1: Print "MAXIMUM = "; QW
Locate , 25: Input "ENTER LENGTH OF LIST"; LG
If LG > QW Then 8000
GoTo 8015

8007 '
Locate 10, 20: Color 4, 15: Print " " + Chr$(214); String$(30, 196); "· ";
For II = 1 To J: Locate 10 + II, 20: Print " " + Chr$(186); String$(30, 32); Chr$(186) + " ";: Next
Locate 11 + J, 20: Print " " + Chr$(211); String$(30, 196); "½ ": Locate 11

8015 Return

1440 null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then 4600
ReDim leagueFiles$(0 To Count%)
For X = 1 To Count%
    leagueFiles$(X) = Space$(12)
Next
leagueFiles$(0) = Spec$
Call ReadFile(leagueFiles$())
'CALL SortStr(VARPTR(leagueFiles$(1)), Count%, 0)

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
MAMenu leagueFiles$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If ExitCode% = 27 Then 5
targetFile$ = leagueFiles$(SELECTION%)
Locate 2, 33: Print targetFile$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3912 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1445

3913 targetFile$ = RTrim$(targetFile$)
targetFile$ = Left$(targetFile$, Len(targetFile$) - 4)
LG$ = targetFile$
Return

4600 Locate 10, 8
Print "NO LEAGUE FILES WERE FOUND"
Print "PRESS ANY KEY TO RETURN TO MAIN MENU"
I$ = GetKeyPress$
GoTo 5


'------------------------------
'      BUILDSTATS Subroutine
'------------------------------
' ...explanation...
Sub BUILDSTATS ()

    11 For KK = 1 To 30: YY = YY + 1
        If Z1$(KK) = "*" Or Z1$(KK) = "XXX" Or Z1$(KK) = "" Then 60

        10 Print "CONVERTING STAT FILE OF "; Z1$(KK)
        If Not _FileExists(D$(1) + Z1$(KK)) Then 60
        Open D$(1) + Z1$(KK) For Input As #1
        Input #1, B$
        For I = 0 To 13: Input #1, A$(I)
        For J = 0 To 13: Input #1, A(I, J): Next: Input #1, W0(I): Input #1, W1(I): Next
        For L = 0 To 15: Input #1, Z(L): Next
        For L = 0 To 15: Input #1, Z1(L): Next
        Close #1
        Open D$(1) + Z1$(KK) + "S" For Input As #1
        Input #1, Y
        Close #1
        TS = TS + 1: For I = 0 To 14: TT(TS, I) = Z(I): TT$(TS, I) = B$: GM(TS) = Y: Next
        TTS = TTS + 1: For I = 0 To 14: TT1(TTS, I) = Z1(I): TT1$(TTS, I) = B$: GM1(TTS) = Y: Next
        For I = 0 To 14
            If A$(I) = "XXX" Then 35

            33 JB = JB + 1: AL$(JB) = A$(I): TBN = TBN + 1: TMA$(TBN) = B$: GMA(JB) = Y
            K1 = 0: For I1 = 0 To 13: K1 = K1 + 1: AL(JB, K1) = A(I, I1)

            35 Next I1
            W0L(JB) = W0(I): W1L(JB) = W1(I)
        Next I

    60 Next KK
    Print "SAVING LEADER FILES . . . . PLEASE WAIT"
    Open D$(1) + targetFile$ + ".LDS" For Output As #1
    Write #1, JB, TS, TTS
    For I = 1 To 600: Write #1, AL$(I), TMA$(I), GMA(I): Color 14: Locate 18, 45: TMR = TMR - 1: Print Using "#####"; TMR
    For I1 = 1 To 14: Write #1, AL(I, I1): Next I1: Write #1, W0L(I): Write #1, W1L(I): Color 14: Locate 18, 45: TMR = TMR - 1: Print Using "#####"; TMR: Next I
    For I = 1 To 40: For I1 = 0 To 14: Write #1, TT(I, I1), TT$(I, I1): Next: Color 14: Locate 18, 45: TMR = TMR - 1: Print Using "#####"; TMR: Write #1, GM(I): Next
    For I = 1 To 40: For I1 = 0 To 14: Write #1, TT1(I, I1), TT1$(I, I1): Next: Color 14: Locate 18, 45: TMR = TMR - 1: Print Using "#####"; TMR: Write #1, GM1(I): Next
    Close #1
End Sub


'------------------------------
'      TITLE Subroutine
'------------------------------
' Displays window title / game copyright
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

