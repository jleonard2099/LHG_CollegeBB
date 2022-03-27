'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Courtside College Basketball - Create Tournament"
''$ExeIcon:'./lhg_ccb.ico'
'_Icon
_AllowFullScreen

DefInt A-Z
DECLARE FUNCTION PadRight$ (X$, Length)
DECLARE SUB TITLE ()
DECLARE SUB CHAMPIONSHIP ()
DECLARE SUB SETTINGS ()
DECLARE SUB SORTGAMES ()
DECLARE SUB WRITEGAME ()

Dim NN%(16): ' NUMBER OF TEAMS IN A REGION, UP TO 16 REGIONS
Dim N$(16, 16, 3): ' TEAM NAMES FOR EACH SEED SLOT
Dim R$(16): ' REGIONAL NAMES
Dim D$(3), U6 As Single, Y As Long
Dim OP%(14): ' TOURNAMENT DEFAULTS
Dim Q$(300), teamItems$(500), V%(500), NG%(300, 17), H$(300), V$(300), HS%(300), VS%(300)
Dim YN$(300, 3), L%(4)
Dim N%(16, 16, 3): ' REGION, SEED NUMBER, TEAM#/MODE OF PLAY (0,1)
Dim C%(1): ' 0-CONSOLATION IN REGIONS, 1-CONSOLATION IN CHAMPIONSHIP
Dim CM$(1), TE$(1), HC$(2), TS$(1), SC$(2), TF$(1), menuItems$(17), T%(9)
Dim Shared R, TC, NG%, OP%, V$, H$, YN$, F$, Q$, C%, D$


TS$(0) = "N": TS$(1) = "Y": SC$(0) = "NO SHOT CLOCK": SC$(1) = "45 SEC CLOCK ": SC$(2) = "35 SEC CLOCK "
HC$(1) = "Y": HC$(2) = "N"
TF$(0) = "N": TF$(1) = "Y"
TE$(0) = "SHOOT 1+1": TE$(1) = "SHOOT 2  "
CM$(0) = "COACH AND SELECT SHOTS           "
CM$(1) = "COACH ONLY AND LET COMPUTER SHOOT"

Open "DEV.BAS" For Input As #2
Input #2, D$(0): Input #2, D$(1)
Close 2

OP%(3) = 1
OP%(7) = 2
OP%(5) = 5
OP%(6) = 1
OP%(10) = 1
OP%(14) = 0


10 Call TITLE

'--------------------
'   MENU OPTIONS
'--------------------
menuItems$(1) = "CREATE TOURNAMENT SEEDS      "
menuItems$(2) = "EDIT EXISTING SEEDS          "
menuItems$(3) = "VIEW BRACKETS / SEEDS        "
menuItems$(4) = "RESET A TOURNAMENT           "
menuItems$(5) = "PLAY TOURNAMENT GAMES        "
menuItems$(6) = "RETURN TO COURTSIDE MAIN MENU"
SELECTION% = 1
Count% = 6
Start% = 1
SCANCODE% = 6
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 10
Column% = 25

Call box0(9, 24, 16, 54, 2, 31)
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then Run "HELLO.EXE"
Q = SELECTION% - 1
If Q = 2 Then Run "BRACKET.EXE"
If Q = 5 Then Run "HELLO.EXE"
If Q = 4 Then Run "TOURNEY.EXE"
GoSub 611
If Q = 1 Or Q = 3 Then 610
    
'GLOBAL SETUP TOURNEY DEFAULTS

20 Cls
Color , 4: Locate 1, 1: Print Tab(80);: Locate , 19: Color 0
Print "SELECT GENERAL TOURNAMENT OPTIONS"
Locate 25, 1: Print Tab(80);: Locate , 1: Color , 0

154 Locate 5, 2: Color 14: Print "(1) ";: Color 15: Print "3-POINT SHOT: "; TS$(OP%(3))

155 Locate 6, 2: Color 14: Print "(2) ";: Color 15: Print SC$(OP%(7))

157 Locate 7, 2: Color 14: Print "(3) ";: Color 15: Print Using "DISPLAY PAUSE IN SECONDS: ##.#  "; OP%(4) / 10

158 Locate 8, 2: Color 14: Print "(4) ";: Color 15: Print "FOULS TO DISQUALIFY: "; OP%(5)

162 Locate 9, 2: Color 14: Print "(5) ";: Color 15: Print "FREE THROWS ON 10TH FOUL "; TE$(OP%(6))

164 Locate 10, 2: Color 14: Print "(6) ";: Color 15: Print "3-FT WHEN FOULED ON 3-PT ATTEMPT "; TF$(OP%(10))
Locate 11, 2: Color 14: Print "(7) ";: Color 15: Print "1 FT ON 1ST 6 FLOOR FOULS (PRIOR TO 1973): "; TF$(OP%(14))

163 Locate 12, 2: Color 14: Print "(8) ";: Color 15: Print "ACCEPT SETTINGS"
165 I$ = GetKeyPress$
If I$ = Chr$(27) Then 10
I = Val(I$): If I < 1 Or I > 8 Then 165
If I = 1 Then OP%(3) = 1 - OP%(3): GoTo 154
If I = 2 Then OP%(7) = OP%(7) + 1
If OP%(7) > 2 Then OP%(7) = 0
If I = 2 Then 154
If I = 3 Then Locate 7, 32: Input U6: OP%(4) = U6 * 10: GoTo 154
If I = 4 Then OP%(5) = OP%(5) + 1
If OP%(5) > 6 Then OP%(5) = 5
If I = 4 Then 154
If I = 5 Then OP%(6) = 1 - OP%(6): GoTo 154
If I = 6 Then OP%(10) = 1 - OP%(10): GoTo 154
If I = 7 Then OP%(14) = 1 - OP%(14): GoTo 154
    
Color 15, 1
FG = 0
BG = 7
Colr = OneColor%(FG, BG) 'pack FG and BG into a single byte
Cls
    
Color , 6: Locate 1, 1: Print Tab(80);: Locate , 22: Color 15
Print "TOURNAMENT  - GENERAL INFORMATION"
Locate 25, 1: Print Tab(80);: Locate , 1: Color , 0
Locate 7, 1: QPrint "FILE NAME (8 CHARACTERS) ", 14, 1, -1
Locate , 26: QPrint PadRight$(F$, 8), FG, BG, -1
Locate 9, 1: QPrint "NAME OF TOURNAMENT ", 14, 1, -1
Locate , 20: QPrint PadRight$(FF$, 40), FG, BG, -1
Locate 11, 1: QPrint "CONSOLATION GAMES IN REGIONALS/FINALS ", 14, 1, -1
Locate , 40: QPrint PadRight$(C$, 1), FG, BG, -1
Locate 13, 1: QPrint "NUMBER OF BRACKETS/REGIONS (1-16) ", 14, 1, -1
Locate , 35: QPrint PadRight$(NR$, 2), FG, BG, -1

GETDIV1:
Locate 7, 26 'location of beginning of field
CapsOn = 0: NumOnly = 0: Max = 8 'specify field parameters
Call TextIn(F$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
If XCode = 1 GoTo GETDIV1: 'can't back up any more, stay here
If XCode = 2 GoTo Done 'all done entering data
F$ = _Trim$(F$)
GetDiv2:
Locate 9, 20 'location of beginning of field
CapsOn = 0: NumOnly = 0: Max = 40 'specify field parameters
Call TextIn(FF$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
If XCode = 1 GoTo GETDIV1: 'can't back up any more, stay here
If XCode = 2 GoTo Done 'all done entering data
FF$ = _Trim$(FF$)
GetDiv3:
Locate 11, 40 'location of beginning of field
CapsOn = 0: NumOnly = 0: Max = 1 'specify field parameters
Call TextIn(C$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
If XCode = 1 GoTo GetDiv2: 'can't back up any more, stay here
If XCode = 2 GoTo Done 'all done entering data
C$ = _Trim$(C$)

GETDIV4:
Locate 13, 35 'location of beginning of field
CapsOn = 0: NumOnly = 0: Max = 2 'specify field parameters
Call TextIn(NR$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
If XCode = 1 GoTo GetDiv3: 'can't back up any more, stay here
NR$ = _Trim$(NR$)

Done:
Locate 23, 1
Color 15, 1
Print "Is this okay? (Yes/No) ";
X$ = ""
Call YesNoB(X$, XCode, FG, BG)
Locate , 1
ClearEOL 14, 1
If XCode = 1 GoTo GETDIV4
If X$ = Chr$(27) Then 10
If X$ = "N" GoTo GETDIV1
If X$ <> "Y" GoTo Done 'insist on either Y or N or up-arrow
If UCase$(C$) = "Y" Then C%(0) = 1 Else C%(0) = 0
C%(1) = C%(0)
If F$ = "" Then

    Locate 10
    Call CSMsgBox("TOUNAMENT FILE NAME IS BLANK PRESS ANY KEY TO CONTINUE SELECTING", 28, Pass)
    I$ = GetKeyPress$
    Call CSMsgBox("", 0, Pass)

    GoTo GETDIV1
End If

R = Val(NR$)
If R < 1 Or R > 16 Then GoTo GETDIV4

Open D$(0) + F$ + ".TNY" For Output As #1
For I = 0 To 14
    Print #1, OP%(I)
Next
Close 1

If EDITCODE% = 1 Then 30

For NR = 1 To R

    SEEDBRACKET:

    Cls
    Color 15, 1
    FG = 0
    BG = 7
    Colr = OneColor%(FG, BG) 'pack FG and BG into a single byte
    Cls
    Color , 6: Locate 1, 1: Print Tab(80);: Locate , 26: Color 15
    Print "REGIONAL/BRACKET INFORMATION"
    Locate 25, 1: Print Tab(80);: Locate , 1: Color , 0
    X$ = "NAME OF REGION" + Str$(NR) + " "
    Locate 7, 1: QPrint X$, FG, BG, -1
    Locate , 26: QPrint PadRight$(R$(NR), 16), FG, BG, -1
    Locate 9, 1: QPrint "NUMBER OF TEAMS IN REGION (4-16) ", FG, BG, -1
    Locate , 34: QPrint PadRight$(NT$, 2), FG, BG, -1

    NameRegion:
    Locate 7, 26 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 16 'specify field parameters
    Call TextIn(R$(NR), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo REGIONSIZE: 'can't back up any more, stay here
    If XCode = 2 GoTo Done1 'all done entering data
    R$(NR) = _Trim$(R$(NR))

    REGIONSIZE:
    Locate 9, 34 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 2 'specify field parameters
    Call TextIn(NT$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo NameRegion: 'can't back up any more, stay here
    NT$ = _Trim$(NT$)

    Done1:
    Locate 23, 1: Color 15, 1
    Print "Is this okay? (Yes/No) ";
    X$ = ""
    Call YesNoB(X$, XCode, FG, BG)
    Locate , 1
    ClearEOL 14, 1
    If XCode = 1 GoTo REGIONSIZE
    If X$ = Chr$(27) Then 10
    If X$ = "N" GoTo NameRegion
    If X$ <> "Y" GoTo Done1 'insist on either Y or N or up-arrow

    NN%(NR) = Val(NT$)
    If NN%(NR) < 4 Or NN%(NR) > 16 Then GoTo REGIONSIZE

    220 Color , 0: Cls
    Color , 6: Locate 1, 1: Print Tab(80);: Locate , 26: Color 15
    X = Len(R$(NR) + " REGIONAL/CONFERENCE SEEDING")
    X = (80 - X) / 2
    Locate , X
    Print R$(NR) + " REGIONAL/CONFERENCE SEEDING"
    Locate 25, 1: Print Tab(80);: Locate , 1: Color , 0
    Color 12
    MAXSEED = NN%(NR) + 1 + 4
    Call box0(3, 24, MAXSEED, 54, 2, 15)
    Locate 4, 25: Print "SEED  TEAM"
    For I1 = 1 To NN%(NR)
        Color 2
        Locate , 25
        Print Using "##    "; I1;: Color 15: Print N$(NR, I1, 0)
    Next I1
    Locate 25, 1
    Color 0, 6: Print "A-";: Color 15: Print "ADD A TEAM    ";
    Color 0, 6: Print "R-";: Color 15: Print "REMOVE A TEAM    ";
    Color 0, 6: Print "S-";: Color 15: Print "SAVE SEEDING ";
    Color , 0: Locate 1, 1
    250 I$ = GetKeyPress$
    If UCase$(I$) = "A" Then I$ = "1"
    If UCase$(I$) = "R" Then I$ = "2"
    If UCase$(I$) = "S" Then I$ = "3"
    If I$ = Chr$(27) Then GoTo SEEDBRACKET
    If Val(I$) < 1 Or Val(I$) > 3 Then 250

    260 If I$ = "3" Then GoSub 80
    If I$ = "3" And NF = 0 Then 75
    If NF = 1 Then NF = 0: GoTo 250

    270 Locate 22, 30: Print "SEED # (1-"; Str$(NN%(NR)); ") ";

    280 Input B

    290 If B < 1 Or B > NN%(NR) Then 270
    If I$ = "2" Then N$(NR, B, 0) = "": GoTo 220

    1440 Color 15, 0: Cls
    JJ = 7
    Locate 6, 8: Color 15, 4
    Print " Ö"; String$(61, 196); "· ";: For II = 1 To JJ: Locate 6 + II, 8
    Print " º"; String$(61, 32); "º ";: Next
    Locate 7 + JJ, 8
    Print " Ó"; String$(61, 196); "½ ";: Locate 7, 32
    Locate , 12: Print "ENTER DISK ID: ": Print
    Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
    Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
    Locate , 12: Print
    Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
    Locate 7, 27: Input NY$
    If NY$ = "" Then GoSub 1102: GoTo 182
    YN$ = NY$
    For I = 1 To 500: teamItems$(I) = "": Next
    Cls
    If UCase$(Left$(D$(0), 1)) = "C" Then 210
    GoSub 4505

    210 GoSub 1104

    182 If BO% = 1 Then BO% = 0: GoTo 1440

    183 Locate 2, 33: Color 15, 0: Color T%(0), T%(1): Print A$: Color 15, 4
    Locate , 33: Print "ANY CHANGE (YN)";
    I$ = GetKeyPress$
    If UCase$(I$) = "Y" Then 1440

    390 N$(NR, B, 0) = A$
    N$(NR, B, 1) = YN$
    N%(NR, B, 0) = D
    N%(NR, B, 1) = 3
    N%(NR, B, 2) = T%(0)
    N%(NR, B, 3) = T%(1)
    '    LOCATE , 23: PRINT "(C)OMPUTER OR (H)UMAN CONTROL";
    '
    391 I$ = GetKeyPress$
    '    IF UCASE$(I$) <> "C" AND UCASE$(I$) <> "H" THEN 391
    '    IF UCASE$(I$) = "C" THEN N%(NR, B, 1) = 1 ELSE N%(NR, B, 1) = 0
    N%(NR, B, 1) = 1
    GoTo 220

    80 For I = 1 To NN%(NR)
        If N$(NR, I, 0) <> "" Then 81
        Locate 10

        Call CSMsgBox("ALL SEEDS HAVE NOT BEEN FILLED PRESS ANY KEY TO CONTINUE SELECTING", 30, Pass)
        82 I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)
        NF = 1: Return

    81 Next
    Return

    75 If EDITCODE% = 1 Then 30
Next

76 If _FileExists(D$(0) + F$ + ".TMP") Then Kill D$(0) + F$ + ".TMP"
If _FileExists(D$(0) + F$ + ".TCX") Then Kill D$(0) + F$ + ".TCX"
If _FileExists(D$(0) + F$ + ".TOU") Then Kill D$(0) + F$ + ".TOU"
For NR = 1 To R
    If NN%(NR) < 4 Then 50
    On NN%(NR) - 3 GOTO 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300

50 Next

Call CHAMPIONSHIP

5050 'SAVE TOURNAMENT INFO AND QUIT
Open D$(0) + F$ + ".INF" For Output As #2
Print #2, FF$
Print #2, R: ' NUMBER OF REGIONS
For I = 1 To 16
    Print #2, NN%(I): ' NUMBER OF TEAMS IN REGION
    Print #2, R$(I): ' NAME OF REGION
Next
For I = 1 To 16
    For I1 = 1 To 16
        For I2 = 0 To 3
            Print #2, N$(I, I1, I2): ' SEED SLOT NAME
            Print #2, N%(I, I1, I2): ' REGION, SEED NUMBER, MODE PLAY/TM#
Next: Next: Next
Print #2, C%(0): Print #2, C%(1): ' CONSOLATION GAME
Close 2
Call SORTGAMES
Locate 22, 25: Print "                                  "
Locate 10
    
Call CSMsgBox("ALL FILES SAVED...PRESS ANY KEY TO RETURN TO TOURNAMENT OPTIONS", 18, Pass)
12 I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
GoTo 10

30 'EDIT A GIVEN BRACKET
Color , 0: Cls
Color , 6: Locate 1, 1: Print Tab(80);: Locate , 23: Color 15
Print "TOURNAMENT  - REGION/BRACKET EDITOR"
Locate 25, 1: Print Tab(80);: Locate , 1: Color , 0
For I = 1 To R
    If R$(I) = "" Then R$(I) = "TO BE NAMED"
    menuItems$(I) = R$(I)
Next
SELECTION% = 1
Count% = R + 1
Start% = 1
SCANCODE% = R + 1
FileColr% = 15
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 4
Column% = 32
menuItems$(R + 1) = "SAVE AND EXIT"
Call box0(3, 31, 3 + R + 2, 48, 2, 15)
    
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then 10
NR = SELECTION%
If NR = R + 1 Then 76
NT$ = Str$(NN%(SELECTION%))
NT$ = _Trim$(NT$)
GoTo SEEDBRACKET
End

4505 JJ = 4
Locate 6, 8: Color 15, 1
Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
For II = 1 To JJ: Locate 6 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 7 + JJ, 8
Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";: Locate 7
Return

4500 Locate 10, 8

Call CSMsgBox("NO TEAMS MATCHING ID WERE FOUND... PRESS ANY KEY TO SELECT ANOTHER ID", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
Return

100 '4 TEAM REGION
TC = TC + 1
REGION = NR
GAME = 1
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 3, 0)
NG%(TC, 1) = N%(NR, 2, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 3, 0)
H$(TC) = N$(NR, 2, 0)
YN$(TC, 0) = N$(NR, 3, 1)
YN$(TC, 1) = N$(NR, 2, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 2
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 4, 0)
NG%(TC, 1) = N%(NR, 1, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 4, 0)
H$(TC) = N$(NR, 1, 0)
YN$(TC, 0) = N$(NR, 4, 1)
YN$(TC, 1) = N$(NR, 1, 1)
Call WRITEGAME
REGION = NR
If C%(0) = 0 Then 105: '1-YES, 0-NO CONSOLATION
TC = TC + 1
GAME = 3
ROUND = 88
Call SETTINGS
NG%(TC, 0) = 801
NG%(TC, 1) = 802
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 1 LOSER"
H$(TC) = "REG" + Str$(REGION) + " GM 2 LOSER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 4
ROUND = 89
Call SETTINGS
NG%(TC, 0) = 901
NG%(TC, 1) = 902
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
GoTo 50

105 TC = TC + 1
REGION = NR
GAME = 3
ROUND = 89
Call SETTINGS
NG%(TC, 0) = 901
NG%(TC, 1) = 902
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
GoTo 50

200 '5 TEAM REGION
TC = TC + 1
REGION = NR
GAME = 1
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 5, 0)
NG%(TC, 1) = N%(NR, 4, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 5, 0)
H$(TC) = N$(NR, 4, 0)
YN$(TC, 0) = N$(NR, 5, 1)
YN$(TC, 1) = N$(NR, 4, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 2
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 901
NG%(TC, 1) = N%(NR, 1, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
H$(TC) = N$(NR, 1, 0)
YN$(TC, 0) = "XXX"
YN$(TC, 1) = N$(NR, 1, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 3
ROUND = 2
Call SETTINGS
NG%(TC, 0) = N%(NR, 3, 0)
NG%(TC, 1) = N%(NR, 2, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 3, 0)
H$(TC) = N$(NR, 2, 0)
YN$(TC, 0) = N$(NR, 3, 1)
YN$(TC, 1) = N$(NR, 2, 1)
Call WRITEGAME
REGION = NR
If C%(0) = 0 Then 205: '1-YES, 0-NO CONSOLATION
TC = TC + 1
GAME = 4
ROUND = 88
Call SETTINGS
NG%(TC, 0) = 802
NG%(TC, 1) = 803
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 2 LOSER"
H$(TC) = "REG" + Str$(REGION) + " GM 3 LOSER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 5
ROUND = 89
Call SETTINGS
NG%(TC, 0) = 902
NG%(TC, 1) = 903
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
GoTo 50

205 TC = TC + 1
REGION = NR
GAME = 4
ROUND = 89
Call SETTINGS
NG%(TC, 0) = 902
NG%(TC, 1) = 903
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
GoTo 50

300 '6 TEAM REGION
TC = TC + 1
REGION = NR
GAME = 1
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 5, 0)
NG%(TC, 1) = N%(NR, 4, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 5, 0)
H$(TC) = N$(NR, 4, 0)
YN$(TC, 0) = N$(NR, 5, 1)
YN$(TC, 1) = N$(NR, 4, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 2
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 6, 0)
NG%(TC, 1) = N%(NR, 3, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 6, 0)
H$(TC) = N$(NR, 3, 0)
YN$(TC, 0) = N$(NR, 6, 1)
YN$(TC, 1) = N$(NR, 3, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 3
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 901
NG%(TC, 1) = N%(NR, 1, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
H$(TC) = N$(NR, 1, 0)
YN$(TC, 0) = "XXX"
YN$(TC, 1) = N$(NR, 1, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 4
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 902
NG%(TC, 1) = N%(NR, 2, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
H$(TC) = N$(NR, 2, 0)
YN$(TC, 0) = "XXX"
YN$(TC, 1) = N$(NR, 2, 1)
Call WRITEGAME
REGION = NR
'IF C%(0) = 0 THEN 305: '1-YES, 0-NO CONSOLATION
TC = TC + 1
GAME = 5
ROUND = 88
Call SETTINGS
NG%(TC, 0) = 803
NG%(TC, 1) = 804
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 3 LOSER"
H$(TC) = "REG" + Str$(REGION) + " GM 4 LOSER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 6
ROUND = 89
Call SETTINGS
NG%(TC, 0) = 903
NG%(TC, 1) = 904
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 4 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
GoTo 50

305 TC = TC + 1
REGION = NR
GAME = 5
ROUND = 89
Call SETTINGS
NG%(TC, 0) = 903
NG%(TC, 1) = 904
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 4 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
GoTo 50

400 '7 TEAM REGION
TC = TC + 1
REGION = NR
GAME = 1
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 6, 0)
NG%(TC, 1) = N%(NR, 3, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 6, 0)
H$(TC) = N$(NR, 3, 0)
YN$(TC, 0) = N$(NR, 6, 1)
YN$(TC, 1) = N$(NR, 3, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 2
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 7, 0)
NG%(TC, 1) = N%(NR, 2, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 7, 0)
H$(TC) = N$(NR, 2, 0)
YN$(TC, 0) = N$(NR, 7, 1)
YN$(TC, 1) = N$(NR, 2, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 3
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 5, 0)
NG%(TC, 1) = N%(NR, 4, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 5, 0)
H$(TC) = N$(NR, 4, 0)
YN$(TC, 0) = N$(NR, 5, 1)
YN$(TC, 1) = N$(NR, 4, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 4
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 901
NG%(TC, 1) = 902
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 5
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 903
NG%(TC, 1) = N%(NR, 1, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
H$(TC) = N$(NR, 1, 0)
YN$(TC, 0) = "XXX"
YN$(TC, 1) = N$(NR, 1, 1)
Call WRITEGAME
REGION = NR
If C%(0) = 0 Then 405: '1-YES, 0-NO CONSOLATION
TC = TC + 1
GAME = 6
ROUND = 88
Call SETTINGS
NG%(TC, 0) = 804
NG%(TC, 1) = 805
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 4 LOSER"
H$(TC) = "REG" + Str$(REGION) + " GM 5 LOSER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 7
ROUND = 89
Call SETTINGS
NG%(TC, 0) = 904
NG%(TC, 1) = 905
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 4 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 5 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
GoTo 50

405 TC = TC + 1
REGION = NR
GAME = 6
ROUND = 89
Call SETTINGS
NG%(TC, 0) = 904
NG%(TC, 1) = 905
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 4 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 5 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
GoTo 50

500 '8 TEAM REGION
TC = TC + 1
REGION = NR
GAME = 1
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 7, 0)
NG%(TC, 1) = N%(NR, 2, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 7, 0)
H$(TC) = N$(NR, 2, 0)
YN$(TC, 0) = N$(NR, 7, 1)
YN$(TC, 1) = N$(NR, 2, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 2
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 6, 0)
NG%(TC, 1) = N%(NR, 3, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 6, 0)
H$(TC) = N$(NR, 3, 0)
YN$(TC, 0) = N$(NR, 6, 1)
YN$(TC, 1) = N$(NR, 3, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 3
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 5, 0)
NG%(TC, 1) = N%(NR, 4, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 5, 0)
H$(TC) = N$(NR, 4, 0)
YN$(TC, 0) = N$(NR, 5, 1)
YN$(TC, 1) = N$(NR, 4, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 4
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 8, 0)
NG%(TC, 1) = N%(NR, 1, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 8, 0)
H$(TC) = N$(NR, 1, 0)
YN$(TC, 0) = N$(NR, 8, 1)
YN$(TC, 1) = N$(NR, 1, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 5
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 901
NG%(TC, 1) = 902
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 6
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 903
NG%(TC, 1) = 904
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 4 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
REGION = NR
If C%(0) = 0 Then 505: '1-YES, 0-NO CONSOLATION
TC = TC + 1
GAME = 7
ROUND = 88
Call SETTINGS
NG%(TC, 0) = 805
NG%(TC, 1) = 806
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 5 LOSER"
H$(TC) = "REG" + Str$(REGION) + " GM 6 LOSER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 8
ROUND = 89
Call SETTINGS
NG%(TC, 0) = 905
NG%(TC, 1) = 906
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 5 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 6 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
GoTo 50

505 TC = TC + 1
REGION = NR
GAME = 7
ROUND = 89
Call SETTINGS
NG%(TC, 0) = 905
NG%(TC, 1) = 906
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 5 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 6 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
GoTo 50

600 '9 TEAM REGION
TC = TC + 1
REGION = NR
GAME = 1
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 9, 0)
NG%(TC, 1) = N%(NR, 8, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 9, 0)
H$(TC) = N$(NR, 8, 0)
YN$(TC, 0) = N$(NR, 9, 1)
YN$(TC, 1) = N$(NR, 8, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 2
ROUND = 2
Call SETTINGS
NG%(TC, 0) = N%(NR, 7, 0)
NG%(TC, 1) = N%(NR, 2, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 7, 0)
H$(TC) = N$(NR, 2, 0)
YN$(TC, 0) = N$(NR, 7, 1)
YN$(TC, 1) = N$(NR, 2, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 3
ROUND = 2
Call SETTINGS
NG%(TC, 0) = N%(NR, 6, 0)
NG%(TC, 1) = N%(NR, 3, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 6, 0)
H$(TC) = N$(NR, 3, 0)
YN$(TC, 0) = N$(NR, 6, 1)
YN$(TC, 1) = N$(NR, 3, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 4
ROUND = 2
Call SETTINGS
NG%(TC, 0) = N%(NR, 5, 0)
NG%(TC, 1) = N%(NR, 4, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 5, 0)
H$(TC) = N$(NR, 4, 0)
YN$(TC, 0) = N$(NR, 5, 1)
YN$(TC, 1) = N$(NR, 4, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 5
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 901
NG%(TC, 1) = N%(NR, 1, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
H$(TC) = N$(NR, 1, 0)
YN$(TC, 0) = "XXX"
YN$(TC, 1) = N$(NR, 1, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 6
ROUND = 3
Call SETTINGS
NG%(TC, 0) = 902
NG%(TC, 1) = 903
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 7
ROUND = 3
Call SETTINGS
NG%(TC, 0) = 904
NG%(TC, 1) = 905
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 4 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 5 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
REGION = NR
If C%(0) = 0 Then 605: '1-YES, 0-NO CONSOLATION
TC = TC + 1
GAME = 8
ROUND = 88
Call SETTINGS
NG%(TC, 0) = 806
NG%(TC, 1) = 807
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 6 LOSER"
H$(TC) = "REG" + Str$(REGION) + " GM 7 LOSER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 9
ROUND = 89
Call SETTINGS
NG%(TC, 0) = 906
NG%(TC, 1) = 907
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 6 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 7 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
GoTo 50

605 TC = TC + 1
REGION = NR
GAME = 8
ROUND = 89
Call SETTINGS
NG%(TC, 0) = 906
NG%(TC, 1) = 907
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 6 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 7 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
GoTo 50

700 '10 TEAM REGION
TC = TC + 1
REGION = NR
GAME = 1
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 10, 0)
NG%(TC, 1) = N%(NR, 7, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 10, 0)
H$(TC) = N$(NR, 7, 0)
YN$(TC, 0) = N$(NR, 10, 1)
YN$(TC, 1) = N$(NR, 7, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 2
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 9, 0)
NG%(TC, 1) = N%(NR, 8, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 9, 0)
H$(TC) = N$(NR, 8, 0)
YN$(TC, 0) = N$(NR, 9, 1)
YN$(TC, 1) = N$(NR, 8, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 3
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 901
NG%(TC, 1) = N%(NR, 2, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
H$(TC) = N$(NR, 2, 0)
YN$(TC, 0) = "XXX"
YN$(TC, 1) = N$(NR, 2, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 4
ROUND = 2
Call SETTINGS
NG%(TC, 0) = N%(NR, 6, 0)
NG%(TC, 1) = N%(NR, 3, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 6, 0)
H$(TC) = N$(NR, 3, 0)
YN$(TC, 0) = N$(NR, 6, 1)
YN$(TC, 1) = N$(NR, 3, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 5
ROUND = 2
Call SETTINGS
NG%(TC, 0) = N%(NR, 5, 0)
NG%(TC, 1) = N%(NR, 4, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 5, 0)
H$(TC) = N$(NR, 4, 0)
YN$(TC, 0) = N$(NR, 5, 1)
YN$(TC, 1) = N$(NR, 4, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 6
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 902
NG%(TC, 1) = N%(NR, 1, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
H$(TC) = N$(NR, 1, 0)
YN$(TC, 0) = "XXX"
YN$(TC, 1) = N$(NR, 1, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 7
ROUND = 3
Call SETTINGS
NG%(TC, 0) = 903
NG%(TC, 1) = 904
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 4 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 8
ROUND = 3
Call SETTINGS
NG%(TC, 0) = 905
NG%(TC, 1) = 906
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 5 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 6 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
REGION = NR
If C%(0) = 0 Then 705: '1-YES, 0-NO CONSOLATION
TC = TC + 1
GAME = 9
ROUND = 88
Call SETTINGS
NG%(TC, 0) = 807
NG%(TC, 1) = 808
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 7 LOSER"
H$(TC) = "REG" + Str$(REGION) + " GM 8 LOSER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 10
ROUND = 89
Call SETTINGS
NG%(TC, 0) = 907
NG%(TC, 1) = 908
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 7 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 8 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
GoTo 50

705 TC = TC + 1
REGION = NR
GAME = 9
ROUND = 89
Call SETTINGS
NG%(TC, 0) = 907
NG%(TC, 1) = 908
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 7 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 8 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
GoTo 50

800 '11 TEAM REGION
TC = TC + 1
REGION = NR
GAME = 1
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 10, 0)
NG%(TC, 1) = N%(NR, 7, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 10, 0)
H$(TC) = N$(NR, 7, 0)
YN$(TC, 0) = N$(NR, 10, 1)
YN$(TC, 1) = N$(NR, 7, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 2
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 11, 0)
NG%(TC, 1) = N%(NR, 6, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 11, 0)
H$(TC) = N$(NR, 6, 0)
YN$(TC, 0) = N$(NR, 11, 1)
YN$(TC, 1) = N$(NR, 6, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 3
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 9, 0)
NG%(TC, 1) = N%(NR, 8, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 9, 0)
H$(TC) = N$(NR, 8, 0)
YN$(TC, 0) = N$(NR, 9, 1)
YN$(TC, 1) = N$(NR, 8, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 4
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 901
NG%(TC, 1) = N%(NR, 2, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
H$(TC) = N$(NR, 2, 0)
YN$(TC, 0) = "XXX"
YN$(TC, 1) = N$(NR, 2, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 5
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 902
NG%(TC, 1) = N%(NR, 3, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
H$(TC) = N$(NR, 3, 0)
YN$(TC, 0) = "XXX"
YN$(TC, 1) = N$(NR, 3, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 6
ROUND = 2
Call SETTINGS
NG%(TC, 0) = N%(NR, 5, 0)
NG%(TC, 1) = N%(NR, 4, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 5, 0)
H$(TC) = N$(NR, 4, 0)
YN$(TC, 0) = N$(NR, 5, 1)
YN$(TC, 1) = N$(NR, 4, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 7
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 903
NG%(TC, 1) = N%(NR, 1, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
H$(TC) = N$(NR, 1, 0)
YN$(TC, 0) = "XXX"
YN$(TC, 1) = N$(NR, 1, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 8
ROUND = 3
Call SETTINGS
NG%(TC, 0) = 904
NG%(TC, 1) = 905
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 4 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 5 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 9
ROUND = 3
Call SETTINGS
NG%(TC, 0) = 906
NG%(TC, 1) = 907
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 6 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 7 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
REGION = NR
If C%(0) = 0 Then 805: '1-YES, 0-NO CONSOLATION
TC = TC + 1
GAME = 10
ROUND = 88
Call SETTINGS
NG%(TC, 0) = 808
NG%(TC, 1) = 809
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 8 LOSER"
H$(TC) = "REG" + Str$(REGION) + " GM 9 LOSER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 11
ROUND = 89
Call SETTINGS
NG%(TC, 0) = 908
NG%(TC, 1) = 909
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 8 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 9 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
GoTo 50

805 TC = TC + 1
REGION = NR
GAME = 10
ROUND = 89
Call SETTINGS
NG%(TC, 0) = 908
NG%(TC, 1) = 909
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 8 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 9 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
GoTo 50

900 '12 TEAM REGION
TC = TC + 1
REGION = NR
GAME = 1
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 10, 0)
NG%(TC, 1) = N%(NR, 7, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 10, 0)
H$(TC) = N$(NR, 7, 0)
YN$(TC, 0) = N$(NR, 10, 1)
YN$(TC, 1) = N$(NR, 7, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 2
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 11, 0)
NG%(TC, 1) = N%(NR, 6, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 11, 0)
H$(TC) = N$(NR, 6, 0)
YN$(TC, 0) = N$(NR, 11, 1)
YN$(TC, 1) = N$(NR, 6, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 3
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 12, 0)
NG%(TC, 1) = N%(NR, 5, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 12, 0)
H$(TC) = N$(NR, 5, 0)
YN$(TC, 0) = N$(NR, 12, 1)
YN$(TC, 1) = N$(NR, 5, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 4
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 9, 0)
NG%(TC, 1) = N%(NR, 8, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 9, 0)
H$(TC) = N$(NR, 8, 0)
YN$(TC, 0) = N$(NR, 9, 1)
YN$(TC, 1) = N$(NR, 8, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 5
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 901
NG%(TC, 1) = N%(NR, 2, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
H$(TC) = N$(NR, 2, 0)
YN$(TC, 0) = "XXX"
YN$(TC, 1) = N$(NR, 2, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 6
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 902
NG%(TC, 1) = N%(NR, 3, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
H$(TC) = N$(NR, 3, 0)
YN$(TC, 0) = "XXX"
YN$(TC, 1) = N$(NR, 3, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 7
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 903
NG%(TC, 1) = N%(NR, 4, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
H$(TC) = N$(NR, 4, 0)
YN$(TC, 0) = "XXX"
YN$(TC, 1) = N$(NR, 4, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 8
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 904
NG%(TC, 1) = N%(NR, 1, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 4 WINNER"
H$(TC) = N$(NR, 1, 0)
YN$(TC, 0) = "XXX"
YN$(TC, 1) = N$(NR, 1, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 9
ROUND = 3
Call SETTINGS
NG%(TC, 0) = 905
NG%(TC, 1) = 906
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 5 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 6 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 10
ROUND = 3
Call SETTINGS
NG%(TC, 0) = 907
NG%(TC, 1) = 908
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 7 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 8 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
REGION = NR
If C%(0) = 0 Then 905: '1-YES, 0-NO CONSOLATION
TC = TC + 1
GAME = 11
ROUND = 88
Call SETTINGS
NG%(TC, 0) = 809
NG%(TC, 1) = 810
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 9 LOSER"
H$(TC) = "REG" + Str$(REGION) + " GM 10 LOSER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 12
ROUND = 89
Call SETTINGS
NG%(TC, 0) = 909
NG%(TC, 1) = 910
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 9 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 10 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
GoTo 50

905 TC = TC + 1
REGION = NR
GAME = 11
ROUND = 89
Call SETTINGS
NG%(TC, 0) = 909
NG%(TC, 1) = 910
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 9 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 10 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
GoTo 50

1000 '13 TEAM REGION
TC = TC + 1
REGION = NR
GAME = 1
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 13, 0)
NG%(TC, 1) = N%(NR, 4, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 13, 0)
H$(TC) = N$(NR, 4, 0)
YN$(TC, 0) = N$(NR, 13, 1)
YN$(TC, 1) = N$(NR, 4, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 2
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 12, 0)
NG%(TC, 1) = N%(NR, 5, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 12, 0)
H$(TC) = N$(NR, 5, 0)
YN$(TC, 0) = N$(NR, 12, 1)
YN$(TC, 1) = N$(NR, 5, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 3
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 9, 0)
NG%(TC, 1) = N%(NR, 8, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 9, 0)
H$(TC) = N$(NR, 8, 0)
YN$(TC, 0) = N$(NR, 9, 1)
YN$(TC, 1) = N$(NR, 8, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 4
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 10, 0)
NG%(TC, 1) = N%(NR, 7, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 10, 0)
H$(TC) = N$(NR, 7, 0)
YN$(TC, 0) = N$(NR, 10, 1)
YN$(TC, 1) = N$(NR, 7, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 5
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 11, 0)
NG%(TC, 1) = N%(NR, 6, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 11, 0)
H$(TC) = N$(NR, 6, 0)
YN$(TC, 0) = N$(NR, 11, 1)
YN$(TC, 1) = N$(NR, 6, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 6
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 901
NG%(TC, 1) = 902
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 7
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 903
NG%(TC, 1) = N%(NR, 1, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
H$(TC) = N$(NR, 1, 0)
YN$(TC, 0) = "XXX"
YN$(TC, 1) = N$(NR, 1, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 8
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 904
NG%(TC, 1) = N%(NR, 2, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 4 WINNER"
H$(TC) = N$(NR, 2, 0)
YN$(TC, 0) = "XXX"
YN$(TC, 1) = N$(NR, 2, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 9
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 905
NG%(TC, 1) = N%(NR, 3, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 5 WINNER"
H$(TC) = N$(NR, 3, 0)
YN$(TC, 0) = "XXX"
YN$(TC, 1) = N$(NR, 3, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 10
ROUND = 3
Call SETTINGS
NG%(TC, 0) = 906
NG%(TC, 1) = 907
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 6 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 7 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 11
ROUND = 3
Call SETTINGS
NG%(TC, 0) = 908
NG%(TC, 1) = 909
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 8 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 9 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
REGION = NR
If C%(0) = 0 Then 1005: '1-YES, 0-NO CONSOLATION
TC = TC + 1
GAME = 12
ROUND = 88
Call SETTINGS
NG%(TC, 0) = 810
NG%(TC, 1) = 811
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 10 LOSER"
H$(TC) = "REG" + Str$(REGION) + " GM 11 LOSER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 13
ROUND = 89
Call SETTINGS
NG%(TC, 0) = 910
NG%(TC, 1) = 911
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 10 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 11 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
GoTo 50

1005 TC = TC + 1
REGION = NR
GAME = 12
ROUND = 89
Call SETTINGS
NG%(TC, 0) = 910
NG%(TC, 1) = 911
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 10 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 11 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
GoTo 50

1100 '14 TEAM REGION
TC = TC + 1
REGION = NR
GAME = 1
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 14, 0)
NG%(TC, 1) = N%(NR, 3, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 14, 0)
H$(TC) = N$(NR, 3, 0)
YN$(TC, 0) = N$(NR, 14, 1)
YN$(TC, 1) = N$(NR, 3, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 2
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 13, 0)
NG%(TC, 1) = N%(NR, 4, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 13, 0)
H$(TC) = N$(NR, 4, 0)
YN$(TC, 0) = N$(NR, 13, 1)
YN$(TC, 1) = N$(NR, 4, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 3
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 12, 0)
NG%(TC, 1) = N%(NR, 5, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 12, 0)
H$(TC) = N$(NR, 5, 0)
YN$(TC, 0) = N$(NR, 12, 1)
YN$(TC, 1) = N$(NR, 5, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 4
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 11, 0)
NG%(TC, 1) = N%(NR, 6, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 11, 0)
H$(TC) = N$(NR, 6, 0)
YN$(TC, 0) = N$(NR, 11, 1)
YN$(TC, 1) = N$(NR, 6, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 5
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 10, 0)
NG%(TC, 1) = N%(NR, 7, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 10, 0)
H$(TC) = N$(NR, 7, 0)
YN$(TC, 0) = N$(NR, 10, 1)
YN$(TC, 1) = N$(NR, 7, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 6
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 9, 0)
NG%(TC, 1) = N%(NR, 8, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 9, 0)
H$(TC) = N$(NR, 8, 0)
YN$(TC, 0) = N$(NR, 9, 1)
YN$(TC, 1) = N$(NR, 8, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 7
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 901
NG%(TC, 1) = 904
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 4 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 8
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 905
NG%(TC, 1) = N%(NR, 2, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 5 WINNER"
H$(TC) = N$(NR, 2, 0)
YN$(TC, 0) = "XXX"
YN$(TC, 1) = N$(NR, 2, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 9
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 902
NG%(TC, 1) = 903
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 10
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 906
NG%(TC, 1) = N%(NR, 1, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 6 WINNER"
H$(TC) = N$(NR, 1, 0)
YN$(TC, 0) = "XXX"
YN$(TC, 1) = N$(NR, 1, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 11
ROUND = 3
Call SETTINGS
NG%(TC, 0) = 907
NG%(TC, 1) = 908
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 7 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 8 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 12
ROUND = 3
Call SETTINGS
NG%(TC, 0) = 909
NG%(TC, 1) = 910
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 9 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 10 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
REGION = NR
If C%(0) = 0 Then 1105: '1-YES, 0-NO CONSOLATION
TC = TC + 1
GAME = 13
ROUND = 88
Call SETTINGS
NG%(TC, 0) = 811
NG%(TC, 1) = 812
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 11 LOSER"
H$(TC) = "REG" + Str$(REGION) + " GM 12 LOSER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 14
ROUND = 89
Call SETTINGS
NG%(TC, 0) = 911
NG%(TC, 1) = 912
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 11 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 12 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
GoTo 50

1105 TC = TC + 1
REGION = NR
GAME = 13
ROUND = 89
Call SETTINGS
NG%(TC, 0) = 911
NG%(TC, 1) = 912
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 11 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 12 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
GoTo 50

1200 '15 TEAM REGION
TC = TC + 1
REGION = NR
GAME = 1
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 15, 0)
NG%(TC, 1) = N%(NR, 2, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 15, 0)
H$(TC) = N$(NR, 2, 0)
YN$(TC, 0) = N$(NR, 15, 1)
YN$(TC, 1) = N$(NR, 2, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 2
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 14, 0)
NG%(TC, 1) = N%(NR, 3, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 14, 0)
H$(TC) = N$(NR, 3, 0)
YN$(TC, 0) = N$(NR, 14, 1)
YN$(TC, 1) = N$(NR, 3, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 3
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 13, 0)
NG%(TC, 1) = N%(NR, 4, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 13, 0)
H$(TC) = N$(NR, 4, 0)
YN$(TC, 0) = N$(NR, 13, 1)
YN$(TC, 1) = N$(NR, 4, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 4
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 12, 0)
NG%(TC, 1) = N%(NR, 5, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 12, 0)
H$(TC) = N$(NR, 5, 0)
YN$(TC, 0) = N$(NR, 12, 1)
YN$(TC, 1) = N$(NR, 5, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 5
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 11, 0)
NG%(TC, 1) = N%(NR, 6, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 11, 0)
H$(TC) = N$(NR, 6, 0)
YN$(TC, 0) = N$(NR, 11, 1)
YN$(TC, 1) = N$(NR, 6, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 6
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 10, 0)
NG%(TC, 1) = N%(NR, 7, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 10, 0)
H$(TC) = N$(NR, 7, 0)
YN$(TC, 0) = N$(NR, 10, 1)
YN$(TC, 1) = N$(NR, 7, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 7
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 9, 0)
NG%(TC, 1) = N%(NR, 8, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 9, 0)
H$(TC) = N$(NR, 8, 0)
YN$(TC, 0) = N$(NR, 9, 1)
YN$(TC, 1) = N$(NR, 8, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 8
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 901
NG%(TC, 1) = 906
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 6 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 9
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 902
NG%(TC, 1) = 905
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 5 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 10
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 903
NG%(TC, 1) = 904
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 4 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 11
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 907
NG%(TC, 1) = N%(NR, 1, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 7 WINNER"
H$(TC) = N$(NR, 1, 0)
YN$(TC, 0) = "XXX"
YN$(TC, 1) = N$(NR, 1, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 12
ROUND = 3
Call SETTINGS
NG%(TC, 0) = 908
NG%(TC, 1) = 909
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 8 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 9 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 13
ROUND = 3
Call SETTINGS
NG%(TC, 0) = 910
NG%(TC, 1) = 911
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 10 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 11 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
REGION = NR
If C%(0) = 0 Then 1205: '1-YES, 0-NO CONSOLATION
TC = TC + 1
GAME = 14
ROUND = 88
Call SETTINGS
NG%(TC, 0) = 812
NG%(TC, 1) = 813
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 12 LOSER"
H$(TC) = "REG" + Str$(REGION) + " GM 13 LOSER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 15
ROUND = 89
Call SETTINGS
NG%(TC, 0) = 912
NG%(TC, 1) = 913
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 12 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 13 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
GoTo 50

1205 TC = TC + 1
REGION = NR
GAME = 14
ROUND = 89
Call SETTINGS
NG%(TC, 0) = 912
NG%(TC, 1) = 913
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 12 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 13 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
GoTo 50

1300 '16 TEAM REGION
TC = TC + 1
REGION = NR
GAME = 1
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 16, 0)
NG%(TC, 1) = N%(NR, 1, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 16, 0)
H$(TC) = N$(NR, 1, 0)
YN$(TC, 0) = N$(NR, 16, 1)
YN$(TC, 1) = N$(NR, 1, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 2
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 15, 0)
NG%(TC, 1) = N%(NR, 2, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 15, 0)
H$(TC) = N$(NR, 2, 0)
YN$(TC, 0) = N$(NR, 15, 1)
YN$(TC, 1) = N$(NR, 2, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 3
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 14, 0)
NG%(TC, 1) = N%(NR, 3, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 14, 0)
H$(TC) = N$(NR, 3, 0)
YN$(TC, 0) = N$(NR, 14, 1)
YN$(TC, 1) = N$(NR, 3, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 4
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 13, 0)
NG%(TC, 1) = N%(NR, 4, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 13, 0)
H$(TC) = N$(NR, 4, 0)
YN$(TC, 0) = N$(NR, 13, 1)
YN$(TC, 1) = N$(NR, 4, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 5
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 12, 0)
NG%(TC, 1) = N%(NR, 5, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 12, 0)
H$(TC) = N$(NR, 5, 0)
YN$(TC, 0) = N$(NR, 12, 1)
YN$(TC, 1) = N$(NR, 5, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 6
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 11, 0)
NG%(TC, 1) = N%(NR, 6, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 11, 0)
H$(TC) = N$(NR, 6, 0)
YN$(TC, 0) = N$(NR, 11, 1)
YN$(TC, 1) = N$(NR, 6, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 7
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 10, 0)
NG%(TC, 1) = N%(NR, 7, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 10, 0)
H$(TC) = N$(NR, 7, 0)
YN$(TC, 0) = N$(NR, 10, 1)
YN$(TC, 1) = N$(NR, 7, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 8
ROUND = 1
Call SETTINGS
NG%(TC, 0) = N%(NR, 9, 0)
NG%(TC, 1) = N%(NR, 8, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 9, 0)
H$(TC) = N$(NR, 8, 0)
YN$(TC, 0) = N$(NR, 9, 1)
YN$(TC, 1) = N$(NR, 8, 1)
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 9
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 901
NG%(TC, 1) = 908
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 8 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 10
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 904
NG%(TC, 1) = 905
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 4 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 5 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 11
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 903
NG%(TC, 1) = 906
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 6 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 12
ROUND = 2
Call SETTINGS
NG%(TC, 0) = 902
NG%(TC, 1) = 907
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 7 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 13
ROUND = 3
Call SETTINGS
NG%(TC, 0) = 909
NG%(TC, 1) = 910
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 9 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 10 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 14
ROUND = 3
Call SETTINGS
NG%(TC, 0) = 911
NG%(TC, 1) = 912
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 11 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 12 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
REGION = NR
If C%(0) = 0 Then 1305: '1-YES, 0-NO CONSOLATION
TC = TC + 1
GAME = 15
ROUND = 88
Call SETTINGS
NG%(TC, 0) = 813
NG%(TC, 1) = 814
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 13 LOSER"
H$(TC) = "REG" + Str$(REGION) + " GM 14 LOSER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
TC = TC + 1
REGION = NR
GAME = 16
ROUND = 89
Call SETTINGS
NG%(TC, 0) = 913
NG%(TC, 1) = 914
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 13 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 14 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
GoTo 50

1305 TC = TC + 1
REGION = NR
GAME = 15
ROUND = 89
Call SETTINGS
NG%(TC, 0) = 913
NG%(TC, 1) = 914
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 13 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 14 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
Call WRITEGAME
GoTo 50

1104 NT = 0
If Not _FileExists(D$(0) + "BASK." + YN$) Then GoSub 4500: BO% = 1: Return
Open D$(0) + "BASK." + YN$ For Random As #1 Len = 655
Y = LOF(1)
Field #1, 15 As Q$(0)
For I1 = 1 To Y / 655
    Get #1, I1
    For I2 = 1 To 15
        If Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then 1103
    Next I2
    NT = NT + 1
    teamItems$(NT) = Q$(0): V%(NT) = I1

1103 Next I1
Close 1

1102 SELECTION% = 1
Count% = NT
Start% = 1
FileFGColr% = 15: FileBGColr% = 4
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Rows% = 20
Columns% = 4
Spacing% = 6
Row% = 4
Column% = 1

1106 Color , 4: Cls: Locate , 33: Color 14: Print "TEAM SELECTION": Color 15
Locate 25, 1: Color 14: Print "ESC";: Color 15: Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14: Print "ENTER";: Color 15: Print "-SELECTS TEAM";

_MouseShow "DEFAULT"
MAMenu teamItems$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then BO% = 1: Return
If EXITCODE% = -81 Then Start% = Start% + 80: SELECTION% = Start%
If EXITCODE% = -73 Then Start% = Start% - 80: SELECTION% = Start%
If EXITCODE% = -71 Then Start% = 1: SELECTION% = 1: GoTo 1106
If EXITCODE% = -79 Then Start% = NT: SELECTION% = NT: GoTo 1106
If Start% < 1 Then Start% = 1: SELECTION% = Start%
If Start% > NT Then Start% = NT: SELECTION% = Start%
If EXITCODE% = -81 Or EXITCODE% = -73 Then 1106
D = V%(SELECTION%)
Locate 25, 1: Color 14: Print "ESC";: Color 15: Print "-SELECT NEW ID  LEFT/RIGHT ARROW  UP/DOWN ARROW  ";: Color 14: Print "ENTER";: Color 15: Print "-SELECTS TEAM";
Open D$(0) + "BASK." + YN$ For Random As #1 Len = 655
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 2 AS Q$(16), 2 AS Q$(17), 2 AS Q$(18), 2 _
AS Q$(19), 602 AS X$
Get #1, D
A$ = Q$(0)

9005 If Right$(A$, 1) = " " Then A$ = Left$(A$, Len(A$) - 1): GoTo 9005
SP = CVI(Q$(1)): DA = CVI(Q$(2)): V1% = CVI(Q$(3)): V2% = CVI(Q$(4))
For I = 0 To 4: L%(I) = CVI(Q$(I + 5)): Next
For I = 0 To 9: T%(I) = CVI(Q$(I + 10)): Next
Close 1
Return

610 ' *** LOAD IN A SAVED TOURNAMENT ***
EDITCODE% = Q
Spec$ = D$(0) + "*.TOU"
null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then 4050
ReDim F$(0 To Count%)
For X = 1 To Count%
    F$(X) = Space$(12)
Next
F$(0) = Spec$
Call ReadFile(F$())
'CALL SortStr(VARPTR(F$(1)), Count, 0)
Cls
Color , 1: Cls: Locate , 28: Color 14
Print "SELECT TOURNAMENT FIELD TO EDIT": Color 15
SELECTION% = 1
Start% = 1
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Rows% = 20
Columns% = 4
Spacing% = 6
Row% = 4
Column% = 1
Locate 25, 1: Color 14: Print "ESC";: Color 15
Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
Print "ENTER";: Color 15
Print "-SELECTS TOURNAMENT FIELD";
    
_MouseShow "DEFAULT"
206 MAMenu F$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then 10
If EXITCODE% = -71 Then Start% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 206
If EXITCODE% = -79 Then Start% = Count: EXITCODE% = 3: SELECTION% = Count: GoTo 206
If EXITCODE% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If EXITCODE% = -81 Then Start% = Start% + 80: If Start% > Count Then Start% = Count
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = Start%: GoTo 206
F$ = F$(SELECTION%)
Locate 2, 33: Print F$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
604 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 610
F$ = _Trim$(F$)
F$ = Left$(F$, Len(F$) - 4)
If EDITCODE% = 3 Then 612
Open D$(0) + F$ + ".INF" For Input As #2
Input #2, FF$
Input #2, R: ' NUMBER OF REGIONS
For I = 1 To 16
    Input #2, NN%(I): ' NUMBER OF TEAMS IN REGION
    Input #2, R$(I): ' NAME OF REGION
Next
For I = 1 To 16
    For I1 = 1 To 16
        For I2 = 0 To 3
            Input #2, N$(I, I1, I2): ' SEED SLOT NAME
            Input #2, N%(I, I1, I2): ' REGION, SEED NUMBER, MODE PLAY/TM#
Next: Next: Next
Input #2, C%(0): Input #2, C%(1): ' CONSOLATION GAME
Close 2
Open D$(0) + F$ + ".TNY" For Input As #1
For I = 0 To 14
    Input #1, OP%(I)
Next
Close 1
If C%(0) = 1 Then C$ = "Y" Else C$ = "N"
NR$ = Str$(R)
GoTo 20

'Basically creating copies of TCX files with TOU extension
612 Call FCopy(D$(0) + F$ + ".TOU", D$(0) + F$ + ".TCX", BUFFER$, ERRCODE%)
'SHELL "COPY " + D$(0) + F$ + ".TCX " + D$(0) + F$ + ".TOU >NUL:"
Locate 10
    
Call CSMsgBox("ALL FILES SAVED...PRESS ANY KEY TO RETURN TO TOURNAMENT OPTIONS", 18, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
    
EDITCODE% = 0
GoTo 10
Print "THERE ARE "; R; " REGIONS"
For I = 1 To R
    Print "REGION"; I; " HAS "; NN%(I); " TEAMS"
    Print "REGION NAME IS "; R$(I)
Next
End

611 For I = 1 To 16
    NN%(I) = 0
    R$(I) = ""
Next
For I = 1 To 16
    For I1 = 1 To 16
        For I2 = 0 To 3
            N$(I, I1, I2) = ""
            N%(I, I1, I2) = 0
Next: Next: Next
C%(0) = 0: C%(1) = 0
Return

4050 Locate 10, 8

Call CSMsgBox("NO TOURNAMENT FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
GoTo 10


'------------------------------
'      PadRight Function
'------------------------------
' Pads string to the right with spaces
Function PadRight$ (X$, Length)
    PadRight$ = X$ + Space$(Length - Len(X$)) 'for display only
End Function


'------------------------------
'      TITLE Subroutine
'------------------------------
' Displays window title / game copyright
Sub TITLE ()
    Color , 0: KEY Off: Cls
    Color , 1: Locate 1, 1: Print Tab(80);: Locate , 16: Color 12
    Print "COURTSIDE COLLEGE BASKETBALL TOURNAMENT SCHEDULER"
    Locate 24, 1: Print Tab(80);: Locate , 1
    Print "(C) LANCE HAFFNER/SHANNON LYNN 1987-1994";
    Locate 3, 1: Color 2, 0
    Color 15: Print "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
    Print "    TECHNICAL SUPPORT, CALL (800) 477-7032 EMAIL AT SportGames@AOL.COM"
    Color 10
End Sub


'------------------------------
'   CHAMPIONSHIP Subroutine
'------------------------------
Sub CHAMPIONSHIP ()
    On R GOTO 5105, 5100, 5200, 5300, 5400, 5500, 5600, 5700, 5800, 5900, 6000, 6100, 6200, 6300, 6400, 6500

    5105 GoTo 5050

    5100 '2 REGIONS
    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 99
    Call SETTINGS
    NG%(TC, 0) = 901
    NG%(TC, 1) = 902
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 1 WINNER"
    H$(TC) = "REGION 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    GoTo 5050

    5200 '3 REGIONS
    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 903
    NG%(TC, 1) = 902
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 3 WINNER"
    H$(TC) = "REGION 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 2
    ROUND = 99
    Call SETTINGS
    NG%(TC, 0) = 1101
    NG%(TC, 1) = 901
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 1 WINNER"
    H$(TC) = "REGION 1 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    GoTo 5050

    5300 '4 REGIONS
    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 901
    NG%(TC, 1) = 904
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 1 WINNER"
    H$(TC) = "REGION 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 2
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 902
    NG%(TC, 1) = 903
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 2 WINNER"
    H$(TC) = "REGION 3 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    If C%(1) = 0 Then 5305
    TC = TC + 1
    REGION = 17
    GAME = 3
    ROUND = 98
    Call SETTINGS
    NG%(TC, 0) = 1001
    NG%(TC, 1) = 1002
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 1 LOSER"
    H$(TC) = "GAME 2 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 4
    ROUND = 99
    Call SETTINGS
    NG%(TC, 0) = 1101
    NG%(TC, 1) = 1102
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 1 WINNER"
    H$(TC) = "GAME 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    GoTo 5050

    5305 TC = TC + 1
    REGION = 17
    GAME = 3
    ROUND = 99
    Call SETTINGS
    NG%(TC, 0) = 1101
    NG%(TC, 1) = 1102
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 1 WINNER"
    H$(TC) = "GAME 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    GoTo 5050

    5400 '5 REGIONS
    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 905
    NG%(TC, 1) = 904
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 5 WINNER"
    H$(TC) = "REGION 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 2
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 1101
    NG%(TC, 1) = 901
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 1 WINNER"
    H$(TC) = "REGION 1 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 3
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 902
    NG%(TC, 1) = 903
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 2 WINNER"
    H$(TC) = "REGION 3 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    If C%(1) = 0 Then 5405
    TC = TC + 1
    REGION = 17
    GAME = 4
    ROUND = 98
    Call SETTINGS
    NG%(TC, 0) = 1003
    NG%(TC, 1) = 1002
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 3 LOSER"
    H$(TC) = "GAME 2 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 5
    ROUND = 99
    Call SETTINGS
    NG%(TC, 0) = 1103
    NG%(TC, 1) = 1102
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 3 WINNER"
    H$(TC) = "GAME 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    GoTo 5050

    5405 TC = TC + 1
    REGION = 17
    GAME = 4
    ROUND = 99
    Call SETTINGS
    NG%(TC, 0) = 1103
    NG%(TC, 1) = 1102
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 3 WINNER"
    H$(TC) = "GAME 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    GoTo 5050

    5500 '6 REGIONS
    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 905
    NG%(TC, 1) = 904
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 5 WINNER"
    H$(TC) = "REGION 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 2
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 903
    NG%(TC, 1) = 906
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 3 WINNER"
    H$(TC) = "REGION 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 3
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 1101
    NG%(TC, 1) = 901
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 1 WINNER"
    H$(TC) = "REGION 1 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 4
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 1102
    NG%(TC, 1) = 902
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 2 WINNER"
    H$(TC) = "REGION 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    If C%(1) = 0 Then 5505
    TC = TC + 1
    REGION = 17
    GAME = 5
    ROUND = 98
    Call SETTINGS
    NG%(TC, 0) = 1003
    NG%(TC, 1) = 1004
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 3 LOSER"
    H$(TC) = "GAME 4 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 6
    ROUND = 99
    Call SETTINGS
    NG%(TC, 0) = 1103
    NG%(TC, 1) = 1104
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 3 WINNER"
    H$(TC) = "GAME 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    GoTo 5050

    5505 TC = TC + 1
    REGION = 17
    GAME = 5
    ROUND = 99
    Call SETTINGS
    NG%(TC, 0) = 1103
    NG%(TC, 1) = 1104
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 3 WINNER"
    H$(TC) = "GAME 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    GoTo 5050

    5600 '7 REGIONS
    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 903
    NG%(TC, 1) = 906
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 3 WINNER"
    H$(TC) = "REGION 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 2
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 902
    NG%(TC, 1) = 907
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 2 WINNER"
    H$(TC) = "REGION 7 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 3
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 904
    NG%(TC, 1) = 905
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 4 WINNER"
    H$(TC) = "REGION 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 4
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 1102
    NG%(TC, 1) = 1101
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 2 WINNER"
    H$(TC) = "GAME 1 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 5
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 1103
    NG%(TC, 1) = 901
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 3 WINNER"
    H$(TC) = "REGION 1 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    If C%(1) = 0 Then 5605
    TC = TC + 1
    REGION = 17
    GAME = 6
    ROUND = 98
    Call SETTINGS
    NG%(TC, 0) = 1005
    NG%(TC, 1) = 1004
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 5 LOSER"
    H$(TC) = "GAME 4 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 7
    ROUND = 99
    Call SETTINGS
    NG%(TC, 0) = 1105
    NG%(TC, 1) = 1104
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 5 WINNER"
    H$(TC) = "GAME 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    GoTo 5050

    5605 TC = TC + 1
    REGION = 17
    GAME = 6
    ROUND = 99
    Call SETTINGS
    NG%(TC, 0) = 1105
    NG%(TC, 1) = 1104
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 5 WINNER"
    H$(TC) = "GAME 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    GoTo 5050

    5700 '8 REGIONS
    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 902
    NG%(TC, 1) = 907
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 2 WINNER"
    H$(TC) = "REGION 7 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 2
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 903
    NG%(TC, 1) = 906
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 3 WINNER"
    H$(TC) = "REGION 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 3
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 904
    NG%(TC, 1) = 905
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 4 WINNER"
    H$(TC) = "REGION 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 4
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 901
    NG%(TC, 1) = 908
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 1 WINNER"
    H$(TC) = "REGION 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 5
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 1101
    NG%(TC, 1) = 1102
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 1 WINNER"
    H$(TC) = "GAME 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 6
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 1103
    NG%(TC, 1) = 1104
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 3 WINNER"
    H$(TC) = "GAME 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    If C%(1) = 0 Then 5705
    TC = TC + 1
    REGION = 17
    GAME = 7
    ROUND = 98
    Call SETTINGS
    NG%(TC, 0) = 1005
    NG%(TC, 1) = 1006
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 5 LOSER"
    H$(TC) = "GAME 6 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 8
    ROUND = 99
    Call SETTINGS
    NG%(TC, 0) = 1105
    NG%(TC, 1) = 1106
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 5 WINNER"
    H$(TC) = "GAME 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    GoTo 5050

    5705 TC = TC + 1
    REGION = 17
    GAME = 7
    ROUND = 99
    Call SETTINGS
    NG%(TC, 0) = 1105
    NG%(TC, 1) = 1106
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 5 WINNER"
    H$(TC) = "GAME 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    GoTo 5050

    5800 '9 REGIONS
    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 909
    NG%(TC, 1) = 908
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 9 WINNER"
    H$(TC) = "REGION 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 2
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 902
    NG%(TC, 1) = 907
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 2 WINNER"
    H$(TC) = "REGION 7 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 3
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 903
    NG%(TC, 1) = 906
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 3 WINNER"
    H$(TC) = "REGION 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 4
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 904
    NG%(TC, 1) = 905
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 4 WINNER"
    H$(TC) = "REGION 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 5
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 901
    NG%(TC, 1) = 1101
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 1 WINNER"
    H$(TC) = "GAME 1 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 6
    ROUND = 3
    Call SETTINGS
    NG%(TC, 0) = 1102
    NG%(TC, 1) = 1103
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 2 WINNER"
    H$(TC) = "GAME 3 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 7
    ROUND = 3
    Call SETTINGS
    NG%(TC, 0) = 1104
    NG%(TC, 1) = 1105
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 4 WINNER"
    H$(TC) = "GAME 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    If C%(1) = 0 Then 5805
    TC = TC + 1
    REGION = 17
    GAME = 8
    ROUND = 98
    Call SETTINGS
    NG%(TC, 0) = 1007
    NG%(TC, 1) = 1006
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 7 LOSER"
    H$(TC) = "GAME 6 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 9
    ROUND = 99
    Call SETTINGS
    NG%(TC, 0) = 1107
    NG%(TC, 1) = 1106
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 7 WINNER"
    H$(TC) = "GAME 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    GoTo 5050

    5805 TC = TC + 1
    REGION = 17
    GAME = 8
    ROUND = 99
    Call SETTINGS
    NG%(TC, 0) = 1107
    NG%(TC, 1) = 1106
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 7 WINNER"
    H$(TC) = "GAME 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    GoTo 5050

    5900 '10 REGIONS
    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 910
    NG%(TC, 1) = 907
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 10 WINNER"
    H$(TC) = "REGION 7 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 2
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 908
    NG%(TC, 1) = 909
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 8 WINNER"
    H$(TC) = "REGION 9 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 3
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 1101
    NG%(TC, 1) = 902
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 1 WINNER"
    H$(TC) = "REGION 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 4
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 903
    NG%(TC, 1) = 906
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 3 WINNER"
    H$(TC) = "REGION 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 5
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 904
    NG%(TC, 1) = 905
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 4 WINNER"
    H$(TC) = "REGION 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 6
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 901
    NG%(TC, 1) = 1102
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 1 WINNER"
    H$(TC) = "GAME 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 7
    ROUND = 3
    Call SETTINGS
    NG%(TC, 0) = 1103
    NG%(TC, 1) = 1104
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 3 WINNER"
    H$(TC) = "GAME 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 8
    ROUND = 3
    Call SETTINGS
    NG%(TC, 0) = 1105
    NG%(TC, 1) = 1106
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 5 WINNER"
    H$(TC) = "GAME 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    If C%(1) = 0 Then 5905
    TC = TC + 1
    REGION = 17
    GAME = 9
    ROUND = 98
    Call SETTINGS
    NG%(TC, 0) = 1007
    NG%(TC, 1) = 1008
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 7 LOSER"
    H$(TC) = "GAME 8 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 10
    ROUND = 99
    Call SETTINGS
    NG%(TC, 0) = 1107
    NG%(TC, 1) = 1108
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 7 WINNER"
    H$(TC) = "GAME 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    GoTo 5050

    5905 TC = TC + 1
    REGION = 17
    GAME = 9
    ROUND = 99
    Call SETTINGS
    NG%(TC, 0) = 1107
    NG%(TC, 1) = 1108
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 7 WINNER"
    H$(TC) = "GAME 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    GoTo 5050

    6000 '11 REGIONS
    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 910
    NG%(TC, 1) = 907
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 10 WINNER"
    H$(TC) = "REGION 7 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 2
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 911
    NG%(TC, 1) = 906
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 11 WINNER"
    H$(TC) = "REGION 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 3
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 909
    NG%(TC, 1) = 908
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 9 WINNER"
    H$(TC) = "REGION 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 4
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 1101
    NG%(TC, 1) = 902
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 1 WINNER"
    H$(TC) = "REGION 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 5
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 903
    NG%(TC, 1) = 1102
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 3 WINNER"
    H$(TC) = "GAME 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 6
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 904
    NG%(TC, 1) = 905
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 4 WINNER"
    H$(TC) = "REGION 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 7
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 901
    NG%(TC, 1) = 1103
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 1 WINNER"
    H$(TC) = "GAME 3 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 8
    ROUND = 3
    Call SETTINGS
    NG%(TC, 0) = 1104
    NG%(TC, 1) = 1105
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 4 WINNER"
    H$(TC) = "GAME 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 9
    ROUND = 3
    Call SETTINGS
    NG%(TC, 0) = 1106
    NG%(TC, 1) = 1107
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 6 WINNER"
    H$(TC) = "GAME 7 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    If C%(1) = 0 Then 6005
    TC = TC + 1
    REGION = 17
    GAME = 10
    ROUND = 98
    Call SETTINGS
    NG%(TC, 0) = 1009
    NG%(TC, 1) = 1008
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 9 LOSER"
    H$(TC) = "GAME 8 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 11
    ROUND = 99
    Call SETTINGS
    NG%(TC, 0) = 1109
    NG%(TC, 1) = 1108
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 9 WINNER"
    H$(TC) = "GAME 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    GoTo 5050

    6005 TC = TC + 1
    REGION = 17
    GAME = 10
    ROUND = 99
    Call SETTINGS
    NG%(TC, 0) = 1109
    NG%(TC, 1) = 1108
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 7 WINNER"
    H$(TC) = "GAME 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    GoTo 5050

    6100 '12 REGIONS
    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 910
    NG%(TC, 1) = 907
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 10 WINNER"
    H$(TC) = "REGION 7 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 2
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 911
    NG%(TC, 1) = 906
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 11 WINNER"
    H$(TC) = "REGION 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 3
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 912
    NG%(TC, 1) = 905
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 12 WINNER"
    H$(TC) = "REGION 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 4
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 909
    NG%(TC, 1) = 908
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 9 WINNER"
    H$(TC) = "REGION 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 5
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 902
    NG%(TC, 1) = 1101
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 2 WINNER"
    H$(TC) = "GAME 1 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 6
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 1102
    NG%(TC, 1) = 903
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 2 WINNER"
    H$(TC) = "REGION 3 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 7
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 904
    NG%(TC, 1) = 1103
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 4 WINNER"
    H$(TC) = "GAME 3 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 8
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 1104
    NG%(TC, 1) = 901
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 4 WINNER"
    H$(TC) = "REGION 1 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 9
    ROUND = 3
    Call SETTINGS
    NG%(TC, 0) = 1106
    NG%(TC, 1) = 1105
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 6 WINNER"
    H$(TC) = "GAME 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 10
    ROUND = 3
    Call SETTINGS
    NG%(TC, 0) = 1107
    NG%(TC, 1) = 1108
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 7 WINNER"
    H$(TC) = "GAME 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    If C%(1) = 0 Then 6105
    TC = TC + 1
    REGION = 17
    GAME = 11
    ROUND = 98
    Call SETTINGS
    NG%(TC, 0) = 1009
    NG%(TC, 1) = 1010
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 9 LOSER"
    H$(TC) = "GAME 10 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 12
    ROUND = 99
    Call SETTINGS
    NG%(TC, 0) = 1109
    NG%(TC, 1) = 1110
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 9 WINNER"
    H$(TC) = "GAME 10 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    GoTo 5050

    6105 TC = TC + 1
    REGION = 17
    GAME = 11
    ROUND = 99
    Call SETTINGS
    NG%(TC, 0) = 1109
    NG%(TC, 1) = 1110
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 9 WINNER"
    H$(TC) = "GAME 10 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    GoTo 5050

    6200 '13 REGIONS
    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 913
    NG%(TC, 1) = 904
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 13 WINNER"
    H$(TC) = "REGION 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 2
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 912
    NG%(TC, 1) = 905
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 12 WINNER"
    H$(TC) = "REGION 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 3
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 909
    NG%(TC, 1) = 908
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 9 WINNER"
    H$(TC) = "REGION 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 4
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 910
    NG%(TC, 1) = 907
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 10 WINNER"
    H$(TC) = "REGION 7 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 5
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 906
    NG%(TC, 1) = 911
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 6 WINNER"
    H$(TC) = "REGION 11 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 6
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 1101
    NG%(TC, 1) = 1102
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 1 WINNER"
    H$(TC) = "GAME 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 7
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 901
    NG%(TC, 1) = 1103
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 1 WINNER"
    H$(TC) = "GAME 3 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 8
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 1104
    NG%(TC, 1) = 902
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 4 WINNER"
    H$(TC) = "REGION 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 9
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 903
    NG%(TC, 1) = 1105
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 3 WINNER"
    H$(TC) = "GAME 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 10
    ROUND = 3
    Call SETTINGS
    NG%(TC, 0) = 1107
    NG%(TC, 1) = 1106
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 7 WINNER"
    H$(TC) = "GAME 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 11
    ROUND = 3
    Call SETTINGS
    NG%(TC, 0) = 1103
    NG%(TC, 1) = 1109
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 3 WINNER"
    H$(TC) = "GAME 9 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    If C%(1) = 0 Then 6205
    TC = TC + 1
    REGION = 17
    GAME = 12
    ROUND = 98
    Call SETTINGS
    NG%(TC, 0) = 1011
    NG%(TC, 1) = 1010
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 11 LOSER"
    H$(TC) = "GAME 10 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 13
    ROUND = 99
    Call SETTINGS
    NG%(TC, 0) = 1111
    NG%(TC, 1) = 1110
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 11 WINNER"
    H$(TC) = "GAME 10 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    GoTo 5050

    6205 TC = TC + 1
    REGION = 17
    GAME = 12
    ROUND = 99
    Call SETTINGS
    NG%(TC, 0) = 1111
    NG%(TC, 1) = 1110
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 11 WINNER"
    H$(TC) = "GAME 10 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    GoTo 5050

    6300 '14 REGIONS
    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 914
    NG%(TC, 1) = 903
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 14 WINNER"
    H$(TC) = "REGION 3 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 2
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 913
    NG%(TC, 1) = 904
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 13 WINNER"
    H$(TC) = "REGION 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 3
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 912
    NG%(TC, 1) = 905
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 12 WINNER"
    H$(TC) = "REGION 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 4
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 911
    NG%(TC, 1) = 906
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 11 WINNER"
    H$(TC) = "REGION 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 5
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 910
    NG%(TC, 1) = 907
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 10 WINNER"
    H$(TC) = "REGION 7 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 6
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 909
    NG%(TC, 1) = 909
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 9 WINNER"
    H$(TC) = "REGION 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 7
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 1104
    NG%(TC, 1) = 1101
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 4 WINNER"
    H$(TC) = "GAME 1 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 8
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 1105
    NG%(TC, 1) = 902
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 5 WINNER"
    H$(TC) = "REGION 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 9
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 1102
    NG%(TC, 1) = 1103
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 2 WINNER"
    H$(TC) = "GAME 3 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 10
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 1106
    NG%(TC, 1) = 901
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 6 WINNER"
    H$(TC) = "REGION 1 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 11
    ROUND = 3
    Call SETTINGS
    NG%(TC, 0) = 1107
    NG%(TC, 1) = 1108
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 7 WINNER"
    H$(TC) = "GAME 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 12
    ROUND = 3
    Call SETTINGS
    NG%(TC, 0) = 1109
    NG%(TC, 1) = 1110
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 9 WINNER"
    H$(TC) = "GAME 10 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    If C%(1) = 0 Then 6305
    TC = TC + 1
    REGION = 17
    GAME = 13
    ROUND = 98
    Call SETTINGS
    NG%(TC, 0) = 1012
    NG%(TC, 1) = 1011
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 12 LOSER"
    H$(TC) = "GAME 11 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 14
    ROUND = 99
    Call SETTINGS
    NG%(TC, 0) = 1112
    NG%(TC, 1) = 1111
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 12 WINNER"
    H$(TC) = "GAME 11 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    GoTo 5050

    6305 TC = TC + 1
    REGION = 17
    GAME = 13
    ROUND = 99
    Call SETTINGS
    NG%(TC, 0) = 1111
    NG%(TC, 1) = 1112
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 11 WINNER"
    H$(TC) = "GAME 12 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    GoTo 5050

    6400 '15 REGIONS
    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 915
    NG%(TC, 1) = 902
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 15 WINNER"
    H$(TC) = "REGION 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 2
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 914
    NG%(TC, 1) = 903
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 14 WINNER"
    H$(TC) = "REGION 3 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 3
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 913
    NG%(TC, 1) = 904
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 13 WINNER"
    H$(TC) = "REGION 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 4
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 912
    NG%(TC, 1) = 905
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 12 WINNER"
    H$(TC) = "REGION 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 5
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 911
    NG%(TC, 1) = 906
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 11 WINNER"
    H$(TC) = "REGION 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 6
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 910
    NG%(TC, 1) = 907
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 10 WINNER"
    H$(TC) = "REGION 7 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 7
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 909
    NG%(TC, 1) = 908
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 9 WINNER"
    H$(TC) = "REGION 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 8
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 1101
    NG%(TC, 1) = 1106
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 1 WINNER"
    H$(TC) = "GAME 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 9
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 1102
    NG%(TC, 1) = 1105
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 2 WINNER"
    H$(TC) = "GAME 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 10
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 1103
    NG%(TC, 1) = 1104
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 3 WINNER"
    H$(TC) = "GAME 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 11
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 1107
    NG%(TC, 1) = 901
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 7 WINNER"
    H$(TC) = "REGION 1 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 12
    ROUND = 3
    Call SETTINGS
    NG%(TC, 0) = 1109
    NG%(TC, 1) = 1108
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 9 WINNER"
    H$(TC) = "GAME 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 13
    ROUND = 3
    Call SETTINGS
    NG%(TC, 0) = 1110
    NG%(TC, 1) = 1111
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 10 WINNER"
    H$(TC) = "GAME 11 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    If C%(1) = 0 Then 6405
    TC = TC + 1
    REGION = 17
    GAME = 14
    ROUND = 98
    Call SETTINGS
    NG%(TC, 0) = 1012
    NG%(TC, 1) = 1013
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 12 LOSER"
    H$(TC) = "GAME 13 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 15
    ROUND = 99
    Call SETTINGS
    NG%(TC, 0) = 1112
    NG%(TC, 1) = 1113
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 12 WINNER"
    H$(TC) = "GAME 13 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    GoTo 5050

    6405 TC = TC + 1
    REGION = 17
    GAME = 14
    ROUND = 99
    Call SETTINGS
    NG%(TC, 0) = 1112
    NG%(TC, 1) = 1113
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 12 WINNER"
    H$(TC) = "GAME 13 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    GoTo 5050

    6500 '16 REGIONS
    TC = TC + 1
    REGION = 17
    GAME = 1
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 916
    NG%(TC, 1) = 901
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 16 WINNER"
    H$(TC) = "REGION 1 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 2
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 915
    NG%(TC, 1) = 902
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 15 WINNER"
    H$(TC) = "REGION 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 3
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 914
    NG%(TC, 1) = 903
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 14 WINNER"
    H$(TC) = "REGION 3 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 4
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 913
    NG%(TC, 1) = 904
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 13 WINNER"
    H$(TC) = "REGION 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 5
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 912
    NG%(TC, 1) = 905
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 12 WINNER"
    H$(TC) = "REGION 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 6
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 911
    NG%(TC, 1) = 906
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 11 WINNER"
    H$(TC) = "REGION 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 7
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 910
    NG%(TC, 1) = 907
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 10 WINNER"
    H$(TC) = "REGION 7 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 8
    ROUND = 1
    Call SETTINGS
    NG%(TC, 0) = 909
    NG%(TC, 1) = 908
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REGION 9 WINNER"
    H$(TC) = "REGION 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 9
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 1101
    NG%(TC, 1) = 1108
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 1 WINNER"
    H$(TC) = "GAME 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 10
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 1104
    NG%(TC, 1) = 1105
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 4 WINNER"
    H$(TC) = "GAME 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 11
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 1103
    NG%(TC, 1) = 1106
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 3 WINNER"
    H$(TC) = "GAME 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 12
    ROUND = 2
    Call SETTINGS
    NG%(TC, 0) = 1102
    NG%(TC, 1) = 1107
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 2 WINNER"
    H$(TC) = "GAME 7 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 13
    ROUND = 3
    Call SETTINGS
    NG%(TC, 0) = 1109
    NG%(TC, 1) = 1110
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 9 WINNER"
    H$(TC) = "GAME 10 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 14
    ROUND = 3
    Call SETTINGS
    NG%(TC, 0) = 1111
    NG%(TC, 1) = 1112
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 11 WINNER"
    H$(TC) = "GAME 12 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    If C%(1) = 0 Then 6505
    TC = TC + 1
    REGION = 17
    GAME = 15
    ROUND = 98
    Call SETTINGS
    NG%(TC, 0) = 1014
    NG%(TC, 1) = 1013
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 14 LOSER"
    H$(TC) = "GAME 13 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    TC = TC + 1
    REGION = 17
    GAME = 16
    ROUND = 99
    Call SETTINGS
    NG%(TC, 0) = 1114
    NG%(TC, 1) = 1113
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 14 WINNER"
    H$(TC) = "GAME 13 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME
    GoTo 5050

    6505 TC = TC + 1
    REGION = 17
    GAME = 15
    ROUND = 99
    Call SETTINGS
    NG%(TC, 0) = 1114
    NG%(TC, 1) = 1113
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "GAME 14 WINNER"
    H$(TC) = "GAME 13 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    Call WRITEGAME

5050 End Sub


'------------------------------
'    SETTINGS Subroutine
'------------------------------
Sub SETTINGS ()
    For I = 0 To 14
        NG%(TC, I) = OP%(I)
    Next
    NG%(TC, 8) = 3
    NG%(TC, 2) = 2
End Sub


'------------------------------
'    SORTGAMES Subroutine
'------------------------------
Sub SORTGAMES ()
    For RD = 1 To 89
        If RD > 5 And RD < 88 Or RD > 89 Then 999
        Locate 22, 25: Print "SCHEDULING GAMES FOR BRACKET"; NR
        For NR = 1 To 16
            If NR > R Then 998
            For AP% = 1 To TC
                GoSub 996
                If NG%(0, 16) <> RD Or NG%(0, 15) <> NR Then 997
                MATCH = MATCH + 1
                GoSub 995

            997 Next AP%

        998 Next NR

    999 Next RD
    NR = 17
    For RD = 1 To 99
        If RD > 5 And RD < 88 Or RD > 89 And RD < 98 Then 993
        For AP% = 1 To TC
            GoSub 996
            If NG%(0, 16) <> RD Or NG%(0, 15) <> NR Then 991
            MATCH = MATCH + 1
            GoSub 995

        991 Next AP%

    993 Next RD
    GoTo 994

    996 Open D$(0) + F$ + ".TMP" For Random As #1 Len = 90
    For X = 0 To 14
        Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
    Next
    Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
    Field #1, 64 As X$, 3 As Q$(20), 3 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
    Field #1, 84 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)
    Get #1, AP%
    For X = 0 To 14: NG%(0, X) = CVI(Q$(X + 1)): Next
    For X = 0 To 3: YN$(0, X) = Q$(20 + X): Next
    VS%(0) = CVI(Q$(16)): HS%(0) = CVI(Q$(17))
    V$(0) = Q$(18): H$(0) = Q$(19)
    For X = 1 To 3: NG%(0, X + 14) = CVI(Q$(X + 23)): Next
    Close 1
    Return

    995 Open D$(0) + F$ + ".TOU" For Random As #1 Len = 90
    For X = 0 To 14
        Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
    Next
    Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
    Field #1, 64 As X$, 3 As Q$(20), 3 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
    Field #1, 84 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)
    For X = 0 To 14: LSet Q$(X + 1) = MKI$(NG%(0, X)): Next
    For X = 0 To 3: LSet Q$(20 + X) = YN$(0, X): Next
    LSet Q$(16) = MKI$(VS%(0)): LSet Q$(17) = MKI$(HS%(0))
    LSet Q$(18) = V$(0): LSet Q$(19) = H$(0)
    For X = 24 To 26: LSet Q$(X) = MKI$(NG%(0, X - 9)): Next
    Put #1, MATCH
    Close 1
    Return

    994 Kill D$(0) + F$ + ".TMP"
    'Basically creating copies of TCX files with TOU extension
    Call FCopy(D$(0) + F$ + ".TOU", D$(0) + F$ + ".TCX", BUFFER$, ERRCODE%)
    'SHELL "COPY "+D$(0)+F$+".TOU "+D$(0)+F$+".TCX >NUL:"
End Sub


'------------------------------
'    WRITEGAME Subroutine
'------------------------------
Sub WRITEGAME ()
    'WRITE OUT A GAME
    Open D$(0) + F$ + ".TMP" For Random As #1 Len = 90
    For X = 0 To 14
        Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
    Next
    Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
    Field #1, 64 As X$, 3 As Q$(20), 3 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
    Field #1, 84 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)
    For X = 0 To 14: LSet Q$(X + 1) = MKI$(NG%(TC, X)): Next
    For X = 0 To 3: LSet Q$(20 + X) = YN$(TC, X): Next
    LSet Q$(16) = MKI$(VS%(TC)): LSet Q$(17) = MKI$(HS%(TC))
    LSet Q$(18) = V$(TC): LSet Q$(19) = H$(TC)
    For X = 24 To 26: LSet Q$(X) = MKI$(NG%(TC, X - 9)): Next
    Put #1, TC
    Close 1
    Open D$(0) + F$ + ".TCX" For Random As #1 Len = 90
    For X = 0 To 14
        Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
    Next
    Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
    Field #1, 64 As X$, 3 As Q$(20), 3 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
    Field #1, 84 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)
    For X = 0 To 14: LSet Q$(X + 1) = MKI$(NG%(TC, X)): Next
    For X = 0 To 3: LSet Q$(20 + X) = YN$(TC, X): Next
    LSet Q$(16) = MKI$(VS%(TC)): LSet Q$(17) = MKI$(HS%(TC))
    LSet Q$(18) = V$(TC): LSet Q$(19) = H$(TC)
    For X = 24 To 26: LSet Q$(X) = MKI$(NG%(TC, X - 9)): Next
    Put #1, TC
    Close 1

End Sub

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
