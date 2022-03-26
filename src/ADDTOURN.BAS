'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

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
Dim Q$(300), VV$(500), V%(500), NG%(300, 17), H$(300), V$(300), HS%(300), VS%(300)
Dim YN$(300, 3), L%(4)
Dim N%(16, 16, 3): ' REGION, SEED NUMBER, TEAM#/MODE OF PLAY (0,1)
Dim C%(1): ' 0-CONSOLATION IN REGIONS, 1-CONSOLATION IN CHAMPIONSHIP
Dim CM$(1), TE$(1), HC$(2), TS$(1), SC$(2), TF$(1), ITEM$(17), T%(9)
Common Shared R, TC, NG%(), OP%(), V$(), H$(), YN$(), F$, HS%(), VS%(), Q$(), C%(), D$()

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

' build menu screen

10 Call TITLE
ITEM$(1) = "CREATE TOURNAMENT SEEDS      "
ITEM$(2) = "EDIT EXISTING SEEDS          "
ITEM$(3) = "VIEW BRACKETS / SEEDS        "
ITEM$(4) = "RESET A TOURNAMENT           "
ITEM$(5) = "PLAY TOURNAMENT GAMES        "
ITEM$(6) = "RETURN TO COURTSIDE MAIN MENU"
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
MMenuVert ITEM$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
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
    CALL CSMsgBox("TOUNAMENT FILE NAME IS BLANK PRESS ANY KEY TO CONTINUE SELECTING", 28, Pass)
    I$ = GetKeyPress$
    CALL CSMsgBox("", 0, Pass)

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
    For I = 1 To 500: VV$(I) = "": Next
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

        CALL CSMsgBox("ALL SEEDS HAVE NOT BEEN FILLED PRESS ANY KEY TO CONTINUE SELECTING", 30, Pass)
        82 I$ = GetKeyPress$
        CALL CSMsgBox("", 0, Pass)
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

CALL CHAMPIONSHIP

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
CALL SORTGAMES
Locate 22, 25: Print "                                  "
Locate 10
    
CALL CSMsgBox("ALL FILES SAVED...PRESS ANY KEY TO RETURN TO TOURNAMENT OPTIONS", 18, Cnf, Pas)
12 I$ = GetKeyPress$
CALL CSMsgBox("", 0, Cnf, Pas)
GoTo 10

30 'EDIT A GIVEN BRACKET
Color , 0: Cls
Color , 6: Locate 1, 1: Print Tab(80);: Locate , 23: Color 15
Print "TOURNAMENT  - REGION/BRACKET EDITOR"
Locate 25, 1: Print Tab(80);: Locate , 1: Color , 0
For I = 1 To R
    If R$(I) = "" Then R$(I) = "TO BE NAMED"
    ITEM$(I) = R$(I)
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
ITEM$(R + 1) = "SAVE AND EXIT"
Call box0(3, 31, 3 + R + 2, 48, 2, 15)
    
_MouseShow "DEFAULT"
MMenuVert ITEM$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
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

CALL CSMsgBox("NO TEAMS MATCHING ID WERE FOUND... PRESS ANY KEY TO SELECT ANOTHER ID", 26, Pass)
I$ = GetKeyPress$
CALL CSMsgBox("", 0, Pass)
Return

100 '4 TEAM REGION
TC = TC + 1
REGION = NR
GAME = 1
ROUND = 1
CALL SETTINGS
NG%(TC, 0) = N%(NR, 3, 0)
NG%(TC, 1) = N%(NR, 2, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 3, 0)
H$(TC) = N$(NR, 2, 0)
YN$(TC, 0) = N$(NR, 3, 1)
YN$(TC, 1) = N$(NR, 2, 1)
CALL WRITEGAME
TC = TC + 1
REGION = NR
GAME = 2
ROUND = 1
CALL SETTINGS
NG%(TC, 0) = N%(NR, 4, 0)
NG%(TC, 1) = N%(NR, 1, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 4, 0)
H$(TC) = N$(NR, 1, 0)
YN$(TC, 0) = N$(NR, 4, 1)
YN$(TC, 1) = N$(NR, 1, 1)
CALL WRITEGAME
REGION = NR
If C%(0) = 0 Then 105: '1-YES, 0-NO CONSOLATION
TC = TC + 1
GAME = 3
ROUND = 88
CALL SETTINGS
NG%(TC, 0) = 801
NG%(TC, 1) = 802
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 1 LOSER"
H$(TC) = "REG" + Str$(REGION) + " GM 2 LOSER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
CALL WRITEGAME
TC = TC + 1
REGION = NR
GAME = 4
ROUND = 89
CALL SETTINGS
NG%(TC, 0) = 901
NG%(TC, 1) = 902
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
CALL WRITEGAME
GoTo 50

105 TC = TC + 1
REGION = NR
GAME = 3
ROUND = 89
CALL SETTINGS
NG%(TC, 0) = 901
NG%(TC, 1) = 902
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
CALL WRITEGAME
GoTo 50

200 '5 TEAM REGION
TC = TC + 1
REGION = NR
GAME = 1
ROUND = 1
CALL SETTINGS
NG%(TC, 0) = N%(NR, 5, 0)
NG%(TC, 1) = N%(NR, 4, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 5, 0)
H$(TC) = N$(NR, 4, 0)
YN$(TC, 0) = N$(NR, 5, 1)
YN$(TC, 1) = N$(NR, 4, 1)
CALL WRITEGAME
TC = TC + 1
REGION = NR
GAME = 2
ROUND = 2
CALL SETTINGS
NG%(TC, 0) = 901
NG%(TC, 1) = N%(NR, 1, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
H$(TC) = N$(NR, 1, 0)
YN$(TC, 0) = "XXX"
YN$(TC, 1) = N$(NR, 1, 1)
CALL WRITEGAME
TC = TC + 1
REGION = NR
GAME = 3
ROUND = 2
CALL SETTINGS
NG%(TC, 0) = N%(NR, 3, 0)
NG%(TC, 1) = N%(NR, 2, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 3, 0)
H$(TC) = N$(NR, 2, 0)
YN$(TC, 0) = N$(NR, 3, 1)
YN$(TC, 1) = N$(NR, 2, 1)
CALL WRITEGAME
REGION = NR
If C%(0) = 0 Then 205: '1-YES, 0-NO CONSOLATION
TC = TC + 1
GAME = 4
ROUND = 88
CALL SETTINGS
NG%(TC, 0) = 802
NG%(TC, 1) = 803
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 2 LOSER"
H$(TC) = "REG" + Str$(REGION) + " GM 3 LOSER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
CALL WRITEGAME
TC = TC + 1
REGION = NR
GAME = 5
ROUND = 89
CALL SETTINGS
NG%(TC, 0) = 902
NG%(TC, 1) = 903
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
CALL WRITEGAME
GoTo 50

205 TC = TC + 1
REGION = NR
GAME = 4
ROUND = 89
CALL SETTINGS
NG%(TC, 0) = 902
NG%(TC, 1) = 903
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
CALL WRITEGAME
GoTo 50

300 '6 TEAM REGION
TC = TC + 1
REGION = NR
GAME = 1
ROUND = 1
CALL SETTINGS
NG%(TC, 0) = N%(NR, 5, 0)
NG%(TC, 1) = N%(NR, 4, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 5, 0)
H$(TC) = N$(NR, 4, 0)
YN$(TC, 0) = N$(NR, 5, 1)
YN$(TC, 1) = N$(NR, 4, 1)
CALL WRITEGAME
TC = TC + 1
REGION = NR
GAME = 2
ROUND = 1
CALL SETTINGS
NG%(TC, 0) = N%(NR, 6, 0)
NG%(TC, 1) = N%(NR, 3, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = N$(NR, 6, 0)
H$(TC) = N$(NR, 3, 0)
YN$(TC, 0) = N$(NR, 6, 1)
YN$(TC, 1) = N$(NR, 3, 1)
CALL WRITEGAME
TC = TC + 1
REGION = NR
GAME = 3
ROUND = 2
CALL SETTINGS
NG%(TC, 0) = 901
NG%(TC, 1) = N%(NR, 1, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 1 WINNER"
H$(TC) = N$(NR, 1, 0)
YN$(TC, 0) = "XXX"
YN$(TC, 1) = N$(NR, 1, 1)
CALL WRITEGAME
TC = TC + 1
REGION = NR
GAME = 4
ROUND = 2
CALL SETTINGS
NG%(TC, 0) = 902
NG%(TC, 1) = N%(NR, 2, 0)
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 2 WINNER"
H$(TC) = N$(NR, 2, 0)
YN$(TC, 0) = "XXX"
YN$(TC, 1) = N$(NR, 2, 1)
CALL WRITEGAME
REGION = NR
'IF C%(0) = 0 THEN 305: '1-YES, 0-NO CONSOLATION
TC = TC + 1
GAME = 5
ROUND = 88
CALL SETTINGS
NG%(TC, 0) = 803
NG%(TC, 1) = 804
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 3 LOSER"
H$(TC) = "REG" + Str$(REGION) + " GM 4 LOSER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
CALL WRITEGAME
TC = TC + 1
REGION = NR
GAME = 6
ROUND = 89
CALL SETTINGS
NG%(TC, 0) = 903
NG%(TC, 1) = 904
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 4 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
CALL WRITEGAME
GoTo 50

305 TC = TC + 1
REGION = NR
GAME = 5
ROUND = 89
CALL SETTINGS
NG%(TC, 0) = 903
NG%(TC, 1) = 904
NG%(TC, 15) = REGION
NG%(TC, 16) = ROUND
NG%(TC, 17) = GAME
V$(TC) = "REG" + Str$(REGION) + " GM 3 WINNER"
H$(TC) = "REG" + Str$(REGION) + " GM 4 WINNER"
YN$(TC, 0) = "XXX"
YN$(TC, 1) = "XXX"
CALL WRITEGAME
GoTo 50

400 '7 TEAM REGION
TC = TC + 1
REGION = NR
GAME = 1
ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 6, 0)
    NG%(TC, 1) = N%(NR, 3, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 6, 0)
    H$(TC) = N$(NR, 3, 0)
    YN$(TC, 0) = N$(NR, 6, 1)
    YN$(TC, 1) = N$(NR, 3, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 2
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 7, 0)
    NG%(TC, 1) = N%(NR, 2, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 7, 0)
    H$(TC) = N$(NR, 2, 0)
    YN$(TC, 0) = N$(NR, 7, 1)
    YN$(TC, 1) = N$(NR, 2, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 3
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 5, 0)
    NG%(TC, 1) = N%(NR, 4, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 5, 0)
    H$(TC) = N$(NR, 4, 0)
    YN$(TC, 0) = N$(NR, 5, 1)
    YN$(TC, 1) = N$(NR, 4, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 4
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 901
    NG%(TC, 1) = 902
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 1 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 5
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 903
    NG%(TC, 1) = N%(NR, 1, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 3 WINNER"
    H$(TC) = N$(NR, 1, 0)
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = N$(NR, 1, 1)
    CALL WRITEGAME
     REGION = NR
    IF C%(0) = 0 THEN 405: '1-YES, 0-NO CONSOLATION
    TC = TC + 1
     GAME = 6
     ROUND = 88
    CALL SETTINGS
    NG%(TC, 0) = 804
    NG%(TC, 1) = 805
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 4 LOSER"
    H$(TC) = "REG" + STR$(REGION) + " GM 5 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
        TC = TC + 1
     REGION = NR
     GAME = 7
     ROUND = 89
    CALL SETTINGS
    NG%(TC, 0) = 904
    NG%(TC, 1) = 905
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 4 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     GOTO 50

405   TC = TC + 1
     REGION = NR
     GAME = 6
     ROUND = 89
    CALL SETTINGS
    NG%(TC, 0) = 904
    NG%(TC, 1) = 905
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 4 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     GOTO 50

500 '8 TEAM REGION
    TC = TC + 1
    REGION = NR
    GAME = 1
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 7, 0)
    NG%(TC, 1) = N%(NR, 2, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 7, 0)
    H$(TC) = N$(NR, 2, 0)
    YN$(TC, 0) = N$(NR, 7, 1)
    YN$(TC, 1) = N$(NR, 2, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 2
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 6, 0)
    NG%(TC, 1) = N%(NR, 3, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 6, 0)
    H$(TC) = N$(NR, 3, 0)
    YN$(TC, 0) = N$(NR, 6, 1)
    YN$(TC, 1) = N$(NR, 3, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 3
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 5, 0)
    NG%(TC, 1) = N%(NR, 4, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 5, 0)
    H$(TC) = N$(NR, 4, 0)
    YN$(TC, 0) = N$(NR, 5, 1)
    YN$(TC, 1) = N$(NR, 4, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 4
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 8, 0)
    NG%(TC, 1) = N%(NR, 1, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 8, 0)
    H$(TC) = N$(NR, 1, 0)
    YN$(TC, 0) = N$(NR, 8, 1)
    YN$(TC, 1) = N$(NR, 1, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 5
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 901
    NG%(TC, 1) = 902
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 1 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 6
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 903
    NG%(TC, 1) = 904
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 3 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     REGION = NR
    IF C%(0) = 0 THEN 505: '1-YES, 0-NO CONSOLATION
    TC = TC + 1
     GAME = 7
     ROUND = 88
    CALL SETTINGS
    NG%(TC, 0) = 805
    NG%(TC, 1) = 806
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 5 LOSER"
    H$(TC) = "REG" + STR$(REGION) + " GM 6 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
        TC = TC + 1
     REGION = NR
     GAME = 8
     ROUND = 89
    CALL SETTINGS
    NG%(TC, 0) = 905
    NG%(TC, 1) = 906
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 5 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     GOTO 50

505   TC = TC + 1
     REGION = NR
     GAME = 7
     ROUND = 89
    CALL SETTINGS
    NG%(TC, 0) = 905
    NG%(TC, 1) = 906
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 5 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     GOTO 50

600 '9 TEAM REGION
    TC = TC + 1
    REGION = NR
    GAME = 1
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 9, 0)
    NG%(TC, 1) = N%(NR, 8, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 9, 0)
    H$(TC) = N$(NR, 8, 0)
    YN$(TC, 0) = N$(NR, 9, 1)
    YN$(TC, 1) = N$(NR, 8, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 2
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 7, 0)
    NG%(TC, 1) = N%(NR, 2, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 7, 0)
    H$(TC) = N$(NR, 2, 0)
    YN$(TC, 0) = N$(NR, 7, 1)
    YN$(TC, 1) = N$(NR, 2, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 3
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 6, 0)
    NG%(TC, 1) = N%(NR, 3, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 6, 0)
    H$(TC) = N$(NR, 3, 0)
    YN$(TC, 0) = N$(NR, 6, 1)
    YN$(TC, 1) = N$(NR, 3, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 4
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 5, 0)
    NG%(TC, 1) = N%(NR, 4, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 5, 0)
    H$(TC) = N$(NR, 4, 0)
    YN$(TC, 0) = N$(NR, 5, 1)
    YN$(TC, 1) = N$(NR, 4, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 5
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 901
    NG%(TC, 1) = N%(NR, 1, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 1 WINNER"
    H$(TC) = N$(NR, 1, 0)
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = N$(NR, 1, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 6
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 902
    NG%(TC, 1) = 903
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 2 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 3 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 7
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 904
    NG%(TC, 1) = 905
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 4 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     REGION = NR
    IF C%(0) = 0 THEN 605: '1-YES, 0-NO CONSOLATION
    TC = TC + 1
     GAME = 8
     ROUND = 88
    CALL SETTINGS
    NG%(TC, 0) = 806
    NG%(TC, 1) = 807
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 6 LOSER"
    H$(TC) = "REG" + STR$(REGION) + " GM 7 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
        TC = TC + 1
     REGION = NR
     GAME = 9
     ROUND = 89
    CALL SETTINGS
    NG%(TC, 0) = 906
    NG%(TC, 1) = 907
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 6 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 7 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
        GOTO 50

605   TC = TC + 1
     REGION = NR
     GAME = 8
     ROUND = 89
    CALL SETTINGS
    NG%(TC, 0) = 906
    NG%(TC, 1) = 907
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 6 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 7 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     GOTO 50

700 '10 TEAM REGION
    TC = TC + 1
    REGION = NR
    GAME = 1
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 10, 0)
    NG%(TC, 1) = N%(NR, 7, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 10, 0)
    H$(TC) = N$(NR, 7, 0)
    YN$(TC, 0) = N$(NR, 10, 1)
    YN$(TC, 1) = N$(NR, 7, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 2
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 9, 0)
    NG%(TC, 1) = N%(NR, 8, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 9, 0)
    H$(TC) = N$(NR, 8, 0)
    YN$(TC, 0) = N$(NR, 9, 1)
    YN$(TC, 1) = N$(NR, 8, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 3
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 901
    NG%(TC, 1) = N%(NR, 2, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 1 WINNER"
    H$(TC) = N$(NR, 2, 0)
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = N$(NR, 2, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 4
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 6, 0)
    NG%(TC, 1) = N%(NR, 3, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 6, 0)
    H$(TC) = N$(NR, 3, 0)
    YN$(TC, 0) = N$(NR, 6, 1)
    YN$(TC, 1) = N$(NR, 3, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 5
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 5, 0)
    NG%(TC, 1) = N%(NR, 4, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 5, 0)
    H$(TC) = N$(NR, 4, 0)
    YN$(TC, 0) = N$(NR, 5, 1)
    YN$(TC, 1) = N$(NR, 4, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 6
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 902
    NG%(TC, 1) = N%(NR, 1, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 2 WINNER"
    H$(TC) = N$(NR, 1, 0)
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = N$(NR, 1, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 7
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 903
    NG%(TC, 1) = 904
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 3 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 8
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 905
    NG%(TC, 1) = 906
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 5 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     REGION = NR
    IF C%(0) = 0 THEN 705: '1-YES, 0-NO CONSOLATION
    TC = TC + 1
     GAME = 9
     ROUND = 88
    CALL SETTINGS
    NG%(TC, 0) = 807
    NG%(TC, 1) = 808
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 7 LOSER"
    H$(TC) = "REG" + STR$(REGION) + " GM 8 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
        TC = TC + 1
     REGION = NR
     GAME = 10
     ROUND = 89
    CALL SETTINGS
    NG%(TC, 0) = 907
    NG%(TC, 1) = 908
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 7 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     GOTO 50

705   TC = TC + 1
     REGION = NR
     GAME = 9
     ROUND = 89
    CALL SETTINGS
    NG%(TC, 0) = 907
    NG%(TC, 1) = 908
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 7 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     GOTO 50

800 '11 TEAM REGION
    TC = TC + 1
    REGION = NR
    GAME = 1
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 10, 0)
    NG%(TC, 1) = N%(NR, 7, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 10, 0)
    H$(TC) = N$(NR, 7, 0)
    YN$(TC, 0) = N$(NR, 10, 1)
    YN$(TC, 1) = N$(NR, 7, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 2
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 11, 0)
    NG%(TC, 1) = N%(NR, 6, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 11, 0)
    H$(TC) = N$(NR, 6, 0)
    YN$(TC, 0) = N$(NR, 11, 1)
    YN$(TC, 1) = N$(NR, 6, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 3
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 9, 0)
    NG%(TC, 1) = N%(NR, 8, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 9, 0)
    H$(TC) = N$(NR, 8, 0)
    YN$(TC, 0) = N$(NR, 9, 1)
    YN$(TC, 1) = N$(NR, 8, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 4
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 901
    NG%(TC, 1) = N%(NR, 2, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 1 WINNER"
    H$(TC) = N$(NR, 2, 0)
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = N$(NR, 2, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 5
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 902
    NG%(TC, 1) = N%(NR, 3, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 2 WINNER"
    H$(TC) = N$(NR, 3, 0)
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = N$(NR, 3, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 6
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 5, 0)
    NG%(TC, 1) = N%(NR, 4, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 5, 0)
    H$(TC) = N$(NR, 4, 0)
    YN$(TC, 0) = N$(NR, 5, 1)
    YN$(TC, 1) = N$(NR, 4, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 7
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 903
    NG%(TC, 1) = N%(NR, 1, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 3 WINNER"
    H$(TC) = N$(NR, 1, 0)
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = N$(NR, 1, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 8
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 904
    NG%(TC, 1) = 905
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 4 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 9
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 906
    NG%(TC, 1) = 907
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 6 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 7 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     REGION = NR
    IF C%(0) = 0 THEN 805: '1-YES, 0-NO CONSOLATION
    TC = TC + 1
     GAME = 10
     ROUND = 88
    CALL SETTINGS
    NG%(TC, 0) = 808
    NG%(TC, 1) = 809
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 8 LOSER"
    H$(TC) = "REG" + STR$(REGION) + " GM 9 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
        TC = TC + 1
     REGION = NR
     GAME = 11
     ROUND = 89
    CALL SETTINGS
    NG%(TC, 0) = 908
    NG%(TC, 1) = 909
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 8 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 9 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     GOTO 50

805   TC = TC + 1
     REGION = NR
     GAME = 10
     ROUND = 89
    CALL SETTINGS
    NG%(TC, 0) = 908
    NG%(TC, 1) = 909
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 8 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 9 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     GOTO 50

900 '12 TEAM REGION
    TC = TC + 1
    REGION = NR
    GAME = 1
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 10, 0)
    NG%(TC, 1) = N%(NR, 7, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 10, 0)
    H$(TC) = N$(NR, 7, 0)
    YN$(TC, 0) = N$(NR, 10, 1)
    YN$(TC, 1) = N$(NR, 7, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 2
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 11, 0)
    NG%(TC, 1) = N%(NR, 6, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 11, 0)
    H$(TC) = N$(NR, 6, 0)
    YN$(TC, 0) = N$(NR, 11, 1)
    YN$(TC, 1) = N$(NR, 6, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 3
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 12, 0)
    NG%(TC, 1) = N%(NR, 5, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 12, 0)
    H$(TC) = N$(NR, 5, 0)
    YN$(TC, 0) = N$(NR, 12, 1)
    YN$(TC, 1) = N$(NR, 5, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 4
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 9, 0)
    NG%(TC, 1) = N%(NR, 8, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 9, 0)
    H$(TC) = N$(NR, 8, 0)
    YN$(TC, 0) = N$(NR, 9, 1)
    YN$(TC, 1) = N$(NR, 8, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 5
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 901
    NG%(TC, 1) = N%(NR, 2, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 1 WINNER"
    H$(TC) = N$(NR, 2, 0)
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = N$(NR, 2, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 6
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 902
    NG%(TC, 1) = N%(NR, 3, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 2 WINNER"
    H$(TC) = N$(NR, 3, 0)
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = N$(NR, 3, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 7
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 903
    NG%(TC, 1) = N%(NR, 4, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 3 WINNER"
    H$(TC) = N$(NR, 4, 0)
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = N$(NR, 4, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 8
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 904
    NG%(TC, 1) = N%(NR, 1, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 4 WINNER"
    H$(TC) = N$(NR, 1, 0)
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = N$(NR, 1, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 9
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 905
    NG%(TC, 1) = 906
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 5 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 10
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 907
    NG%(TC, 1) = 908
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 7 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     REGION = NR
    IF C%(0) = 0 THEN 905: '1-YES, 0-NO CONSOLATION
    TC = TC + 1
     GAME = 11
     ROUND = 88
    CALL SETTINGS
    NG%(TC, 0) = 809
    NG%(TC, 1) = 810
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 9 LOSER"
    H$(TC) = "REG" + STR$(REGION) + " GM 10 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
        TC = TC + 1
     REGION = NR
     GAME = 12
     ROUND = 89
    CALL SETTINGS
    NG%(TC, 0) = 909
    NG%(TC, 1) = 910
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 9 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 10 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     GOTO 50

905   TC = TC + 1
     REGION = NR
     GAME = 11
     ROUND = 89
    CALL SETTINGS
    NG%(TC, 0) = 909
    NG%(TC, 1) = 910
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 9 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 10 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     GOTO 50

1000 '13 TEAM REGION
    TC = TC + 1
    REGION = NR
    GAME = 1
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 13, 0)
    NG%(TC, 1) = N%(NR, 4, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 13, 0)
    H$(TC) = N$(NR, 4, 0)
    YN$(TC, 0) = N$(NR, 13, 1)
    YN$(TC, 1) = N$(NR, 4, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 2
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 12, 0)
    NG%(TC, 1) = N%(NR, 5, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 12, 0)
    H$(TC) = N$(NR, 5, 0)
    YN$(TC, 0) = N$(NR, 12, 1)
    YN$(TC, 1) = N$(NR, 5, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 3
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 9, 0)
    NG%(TC, 1) = N%(NR, 8, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 9, 0)
    H$(TC) = N$(NR, 8, 0)
    YN$(TC, 0) = N$(NR, 9, 1)
    YN$(TC, 1) = N$(NR, 8, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 4
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 10, 0)
    NG%(TC, 1) = N%(NR, 7, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 10, 0)
    H$(TC) = N$(NR, 7, 0)
    YN$(TC, 0) = N$(NR, 10, 1)
    YN$(TC, 1) = N$(NR, 7, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 5
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 11, 0)
    NG%(TC, 1) = N%(NR, 6, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 11, 0)
    H$(TC) = N$(NR, 6, 0)
    YN$(TC, 0) = N$(NR, 11, 1)
    YN$(TC, 1) = N$(NR, 6, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 6
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 901
    NG%(TC, 1) = 902
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 1 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 2 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 7
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 903
    NG%(TC, 1) = N%(NR, 1, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 3 WINNER"
    H$(TC) = N$(NR, 1, 0)
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = N$(NR, 1, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 8
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 904
    NG%(TC, 1) = N%(NR, 2, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 4 WINNER"
    H$(TC) = N$(NR, 2, 0)
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = N$(NR, 2, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 9
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 905
    NG%(TC, 1) = N%(NR, 3, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 5 WINNER"
    H$(TC) = N$(NR, 3, 0)
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = N$(NR, 3, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 10
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 906
    NG%(TC, 1) = 907
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 6 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 7 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 11
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 908
    NG%(TC, 1) = 909
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 8 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 9 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     REGION = NR
    IF C%(0) = 0 THEN 1005: '1-YES, 0-NO CONSOLATION
    TC = TC + 1
     GAME = 12
     ROUND = 88
    CALL SETTINGS
    NG%(TC, 0) = 810
    NG%(TC, 1) = 811
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 10 LOSER"
    H$(TC) = "REG" + STR$(REGION) + " GM 11 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
        TC = TC + 1
     REGION = NR
     GAME = 13
     ROUND = 89
    CALL SETTINGS
    NG%(TC, 0) = 910
    NG%(TC, 1) = 911
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 10 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 11 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     GOTO 50

1005  TC = TC + 1
     REGION = NR
     GAME = 12
     ROUND = 89
    CALL SETTINGS
    NG%(TC, 0) = 910
    NG%(TC, 1) = 911
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 10 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 11 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     GOTO 50

1100 '14 TEAM REGION
    TC = TC + 1
    REGION = NR
    GAME = 1
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 14, 0)
    NG%(TC, 1) = N%(NR, 3, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 14, 0)
    H$(TC) = N$(NR, 3, 0)
    YN$(TC, 0) = N$(NR, 14, 1)
    YN$(TC, 1) = N$(NR, 3, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 2
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 13, 0)
    NG%(TC, 1) = N%(NR, 4, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 13, 0)
    H$(TC) = N$(NR, 4, 0)
    YN$(TC, 0) = N$(NR, 13, 1)
    YN$(TC, 1) = N$(NR, 4, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 3
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 12, 0)
    NG%(TC, 1) = N%(NR, 5, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 12, 0)
    H$(TC) = N$(NR, 5, 0)
    YN$(TC, 0) = N$(NR, 12, 1)
    YN$(TC, 1) = N$(NR, 5, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 4
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 11, 0)
    NG%(TC, 1) = N%(NR, 6, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 11, 0)
    H$(TC) = N$(NR, 6, 0)
    YN$(TC, 0) = N$(NR, 11, 1)
    YN$(TC, 1) = N$(NR, 6, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 5
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 10, 0)
    NG%(TC, 1) = N%(NR, 7, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 10, 0)
    H$(TC) = N$(NR, 7, 0)
    YN$(TC, 0) = N$(NR, 10, 1)
    YN$(TC, 1) = N$(NR, 7, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 6
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 9, 0)
    NG%(TC, 1) = N%(NR, 8, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 9, 0)
    H$(TC) = N$(NR, 8, 0)
    YN$(TC, 0) = N$(NR, 9, 1)
    YN$(TC, 1) = N$(NR, 8, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 7
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 901
    NG%(TC, 1) = 904
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 1 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 8
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 905
    NG%(TC, 1) = N%(NR, 2, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 5 WINNER"
    H$(TC) = N$(NR, 2, 0)
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = N$(NR, 2, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 9
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 902
    NG%(TC, 1) = 903
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 2 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 3 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 10
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 906
    NG%(TC, 1) = N%(NR, 1, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 6 WINNER"
    H$(TC) = N$(NR, 1, 0)
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = N$(NR, 1, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 11
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 907
    NG%(TC, 1) = 908
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 7 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 12
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 909
    NG%(TC, 1) = 910
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 9 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 10 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     REGION = NR
    IF C%(0) = 0 THEN 1105: '1-YES, 0-NO CONSOLATION
    TC = TC + 1
     GAME = 13
     ROUND = 88
    CALL SETTINGS
    NG%(TC, 0) = 811
    NG%(TC, 1) = 812
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 11 LOSER"
    H$(TC) = "REG" + STR$(REGION) + " GM 12 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
        TC = TC + 1
     REGION = NR
     GAME = 14
     ROUND = 89
    CALL SETTINGS
    NG%(TC, 0) = 911
    NG%(TC, 1) = 912
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 11 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 12 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     GOTO 50

1105  TC = TC + 1
     REGION = NR
     GAME = 13
     ROUND = 89
    CALL SETTINGS
    NG%(TC, 0) = 911
    NG%(TC, 1) = 912
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 11 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 12 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     GOTO 50

1200 '15 TEAM REGION
    TC = TC + 1
    REGION = NR
    GAME = 1
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 15, 0)
    NG%(TC, 1) = N%(NR, 2, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 15, 0)
    H$(TC) = N$(NR, 2, 0)
    YN$(TC, 0) = N$(NR, 15, 1)
    YN$(TC, 1) = N$(NR, 2, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 2
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 14, 0)
    NG%(TC, 1) = N%(NR, 3, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 14, 0)
    H$(TC) = N$(NR, 3, 0)
    YN$(TC, 0) = N$(NR, 14, 1)
    YN$(TC, 1) = N$(NR, 3, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 3
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 13, 0)
    NG%(TC, 1) = N%(NR, 4, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 13, 0)
    H$(TC) = N$(NR, 4, 0)
    YN$(TC, 0) = N$(NR, 13, 1)
    YN$(TC, 1) = N$(NR, 4, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 4
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 12, 0)
    NG%(TC, 1) = N%(NR, 5, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 12, 0)
    H$(TC) = N$(NR, 5, 0)
    YN$(TC, 0) = N$(NR, 12, 1)
    YN$(TC, 1) = N$(NR, 5, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 5
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 11, 0)
    NG%(TC, 1) = N%(NR, 6, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 11, 0)
    H$(TC) = N$(NR, 6, 0)
    YN$(TC, 0) = N$(NR, 11, 1)
    YN$(TC, 1) = N$(NR, 6, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 6
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 10, 0)
    NG%(TC, 1) = N%(NR, 7, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 10, 0)
    H$(TC) = N$(NR, 7, 0)
    YN$(TC, 0) = N$(NR, 10, 1)
    YN$(TC, 1) = N$(NR, 7, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 7
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 9, 0)
    NG%(TC, 1) = N%(NR, 8, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 9, 0)
    H$(TC) = N$(NR, 8, 0)
    YN$(TC, 0) = N$(NR, 9, 1)
    YN$(TC, 1) = N$(NR, 8, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 8
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 901
    NG%(TC, 1) = 906
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 1 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 9
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 902
    NG%(TC, 1) = 905
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 2 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 10
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 903
    NG%(TC, 1) = 904
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 3 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 4 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 11
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 907
    NG%(TC, 1) = N%(NR, 1, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 7 WINNER"
    H$(TC) = N$(NR, 1, 0)
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = N$(NR, 1, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 12
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 908
    NG%(TC, 1) = 909
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 8 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 9 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 13
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 910
    NG%(TC, 1) = 911
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 10 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 11 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     REGION = NR
    IF C%(0) = 0 THEN 1205: '1-YES, 0-NO CONSOLATION
    TC = TC + 1
     GAME = 14
     ROUND = 88
    CALL SETTINGS
    NG%(TC, 0) = 812
    NG%(TC, 1) = 813
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 12 LOSER"
    H$(TC) = "REG" + STR$(REGION) + " GM 13 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
        TC = TC + 1
     REGION = NR
     GAME = 15
     ROUND = 89
    CALL SETTINGS
    NG%(TC, 0) = 912
    NG%(TC, 1) = 913
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 12 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 13 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     GOTO 50

1205  TC = TC + 1
     REGION = NR
     GAME = 14
     ROUND = 89
    CALL SETTINGS
    NG%(TC, 0) = 912
    NG%(TC, 1) = 913
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 12 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 13 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     GOTO 50

1300 '16 TEAM REGION
    TC = TC + 1
    REGION = NR
    GAME = 1
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 16, 0)
    NG%(TC, 1) = N%(NR, 1, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 16, 0)
    H$(TC) = N$(NR, 1, 0)
    YN$(TC, 0) = N$(NR, 16, 1)
    YN$(TC, 1) = N$(NR, 1, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 2
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 15, 0)
    NG%(TC, 1) = N%(NR, 2, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 15, 0)
    H$(TC) = N$(NR, 2, 0)
    YN$(TC, 0) = N$(NR, 15, 1)
    YN$(TC, 1) = N$(NR, 2, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 3
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 14, 0)
    NG%(TC, 1) = N%(NR, 3, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 14, 0)
    H$(TC) = N$(NR, 3, 0)
    YN$(TC, 0) = N$(NR, 14, 1)
    YN$(TC, 1) = N$(NR, 3, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 4
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 13, 0)
    NG%(TC, 1) = N%(NR, 4, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 13, 0)
    H$(TC) = N$(NR, 4, 0)
    YN$(TC, 0) = N$(NR, 13, 1)
    YN$(TC, 1) = N$(NR, 4, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 5
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 12, 0)
    NG%(TC, 1) = N%(NR, 5, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 12, 0)
    H$(TC) = N$(NR, 5, 0)
    YN$(TC, 0) = N$(NR, 12, 1)
    YN$(TC, 1) = N$(NR, 5, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 6
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 11, 0)
    NG%(TC, 1) = N%(NR, 6, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 11, 0)
    H$(TC) = N$(NR, 6, 0)
    YN$(TC, 0) = N$(NR, 11, 1)
    YN$(TC, 1) = N$(NR, 6, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 7
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 10, 0)
    NG%(TC, 1) = N%(NR, 7, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 10, 0)
    H$(TC) = N$(NR, 7, 0)
    YN$(TC, 0) = N$(NR, 10, 1)
    YN$(TC, 1) = N$(NR, 7, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 8
    ROUND = 1
    CALL SETTINGS
    NG%(TC, 0) = N%(NR, 9, 0)
    NG%(TC, 1) = N%(NR, 8, 0)
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = N$(NR, 9, 0)
    H$(TC) = N$(NR, 8, 0)
    YN$(TC, 0) = N$(NR, 9, 1)
    YN$(TC, 1) = N$(NR, 8, 1)
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 9
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 901
    NG%(TC, 1) = 908
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 1 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 8 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 10
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 904
    NG%(TC, 1) = 905
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 4 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 5 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 11
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 903
    NG%(TC, 1) = 906
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 3 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 6 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 12
    ROUND = 2
    CALL SETTINGS
    NG%(TC, 0) = 902
    NG%(TC, 1) = 907
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 2 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 7 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 13
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 909
    NG%(TC, 1) = 910
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 9 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 10 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     TC = TC + 1
    REGION = NR
    GAME = 14
    ROUND = 3
    CALL SETTINGS
    NG%(TC, 0) = 911
    NG%(TC, 1) = 912
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 11 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 12 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     REGION = NR
    IF C%(0) = 0 THEN 1305: '1-YES, 0-NO CONSOLATION
    TC = TC + 1
     GAME = 15
     ROUND = 88
    CALL SETTINGS
    NG%(TC, 0) = 813
    NG%(TC, 1) = 814
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 13 LOSER"
    H$(TC) = "REG" + STR$(REGION) + " GM 14 LOSER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
        TC = TC + 1
     REGION = NR
     GAME = 16
     ROUND = 89
    CALL SETTINGS
    NG%(TC, 0) = 913
    NG%(TC, 1) = 914
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 13 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 14 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     GOTO 50

1305  TC = TC + 1
     REGION = NR
     GAME = 15
     ROUND = 89
    CALL SETTINGS
    NG%(TC, 0) = 913
    NG%(TC, 1) = 914
    NG%(TC, 15) = REGION
    NG%(TC, 16) = ROUND
    NG%(TC, 17) = GAME
    V$(TC) = "REG" + STR$(REGION) + " GM 13 WINNER"
    H$(TC) = "REG" + STR$(REGION) + " GM 14 WINNER"
    YN$(TC, 0) = "XXX"
    YN$(TC, 1) = "XXX"
    CALL WRITEGAME
     GOTO 50

1104 NT = 0
    IF NOT _FILEEXISTS(D$(0) + "BASK." + YN$) THEN GOSUB 4500: BO% = 1: RETURN
OPEN D$(0) + "BASK." + YN$ FOR RANDOM AS #1 LEN = 655
    Y = LOF(1)
    FIELD #1, 15 AS Q$(0)
    FOR I1 = 1 TO Y / 655
    GET #1, I1
    FOR I2 = 1 TO 15
    IF ASC(MID$(Q$(0), I2, 1)) < 32 OR ASC(MID$(Q$(0), I2, 1)) > 123 OR UCASE$(LEFT$(Q$(0), 3)) = "NOT" OR UCASE$(LEFT$(Q$(0), 3)) = "UN" THEN 1103
    NEXT I2
    NT = NT + 1
    VV$(NT) = Q$(0): V%(NT) = I1

1103 NEXT I1
CLOSE 1

1102 SELECTION% = 1
    Count% = NT
    Start% = 1
    FileFGColr% = 15:FileBGColr% = 4
    HiLiteFGColr% = 0:HiLiteBGColr% = 7
    Rows% = 20
    Columns% = 4
    Spacing% = 6
    Row% = 4
    Column% = 1

1106 COLOR , 4: CLS : LOCATE , 33: COLOR 14: PRINT "TEAM SELECTION": COLOR 15
    LOCATE 25, 1: COLOR 14: PRINT "ESC"; : COLOR 15: PRINT "-SELECT NEW ID  PGDN  PGUP  "; : COLOR 14: PRINT "ENTER"; : COLOR 15: PRINT "-SELECTS TEAM";

_MouseShow "DEFAULT"
MAMenu VV$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

    IF EXITCODE% = 27 THEN BO% = 1: RETURN
    IF EXITCODE% = -81 THEN Start% = Start% + 80: SELECTION% = Start%
    IF EXITCODE% = -73 THEN Start% = Start% - 80: SELECTION% = Start%
    IF EXITCODE% = -71 THEN Start% = 1: SELECTION% = 1: GOTO 1106
    IF EXITCODE% = -79 THEN Start% = NT: SELECTION% = NT: GOTO 1106
    IF Start% < 1 THEN Start% = 1: SELECTION% = Start%
    IF Start% > NT THEN Start% = NT: SELECTION% = Start%
    IF EXITCODE% = -81 OR EXITCODE% = -73 THEN 1106
    D = V%(SELECTION%)
    LOCATE 25, 1: COLOR 14: PRINT "ESC"; : COLOR 15: PRINT "-SELECT NEW ID  LEFT/RIGHT ARROW  UP/DOWN ARROW  "; : COLOR 14: PRINT "ENTER"; : COLOR 15: PRINT "-SELECTS TEAM";
OPEN D$(0) + "BASK." + YN$ FOR RANDOM AS #1 LEN = 655
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 2 AS Q$(16), 2 AS Q$(17), 2 AS Q$(18), 2 _
AS Q$(19), 602 AS X$
GET #1, D
    A$ = Q$(0)

9005 IF RIGHT$(A$, 1) = " " THEN A$ = LEFT$(A$, LEN(A$) - 1): GOTO 9005
    SP = CVI(Q$(1)): DA = CVI(Q$(2)): V1% = CVI(Q$(3)): V2% = CVI(Q$(4))
    FOR I = 0 TO 4: L%(I) = CVI(Q$(I + 5)): NEXT
    FOR I = 0 TO 9: T%(I) = CVI(Q$(I + 10)): NEXT
CLOSE 1
    RETURN

610 ' *** LOAD IN A SAVED TOURNAMENT ***
    EDITCODE% = Q
    Spec$ = D$(0) + "*.TOU"
    null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
    Count% = DirCount%
    IF Count% = 0 THEN 4050
    REDIM F$(0 TO Count%)
    FOR X = 1 TO Count%
      F$(X) = SPACE$(12)
    NEXT
    F$(0) = Spec$
    CALL ReadFile(F$())
    CALL SortStr(VARPTR(F$(1)), Count, 0)
     CLS
    COLOR , 1: CLS : LOCATE , 28: COLOR 14
    PRINT "SELECT TOURNAMENT FIELD TO EDIT": COLOR 15
    SELECTION% = 1
    Start% = 1
    FileFGColr% = 15:FileBGColr% = 1
    HiLiteFGColr% = 0:HiLiteBGColr% = 7
    Rows% = 20
    Columns% = 4
    Spacing% = 6
    Row% = 4
    Column% = 1
    LOCATE 25, 1: COLOR 14: PRINT "ESC"; : COLOR 15
    PRINT "-SELECT NEW ID  PGDN  PGUP  "; : COLOR 14
    PRINT "ENTER"; : COLOR 15
    PRINT "-SELECTS TOURNAMENT FIELD";
    
_MouseShow "DEFAULT"
206  MAMenu F$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

    IF EXITCODE% = 27 THEN 10
    IF EXITCODE% = -71 THEN Start% = 1: EXITCODE% = 3: SELECTION% = 1: GOTO 206
    IF EXITCODE% = -79 THEN Start% = COUNT: EXITCODE% = 3: SELECTION% = COUNT: GOTO 206
    IF EXITCODE% = -73 THEN Start% = Start% - 80: IF Start% < 1 THEN Start% = 1
    IF EXITCODE% = -81 THEN Start% = Start% + 80: IF Start% > COUNT THEN Start% = COUNT
    IF EXITCODE% = -81 OR EXITCODE% = -73 THEN EXITCODE% = 3: SELECTION% = Start%: GOTO 206
    F$ = F$(SELECTION%)
    LOCATE 2, 33: PRINT F$
LOCATE , 33: PRINT "ANY CHANGE (YN)";
    COLOR 15, 0
604 I$ = GetKeyPress$
    IF UCASE$(I$) = "Y" THEN 610
    F$ = _TRIM$(F$)
    F$ = LEFT$(F$, LEN(F$) - 4)
    IF EDITCODE% = 3 THEN 612
OPEN D$(0) + F$ + ".INF" FOR INPUT AS #2
    INPUT #2, FF$
    INPUT #2, R: ' NUMBER OF REGIONS
    FOR I = 1 TO 16
    INPUT #2, NN%(I): ' NUMBER OF TEAMS IN REGION
    INPUT #2, R$(I): ' NAME OF REGION
    NEXT
    FOR I = 1 TO 16
    FOR I1 = 1 TO 16
    FOR I2 = 0 TO 3
    INPUT #2, N$(I, I1, I2): ' SEED SLOT NAME
    INPUT #2, N%(I, I1, I2): ' REGION, SEED NUMBER, MODE PLAY/TM#
    NEXT: NEXT: NEXT
    INPUT #2, C%(0): INPUT #2, C%(1): ' CONSOLATION GAME
CLOSE 2
OPEN D$(0) + F$ + ".TNY" FOR INPUT AS #1
    FOR I = 0 TO 14
    INPUT #1, OP%(I)
    NEXT
CLOSE 1
    IF C%(0) = 1 THEN C$ = "Y" ELSE C$ = "N"
    NR$ = STR$(R)
    GOTO 20

'Basically creating copies of TCX files with TOU extension
612 CALL FCOPY (D$(0)+F$+".TOU", D$(0)+F$+".TCX", BUFFER$, ERRCODE%)
    'SHELL "COPY " + D$(0) + F$ + ".TCX " + D$(0) + F$ + ".TOU >NUL:"
    LOCATE 10
    
    CALL CSMsgBox("ALL FILES SAVED...PRESS ANY KEY TO RETURN TO TOURNAMENT OPTIONS", 18, Pass)
    I$=GetKeyPress$
    CALL CSMsgBox("", 0, Pass)
    
    EDITCODE% = 0
    GOTO 10
      PRINT "THERE ARE "; R; " REGIONS"
    FOR I = 1 TO R
    PRINT "REGION"; I; " HAS "; NN%(I); " TEAMS"
    PRINT "REGION NAME IS "; R$(I)
    NEXT
    END

611  FOR I = 1 TO 16
    NN%(I) = 0
    R$(I) = ""
    NEXT
    FOR I = 1 TO 16
    FOR I1 = 1 TO 16
    FOR I2 = 0 TO 3
    N$(I, I1, I2) = ""
    N%(I, I1, I2) = 0
    NEXT: NEXT: NEXT
    C%(0) = 0: C%(1) = 0
RETURN

4050 LOCATE 10, 8

    CALL CSMsgBox("NO TOURNAMENT FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
    I$ = GetKeyPress$
    CALL CSMsgBox("", 0, Pass)
    GOTO 10


FUNCTION PadRight$ (X$, Length) 
'Essentially this pads space to the right of the string
    PadRight$ = X$ + SPACE$(Length - LEN(X$))  'for display only
END FUNCTION


SUB TITLE ()
    COLOR , 0: KEY OFF: CLS
    COLOR , 1: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 16: COLOR 12
    PRINT "COURTSIDE COLLEGE BASKETBALL TOURNAMENT SCHEDULER"
    LOCATE 24, 1: PRINT TAB(80); : LOCATE , 1
    PRINT "(C) LANCE HAFFNER/SHANNON LYNN 1987-1994";
    LOCATE 3, 1: COLOR 2, 0
    COLOR 15: PRINT "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
      PRINT "    TECHNICAL SUPPORT, CALL (800) 477-7032 EMAIL AT SportGames@AOL.COM"
    COLOR 10
END SUB

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
