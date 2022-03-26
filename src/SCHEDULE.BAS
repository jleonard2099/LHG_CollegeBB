'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'
' -- QEditype.BI'

DefInt A-Z
DECLARE SUB TITLE ()
'DECLARE SUB QEdit (array$(), Ky$, Action, Ed AS EditInfo)

Dim VV$(500), V%(500), TE$(1), TF$(1)
Dim Q$(375), AP%(1), T%(9)
Dim Z1$(30), Z1%(30), Z2$(30)
Dim H$(20), V$(20), MO$(3), item$(11)
Dim HS%(1700), VS%(1700), LO$(1), P$(2), E%(13)
Dim TS$(1), SC$(2), U5$(4), CM$(1), LC$(2)
Dim Y As Long, U6 As Single, M As Long
Dim D As Long
ReDim YN$(1700, 1), NG%(1700, 18)
ReDim DY(1700) As Long

' DIM Ed AS EditInfo              'create the editor's data structure

'commenting out here as it is mostly irrelevant
'CALL InitMouse(There)

F2$ = Chr$(0) + Chr$(60) 'extended code for the F2 function key
F3$ = Chr$(0) + Chr$(61) 'extended code for the F3 function key

Open "DEV.BAS" For Input As #1: Input #1, P$(0): Input #1, P$(1): Close 1

Data HH,CV,CH,CC
For X = 0 To 3
    Read MO$(X)
Next

10 Color 7, 0: Cls
NG%(0, 8) = 3
NG%(0, 3) = 1
NG%(0, 7) = 1
NG%(0, 2) = 1
NG%(0, 5) = 5
DT$ = "10-01-96"
TM$ = "07:30P"
TF$(0) = "N": TF$(1) = "Y"
TS$(0) = "N": TS$(1) = "Y"
SC$(0) = "NO SHOT CLOCK       "
SC$(1) = "45 SECOND SHOT CLOCK"
SC$(2) = "35 SECOND SHOT CLOCK"
U5$(0) = "HUMAN OPPONENT            "
U5$(1) = "COMPUTER IS VISITING TEAM "
U5$(2) = "COMPUTER IS HOME TEAM     "
U5$(3) = "COMPUTER VS COMPUTER      "
TE$(0) = "SHOOT 1+1"
TE$(1) = "SHOOT 2  "
LC$(1) = "HOME COURT ADVANTAGE"
LC$(2) = "NEUTRAL SITE        "
CM$(0) = "COACH AND SELECT SHOTS           "
CM$(1) = "COACH ONLY AND LET COMPUTER SHOOT"
Call TITLE
item$(1) = "ENTER A NEW SCHEDULE                             "
item$(2) = "RETRIEVE/EDIT/PLAY AN EXISTING SCHEDULE          "
item$(3) = "EXTRACT ONE TEAM'S SCHEDULE FROM LEAGUE SCHEDULE "
item$(4) = "EXTRACT A CONFERENCE SCHEDULE FROM MAIN SCHEDULE "
item$(5) = "CHANGE THE MODE OF PLAY FOR A TEAM IN A SCHEDULE "
item$(6) = "MAKE GLOBAL CHANGES TO A SCHEDULE                "
item$(7) = "GENERATE A STANDARD CONFERENCE SCHEDULE          "
item$(8) = "BOXSCORE MAINTENANCE / SAVE BOXSCORE PARAMETERS  "
item$(9) = "EXIT AUTO SCHEDULER TO COURTSIDE MAIN MENU       "
SELECTION% = 1
Count% = 9
Start% = 1
SCANCODE% = 9
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 6
Column% = 16
Call box0(5, 15, 15, 65, 2, 31)
    
_MouseShow "DEFAULT"
MMenuVert item$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then SELECTION% = 9
D3% = SELECTION% - 1

'D3% = 0 ENTER A NEW SCHEDULE
'D3% = 1 LOAD/EDIT EXISTING SCHEDULE
'D3% = 2 EXTRACT A TEAM'S SCHEDULE
'D3% = 3 NO LONGER AVAILABLE
'D3% = 4 MAIN MENU
On SELECTION% GOTO 20, 600, 600, 600, 600, 600, 1300, 4500

20 AP%(0) = 0: NS% = 0: TS% = 0

21 Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 1
Print " Ö"; String$(61, 196); "· ";: For II = 1 To JJ: Locate 6 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 7 + JJ, 8
Print " Ó"; String$(61, 196); "½ ";: Locate 7, 32
Locate , 12: Print "ENTER NAME FOR SCHEDULE: ": Print
Locate , 12: Print "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
Locate , 12: Print "SPACES. DO NOT ADD A FILE EXTENSION, THE SCHEDULER"
Locate , 12: Print "WILL AUTOMATICALLY ADD .SCD"
Locate , 12: Print "SAMPLE NAMES ARE ARKANSAS, LSU-68, IU76, ETC."
Locate 7, 37: Input F$
If F$ = "" Then 21
Locate 13, 12: Print "ANY CHANGE (YN) "
22 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 21
Color 15, 0

25 Cls
Locate 23, 1: Color , 1: Print Tab(80);: Locate 23, 1
Color 13: Print "SC";: Color 14: Print "-SHOT CLOCK  ";
Color 13: Print "MA";: Color 14: Print "-MODE OF PLAY  ";
Color 11: Print " G";: Color 14: Print "-GOTO GAME";
Color 11: Print " B";: Color 14: Print "-BOXSCORE OPTIONS"
Locate 24, 1: Color , 1: Print Tab(80);: Locate 24, 1
Color 11: Print "C";: Color 14: Print "-SAVE AND CONTINUE  ";
Color 11: Print "A";: Color 14: Print "-ADD A GAME  ";
Color 11: Print "D";: Color 14: Print "-DELETE A GAME  ";
Color 11: Print "E";: Color 14: Print "-EDIT A GAME  ";
Color 11: Print "PGUP  PGDN";
Locate 25, 1: Color , 1: Print Tab(80);: Locate 25, 1
Color 11: Print "S";: Color 14: Print "-SORT GAMES  ";
Color 11: Print "P";: Color 14: Print "-PLAY GAMES   ";
Color 11: Print "X";: Color 14: Print "-SAVE AND EXIT  ";
Color 11: Print "Q";: Color 14: Print "-QUIT  ";
Color 11: Print "R";: Color 14: Print "-RESET SCORES";
Locate 1, 1: Print Tab(80);: Locate 1, 1: Print F$
Locate 1, 33: Color 14: Print "AUTO SCHEDULER"
Color , 0

50 Locate 2, 1
Color 14
Print "GAME  DATE      TIME    VISITING TEAM        HOME TEAM            MA  HC  3  SC": Color 15
For X = 1 To 20
    Locate X + 2, 1
    If X + NS% * 20 > AP%(0) Then 100
    T1 = NG%(X + NS% * 20, 0): YN$ = YN$(X + NS% * 20, 0)
    GoSub 9000
    V$(X) = N$
    T1 = NG%(X + NS% * 20, 1): YN$ = YN$(X + NS% * 20, 1)
    GoSub 9000
    H$(X) = N$
    Print Using "####  "; X + NS% * 20;
    Print Using "##/##/##"; NG%(X + NS% * 20, 17); NG%(X + NS% * 20, 16); NG%(X + NS% * 20, 18);
    Locate , 17
    I$ = "A"
    HR% = NG%(X + NS% * 20, 15) / 100
    MIN% = NG%(X + NS% * 20, 15) - HR% * 100
    If HR% = 12 Then I$ = "P"
    If HR% > 12 Then HR% = HR% - 12: I$ = "P"
    TM$ = Right$(Str$(HR%), 2) + ":"
    If MIN% < 10 Then TM$ = TM$ + "0" + Right$(Str$(MIN%), 1)
    If MIN% >= 10 Then TM$ = TM$ + Right$(Str$(MIN%), 2)
    TM$ = TM$ + I$
    Print TM$;
    Locate , 25: Print V$(X); " ";
    If NG%(X + NS% * 20, 13) = 1 Then Print Using "###"; VS%(X + NS% * 20);
    Locate , 46: Print H$(X); " ";
    If NG%(X + NS% * 20, 13) = 1 Then Print Using "###"; HS%(X + NS% * 20);
    Locate , 67: Print MO$(NG%(X + NS% * 20, 8));
    Locate , 72: If NG%(X + NS% * 20, 2) = 1 Then Print "Y";: Else Print "N";
    Locate , 75: Print TS$(NG%(X + NS% * 20, 3));
    Locate , 78: Print Left$(SC$(NG%(X + NS% * 20, 7)), 2)
Next
100 I$ = GetKeyPress$
If UCase$(I$) = "A" Then GoSub 150: GoTo 25
If UCase$(I$) = "E" Then GoSub 140: GoTo 25
'IF UCASE$(I$) = "B" THEN 5000
If UCase$(I$) = "G" Then GoSub 120: GoTo 25
If UCase$(I$) = "D" Then GoSub 200: GoTo 25
If UCase$(I$) = "S" Then GoSub 8500: GoTo 25
If UCase$(I$) = "C" Then GoSub 350: GoTo 25
If UCase$(I$) = "X" Then GoSub 350: GoTo 10
If I$ = Chr$(0) + "Q" Then GoSub 130: GoTo 25
If I$ = Chr$(0) + "I" Then GoSub 135: GoTo 25
If UCase$(I$) = "Q" Then 10
If UCase$(I$) = "P" Then GoSub 350: GoTo 300
If UCase$(I$) = "R" Then GoSub 400: GoTo 25
GoTo 100

120 'GOTO A SPECIFIC PAGE
Locate 23, 1: Print "ENTER GAME # TO GO TO        "
Locate 23, 1: Input "ENTER GAME # TO GO TO "; NG%
If NG% < 1 Or NG% > AP%(0) Then 120
NS% = Int((NG% - 1) / 20)
Return

130 ' *** PAGE DOWN ***
If NS% >= BS% Then NS% = BS%: GoTo 133
NS% = NS% + 1

133 Return

135 ' *** PAGE UP ***
If NS% = 0 Then 137
NS% = NS% - 1

137 Return

140 ' *** EDIT A GAME ***
Locate 23, 1: Print "ENTER GAME # TO EDIT        "
Locate 23, 1: Input "ENTER GAME # TO EDIT "; NG%
If NG% < 1 Or NG% > AP%(0) Then 140
V$ = V$(NG% - Int(NG% / 20) * 20)
H$ = H$(NG% - Int(NG% / 20) * 20)
If NG% - Int(NG% / 20) * 20 = 0 Then V$ = V$(20): H$ = H$(20)
I$ = "A"
HR% = Int(NG%(NG%, 15) / 100)
MIN% = NG%(NG%, 15) - HR% * 100
If HR% = 12 Then I$ = "P"
If HR% > 12 Then HR% = HR% - 12: I$ = "P"
TM$ = Right$(Str$(HR%), 2) + ":"
If MIN% < 10 Then TM$ = TM$ + "0" + Right$(Str$(MIN%), 1)
If MIN% >= 10 Then TM$ = TM$ + Right$(Str$(MIN%), 2)
TM$ = TM$ + I$
DT$ = Right$(Str$(NG%(NG%, 17)), 2) + "/"
DY = Val(Right$(Str$(NG%(NG%, 16)), 2))
If DY < 10 Then DAY$ = "0" + Right$(Str$(NG%(NG%, 16)), 1)
If DY >= 10 Then DAY$ = Right$(Str$(NG%(NG%, 16)), 2)
DT$ = DT$ + DAY$ + "/" + Right$(Str$(NG%(NG%, 18)), 2)
GoTo 149

150 ' *** ADD A GAME TO THE SCHEDULE ***
V$ = "": H$ = ""
AP%(0) = AP%(0) + 1
NG% = AP%(0)
BS% = Int((NG% - 1) / 20): If NS% < BS% Then NS% = BS%
For X = 0 To 18
    If X = 13 Then 139
    NG%(NG%, X) = NG%(NG% - 1, X)

139 Next
For X = 0 To 1: YN$(NG%, X) = YN$(NG% - 1, X): Next

149 Cls: Color 2: Print Tab(29); "SELECT GAME OPTIONS": Print

151 Locate 2, 2: Color 14: Print "(1) ";: Color 15: Print U5$(NG%(NG%, 8))

152 Locate 3, 2: Color 14: Print "(2) ";: Color 15: Print "VISITING TEAM: "; V$

153 Locate 4, 2: Color 14: Print "(3) ";: Color 15: Print "HOME TEAM: "; H$

154 Locate 5, 2: Color 14: Print "(4) ";: Color 15: Print "3-POINT SHOT: "; TS$(NG%(NG%, 3))

155 Locate 6, 2: Color 14: Print "(5) ";: Color 15: Print "SHOT CLOCK "; SC$(NG%(NG%, 7))

156 Locate 7, 2: Color 14: Print "(6) ";: Color 15: Print "HOME COURT ADVANTAGE: "; LC$(NG%(NG%, 2))

157 Locate 8, 2: Color 14: Print "(7) ";: Color 15: Print Using "DISPLAY PAUSE IN SECONDS: ##.#  "; NG%(NG%, 4) / 10

158 Locate 9, 2: Color 14: Print "(8) ";: Color 15: Print "FOULS TO DISQUALIFY: "; NG%(NG%, 5)

159 Locate 10, 2: Color 14: Print "(9) ";: Color 15: Print "HUMAN COACHES WILL: "; CM$(NG%(NG%, 9))

160 Locate 11, 2: Color 14: Print "(A) ";: Color 15: Print "GAME DATE (XX-XX-XX) "; DT$; "   "

161 Locate 12, 2: Color 14: Print "(B) ";: Color 15: Print "GAME TIME (XX:XXP) "; TM$; "   "

162 Locate 13, 2: Color 14: Print "(C) ";: Color 15: Print "FREE THROWS ON 10TH FOUL "; TE$(NG%(NG%, 6))

164 Locate 14, 2: Color 14: Print "(D) ";: Color 15: Print "3-FT WHEN FOULED ON 3-PT ATTEMPT "; TF$(NG%(NG%, 10))

165 Locate 15, 2: Color 14: Print "(E) ";: Color 15: Print "1 FT ON 1ST 6 FLOOR FOULS (PRIOR TO 1973): "; TF$(NG%(NG%, 14))

163 Locate 17, 2: Color 14: Print "(0) ";: Color 15: Print "ACCEPT SETTINGS"
175 I$ = GetKeyPress$
If UCase$(I$) = "A" Then I$ = "11"
If UCase$(I$) = "B" Then I$ = "12"
If UCase$(I$) = "C" Then I$ = "13"
If UCase$(I$) = "D" Then I$ = "14"
If UCase$(I$) = "E" Then I$ = "15"
I8% = Val(I$): If I8% < 0 Or I8% > 15 Then 175
On I8% GOTO 180, 181, 181, 184, 185, 186, 187, 188, 189, 175, 190, 191, 192, 193, 194
If V$ <> "" And H$ <> "" And TM$ <> "" And DT$ <> "" Then 250
GoTo 175

250 I$ = Right$(TM$, 1): TM$ = Left$(TM$, Len(TM$) - 1)
MIN% = Val(Right$(TM$, 2))
TM$ = Left$(TM$, Len(TM$) - 3)
HR% = Val(TM$)
If UCase$(I$) = "P" And HR% < 12 Then HR% = HR% + 12
NG%(NG%, 15) = HR% * 100 + MIN%
NG%(NG%, 17) = Val(Left$(DT$, 2))
NG%(NG%, 18) = Val(Mid$((DT$), 7, 2))
NG%(NG%, 16) = Val(Mid$((DT$), 4, 2))
Return

193 NG%(NG%, 10) = 1 - NG%(NG%, 10): GoTo 149

194 NG%(NG%, 14) = 1 - NG%(NG%, 14): GoTo 149

180 NG%(NG%, 8) = NG%(NG%, 8) + 1: If NG%(NG%, 8) > 3 Then NG%(NG%, 8) = 0
GoTo 151

181 Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 4
Print " Ö"; String$(61, 196); "· ";: For II = 1 To JJ: Locate 6 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 7 + JJ, 8
Print " Ó"; String$(61, 196); "½ ";: Locate 7, 32
Locate , 12: Print "ENTER DISK ID: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
Locate 7, 27: Input NY$
If NY$ = "" Then GoSub 1102: GoTo 182
YN$ = NY$
For II = 1 To 500: VV$(II) = "": Next
GoSub 1100

182 If BO% = 1 Then BO% = 0: GoTo 181

183 Locate 2, 33: Color T%(0), T%(1): Print N$: Color 15, 4
Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 181
Color 15, 0
NG%(NG%, I8% - 2) = T1: YN$(NG%, I8% - 2) = YN$

2 If I8% = 2 Then V$ = N$: V$(NG% - Int(NG% / 20) * 20) = N$

3 If I8% = 3 Then H$ = N$: H$(NG% - Int(NG% / 20) * 20) = N$
GoTo 149

184 NG%(NG%, 3) = 1 - NG%(NG%, 3): GoTo 154

185 NG%(NG%, 7) = 1 + NG%(NG%, 7)
If NG%(NG%, 7) > 2 Then NG%(NG%, 7) = 0
GoTo 155

186 N = 1 - (NG%(NG%, 2) - 1): NG%(NG%, 2) = N + 1: GoTo 156

187 Locate 8, 32: Input U6: NG%(NG%, 4) = U6 * 10: GoTo 157

188 NG%(NG%, 5) = 1 + NG%(NG%, 5): If NG%(NG%, 5) > 6 Then NG%(NG%, 5) = 5
GoTo 158

189 NG%(NG%, 9) = 1 - NG%(NG%, 9): GoTo 159

190 Locate 11, 27: Input DT$: GoTo 160

191 Locate 12, 25: Input TM$: GoTo 161

192 NG%(NG%, 6) = 1 - NG%(NG%, 6): GoTo 162

200 ' *** DELETE A GAME FROM THE SCHEDULE ***
Locate 24, 1: Input "ENTER GAME # TO DELETE "; NG%
If NG% < 1 Or NG% > AP%(0) Then 200
For X = NG% To AP%(0) - 1
    For X1 = 0 To 18: NG%(X, X1) = NG%(X + 1, X1): Next
    VS%(X) = VS%(X + 1)
    HS%(X) = HS%(X + 1)
    For X1 = 0 To 1: YN$(X, X1) = YN$(X + 1, X1): Next
Next
AP%(0) = AP%(0) - 1: BS% = Int((AP%(0) - 1) / 20)
If NS% > BS% Then NS% = BS%
Return

300 ' *** PLAY GAMES ***
Color , 0: Cls
JJ = 8
Locate 6, 8: Color 15, 4
Print " Ö"; String$(61, 196); "· ";: For II = 1 To JJ: Locate 6 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 7 + JJ, 8
Print " Ó"; String$(61, 196); "½ ";: Locate 7, 32
Locate , 12: Print "TOTAL GAMES ="; AP%(0)
Locate , 12: Print "SELECT GAME # TO START AND END WITH": Print
Locate , 12: Input "ENTER START GAME #: "; SG%: Print
Locate , 12: Input "ENTER END GAME #:"; EG%: Print
Locate , 12: Print "ANY CHANGE (YN)"
301 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 300
If SG% <= 0 Then SG% = 1
If EG% > AP%(0) Then EG% = AP%(0)
Open "GAME82" For Output As #2
Print #2, F$ + ".SCD"
Print #2, SG%
Print #2, EG%
Print #2, 0
Close 2
GoSub 1200
Open "TICKER" For Output As #2
For I = 1 To 32: For I1 = 0 To 1: For I2 = 1 To 14
Print #2, -1: Next: Next: Next
For I = 1 To 32: For I1 = 0 To 1: Print #2, "XXX"
Next: Next
For I = SG% To SG% + 31
    Print #2, NG%(I, 12)
Next
Close 2

335 Run "COLHOOP.EXE"

350 ' *** SAVE SCHEDULE FILE ***

351 If _FileExists(P$(0) + F$ + ".SCD") Then Kill P$(0) + F$ + ".SCD"
Open P$(0) + F$ + ".SCD" For Random As #1 Len = 48
For X = 0 To 18
    Field #1, X * 2 As X$, 2 As Q$(X + 1), 48 - 2 - 2 * X As X$
Next
Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)
For I = 1 To AP%(0)
    For X = 0 To 18: LSet Q$(X + 1) = MKI$(NG%(I, X)): Next
    For X = 0 To 1: LSet Q$(20 + X) = YN$(I, X): Next
    LSet Q$(22) = MKI$(VS%(I)): LSet Q$(23) = MKI$(HS%(I))
    Put #1, I
Next
Close 1
Return

400 ' *** RESET GAMES ***
For I = 1 To AP%(0)
    NG%(I, 13) = 0
Next
Return

600 ' *** LOAD IN A SAVED SCHEDULE ***
Spec$ = P$(0) + "*.SCD"
GoSub 2100

601 Open P$(0) + F$ + ".SCD" For Random As #1 Len = 48
Y = LOF(1)
Close 1
AP%(0) = Y / 48
BS% = Int((AP%(0) - 1) / 20)
Open P$(0) + F$ + ".SCD" For Random As #1 Len = 48
For X = 0 To 18
    Field #1, X * 2 As X$, 2 As Q$(X + 1), 48 - 2 - 2 * X As X$
Next
Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)
For I = 1 To AP%(0)
    Get #1, I
    For X = 0 To 18: NG%(I, X) = CVI(Q$(X + 1)): Next
    For X = 0 To 1: YN$(I, X) = Q$(20 + X): Next
    VS%(I) = CVI(Q$(22)): HS%(I) = CVI(Q$(23))
Next
Close 1: NS% = 0: TS% = 0
If D3% = 2 Or D3% = 3 Or D3% = 4 Or D3% = 5 Then 700
GoTo 25

700 'EXTRACT A SCHEDULE FROM ANOTHER
If D3% = 4 Then 725
If D3% = 5 Then
    GoSub 900
    GoSub 350
    GoTo 25
End If
Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 1
Print " Ö"; String$(61, 196); "· ";: For II = 1 To JJ: Locate 6 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 7 + JJ, 8
Print " Ó"; String$(61, 196); "½ ";: Locate 7, 32
Locate , 12: Print "ENTER NAME FOR SUB-SCHEDULE: ": Print
Locate , 12: Print "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
Locate , 12: Print "SPACES. DO NOT ADD A FILE EXTENSION, THE SCHEDULER"
Locate , 12: Print "WILL AUTOMATICALLY ADD .SCD"
Locate , 12: Print "SAMPLE NAMES ARE INDIANA, UCLA-68, KY-76, ETC"
Locate 7, 37: Input SF$
If SF$ = "" Then 700
Locate 13, 12: Print "ANY CHANGE (YN) "
705 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 700
Color 15, 0
Cls

725 If D3% = 3 Then 800
Color 15, 0: Cls
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
If NY$ = "" Then GoSub 1102: GoTo 730
YN$ = NY$
For I = 1 To 500: VV$(I) = "": Next
GoSub 1100

730 If BO% = 1 Then BO% = 0: GoTo 725

732 Locate 2, 33: Color T%(0), T%(1): Print N$: Color 15, 4
Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 725
Color 15, 0
If D3% = 4 Then GoSub 775
'HAVE T1 AND YN$
For I = 1 To AP%(0)

    735 If Right$(YN$(I, 0), 1) = " " Then YN$(I, 0) = Left$(YN$(I, 0), Len(YN$(I, 0)) - 1): GoTo 735

    736 If Right$(YN$(I, 1), 1) = " " Then YN$(I, 1) = Left$(YN$(I, 1), Len(YN$(I, 1)) - 1): GoTo 736
    If T1 = NG%(I, 0) And YN$ = YN$(I, 0) Or T1 = NG%(I, 1) And YN$ = YN$(I, 1) Then GoSub 780: GoSub 750
Next
If D3% = 4 Then GoSub 350
GoTo 10

750 If D3% = 4 Then 755
Locate 1, 1: Print NG% + 1; " MATCHES"
Open P$(0) + SF$ + ".SCD" For Random As #1 Len = 48
Y = LOF(1): GoTo 753

752 Y = 0

753 NG% = Y / 48
For X = 0 To 18
    Field #1, X * 2 As X$, 2 As Q$(X + 1), 48 - 2 - 2 * X As X$
Next
Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)
For X = 0 To 18: LSet Q$(X + 1) = MKI$(NG%(I, X)): Next
For X = 0 To 1: LSet Q$(20 + X) = YN$(I, X): Next
LSet Q$(22) = MKI$(VS%(I)): LSet Q$(23) = MKI$(HS%(I))
Put #1, NG% + 1
Close 1

755 Return

775 Cls
Color T%(0), T%(1): Print N$;: Color 15, 0: Print " IS TO BE:"
Print
Print "(0) MANUALLY CONTROLLED": Print
Print "(1) COMPUTER CONTROLLED"
776 I$ = GetKeyPress$
I1 = Val(I$)
If I1 < 0 Or I1 > 1 Then 776
Input "ENTER DISPLAY VARIABLE FOR THESE GAMES "; U6
U6 = U6 * 10
Return

780 ' CHANGE SETTINGS FOR A TEAM
If T1 = NG%(I, 0) And YN$ = YN$(I, 0) Then NG%(I, 8) = 2
If T1 = NG%(I, 1) And YN$ = YN$(I, 1) Then NG%(I, 8) = 1
If (T1 = NG%(I, 0) And YN$ = YN$(I, 0) Or T1 = NG%(I, 1) And YN$ = YN$(I, 1)) And I1 = 1 And D3% = 3 Then NG%(I, 8) = 3
NG%(I, 4) = U6
Return

800 Spec$ = P$(1) + "*.LIB"
null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then 4700
ReDim F$(0 To Count%)
For X = 1 To Count%
    F$(X) = Space$(12)
Next
F$(0) = Spec$
Call ReadFile(F$())
'CALL SortStr(VARPTR(F$(1)), Count%, 0)
GoSub 1310
Open P$(1) + F$ + ".LIB" For Input As #1
Input #1, X%
Input #1, LG$
Input #1, X$: Input #1, X$
For X = 1 To 30
    Input #1, Z1$(X): Input #1, Z1%(X): Input #1, Z2$(X)
Next
NT = 30
Close 1
For I = 1 To AP%(0)

    835 If Right$(YN$(I, 0), 1) = " " Then YN$(I, 0) = Left$(YN$(I, 0), Len(YN$(I, 0)) - 1): GoTo 835

    836 If Right$(YN$(I, 1), 1) = " " Then YN$(I, 1) = Left$(YN$(I, 1), Len(YN$(I, 1)) - 1): GoTo 836
    For I1 = 1 To NT
        '    PRINT AP%(0),Z1%(I1),NG%(I,0),NG%(I,1), Z2$(I1),YN$(I,0),YN$(I,1)
        '    839 I$=GetKeyPress$
        If Z1$(I1) = "XXX" Then 837
        If Z1%(I1) = NG%(I, 0) And Z2$(I1) = YN$(I, 0) Or Z1%(I1) = NG%(I, 1) And Z2$(I1) = YN$(I, 1) Then GoSub 850: GoTo 838

    837 Next I1

838 Next I
GoTo 10

850 Print "FOUND A MATCH"
'    IF NOT _FILEEXISTS(DRI$(0) + SF$ + ".SCD") THEN 852
Open P$(0) + SF$ + ".SCD" For Random As #1 Len = 48
Y = LOF(1): GoTo 853

852 Y = 0

853 NG% = Y / 48
For X = 0 To 18
    Field #1, X * 2 As X$, 2 As Q$(X + 1), 48 - 2 - 2 * X As X$
Next
Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)
For X = 0 To 18: LSet Q$(X + 1) = MKI$(NG%(I, X)): Next
For X = 0 To 1: LSet Q$(20 + X) = YN$(I, X): Next
LSet Q$(22) = MKI$(VS%(I)): LSet Q$(23) = MKI$(HS%(I))
Put #1, NG% + 1
Close 1

864 Return
    

900 ' GLOBAL SETTINGS FOR A SCHEDULE
U5% = 3: RC% = 1: SC% = 1: CM% = 1
NF% = 5: FT% = 0
FR% = 1: LP% = 0
Color 15, 0: Cls: Print Tab(30); "GLOBAL SETTINGS FOR SCHEDULE"

901 Locate 3, 2: Color 14: Print "(1) ";: Color 15: Print "TYPE OF GAME: "; U5$(U5%)
Locate 4, 2: Color 14: Print "(2) ";: Color 15: Print Using "DISPLAY PAUSE IN SECONDS: ##.#  "; U6 / 10
Locate 5, 2: Color 14: Print "(3) ";: Color 15: Print "OPTION RESERVED FOR FUTURE USE"
Locate 6, 2: Color 14: Print "(4) ";: Color 15: Print "SHOT CLOCK: "; SC$(SC%)
Locate 7, 2: Color 14: Print "(5) ";: Color 15: Print "HUMAN PLAYERS: "; CM$(CM%)
Locate 8, 2: Color 14: Print "(6) ";: Color 15: Print "3-POINT SHOT: "; TS$(RC%)
Locate 9, 2: Color 14: Print "(7) ";: Color 15: Print Using "FOULS TO DISQUALIFY: ##"; NF%
Locate 10, 2: Color 14: Print "(8) ";: Color 15: Print "FREE THROWS ON 10TH FOUL "; TE$(FT%)
Locate 11, 2: Color 14: Print "(9) ";: Color 15: Print "3-FT WHEN FOULED ON 3-PT ATTEMPT "; TF$(FR%)
Locate 12, 2: Color 14: Print "(A) ";: Color 15: Print "1 FT ON 1ST 6 FLOOR FOULS (PRIOR TO 1973): "; TF$(LP%)
Locate 13, 2: Color 14: Print "(0) ";: Color 15: Print "ACCEPT GLOBAL SETTINGS"
905 I$ = GetKeyPress$
If UCase$(I$) = "A" Then I$ = "10"
I = Val(I$)
If I < 0 Or I > 10 Then 905
If I = 3 Then 901
If I = 1 Then
    U5% = U5% + 1
    If U5% > 3 Then U5% = 0
    GoTo 901
End If
If I = 4 Then
    SC% = 1 + SC%
    If SC% > 2 Then SC% = 0
    GoTo 901
End If
If I = 5 Then CM% = 1 - CM%: GoTo 901
If I = 8 Then FT% = 1 - FT%: GoTo 901
If I = 9 Then FR% = 1 - FR%: GoTo 901
If I = 10 Then LP% = 1 - LP%: GoTo 901
If I = 6 Then RC% = 1 - RC%: GoTo 901
If I = 2 Then Input "DISPLAY VARIABLE "; U6: U6 = U6 * 10: GoTo 901
If I = 7 Then
    NF% = NF% + 1
    If NF% > 6 Then NF% = 5
    GoTo 901
End If
For I = 1 To AP%(0)
    NG%(I, 8) = U5%
    NG%(I, 4) = U6
    NG%(I, 7) = SC%
    NG%(I, 9) = CM%
    NG%(I, 3) = RC%
    NG%(I, 5) = NF%
    NG%(I, 6) = FT%
    NG%(I, 10) = FR%
    NG%(I, 14) = LP%
Next
Return

1100 NT = 0
Open P$(0) + "BASK." + YN$ For Random As #1 Len = 655
Y = LOF(1)
Field #1, 15 As Q$(0), 640 As X$
For I1 = 1 To Y / 655
    Get #1, I1
    NT = NT + 1
    VV$(I1) = Q$(0): V%(I1) = I1
Next
Close 1
NT = Y / 655
TS% = Int((NT - 1) / 80)

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
Color , 4: Cls: Locate , 33: Color 14
Print "TEAM SELECTION": Color 15
Locate 25, 1: Color 14: Print "ESC";: Color 15
Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
Print "ENTER";: Color 15: Print "-SELECTS TEAM";
    
_MouseShow "DEFAULT"
206 MAMenu VV$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then BO% = 1: Return
If EXITCODE% = -71 Then Start% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 206
If EXITCODE% = -79 Then Start% = NT: EXITCODE% = 3: SELECTION% = NT: GoTo 206
If EXITCODE% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If EXITCODE% = -81 Then Start% = Start% + 80: If Start% > NT Then Start% = NT
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = Start%: GoTo 206
T1 = V%(SELECTION%)
GoTo 9000

1200 For I = 1 To AP%(0)
    HR% = Int(NG%(I, 15) / 100)
    MIN% = NG%(I, 15) - HR% * 100
    TM% = HR% * 60 + MIN%
    NG%(I, 12) = Int(TM% / 30 - 18)
Next
Return

2100 null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then 4400
ReDim F$(0 To Count%)
For X = 1 To Count%
    F$(X) = Space$(12)
Next
F$(0) = Spec$
Call ReadFile(F$())
'CALL SortStr(VARPTR(F$(1)), Count%, 0)

1310 Cls
Color , 1
Cls
Locate , 28
Color 14
Print "SELECT SCHEDULE TO VIEW": Color 15
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
Print "-SELECTS SCHEDULE";

_MouseShow "DEFAULT"
320 MAMenu F$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then 2900
If EXITCODE% = -71 Then Start% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 320
If EXITCODE% = -79 Then Start% = Count%: EXITCODE% = 3: SELECTION% = Count%: GoTo 320
If EXITCODE% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If EXITCODE% = -81 Then Start% = Start% + 80: If Start% > Count% Then Start% = Count%
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = Start%: GoTo 320
F$ = F$(SELECTION%)
Locate 2, 33: Print F$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3910 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1310
F$ = RTrim$(F$)
WE$ = Left$(F$, Len(F$) - 4)
Color 15, 0
F$ = WE$
Return

8500 ' *** SORT SCHEDULE ***
For I = 1 To AP%(0)
    HR% = Int(NG%(I, 15) / 100)
    MIN% = NG%(I, 15) - HR% * 100
    TM% = HR% * 60 + MIN%
    NG%(I, 12) = Int(TM% / 30 - 18)
    D = (NG%(I, 18) - 1): D = D * 372
    M = (NG%(I, 17) - 1)
    M = M * 31
    Y = NG%(I, 16)
    DY(I) = (D + M + Y) * 24 * 60 + TM%
Next
For Z = 1 To AP%(0) - 1: For Z1 = Z + 1 To AP%(0)
    Locate 1, 1: Print Z, Z1
    If NG%(Z, 12) <= NG%(Z1, 12) Then 8505
        GoSub 8525

8505 Next Z1: Next Z
For Z = 1 To AP%(0) - 1: For Z1 = Z + 1 To AP%(0)
    If DY(Z) <= DY(Z1) Then 8510
        GoSub 8525

8510 Next Z1: Next Z
Return

8525 For I = 0 To 18: Swap NG%(Z, I), NG%(Z1, I): Next
For I = 0 To 1: Swap YN$(Z, I), YN$(Z1, I): Next
Swap DY(Z), DY(Z1)
Swap VS%(Z), VS%(Z1)
Swap HS%(Z), HS%(Z1)
Return

9000 Open P$(0) + "BASK." + YN$ For Random As #1 Len = 655
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 2 AS Q$(16), 2 AS Q$(17), 2 AS Q$(18), 2 _
AS Q$(19)

Get #1, T1
N$ = Q$(0)
For I3 = 0 To 9: T%(I3) = CVI(Q$(I3 + 10)): Next
Close 1

9005 If Right$(N$, 1) = " " Then N$ = Left$(N$, Len(N$) - 1): GoTo 9005
Return

'EDIT THE PARAMETERS TO SAVE A SPECIAL EVENTS BOXSCORE

5070 If Not _FileExists("EVENTS.BOX") Then 5075
Open "EVENTS.BOX" For Input As #1
For X = 1 To 13
    Input #1, E%(X)
Next
Close 1

5075 Cls: Color 14
Print "PARAMETERS FOR SAVING SPECIAL EVENTS BOXSCORES": X% = 2: Color 15
Print " 1. OVERTIME PERIODS ARE OVER "; E%(1)
Print " 2. TOTAL POINTS GREATER THAN "; E%(2)
Print " 3. TOTAL REBOUNDS GREATER THAN "; E%(3)
Print " 4. TEAM POINTS GREATER THAN "; E%(4)
Print " 5. TEAM REBOUNDS GREATER THAN "; E%(5)
Print " 6. PLAYER'S POINTS MORE THAN "; E%(6)
Print " 7. PLAYER'S REBOUNDS MORE THAN "; E%(7)
Print " 8. PLAYER'S ASSISTS MORE THAN "; E%(8)
Print " 9. PLAYER'S BLOCKS MORE THAN "; E%(9)
Print "10. PLAYER'S STEALS MORE THAN "; E%(10)
Print "11. PLAYER'S 3-FGM MORE THAN "; E%(11)
GoTo 197

147 Locate 2: Input " 1. OVERTIME PERIODS ARE OVER "; E%(1): If C1 = 1 Then 197

3162 Locate 3: Input " 2. TOTAL POINTS GREATER THAN "; E%(2): If C1 = 1 Then 197

3164 Locate 4: Input " 3. TOTAL REBOUNDS GREATER THAN "; E%(3): If C1 = 1 Then 197

3165 Locate 5: Input " 4. TEAM POINTS GREATER THAN "; E%(4): If C1 = 1 Then 197

3163 Locate 6: Input " 5. TEAM REBOUNDS GREATER THAN "; E%(5): If C1 = 1 Then 197

148 Locate 7: Input " 6. PLAYER'S POINTS MORE THAN "; E%(6): If C1 = 1 Then 197

178 Locate 8: Input " 7. PLAYER'S REBOUNDS MORE THAN "; E%(7): If C1 = 1 Then 197

3152 Locate 9: Input " 8. PLAYER'S ASSISTS MORE THAN "; E%(8): If C1 = 1 Then 197

111 Locate 10: Input " 9. PLAYER'S BLOCKS MORE THAN "; E%(9): If C1 = 1 Then 197

112 Locate 11: Input "10. PLAYER'S STEALS MORE THAN "; E%(10): If C1 = 1 Then 197

3153 Locate 12: Input "11. PLAYER'S 3-FGM MORE THAN "; E%(11): If C1 = 1 Then 197

197 C1 = 1
Locate 23, 1: Print "SELECT # TO CHANGE 1-11 (0 TO CONTINUE) ";
Input B9
On B9 GOTO 147, 3162, 3164, 3165, 3163, 148, 178, 3152, 111, 112, 3153
Open "EVENTS.BOX" For Output As #1
For X = 1 To 13
    Print #1, E%(X)
Next
Close 1
GoTo 4500

4011 Locate 10, 8
Call CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
GoTo 10

4010 Locate 10, 8
Call CSMsgBox("NO SPECIAL EVENTS BOXSCORES WERE FOUND PRESS ANY KEY TO RETURN TO MENU", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
GoTo 4500

4020 Locate 10, 8
Call CSMsgBox("NO BOXSCORES WERE FOUND PRESS ANY KEY TO RETURN TO MENU", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
GoTo 4500

4060 Locate 10, 8
Call CSMsgBox("BOXSCORE DOES NOT EXIST PRESS ANY KEY TO RETURN TO MAIN MENU", 23, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
Return

4500 'BOXSCORE MAINTENANCE
Call TITLE
item$(1) = "SET PARAMETERS TO SAVE SPECIAL EVENTS BOXSCORES  "
item$(2) = "ACCESS USER SAVED AND SPECIAL EVENTS BOXSCORES   "
item$(3) = "DELETE ALL SCHEDULE GENERATED BOXSCORES          "
item$(4) = "DELETE ALL USER SAVED, & SPECIAL BOXSCORES       "
item$(5) = "DELETE ALL BOXSCORES (SCHEDULE, USER, &  SPECIAL)"
item$(6) = "RETURN TO MAIN AUTO SCHEDULER MENU               "
SELECTION% = 1
Count% = 6
Start% = 1
SCANCODE% = 6
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 6
Column% = 16
Call box0(5, 15, 12, 65, 2, 31)
    
_MouseShow "DEFAULT"
MMenuVert item$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then SELECTION% = 6
On SELECTION% GOTO 5070, 5060, 5010, 5010, 5010
GoTo 10

5000 ReDim array$(500) 'holds the main editor string array
Locate 23, 1: Print "ENTER GAME # TO GO TO        "
Locate 23, 1: Input "ENTER GAME # TO GO TO "; NG%
If NG% < 1 Or NG% > AP%(0) Then 120
NS% = Int((NG% - 1) / 20)
BN$ = P$(1) + "box" + Mid$(Str$(NG%), 2, Len(Str$(NG%)))
'GOSUB 5050
GoTo 25

5010 If SELECTION% = 3 Then Spec$ = P$(1) + "BOX*."
If SELECTION% = 4 Then Spec$ = P$(1) + "*.SPC"
If SELECTION% = 5 Then Spec$ = P$(1) + "*.SPC"
GoSub 5020
If SELECTION% = 5 Then Spec$ = P$(1) + "BOX*.": GoSub 5020
GoTo 4500

5020 null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 And SELECTION% = 4 Then 4010
If Count% = 0 And SELECTION% = 3 Then 4020
If Count% = 0 And SELECTION% = 5 Then Return
ReDim F$(0 To Count%)
For X = 1 To Count%
    F$(X) = Space$(12)
Next
F$(0) = Spec$
Call ReadFile(F$())
For X = 1 To Count%
    Kill (P$(1) + F$(X))
    Locate 12, 34: Print "DELETING "; F$(X)
Next
Return

5060 Spec$ = P$(1) + "*.SPC"
null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then 4010
ReDim F$(0 To Count%)
For X = 1 To Count%
    F$(X) = Space$(12)
Next
F$(0) = Spec$
Call ReadFile(F$())
'CALL SortStr(VARPTR(F$(1)), Count, 0)

5065 Cls
Color , 1
Cls
Locate , 28
Color 14
Print "SELECT BOXSCORE TO VIEW": Color 15
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
Print "-SELECTS BOXSCORE";

_MouseShow "DEFAULT"
MAMenu F$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then 4500
F$ = F$(SELECTION%)
Locate 2, 33: Print F$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3912 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 5065
BN$ = P$(1) + F$
ReDim array$(500) 'holds the main editor string array
'GOSUB 5050
GoTo 5060

'EDIT A BOXSCORE LOADED FROM THE FILE NAME BN$

5050 If Not _FileExists(BN$) Then GoSub 4060: Return
Open BN$ For Input As #1
CURLINE = 1
While Not EOF(1)
    Line Input #1, array$(CURLINE)
    CURLINE = CURLINE + 1
Wend
Close #1

Cls '  show how QEdit saves the screen
    
Color 15, 0 'print instructions on bottom of screen
Locate 25, 1
Print " ESC - Quit    F1 - Help    F2 - Print Boxscore   F3 - Save Boxscore"; Tab(79);

'----- fill in editing parameters
Select Case Monitor% 'determine the monitor type
    Case 3, 5, 7, 9, 10, 12 'CGA, EGA, VGA monitors
        Ed.AColor = 23 'white on blue
    Case Else 'all other monitors
        Ed.AColor = 7 'white on black
End Select
Ed.Wrap = 0 'word wrap at column 65 (use 0 for to disable word wrap)
Ed.Frame = 0 'frame code (1 = show ruler line & scroll bars, 0 = don't)
Ed.Rows = 24 'number of text lines to display on the screen (3 - 25)
Ed.Wide = 80 'width of the active screen (10 - 80)
Ed.HTab = 4 'set tab stops to 4 spaces
Locate 1, 1 'locate the upper left corner of the edit window frame
Action = 1 'Action 1 to save underlying screen and
'  display text, operate in polled mode

'----- main loop for editing
Do
    '----- call QEdit in polled mode
    'CALL QEdit(array$(), Ky$, Action, Ed)
    '----- F2 function key pressed? - change margin
    If Ky$ = F2$ Then: For I = 1 To CURLINE - 1: LPrint array$(I): Next
    If Ky$ = F3$ Then
        Open BN$ For Output As #1
        For I = 1 To CURLINE - 1
            Print #1, array$(I)
        Next
        Close 1

        Call CSMsgBox("REVISED BOXSCORE HAS BEEN SAVED...PRESS ANY KEY TO CONTINUE", 31, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    End If

Loop Until Ky$ = Chr$(27) 'keep calling QEdit until Esc is pressed

'----- call QEdit again with an Action flag of 5 to restore the screen
'CALL QEdit(array$(), Ky$, 5, Ed)

Return

2900 Run "HELLO.EXE 1"

4400 Locate 10, 8
Call CSMsgBox("NO SCHEDULE FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
GoTo 2900

4700 Locate 10, 8
GoSub 4600
Locate , 12: Print "NO LEAGUE FILES WERE FOUND"
Locate , 12: Print "PRESS ANY KEY TO RETURN TO MAIN MENU"
I$ = GetKeyPress$
GoTo 2900

4600 JJ = 4
Locate 6, 8: Color 15, 1
Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
For II = 1 To JJ: Locate 6 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 7 + JJ, 8
Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";: Locate 7
Return

1300 Cls: Color , 1: Locate 1, 1: Print Tab(80);: Locate , 33: Color 14
Print "AUTO SCHEDULER"
Locate 25, 1: Print Tab(80);: Locate , 1: Print "(C) SHANNON LYNN 1990 V3.0";
Locate 5, 1: Color , 0
Color 15: Print "(1) ";: Color 14: Print "6 TEAM, 30 GAME SCHEDULE"
Color 15: Print "(2) ";: Color 14: Print "7 TEAM, 42 GAME SCHEDULE"
Color 15: Print "(3) ";: Color 14: Print "8 TEAM, 56 GAME SCHEDULE"
Color 15: Print "(4) ";: Color 14: Print "9 TEAM, 72 GAME SCHEDULE"
Color 15: Print "(5) ";: Color 14: Print "10 TEAM, 90 GAME SCHEDULE"
Color 15: Print "(6) ";: Color 14: Print "11 TEAM, 88 GAME SCHEDULE"
Color 15: Print "(7) ";: Color 14: Print "12 TEAM  96 GAME SCHEDULE"
Color 15: Print "(8) ";: Color 14: Print "13 TEAM 117 GAME SCHEDULE"
Color 15: Print "(9) ";: Color 14: Print "16 TEAM 128 GAME SCHEDULE"
Color 15: Print "(0) ";: Color 14: Print "RETURN TO COURTSIDE MAIN MENU"
1305 I$ = GetKeyPress$
D3% = Val(I$)
If I$ = Chr$(27) Then D3% = 0
If D3% < 0 Or D3% > 9 Then 1305
If D3% = 0 Then 10
Spec$ = P$(1) + "*.LIB"
null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then 4011
ReDim F$(0 To Count%)
For X = 1 To Count%
    F$(X) = Space$(12)
Next
F$(0) = Spec$
Call ReadFile(F$())
'CALL SortStr(VARPTR(F$(1)), Count, 0)

1311 Cls
Color , 1
Cls
Locate , 28
Color 14
Print "SELECT CONFERENCE TO SCHEDULE": Color 15
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
Print "-SELECTS CONFERENCE";

_MouseShow "DEFAULT"
1320 MAMenu F$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then 1300
If EXITCODE% = -71 Then Start% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 1320
If EXITCODE% = -79 Then Start% = Count%: EXITCODE% = 3: SELECTION% = Count%: GoTo 1320
If EXITCODE% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If EXITCODE% = -81 Then Start% = Start% + 80: If Start% > Count% Then Start% = Count%
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = Start%: GoTo 1320
F$ = F$(SELECTION%)
Locate 2, 33: Print F$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3911 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1311
Open P$(1) + F$ For Input As #2
X1 = 0
Input #2, LG%
Input #2, LG$
Input #2, D1$: Input #2, D2$
For X = 1 To 30
    Input #2, D1$: Input #2, LG%: Input #2, D2$
    If D1$ = "XXX" Or D1$ = "" Then 3913
    X1 = X1 + 1
    Z1$(X1) = D1$: Z1%(X1) = LG%: Z2$(X1) = D2$

3913 Next
Close 2
If D3% = 1 Then F$ = "6TM.SCX"
If D3% = 2 Then F$ = "7TM.SCX"
If D3% = 3 Then F$ = "8TM.SCX"
If D3% = 4 Then F$ = "9TM.SCX"
If D3% = 5 Then F$ = "10TM.SCX"
If D3% = 6 Then F$ = "11TM.SCX"
If D3% = 7 Then F$ = "12TM.SCX"
If D3% = 8 Then F$ = "13TM.SCX"
If D3% = 9 Then F$ = "16TM.SCX"
Open F$ For Random As #1 Len = 48
Y = LOF(1)
AP%(0) = Y / 48
BS% = Int((AP%(0) - 1) / 20)
For X = 0 To 18
    Field #1, X * 2 As X$, 2 As Q$(X + 1), 48 - 2 - 2 * X As X$
Next
Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)
For I = 1 To AP%(0)
    Get #1, I
    For X = 0 To 18: NG%(I, X) = CVI(Q$(X + 1)): Next
    For X = 0 To 1: YN$(I, X) = Q$(20 + X): Next
    VS%(I) = CVI(Q$(22)): HS%(I) = CVI(Q$(23))
Next
Close 1
GoSub 900
GoSub 4100
For I = 1 To AP%(0)
    Print "GAME"; I
    Print NG%(I, 0), YN$(I, 0)
    Print NG%(I, 1), YN$(I, 1)
    X = NG%(I, 0): NG%(I, 0) = Z1%(X): YN$(I, 0) = Z2$(X): X = NG%(I, 1): NG%(I, 1) = Z1%(X): YN$(I, 1) = Z2$(X)
    '    IF D3% < 7 THEN X = NG%(I, 0): NG%(I, 0) = Z1%(X): YN$(I, 0) = Z2$(X): X = NG%(I, 1): NG%(I, 1) = Z1%(X): YN$(I, 1) = Z2$(X)
    '    IF D3% = 7 AND NG%(I, 0) <= 6 THEN X = NG%(I, 0): PRINT X: NG%(I, 0) = Z1%(X): YN$(I, 0) = Z2$(X): GOTO 3915
    '    IF D3% = 7 AND NG%(I, 0) > 6 THEN X = NG%(I, 0): PRINT X: NG%(I, 0) = Z1%(X + 9): YN$(I, 0) = Z2$(X + 9)
    '3915 IF D3% = 7 AND NG%(I, 1) <= 6 THEN X = NG%(I, 1): PRINT X: NG%(I, 1) = Z1%(X): YN$(I, 1) = Z2$(X): GOTO 3916
    '     IF D3% = 7 AND NG%(I, 1) > 6 THEN X = NG%(I, 1): PRINT X: NG%(I, 1) = Z1%(X + 9): YN$(I, 1) = Z2$(X + 9)

    3916 Print "GAME"; I
    Print NG%(I, 0), YN$(I, 0)
    Print NG%(I, 1), YN$(I, 1)
Next
Open P$(0) + F$ + ".SCD" For Random As #1 Len = 48
Y = LOF(1) / 48
For X = 0 To 18
    Field #1, X * 2 As X$, 2 As Q$(X + 1), 48 - 2 - 2 * X As X$
Next
Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)
For I = 1 To AP%(0)
    For X = 0 To 18: LSet Q$(X + 1) = MKI$(NG%(I, X)): Next
    For X = 0 To 1: LSet Q$(20 + X) = YN$(I, X): Next
    LSet Q$(22) = MKI$(VS%(I)): LSet Q$(23) = MKI$(HS%(I))
    Put #1, I + Y
Next
Close 1
D3% = 1: GoTo 601

4100 Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 1
Print " Ö"; String$(61, 196); "· ";: For II = 1 To JJ: Locate 6 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 7 + JJ, 8
Print " Ó"; String$(61, 196); "½ ";: Locate 7, 32
Locate , 12: Print "ENTER NAME FOR SCHEDULE: ": Print
Locate , 12: Print "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
Locate , 12: Print "SPACES. DO NOT ADD A FILE EXTENSION, THE SCHEDULER"
Locate , 12: Print "WILL AUTOMATICALLY ADD .SCD"
Locate , 12: Print "SAMPLE NAMES ARE MONTREAL, STL68, NY76, CIN90, ETC"
Locate 7, 37: Input F$
If F$ = "" Then 21
Locate 13, 12: Print "ANY CHANGE (YN) "
23 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 4100
Color 15, 0
Return


Sub TITLE ()
    Color , 0: KEY Off: Cls
    Color , 1: Locate 1, 1: Print Tab(80);: Locate , 33: Color 14
    Print "AUTO SCHEDULER"
    Locate 24, 1: Print Tab(80);: Locate , 1
    Print "(C) LANCE HAFFNER/SHANNON LYNN 1987-1997";
    Locate 5, 1: Color , 0
End Sub


'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
