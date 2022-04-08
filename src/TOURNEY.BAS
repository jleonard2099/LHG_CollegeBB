'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Courtside College Basketball - Tournament"
''$ExeIcon:'./lhg_ccb.ico'
'_Icon
_AllowFullScreen

'Dim teamItems$(500), A$(11), A(11, 6), T(34)
Dim Q$(375), YN$(500, 3), AP%(1), NG%(500, 17), NGG(500)
Dim SC$(2), D$(1), TS$(1), U5$(3), LC$(2), CM$(1), TE$(1), TF$(1)
ReDim MON%(500), DY%(500), yr%(500), H$(500), V$(500), HS%(500), VS%(500)

Open "DEV.BAS" For Input As #1: Input #1, D$(0): Close 1

U5$(0) = "HUMAN OPPONENT            "
U5$(1) = "COMPUTER IS VISITING TEAM "
U5$(2) = "COMPUTER IS HOME TEAM     "
U5$(3) = "COMPUTER VS COMPUTER      "
LC$(1) = "HOME COURT ADVANTAGE"
LC$(2) = "NEUTRAL SITE        "
CM$(0) = "COACH AND SELECT SHOTS           "
CM$(1) = "COACH ONLY AND LET COMPUTER SHOOT"
TE$(0) = "SHOOT 1+1"
TE$(1) = "SHOOT 2  "
TF$(0) = "N": TF$(1) = "Y"

10 Spec$ = D$(0) + "*.TOU"
null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%

If Count% > 0 Then
	ReDim F$(0 To Count%)

	For X = 1 To Count%
		F$(X) = Space$(12)
	Next

	F$(0) = Spec$
	Call ReadFile(F$())
	'CALL SortStr(VARPTR(F$(1)), Count%, 0)
	nf% = Count%
Else
	'This was added to handle if no TOU files are available
	'Original game would just try to push ahead anyway
	Locate 10
    Call CSMsgBox("NO TOUNAMENT FILES WERE FOUND PRESS ANY KEY TO RETURN TO MENU", 28, Pass)
    I$ = GetKeyPress$
    Call CSMsgBox("", 0, Pass)
	RUN "ADDTOURN.EXE"
End If

Color 7, 0
Cls
NG%(0, 8) = 0
NG%(0, 3) = 1
NG%(0, 7) = 0
NG%(0, 2) = 1
NG%(0, 5) = 5
YN$(0, 0) = ""
YN$(0, 1) = ""

For I = 1 To 500
    V$(I) = "": NG%(I, 13) = 0
    H$(I) = ""
Next

YN$(0, 3) = "10-01-91"
YN$(0, 2) = "07:30P"
TS$(0) = "N": TS$(1) = "Y"
SC$(0) = "NO SHOT CLOCK"
SC$(1) = "45 SEC CLOCK "
SC$(2) = "35 SEC CLOCK "

600 ' *** LOAD IN A SAVED SCHEDULE ***
GoTo 2100

605 Open D$(0) + F$ + ".TOU" For Random As #1 Len = 90
Y = LOF(1)
Close 1
AP%(0) = Y / 90
BS% = Int((AP%(0) - 1) / 20)
For I = 1 To AP%(0)
    Open D$(0) + F$ + ".TOU" For Random As #1 Len = 90
    For X = 0 To 14
        Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
    Next
    Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
    Field #1, 64 As X$, 3 As Q$(20), 3 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
    Field #1, 84 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)
    Get #1, I
    For X = 0 To 14: NG%(I, X) = CVI(Q$(X + 1)): Next
    For X = 0 To 3: YN$(I, X) = Q$(20 + X): Next
    VS%(I) = CVI(Q$(16)): HS%(I) = CVI(Q$(17))
    V$(I) = Q$(18): H$(I) = Q$(19)
    For X = 1 To 3: NG%(I, X + 14) = CVI(Q$(X + 23)): Next
    For X = 2 To 3

        610 If Right$(YN$(I, X), 1) = " " Then YN$(I, X) = Left$(YN$(I, X), Len(YN$(I, X)) - 1): GoTo 610
    Next
Close 1: Next

25 Cls
Locate 23, 1: Color , 1: Print Tab(80);: Locate 23, 1
Color 13: Print "SC";: Color 14: Print "-SHOT CLOCK  ";
Color 13: Print "HC";: Color 14: Print "-HOME COURT ADVANTAGE  ";
Color 13: Print "3";: Color 14: Print "-USE 3-POINT SHOT  ";
Locate 24, 1: Color , 1: Print Tab(80);: Locate 24, 1
Color 11: Print "PGUP  PGDN  ";
Color 13: Print "REG";: Color 14: Print "-REGION  ";
Color 13: Print "RND";: Color 14: Print "-ROUND  ";
Color 13: Print "GM";: Color 14: Print "-GAME  ";
Locate 25, 1: Color , 1: Print Tab(80);: Locate 25, 1
Color 11: Print "P";: Color 14: Print "-PLAY TOURNAMENT   ";
Color 11: Print "Q";: Color 14: Print "-QUIT  ";
Color 11: Print "E";: Color 14: Print "-EDIT A GAME  ";
Locate 1, 1: Print Tab(80);: Locate 1, 1: Print F$
Locate 1, 33: Color 14: Print "AUTO SCHEDULER"
Color , 0

50 Locate 2, 1
Color 14
Print "GM ID#  REG  RND  GM    VISITING TEAM        HOME TEAM            HC  3  SC": Color 15
For X = 1 To 20
    Locate X + 2, 1
    If X + NS% * 20 > AP%(0) Then 100
    Print Using "###      ##   ##   ##"; X + NS% * 20; NG%(X + NS% * 20, 15); NG%(X + NS% * 20, 16); NG%(X + NS% * 20, 17);
    'LOCATE , 17: PRINT YN$(X + NS% * 20, 2);
    Locate , 25: Print V$(X + NS% * 20); " ";
    If NG%(X + NS% * 20, 13) = 1 Then Print Using "###"; VS%(X + NS% * 20);
    Locate , 46: Print H$(X + NS% * 20); " ";
    If NG%(X + NS% * 20, 13) = 1 Then Print Using "###"; HS%(X + NS% * 20);
    Locate , 67: If NG%(X + NS% * 20, 2) = 1 Then Print "Y";: Else Print "N";
    Locate , 71: Print TS$(NG%(X + NS% * 20, 3));
    Locate , 74: Print Left$(SC$(NG%(X + NS% * 20, 7)), 2)
Next
100 I$ = GetKeyPress$
If I$ = Chr$(0) + "Q" Then GoSub 130: GoTo 25
If I$ = Chr$(0) + "I" Then GoSub 135: GoTo 25
If UCase$(I$) = "E" Then GoSub 140: GoTo 25
If UCase$(I$) = "Q" Then Run "ADDTOURN.EXE"
If UCase$(I$) = "P" Then GoSub 350: GoTo 300
GoTo 100

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
V$ = V$(NG%)
H$ = H$(NG%)
GoTo 149

149 Cls: Color 2: Print Tab(29); "SELECT GAME OPTIONS": Print

151 Locate 2, 2: Color 14: Print "    ";: Color 15: Print "VISITING TEAM: "; V$

152 Locate 3, 2: Color 14: Print "    ";: Color 15: Print "HOME TEAM: "; H$

153 Locate 4, 2: Color 14: Print "(1) ";: Color 15: Print U5$(NG%(NG%, 8))

154 Locate 5, 2: Color 14: Print "(2) ";: Color 15: Print "3-POINT SHOT: "; TS$(NG%(NG%, 3))

155 Locate 6, 2: Color 14: Print "(3) ";: Color 15: Print "SHOT CLOCK "; SC$(NG%(NG%, 7))

156 Locate 7, 2: Color 14: Print "(4) ";: Color 15: Print "HOME COURT ADVANTAGE: "; LC$(NG%(NG%, 2))

157 Locate 8, 2: Color 14: Print "(5) ";: Color 15: Print Using "DISPLAY PAUSE IN SECONDS: ##.#  "; NG%(NG%, 4) / 10

158 Locate 9, 2: Color 14: Print "(6) ";: Color 15: Print "FOULS TO DISQUALIFY: "; NG%(NG%, 5)

159 Locate 10, 2: Color 14: Print "(7) ";: Color 15: Print "HUMAN COACHES WILL: "; CM$(NG%(NG%, 9))

162 Locate 11, 2: Color 14: Print "(8) ";: Color 15: Print "FREE THROWS ON 10TH FOUL "; TE$(NG%(NG%, 6))

164 Locate 12, 2: Color 14: Print "(9) ";: Color 15: Print "3-FT WHEN FOULED ON 3-PT ATTEMPT "; TF$(NG%(NG%, 10))

165 Locate 13, 2: Color 14: Print "(A) ";: Color 15: Print "1 FT ON 1ST 6 FLOOR FOULS (PRIOR TO 1973): "; TF$(NG%(NG%, 14))

163 Locate 14, 2: Color 14: Print "(0) ";: Color 15: Print "ACCEPT SETTINGS"
175 I$ = GetKeyPress$
If UCase$(I$) = "A" Then I$ = "10"
I8% = Val(I$): If I8% < 0 Or I8% > 10 Then 175
On I8% GOTO 180, 184, 185, 186, 187, 188, 189, 192, 193, 194
Return

193 NG%(NG%, 10) = 1 - NG%(NG%, 10): GoTo 149

194 NG%(NG%, 14) = 1 - NG%(NG%, 14): GoTo 149

180 NG%(NG%, 8) = NG%(NG%, 8) + 1: If NG%(NG%, 8) > 3 Then NG%(NG%, 8) = 0
GoTo 151

184 NG%(NG%, 3) = 1 - NG%(NG%, 3): GoTo 154

185 NG%(NG%, 7) = 1 + NG%(NG%, 7)
If NG%(NG%, 7) > 2 Then NG%(NG%, 7) = 0
GoTo 155

186 N = 1 - (NG%(NG%, 2) - 1): NG%(NG%, 2) = N + 1: GoTo 156

187 Locate 8, 32: Input U6: NG%(NG%, 4) = U6 * 10: GoTo 157

188 NG%(NG%, 5) = 1 + NG%(NG%, 5): If NG%(NG%, 5) > 6 Then NG%(NG%, 5) = 5
GoTo 158

189 NG%(NG%, 9) = 1 - NG%(NG%, 9): GoTo 159

192 NG%(NG%, 6) = 1 - NG%(NG%, 6): GoTo 162

300 ' *** PLAY GAMES ***
Color , 0: Cls
JJ = 7
Locate 6, 8: Color 15, 4
Print " Ö"; String$(61, 196); "· ";: For II = 1 To JJ: Locate 6 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 7 + JJ, 8
Print " Ó"; String$(61, 196); "½ ";: Locate 7, 32
Locate , 12: Print "TOTAL GAMES ="; AP%(0)
Locate , 12: Print "SELECT GAME # TO START AND END WITH": Print
Locate , 12: Input "ENTER START GAME #: "; SG%: Print
Locate , 12: Input "ENTER END GAME #:"; EG%
Locate , 12: Print "ANY CHANGE (YN)"
301 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 300
If SG% <= 0 Then SG% = 1
If EG% > AP%(0) Then EG% = AP%(0)
Open "TOURNEY" For Output As #2
Print #2, F$: ' + ".TNY"
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
'    IF _FILEEXISTS(D$(0) + F$ + ".TNY") THEN
'       KILL D$(0) + F$ + ".TNY"
'    END IF
Open D$(0) + F$ + ".TOU" For Random As #1 Len = 90
For X = 0 To 14
    Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
Next
Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
Field #1, 64 As X$, 3 As Q$(20), 3 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
Field #1, 84 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)
For I = 1 To AP%(0)
    For X = 0 To 14: LSet Q$(X + 1) = MKI$(NG%(I, X)): Next
    For X = 0 To 3: LSet Q$(20 + X) = YN$(I, X): Next
    LSet Q$(16) = MKI$(VS%(I)): LSet Q$(17) = MKI$(HS%(I))
    LSet Q$(18) = V$(I): LSet Q$(19) = H$(I)
    For X = 1 To 3: LSet Q$(X + 23) = MKI$(NG%(I, X + 14)): Next
    Put #1, I
Next
Close 1
Return

1200 For I = 1 To AP%(0)
    L = Len(YN$(I, 2))
    If L = 5 Then HR = Val(Left$(YN$(I, 2), 1)): MIN = Val(Mid$(YN$(I, 2), 3, 2))
    If L = 6 Then HR = Val(Left$(YN$(I, 2), 2)): MIN = Val(Mid$(YN$(I, 2), 4, 2))
    MON%(I) = Val(Left$(YN$(I, 3), 2))
    DY%(I) = Val(Mid$(YN$(I, 3), 4, 2))
    yr%(I) = Val(Mid$(YN$(I, 3), 7, 2))
    T$ = Right$(YN$(I, 2), 1)
    If UCase$(T$) = "P" And Left$(YN$(I, 2), 2) <> "12" Then HR = HR + 12
    TM = HR * 60 + MIN
    NGG(I) = Int(TM / 30 - 18): NG%(I, 12) = NGG(I)
Next
Return

2100 Cls
Color , 1
Cls
Locate , 28
Color 14
Print "SELECT FIELD TO PLAY": Color 15
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
Print "-SELECTS TOURNEY";

_MouseShow "DEFAULT"
320 MAMenu F$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then Run "ADDTOURN.EXE"
If EXITCODE% = -71 Then Start% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 320
If EXITCODE% = -79 Then Start% = Count%: EXITCODE% = 3: SELECTION% = Count%: GoTo 320
If EXITCODE% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If EXITCODE% = -81 Then Start% = Start% + 80: If Start% > Count% Then Start% = Count%
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = Start%: GoTo 320
F$ = F$(SELECTION%)
T1 = SELECTION%
Locate 2, 33: Print F$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3910 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 2100
F$ = RTrim$(F$)
WE$ = Left$(F$, Len(F$) - 4)
Color 15, 0
F$ = WE$
GoTo 605
    
'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
