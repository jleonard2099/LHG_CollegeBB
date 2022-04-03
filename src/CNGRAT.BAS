'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Courtside College Basketball - Change Team"
''$ExeIcon:'./lhg_ccb.ico'
'_Icon
_AllowFullScreen

DECLARE SUB TITLE ()

10 ' **************** REVISED NOVEMBER 1, 1988 ****************************

20 Dim A$(13), A(13, 6), Q$(250), X(13, 6), DF(13), P$(13), RS(13, 13), T1%(20)

30 Dim V$(500), L%(4), T%(9), D$(1), menuItems$(5)
'Dim W$(11), N$(13)

40 Open "DEV.BAS" For Input As #2: Input #2, D$(0): Close 2

100 Call TITLE
menuItems$(1) = "CHANGE OR EDIT AN EXISTING TEAM"
menuItems$(2) = "PRINT A TEAM ROSTER            "
menuItems$(3) = "VIEW A TEAM ROSTER             "
menuItems$(4) = "EXIT TO MAIN MENU              "
SELECTION% = 1
Count% = 4
Start% = 1
SCANCODE% = 4
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 25

Call box0(7, 24, 12, 56, 2, 31)
    
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
D1 = SELECTION%
If SCANCODE% = 27 Then D1 = 4

901 If RN = 4 And D% >= 67 Then Run "HELLO.EXE 2"
If RN = 4 Then Cls: Run "HELLO.EXE 2"
If D1 < 1 Or D1 > 4 Then 100
On D1 GOTO 56, 56, 56, 880

56 Color 15, 0: Cls
JJ = 6
SN = 1: Locate 6, 8: Color 15, 4
Print " Ö"; String$(61, 196); "· ";: For II = 1 To JJ: Locate 6 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 7 + JJ, 8
Print " Ó"; String$(61, 196); "½ ";: Locate 7, 32
Locate , 12: Print "ENTER DISK ID: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Locate 7, 27: Input NY$
Color 15, 0
GoSub 4105
If NY$ = "" Then GoSub 1102: GoTo 60
YN$ = NY$
For I = 1 To 500: V$(I) = "": Next

57 GoSub 1100

60 If BO% = 1 Then BO% = 0: GoTo 56

58 Locate 2, 33: Color T%(0), T%(1): Print A$
Color 15, 4: Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$

59 If UCase$(I$) = "Y" Then 56
Color 15, 0
If D1 = 2 Then GoSub 620: GoTo 100
If D1 = 3 Then GoSub 720: GoTo 100
GoTo 1600

145 Cls: Color 14: Print "CHANGE LEAGUE INFORMATION": X% = 2: Color 15

147 Locate 2: Input " 1. TEAM NUMBER "; T1: If C1 = 1 Then 190

162 Locate 3: Input " 2. DISK ID "; YN$: If C1 = 1 Then 190

163 Locate 4: Input " 3. TEAM NAME "; A$: If C1 = 1 Then 190

148 Locate 5: Input " 4. TEAM STAMINA RATING "; SP: If C1 = 1 Then 190

149 Locate 6: Input " 5. DEF FG% ADJ "; DA: If C1 = 1 Then 190

150 Locate 7: Input " 6. DEF 3FGA/FGA ADJ "; V2%: If C1 = 1 Then 190

151 Locate 8: Input " 7. DEF 3FG% ADJ "; V1%: If C1 = 1 Then 190

152 Locate 9: Input " 8. OFF STEAL RATING "; L%(0): If C1 = 1 Then 190

153 Locate 10: Input " 9. OFF TURNOVER RATING "; L%(1): If C1 = 1 Then 190

154 Locate 11: Input "10. DEF TURNOVER ADJ "; L%(2): If C1 = 1 Then 190

155 Locate 12: Input "11. DEF FOUL ADJ "; L%(3): If C1 = 1 Then 190

156 Locate 13: Input "12. ENTER 99 IF USING 6-9 "; L%(4): If C1 = 1 Then 190

157 Locate 14: Input "13. TEXT COLOR (0-15) "; T%(0): If C1 = 1 Then 190

158 Locate 15: Input "14. BACKGROUND (0-7) "; T%(1): If C1 = 1 Then 190

159 Locate 16: Input "15. TEAM FGA/G "; T%(3): If C1 = 1 Then 190

160 Locate 17: Input "16. LEAGUE FGA/G "; T%(4): If C1 = 1 Then 190

161 Locate 18: Input "17. TEAM GAMES PLAYED "; T%(5): If C1 = 1 Then 190

164 Locate 19: Input "18. HOMECOURT FACTOR "; T%(6): If C1 = 1 Then 190

165 Locate 20: Input "19. ARENA NAME "; S$: If C1 = 1 Then 190

166 Locate 4, 40: Input "20. NICKNAME "; A1$: If C1 = 1 Then 190

167 Locate 5, 40: Input "21. COACH "; C$: If C1 = 1 Then 190

168 Locate 7, 40: Input "22. AVG ATTENDANCE "; ATT: If C1 = 1 Then 190

169 Locate 8, 40: Input "23. WINS "; T%(7): If C1 = 1 Then 190

170 Locate 9, 40: Input "24. LOSSES "; T%(8): If C1 = 1 Then 190

171 Locate 10, 40: Input "25. POWER RATING "; T%(9): If C1 = 1 Then 190

190 Locate 21, 1: Color T%(0), T%(1): Print "TEAM APPEARANCE": Color 15, 0
C1 = 1
Locate 23, 1: Print "SELECT # TO CHANGE 1-24 (0 TO CONTINUE) ";
Input B9: If B9 < 1 Or B9 > 24 Then 200
On B9 GOTO 147, 162, 163, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 164, 165, 166, 167, 168, 169, 170

200 If T%(6) < 0 Then T%(6) = 0
If T%(6) > 6 Then T%(6) = 6
C1 = 0: GoTo 1835

210 Locate 1: Input " 1. NAME "; A$(I): If C1 = 1 Then 250

224 Locate 2: Input " 2. POSITION "; P$(I): If C1 = 1 Then 250

211 Locate 3: Input " 3. GAMES PLAYED "; X(I, 0): If C1 = 1 Then 250

212 Locate 4: Input " 4. ADJ FG% "; A(I, 0): If C1 = 1 Then 250

213 Locate 5: Input " 5. FT% "; A(I, 1): If C1 = 1 Then 250

214 Locate 6: Input " 6. OFF RB/40 MIN X 10 "; X(I, 4): If C1 = 1 Then 250

215 Locate 7: Input " 7. DEF RB/40 MIN X 10 "; A(I, 2): If C1 = 1 Then 250

216 Locate 8: Input " 8. ASSIST RATING "; A(I, 4): If C1 = 1 Then 250

217 Locate 9: Input " 9. STEAL RATING "; A(I, 5): If C1 = 1 Then 250

218 Locate 10: Input "10. BLOCK RATING "; A(I, 6): If C1 = 1 Then 250

219 Locate 11: Input "11. FGA "; X(I, 1): If C1 = 1 Then 250

220 Locate 12: Input "12. FOUL COMMIT RATING "; X(I, 5): If C1 = 1 Then 250

221 Locate 13: Input "13. FOUL DRAWING RATING "; X(I, 6): If C1 = 1 Then 250

222 Locate 14: Input "14. 3FG% "; X(I, 2): If C1 = 1 Then 250

223 Locate 15: Input "15. 3FGA/TOT FGA "; X(I, 3): If C1 = 1 Then 250

225 Locate 16: Input "16. DEFENSIVE RATING (1-9) "; DF(I): If C1 = 1 Then 250

226 Locate 1, 40: Input "17. GAMES "; RS(I, 0): If C1 = 1 Then 250

227 Locate 2, 40: Input "18. MINUTES "; RS(I, 1): If C1 = 1 Then 250

228 Locate 3, 40: Input "19. FGM "; RS(I, 3): If C1 = 1 Then 250

229 Locate 4, 40: Input "20. FGA "; RS(I, 2): If C1 = 1 Then 250

230 Locate 5, 40: Input "21. FTM "; RS(I, 5): If C1 = 1 Then 250

231 Locate 6, 40: Input "22. FTA "; RS(I, 4): If C1 = 1 Then 250

232 Locate 7, 40: Input "23. OFF REB "; RS(I, 6): If C1 = 1 Then 250

233 Locate 8, 40: Input "24. TOTAL REB "; RS(I, 7): If C1 = 1 Then 250

234 Locate 9, 40: Input "25. PER FOULS "; RS(I, 8): If C1 = 1 Then 250

235 Locate 10, 40: Input "26. ASSISTS "; RS(I, 9): If C1 = 1 Then 250

236 Locate 11, 40: Input "27. BLOCKS "; RS(I, 10): If C1 = 1 Then 250

237 Locate 12, 40: Input "28. STEALS "; RS(I, 11): If C1 = 1 Then 250

238 Locate 13, 40: Input "29. 3 FGM "; RS(I, 13): If C1 = 1 Then 250

239 Locate 14, 40: Input "30. 3 FGA "; RS(I, 12): If C1 = 1 Then 250

250 C1 = 1: Locate 22, 1: Print "SELECT # TO CHANGE 1-30 (0 TO CONTINUE) ";
Input B9: If B9 < 1 Or B9 > 30 Then 1835
On B9 GOTO 210, 224, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239

300 Color 15, 0: Cls
JJ = 6
SN = 1: Locate 6, 8: Color 15, 4
Print " Ö"; String$(61, 196); "· ";: For II = 1 To JJ: Locate 6 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 7 + JJ, 8
Print " Ó"; String$(61, 196); "½ ";: Locate 7, 32
Locate , 12: Print "ENTER NEW DISK ID: ": Print
Locate 7, 31: Input NY$
Locate , 12: Print "IS THIS CORRECT (YN)"
305 I$ = GetKeyPress$
If UCase$(I$) <> "N" Then NID% = 1: GoSub 9800: GoTo 100
GoTo 300

620 Cls: LPrint "TEAM ROSTER FOR "; A$

630 LPrint: LPrint "AVG SHOTS PER GAME = "; T%(3); "      DEF FG ADJ = "; DA

640 LPrint "OPP PCT OF 3FG ATT ADJ = "; V2%; "  OPP 3FG PCT ADJ = "; V1%: LPrint
LPrint "DEF STEAL ADJ "; L%(0)
LPrint "OFF TURNOVER ADJ "; L%(1)
LPrint "DEF TURNOVER ADJ "; L%(2)
LPrint "OFF FOUL ADJ "; L%(3)
LPrint

650 LPrint "NAME           FG  FT ORB DRB  CX PS  D  B FC FD  3FG 3FG%": LPrint

660 For I = 0 To 13
    If A$(I) = "XXX" Then 670
    LPrint A$(I); Tab(15);
    LPrint Using "### ### ### ### ### "; A(I, 0); A(I, 1); X(I, 4); A(I, 2); A(I, 3);
    LPrint Using "## ## ## ## ## ### ###"; A(I, 4); A(I, 5); A(I, 6); X(I, 5); X(I, 6); X(I, 3); X(I, 2)

670 Next I

680 LPrint: LPrint "FG- FIELD GOAL %                 B- BLOCK SHOT RATING"

690 LPrint "FT- FREE THROW %                FC- FOUL COMMIT RATING"

700 LPrint "RB- REBOUNDING RATING           FD- FOUL DRAWING RATING"

710 LPrint "CX- CONTRIBUTION FACTOR        3FG- 3FGA/TOTAL FGA"
LPrint "PS- PASSING RATING            3FG%- THREE POINT GOAL %"

730 LPrint " D- DEFENSIVE RATING"
Return

720 Cls: Print "TEAM ROSTER FOR ";: Color T%(0), T%(1): Print A$;: Color 14, 0
Print "    AVG SHOTS PER GAME = ";: Color 15, 0
Print T%(3);: Color 14, 0
Print " DEF FG ADJ = ";: Color 15, 0: Print DA
Color 14, 0: Print "OPP PCT OF 3FG ATT ADJ = ";
Color 15, 0: Print V2%;: Color 14, 0: Print "  OPP 3FG PCT ADJ = ";
Color 15, 0: Print V1%;
Color 14, 0: Print "   DEF STEAL ADJ ";: Color 15, 0: Print L%(0)
Color 14, 0: Print "OFF TURNOVER ADJ ";: Color 15, 0: Print L%(1);
Color 14, 0: Print "    DEF TURNOVER ADJ ";: Color 15, 0: Print L%(2);
Color 14, 0: Print "    OFF FOUL ADJ ";: Color 15, 0: Print L%(3)
Print
Color 2, 0: Print "NAME           FG% FT% ORB DRB  CX PS  D  B  FC FD  3FG 3FG%": Color 15, 0
For I = 0 To 13
    If A$(I) = "XXX" Then 675
    Print A$(I); Tab(16);
    Print Using "### ### ### ### ### "; A(I, 0); A(I, 1); X(I, 4); A(I, 2); A(I, 3);
    Print Using "## ## ## ### ##  ###  ###"; A(I, 4); A(I, 5); A(I, 6); X(I, 5); X(I, 6); X(I, 3); X(I, 2)

675 Next I
Print " B- BLOCK SHOT RATING           FC- FOUL COMMIT RATING"
Print "RB- REBOUNDING RATING           FD- FOUL DRAWING RATING"
Print "CX- CONTRIBUTION FACTOR        3FG- 3FGA/TOTAL FGA"
Print "PS- PASSING RATING               D- DEFENSIVE RATING"
I$ = GetKeyPress$
Return
    

800 Cls: Color T%(0), T%(1): Print A$: Color 14, 0
Print "PLAYER          CONTRIBUTION"
Print "                PERCENT"
Color 15, 0
T3 = 0
For I = 0 To 13
    Print Using "## "; I + 1;: Print A$(I);
    Locate , 17: Print A(I, 3): Locate , 1
    T3 = T3 + A(I, 3)
Next I
Print
Color 14: Print "TOTAL CP ";: Locate , 17: Color 15, 0: Print T3

805 Locate 21: Print "SELECT PLAYER TO CHANGE 1-14 (0-NO MORE CHANGE) ";
Input X9
If X9 < 1 Or X9 > 14 Then 825
X9 = X9 - 1
Locate 22, 18: Print A$(X9): Locate , 1
Locate 23, 1
Input "ENTER CP: "; A(X9, 3)
GoTo 800

825 If T3 > 98 And T3 < 102 Then 840
Cls
GoSub 4500
U$ = "TOTAL CONTRIBUTION MUST BE 100 %"
Locate , (80 - Len(U$)) / 2
Print U$
U$ = "HIT ANY KEY TO CONTINUE"
Locate , (80 - Len(U$)) / 2
Print U$
Color 15, 0
830 I$ = GetKeyPress$
GoTo 800

840 Cls

860 GoSub 9800

870 GoTo 100

880 Cls
GoSub 4500

881 Run "HELLO.EXE 2"
End
    
1100 Open D$(0) + "BASK." + YN$ For Random As #1 Len = 655
Y = LOF(1)
Field #1, 15 As Q$(0), 640 As X$
For I1 = 1 To Y / 655
    Get #1, I1
    V$(I1) = Q$(0)
Next
Close 1
NT = Y / 655
TS% = Int((NT - 1) / 80)

1102 NS% = 0

1101 Color , 4: Cls: Locate , 33: Color 14: Print "TEAM SELECTION": Color 15
For X = 1 To 20
    Locate 3 + X, 1: Print V$(NS% * 80 + X);
    Locate , 21: Print V$(NS% * 80 + 20 + X);
    Locate , 41: Print V$(NS% * 80 + 40 + X);
    Locate , 61: Print V$(NS% * 80 + 60 + X)
Next
Locate 25, 1: Color 14: Print "ESC";: Color 15: Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14: Print "ENTER";: Color 15: Print "-SELECTS TEAM";
Locate 4, 1: Color 0, 7: Print V$(NS% * 80 + 1);: Color 15, 4

1108 X = Int((Pos(X) - 1) / 20) + 1: Y = CsrLin
1109 I$ = GetKeyPress$
If I$ = Chr$(0) + "Q" Then 1110
If I$ = Chr$(0) + "I" Then 1120
If I$ = Chr$(0) + "G" Then NS% = 0: GoTo 1101
If I$ = Chr$(0) + "O" Then NS% = TS%: GoTo 1101
If I$ = Chr$(0) + "H" Then 1130
If I$ = Chr$(0) + "M" Then 1135
If I$ = Chr$(0) + "P" Then 1140
If I$ = Chr$(0) + "K" Then 1145
If I$ = Chr$(27) Then BO% = 1: Return
If I$ = Chr$(13) Then 1190

1110 ' *** PAGE DOWN ***
If NS% = TS% Then 1108
NS% = NS% + 1: GoTo 1101

1120 ' *** PAGE UP ***
If NS% = 0 Then 1108
NS% = NS% - 1: GoTo 1101

1130 ' *** CURSOR UP ***
If Y < 4 Then 1108
GoSub 1150
Y = Y - 1: If Y < 4 Then Y = 4
GoSub 1151
GoTo 1108

1135 ' *** CURSOR RIGHT ***
If X = 4 Then 1108
If NS% * 80 + Y - 3 + (X) * 20 > NT Then 1108
GoSub 1150
X = X + 1: If X >= 4 Then X = 4
GoSub 1151
GoTo 1108

1140 ' *** CURSOR DOWN ***
If Y >= 23 Then 1108
If NS% * 80 + Y + 1 - 3 + (X - 1) * 20 > NT Then 1108
GoSub 1150
Y = Y + 1: If Y >= 23 Then Y = 23
GoSub 1151
GoTo 1108

1145 ' *** CURSOR LEFT ***
If X = 1 Then 1108
GoSub 1150
X = X - 1: If X <= 1 Then X = 1
GoSub 1151
GoTo 1108

1150 Locate Y, (X - 1) * 20 + 1: Color 15, 4
Print V$(Y - 3 + (X - 1) * 20 + NS% * 80);
Return

1151 Color 0, 7: Locate Y, (X - 1) * 20 + 1
Print V$(Y - 3 + (X - 1) * 20 + NS% * 80);
Color 15, 4
Return

1190 T1 = NS% * 80 + Y - 3 + (X - 1) * 20: GoTo 9000

1600 Cls: Color 14
Print "CHANGE LEAGUE INFORMATION": X% = 2: Color 15
Print " 1. TEAM NUMBER "; T1
Print " 2. DISK ID "; YN$
Print " 3. TEAM NAME "; A$; Tab(40); "20. NICKNAME "; A1$
Print " 4. TEAM STAMINA RATING "; SP; Tab(40); "21. COACH "; C$
Print " 5. DEF FG% ADJ "; DA
Print " 6. DEF 3FGA/FGA ADJ "; V2%; Tab(40); "22. AVG ATTENDANCE "; ATT
Print " 7. DEF 3FG% ADJ "; V1%; Tab(40); "23. WINS "; T%(7)
Print " 8. OFF STEAL RATING "; L%(0); Tab(40); "24. LOSSES "; T%(8)
Print " 9. OFF TURNOVER RATING "; L%(1); Tab(40); "25. POWER RATING "; T%(9)
Print "10. DEF TURNOVER ADJ "; L%(2)
Print "11. DEF FOUL ADJ "; L%(3)
Print "12. ENTER 99 IF USING 6-9 "; L%(4)
Print "13. TEXT COLOR (0-15) "; T%(0)
Print "14. BACKGROUND (0-7) "; T%(1)
Print "15. TEAM FGA/G "; T%(3)
Print "16. LEAGUE FGA/G "; T%(4)
Print "17. TEAM GAMES PLAYED "; T%(5)
Print "18. HOMECOURT FACTOR "; T%(6)
Print "19. ARENA NAME "; S$
GoTo 190

1835 Cls: Print "CHANGE PLAYERS": X% = 2
For I = 0 To 6
Locate I + 5, 5: Print Using "##"; I + 1;: Print " "; A$(I); Tab(40);: Print Using "##"; I + 8;: Print " "; A$(I + 7): Next
Print

1855 Print "# TO CHANGE (1-14) (0 TO CONTINUE) ";: Input I

1865 I = I - 1

1875 If I < 0 Or I > 13 Then X% = 0: GoTo 800

1885 Cls: Print " 1. NAME "; A$(I); Tab(40); "17. GAMES "; RS(I, 0)
Print " 2. POSITION "; P$(I); Tab(40); "18. MINUTES "; RS(I, 1)
Print " 3. GAMES PLAYED "; X(I, 0); Tab(40); "19. FGM "; RS(I, 3)
Print " 4. ADJ FG% "; A(I, 0); Tab(40); "20. FGA "; RS(I, 2)
Print " 5. FT% "; A(I, 1); Tab(40); "21. FTM "; RS(I, 5)
Print " 6. OFF RB/40 MIN X 10 "; X(I, 4); Tab(40); "22. FTA "; RS(I, 4)
Print " 7. DEF RB/40 MIN X 10 "; A(I, 2); Tab(40); "23. OFF REB "; RS(I, 6)
Print " 8. ASSIST RATING "; A(I, 4); Tab(40); "24. TOTAL REB "; RS(I, 7)
Print " 9. STEAL RATING "; A(I, 5); Tab(40); "25. PER FOULS "; RS(I, 8)
Print "10. BLOCK RATING "; A(I, 6); Tab(40); "26. ASSISTS "; RS(I, 9)
Print "11. FGA "; X(I, 1); Tab(40); "27. BLOCKS "; RS(I, 10)
Print "12. FOUL COMMIT RATING "; X(I, 5); Tab(40); "28. STEALS "; RS(I, 11)
Print "13. FOUL DRAWING RATING "; X(I, 6); Tab(40); "29. 3 FGM "; RS(I, 13)
Print "14. 3FG% "; X(I, 2); Tab(40); "30. 3 FGA "; RS(I, 12)
Print "15. 3FGA/TOT FGA "; X(I, 3)
Print "16. DEFENSIVE RATING (1-9) "; DF(I)
GoTo 250

4105 Cls

4115 Return

4500 JJ = 4
Locate 6, 8: Color 15, 1
Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
For II = 1 To JJ: Locate 6 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 7 + JJ, 8
Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";: Locate 7
Return

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
SP = CVI(Q$(1)): DA = CVI(Q$(2)): V1% = CVI(Q$(3)): V2% = CVI(Q$(4))
For I = 0 To 4: L%(I) = CVI(Q$(I + 5)): Next
For I = 0 To 9: T%(I) = CVI(Q$(I + 10)): Next
For I = 0 To 13
    A$(I) = Q$(I * 15 + 20)

    9007 If Right$(A$(I), 1) = " " Then A$(I) = Left$(A$(I), Len(A$(I)) - 1): GoTo 9007
    For I1 = 0 To 6
        A(I, I1) = CVI(Q$(I * 15 + I1 + 21))
        X(I, I1) = CVI(Q$(I * 15 + I1 + 28))
Next: Next
Close 1

Open D$(0) + "COLBBTMS." + YN$ For Random As #1 Len = 566
Field #1, 15 As Q$(0), 40 As Q$(1), 15 As Q$(2), 4 As Q$(3)
For I = 0 To 13
FIELD #1, 74 + I * 32 AS X$, 2 AS Q$(I * 16 + 4), 2 AS Q$(I * 16 + 5), 2 AS Q$(I * 16 + 6), 2 AS Q$(I * 16 + 7), 2 AS Q$(I * 16 + 8), 2 AS Q$(I * 16 + 9), 2 AS Q$(I * 16 + 10), 2 AS Q$(I * 16 + 11), 2 AS Q$(I * 16 + 12), 2 AS Q$(I * 16 + 13), 2 AS  _
Q$(I * 16 + 14), 2 AS Q$(I * 16 + 15), 2 AS Q$(I * 16 + 16), 2 AS Q$(I * 16 + 17), 2 AS Q$(I * 16 + 18), 2 AS Q$(I * 16 + 19)
FIELD #1, 522 AS X$, 2 AS Q$(228), 2 AS Q$(229), 2 AS Q$(230), 2 AS Q$(231), 2 AS Q$(232), 2 AS Q$(233), 2 AS Q$(234), 2 AS Q$(235), 2 AS Q$(236), 2 AS Q$(237), 2 AS Q$(238), 2 AS Q$(239), 2 AS Q$(240), 2 AS Q$(241), 2 AS Q$(242), 2 AS Q$(243), 2 AS _
Q$(244), 2 AS Q$(245), 2 AS Q$(246), 2 AS Q$(247), 2 AS Q$(248), 2 AS Q$(249)
Next
Get #1, T1
C$ = Q$(0)
S$ = Q$(1)
A1$ = Q$(2)
ATT = CVL(Q$(3))
For I = 0 To 13
    DF(I) = CVI(Q$(I * 16 + 4))
    P$(I) = Q$(I * 16 + 5)
    For I1 = 0 To 13
        RS(I, I1) = CVI(Q$(I * 16 + I1 + 6))
Next: Next
For I = 0 To 20
    T1%(I) = CVI(Q$(228 + I))
Next
CK = CVI(Q$(249))
Close 1
If CK <> 999 Then
    C$ = "": S$ = "": A1$ = "": ATT = 0
    For I = 0 To 13: DF(I) = 5: P$(I) = ""
        For I1 = 0 To 13: RS(I, I1) = 0
    Next: Next
    For I = 0 To 20: T1%(I) = 0: Next
End If

9180 Return

9200 Open D$(0) + "BASK." + NY$ For Random As #1 Len = 655
Y = LOF(1) / 655
Close 1
T1 = Y + 1
Return

9800 If NID% = 1 Then GoSub 9200: YN$ = NY$
Open D$(0) + "BASK." + YN$ For Random As #1 Len = 655
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 2 AS Q$(16), 2 AS Q$(17), 2 AS Q$(18), 2 _
AS Q$(19), 602 AS X$
For I = 0 To 13
FIELD #1, 53 + I * 43 AS X$, 15 AS Q$(I * 15 + 20), 2 AS Q$(I * 15 + 21), 2 AS Q$(I * 15 + 22), 2 AS Q$(I * 15 + 23), 2 AS Q$(I * 15 + 24), 2 AS Q$(I * 15 + 25), 2 AS Q$(I * 15 + 26), 2 AS Q$(I * 15 + 27), 2 AS Q$(I * 15 + 28), 2 AS Q$(I * 15 + 29) _
, 2 AS Q$(I * 15 + 30), 2 AS Q$(I * 15 + 31), 2 AS Q$(I * 15 + 32), 2 AS Q$(I * 15 + 33), 2 AS Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 AS X$
Next
LSet Q$(0) = A$: LSet Q$(1) = MKI$(SP): LSet Q$(2) = MKI$(DA): LSet Q$(3) = MKI$(V1%)
LSet Q$(4) = MKI$(V2%): For I = 0 To 4: LSet Q$(I + 5) = MKI$(L%(I)): Next
T%(2) = 999
For I = 0 To 9: LSet Q$(I + 10) = MKI$(T%(I)): Next
For I = 0 To 13
    LSet Q$(I * 15 + 20) = A$(I)
    For I1 = 0 To 6
        LSet Q$(I * 15 + I1 + 21) = MKI$(A(I, I1))
        LSet Q$(I * 15 + I1 + 28) = MKI$(X(I, I1))
Next: Next
Put #1, T1: Close 1

Open D$(0) + "COLBBTMS." + YN$ For Random As #1 Len = 566
Field #1, 15 As Q$(0), 40 As Q$(1), 15 As Q$(2), 4 As Q$(3)
For I = 0 To 13
FIELD #1, 74 + I * 32 AS X$, 2 AS Q$(I * 16 + 4), 2 AS Q$(I * 16 + 5), 2 AS Q$(I * 16 + 6), 2 AS Q$(I * 16 + 7), 2 AS Q$(I * 16 + 8), 2 AS Q$(I * 16 + 9), 2 AS Q$(I * 16 + 10), 2 AS Q$(I * 16 + 11), 2 AS Q$(I * 16 + 12), 2 AS Q$(I * 16 + 13), 2 AS  _
Q$(I * 16 + 14), 2 AS Q$(I * 16 + 15), 2 AS Q$(I * 16 + 16), 2 AS Q$(I * 16 + 17), 2 AS Q$(I * 16 + 18), 2 AS Q$(I * 16 + 19)
FIELD #1, 522 AS X$, 2 AS Q$(228), 2 AS Q$(229), 2 AS Q$(230), 2 AS Q$(231), 2 AS Q$(232), 2 AS Q$(233), 2 AS Q$(234), 2 AS Q$(235), 2 AS Q$(236), 2 AS Q$(237), 2 AS Q$(238), 2 AS Q$(239), 2 AS Q$(240), 2 AS Q$(241), 2 AS Q$(242), 2 AS Q$(243), 2 AS _
Q$(244), 2 AS Q$(245), 2 AS Q$(246), 2 AS Q$(247), 2 AS Q$(248), 2 AS Q$(249)
Next
LSet Q$(0) = C$
LSet Q$(1) = S$
LSet Q$(2) = A1$
LSet Q$(3) = MKL$(ATT)
For I = 0 To 13
    LSet Q$(I * 16 + 4) = MKI$(DF(I))
    LSet Q$(I * 16 + 5) = P$(I)
    For I1 = 0 To 13
        LSet Q$(I * 16 + I1 + 6) = MKI$(RS(I, I1))
Next: Next
For I = 0 To 20
    LSet Q$(228 + I) = MKI$(T1%(I))
Next
LSet Q$(249) = MKI$(999)
Put #1, T1: Close 1

10010 Return

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
