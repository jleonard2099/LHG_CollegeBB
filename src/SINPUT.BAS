'$INCLUDE: 'KeyInput.BI'

5 PX = 0
Open "UDEV.BAS" For Input As #1
Input #1, D$(0): Input #1, D$(1): Close #1

15 Dim W0(14), W1(14), A$(14), A(14, 14), Z2(14, 14), Z(15), Z1(15), Q$(500), V$(500)
Color 7, 0: KEY Off: On Error GoTo 0
Cls: GoSub 2200
If UCase$(Left$(D$(1), 1)) = "C" Then 55

55 Cls: GoSub 2200

65 Color 15: Print "C";: Color 3: Print "HANGE/INPUT COMPILED TEAM STATS"
Print
Color 15: Print "R";: Color 3: Print "ETURN TO MAIN MENU"

75 I$ = GetKeyPress$
85 If UCase$(I$) <> "R" Then Q = 1
If UCase$(I$) = "R" Then Q = 2

95 If Q < 1 Or Q > 2 Then 75

105 If Q = 1 Then 2015

115 If Q = 2 Then Cls
GoSub 4500

135 Run "UTIL.EXE"
End

165 Cls: X$ = "OFFENSIVE TEAM STATS": GoSub 2205

175 Locate 3, 1: Input "1. STAT FILE NAME "; Z$: If C1 = 1 Then 210

177 Locate 4, 1: Input "2. TM MINUTES "; Z(0): Z(0) = Z(0) * 60: If C1 = 1 Then 210

179 Locate 5, 1: Input "3. TM FGA "; Z(1): If C1 = 1 Then 210

181 Locate 6, 1: Input "4. TM FGM "; Z(2): If C1 = 1 Then 210

183 Locate 7, 1: Input "5. TM 3FGA "; Z(14): If C1 = 1 Then 210

185 Locate 8, 1: Input "6. TM 3FGM "; Z(13): If C1 = 1 Then 210

187 Locate 9, 1: Input "7. TM FTA "; Z(3): If C1 = 1 Then 210

189 Locate 10, 1: Input "8. TM FTM "; Z(4): If C1 = 1 Then 210

191 Locate 11, 1: Input "9. TM OFF RB  "; Z(12): If C1 = 1 Then 210

193 Locate 12, 1: Input "10. TM TOT RB "; Z(5): If C1 = 1 Then 210

195 Locate 13, 1: Input "11. TM FOULS "; Z(6): If C1 = 1 Then 210

197 Locate 14, 1: Input "12. TM STEALS "; Z(7): If C1 = 1 Then 210

199 Locate 15, 1: Input "13. TM ASSISTS "; Z(8): If C1 = 1 Then 210

201 Locate 16, 1: Input "14. TM BLOCKS "; Z(9): If C1 = 1 Then 210

203 Locate 17, 1: Input "15. TM TURNOVERS "; Z(11): If C1 = 1 Then 210

205 Locate 18, 1: Input "16. TM POINTS  "; Z(10): If C1 = 1 Then 210

210 C1 = 1

211 Locate 20, 1: Input "# TO CHANGE (0 TO CONTINUE) "; B9

212 If B9 < 0 Or B9 > 16 Then 211

213 On B9 GOTO 175, 177, 179, 181, 183, 185, 187, 189, 191, 193, 195, 197, 199, 201, 203, 205

215 C1 = 0

217 If X% > 0 Then 1580

365 Cls: X$ = "DEFENSIVE TEAM STATS": GoSub 2205

379 Locate 3, 1: Input "1. OPP FGA "; Z1(1): If C1 = 1 Then 410

381 Locate 4, 1: Input "2. OPP FGM "; Z1(2): If C1 = 1 Then 410

383 Locate 5, 1: Input "3. OPP 3FGA "; Z1(14): If C1 = 1 Then 410

385 Locate 6, 1: Input "4. OPP 3FGM "; Z1(13): If C1 = 1 Then 410

387 Locate 7, 1: Input "5. OPP FTA "; Z1(3): If C1 = 1 Then 410

389 Locate 8, 1: Input "6. OPP FTM "; Z1(4): If C1 = 1 Then 410

391 Locate 9, 1: Input "7. OPP OFF RB  "; Z1(12): If C1 = 1 Then 410

393 Locate 10, 1: Input "8. OPP TOT RB "; Z1(5): If C1 = 1 Then 410

395 Locate 11, 1: Input "9. OPP FOULS "; Z1(6): If C1 = 1 Then 410

397 Locate 12, 1: Input "10. OPP STEALS "; Z1(7): If C1 = 1 Then 410

399 Locate 13, 1: Input "11. OPP ASSISTS "; Z1(8): If C1 = 1 Then 410

401 Locate 14, 1: Input "12. OPP BLOCKS "; Z1(9): If C1 = 1 Then 410

403 Locate 15, 1: Input "13. OPP TURNOVERS "; Z1(11): If C1 = 1 Then 410

405 Locate 16, 1: Input "14. OPP POINTS  "; Z1(10): If C1 = 1 Then 410

410 C1 = 1

411 Locate 20, 1: Input "# TO CHANGE (0 TO CONTINUE) "; B9

412 If B9 < 0 Or B9 > 14 Then 411

413 On B9 GOTO 379, 381, 383, 385, 387, 389, 391, 393, 395, 397, 399, 401, 403, 405

645 C1 = 0

655 If X% > 0 Then 1605

665 For I = 0 To 13: Cls: X$ = "PLAYER -" + Str$(I + 1): GoSub 2205

    675 Locate 3, 1: Input "1. NAME "; A$(I): If C1 = 1 Then 1225

    705 Locate 4, 1: Input "2. GAMES "; Z2(I, 0): If C1 = 1 Then 1225

    745 Locate 5, 1: Input "3. MINUTES "; Z2(I, 10): Z2(I, 10) = Z2(I, 10) * 60: If C1 = 1 Then 1225

    785 Locate 6, 1: Input "4. FGA "; Z2(I, 1): If C1 = 1 Then 1225

    825 Locate 7, 1: Input "5. FGM "; Z2(I, 2): If C1 = 1 Then 1225

    865 Locate 8, 1: Input "6. FTA "; Z2(I, 3): If C1 = 1 Then 1225

    905 Locate 9, 1: Input "7. FTM "; Z2(I, 4): If C1 = 1 Then 1225

    906 Locate 10, 1: Input "8. OFF REB "; Z2(I, 12): If C1 = 1 Then 1225

    945 Locate 11, 1: Input "9. TOT REB  "; Z2(I, 5): If C1 = 1 Then 1225

    985 Locate 12, 1: Input "10. FOULS "; Z2(I, 6): If C1 = 1 Then 1225

    1025 Locate 13, 1: Input "11. STEALS "; Z2(I, 7): If C1 = 1 Then 1225

    1065 Locate 14, 1: Input "12. ASSISTS "; Z2(I, 8): If C1 = 1 Then 1225

    1105 Locate 15, 1: Input "13. BLOCKS "; Z2(I, 9): If C1 = 1 Then 1225

    1106 Locate 16, 1: Input "14. TURNOVERS "; Z2(I, 11): If C1 = 1 Then 1225

    1184 Locate 17, 1: Input "15. 3FGA "; W1(I): If C1 = 1 Then 1225

    1185 Locate 18, 1: Input "16. 3FGM "; W0(I): If C1 = 1 Then 1225

    1186 Locate 19, 1: Input "17. HI-GAME (PTS) "; Z2(I, 13): If C1 = 1 Then 1225

    1225 C1 = 1

    1235 Locate 21, 1

    1245 Input "# TO CHANGE (0 TO CONTINUE) "; B9

    1255 If B9 < 0 Or B9 > 17 Then 1235

    1265 On B9 GOTO 675, 705, 745, 785, 825, 865, 905, 906, 945, 985, 1025, 1065, 1105, 1106, 1184, 1185, 1186

    1275 C1 = 0

    1285 If X% > 0 Then 1605

1295 Next I

1305 Cls: X$ = "OFFENSIVE TEAM STATS": GoSub 2205

1335 X% = 2

1345 Locate 3, 1
Print "1. STAT FILE NAME "; Z$
Print "2. TM MINUTES ";: Print Using "####"; Int(Z(0) / 60)
Print "3. TM FGA "; Z(1)
Print "4. TM FGM "; Z(2)
Print "5. TM 3FGA "; Z(14)
Print "6. TM 3FGM "; Z(13)
Print "7. TM FTA "; Z(3)
Print "8. TM FTM "; Z(4)
Print "9. TM OFF RB  "; Z(12)
Print "10. TM TOT RB "; Z(5)
Print "11. TM FOULS "; Z(6)
Print "12. TM STEALS "; Z(7)
Print "13. TM ASSISTS "; Z(8)
Print "14. TM BLOCKS "; Z(9)
Print "15. TM TURNOVERS "; Z(11)
Print "16. TM POINTS  "; Z(10)

1575 GoTo 210

1580 Cls: X$ = "DEFENSIVE TEAM STATS": GoSub 2205
X% = 2
Locate 3, 1
Print "1. OPP FGA "; Z1(1)
Print "2. OPP FGM "; Z1(2)
Print "3. OPP 3FGA "; Z1(14)
Print "4. OPP 3FGM "; Z1(13)
Print "5. OPP FTA "; Z1(3)
Print "6. OPP FTM "; Z1(4)
Print "7. OPP OFF RB  "; Z1(12)
Print "8. OPP TOT RB "; Z1(5)
Print "9. OPP FOULS "; Z1(6)
Print "10. OPP STEALS "; Z1(7)
Print "11. OPP ASSISTS "; Z1(8)
Print "12. OPP BLOCKS "; Z1(9)
Print "13. OPP TURNOVERS "; Z1(11)
Print "14. OPP POINTS  "; Z1(10)
GoTo 410

1605 Cls: GoSub 2200: Locate 3, 1: Color 14: Print "CHANGE PLAYERS "

1615 Print

1625 X% = 2

1635 For I = 0 To 13
    Color 7
    If I / 2 = Int(I / 2) Then Color 15

    1645 Print (I + 1);

1655 Print " "; A$(I): Next I

1665 Print

1675 Print "# TO CHANGE (0 TO CONTINUE) ";

1685 Input I: I = I - 1

1695 If I > 13 Or I < 0 Then X% = 0: GoTo 1865

1705 Cls
X$ = "PLAYER -" + Str$(I + 1): GoSub 2205
Locate 3, 1
Print "1. NAME "; A$(I)
Print "2. GAMES "; Z2(I, 0)
Print "3. MINUTES "; Int(Z2(I, 10) / 60 + .5)
Print "4. FGA "; Z2(I, 1)
Print "5. FGM "; Z2(I, 2); ""
Print "6. FTA "; Z2(I, 3)
Print "7. FTM "; Z2(I, 4)
Print "8. OFF REB "; Z2(I, 12)
Print "9. TOT REB  "; Z2(I, 5)
Print "10. FOULS "; Z2(I, 6)
Print "11. STEALS "; Z2(I, 7)
Print "12. ASSISTS "; Z2(I, 8)
Print "13. BLOCKS "; Z2(I, 9)
Print "14. TURNOVERS "; Z2(I, 11)
Print "15. 3FGA "; W1(I)
Print "16. 3FGM "; W0(I)
Print "17. HI-GAME (PTS) "; Z2(I, 13)
GoTo 1225

1865 GoTo 1935

1875 Print

1885 Print "# TO CHANGE (1-12) (0 TO CONTINUE) ";

1895 Input I

1905 I = I - 1

1915 If I > 10 Or I < 0 Then X% = 0: GoTo 1935

1925 Cls: GoSub 2200

1935 Open D$(1) + Z$ For Output As #1

1945 Print #1, A$

1955 For I = 0 To 13: Print #1, A$(I)

1965 For J = 0 To 13: Print #1, Z2(I, J): Next J: Print #1, W0(I): Print #1, W1(I): Next I

1975 For L = 0 To 15: Print #1, Z(L): Next L

1985 For L = 0 To 15: Print #1, Z1(L): Next L

1995 Close 1

2005 GoTo 55

2015 GoSub 3056
GoSub 6585
Z$ = Right$(B$, Len(B$) - 3) + "." + Left$(B$, 2)
If Left$(D$(1), 1) = "C" Then 2025
Cls
GoSub 4500

2025 Cls: X$ = "OFFENSIVE TEAM STATS": GoSub 2205: Locate 3, 1
Print "1. STAT FILE NAME "; Z$

2035 Open D$(1) + Z$ For Input As #1

2045 Input #1, A$

2055 For I = 0 To 13: Input #1, A$(I)

2065 For J = 0 To 13: Input #1, Z2(I, J): Next J: Input #1, W0(I): Input #1, W1(I): Next I

2075 For L = 0 To 15: Input #1, Z(L): Next L

2085 For L = 0 To 15: Input #1, Z1(L): Next L

2095 Close 1

2105 X% = 2: GoTo 1305

2115 Print "                              ";

2125 Return

2200 Color , 1: Locate 1, 1: Print Tab(80);: Locate , 28: Color 14
Print "EDIT COMPILED STATISTICS"
Locate 24, 1: Print Tab(80);: Locate , 1
Print "(C) SHANNON LYNN 1991"; Tab(70); Left$(Time$, 5);
Locate 4, 1: Color 15, 0
Return

2205 Color , 1: Locate 1, 1: Print Tab(80);: Locate , Int(80 - Len(X$)) / 2: Color 14
Print X$
Locate 24, 1: Print Tab(80);: Locate , 1
Print "(C) SHANNON LYNN 1991"; Tab(70); Left$(Time$, 5);
Locate 4, 1: Color 15, 0
Return

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
Color 15, 0

3057 I$ = GetKeyPress$
If I$ = Chr$(27) Then 55
If I$ = Chr$(13) Then GoSub 1102: GoTo 3060
Locate 7, 27: Print I$
Locate 7, 28: Line Input NY$
NY$ = I$ + NY$
Color 15, 0

If NY$ = "" Then GoSub 1102: GoTo 3060
YN$ = NY$
For I = 1 To 500: V$(I) = "": Next
GoSub 1100

3060 If BO% = 1 Then BO% = 0: GoTo 3056

3058 Locate 2, 33: Color T%(0), T%(1): Print A$
Color 15, 4: Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
59 If UCase$(I$) = "Y" Then 3056
Color 15, 0
Return

1100 Open D$(0) + "BASK." + YN$ As #1 Len = 655
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
'PRINT X,Y

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

1190 T1 = NS% * 80 + Y - 3 + (X - 1) * 20

9000 Open D$(0) + "BASK." + YN$ As #1 Len = 655
FIELD #1,15 AS Q$(0),2 AS Q$(1),2 AS Q$(2),2 AS Q$(3),2 AS Q$(4),2 AS Q$(5),2 AS Q$(6),2 AS Q$(7),2 AS Q$(8),2 AS Q$(9),2 AS Q$(10),2 AS Q$(11),2 AS Q$(12),2 AS Q$(13),2 AS Q$(14),2 AS Q$(15),2 AS Q$(16),2 AS Q$(17),2 AS Q$(18),2 AS Q$(19),602 AS _
X$
For I = 0 To 13
FIELD #1,53+I*43 AS X$,15 AS Q$(I*15+20),2 AS Q$(I*15+21),2 AS Q$(I*15+22),2 AS Q$(I*15+23),2 AS Q$(I*15+24),2 AS Q$(I*15+25),2 AS Q$(I*15+26),2 AS Q$(I*15+27),2 AS Q$(I*15+28),2 AS Q$(I*15+29),2 AS Q$(I*15+30),2 AS Q$(I*15+31),2 AS Q$(I*15+32),2 _
AS Q$(I*15+33),2 AS Q$(I*15+34),655-53-43-I*43 AS X$
Next
Get #1, T1
A$ = Q$(0)

9005 If Right$(A$, 1) = " " Then A$ = Left$(A$, Len(A$) - 1): GoTo 9005
B$ = A$
SP = CVI(Q$(1)): DA = CVI(Q$(2)): V1% = CVI(Q$(3)): V2% = CVI(Q$(4))
For I = 0 To 4: L%(I) = CVI(Q$(I + 5)): Next
For I = 0 To 9: T%(I) = CVI(Q$(I + 10)): Next
Close 1
Return

4500 JJ = 4
Locate 6, 8: Color 15, 1
Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
For II = 1 To JJ: Locate 6 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 7 + JJ, 8
Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";: Locate 7
Return

4501 I$ = GetKeyPress$
Return

6585 For XX = 4 To 11: If Mid$(B$, XX, 1) = " " Or Mid$(B$, XX, 1) = "." Then Mid$(B$, XX, 1) = "_"

6590 Next XX

6595 For XX = 12 To 14: If Mid$(B$, XX, 2) = "ST" Then Mid$(B$, 9, 1) = "_": Mid$(B$, 10, 2) = "ST"

6600 Next XX

6605 QQ = 8: For XX = 11 To 4 Step -1: If Mid$(B$, XX, 1) = "_" Then QQ = XX - 4: GoTo 6615

    6610 GoTo 6620

6615 Next XX

6620 Return

'$INCLUDE: 'KeyInput.BM'
