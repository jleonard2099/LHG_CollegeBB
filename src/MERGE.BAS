'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

Dim item$(3), D$(1), A$(14)
'Dim AR$(1), G$(4), G%(4, 10), B%(26, 11)
Dim T%(100), B$(26)
'Dim TR%(12), GR$(4), GR%(4, 10), BR$(26), BR%(26, 11)
'Dim H1(100), O1$(100), V1(100), L1$(100), WG1$(100), LG1$(100), P1(100)
Dim O$(100)
'Dim H(100), V(100), L$(100), WG$(100), LG$(100), P(100)
Dim VV$(500), VV%(500), Q$(500)
'Dim A%(17), APR%(1, 120), CRDR(120), SITER$(120)
Dim APRD%(100, 1), CRDRD(100), SITERD$(100)
Dim AP%(100, 1), CRD(100), SITE$(100)
Dim O%(100), H$(100), L%(4)
'Dim P%(100), Z2(14, 14), PRD%(100), ZRD2(14, 14)
Dim Z(15), Z1(15), A(14, 14), W0(15), W1(15)
Dim ORD%(100), ORD$(100), TRD%(100), HRD$(100)
Dim ZRD(15), ZRD1(15), ARD$(14), ARD(14, 14), W0RD(15), W1RD(15)

Open "DEV.BAS" For Input As #1: For I = 0 To 1: Input #1, D$(I): Next: Close #1

1 Cls: Color , 1: Locate 1, 1: Print Tab(80);: Locate , 26: Color 0
Print "COURTSIDE COLLEGE BASKETBALL: MERGE"
Locate 24, 1: Print Tab(80);: Locate , 1
Print "(C) LANCE HAFFNER/SHANNON LYNN 1991"; Tab(70); Left$(Time$, 5);
Locate 4, 1: Color 1, 0
item$(1) = "MERGE STATISTICAL FILES           "
item$(2) = "ALIGN STATISTICAL FILES           "
item$(3) = "EXIT TO MAIN MENU                 "
SELECTION% = 1
Count% = 3
Start% = 1
SCANCODE% = 3
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 25

Call box0(7, 24, 11, 59, 2, 31)
_MouseShow "DEFAULT"
MMenuVert item$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
RN = SELECTION%
If SCANCODE% = 27 Then RN = 3
If RN = 1 Then 14
If RN = 2 Then Run "ALIGN.EXE"

5 Run "HELLO.EXE 4"

14 Color 12, 0: Cls: Locate 2, 5: Print "ENTER COMPLETE PATH FOR TEAM STATS TO MERGE"

23 Color 15: Locate 4, 5: Print "EXAMPLE - A:"

24 Locate , 5: Print "          C:\HAFFNER"

25 Locate , 5: Print "          C:\HAFFNER\1980"

26 Locate , 5: Print "          D:\BASKET"

27 For J = 10 To 13: Locate J

28 Print "                                                            ": Next

35 Color 10: Locate 10, 5: Line Input "ROAD STATS:     "; PR$

36 Locate , 5: Locate , 5: Line Input "CURRENT STATS:  "; PC$

37 Color 14: Locate 13, 5: Print "ANY CHANGE (Y/N)"
38 I$ = GetKeyPress$

39 If UCase$(I$) = "Y" Then 27
GoSub 3056
B$ = A$: GoSub 6585
Z$ = Mid$(B$, 4, QQ) + "." + Left$(B$, 2)
Cls
Locate 10, 15: Print "ROAD STATS ARE ON DRIVE "; PR$
Locate , 15: Print "MASTER STATS ARE ON DRIVE "; PC$
Locate , 15: Print "TEAM TO MERGE STATS FOR IS "; A$
Locate 14, 15: Print "ANY CHANGE (YN)"
60 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1

100 GoSub 1002

111 If Not _FileExists(PR$ + "\" + Z$) Then 900

120 Open PR$ + "\" + Z$ For Input As #1

121 Input #1, B$

122 For I = 0 To 13: Input #1, ARD$(I)

123 For J = 0 To 13: Input #1, ARD(I, J): Next: Input #1, W0RD(I): Input #1, W1RD(I): Next

124 For L = 0 To 15: Input #1, ZRD(L): Next

125 For L = 0 To 15: Input #1, ZRD1(L): Next

130 Close #1

180 GoTo 500

200 GoSub 1100

210 If Not _FileExists(PC$ + "\" + Z$) Then 900

220 Open PC$ + "\" + Z$ For Input As #1

221 Input #1, B$

222 For I = 0 To 13: Input #1, A$(I)

223 For J = 0 To 13: Input #1, A(I, J): Next: Input #1, W0(I): Input #1, W1(I): Next

224 For L = 0 To 15: Input #1, Z(L): Next

225 For L = 0 To 15: Input #1, Z1(L): Next

230 Close #1

280 GoTo 600

319 Locate 18, 20: Color 12: Print "MERGING STAT FILE AND SCHEDULE RESULTS . . . ."

324 For I = 0 To 13: For J = 0 To 12: A(I, J) = A(I, J) + ARD(I, J): Next J: W0(I) = W0(I) + W0RD(I): W1(I) = W1(I) + W1RD(I): GoSub 350: Next I

325 For L = 0 To 15: Z(L) = Z(L) + ZRD(L): Z1(L) = Z1(L) + ZRD1(L): Next

326 GoTo 420

350 If A(I, 13) < ARD(I, 13) Then A(I, 13) = ARD(I, 13) Else A(I, 13) = A(I, 13)

352 Return

420 Open PC$ + "\" + Z$ For Output As #1

421 Write #1, B$

422 For I = 0 To 13: Write #1, A$(I)

423 For J = 0 To 13: Write #1, A(I, J): Next: Write #1, W0(I): Write #1, W1(I): Next

424 For L = 0 To 15: Write #1, Z(L): Next

425 For L = 0 To 15: Write #1, Z1(L): Next

460 Close #1

495 GoTo 800

500 Open PR$ + "\" + Z$ + "S" For Input As #1

501 Input #1, YR%

502 For I = 1 To 100: Input #1, ORD%(I): Next I

503 For I = 1 To 100: Input #1, ORD$(I): Next I

504 For I = 1 To 100: Input #1, TRD%(I): Next I

505 For I = 1 To 100: Input #1, HRD$(I): Next I
For I = 1 To 100
For I1 = 0 To 1: Input #1, APRD%(I, I1): Next: Next
For I = 1 To 100: Input #1, CRDRD(I): Next I
For I = 1 To 100: Input #1, SITERD$(I): Next I

506 Close #1

595 GoTo 200

600 Open PR$ + "\" + Z$ + "S" For Input As #1

601 Input #1, Y%

602 For I = 1 To 100: Input #1, O%(I): Next I

603 For I = 1 To 100: Input #1, O$(I): Next I

604 For I = 1 To 100: Input #1, T%(I): Next I

605 For I = 1 To 100: Input #1, H$(I): Next I
For I = 1 To 100
For I1 = 0 To 1: Input #1, AP%(I, I1): Next: Next
For I = 1 To 100: Input #1, CRD(I): Next I
For I = 1 To 100: Input #1, SITE$(I): Next I

606 Close #1

650 GoTo 1200

700 For K = 1 To YR%

    701 Y% = Y% + 1

    705 O%(Y%) = ORD%(K)

    710 O$(Y%) = ORD$(K)

    715 T%(Y%) = TRD%(K)

    720 H$(Y%) = HRD$(K)
    AP%(Y%, 0) = APRD%(Y%, 0)
    AP%(Y%, 1) = APRD%(Y%, 1)
    CRD(Y%) = CRDRD(Y%)
    SITE$(Y%) = SITERD$(Y%)

750 Next

790 GoTo 319

800 Open PC$ + "\" + Z$ + "S" For Output As #1

801 Write #1, Y%

805 For I = 1 To 100: Write #1, O%(I): Next I

810 For I = 1 To 100: Write #1, O$(I): Next I

815 For I = 1 To 100: Write #1, T%(I): Next I

820 For I = 1 To 100: Write #1, H$(I): Next I
For I = 1 To 100
For I1 = 0 To 1: Write #1, AP%(I, I1): Next: Next
For I = 1 To 100: Write #1, CRD(I): Next I
For I = 1 To 100: Write #1, SITE$(I): Next I

840 Close #1

850 Y = 0

860 Locate 20, 20: Color 12: Print "RESULTS SUCCESSFULLY MERGED!!"

861 For J = 1 To 5000: Next J

875 GoTo 950

900 Cls: Locate 2, 10: Color 7: Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"

901 Print: Locate , 10: Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$

902 GoTo 1

950 For J = 13 To 23

    951 Locate J

    952 Print "                                                                 "

953 Next

985 GoTo 1

993 Run "HELLO.EXE 2"
1002 For J = 21 To 23
    1003 Locate J
    1004 Print "                                                                "
1005 Next

1006 Return

1100 Locate 21, 5: Color 3
For J = 21 To 23
    Locate J
    1104 Print "                                                                "
1105 Next

1106 Return

1200 SEQ = 0

1201 For K = 0 To 13

    1202 If ARD$(K) = A$(K) Then 1220 Else GoTo 1210

    1210 SEQ = SEQ + 1

1220 Next

1260 If SEQ = 0 Then 700 Else GoTo 1270

1270 Cls: Color 14: Locate 5, 27: Print "PLAYERS NOT IN SEQUENCE !!"; DUMMY$

1275 Color 15: Locate 7, 20: Print "STATS WILL BE DISTORTED UNLESS THE ROAD"

1276 Locate , 19: Print "FILE LISTS THE PLAYERS IN THE SAME SEQUENCE"

1277 Locate , 24: Print "AS YOUR CURRENT PLAYER STAT FILE."

1278 Locate 11, 24: Color 12: Print "YOU MUST REALIGN ROAD STAT FILE OR"

1279 Locate , 31: Print "ENTER STATS MANUALLY."

1280 Locate 15, 29: Color 13: Line Input "HIT ENTER TO GO TO MENU"; DUMMY$

1290 GoTo 5

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
If I$ = Chr$(27) Then Z$ = "END": Clear: GoTo 1
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

3058 Locate 2, 33: Color T%(0), T%(1): Print A$
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
20 MAMenu VV$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
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

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
