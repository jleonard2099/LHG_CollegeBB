'$INCLUDE: 'KeyInput.BI'

1 Cls

5 Dim Z(15), Z1(15), A$(15), A(15, 14), W0(15), W1(15)

7 Dim ZR(15), ZR1(15), AR$(15), AR(15, 14), W0R(15), WR1(15)

8 Dim AN$(15), AN(15, 14), W0N(15), WN1(15)

12 Color 12, 0: Cls: Locate 2, 5: Print "ENTER COMPLETE PATH FOR TEAM STATS TO ALIGN"

13 Color 15: Locate 4, 5: Print "EXAMPLE - A:"

14 Locate , 5: Print "          C:\HAFFNER"

15 Locate , 5: Print "          C:\HAFFNER\1960"

16 Locate , 5: Print "          D:\BASKET"

20 For J = 10 To 13: Locate J

21 Print "                                                              ": Next

25 Color 10: Locate 10, 5: Line Input "ROAD STATS:     "; PR$

26 Locate , 5: Line Input "CURRENT STATS:  "; PC$

27 Color 14: Locate 13, 5: Print "ANY CHANGE (Y/N)"

28 I$ = GetKeyPress$
29 If UCase$(I$) = "Y" Then 20

45 Locate 15, 5: Print "                                                       "

46 Locate 16, 5: Print "                                                       "

47 Color 11: Locate 15, 5: Input "ENTER NAME OF STAT FILE (EX.- MINNESOT.90)"; Z$

50 Locate , 5: Print "ANY CHANGE (YN)"

60 I$ = GetKeyPress$
70 If UCase$(I$) = "Y" Then 45

110 If Not _FileExists(PR$ + "\" + Z$) Then 950

120 Open PR$ + "\" + Z$ For Input As #1

121 Input #1, BR$

122 For I = 0 To 13: Input #1, AR$(I)

123 For J = 0 To 13: Input #1, AR(I, J): Next: Input #1, W0R(I): Input #1, WR1(I): Next

124 For L = 0 To 15: Input #1, ZR(L): Next

125 For L = 0 To 15: Input #1, ZR1(L): Next

160 Close #1

210 If Not _FileExists(PC$ + "\" + Z$) Then 950

220 Open PC$ + "\" + Z$ For Input As #1

221 Input #1, B$

222 For I = 0 To 13: Input #1, A$(I)

223 For J = 0 To 13: Input #1, A(I, J): Next: Input #1, W0(I): Input #1, W1(I): Next

224 For L = 0 To 15: Input #1, Z(L): Next

225 For L = 0 To 15: Input #1, Z1(L): Next

260 Close #1

400 GoTo 1000

404 N = 0

405 For J = 0 To 13

    410 If A$(N) = AR$(J) Then 420 Else GoTo 424

    420 AN$(N) = AR$(J)

    421 For K = 0 To 13: AN(N, K) = AR(J, K): W0N(N) = W0R(J): WN1(N) = WR1(J)

    422 Next K

424 Next J

425 N = N + 1

426 If N = 15 Then GoTo 820 Else GoTo 405

820 KKK = 1: GoTo 1300

821 Open PR$ + "\" + Z$ For Output As #1
Write #1, BR$

822 For I = 0 To 13: Write #1, AN$(I)

823 For J = 0 To 13: Write #1, AN(I, J): Next: Write #1, W0N(I): Write #1, WN1(I): Next

824 For L = 0 To 15: Write #1, ZR(L): Next

825 For L = 0 To 15: Write #1, ZR1(L): Next

860 Close #1

869 Locate 19, 20: Print "                                                  "

880 Color 15: Cls: Locate 8, 20: Print "RE-ALIGNMENT COMPLETED"

881 Print: Locate 12, 20: Color 12: Line Input "HIT ENTER TO CONTINUE"; DUMMY$

900 Run "MERGE.EXE"

950 Cls: Locate 2, 10: Color 7: Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"

951 Print: Locate , 10: Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$

952 GoTo 12

1000 Cls: Locate 1, 32: Color 12: Print "STAT ROSTERS"

1001 Locate , 26: Color 15: Print "CURRENT  ";: Color 7: Print "             ROAD"

1002 For I = 0 To 13: Locate I + 4, 22: Color 12: Print Using "##"; I;: Print " ";: Color 15: Print A$(I);: Color 12: Print Tab(44); Using "##"; I;: Print " ";: Color 7: Print AR$(I): Next

1003 Color 11: Locate 22: Line Input "HIT ENTER TO ALIGN"; DUMMY$

1004 GoTo 1209

1010 Cls: Locate 1, 32: Color 12: Print "STAT ROSTERS"

1011 Locate , 26: Color 15: Print "CURRENT  ";: Color 7: Print "             ROAD"

1021 For I = 0 To 13: Locate I + 4, 22: Color 12: Print Using "##"; I;: Print " ";: Color 15: Print A$(I);: Color 12: Print Tab(44); Using "##"; I;: Print " ";: Color 7: Print AN$(I): Next

1022 Color 11: Locate 22: Line Input "HIT ENTER TO ALIGN"; DUMMY$

1023 GoTo 821

1209 Color 11: Locate 22: Print "ALIGNING FILES . . .             "

1212 GoTo 404

1300 Locate 22: Print "                                                     "

1301 SEQ = 0

1302 For K = 0 To 13

    1303 If AN$(K) = A$(K) Then 1305 Else GoTo 1304

    1304 SEQ = SEQ + 1

1305 Next

1320 If SEQ = 0 Then 1010 Else GoTo 1400

1400 Locate 20: Color 14: Print "WARNING !! - - ROAD STAT FILE CONTAINS PLAYERS NOT IN EXISTING STATS.  YOU MUST"

1402 Print "CHANGE ROAD STAT FILE USING OPTION #6 ON GAME MENU BEFORE ATTEMPTING STAT MERGE."

1405 Locate 23, 30: Color 13: Line Input "HIT ENTER TO CONTINUE"; DUMMY$

1406 GoTo 900

'$INCLUDE: 'KeyInput.BM'
