Sub CAREER ()
    'CH = 0
    'NP = -1

    _Title "Courtside College Basketball - Career Stats"

    ReDim menuChoices$(7)

    Do

        Cls

        CALL TITLE_CAREER
        
        menuChoices$(1) = "CREATE AND MERGE INITIAL STAT FILE"
        menuChoices$(2) = "MERGE / UPDATE CAREER FILES       "
        'menuChoices$(3) = "VIEW / PRINT CAREER FILE          "
        menuChoices$(3) = "VIEW CAREER FILE                  "
        menuChoices$(4) = "PRINT CAREER FILE                 "
        menuChoices$(5) = "VIEW / PRINT CAREER RECORDS       "
        menuChoices$(6) = "EXIT TO MAIN MENU                 "
        SELECTION% = 1
        Count% = 6
        Start% = 1
        EXITCODE% = 6
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        CALL box0(7, 24, 14, 59, 2,  15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0
        'I = SELECTION%

        If Not(EXITCODE% = 27) Then
            Select Case SELECTION%
                Case 1: Call CreateCareerFile
                Case 2: 
                    Color 14, 0: Cls: Locate 5, 27: Print "- - - C A U T I O N - - -"
                    Color 15: Locate 8, 1: Print "THIS OPTION SHOULD ONLY BE USED WHEN ADDING YEARS TO AN EXISTING STAT"
                    Print "FILE. IF YOU ARE CREATING A NEW CAREER FILE, USE THE CREATE OPTION.": Print
                    Print "IF YOU ARE ADDING AN ADDITIONAL CAREER YEAR, MAKE SURE THAT ANY UNUSED PLAYER"
                    Print "SLOTS (XXX, END, ETC.) ARE AT THE END OF THE SEASON STAT LIST BEFORE YOU MERGE."
                    Print: Print "ALSO MAKE SURE THAT THE PLAYERS NAMES IN THE FILE YOU ARE ADDING, MATCH"
                    Print "THE EXACT SPELLING OF THE NAME OF THE SAME PLAYER IN THE CAREER FILE FOR THE"
                    Print "PLAYERS THAT HAD PLAYED IN PREVIOUS YEARS.  EXAMPLE - IF BILLY WILLIAMS IS"
                    Print "LISTED AS 'WILLIAMS' IN THE CAREER FILE AND THE SEASON STATS HAD HIM AS"
                    Print "'B.WILLIAMS', HE SHOULD BE CHANGED TO 'WILLIAMS' IN THE SEASON FILE BEFORE"
                    Print "MERGING."
                    Print: Print
                    Color 12: Locate , 25: Print "DO YOU WISH TO CONTINUE? (Y/N)"
                    2018 I$ = GetKeyPress$
                    If UCase$(I$) = "Y" Then Call DoTheMerge        
            
                Case 3, 4: Call CARVIEW
                Case 5: Call ViewCareerRecords
                Case 6: Cls
            End Select
        End If
        
    Loop Until SELECTION% = 6 OR EXITCODE% = 27

End Sub


'------------------------------
'   CreateCareerFile Subroutine
'------------------------------
' ...explanation...
Sub CreateCareerFile ()

    TMR = 945

    Dim A$(62)
    Dim A(62, 14)
    Dim W0(62), W1(62)
    Dim Z(15), Z1(15)

    Shared AR$()
    Shared ars(), W0S(), W1S()
    
    Cls

    Color 14: Locate 5, 27: Print " - - - C A U T I O N - - -"
    Color 15: Locate 8, 18: Print "THIS OPTION SHOULD ONLY BE USED WHEN CREATING"
    Locate , 18: Print "AND MERGING THE INITIAL STAT FILE.  IF YOU ARE"
    Locate , 18: Print "ADDING AN ADDITIONAL YEAR TO AN EXISTING FILE,"
    Locate , 18: Print "            USE OPTION #2."
    Print
    Print
    Print
    Color 12: Locate , 25: Print "DO YOU WISH TO CONTINUE? (Y/N)"
    I$ = GetKeyPress$
    If UCase$(I$) = "Y" Then

        Cls
        For J = 15 To 18
            Locate J
            Print Space$(68)
        Next

        Color 11: Locate 15, 5: Input "ENTER NAME OF CAREER STAT FILE (MAX. - 8 CHARACTERS)"; Z$
        Call DiskSelection_CAREER (B$)
        'B$ = A$

        For XX = 4 To 11
            If Mid$(B$, XX, 1) = " " Or Mid$(B$, XX, 1) = "." Then Mid$(B$, XX, 1) = "_"
        Next XX
        
        For XX = 12 To 14
            If Mid$(B$, XX, 2) = "ST" Then Mid$(B$, 9, 1) = "_": Mid$(B$, 10, 2) = "ST"
        Next XX
        QQ = 8
        For XX = 11 To 4 Step -1
            If Not (Mid$(B$, XX, 1) = "_") Then 
                Exit For
            Else
                QQ = XX - 4
            End If
        Next XX

        Z1$ = Mid$(B$, 4, QQ) + "." + Left$(B$, 2)

        Cls
        Locate 8, 25: Input "ENTER YEAR TO ADD (EX. - 1961)"; Y
        Locate 9, 25: Print "ANY CHANGE (YN)"
        I$ = GetKeyPress$
        If UCase$(I$) = "N" Then

            VV = 1: Locate , 5: Print "IS THIS THE FINAL UPDATE FOR THE CURRENT SEASON? (Y/N)"
            I$ = GetKeyPress$

            If UCase$(I$) = "Y" Then JX = 1
            If UCase$(I$) = "N" Then JX = 0

            If Not _FileExists(diskPaths$(1) + Z1$) Then 
                Cls
                Color 7: Print "UNABLE TO LOCATE STAT FILE . . . PLEASE RE-ENTER"
                Print
                Line Input "HIT ENTER TO CONTINUE"; DUMMY$
            Else

                Open diskPaths$(1) + Z1$ For Input As #1
                Input #1, B$
                For I = 0 To 13: Input #1, A$(I)
                    For J = 0 To 13: Input #1, A(I, J): Next: Input #1, W0(I)
                    Input #1, W1(I)
                    Locate 21, 38: Color 5: TMR = TMR - 1
                    Print Using "####"; TMR
                Next
                For L = 0 To 15: Input #1, Z(L): Next
                For L = 0 To 15: Input #1, Z1(L): Next
                Close #1
                For I = 0 To 61
                    N = 1
                    If A$(I) = "END" Or A$(I) = "XXX" Or A$(I) = "" Then 404
                    NB = NB + 1: GoTo 408

                    404 A$(I) = ""

                    408 For J = 0 To 13: ars(N, I, J) = A(I, J)
                    Next J
                    ars(N, I, 14) = 1: ars(N, I, 15) = Y
                    W0S(N, I) = W0(I): W1S(N, I) = W1(I)
                Next I
                For I = 0 To 61: AR$(I) = A$(I): Next

                'Are 0/1 the only values this can have?
                If JX = 1 Then Call MergeCareerFile (Z$)

                FOR J = 20 TO 22
                    LOCATE J
                    PRINT Space$(69)
                NEXT    

                Locate 20, 20
                Color 15: Print "MERGING CAREER STATS . . . . PLEASE WAIT"

                Open diskPaths$(1) + Z$ + ".CAR" For Output As #2
                Write #2, NB, B$
                For N = 1 To 15
                    For I = 0 To 61
                        Write #2, AR$(I)
                        For J = 0 To 15
                            Write #2, ars(N, I, J)
                        Next J
                        Write #2, W0S(N, I)
                        Write #2, W1S(N, I)
                        Locate 21, 38
                        TMR = TMR - 1
                        Color 5: Print Using "####"; TMR
                    Next I
                Next N
                Close #2
            
            End If 'Done checking for file

        End If 'If we continue the second prompt
        
    End If 'If we continue the first prompt
    
End Sub


'------------------------------
'   MergeCareerFile Subroutine
'------------------------------
' ...explanation...
Sub MergeCareerFile (targetFile$)

    Dim BL$(21)
    Dim BLYR(21), BL(21)

    Shared AR$()
    Shared ars()

    If _FileExists(diskPaths$(1) + targetFile$ + ".cld") Then
        Open diskPaths$(1) + targetFile$ + ".CLD" For Input As #1
        For I = 0 To 20
        Input #1, BLYR(I), BL$(I), BL(I): Next
        Close #1
    End If

    2100 For I = 0 To 61
        If AR$(I) = "" Or AR$(I) = "XXX" Or AR$(I) = "END" Then 2360
        For J = 1 To 15
            If ars(J, I, 15) = 0 Then 2350
            If ars(J, I, 1) <= BL(0) Then 2113
            BL(0) = ars(J, I, 1): BL$(0) = AR$(I): BLYR(0) = ars(J, I, 15)

            2113 If ars(J, I, 2) <= BL(1) Then 2123
            BL(1) = ars(J, I, 2): BL$(1) = AR$(I): BLYR(1) = ars(J, I, 15)

            2123 If ars(J, I, 1) = 0 Or ars(J, I, 1) / ars(J, I, 0) < 3.65 Then 2133
            If ars(J, I, 2) / ars(J, I, 1) <= BL(2) Then 2133
            BL(2) = ars(J, I, 2) / ars(J, I, 1): BL$(2) = AR$(I): BLYR(2) = ars(J, I, 15)

            2133 If ars(J, I, 4) <= BL(3) Then 2143
            BL(3) = ars(J, I, 4): BL$(3) = AR$(I): BLYR(3) = ars(J, I, 15)

            2143 If ars(J, I, 3) <= BL(4) Then 2153
            BL(4) = ars(J, I, 3): BL$(4) = AR$(I): BLYR(4) = ars(J, I, 15)

            2153 If ars(J, I, 3) = 0 Or ars(J, I, 3) / ars(J, I, 0) < 1.5 Then 2163
            If ars(J, I, 4) / ars(J, I, 3) <= BL(5) Then 2163
            BL(5) = ars(J, I, 4) / ars(J, I, 3): BL$(5) = AR$(I): BLYR(5) = ars(J, I, 15)

            2163 If W0S(J, I) <= BL(6) Then 2173

            2164 BL(6) = W0S(J, I): BL$(6) = AR$(I): BLYR(6) = ars(J, I, 15)

            2173 If W1S(J, I) <= BL(7) Then 2182
            BL(7) = W1S(J, I): BL$(7) = AR$(I): BLYR(7) = ars(J, I, 15)

            2182 If W1S(J, I) = 0 Or W1S(J, I) / ars(J, I, 0) < .3 Then 2193
            If W0S(J, I) / W1S(J, I) <= BL(8) Then 2193
            BL(8) = W0S(J, I) / W1S(J, I): BL$(8) = AR$(I): BLYR(8) = ars(J, I, 15)

            2193 If ars(J, I, 2) * 2 + ars(J, I, 4) + W0S(J, I) <= BL(9) Then 2203
            BL(9) = ars(J, I, 2) * 2 + ars(J, I, 4) + W0S(J, I): BL$(9) = AR$(I): BLYR(9) = ars(J, I, 15)

            2203 If ars(J, I, 0) = 0 Then 2213
            If (ars(J, I, 2) * 2 + ars(J, I, 4) + W0S(J, I)) / ars(J, I, 0) <= BL(10) Then 2213
            BL(10) = (ars(J, I, 2) * 2 + ars(J, I, 4) + W0S(J, I)) / ars(J, I, 0): BL$(10) = AR$(I): BLYR(10) = ars(J, I, 15)

            2213 If ars(J, I, 12) <= BL(11) Then 2223
            BL(11) = ars(J, I, 12): BL$(11) = AR$(I): BLYR(11) = ars(J, I, 15)

            2223 If ars(J, I, 5) - ars(J, I, 12) <= BL(12) Then 2233
            BL(12) = ars(J, I, 5) - ars(J, I, 12): BL$(12) = AR$(I): BLYR(12) = ars(J, I, 15)

            2233 If ars(J, I, 5) <= BL(13) Then 2243
            BL(13) = ars(J, I, 5): BL$(13) = AR$(I): BLYR(13) = ars(J, I, 15)

            2243 If ars(J, I, 0) = 0 Then 2253
            If ars(J, I, 5) / ars(J, I, 0) <= BL(14) Then 2253
            BL(14) = ars(J, I, 5) / ars(J, I, 0): BL$(14) = AR$(I): BLYR(14) = ars(J, I, 15)

            2253 If ars(J, I, 6) <= BL(15) Then 2263
            BL(15) = ars(J, I, 6): BL$(15) = AR$(I): BLYR(15) = ars(J, I, 15)

            2263 If ars(J, I, 8) <= BL(16) Then 2273
            BL(16) = ars(J, I, 8): BL$(16) = AR$(I): BLYR(16) = ars(J, I, 15)

            2273 If ars(J, I, 9) <= BL(17) Then 2283
            BL(17) = ars(J, I, 9): BL$(17) = AR$(I): BLYR(17) = ars(J, I, 15)

            2283 If ars(J, I, 7) <= BL(18) Then 2293
            BL(18) = ars(J, I, 7): BL$(18) = AR$(I): BLYR(18) = ars(J, I, 15)

            2293 If ars(J, I, 11) <= BL(19) Then 2303
            BL(19) = ars(J, I, 11): BL$(19) = AR$(I): BLYR(19) = ars(J, I, 15)

            2303 If ars(J, I, 13) <= BL(20) Then 2350
            BL(20) = ars(J, I, 13): BL$(20) = AR$(I): BLYR(20) = ars(J, I, 15)

        2350 Next J

    2360 Next I
    
    'If VV = 1 Then Open diskPaths$(1) + targetFile$ + ".CLD" For Output As #1
    'If VV = 0 Then Open diskPaths$(1) + targetFile$ + ".CLD" For Output As #1
    Open diskPaths$(1) + targetFile$ + ".CLD" For Output As #1
        For I = 0 To 20
            Write #1, BLYR(I), BL$(I), BL(I)
        Next
    Close #1

End Sub


'------------------------------
'   ViewCareerRecords Subroutine
'------------------------------
' ...explanation...
Sub ViewCareerRecords ()

    Dim BL$(21)
    Dim BLYR(21), BL(21)

    Do
    'Display list of files...

        3000 Spec$ = diskPaths$(1) + "*.CLD"
        null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
        Count% = DirCount%

        If Count% = 0 Then
            'GoSub 4600
            'This was a box around which text was printed
            'I've replaced it with CSMsgBox instead for GUI consistency
            Call CSMsgBox("NO CAREER FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        Else

            ReDim conferenceLeaderFiles$(0 To Count%)
            For X = 1 To Count%
                conferenceLeaderFiles$(X) = Space$(12)
            Next

            conferenceLeaderFiles$(0) = Spec$
            Call ReadFile(conferenceLeaderFiles$())
            'CALL SortStr(VARPTR(conferenceLeaderFiles$(1)), Count%, 0)

            Do
                1310 Cls
                Color , 1
                Cls
                Locate , 28
                Color 14
                Print "SELECT FILE TO VIEW": Color 15
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
                Print "-SELECTS FILE";

                _MouseShow "DEFAULT"
                MAMenu conferenceLeaderFiles$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                If EXITCODE% = 27 Then Exit Sub

                targetFile$ = conferenceLeaderFiles$(SELECTION%)
                Locate 2, 33: Print targetFile$
                Locate , 33: Print "ANY CHANGE (YN)";
                Color 15, 0
                'If UCase$(I$) = "Y" Then 1310
            Loop Until UCase$(I$) = "N"

            targetFile$ = RTrim$(targetFile$)
            ZZ$ = Left$(targetFile$, Len(targetFile$) - 4)

            Open diskPaths$(1) + ZZ$ + ".CLD" For Input As #1
            For I = 0 To 20: Input #1, BLYR(I), BL$(I), BL(I): Next
            Close #1

            Locate 15, 10
            Color 12: Print "V";: Color 7: Print "iew or ";
            Color 12: Print "P";: Color 7: Print "rint"

            Do
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "V" Or UCase$(I$) = "P"

            Select Case UCase$(I$)
                Case "V":
                    Do
                        Cls
                        Color 12: Print "CATEGORY                     RECORD           PLAYER              YEAR"
                        Print
                        Color 3: Print "FIELD GOALS MADE";: Color 7: Print Tab(33); Using "####"; BL(0);: Print Tab(47); BL$(0);: Print Tab(66); BLYR(0)
                        Color 3: Print "FIELD GOAL ATTEMPTS";: Color 7: Print Tab(33); Using "####"; BL(1);: Print Tab(47); BL$(1);: Print Tab(66); BLYR(1)
                        Color 3: Print "FIELD GOAL PCT.";: Color 7: Print Tab(32); Using "#.###"; BL(2);: Print Tab(47); BL$(2);: Print Tab(66); BLYR(2)
                        Color 3: Print "FREE THROWS MADE";: Color 7: Print Tab(33); Using "####"; BL(3);: Print Tab(47); BL$(3);: Print Tab(66); BLYR(3)
                        Color 3: Print "FREE THROW ATTEMPTS";: Color 7: Print Tab(33); Using "####"; BL(4);: Print Tab(47); BL$(4);: Print Tab(66); BLYR(4)
                        Color 3: Print "FREE THROW PCT.";: Color 7: Print Tab(32); Using "#.###"; BL(5);: Print Tab(47); BL$(5);: Print Tab(66); BLYR(5)
                        Color 3: Print "3 PT. FIELD GOALS MADE";: Color 7: Print Tab(33); Using "####"; BL(6);: Print Tab(47); BL$(6);: Print Tab(66); BLYR(6)
                        Color 3: Print "3 PT. FILED GOAL ATTEMPTS";: Color 7: Print Tab(33); Using "####"; BL(7);: Print Tab(47); BL$(7);: Print Tab(66); BLYR(7)
                        Color 3: Print "3 PT. FIELD GOAL PCT.";: Color 7: Print Tab(32); Using "#.###"; BL(8);: Print Tab(47); BL$(8);: Print Tab(66); BLYR(8)
                        Color 3: Print "POINTS";: Color 7: Print Tab(32); Using "#####"; BL(9);: Print Tab(47); BL$(9);: Print Tab(66); BLYR(9)
                        Color 3: Print "POINTS PER GAME";: Color 7: Print Tab(32); Using "###.#"; BL(10);: Print Tab(47); BL$(10);: Print Tab(66); BLYR(10)
                        Color 3: Print "OFFENSIVE REBOUNDS";: Color 7: Print Tab(32); Using "#####"; BL(11);: Print Tab(47); BL$(11);: Print Tab(66); BLYR(11)
                        Color 3: Print "DEFENSIVE REBOUNDS";: Color 7: Print Tab(32); Using "#####"; BL(12);: Print Tab(47); BL$(12);: Print Tab(66); BLYR(12)
                        Color 3: Print "TOTAL REBOUNDS";: Color 7: Print Tab(32); Using "#####"; BL(13);: Print Tab(47); BL$(13);: Print Tab(66); BLYR(13)
                        Color 3: Print "REBOUNDS PER GAME";: Color 7: Print Tab(32); Using "###.#"; BL(14);: Print Tab(47); BL$(14);: Print Tab(66); BLYR(14)
                        Color 15: Locate 22: Print "HIT ANY KEY TO CONTINUE"
                        I$ = GetKeyPress$

                        Cls
                        Color 12: Print "CATEGORY                     RECORD           PLAYER              YEAR"
                        Print
                        Color 3: Print "PERSONAL FOULS";: Color 7: Print Tab(33); Using "####"; BL(15);: Print Tab(47); BL$(15);: Print Tab(66); BLYR(15)
                        Color 3: Print "ASSISTS";: Color 7: Print Tab(32); Using "#####"; BL(16);: Print Tab(47); BL$(16);: Print Tab(66); BLYR(16)
                        Color 3: Print "SHOTS BLOCKED";: Color 7: Print Tab(33); Using "####"; BL(17);: Print Tab(47); BL$(17);: Print Tab(66); BLYR(17)
                        Color 3: Print "STEALS";: Color 7: Print Tab(32); Using "#####"; BL(18);: Print Tab(47); BL$(18);: Print Tab(66); BLYR(18)
                        Color 3: Print "TURNOVERS";: Color 7: Print Tab(32); Using "#####"; BL(19);: Print Tab(47); BL$(19);: Print Tab(66); BLYR(19)
                        Color 3: Print "HI-GAME (POINTS)";: Color 7: Print Tab(33); Using "####"; BL(20);: Print Tab(47); BL$(20);: Print Tab(66); BLYR(20)
                        Color 15: Locate 22: Print "VIEW AGAIN (Y/N)"
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "N"

                Case "P":
                    LPrint "CATEGORY                     RECORD           PLAYER              YEAR"
                    LPrint
                    LPrint "FIELD GOALS MADE";: LPrint Tab(33); Using "####"; BL(0);: LPrint Tab(47); BL$(0);: LPrint Tab(66); BLYR(0)
                    LPrint "FIELD GOAL ATTEMPTS";: LPrint Tab(33); Using "####"; BL(1);: LPrint Tab(47); BL$(1);: LPrint Tab(66); BLYR(1)
                    LPrint "FIELD GOAL PCT.";: LPrint Tab(32); Using "#.###"; BL(2);: LPrint Tab(47); BL$(2);: LPrint Tab(66); BLYR(2)
                    LPrint "FREE THROWS MADE";: LPrint Tab(33); Using "####"; BL(3);: LPrint Tab(47); BL$(3);: LPrint Tab(66); BLYR(3)
                    LPrint "FREE THROW ATTEMPTS";: LPrint Tab(33); Using "####"; BL(4);: LPrint Tab(47); BL$(4);: LPrint Tab(66); BLYR(4)
                    LPrint "FREE THROW PCT.";: LPrint Tab(32); Using "#.###"; BL(5);: LPrint Tab(47); BL$(5);: LPrint Tab(66); BLYR(5)
                    LPrint "3 PT. FIELD GOALS MADE";: LPrint Tab(33); Using "####"; BL(6);: LPrint Tab(47); BL$(6);: LPrint Tab(66); BLYR(6)
                    LPrint "3 PT. FILED GOAL ATTEMPTS";: LPrint Tab(33); Using "####"; BL(7);: LPrint Tab(47); BL$(7);: LPrint Tab(66); BLYR(7)
                    LPrint "3 PT. FIELD GOAL PCT.";: LPrint Tab(32); Using "#.###"; BL(8);: LPrint Tab(47); BL$(8);: LPrint Tab(66); BLYR(8)
                    LPrint "POINTS";: LPrint Tab(32); Using "#####"; BL(9);: LPrint Tab(47); BL$(9);: LPrint Tab(66); BLYR(9)
                    LPrint "POINTS PER GAME";: LPrint Tab(32); Using "###.#"; BL(10);: LPrint Tab(47); BL$(10);: LPrint Tab(66); BLYR(10)
                    LPrint "OFFENSIVE REBOUNDS";: LPrint Tab(32); Using "#####"; BL(11);: LPrint Tab(47); BL$(11);: LPrint Tab(66); BLYR(11)
                    LPrint "DEFENSIVE REBOUNDS";: LPrint Tab(32); Using "#####"; BL(12);: LPrint Tab(47); BL$(12);: LPrint Tab(66); BLYR(12)
                    LPrint "TOTAL REBOUNDS";: LPrint Tab(32); Using "#####"; BL(13);: LPrint Tab(47); BL$(13);: LPrint Tab(66); BLYR(13)
                    LPrint "REBOUNDS PER GAME";: LPrint Tab(32); Using "###.#"; BL(14);: LPrint Tab(47); BL$(14);: LPrint Tab(66); BLYR(14)
                    LPrint "PERSONAL FOULS";: LPrint Tab(33); Using "####"; BL(15);: LPrint Tab(47); BL$(15);: LPrint Tab(66); BLYR(15)
                    LPrint "ASSISTS";: LPrint Tab(32); Using "#####"; BL(16);: LPrint Tab(47); BL$(16);: LPrint Tab(66); BLYR(16)
                    LPrint "SHOTS BLOCKED";: LPrint Tab(33); Using "####"; BL(17);: LPrint Tab(47); BL$(17);: LPrint Tab(66); BLYR(17)
                    LPrint "STEALS";: LPrint Tab(32); Using "#####"; BL(18);: LPrint Tab(47); BL$(18);: LPrint Tab(66); BLYR(18)
                    LPrint "TURNOVERS";: LPrint Tab(32); Using "#####"; BL(19);: LPrint Tab(47); BL$(19);: LPrint Tab(66); BLYR(19)
                    LPrint "HI-GAME (POINTS)";: LPrint Tab(33); Using "####"; BL(20);: LPrint Tab(47); BL$(20);: LPrint Tab(66); BLYR(20)
                    LPrint Chr$(12)

            End Select

        End If 'End checking for files

    '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0

End Sub


'----------------------------------------
'     DiskSelection_CAREER Subroutine
'----------------------------------------
' ...explanation...
Sub DiskSelection_CAREER (B$)

    Do
        Color 15, 0: Cls
        JJ = 6
        SN = 1
        Locate 6, 8: Color 15, 4
        'Print box top
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

        'Print box sides
        For II = 1 To JJ
        Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
        Next

        'Print box bottom
        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

        Locate 7, 32
        Locate , 12: Print "ENTER DISK ID: ": Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Print
        Locate , 12: Print "ESC TO RETURN TO MAIN MENU"
        I$ = GetKeyPress$
        If I$ <> Chr$(27) Then
            If I$ = Chr$(13) Then 
                Call SelectTeam_CAREER (YN$, NTMS, B$)
            Else
                Locate 7, 27: Print I$
                Locate 7, 28: Line Input NY$
                NY$ = I$ + NY$
                Color 15, 0

                If NY$ = "" Then 
                    Call SelectTeam_CAREER (NY$, NTMS, B$)
                Else
                    YN$ = NY$
                    ERASE teamNames$

                    1100 Open diskPaths$(0) + "BASK." + YN$ For Random As #1 Len = 655
                    Y1 = LOF(1)
                    Field #1, 15 As Q$(0), 640 As X$
                    For I1 = 1 To Y1 / 655
                        Get #1, I1
                        teamNames$(I1) = Q$(0): teamIndex%(I1) = I1
                    Next
                    Close 1
                    NTMS = Y1 / 655

                    Call SelectTeam_CAREER (YN$, NTMS, B$)
                
                End If
                
            End If
            
            If BO% = 1 Then 
                BO% = 0
            Else
                Locate 2, 33: Color T%(0), T%(1): Print A$
                Color 15, 4: Locate , 33: Print "ANY CHANGE (YN)";
                I$ = GetKeyPress$
            End If

        End If 'Done checking if ESC was pressed
        
    Loop Until UCase$(I$) = "N" or EXITCODE% = 27

    Color 15, 0

End Sub


'------------------------------
'    DoTheMerge Subroutine
'------------------------------
' ...explanation...
Sub DoTheMerge ()

    TMR1 = 1875
    NB = -1

    Dim A$(62)
    Dim A(62, 14)
    Dim W0(62), W1(62)
    Dim Z(15), Z1(15)

    Shared AR$()
    Shared ars(), W0S(), W1S()

    1150 Spec$ = diskPaths$(1) + "*.CAR"
    null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
    Count% = DirCount%
    If Count% = 0 Then 
        'GoSub 4600
        'This was a box around which text was printed
        'I've replaced it with CSMsgBox instead for GUI consistency
        Call CSMsgBox("NO CAREER FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)
        Cls
    Else
        ReDim careerFiles$(0 To Count%)
        For X = 1 To Count%
            careerFiles$(X) = Space$(12)
        Next
        careerFiles$(0) = Spec$
        Call ReadFile(careerFiles$())
        'CALL SortStr(VARPTR(careerFiles$(1)), Count%, 0)

        Do

            Cls
            Color , 1
            Cls
            Locate , 28
            Color 14
            Print "SELECT FILE TO UPDATE": Color 15
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
            Print "-SELECTS TEAM LIBRARY";

            _MouseShow "DEFAULT"
            MAMenu careerFiles$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
            _MouseHide

            If EXITCODE% = 27 Then Exit Sub

            targetFile$ = careerFiles$(SELECTION%)
            Locate 2, 33: Print targetFile$
            Locate , 33: Print "ANY CHANGE (YN)";
            Color 15, 0
            I$ = GetKeyPress$

        Loop Until UCase$(I$) = "N"

        targetFile$ = RTrim$(targetFile$)
        Z$ = Left$(targetFile$, Len(targetFile$) - 4)
        Call DiskSelection_CAREER (B$)
        'B$ = A$

        For XX = 4 To 11: If Mid$(B$, XX, 1) = " " Or Mid$(B$, XX, 1) = "." Then Mid$(B$, XX, 1) = "_"
        Next XX
        For XX = 12 To 14: If Mid$(B$, XX, 2) = "ST" Then Mid$(B$, 9, 1) = "_": Mid$(B$, 10, 2) = "ST"
        Next XX
        QQ = 8
        For XX = 11 To 4 Step -1
            If Not (Mid$(B$, XX, 1) = "_") Then 
                Exit For
            Else
                QQ = XX - 4
            End If
        Next XX

        Z1$ = Mid$(B$, 4, QQ) + "." + Left$(B$, 2)

        Locate 8, 25: Input "ENTER YEAR TO ADD (EX. - 1961)"; Y
        Locate 9, 25: Print "ANY CHANGE (YN)"
        I$ = GetKeyPress$
        If UCase$(I$) = "N" Then
        
            VV = 0: Locate , 5: Print "IS THIS THE FINAL UPDATE FOR THE CURRENT SEASON? (Y/N)"
            I$ = GetKeyPress$
            If UCase$(I$) = "Y" Then JX = 1
            If UCase$(I$) = "N" Then JX = 0

            Locate 20, 5: Color 15: Print "MERGING CAREER STATS AND UPDATING CAREER LEADER FILE . . . . PLEASE WAIT"
            Open diskPaths$(1) + Z$ + ".CAR" For Input As #2
            Input #2, NB, B$
            For N = 1 To 15
                For I = 0 To 61: Input #2, AR$(I)
                    For J = 0 To 15: Input #2, ars(N, I, J): Next J: Input #2, W0S(N, I)
                    Input #2, W1S(N, I): Locate 21, 38: Color 5: TMR1 = TMR1 - 1
            Print Using "####"; TMR1: Next I: Next N
            Close #2

            Locate 20, 20: Color 15: Print "MERGING CAREER STATS . . . . PLEASE WAIT"
            If Not _FileExists(diskPaths$(1) + Z1$) Then 
                Cls
                Color 7: Print "UNABLE TO LOCATE STAT FILE . . . PLEASE RE-ENTER"
                Print
                Line Input "HIT ENTER TO CONTINUE"; DUMMY$
                Exit Sub
            End If

            Open diskPaths$(1) + Z1$ For Input As #1
            Input #1, B$
            For I = 0 To 13: Input #1, A$(I)
            For J = 0 To 13: Input #1, A(I, J): Next: Input #1, W0(I): Input #1, W1(I): Locate 21, 38: Color 5: TMR1 = TMR1 - 1: Print Using "####"; TMR1: Next
            For L = 0 To 15: Input #1, Z(L): Next
            For L = 0 To 15: Input #1, Z1(L): Next
            Close #1
            For I = 0 To 13
                RR = 0
                CHK = 0
                For J = 0 To 61

                    If AR$(J) = A$(I) Then

                        For W = 1 To 15
                            If ars(W, J, 15) = Y Then
                                CHK = CHK + 1
                            End If
                        Next W

                        If CHK > 0 Then
                            N = ars(1, J, 14) + ars(2, J, 14) + ars(3, J, 14) + ars(4, J, 14)
                            N = N + ars(5, J, 14) + ars(6, J, 14) + ars(7, J, 14) + ars(8, J, 14)
                            N = N + ars(9, J, 14) + ars(10, J, 14) + ars(11, J, 14)
                            N = N + ars(12, J, 14) + ars(13, J, 14) + ars(14, J, 14) + ars(15, J, 14)
                        Else
                            N = ars(1, J, 14) + ars(2, J, 14) + ars(3, J, 14) + ars(4, J, 14)
                            N = N + ars(5, J, 14) + ars(6, J, 14) + ars(7, J, 14) + ars(8, J, 14)
                            N = N + ars(9, J, 14) + ars(10, J, 14) + ars(11, J, 14)
                            N = N + ars(12, J, 14) + ars(13, J, 14) + ars(14, J, 14) + ars(15, J, 14) + 1
                        End If

                        For K = 0 To 13
                            ars(N, J, K) = A(I, K)
                        Next K
                        ars(N, J, 14) = 1: ars(N, J, 15) = Y
                        W0S(N, J) = W0(J): W1S(N, J) = W1(J)
                        'GoTo 1430
                    Else
                        RR = RR + 1
                        If RR = 62 Then
                            If A$(I) <> "END" And  A$(I) <> "XXX" And A$(I) <> "" Then
                                NB = NB + 1
                                AR$(NB) = A$(I)
                                N = 1
                                For K = 0 To 13: ars(N, NB, K) = A(I, K): Next
                                ars(N, NB, 14) = 1: ars(N, NB, 15) = Y
                                W0S(N, NB) = W0(NB): W1S(N, NB) = W1(NB)
                            End If

                        End If

                    End If

                Next J

            Next I
            'GoTo 1700

            'Are 0/1 the only values this can have?
            If JX = 1 Then Call MergeCareerFile (Z$)

            Locate 20, 20: Color 15: Print "MERGING CAREER STATS . . . . PLEASE WAIT"
            Open diskPaths$(1) + Z$ + ".CAR" For Output As #2
            Write #2, NB, B$
            For N = 1 To 15
                For I = 0 To 61: Write #2, AR$(I)
            For J = 0 To 15: Write #2, ars(N, I, J): Next J: Write #2, W0S(N, I): Write #2, W1S(N, I): Locate 21, 38: Color 5: TMR1 = TMR1 - 1: Print Using "####"; TMR1: Next I: Next N
            Close #2

        End If 'If we chose to continue

    End If 'If we had files to process
    
End Sub


'----------------------------------------
'   SelectTeam_CAREER subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_CAREER (yearNumber$, numberTeams, B$)

    Dim L%(4), T%(34)

    SELECTION% = 1
    Count% = numberTeamsMS
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
    Print "EnumberTeamsER";: Color 15: Print "-SELECTS TEAM";
        
    _MouseShow "DEFAULT"
    MAMenu teamNames$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    If EXITCODE% = 27 Then BO% = 1: Exit Sub

    T1 = teamIndex%(SELECTION%)

    Open diskPaths$(0) + "BASK." + yearNumber$ For Random As #1 Len = 655
    
    FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 2 AS Q$(16), 2 AS Q$(17), 2 AS Q$(18), 2 AS Q$(19), 602 AS X$
    
    For I = 0 To 13
        FIELD #1, 53 + I * 43 AS X$, 15 AS Q$(I * 15 + 20), 2 AS Q$(I * 15 + 21), 2 AS Q$(I * 15 + 22), 2 AS Q$(I * 15 + 23), 2 AS Q$(I * 15 + 24), 2 AS Q$(I * 15 + 25), 2 AS Q$(I * 15 + 26), 2 AS Q$(I * 15 + 27), 2 AS Q$(I * 15 + 28), 2 AS Q$(I * 15 + 29), 2 AS Q$(I * 15 + 30), 2 AS Q$(I * 15 + 31), 2 AS Q$(I * 15 + 32), 2 AS Q$(I * 15 + 33), 2 AS Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 AS X$
    Next
    
    Get #1, T1

    A$ = Q$(0)

    While Right$(A$, 1) = " "
        A$ = Left$(A$, Len(A$) - 1)
    Wend

    B$ = A$
    SP = CVI(Q$(1))
    DA = CVI(Q$(2))
    V1% = CVI(Q$(3))
    V2% = CVI(Q$(4))
    For I = 0 To 4: L%(I) = CVI(Q$(I + 5)): Next
    For I = 0 To 9: T%(I) = CVI(Q$(I + 10)): Next
    Close 1

End Sub
