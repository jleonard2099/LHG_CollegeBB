Sub MERGE ()

    _Title "Courtside College Basketball - Merge Stats"

    'Dim AR$(1), G$(4), G%(4, 10), B%(26, 11)
    'Dim TR%(12), GR$(4), GR%(4, 10), BR$(26), BR%(26, 11)
    'Dim H1(100), O1$(100), V1(100), L1$(100), WG1$(100), LG1$(100), P1(100)
    'Dim H(100), V(100), L$(100), WG$(100), LG$(100), P(100)
    'Dim A%(17), APR%(1, 120), CRDR(120), SITER$(120)
    'Dim P%(100), Z2(14, 14), PRD%(100), ZRD2(14, 14)

    '-----
    ReDim menuChoices$(1 To 3)

    Do

        Cls

    Call TITLE_MERGE
    menuChoices$(1) = "MERGE STATISTICAL FILES           "
    menuChoices$(2) = "ALIGN STATISTICAL FILES           "
    menuChoices$(3) = "EXIT TO MAIN MENU                 "
    SELECTION% = 1
    Count% = 3
    Start% = 1
    EXITCODE% = 3
    FileFGColr% = 15: FileBGColr% = 1
    HiLiteFGColr% = 0: HiLiteBGColr% = 7
    Spacing% = 1
    Row% = 8
    Column% = 25

    CALL box0(7, 24, 11, 59, 2,  15, 1)
    _MouseShow "DEFAULT"
    MMenuVert menuChoices$(), SELECTION%, Start%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
    _MouseHide

    Color , 0

    If Not(EXITCODE% = 27) Then
        Select Case SELECTION%
            Case 1: Call MergeIt
            Case 2: CALL ALIGN
            Case 3: Cls
        End Select
    End If

Loop Until SELECTION% = 3 OR EXITCODE% = 27

End Sub


'----------------------------------------
'       MergeIt Subroutine
'----------------------------------------
Sub MergeIt ()

    'Dim A$(14)
    Dim ARP$(14), mergeB$(26), mergeH$(100), HRP$(100)
    Dim O$(100), ORP$(100), SITE$(100), SITERP$(100)

    Dim AP%(100, 1), APRD%(100, 1)
    Dim O%(100), ORD%(100), TRD%(100)

    Dim mergeA(14, 14), ARD(14, 14), CRD(100), CRDRD(100)
    Dim W0RD(15), W1RD(15), ZRD(15), ZRD1(15)

    Shared A$()
    Shared L%(), mergeT%()
    Shared W0(), W1(), Z(), Z1()

    14 Color 12, 0
    Cls
    Locate 2, 5: Print "ENTER COMPLETE PATH FOR TEAM STATS TO MERGE"

    23 Color 15
    Locate 4, 5: Print "EXAMPLE - A:"

    24 Locate , 5: Print "          C:\HAFFNER"

    25 Locate , 5: Print "          C:\HAFFNER\1980"

    26 Locate , 5: Print "          D:\BASKET"

    27 For J = 10 To 13: Locate J

    28 Print Space$(60): Next

    35 Color 10: Locate 10, 5: Line Input "ROAD STATS:     "; PR$

    36 Locate , 5: Locate , 5: Line Input "CURRENT STATS:  "; PC$

    37 Color 14: Locate 13, 5: Print "ANY CHANGE (Y/N)"
    38 I$ = GetKeyPress$

    39 If UCase$(I$) = "Y" Then 27

    Do
        3056 Color 15, 0: Cls
        JJ = 6
        SN = 1: Locate 6, 8: Color 15, 4
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";: Locate 7, 32
        Locate , 12: Print "ENTER DISK ID: ": Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Print
        Locate , 12: Print "ESC TO RETURN TO MAIN MENU"
        I$ = GetKeyPress$
        If I$ = Chr$(27) Then Z$ = "END": Cls: Exit Sub

        If I$ = Chr$(13) Then 
            Call GetTeamFile_MERGE
        Else
            Locate 7, 27: Print I$
            Locate 7, 28: Line Input NY$
            NY$ = I$ + NY$
            Color 15, 0

            If NY$ = "" Then 
                Call GetTeamFile_MERGE
                'GoTo 3060
            Else
                YN$ = NY$
                ERASE teamNames$
                1101 Open diskPaths$(0) + "BASK." + YN$ For Random As #1 Len = 655
                Y = LOF(1)
                Field #1, 15 As Q$(0), 640 As X$
                For I1 = 1 To Y / 655
                    Get #1, I1
                    teamNames$(I1) = Q$(0): teamIndex%(I1) = I1
                Next
                Close 1
                NTMS = Y / 655

                Call GetTeamFile_MERGE
            End If
        
        End If
        
        3060 If BO% = 1 Then 
            BO% = 0
        Else
            3058 Locate 2, 33: Color mergeT%(0), mergeT%(1): Print A$
            Color 15, 4: Locate , 33: Print "ANY CHANGE (YN)";
            I$ = GetKeyPress$
        End If
        'If UCase$(I$) = "Y" Then 3056

    Loop Until UCase$(I$) = "N"

    Color 15, 0

    B$ = A$

    For XX = 4 To 11
        If Mid$(B$, XX, 1) = " " Or Mid$(B$, XX, 1) = "." Then Mid$(B$, XX, 1) = "_"
    Next XX
    For XX = 12 To 14
        If Mid$(B$, XX, 2) = "ST" Then Mid$(B$, 9, 1) = "_": Mid$(B$, 10, 2) = "ST"
    Next XX
    QQ = 8
    For XX = 11 To 4 Step -1
        If Mid$(B$, XX, 1) = "_" Then 
            QQ = XX - 4
        Else
            Exit For
        End If
    Next XX

    Z$ = Mid$(B$, 4, QQ) + "." + Left$(B$, 2)
    Cls
    Locate 10, 15: Print "ROAD STATS ARE ON DRIVE "; PR$
    Locate , 15: Print "MASTER STATS ARE ON DRIVE "; PC$
    Locate , 15: Print "TEAM TO MERGE STATS FOR IS "; A$
    Locate 14, 15: Print "ANY CHANGE (YN)"
    I$ = GetKeyPress$

    If UCase$(I$) = "N" Then

        For J = 21 To 23
            Locate J
            Print Space$(64)
        Next

        If Not _FileExists(PR$ + "\" + Z$) Then
            Cls
            Locate 2, 10: Color 7: Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
            Print
            Locate , 10: Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$
        Else
            120 Open PR$ + "\" + Z$ For Input As #1

            121 Input #1, B$

            122 For I = 0 To 13: Input #1, ARP$(I)

            123 For J = 0 To 13: Input #1, ARD(I, J): Next: Input #1, W0RD(I): Input #1, W1RD(I): Next

            124 For L = 0 To 15: Input #1, ZRD(L): Next

            125 For L = 0 To 15: Input #1, ZRD1(L): Next

            130 Close #1

            180 'GoTo 500

            500 Open PR$ + "\" + Z$ + "S" For Input As #1

            501 Input #1, YR%

            502 For I = 1 To 100: Input #1, ORD%(I): Next I

            503 For I = 1 To 100: Input #1, ORP$(I): Next I

            504 For I = 1 To 100: Input #1, TRD%(I): Next I

            505 For I = 1 To 100: Input #1, HRP$(I): Next I
            For I = 1 To 100
            For I1 = 0 To 1: Input #1, APRD%(I, I1): Next: Next
            For I = 1 To 100: Input #1, CRDRD(I): Next I
            For I = 1 To 100: Input #1, SITERP$(I): Next I

            506 Close #1

            595 'GoTo 200
            200 'GoSub 1100
            1100 Locate 21, 5: Color 3
            For J = 21 To 23
                Locate J
                1104 Print "                                                                "
            1105 Next


            If Not _FileExists(PC$ + "\" + Z$) Then
                Cls
                Locate 2, 10: Color 7: Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
                Print
                Locate , 10: Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$
            Else
                Open PC$ + "\" + Z$ For Input As #1
                Input #1, B$
                For I = 0 To 13: Input #1, A$(I)
                For J = 0 To 13: Input #1, mergeA(I, J): Next: Input #1, W0(I): Input #1, W1(I): Next
                For L = 0 To 15: Input #1, Z(L): Next
                For L = 0 To 15: Input #1, Z1(L): Next
                Close #1


                Open PR$ + "\" + Z$ + "S" For Input As #1
                Input #1, Y%
                For I = 1 To 100: Input #1, O%(I): Next I
                For I = 1 To 100: Input #1, O$(I): Next I
                For I = 1 To 100: Input #1, mergeT%(I): Next I
                For I = 1 To 100: Input #1, mergeH$(I): Next I
                For I = 1 To 100
                For I1 = 0 To 1: Input #1, AP%(I, I1): Next: Next
                For I = 1 To 100: Input #1, CRD(I): Next I
                For I = 1 To 100: Input #1, SITE$(I): Next I
                Close #1

                SEQ = 0

                For K = 0 To 13

                    If ARP$(K) <> A$(K) Then
                        SEQ = SEQ + 1
                    End If

                Next

                If SEQ = 0 Then
                    For K = 1 To YR%
                        Y% = Y% + 1
                        O%(Y%) = ORD%(K)
                        O$(Y%) = ORP$(K)
                        mergeT%(Y%) = TRD%(K)
                        mergeH$(Y%) = HRP$(K)
                        AP%(Y%, 0) = APRD%(Y%, 0)
                        AP%(Y%, 1) = APRD%(Y%, 1)
                        CRD(Y%) = CRDRD(Y%)
                        SITE$(Y%) = SITERP$(Y%)
                    Next

                    Locate 18, 20: Color 12: Print "MERGING STAT FILE AND SCHEDULE RESULTS . . . ."

                    For I = 0 To 13
                        For J = 0 To 12: mergeA(I, J) = mergeA(I, J) + ARD(I, J): Next J
                        W0(I) = W0(I) + W0RD(I)
                        W1(I) = W1(I) + W1RD(I)
                        If mergeA(I, 13) < ARD(I, 13) Then mergeA(I, 13) = ARD(I, 13) Else mergeA(I, 13) = mergeA(I, 13)
                    Next I

                    For L = 0 To 15: Z(L) = Z(L) + ZRD(L): Z1(L) = Z1(L) + ZRD1(L): Next

                    Open PC$ + "\" + Z$ For Output As #1
                    Write #1, B$
                    For I = 0 To 13: Write #1, A$(I)
                    For J = 0 To 13: Write #1, mergeA(I, J): Next: Write #1, W0(I): Write #1, W1(I): Next
                    For L = 0 To 15: Write #1, Z(L): Next
                    For L = 0 To 15: Write #1, Z1(L): Next
                    Close #1

                    Open PC$ + "\" + Z$ + "S" For Output As #1
                    Write #1, Y%
                    For I = 1 To 100: Write #1, O%(I): Next I
                    For I = 1 To 100: Write #1, O$(I): Next I
                    For I = 1 To 100: Write #1, mergeT%(I): Next I
                    For I = 1 To 100: Write #1, mergeH$(I): Next I
                    For I = 1 To 100
                        For I1 = 0 To 1: Write #1, AP%(I, I1): Next
                    Next
                    For I = 1 To 100: Write #1, CRD(I): Next I
                    For I = 1 To 100: Write #1, SITE$(I): Next I

                    Close #1

                    Y = 0

                    Locate 20, 20: Color 12: Print "RESULTS SUCCESSFULLY MERGED!!"

                    For J = 13 To 23
                        Locate J
                        Print Space$(65)
                    Next

                Else
                    Cls
                    Color 14
                    Locate 5, 27: Print "PLAYERS NOT IN SEQUENCE !!"'; DUMMY$
                    Color 15
                    Locate 7, 20: Print "STATS WILL BE DISTORTED UNLESS THE ROAD"
                    Locate , 19: Print "FILE LISTS THE PLAYERS IN THE SAME SEQUENCE"
                    Locate , 24: Print "AS YOUR CURRENT PLAYER STAT FILE."
                    Locate 11, 24: Color 12: Print "YOU MUST REALIGN ROAD STAT FILE OR"
                    Locate , 31: Print "ENTER STATS MANUALLY."
                    Locate 15, 29: Color 13: Line Input "HIT ENTER TO GO TO MENU"; DUMMY$

                End If 'Done checking SEQ value

            End If 'Done checking if home file exists

        End If 'Done checking if road file exists

    End If 'Done checking if ready to proceed

    '1290 GoTo 5

    '993 Exit Sub

End Sub


'----------------------------------------
'       GetTeamFile_MERGE Subroutine
'----------------------------------------
'...explanation...
Sub GetTeamFile_MERGE ()

    Shared  mergeT%(), L%()


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

    Do
        
        _MouseShow "DEFAULT"
        MAMenu teamNames$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
        _MouseHide

        If EXITCODE% = 27 Then BO% = 1: Exit Sub

    Loop Until EXITCODE% = 13

    T1 = teamIndex%(SELECTION%)

    Open diskPaths$(0) + "BASK." + YN$ For Random As #1 Len = 655
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
    SP = CVI(Q$(1)): DA = CVI(Q$(2)): V1% = CVI(Q$(3)): V2% = CVI(Q$(4))
    For I = 0 To 4: L%(I) = CVI(Q$(I + 5)): Next
    For I = 0 To 9: mergeT%(I) = CVI(Q$(I + 10)): Next
    Close 1
    B$ = A$

END SUB
