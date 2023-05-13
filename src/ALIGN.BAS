Sub ALIGN ()

    _Title "Courtside College Basketball - Align Stats"

    Shared A$()
    Shared alignAR$()

    Shared AR!(), compareA!()
    Shared statsW0!(), statsW1!()
    Shared W0R!(), WR1!()
    Shared Z!(), Z1!()
    Shared ZR!(), ZR1!()

    Do

        Call GetMergePaths (pathCurrentStats$, pathRoadStats$, I$)

        If UCase$(I$) = "N" And I$ <> Chr$(27) Then

            Locate 15, 5: Print Space$(55)
            Locate 16, 5: Print Space$(55)
            Color 11

			Call GetDiskID_ALIGN(A$)

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

            statFile$ = Mid$(B$, 4, QQ) + "." + Left$(B$, 2)

            Color 12, 0
            Cls
            Locate 18, 5: Color 12: Print "SEARCHING FOR FILES . . ."

            If _FileExists(pathRoadStats$ + statFile$) Then

                Open pathRoadStats$ + statFile$ For Input As #1
                Input #1, BR$

                For I = 0 To 13:
                    Input #1, alignAR$(I)
                    For J = 0 To 13: Input #1, AR!(I, J): Next
                    Input #1, W0R!(I)
                    Input #1, WR1!(I)
                Next

                For L = 0 To 15: Input #1, ZR!(L): Next
                For L = 0 To 15: Input #1, ZR1!(L): Next

                Close #1

                If _FileExists(pathCurrentStats$ + statFile$) Then

                    Open pathCurrentStats$ + statFile$ For Input As #1
                    Input #1, B$

                    For I = 0 To 13:
                        Input #1, A$(I)
                        For J = 0 To 13: Input #1, compareA(I, J): Next
                        Input #1, statsW0!(I)
                        Input #1, statsW1!(I)
                    Next

                    For L = 0 To 15: Input #1, Z!(L): Next
                    For L = 0 To 15: Input #1, Z1!(L): Next

                    Close #1

                    Call AlignStats(pathRoadStats$, statFile$)
                    DUMMY$ = "OK"

                Else
                    Cls
                    Locate 2, 10: Color 7
                    Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
                    Print
                    Locate , 10
                    Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$

                End If 'Done checking if current file exists

            Else
                Cls
                Locate 2, 10: Color 7
                Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
                Print
                Locate , 10
                Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$

            End If 'Done checking if road file exists

        End If 'Done checking for ESC

    Loop Until DUMMY$ <> "" Or UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'       AlignStats Subroutine
'----------------------------------------
'...explanation...
Sub AlignStats (pathRoadStats$, statFile$)

    Shared A$()

    Shared alignAR$()
    Shared AR!()
    Shared W0R!(), WR1!()

    Shared AN$()
    Shared AN!()
    Shared W0N!(), WN1!()

    Cls
    Locate 1, 34
    Color 12: Print "STAT ROSTERS"

    Locate , 26
    Color 15: Print "CURRENT  ";
    Color 7: Print "             ROAD"

    For I = 0 To 13:
        Locate I + 4, 22
        Color 12: Print Using "##"; I;: Print " ";
        Color 15: Print A$(I);
        Color 12: Print Tab(44); Using "##"; I;: Print " ";
        Color 7: Print alignAR$(I)
    Next

    Color 11
	Locate 22: Line Input "HIT ENTER TO ALIGN"; DUMMY$
    Color 11
	Locate 22: Print "ALIGNING FILES . . .             "

    N = 0

    Do
        For J = 0 To 13
            If A$(N) = alignAR$(J) Then
                AN$(N) = alignAR$(J)
                For K = 0 To 13
                    AN!(N, K) = AR!(J, K)
                    W0N!(N) = W0R!(J)
                    WN1!(N) = WR1!(J)
                Next K
            End If
        Next J

        N = N + 1

        If N = 15 Then

            Locate 22: Print Space$(53)

            SEQ = 0

            For K = 0 To 13
                If AN$(K) <> A$(K) Then
                    SEQ = SEQ + 1
                End If
            Next

            If SEQ = 0 Then
                Call ReviewAndSaveAlignment(pathRoadStats$, statFile$)
            Else
                Locate 20
                Color 14
                Print "WARNING !! - - ROAD STAT FILE CONTAINS PLAYERS NOT IN EXISTING STATS.  YOU MUST"
                Print "CHANGE ROAD STAT FILE USING OPTION #6 ON GAME MENU BEFORE ATTEMPTING STAT MERGE."
                Locate 23, 30
                Color 13: Line Input "HIT ENTER TO CONTINUE"; DUMMY$
            End If 'Done checking if players are in sequence

        End If 'Done checking N = 15

    Loop Until N = 15


End Sub


'----------------------------------------
'    ReviewAndSaveAlignment Subroutine
'----------------------------------------
'...explanation...
Sub ReviewAndSaveAlignment (pathRoadStats$, statFile$)

    Shared A$()

    Shared ZR!(), ZR1!()

    Shared AN$()
    Shared AN!()
    Shared W0N!(), WN1!()

    Cls
    Locate 1, 32
    Color 12: Print "STAT ROSTERS"

    Locate , 26
    Color 15: Print "CURRENT  ";
    Color 7: Print "             ROAD"

    For I = 0 To 13:
        Locate I + 4, 22: Color 12: Print Using "##"; I;: Print " ";
        Color 15: Print A$(I);
        Color 12: Print Tab(44); Using "##"; I;: Print " ";
        Color 7: Print AN$(I)
    Next

    Color 11
	Locate 22: Line Input "HIT ENTER TO ALIGN"; DUMMY$
    Color 11
	Locate 22: Print "ALIGNING FILES . . .             "

    Open pathRoadStats$ + statFile$ For Output As #1
    Write #1, BR$
    For I = 0 To 13: Write #1, AN$(I)
    For J = 0 To 13: Write #1, AN!(I, J): Next: Write #1, W0N!(I): Write #1, WN1!(I): Next
    For L = 0 To 15: Write #1, ZR!(L): Next
    For L = 0 To 15: Write #1, ZR1!(L): Next
    Close #1

    Cls

    Locate 8, 20: Print "RE-ALIGNMENT COMPLETED"
    Print
    Locate 12, 20
    Color 12: Line Input "HIT ENTER TO CONTINUE"; DUMMY$

    Cls

End Sub


'----------------------------------------
'       GetDiskID_ALIGN  Subroutine
'----------------------------------------
'...explanation...
Sub GetDiskID_ALIGN (A$)

    Shared mergeT%()

    Do

        Color 15, 0
        Cls
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
                Call SelectTeam_MERGE(yearNumber$, numberTeams)
            Else
                Locate 7, 27: Print I$
                Locate 7, 28: Line Input NY$
                NY$ = I$ + NY$
                Color 15, 0

                If NY$ = "" And yearNumber$ <> "" Then
                    Call SelectTeam_MERGE(yearNumber$, numberTeams)
                Else
                    yearNumber$ = NY$
                    Erase teamNames$
                    numberTeams = 0

                    Open diskPaths$(0) + "BASK." + yearNumber$ For Random As #1 Len = 655
                    Y = LOF(1)
                    Field #1, 15 As Q$(0), 640 As X$
                    For I1 = 1 To Y / 655
                        Get #1, I1
                        teamNames$(I1) = Q$(0)
                        teamIndex%(I1) = I1
                    Next
                    Close 1
                    numberTeams = Y / 655

                    Call SelectTeam_MERGE(yearNumber$, numberTeams)
                End If

            End If

            A$ = RTrim$(Q$(0))

            If BO% = 1 Then
                BO% = 0
            Else
                Locate 2, 33
                Color mergeT%(0), mergeT%(1): Print A$

                Color 15, 4

                Do
                    Locate 3, 33: Print "ANY CHANGE (YN)";
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            End If

        Else
            statFile$ = "END"
        End If

    Loop Until UCase$(I$) = "N" Or statFile$ = "END"

End Sub


'----------------------------------------
'       SelectTeam_ALIGN Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_ALIGN (yearNumber$, numberTeams)

    Shared mergeT%(), statsL%()

    Selection% = 1
    Count% = numberTeams
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
    MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    If ExitCode% <> 27 Then

        T1 = teamIndex%(Selection%)

        Locate 2, 33: Color 31: Print "LOADING..."

        If Not _FileExists(diskPaths$(0) + "BASK." + yearNumber$) Then
            BO% = 1
            Locate 10, 8
            Call CSMsgBox("NO TEAM FILES WERE FOUND MATCHING THIS ID.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, PASS)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, PASS)
        Else

            Open diskPaths$(0) + "BASK." + yearNumber$ For Random As #1 Len = 655

            Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 2 As Q$(16), 2 As Q$(17), 2 As Q$(18), 2 As Q$(19), 602 As X$

            For I = 0 To 13
                Field #1, 53 + I * 43 As X$, 15 As Q$(I * 15 + 20), 2 As Q$(I * 15 + 21), 2 As Q$(I * 15 + 22), 2 As Q$(I * 15 + 23), 2 As Q$(I * 15 + 24), 2 As Q$(I * 15 + 25), 2 As Q$(I * 15 + 26), 2 As Q$(I * 15 + 27), 2 As Q$(I * 15 + 28), 2 As Q$(I * 15 + 29), 2 As Q$(I * 15 + 30), 2 As Q$(I * 15 + 31), 2 As Q$(I * 15 + 32), 2 As Q$(I * 15 + 33), 2 As Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 As X$
            Next

            Get #1, T1

            staminaRating% = CVI(Q$(1))
            defFGPctAdj% = CVI(Q$(2))
            V1% = CVI(Q$(3))
            V2% = CVI(Q$(4))

            For I = 0 To 4: statsL%(I) = CVI(Q$(I + 5)): Next
            For I = 0 To 9: mergeT%(I) = CVI(Q$(I + 10)): Next

            Close 1

        End If

    Else
        BO% = 1
    End If

End Sub
