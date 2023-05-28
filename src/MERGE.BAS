Sub MERGE ()

    _Title "Courtside College Basketball - Merge Stats"

    ReDim menuChoices$(1 To 3)

    Do

        Color , 0
        Cls

        Call TITLE_MERGE

        menuChoices$(1) = "MERGE STATISTICAL FILES           "
        menuChoices$(2) = "ALIGN STATISTICAL FILES           "
        menuChoices$(3) = "EXIT TO MAIN MENU                 "
        Selection% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 11, 59, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1: Call MergeStats
                Case 2: Call ALIGN
                Case 3: Cls
            End Select
        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

End Sub

'----------------------------------------
'          MergeStats  + routine
'----------------------------------------
'...explanation...
Sub MergeStats ()

    Dim ARP$(14)

    Shared Ycurr%, Yroad%

    Shared A$()
    Shared mergeH$(), HRP$()
    Shared mergeO$(), ORP$(), gameSite$(), SITERP$()

    Shared ARD(), CRD(), CRDRD()

    Shared APRD%()
    Shared mergeO%(), mergeT%()
    Shared mergeA(), mergeAP%()
    Shared ORD%(), TRD%()

    Shared statsW0!(), statsW1!()
    Shared statsZ!(), statsZ1!()
    Shared W0RD!(), W1RD!()
    Shared ZRD!(), ZRD1!()

    Do

        Call GetMergePaths(pathCurrentStats$, pathRoadStats$, I$)

        If UCase$(I$) = "N" And I$ <> Chr$(27) Then

            Call GetDiskID_MERGE(A$)

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

            Color , 0
            Cls

            Locate 10, 15: Print "ROAD STATS ARE ON DRIVE "; pathRoadStats$
            Locate , 15: Print "MASTER STATS ARE ON DRIVE "; pathCurrentStats$
            Locate , 15: Print "TEAM TO MERGE STATS FOR IS "; A$

            Do
                Locate 14, 15: Print "ANY CHANGE (YN)"
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            If UCase$(I$) = "N" Then

                If _FileExists(pathRoadStats$ + statFile$) Then

                    Open pathRoadStats$ + statFile$ For Input As #1
                    Input #1, B$

                    For I = 0 To 13: Input #1, ARP$(I)
                    For J = 0 To 13: Input #1, ARD(I, J): Next: Input #1, W0RD!(I): Input #1, W1RD!(I): Next
                    For L = 0 To 15: Input #1, ZRD!(L): Next
                    For L = 0 To 15: Input #1, ZRD1!(L): Next
                    Close #1

                    Open pathRoadStats$ + statFile$ + "S" For Input As #1
                    Input #1, Yroad%
                    For I = 1 To 100: Input #1, ORD%(I): Next I
                    For I = 1 To 100: Input #1, ORP$(I): Next I
                    For I = 1 To 100: Input #1, TRD%(I): Next I
                    For I = 1 To 100: Input #1, HRP$(I): Next I
                    For I = 1 To 100
                    For I1 = 0 To 1: Input #1, APRD%(I, I1): Next: Next
                    For I = 1 To 100: Input #1, CRDRD(I): Next I
                    For I = 1 To 100: Input #1, SITERP$(I): Next I
                    Close #1

                    Locate 21, 5: Color 3

                    For J = 21 To 23
                        Locate J
                        Print Space$(64)
                    Next

                    If _FileExists(pathCurrentStats$ + statFile$) Then

                        Open pathCurrentStats$ + statFile$ For Input As #1
                        Input #1, B$

                        For I = 0 To 13: Input #1, A$(I)
                        For J = 0 To 13: Input #1, mergeA(I, J): Next: Input #1, statsW0!(I): Input #1, statsW1!(I): Next
                        For L = 0 To 15: Input #1, statsZ!(L): Next
                        For L = 0 To 15: Input #1, statsZ1!(L): Next
                        Close #1

                        Open pathRoadStats$ + statFile$ + "S" For Input As #1
                        Input #1, Ycurr%
                        For I = 1 To 100: Input #1, mergeO%(I): Next I
                        For I = 1 To 100: Input #1, mergeO$(I): Next I
                        For I = 1 To 100: Input #1, mergeT%(I): Next I
                        For I = 1 To 100: Input #1, mergeH$(I): Next I
                        For I = 1 To 100
                        For I1 = 0 To 1: Input #1, mergeAP%(I, I1): Next: Next
                        For I = 1 To 100: Input #1, CRD(I): Next I
                        For I = 1 To 100: Input #1, gameSite$(I): Next I
                        Close #1

                        SEQ = 0

                        For K = 0 To 13
                            If UCase$(ARP$(K)) <> UCase$(A$(K)) Then
                                SEQ = SEQ + 1
                            End If
                        Next

                        If SEQ = 0 Then

                            Call DoTheMerge(pathCurrentStats$, statFile$, B$)

                            For J = 13 To 23
                                Locate J
                                Print Space$(65)
                            Next

                        Else

                            Cls
                            Color 14
                            Locate 5, 27: Print "PLAYERS NOT IN SEQUENCE !!"
                            Color 15
                            Locate 7, 20: Print "STATS WILL BE DISTORTED UNLESS THE ROAD"
                            Locate , 19: Print "FILE LISTS THE PLAYERS IN THE SAME SEQUENCE"
                            Locate , 24: Print "AS YOUR CURRENT PLAYER STAT FILE."
                            Color 12
                            Locate 11, 24: Print "YOU MUST REALIGN ROAD STAT FILE OR"
                            Locate , 31: Print "ENTER STATS MANUALLY."
                            Color 13
                            Locate 15, 29: Line Input "HIT ENTER TO GO TO MENU"; DUMMY$

                        End If 'Done checking sequence

                    Else

                        Cls
                        Color 7
                        Locate 2, 10: Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
                        Print
                        Locate , 10: Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$

                        Do
                            Locate , 10
                            Print "TRY AGAIN (YN) "
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                        If UCase$(I$) = "Y" Then
                            Print
                            Print "HIT ENTER TO RE-ENTER INFORMATION"
                            I$ = GetKeyPress$
                        End If

                    End If 'Done checking if home file exists

                Else

                    Cls
                    Locate 2, 10
                    Color 7: Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
                    Print
                    Locate , 10: Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$

                    Do
                        Locate , 10
                        Print "TRY AGAIN (YN) "
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    If UCase$(I$) = "Y" Then
                        Print
                        Print "HIT ENTER TO RE-ENTER INFORMATION"
                        I$ = GetKeyPress$
                    End If

                End If 'Done checking if road file exists

            End If 'Done checking if ready to proceed

        End If 'Done checking for ESC

    Loop Until DUMMY$ <> "" Or UCase$(I$) = "N"

End Sub


'----------------------------------------
'       DoTheMerge  + routine
'----------------------------------------
'...explanation...
Sub DoTheMerge (pathCurrentStats$, statFile$, B$)

    Shared Ycurr%, Yroad%

    Shared A$()
    Shared mergeH$(), HRP$()
    Shared mergeO$(), ORP$(), gameSite$(), SITERP$()

    Shared APRD%(), ARD(), CRD(), CRDRD()
    Shared mergeO%(), mergeT%()
    Shared mergeA(), mergeAP%()
    Shared ORD%(), TRD%()
    Shared W0RD!(), W1RD!()

    Shared statsW0!(), statsW1!()
    Shared statsZ!(), statsZ1!()
    Shared ZRD!(), ZRD1!()

    For K = 1 To Yroad%
        Ycurr% = Ycurr% + 1
        mergeO%(Ycurr%) = ORD%(K)
        mergeO$(Ycurr%) = ORP$(K)
        mergeT%(Ycurr%) = TRD%(K)
        mergeH$(Ycurr%) = HRP$(K)
        mergeAP%(Ycurr%, 0) = APRD%(Ycurr%, 0)
        mergeAP%(Ycurr%, 1) = APRD%(Ycurr%, 1)
        CRD(Ycurr%) = CRDRD(Ycurr%)
        gameSite$(Ycurr%) = SITERP$(Ycurr%)
    Next

    Locate 18, 20: Color 12: Print "MERGING STAT FILE AND SCHEDULE RESULTS . . . ."

    For I = 0 To 13

        For J = 0 To 12: 
            mergeA(I, J) = mergeA(I, J) + ARD(I, J)
        Next J

        statsW0!(I) = statsW0!(I) + W0RD!(I)
        statsW1!(I) = statsW1!(I) + W1RD!(I)

        If mergeA(I, 13) < ARD(I, 13) Then 
            mergeA(I, 13) = ARD(I, 13) 
        Else 
            mergeA(I, 13) = mergeA(I, 13)
        End If

    Next I

    For L = 0 To 15:
        statsZ!(L) = statsZ!(L) + ZRD!(L)
        statsZ1!(L) = statsZ1!(L) + ZRD1!(L)
    Next

    Open pathCurrentStats$ + statFile$ For Output As #1
    Write #1, B$

    For I = 0 To 13: 
        Write #1, A$(I)
        For J = 0 To 13: Write #1, mergeA(I, J): Next
        Write #1, statsW0!(I)
        Write #1, statsW1!(I)
    Next

    For L = 0 To 15: Write #1, statsZ!(L): Next
    For L = 0 To 15: Write #1, statsZ1!(L): Next

    Close #1

    Open pathCurrentStats$ + statFile$ + "S" For Output As #1
    Write #1, Ycurr%

    For I = 1 To 100: Write #1, mergeO%(I): Next I
    For I = 1 To 100: Write #1, mergeO$(I): Next I
    For I = 1 To 100: Write #1, mergeT%(I): Next I
    For I = 1 To 100: Write #1, mergeH$(I): Next I

    For I = 1 To 100
        For I1 = 0 To 1: Write #1, mergeAP%(I, I1): Next
    Next

    For I = 1 To 100: Write #1, CRD(I): Next I
    For I = 1 To 100: Write #1, gameSite$(I): Next I

    Close #1

    Y = 0

    Locate 20, 20: Color 12: Print "RESULTS SUCCESSFULLY MERGED!!"

End Sub


'----------------------------------------
'       GetMergePaths  + routine
'----------------------------------------
'...explanation...
Sub GetMergePaths (pathCurrentStats$, pathRoadStats$, promptResponse$)

    Shared opSysType$

    Do
        Color 12, 0
        Cls

        Locate 2, 5: Print "ENTER COMPLETE PATH FOR TEAM STATS TO MERGE/ALIGN"
        Color 15
        Select Case opSysType$
            Case "[WINDOWS]":
                Locate 4, 5: Print "EXAMPLE - A:"
                Locate , 5: Print "          C:\HAFFNER"
                Locate , 5: Print "          C:\HAFFNER\1960"
                Locate , 5: Print "          D:\CBASKET"
            Case "[LINUX]":
                Locate 4, 5: Print "EXAMPLE - /mnt/floppy"
                Locate , 5: Print "           /usr/bin/haffner"
                Locate , 5: Print "           /usr/bin/haffner/1960"
                Locate , 5: Print "           /home/haffner/cbasket"
            Case "[MAC]", "[MACOSX]":
                Locate 4, 5: Print "EXAMPLE - /Volumes/floppy"
                Locate , 5: Print "           /Applications/haffner"
                Locate , 5: Print "           /Applications/haffner/1960"
                Locate , 5: Print "           /Users/haffner/cbasket"
        End Select

        For J = 10 To 13
            Locate J
            Print Space$(70)
        Next

        Color 10
        Locate 10, 5
        Line Input "ROAD STATS:     "; pathRoadStats$
        Locate , 5
        Locate , 5
        Line Input "CURRENT STATS:  "; pathCurrentStats$

        Color 14
        Locate 13, 5: Print "ANY CHANGE (Y/N)"
        promptResponse$ = GetKeyPress$

    Loop Until UCase$(promptResponse$) = "N" Or promptResponse$ = Chr$(27)

    'Adjust path terminator for OS type
    Select Case opSysType$
        Case "[WINDOWS]":
            If Not (Right$(pathCurrentStats$, 1) = "\") Then
                pathCurrentStats$ = pathCurrentStats$ + "\"
            End If

            If Not (Right$(pathRoadStats$, 1) = "\") Then
                pathRoadStats$ = pathRoadStats$ + "\"
            End If

        Case "[MAC]", "[MACOSX]", "[LINUX]":
            If Not (Right$(pathCurrentStats$, 1) = "/") Then
                pathCurrentStats$ = pathCurrentStats$ + "/"
            End If

            If Not (Right$(pathRoadStats$, 1) = "/") Then
                pathRoadStats$ = pathRoadStats$ + "/"
            End If
    End Select

End Sub


'----------------------------------------
'       GetDiskID_MERGE   + routine
'----------------------------------------
'...explanation...
Sub GetDiskID_MERGE (A$)

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
                    yearNumber$ = RTrim$(NY$)
                    Erase teamNames$
                    numberTeams = 0

                    Open diskPaths$(0) + "BASK." + yearNumber$ For Random As #1 Len = 655

                    fileLength& = LOF(1)

                    Field #1, 15 As Q$(0), 640 As X$

                    For I1 = 1 to fileLength& / 655
                        Get #1, I1
                        teamNames$(I1) = Q$(0)
                        teamIndex%(I1) = I1
                    Next

                    Close 1

                    numberTeams = fileLength& / 655

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
'       SelectTeam_MERGE  + routine
'----------------------------------------
'...explanation...
Sub SelectTeam_MERGE (yearNumber$, numberTeams)

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
		teamYear$ = RTrim$(yearNumber$)

        Locate 2, 33: Color 31: Print "LOADING..."

        If Not _FileExists(diskPaths$(0) + "BASK." + teamYear$) Then
            BO% = 1
            Locate 10, 8
            Call CSMsgBox("NO TEAM FILES WERE FOUND MATCHING THIS ID.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, PASS)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, PASS)
        Else

            Open diskPaths$(0) + "BASK." + teamYear$ For Random As #1 Len = 655

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

