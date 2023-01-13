Sub CREATCON ()

    _Title "Courtside College Basketball - Manage Conference"

    DefInt A-Z

    Dim B(1, 2), T1(1)

    Shared Z1$(), Z2$()
    Shared scheduleZ1%()

    Shared T%()

    ReDim menuChoices$(1 To 3)

    FG = 0
    BG = 7

    Do

        Cls

        Call TITLE
        menuChoices$(1) = "CREATE / DEFINE NEW CONFERENCES "
        menuChoices$(2) = "ADD / REVIEW CONFERENCE MEMBERS "
        menuChoices$(3) = "RETURN TO STANDINGS/LEADERS MENU"
        Selection% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 11, 57, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1:
                    For X = 1 To 30
                        Z1$(X) = "XXX"
                    Next

                    Do
                        Color 15, 0: Cls
                        JJ = 7
                        Locate 6, 8: Color 15, 1
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
                        Locate , 12: Print "ENTER NAME FOR CONFERENCE: ": Print
                        Locate , 12: Print "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
                        Locate , 12: Print "SPACES. TO EXIT, JUST HIT 'ENTER'"
                        Locate , 12: Print "SAMPLE NAMES ARE SEC, SWC, 94-ACC"
                        Locate 7, 37: Input LG$

                        If LG$ <> "" Then
                            LG$ = Left$(LG$, 8)

                            Do
                                Locate 13, 12: Print "ANY CHANGE (YN) "
                                I$ = GetKeyPress$
                            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                        End If

                    Loop Until UCase$(I$) = "N"

                    If LG$ <> "" Then
                        Color 15, 0
                        Print
                        Print
                        Locate , 12: Input "ENTER NAME OF DIVISION 1 (ENTER FOR NONE) "; D1$
                        Locate , 12: Input "ENTER NAME OF DIVISION 2 (ENTER FOR NONE) "; D2$
                    End If

                Case 2:
                    Call SelectFile_CREATCON(LG$)

                    If LG$ <> "" Then

                        LG$ = Left$(LG$, Len(LG$) - 4)

                        Open diskPaths$(1) + LG$ + ".LIB" For Input As #2
                        Input #2, LG%
                        Input #2, LG$
                        Input #2, D1$
                        Input #2, D2$

                        For X = 1 To 30
                            Input #2, Z1$(X)
                            Input #2, scheduleZ1%(X)
                            Input #2, Z2$(X)
                        Next

                        Close 2
                    End If
            End Select

            'A new routine of it's own?
            'We call this whether we did a new conf or are reviewing an existing
            If Selection% < 3 And LG$ <> "" Then

                Do
                    Color 15, 0

                    Cls

                    Print
                    Color 14: Print "  "; LG$; " CONFERENCE"
                    Print
                    Color 15
                    Locate , 6: Print D1$;
                    Locate , 53: Print D2$

                    For I = 1 To 15
                        Print Using "  ##"; I;
                        Print " "; Z1$(I);
                        Locate , 50
                        Print Using "##"; I + 15;
                        Print " "; Z1$(I + 15)
                    Next I

                    Locate 21, 3: Color 2
                    Print "1-ADD TEAM"
                    Print "  2-REMOVE TEAM"
                    Print "  3-END AND SAVE"
                    I$ = GetKeyPress$

                    'If I$ = Chr$(27) Then Exit Sub

                    If I$ = "3" Then
                        'Create LIB file
                        Open diskPaths$(1) + LG$ + ".LIB" For Output As #2
                        Print #2, 0

                        Print #2, LG$ 'League / Conference Name
                        Print #2, D1$ 'Division 1 Name
                        Print #2, D2$ 'Division 2 Name

                        For X = 1 To 30
                            Print #2, Z1$(X) 'Team Name
                            Print #2, scheduleZ1%(X) 'Team Index ?
                            Print #2, Z2$(X) 'Year Number
                        Next

                        Close 2

                        For X = 1 To 30

                            If Z1$(X) <> "XXX" Then
                                For XX = 4 To 11
                                    If Mid$(Z1$(X), XX, 1) = " " Or Mid$(Z1$(X), XX, 1) = "/" Or Mid$(Z1$(X), XX, 1) = "." Then Mid$(Z1$(X), XX, 1) = "_"
                                Next XX

                                For XX = 12 To 14
                                    If Mid$(Z1$(X), XX, 2) = "ST" Then Mid$(Z1$(X), 9, 1) = "_": Mid$(Z1$(X), 10, 2) = "ST"
                                Next XX

                                'This is updating Z1$ with the stat file name
                                QQ = 8
                                For XX = 11 To 4 Step -1
                                    If Mid$(Z1$(X), XX, 1) <> "_" Then
                                        Exit For
                                    Else
                                        QQ = XX - 4
                                    End If
                                Next XX

                                YR$ = Left$(Z1$(X), 2)
                                Z1$(X) = Mid$(Z1$(X), 4, QQ) + "." + YR$
                            Else
                                Z1$(X) = Chr$(42)
                            End If

                        Next X

                        'Create TMS file
                        J$ = Left$(LG$, 8)
                        Q$ = J$ + ".TMS"

                        Open diskPaths$(1) + Q$ For Output As #2

                        Write #2, LG$
                        Write #2, D1$
                        Write #2, D2$

                        For X = 1 To 30: Write #2, Z1$(X): Next

                        Close 2

                    Else
                        'Add / Remove
                        Do
                            Locate 20, 30: Print "TEAM # (1-30) ";
                            '-- what is this???
                            N = 1
                            Input teamIndex
                        Loop Until teamIndex >= 1 And teamIndex <= 30

                        If I$ = "2" Then ' "Remove"
                            Z1$(teamIndex) = "XXX"
                        Else
                            Do
                                'I$ = 1 AKA "Add"
                                Color 15, 0

                                Cls

                                JJ = 7
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
                                Locate , 12: Print
                                Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
                                Locate 7, 27: Input NY$

                                If NY$ = "" And YN$ <> "" Then
                                    Call SelectTeam_CREATCON(YN$, NTMS, T1, A$)

                                Else
                                    YN$ = NY$
                                    Erase teamNames$
                                    NTMS = 0

                                    Open diskPaths$(0) + "BASK." + YN$ For Random As #1 Len = 655
                                    Y = LOF(1)

                                    Field #1, 15 As Q$(0), 640 As X$

                                    For I1 = 1 To Y / 655
                                        Get #1, I1
                                        NTMS = NTMS + 1
                                        teamNames$(I1) = Q$(0)
                                        teamIndex%(I1) = I1
                                    Next

                                    Close 1

                                    NTMS = Y / 655
                                    'TS% = Int((NTMS - 1) / 80)

                                    Call SelectTeam_CREATCON(YN$, NTMS, T1, A$)

                                End If

                                If BO% = 1 Then
                                    BO% = 0
                                Else
                                    Locate 2, 33
                                    Color T%(0), T%(1): Print A$

                                    Color 15, 4

                                    Do
                                        Locate , 33: Print "ANY CHANGE (YN)";
                                        prompt$ = GetKeyPress$
                                    Loop Until UCase$(prompt$) = "Y" Or UCase$(prompt$) = "N"

                                    If UCase$(prompt$) = "N" Then
                                        Z1$(teamIndex) = A$
                                        Z2$(teamIndex) = YN$
                                        scheduleZ1%(teamIndex) = T1
                                    End If
                                End If

                            Loop Until UCase$(prompt$) = "N"

                        End If 'Done checking I$ value of 2

                    End If 'Done checking I$ value of 3

                Loop Until I$ = "3" 'Or UCase$(prompt$) = "N"

            End If 'Done checking for blank LG$ file or Selection% = 3

        End If 'Done checking for ESC

    Loop Until Selection% = 3 Or ExitCode% = 27

    Color 15, 0

End Sub

'----------------------------------------
'       SelectFile_CREATCON Routine
'----------------------------------------
'...explanation...
Sub SelectFile_CREATCON (targetFile$)

    Do
        'Display list of files...

        Spec$ = diskPaths$(1) + "*.LIB"
        null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
        Count% = DirCount%

        If Count% = 0 Then
            Locate 10, 8
            Call CSMsgBox("NO CONFERENCE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        Else

            ReDim conferenceFiles$(0 To Count%)

            For X = 1 To Count%
                conferenceFiles$(X) = Space$(12)
            Next

            conferenceFiles$(0) = Spec$

            Call ReadFile(conferenceFiles$())

            Do
                Cls
                Color , 1
                Cls
                Locate , 28
                Color 14
                Print "SELECT CONFERENCE TO SCHEDULE": Color 15
                Selection% = 1
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
                Print "-SELECTS CONFERENCE ";

                _MouseShow "DEFAULT"
                MAMenu conferenceFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                If ExitCode% <> 27 Then

                    Color 15, 1
                    targetFile$ = conferenceFiles$(Selection%)
                    Locate 2, 33: Print targetFile$

                    Do
                        Locate , 33: Print "ANY CHANGE (YN)";
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    Color 15, 0

                End If

            Loop Until UCase$(I$) = "N" Or ExitCode% = 27

            If ExitCode% <> 27 Then
                targetFile$ = RTrim$(targetFile$)
            End If

        End If

        '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0 Or UCase$(I$) = "N"

End Sub

'----------------------------------------
'       SelectTeam_CREATCON Routine
'----------------------------------------
Sub SelectTeam_CREATCON (yearNumber$, numberTeams, teamIdx, teamName$)

    Shared T%()

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

        teamIdx = teamIndex%(Selection%)

        Open diskPaths$(0) + "BASK." + yearNumber$ For Random As #1 Len = 655
        Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 2 As Q$(16), 2 As Q$(17), 2 As Q$(18), 2 As Q$(19), 602 As X$
        For I = 0 To 13
            Field #1, 53 + I * 43 As X$, 15 As Q$(I * 15 + 20), 2 As Q$(I * 15 + 21), 2 As Q$(I * 15 + 22), 2 As Q$(I * 15 + 23), 2 As Q$(I * 15 + 24), 2 As Q$(I * 15 + 25), 2 As Q$(I * 15 + 26), 2 As Q$(I * 15 + 27), 2 As Q$(I * 15 + 28), 2 As Q$(I * 15 + 29), 2 As Q$(I * 15 + 30), 2 As Q$(I * 15 + 31), 2 As Q$(I * 15 + 32), 2 As Q$(I * 15 + 33), 2 As Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 As X$
        Next
        Get #1, teamIdx

        teamName$ = RTrim$(Q$(0))

        For I = 0 To 9
            T%(I) = CVI(Q$(I + 10))
        Next

        Close 1
    Else
        BO% = 1
    End If

End Sub
