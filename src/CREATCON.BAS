Sub CREATCON ()

    _Title "Courtside College Basketball - Manage Conferences"

    Shared scheduleZ1%()

    Shared Z1$(), Z2$()

    ReDim menuChoices$(1 To 3)

    Do

        Cls

        Call TITLE
        menuChoices$(1) = "CREATE / DEFINE NEW CONFERENCES  "
        menuChoices$(2) = "ADD / DELETE / REVIEW CONFERENCES"
        menuChoices$(3) = "RETURN TO STANDINGS/LEADERS MENU "
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

            For X = 1 To 30
                Z1$(X) = "XXX"
            Next

            D1$ = ""
            D2$ = ""

            Select Case Selection%

                Case 1:
                    Call DefineConference(LG$, D1$, D2$)
                    Call ReviewEditConference(LG$, D1$, D2$)

                Case 2:
                    Call ChooseConference(LG$)
                    Call ReviewEditConference(LG$, D1$, D2$)

            End Select

        End If 'Done checking for ESC

    Loop Until Selection% = 3 Or ExitCode% = 27

    Color 15, 0

End Sub


'----------------------------------------
'       DefineConference Subroutine
'----------------------------------------
'...explanation...
Sub DefineConference (LG$, D1$, D2$)

    Do

        Color 15, 0
        Cls

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

        Do

            Locate 7, 32

            Locate , 12: Print "ENTER NAME FOR CONFERENCE: ": Print
            Locate , 12: Print "THE NAME SHOULD HAVE NO PERIODS OR SPACES."
            Locate , 12: Print "SAMPLE NAMES ARE SEC, SWC, 94-ACC"
            Locate 7, 37: Input LG$

            If LG$ <> "" Then

                Do
                    Locate 13, 12: Print "ANY CHANGE (YN) "
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            End If

        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

    Color 15, 0
    Print
    Print
    Locate , 12: Input "ENTER NAME OF DIVISION 1 (ENTER FOR NONE) "; D1$
    Locate , 12: Input "ENTER NAME OF DIVISION 2 (ENTER FOR NONE) "; D2$

End Sub


'----------------------------------------
'       ChooseConference Routine
'----------------------------------------
'...explanation...
Sub ChooseConference (conference$)

    Do
        'Display list of files...
        Spec$ = diskPaths$(1) + "*.LIB"
        Count% = FCount%(Spec$)

        If Count% = 0 Then
            Locate 10, 8
            Call CSMsgBox("NO CONFERENCE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, PASS)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, PASS)
        Else

            ReDim conferenceFiles$(0 To Count%)

            For X = 1 To Count%
                conferenceFiles$(X) = Space$(12)
            Next

            conferenceFiles$(0) = Spec$
            Call ReadFile(conferenceFiles$())

            Do

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
                        Locate 3, 33: Print "ANY CHANGE (YN)";
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    Color 15, 0

                End If

            Loop Until UCase$(I$) = "N" Or ExitCode% = 27

            If ExitCode% <> 27 Then
                targetFile$ = RTrim$(targetFile$)
                conference$ = Left$(targetFile$, Len(targetFile$) - 4)
            End If

        End If

        '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0 Or UCase$(I$) = "N"

End Sub


'----------------------------------------
'      ReviewEditConference Subroutine
'----------------------------------------
'...explanation...
Sub ReviewEditConference (conference$, D1$, D2$)

    Shared scheduleZ1%()

    Shared Z1$(), Z2$()

    If _FileExists(diskPaths$(1) + conference$ + ".LIB") Then

        Open diskPaths$(1) + conference$ + ".LIB" For Input As #2

        Input #2, LG%, conference$, D1$, D2$

        For X = 1 To 30
            Input #2, Z1$(X)
            Input #2, scheduleZ1%(X)
            Input #2, Z2$(X)
        Next

        Close 2

    End If

    Do

        Color 14, 0
        Cls

        Print
        Print "  "; conference$; " CONFERENCE"
        Print

        Color 12
        Locate , 6: Print D1$;
        Locate , 53: Print D2$

        For I = 1 To 15

            Color 2: Print Using "  ##"; I;
            Color 15: Print " "; Z1$(I);

            Locate , 50
            Color 2: Print Using "##"; I + 15;
            Color 15: Print " "; Z1$(I + 15)

        Next I

        Do

            Locate 21, 1

            Color 14: Print "(1) ";
            Color 15: Print "ADD A TEAM"
            Color 14: Print "(2) ";
            Color 15: Print "REMOVE A TEAM"
            Color 14: Print "(3) ";
            Color 15: Print "END AND SAVE CONFERENCE"

            I$ = GetKeyPress$
            choice = Val(I$)
        Loop Until choice >= 1 And choice <= 3

        If choice <> 3 Then

            Do
                Locate 22, 35
                Print "TEAM # (1-30) ";
                Input teamIndex
            Loop Until teamIndex >= 1 And teamIndex <= 30

            If choice = 1 Then
                'Add team
                'Call GetTeam_CREATCON(Z1$(teamIndex), Z2$(teamIndex), scheduleZ1%(teamIndex))
                Call ChooseTeam (Z2$(teamIndex), NTMS, scheduleZ1%(teamIndex), Z1$(teamIndex))
            Else
                'Remove team
                Z1$(teamIndex) = "XXX"
            End If

        End If 'Done checking if ready to save

    Loop Until choice = 3

    Call SaveConferences(conference$, D1$, D2$)

End Sub


'----------------------------------------
'     SaveConferences Subroutine
'----------------------------------------
'...explanation...
Sub SaveConferences (LG$, D1$, D2$)

    Shared scheduleZ1%()

    Shared Z1$(), Z2$()

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

        If Z1$(X) = "XXX" Then
            Z1$(X) = Chr$(42)
        Else
            teamName$ = Z1$(X)
            statFile$ = nameStatFile$(teamName$)
            Z1$(X) = statFile$
        End If

    Next X

    Open diskPaths$(1) + LG$ + ".TMS" For Output As #2

    Write #2, LG$
    Write #2, D1$
    Write #2, D2$

    For X = 1 To 30:
        Write #2, Z1$(X)
    Next

    Close 2

End Sub


