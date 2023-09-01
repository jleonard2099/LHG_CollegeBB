'----------------------------------------
'          CREATCOL Subroutine
'----------------------------------------
'This subroutine provides a menu for the
'user to choose options related to the
'creation and editing of conferences
Sub CREATCOL ()

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
                    Call DefineConference(conference$, D1$, D2$)
                    Call ReviewEditConference(conference$, D1$, D2$)

                Case 2:
                    conference$ = ChooseFile$ (diskPaths$(1) + "*.LIB", "CONFERENCE", FileExit%)

                    If FileExit% <> 27 And FileExit% <> 0 Then
                        Call ReviewEditConference(conference$, D1$, D2$)
                    End If

            End Select

        End If 'Done checking for ESC

    Loop Until Selection% = 3 Or ExitCode% = 27

    Color 15, 0

End Sub


'----------------------------------------
'       DefineConference Subroutine
'----------------------------------------
'This subroutine assists the user with the
'input of paramters related to the conf-
'erence's general naming and setup
Sub DefineConference (conference$, D1$, D2$)

    Do

        Color 15, 0
        Cls

        Call MenuBox (6, 8, 61, 7, 15, 1)

        Do

            Locate 7, 32

            Locate , 12: Print "ENTER NAME FOR CONFERENCE: ": Print
            Locate , 12: Print "THE NAME SHOULD HAVE NO PERIODS OR SPECIAL CHARACTERS."
            Locate , 12: Print "SAMPLE NAMES ARE SEC, SWC, 94-ACC"
            Locate 7, 37: Input conference$

            If conference$ <> "" Then

                Do
                    Locate 13, 12: Print "ANY CHANGE? (Y/N) "
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
'     ReviewEditConference Subroutine
'----------------------------------------
'This subroutine provides an interface to
'the user allowing them to choose / clear
'teams from the conference
Sub ReviewEditConference (conference$, D1$, D2$)

    Shared scheduleZ1%()

    Shared Z1$(), Z2$()

    If _FileExists(diskPaths$(1) + conference$ + ".LIB") Then

        Open diskPaths$(1) + conference$ + ".LIB" For Input As #1

        Input #1, LG%, conference$, D1$, D2$

        For X = 1 To 30
            Input #1, Z1$(X)
            Input #1, scheduleZ1%(X)
            Input #1, Z2$(X)
        Next

        Close #1

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
'       SaveConferences Subroutine
'----------------------------------------
'This subroutine writes the conference's
'team memberships and definition to 
'each of the corresponding files (.LIB, .TMS)
Sub SaveConferences (conference$, D1$, D2$)

    Shared scheduleZ1%()

    Shared Z1$(), Z2$()

    Open diskPaths$(1) + conference$ + ".LIB" For Output As #1

    Print #1, 0

    Print #1, conference$ 'League / Conference Name
    Print #1, D1$ 'Division 1 Name
    Print #1, D2$ 'Division 2 Name

    For X = 1 To 30
        Print #1, Z1$(X) 'Team Name
        Print #1, scheduleZ1%(X) 'Team Index ?
        Print #1, Z2$(X) 'Year Number
    Next

    Close #1

    For X = 1 To 30

        If Z1$(X) = "XXX" Then
            Z1$(X) = Chr$(42)
        Else
            teamName$ = Z1$(X)
            statFile$ = nameStatFile$(teamName$)
            Z1$(X) = statFile$
        End If

    Next X

    Open diskPaths$(1) + conference$ + ".TMS" For Output As #2

    Write #2, conference$
    Write #2, D1$
    Write #2, D2$

    For X = 1 To 30:
        Write #2, Z1$(X)
    Next

    Close #2

End Sub


