'----------------------------------------
'          CREATCON Subroutine
'----------------------------------------
'This subroutine provides a menu for the
'user to choose options related to the
'creation and editing of conferences
Sub CREATCON ()

    _Title "Courtside College Basketball - Manage Conferences"

    Shared memberIdx()
    Shared memberName$(), memberYear$()

    Shared conferenceName$
    Shared div1Name$, div2Name$

    ReDim menuChoices$(1 To 3)

    Do

        Color 15, 0
        Cls

        Call TITLE
        menuChoices$(1) = "CREATE A CONFERENCE      "
        menuChoices$(2) = "MODIFY CONFERENCE MEMBERS"
        menuChoices$(3) = "RETURN TO LEADERS MENU   "
        SELECTION% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            For X = 1 To TEAMS_PER_CONFERENCE
                memberIdx(X) = -1
                memberName$(X) = "XXX"
                memberYear$(X) = "XXX"
            Next

            div1Name$ = ""
            div2Name$ = ""

            Select Case SELECTION%

                Case 1:
                    Call DefineConference
                    Call ReviewEditConference(conferenceName$)

                Case 2:
                    Do

                        confFile$ = ChooseFile$(diskPaths$(2) + "*.TMI", "CONFERENCE", FileExit%)

                        If FileExit% <> 27 And FileExit% <> 0 Then
                            Call ReviewEditConference(confFile$)
                        End If

                    Loop Until FileExit% = 27 Or FileExit% = 0

            End Select

        End If 'Done checking for ESC

    Loop Until SELECTION% = 3 Or ExitCode% = 27

    Color 15, 0
    Cls

End Sub


'----------------------------------------
'       DefineConference Subroutine
'----------------------------------------
'This subroutine assists the user with the
'input of paramters related to the conf-
'erence's general naming and setup
Sub DefineConference ()

    Shared conferenceName$
    Shared div1Name$, div2Name$

    Do

        Color 15, 0
        Cls

        Call MenuBox(6, 8, 61, 7, 15, 1)

        Do

            Locate 7, 32

            Locate , 12: Print "ENTER NAME FOR CONFERENCE: "
            Print
            Locate , 12: Print "THE NAME SHOULD HAVE NO PERIODS OR SPECIAL CHARACTERS."
            Locate , 12: Print "SAMPLE NAMES ARE SEC, SWC, 94-ACC"
            Locate 7, 37: Input conferenceName$

            If conferenceName$ <> "" Then

                Do
                    Locate 13, 12: Print "ANY CHANGE? (Y/N) "
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            End If

        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

    FG = 0
    BG = 7

    Cls
    Color 15, 1

    Locate 5, 1: QPrint "Name for Division 1 ", 14, 1, -1
    Locate , 21: QPrint PadRight$(div1Name$, 50), FG, BG, -1
    Locate 7, 1: QPrint "Name for Division 2 ", 14, 1, -1
    Locate , 21: QPrint PadRight$(div2Name$, 50), FG, BG, -1

    GetDiv1:
    Locate 5, 21 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 50 'specify field parameters
    Call TextIn(div1Name$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data

    GetDiv2:
    Locate 7, 21 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 50 'specify field parameters
    Call TextIn(div2Name$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here

    Done:
    Color 14, 1
    Locate 23, 1
    Print "Is this okay? (Yes/No) ";
    X$ = ""
    Call YesNoB(X$, XCode, FG, BG)
    Locate , 1
    ClearEOL 14, 1
    If XCode = 1 GoTo GetDiv2
    If X$ = "N" GoTo GetDiv1
    If X$ <> "Y" GoTo Done 'insist on either Y or N or up-arrow

End Sub


'----------------------------------------
'     ReviewEditConference Subroutine
'----------------------------------------
'This subroutine provides an interface to
'the user allowing them to choose / clear
'teams from the conference
Sub ReviewEditConference (confFile$)

    Shared memberIdx()
    Shared memberName$(), memberYear$()

    Shared conferenceName$
    Shared div1Name$, div2Name$

    test$ = confFile$

    If _FileExists(diskPaths$(2) + confFile$ + ".TMI") Then
        Call ReadConferenceIndex(confFile$)
    End If

    Do

        Color 14, 0
        Cls

        Print
        Print "  "; conferenceName$; " CONFERENCE"
        Print

        Color 12
        Locate , 6: Print div1Name$;
        Locate , 53: Print div2Name$

        For I = 1 To TEAMS_PER_DIV

            Color 2: Print Using "  ##"; I;
            Color 15: Print " "; Left$(memberName$(I), 25);

            Locate , 50
            Color 2: Print Using "##"; I + 15;
            Color 15: Print " "; Left$(memberName$(I + 15), 25)

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
                Print "SLOT # (1-" + Str$(TEAMS_PER_CONFERENCE) + ") ";
                Input teamIndex
            Loop Until teamIndex >= 1 And teamIndex <= TEAMS_PER_CONFERENCE

            If choice = 1 Then
                'Add team
                teamNbr = 0
                Call ChooseTeam(memberYear$(teamIndex), NTMS, memberIdx(teamIndex), memberName$(teamIndex))
            Else
                'Remove team
                memberName$(teamIndex) = "XXX"
            End If

        End If 'Done checking if ready to save

    Loop Until choice = 3

    Call SaveConferences(confFile$)

End Sub


'----------------------------------------
'       SaveConferences Subroutine
'----------------------------------------
'This subroutine writes the conference's
'team memberships and definition to
'each of the corresponding files (.TMI, .TMS)
Sub SaveConferences (confFile$)

    Shared memberIdx()
    Shared memberName$(), memberYear$()

    Shared conferenceName$
    Shared div1Name$, div2Name$

    Dim statFileNames$(TEAMS_PER_CONFERENCE)

    Open diskPaths$(2) + confFile$ + ".TMI" For Output As #1

    Print #1, 0
    Print #1, conferenceName$
    Print #1, div1Name$
    Print #1, div2Name$

    For X = 1 To TEAMS_PER_CONFERENCE
        Print #1, memberName$(X)
        Print #1, memberIdx(X)
        Print #1, memberYear$(X)
    Next

    Close #1

    For X = 1 To TEAMS_PER_CONFERENCE

        If memberName$(X) = "XXX" Then
            statFileNames$(X) = Chr$(42)
        Else
            teamName$ = memberName$(X)
            statFileNames$(X) = nameStatFile$(teamName$)
        End If

    Next X

    Open diskPaths$(2) + confFile$ + ".TMS" For Output As #2

    Write #2, conferenceName$
    Write #2, div1Name$
    Write #2, div2Name$

    For X = 1 To TEAMS_PER_CONFERENCE:
        Write #2, statFileNames$(X)
    Next

    Close #2

End Sub
