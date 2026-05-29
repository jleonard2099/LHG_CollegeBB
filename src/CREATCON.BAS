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

    ReDim menuChoices$(1 To 4)

    menuSelection = 1

    Do

        Color 15, 0
        Cls

        Call TITLE
        menuChoices$(1) = "CREATE A CONFERENCE      "
        menuChoices$(2) = "MODIFY CONFERENCE MEMBERS"
        menuChoices$(3) = "UPDATE MEMBER INDEXES    "
        menuChoices$(4) = "RETURN TO LEADERS MENU   "
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
        MMenuVert menuChoices$(), menuSelection, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
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

            Select Case menuSelection

                Case 1:
                    Call DefineConference
                    conferenceFile$ = diskPaths$(2) + conferenceName$ + ".TMI"
                    Call ReviewEditConference(conferenceFile$)

                Case 2:
                    confFile$ = _OpenFileDialog$("Choose League Definition file", diskPaths$(2), "*.TMI", "League Definition files [.TMI]")

                    If confFile$ <> "" Then
                        Call ReviewEditConference(confFile$)
                    End If

                Case 3
                    fileSpec$ = diskPaths$(2) + "*.TMI"

                    Count% = FileCount%(fileSpec$)
                    ReDim foundFiles$(0 To Count%)
                    foundFiles$(0) = fileSpec$
                    Call ReadFile(foundFiles$())

                    For X = 1 To Count%
                        target$ = diskPaths$(2) + foundFiles$(X)
                        Print "Converting "; target$
                        Call UpdateConferenceIndexes(target$)
                    Next X

            End Select

        End If 'Done checking for ESC

    Loop Until menuSelection = 4 Or ExitCode% = 27

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

        'boxText$ = "The name should have no periods or special characters" + Chr$(13) + Chr$(10) + "Sample names are ACC 2020, SEC 2020, Pac-12 2020"
        'conferenceName$ = _InputBox$("Enter Name for Conference", boxText$)

        Color 15, 0
        Cls

        Call MenuBox(6, 8, 61, 8, 15, 1)

        Do

            Locate 8, 32

            Locate , 12: Print "ENTER NAME FOR CONFERENCE: "
            Print
            Locate , 12: Print "THE NAME SHOULD HAVE NO PERIODS OR SPECIAL CHARACTERS."
            Locate , 12: Print "SAMPLE NAMES ARE ACC 2020, SEC 2020, Pac-12 2020"
            Locate 8, 37: Input conferenceName$

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
    Locate LAST_ROW - 2, 1
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
Sub ReviewEditConference (targetFile$)

    Shared memberIdx()
    Shared memberName$(), memberYear$()

    Shared conferenceName$
    Shared div1Name$, div2Name$

    If _FileExists(targetFile$) Then
        Call ReadConferenceIndex(targetFile$)
    End If

    Do

        Color 14, 0
        Cls

        Print
        Print "  "; conferenceName$; " CONFERENCE"
        Print

        Color 12
        Locate , 6: Print div1Name$;
        Locate , 44: Print div2Name$

        For I = 1 To TEAMS_PER_DIV

            Color 2: Print Using "  ##"; I;
            Color 15: Print " "; Left$(memberName$(I), 25);

            Locate , 41
            Color 2: Print Using "##"; I + 15;
            Color 15: Print " "; Left$(memberName$(I + 15), 25)

        Next I

        Do

            Locate 22, 1

            Color 14: Print "  (1) ";
            Color 15: Print "ADD A TEAM"
            Color 14: Print "  (2) ";
            Color 15: Print "REMOVE A TEAM"
            Color 14: Print "  (3) ";
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

    Call SaveConferences(targetFile$)

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

    'Open Definition File
    Open confFile$ For Output As #1

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

    'Open Stat File
    confStatFile$ = String.Replace$(confFile$, ".TMI", ".TMS")

    Open confStatFile$ For Output As #2

    Write #2, conferenceName$
    Write #2, div1Name$
    Write #2, div2Name$

    For X = 1 To TEAMS_PER_CONFERENCE:
        Write #2, statFileNames$(X)
    Next

    Close #2

End Sub


'----------------------------------------
'   UpdateConferenceIndexes Subroutine
'----------------------------------------
' This routine was specifically designed
' to assist with fixing league / conference
' files after a Teams file has been sorted.
' It will find the new indexes of each team
' in the file and update it.
Sub UpdateConferenceIndexes (sourceFile$)

    Shared memberIdx()
    Shared memberName$(), memberYear$()

    Dim totalMembers As Integer
    Dim currMember As Integer, currTeam As Integer
    Dim found As Integer
    Dim searchName As String
    Dim teamName$

    fileType = 0

    ' Need to read in conference indexes
    Call ReadConferenceIndex(sourceFile$)

    'Assume all teams are in the same year
    yearNumber$ = memberYear$(1)

    'Read in all teams for the disk number that year
    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$ For Random As #1 Len = TEAM_SIZE_BYTES
    fileLength& = LOF(1)
    Field #1, 15 As team$

    Open "convertlog" For Append As #21

    numberTeams = fileLength& / TEAM_SIZE_BYTES

    For teamIdx = 1 To numberTeams
        Get #1, teamIdx
        teamNames$(teamIdx) = team$
        teamIndex(teamIdx) = teamIdx
    Next teamIdx

    Close #1

    totalMembers = TEAMS_PER_CONFERENCE

    For currMember = 1 To totalMembers

        searchName = RTrim$(memberName$(currMember))
        found = 0

        For currTeam = 1 To numberTeams
            If RTrim$(teamNames$(currTeam)) = searchName Then
                memberIdx(currMember) = currTeam
                found = 1
                Exit For
            End If
        Next currTeam

        If found = 0 And searchName <> "XXX" Then
            Print #21, "WARNING: No match found for [" + searchName + "] at slot " + Str$(X)
        End If

    Next currMember

    Close #21

    'Here we can write the index file back
    Call SaveConferences(sourceFile$)

End Sub
