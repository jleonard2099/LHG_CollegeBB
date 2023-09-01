'----------------------------------------
'          CREATNAT Subroutine
'----------------------------------------
'This subroutine provides a menu for the
'user to choose options related to the
'creation and editing of national orgs
Sub CREATNAT ()

    _Title "Courtside College Basketball - Manage Organizations"

    Shared creatZ1$()

    ReDim menuChoices$(1 To 3)

    Do
        Cls

        Call TITLE
        menuChoices$(1) = "CREATE / DEFINE NEW LEAGUES      "
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

            For X = 1 To MAX_CONFERENCES
                creatZ1$(X) = "XXX"
            Next

            Select Case Selection%

                Case 1:
                    Call DefineNatOrg(league$)
                    Call ReviewEditNatOrg(league$)

                Case 2:
                    league$ = ChooseFile$ (diskPaths$(1) + "*.CON", "ORGANIZATION", FileExit%)

                    If FileExit% <> 27 And FileExit% <> 0 Then
                        Call ReviewEditNatOrg(league$)
                    End If

            End Select

        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

End Sub


'----------------------------------------
'       DefineNatOrg Subroutine
'----------------------------------------
'This subroutine assists the user with the
'input of paramters related to the org's
'general naming and setup
Sub DefineNatOrg (org$)

    Do

        Color 15, 0
        Cls

        Call MenuBox (6, 8, 61, 7, 15, 1)

        Do

            Locate 7, 32

            Locate , 12: Print "ENTER NAME FOR ORGANIZATION: "
            Print
            Locate , 12: Print "THE NAME SHOULD HAVE NO PERIODS OR SPECIAL CHARACTERS."
            Locate , 12: Print "SAMPLE NAMES ARE NCAA-90, SOUTH, MIDWEST"

            Locate 7, 40: Input org$

            If org$ <> "" Then

                Do
                    Locate 13, 12: Print "ANY CHANGE? (Y/N) "
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            End If

        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

End Sub


'----------------------------------------
'       ReviewEditNatOrg Subroutine
'----------------------------------------
'This subroutine provides an interface to
'the user allowing them to choose / clear
'conferences from the org definition.
Sub ReviewEditNatOrg (organization$)

    Shared creatZ1$()

    If _FileExists(diskPaths$(1) + organization$ + ".CON") Then

        Open diskPaths$(1) + organization$ + ".CON" For Input As #1

        Input #1, organization$

        For X = 1 To MAX_CONFERENCES
            Input #1, creatZ1$(X)
        Next

        Close #1

    End If

    Do

        Color 14, 0
        Cls

        Print
        Print "  "; organization$
        Print

        Color 12

        For I = 1 To 10

            Color 2: Print Using "## "; I;
            Color 15: Print creatZ1$(I);

            Locate , 21
            Color 2: Print Using "## "; I + 10;
            Color 15: Print creatZ1$(I + 10);

            Locate , 41
            Color 2: Print Using "## "; I + 20;
            Color 15: Print creatZ1$(I + 20);

            Locate , 61
            Color 2: Print Using "## "; I + 30;
            Color 15: Print creatZ1$(I + 30)

        Next I

        Do

            Locate 21, 1

            Color 14: Print "(1) ";
            Color 15: Print "ADD A CONFERENCE"
            Color 14: Print "(2) ";
            Color 15: Print "REMOVE A CONFERENCE"
            Color 14: Print "(3) ";
            Color 15: Print "END AND SAVE ORGANIZATION"

            I$ = GetKeyPress$
            choice = Val(I$)
        Loop Until choice >= 1 And choice <= 3

        If choice <> 3 Then

            Do
                Locate 22, 35
                Print "SLOT # (1-40) ";
                Input conferenceIndex
            Loop Until conferenceIndex >= 1 And conferenceIndex <= MAX_CONFERENCES

            If choice = 1 Then
                'Add conference
                creatZ1$(conferenceIndex) = ChooseFile$ (diskPaths$(1) + "*.LIB", "CONFERENCE", ExitCode%)
            Else
                'Remove conference
                creatZ1$(conferenceIndex) = "XXX"
            End If

        End If 'Done checking if ready to save

    Loop Until choice = 3

    Call SaveNatOrg(organization$)

End Sub


'----------------------------------------
'        SaveNatOrg Subroutine
'----------------------------------------
'This subroutine writes the organization's
'conference memberships and definition to 
'each of the corresponding files (.CON)
Sub SaveNatOrg (leagueFile$)

    Shared creatZ1$()

    Open diskPaths$(1) + leagueFile$ + ".CON" For Output As #1

    Print #1, leagueFile$

    For idx = 1 To MAX_CONFERENCES
        Print #1, creatZ1$(idx)
    Next

    Close #1

    Cls

End Sub

