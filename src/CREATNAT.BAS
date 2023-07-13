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
                    Call DefineNatOrg(LG$)
                    Call ReviewEditNatOrg(LG$)

                Case 2:
                    Call ChooseNatOrg(LG$)
                    Call ReviewEditNatOrg(LG$)

            End Select

        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

End Sub


'----------------------------------------
'       DefineNatOrg Subroutine
'----------------------------------------
'...explanation...
Sub DefineNatOrg (LG$)

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

            Locate , 12: Print "ENTER NAME FOR LEAGUE: ": Print
            Locate , 12: Print "THE NAME SHOULD HAVE NO PERIODS OR SPACES."
            Locate , 12: Print "SAMPLE NAMES ARE NCAA-90, SOUTH, MIDWEST"
            Locate 7, 37: Input LG$

            If LG$ <> "" Then

                Do
                    Locate 13, 12: Print "ANY CHANGE (YN) "
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            End If

        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

End Sub


'----------------------------------------
'      ChooseNatOrg Subroutine
'----------------------------------------
'...explanation...
Sub ChooseNatOrg (organization$)

    Do
        'Display list of files...
        Spec$ = diskPaths$(1) + "*.CON"
        Count% = FCount%(Spec$)

        If Count% = 0 Then
            Locate 10, 8
            Call CSMsgBox("NO ORGANIZATION FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, PASS)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, PASS)

        Else

            ReDim organizationFiles$(0 To Count%)
            For X = 1 To Count%
                organizationFiles$(X) = Space$(12)
            Next

            organizationFiles$(0) = Spec$
            Call ReadFile(organizationFiles$())

            Do

                Color , 1
                Cls

                Locate , 28
                Color 14
                Print "SELECT ORGANIZATION TO SCHEDULE": Color 15
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
                Print "-SELECTS ORGANIZATION";

                _MouseShow "DEFAULT"
                MAMenu organizationFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                If ExitCode% <> 27 Then

                    Color 15, 1
                    targetFile$ = organizationFiles$(Selection%)
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
                organization$ = Left$(targetFile$, Len(targetFile$) - 4)
            End If

        End If

        '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0 Or UCase$(I$) = "N"

End Sub


'----------------------------------------
'   ChooseConference_NatOrg Subroutine
'----------------------------------------
'...explanation...
Sub ChooseConference_NatOrg (targetFile$)

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
            Cls
            Color , 1
            Cls
            Locate , 28
            Color 14
            Print "SELECT CONFERENCE": Color 15
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
            Print "-SELECTS CONFERENCE";

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
            targetFile$ = Left$(targetFile$, Len(targetFile$) - 4)
        End If 'Done checking for ESC

    End If 'Done checking file count

End Sub

'----------------------------------------
'       ReviewEditNatOrg Subroutine
'----------------------------------------
'...explanation...
Sub ReviewEditNatOrg (organization$)

    Shared creatZ1$()

    If _FileExists(diskPaths$(1) + organization$ + ".CON") Then

        Open diskPaths$(1) + organization$ + ".CON" For Input As #2

        Input #2, organization$

        For X = 1 To MAX_CONFERENCES
            Input #2, creatZ1$(X)
        Next

        Close 2

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
                Call ChooseConference_NatOrg(creatZ1$(conferenceIndex))
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
'...explanation...
Sub SaveNatOrg (leagueFile$)

    Shared creatZ1$()

    Open diskPaths$(1) + leagueFile$ + ".CON" For Output As #2

    Print #2, leagueFile$

    For idx = 1 To MAX_CONFERENCES
        Print #2, creatZ1$(idx)
    Next

    Close 2

    Cls

End Sub

