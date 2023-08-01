Sub BLANK ()

    _Title "Courtside College Basketball - Delete Conference Stats"

    Dim H1$(MAX_CONFERENCES)

    conferenceFile$ = ChooseFile$ (diskPaths$(1) + "*.LIB", "CONFERENCE", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Locate 10, 8
        Call CSMsgBox("WARNING!!!::: THIS WILL ERASE ALL COMPILED STATS FOR THIS CONFERENCE. IF YOU STILL WISH TO ERASE ALL STATS, HIT 'Y' FOR YES", 26, Pass)
        G$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

        If UCase$(G$) = "Y" Then

            Open diskPaths$(1) + conferenceFile$ + ".TMS" For Input As #2

            Input #2, LG$, D1$, D2$ ', D3$, D4$

            For XW = 1 To TEAMS_PER_CONFERENCE
                Input #2, H1$(XW)
            Next

            Close 2

            For XW = 1 To TEAMS_PER_CONFERENCE

                If H1$(XW) <> Chr$(42) Then
                    statFileName$ = diskPaths$(1) + H1$(XW)
                    Print "DELETING "; H1$(XW); " STATS"
                    If _FileExists(statFileName$) Then Kill statFileName$
                    If _FileExists(statFileName$ + "D") Then Kill statFileName$ + "D"
                    If _FileExists(statFileName$ + "R") Then Kill statFileName$ + "R"
                    If _FileExists(statFileName$ + "S") Then Kill statFileName$ + "S"
                End If

            Next

        End If 'Done confirming to move forward with deletion

    End If

End Sub
