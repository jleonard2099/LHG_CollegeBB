'----------------------------------------
'          BLANKCON Subroutine
'----------------------------------------
'This routine assists the user in choosing
'a college conference, and then proceeds to 
'delete all stats files for all associated 
'teams in the conference
Sub BLANKCON ()

    _Title "Courtside College Basketball - Erase Conference Stats"

    Dim statFileNames$(TEAMS_PER_CONFERENCE)

    conferenceFile$ = ChooseFile$ (diskPaths$(2) + "*.TMI", "CONFERENCE", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        MsgText$ = "WARNING!!! THIS WILL ERASE ALL COMPILED STATS FOR THIS CONFERENCE. IF YOU STILL WISH TO ERASE ALL STATS, PRESS 'Y' FOR YES"
        BoxWidth = 31
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        G$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

        If UCase$(G$) = "Y" Then

            Open diskPaths$(2) + conferenceFile$ + ".TMS" For Input As #1

            Input #1, conf$, div1$, div2$ ', D3$, D4$

            For XW = 1 To TEAMS_PER_CONFERENCE
                Input #1, statFileNames$(XW)
            Next

            Close #1

            For XW = 1 To TEAMS_PER_CONFERENCE

                If statFileNames$(XW) <> Chr$(42) Then
                    Print "DELETING "; statFileNames$(XW); " STATS"
                    statFile$ = diskPaths$(1) + statFileNames$(XW)
                    If _FileExists(statFile$) Then Kill statFile$
                    If _FileExists(statFile$) Then Kill statFile$
                    If _FileExists(statFile$ + "D") Then Kill statFile$ + "D"
                    If _FileExists(statFile$ + "R") Then Kill statFile$ + "R"
                    If _FileExists(statFile$ + "S") Then Kill statFile$ + "S"
                End If

            Next

            leaderFile$ = diskPaths$(2) + conferenceFile$ + ".LDR"
            If _FileExists(leaderFile$) Then Kill leaderFile$

            standingsFile$ = diskPaths$(2) + conferenceFile$ + ".LDS"
            If _FileExists(standingsFile$) Then Kill standingsFile$

        End If 'Done confirming to move forward with deletion

    End If

End Sub
