'----------------------------------------
'          BLANKCON Subroutine
'----------------------------------------
'This routine assists the user in choosing
'a college conference, and then proceeds to 
'delete all stats files for all associated 
'teams in the conference
Sub BLANKCON ()

    _Title "Courtside College Basketball - Erase Conference Stats"

	Shared memberName$()

    conferenceFile$ = ChooseFile$ (diskPaths$(2) + "*.TMI", "CONFERENCE", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        MsgText$ = "WARNING!!! THIS WILL ERASE ALL COMPILED STATS FOR THIS CONFERENCE. IF YOU STILL WISH TO ERASE ALL STATS, PRESS 'Y' FOR YES"
        BoxWidth = 31
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        G$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

        If UCase$(G$) = "Y" Then

            Call ReadConferenceStats (conferenceFile$)

            For XW = 1 To TEAMS_PER_CONFERENCE

                If memberName$(XW) <> Chr$(42) Then
                    Print "DELETING "; memberName$(XW); " STATS"
                    statFile$ = diskPaths$(1) + memberName$(XW)
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
