'----------------------------------------
'            BLANK Subroutine
'----------------------------------------
'This routine assists the user in choosing
'a college conference, and then proceeds to 
'delete all stats files for all associated 
'teams in the conference
Sub BLANKCON ()

    _Title "Courtside College Basketball - Erase Conference Stats"

    Dim statFiles$(MAX_CONFERENCES)

    conferenceFile$ = ChooseFile$ (diskPaths$(1) + "*.LIB", "CONFERENCE", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        MsgText$ = "WARNING!!! THIS WILL ERASE ALL COMPILED STATS FOR THIS CONFERENCE. IF YOU STILL WISH TO ERASE ALL STATS, HIT 'Y' FOR YES"
        BoxWidth = 27
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        G$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

        If UCase$(G$) = "Y" Then

            Open diskPaths$(1) + conferenceFile$ + ".TMS" For Input As #1

            Input #1, conf$, D1$, D2$ ', D3$, D4$

            For XW = 1 To TEAMS_PER_CONFERENCE
                Input #1, statFiles$(XW)
            Next

            Close #1

            For XW = 1 To TEAMS_PER_CONFERENCE

                If statFiles$(XW) <> Chr$(42) Then
                    statFileName$ = diskPaths$(1) + statFiles$(XW)
                    Print "DELETING "; statFiles$(XW); " STATS"
                    If _FileExists(statFileName$) Then Kill statFileName$
                    If _FileExists(statFileName$ + "D") Then Kill statFileName$ + "D"
                    If _FileExists(statFileName$ + "R") Then Kill statFileName$ + "R"
                    If _FileExists(statFileName$ + "S") Then Kill statFileName$ + "S"
                End If

            Next

        End If 'Done confirming to move forward with deletion

    End If

End Sub
