'----------------------------------------
'          BLANKORG Subroutine
'----------------------------------------
'This routine assists the user in choosing
'a national org, and then proceeds to delete 
' all stats files for all associated teams
Sub BLANKORG ()

    _Title "Courtside College Basketball - Erase Org Stats"

    Dim statFiles$(MAX_CONFERENCES), Z4$(MAX_CONFERENCES)

    organizationFile$ = ChooseFile$ (diskPaths$(2) + "*.CON", "ORGANIZATION", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        MsgText$ = "WARNING!!! THIS WILL ERASE ALL COMPILED STATS FOR THIS ORGANIZATION. IF YOU STILL WISH TO ERASE ALL STATS, HIT 'Y' FOR YES"
        BoxWidth = 27
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

        If UCase$(I$) = "Y" Then

            Open diskPaths$(2) + organizationFile$ + ".CON" For Input As #1

            Input #1, org$

            For X = 1 To MAX_CONFERENCES
                Input #1, Z4$(X)
            Next

            Close #1

            For X = 1 To MAX_CONFERENCES

                If Z4$(X) <> "XXX" Then

                    Open diskPaths$(2) + Z4$(X) + ".TMS" For Input As #2

                    Input #2, org$, D1$, D2$

                    For XW = 1 To TEAMS_PER_CONFERENCE
                        Input #2, statFiles$(XW)
                    Next

                    Close #2

                    For XW = 1 To TEAMS_PER_CONFERENCE
                        If statFiles$(XW) <> Chr$(42) Then
                            Print "DELETING "; statFiles$(XW); " STATS"
                            statFile$ = diskPaths$(1) + statFiles$(XW)
                            If _FileExists(statFile$) Then Kill statFile$
                            If _FileExists(statFile$ + "S") Then Kill statFile$ + "S"
                        End If
                    Next

                End If 'done checking for XXX

            Next

        End If 'Done confirming to move forward with deletion
    
    End If

End Sub
