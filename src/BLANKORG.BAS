'----------------------------------------
'          BLANKORG Subroutine
'----------------------------------------
'This routine assists the user in choosing
'a national org, and then proceeds to delete 
' all stats files for all associated teams
Sub BLANKORG ()

    _Title "Courtside College Basketball - Erase National Stats"

    Dim statFileNames$(MAX_CONFERENCES)
    Shared memberConf$()
    
    organizationFile$ = ChooseFile$ (diskPaths$(2) + "*.CON", "ORGANIZATION", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        MsgText$ = "WARNING!!! THIS WILL ERASE ALL COMPILED STATS FOR THIS ORGANIZATION. IF YOU STILL WISH TO ERASE ALL STATS, PRESS 'Y' FOR YES"
        BoxWidth = 31
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

        If UCase$(I$) = "Y" Then

            Open diskPaths$(2) + organizationFile$ + ".CON" For Input As #1

            Input #1, org$

            For X = 1 To MAX_CONFERENCES
                Input #1, memberConf$(X)
            Next

            Close #1

            For X = 1 To MAX_CONFERENCES

                If memberConf$(X) <> "XXX" Then

                    Open diskPaths$(2) + memberConf$(X) + ".TMS" For Input As #2

                    Input #2, org$, div1$, div2$

                    For XW = 1 To TEAMS_PER_CONFERENCE
                        Input #2, statFileNames$(XW)
                    Next

                    Close #2

                    For XW = 1 To TEAMS_PER_CONFERENCE
                        If statFileNames$(XW) <> Chr$(42) Then
                            Print "DELETING "; statFileNames$(XW); " STATS"
                            statFile$ = diskPaths$(1) + statFileNames$(XW)
                            If _FileExists(statFile$) Then Kill statFile$
                            If _FileExists(statFile$ + "S") Then Kill statFile$ + "S"
                        End If
                    Next

                End If 'done checking for XXX

            Next

        End If 'Done confirming to move forward with deletion
    
    End If

End Sub
