'----------------------------------------
'          BLANKORG Subroutine
'----------------------------------------
'This routine assists the user in choosing
'a national org, and then proceeds to delete 
' all stats files for all associated teams
Sub BLANKORG ()

    _Title "Courtside College Basketball - Erase National Stats"

    Shared memberConf$()
	Shared memberName$()
    
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

            Call ReadOrganizationFile (organizationFile$, orgName$)

            For X = 1 To MAX_CONFERENCES

                If memberConf$(X) <> "XXX" Then

                    Call ReadConferenceStats(memberConf$(X))

                    For XW = 1 To TEAMS_PER_CONFERENCE
                        If memberName$(XW) <> Chr$(42) Then
                            Print "DELETING "; memberName$(XW); " STATS"
                            statFile$ = diskPaths$(1) + memberName$(XW)
                            If _FileExists(statFile$) Then Kill statFile$
                            If _FileExists(statFile$ + "S") Then Kill statFile$ + "S"
                        End If
                    Next

                End If 'done checking for XXX

                leaderFile$ = diskPaths$(2) + memberConf$(X) + ".LDR"
                If _FileExists(leaderFile$) Then Kill leaderFile$

                standingsFile$ = diskPaths$(2) + memberConf$(X) + ".LDS"
                If _FileExists(standingsFile$) Then Kill standingsFile$

            Next

        End If 'Done confirming to move forward with deletion
    
    End If

End Sub
