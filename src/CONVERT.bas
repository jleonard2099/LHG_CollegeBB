'----------------------------------------
'       ReadExtraDataOld Subroutine
'----------------------------------------
'Reads in extra data for the selected team,
' given it's index in the team file.
'This extra file usually includes info.
' related to stadiums, coaches, etc.
'This routine is highly customized TO
' the specific game for which it has
' been included for.
Sub ReadExtraDataOld (fileName$, teamIdx)

    Shared CK, teamAttendance&

    Shared coach$, arenaName$, mascot$

    Shared playerDef!(), playerStats!(), teamStats!()

    Shared position$()

    If Not _FileExists(fileName$) Then

        MsgText$ = "WARNING!!! NO DATA FILE WAS FOUND FOR THIS TEAM. PRESS ANY KEY TO CONTINUE"
        BoxWidth = 20
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, PASS)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, PASS)

    Else

        Open fileName$ For Random As #1 Len = (DATA_SIZE_BYTES - 8)

        Field #1, 15 As Q$(0), 40 As Q$(1), 15 As Q$(2), 4 As Q$(3)

        For I = 0 To 13
            Field #1, 74 + I * 32 As X$, 2 As Q$(I * 16 + 4), 2 As Q$(I * 16 + 5), 2 As Q$(I * 16 + 6), 2 As Q$(I * 16 + 7), 2 As Q$(I * 16 + 8), 2 As Q$(I * 16 + 9), 2 As Q$(I * 16 + 10), 2 As Q$(I * 16 + 11), 2 As Q$(I * 16 + 12), 2 As Q$(I * 16 + 13), 2 As Q$(I * 16 + 14), 2 As Q$(I * 16 + 15), 2 As Q$(I * 16 + 16), 2 As Q$(I * 16 + 17), 2 As Q$(I * 16 + 18), 2 As Q$(I * 16 + 19)
        Next

        Field #1, 522 As X$, 2 As Q$(228), 2 As Q$(229), 2 As Q$(230), 2 As Q$(231), 2 As Q$(232), 2 As Q$(233), 2 As Q$(234), 2 As Q$(235), 2 As Q$(236), 2 As Q$(237), 2 As Q$(238), 2 As Q$(239), 2 As Q$(240), 2 As Q$(241), 2 As Q$(242), 2 As Q$(243), 2 As Q$(244), 2 As Q$(245), 2 As Q$(246), 2 As Q$(247), 2 As Q$(248), 2 As Q$(249)

        Get #1, teamIdx

        coach$ = RTrim$(Q$(0))
        arenaName$ = RTrim$(Q$(1))
        mascot$ = RTrim$(Q$(2))
        teamAttendance& = CVL(Q$(3))

        For I = 0 To 13

            playerDef!(I) = CVI(Q$(I * 16 + 4))
            position$(I) = Q$(I * 16 + 5)

            For I1 = 0 To 13
                playerStats!(I, I1) = CVI(Q$(I * 16 + I1 + 6))
            Next

        Next

        For I = 0 To 20
            teamStats!(I) = CVI(Q$(228 + I))
        Next

        CK = CVI(Q$(249))

        Close #1

    End If

End Sub


'----------------------------------------
'       SaveChangedData Subroutine
'----------------------------------------
'This routine saves the modified data back
'to the original team file
Sub SaveChangedData (fileName$, teamIdx)

    Shared teamAttendance&
    Shared mascot$, coach$, arenaName$

    Shared teamStats!()
    Shared position$()
    Shared playerDef!(), playerStats!()

    Open fileName$ For Random As #2 Len = DATA_SIZE_BYTES

    Field #2, 15 As Q$(0), 40 As Q$(1), 15 As Q$(2), 4 As Q$(3)

    For I = 0 To 13
        Field #2, 74 + I * 32 As X$, 2 As Q$(I * 16 + 4), 2 As Q$(I * 16 + 5), 2 As Q$(I * 16 + 6), 2 As Q$(I * 16 + 7), 2 As Q$(I * 16 + 8), 2 As Q$(I * 16 + 9), 2 As Q$(I * 16 + 10), 2 As Q$(I * 16 + 11), 2 As Q$(I * 16 + 12), 2 As Q$(I * 16 + 13), 2 As Q$(I * 16 + 14), 2 As Q$(I * 16 + 15), 2 As Q$(I * 16 + 16), 2 As Q$(I * 16 + 17), 2 As Q$(I * 16 + 18), 2 As Q$(I * 16 + 19)
    Next

    Field #2, 522 As X$, 2 As Q$(228), 2 As Q$(229), 2 As Q$(230), 2 As Q$(231), 2 As Q$(232), 2 As Q$(233), 2 As Q$(234), 2 As Q$(235), 2 As Q$(236), 2 As Q$(237), 2 As Q$(238), 2 As Q$(239), 2 As Q$(240), 2 As Q$(241), 2 As Q$(242), 2 As Q$(243), 2 As Q$(244), 2 As Q$(245), 2 As Q$(246), 2 As Q$(247), 2 As Q$(248), 2 As Q$(249), 2 As Q$(250), 2 As Q$(251), 2 As Q$(252), 2 As Q$(253)

    LSet Q$(0) = RTrim$(coach$)
    LSet Q$(1) = RTrim$(arenaName$)
    LSet Q$(2) = RTrim$(mascot$)
    LSet Q$(3) = MKL$(teamAttendance&)

    For I = 0 To 13

        LSet Q$(I * 16 + 4) = MKI$(playerDef!(I))
        LSet Q$(I * 16 + 5) = position$(I)

        For I1 = 0 To 13
            LSet Q$(I * 16 + I1 + 6) = MKI$(playerStats!(I, I1))
        Next

    Next

    For I = 0 To 24
        LSet Q$(228 + I) = MKI$(teamStats!(I))
    Next

    LSet Q$(253) = MKI$(999)

    Put #2, teamIdx

    Close #2

End Sub


'----------------------------------------
'       ConvertTeamData Subroutine
'----------------------------------------
' Reads in existing data
Sub ConvertTeamData ()

    Shared CK, teamAttendance&

    Shared coach$, arenaName$, mascot$

    Shared playerDef!(), playerStats!(), teamStats!()

    Shared position$()

    targetFile$ = _OpenFileDialog$("Select team file", diskPaths$(0), "COLBBTMS.*", "Team files")
    newFile$ = targetFile$ + ".NEW"

    If targetFile$ <> "" Then

        Open targetFile$ For Random As #1
        fileLength& = LOF(1)
        Close #1

        numberTeams = fileLength& / (DATA_SIZE_BYTES - 8)

        For currTeam = 1 To numberTeams

            Call ReadExtraDataOld(targetFile$, currTeam)

            ' Fill new "actuals" with 0's
            For I = 21 To 24
                'For I = 0 To 20
                teamStats!(I) = 0
            Next

            Call SaveChangedData(newFile$, currTeam)

        Next currTeam

        Kill targetFile$

        Call FCopy(newFile$, targetFile$, Buff$, copyErr%)

        Kill newFile$

        result& = _MessageBox("Success!", "The team file has been converted for use", "ok", "info", 1)

    End If

End Sub





Sub NotWorthy

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + teamYear$ For Random As #1 Len = TEAM_SIZE_BYTES

    fileLength& = LOF(1)

    If newID = 1 Or teamIdx = 0 Then teamIdx = fileLength& / TEAM_FILE_BYTES + 1

    Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 2 As Q$(16), 2 As Q$(17), 2 As Q$(18), 2 As Q$(19), 602 As X$

    For I = 0 To 13
        Field #1, 53 + I * 43 As X$, 15 As Q$(I * 15 + 20), 2 As Q$(I * 15 + 21), 2 As Q$(I * 15 + 22), 2 As Q$(I * 15 + 23), 2 As Q$(I * 15 + 24), 2 As Q$(I * 15 + 25), 2 As Q$(I * 15 + 26), 2 As Q$(I * 15 + 27), 2 As Q$(I * 15 + 28), 2 As Q$(I * 15 + 29), 2 As Q$(I * 15 + 30), 2 As Q$(I * 15 + 31), 2 As Q$(I * 15 + 32), 2 As Q$(I * 15 + 33), 2 As Q$(I * 15 + 34), TEAM_SIZE_BYTES - 53 - 43 - I * 43 As X$
    Next

    LSet Q$(0) = RTrim$(teamName$)
    LSet Q$(1) = MKI$(staminaRating!)
    LSet Q$(2) = MKI$(defFGPctAdj!)
    LSet Q$(3) = MKI$(def3FG_Adj)
    LSet Q$(4) = MKI$(def3FGA_Adj)

    For I = 0 To 4:
        LSet Q$(I + 5) = MKI$(leagueRatings%(I))
    Next

    teamRatings%(2) = 999

    For I = 0 To 9:
        LSet Q$(I + 10) = MKI$(teamRatings%(I))
    Next

    'Players
    For I = 0 To 13
        LSet Q$(I * 15 + 20) = playerNames$(I)
        For I1 = 0 To 6
            LSet Q$(I * 15 + I1 + 21) = MKI$(playerOff!(I, I1))
            LSet Q$(I * 15 + I1 + 28) = MKI$(playerRatings!(I, I1))
        Next
    Next

    Put #1, teamIdx

    Close #1

End Sub
