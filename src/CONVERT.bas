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

    Shared playerDef!(), playerOff!()
    Shared playerRatings!(), playerStats!(), teamStats!()

    Shared playerNames$(), position$()

    targetFile$ = _OpenFileDialog$("Select team file", diskPaths$(0), "COLBBTMS.*", "Team files")

    diskID$ = Right$(targetFile$, Len(targetFile$) - InStr(targetFile$, "COLBBTMS.") - 8)
    newFile$ = targetFile$ + ".NEW"

    If targetFile$ <> "" Then

        Open targetFile$ For Random As #1
        fileLength& = LOF(1)
        Close #1

        numberTeams = fileLength& / (DATA_SIZE_BYTES - 8)

        For currTeam = 1 To numberTeams

            Call ReadTeam(diskID$, nothing$, currTeam)
            Call ReadExtraDataOld(targetFile$, currTeam)

            ' Fill new "actuals" with 0's
            For I = 21 To 24
                teamStats!(I) = 0
            Next I

            'Calculate values from existing players
            For I = 0 To 13

                If playerNames$(I) <> "XXX" Then

                    '3FGA
                    teamStats!(21) = teamStats!(21) + playerStats!(I, 12)

                    '3FGM - based on 3FGA / PCT
                    '3FGM + (100 * 3FGA) / (3FG PCT)
                    playerFGM = (100 * playerStats!(I, 12)) / playerRatings!(I, 2)
                    tm3FGM = tm3FGM + playerFGM

                    'Blocks
                    teamStats!(23) = teamStats!(23) + playerStats!(I, 10)

                    'We can't "calculate" opponent blocks without having total somewhere
                    'teamStats!(24) = teamStats!(24) + playerOff!(I, _)

                End If

            Next I

            'Calculate 3FGA PCT now that we have the team whole stats
            teamStats!(22) = (tm3FGM / teamStats!(21)) * 100

            Call SaveChangedData(newFile$, currTeam)

        Next currTeam

        Kill targetFile$

        Call FCopy(newFile$, targetFile$, Buff$, copyErr%)

        Kill newFile$

        result& = _MessageBox("Success!", "The team file has been converted for use", "ok", "info", 1)

    End If

End Sub

