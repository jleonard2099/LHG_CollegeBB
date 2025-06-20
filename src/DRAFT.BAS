'----------------------------------------
'            DRAFT Subroutine
'----------------------------------------
'This subroutine presents a menu to the
'user with choices for drafting players.
Sub DRAFT ()

    _Title "Courtside College Basketball - Team Draft"

    Shared d3FG_Adj(), d3FGA_Adj()
    Shared dFGPctAdj_DRAFT!()

    Shared leagRat_DRAFT()
    Shared plyrOff_DRAFT!(), plyrRat_DRAFT!()
    Shared stamina_DRAFT!(), teamRat_DRAFT()

    Shared tmYears_DRAFT$(), plyrNames_DRAFT$(), tmName_DRAFT$()

    ReDim menuChoices$(1 To 3)

    Do

        Call TITLE

        menuChoices$(1) = "DRAFT A NEW TEAM         "
        menuChoices$(2) = "DRAFT TO AN EXISTING TEAM"
        menuChoices$(3) = "EXIT TO MAIN MENU        "
        SELECTION% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1:

                    Erase dFGPctAdj_DRAFT!, d3FGA_Adj, d3FG_Adj
                    Erase leagRat_DRAFT, plyrOff_DRAFT!, plyrRat_DRAFT!
                    Erase stamina_DRAFT!, teamRat_DRAFT

                    For I = 0 To 1

                        tmName_DRAFT$(I) = "XXX"

                        For I1 = 0 To 13
                            plyrNames_DRAFT$(I, I1) = "XXX"
                        Next

                    Next

                    draftIdx = 0
                    Call NewTeamParameters(tmYears_DRAFT$(draftIdx), tmName_DRAFT$(draftIdx), teamIdx)
                    Call DraftChoices(1, doneDraft, teamIdx)

                Case 2:
                    draftIdx = 0
                    Call ChooseDraftTeam(tmYears_DRAFT$(draftIdx), NTMS, teamIdx, tmName_DRAFT$(draftIdx), draftIdx)
                    If teamIdx > 0 Then 
                        Call DraftChoices(0, doneDraft, teamIdx)
                    End If

            End Select

        End If

        If doneDraft = 1 Then SELECTION% = 3

    Loop Until SELECTION% = 3 Or ExitCode% = 27

    '1 Exit Sub
End Sub

'----------------------------------------
'      NewTeamParameters Subroutine
'----------------------------------------
'This subroutine prompts the user for the
'minimum required parameters to create a
'new team that players will be drafted to.
Sub NewTeamParameters (draftYear$, teamName$, teamIdx)

    Shared teamRat_DRAFT()

    Color 15, 0
    Cls

    Color 14
    Print "TEAM INFORMATION"
    Color 15

    Locate 3: Input "1. TEAM NUMBER (0 = AUTO) "; teamIdx
    Locate 4: Input "2. DISK ID "; draftYear$
    Locate 5: Input "3. TEAM NAME "; teamName$
    Locate 6: Input "4. HOMECOURT FACTOR (0-6) "; teamRat_DRAFT(0, 6)
    Locate 7: Input "5. TEXT COLOR (0-15) "; teamRat_DRAFT(0, 0)
    Locate 8: Input "6. BACKGROUND COLOR (0-7) "; teamRat_DRAFT(0, 1)

    If teamRat_DRAFT(0, 0) >= 0 And teamRat_DRAFT(0, 1) >= 0 Then
        Locate 10, 1
        Print "TEAM APPEARANCE"
        Color teamRat_DRAFT(0, 0), teamRat_DRAFT(0, 1)
        Print teamName$
        Color 15, 0
    End If

    Do

        Do
            Locate 20, 53
            Print Space$(10)
            Locate 20, 25
            Input "# TO CHANGE (0 TO CONTINUE) "; B9
        Loop Until B9 >= 0 And B9 <= 6

        If B9 <> 0 Then

            Select Case B9

                Case 1
                    Locate 3, 27: Print Space$(10)
                    Locate 3, 1: Input "1. TEAM NUMBER (0 = AUTO) "; teamIdx
                    Locate 3, 27: Print " "

                Case 2
                    Locate 4, 1: Input "2. DISK ID "; draftYear$
                    Locate 4, 12: Print " "

                Case 3
                    Locate 5, 1: Input "3. TEAM NAME "; teamName$
                    Locate 5, 14: Print " "

                Case 4
                    Locate 6, 27: Print Space$(10)
                    Locate 6, 1: Input "4. HOMECOURT FACTOR (0-6) "; teamRat_DRAFT(0, 6)
                    Locate 6, 27: Print " "

                Case 5
                    Locate 7, 22: Print Space$(10)
                    Locate 7, 1: Input "5. TEXT COLOR (0-15) "; teamRat_DRAFT(0, 0)
                    Locate 7, 22: Print " "

                    If teamRat_DRAFT(0, 0) >= 0 And teamRat_DRAFT(0, 1) >= 0 Then
                        Locate 10, 1
                        Print "TEAM APPEARANCE"
                        Color teamRat_DRAFT(0, 0), teamRat_DRAFT(0, 1)
                        Print RTrim$(teamName$)
                        Color 15, 0
                    End If

                Case 6
                    Locate 8, 27: Print Space$(10)
                    Locate 8, 1: Input "6. BACKGROUND COLOR (0-7) "; teamRat_DRAFT(0, 1)
                    Locate 8, 27: Print " "

                    If teamRat_DRAFT(0, 0) >= 0 And teamRat_DRAFT(0, 1) >= 0 Then
                        Locate 10, 1
                        Print "TEAM APPEARANCE"
                        Color teamRat_DRAFT(0, 0), teamRat_DRAFT(0, 1)
                        Print RTrim$(teamName$)
                        Color 15, 0
                    End If

            End Select

        End If

    Loop Until B9 = 0

    If teamRat_DRAFT(0, 6) < 0 Then teamRat_DRAFT(0, 6) = 0
    If teamRat_DRAFT(0, 6) > 6 Then teamRat_DRAFT(0, 6) = 6

End Sub


'----------------------------------------
'     DraftChoices Subroutine
'----------------------------------------
'This subroutine provides an interface the
'user can operate to choose their draft
'selections.
'
'draftNew
'  0 = Existing Team
'  1 = New Team
Sub DraftChoices (draftNew, doneDrafting, teamPosn)

    Shared teamRat_DRAFT(), avgRat_DRAFT()

    Shared plyrDef_DRAFT!(), plyrStat_DRAFT!()
    Shared plyrOff_DRAFT!(), plyrRat_DRAFT!()

    Shared posn_DRAFT$(), tmYears_DRAFT$()
    Shared plyrNames_DRAFT$(), tmName_DRAFT$()

    Do

        Color 15, 0
        Cls

        Color teamRat_DRAFT(0, 0), teamRat_DRAFT(0, 1): Print tmName_DRAFT$(0);

        Color 14, 0
        Print "     (DRAFTING)"
        Print
        Print "PLAYERS"

        For I = 0 To 13
            Color 2: Print Using "## "; I;
            Color 15: Print plyrNames_DRAFT$(0, I)
        Next

        Do
            Locate 21, 1
            Color 14: Print "(1) ";
            Color 15: Print "DRAFT PLAYER"
            Color 14: Print "(2) ";
            Color 15: Print "SAVE TEAM"

            I$ = GetKeyPress$
            I1 = Val(I$)
        Loop Until I1 >= 1 And I1 <= 2

        Select Case I1

            Case 1

                Do
                    Locate 21, 20: Print "SLOT # (0-13) ";
                    N = 1
                    Input slot
                Loop Until slot >= 0 And slot <= 14

                draftIdx = 1
                Call ChooseDraftTeam(tmYears_DRAFT$(draftIdx), NTMS, destIdx, tmName_DRAFT$(draftIdx), draftIdx)

                Color , 0
                Cls
                Color teamRat_DRAFT(draftIdx, 0), teamRat_DRAFT(draftIdx, 1): Print tmName_DRAFT$(draftIdx)
                Color 14, 0

                Print
                Print "PLAYERS"

                For I = 0 To 13
                    Color 2: Print Using "## "; I;
                    Color 15: Print plyrNames_DRAFT$(draftIdx, I)
                Next

                Do
                    Color 14
                    Locate 21, 1: Print "SELECT PLAYER # TO DRAFT ";
                    Input player
                Loop Until player >= 0 And player <= 13

                plyrNames_DRAFT$(0, slot) = plyrNames_DRAFT$(1, player)
                plyrDef_DRAFT!(0, slot) = plyrDef_DRAFT!(1, player)
                posn_DRAFT$(0, slot) = posn_DRAFT$(1, player)

                For I = 0 To 13
                    plyrStat_DRAFT!(0, slot, I) = plyrStat_DRAFT!(1, player, I)
                Next

                For I = 1 To 12
                    avgRat_DRAFT(0, slot, I) = avgRat_DRAFT(1, player, I)
                Next

                For I = 0 To 6
                    plyrOff_DRAFT!(0, slot, I) = plyrOff_DRAFT!(1, player, I)
                    plyrRat_DRAFT!(0, slot, I) = plyrRat_DRAFT!(1, player, I)
                Next

            Case 2

                Do

                    Cls
                    Color teamRat_DRAFT(0, 0), teamRat_DRAFT(0, 1): Print tmName_DRAFT$(0)
                    Color 14, 0
                    Print "PLAYER          CONTRIBUTION"
                    Print "                PERCENT"
                    Color 15, 0

                    T3 = 0

                    For I = 1 To 14
                        Print Using "## "; I;: Print plyrNames_DRAFT$(0, I - 1);
                        Locate , 17: Print plyrOff_DRAFT!(0, I - 1, 3)
                        Locate , 1
                        T3 = T3 + plyrOff_DRAFT!(0, I - 1, 3)
                    Next I

                    Print
                    Color 14: Print "TOTAL CP ";
                    Locate , 17
                    Color 15, 0: Print T3

                    Do

                        Locate 21, 46
                        Print Space$(10);
                        Locate 21, 1
                        Input "SELECT PLAYER # TO CHANGE (0-NO MORE CHANGE) "; X9

                        If X9 = 0 Then

                            If T3 <= 98 Or T3 >= 101 Then

                                Cls

                                Call box0(6, 8, 11, 70, 2, 15, 1)

                                Locate 7

                                U$ = "TOTAL CONTRIBUTION MUST BE 100 %"
                                Locate , (80 - Len(U$)) / 2
                                Print U$

                                U$ = "HIT ANY KEY TO CONTINUE"
                                Locate , (80 - Len(U$)) / 2
                                Print U$

                                Color 15, 0
                                I$ = GetKeyPress$
                            End If 'End checking total

                        Else

                            Locate 22, 18: Print plyrNames_DRAFT$(0, X9 - 1)
                            Locate 23, 1
                            Input "ENTER CP: "; plyrOff_DRAFT!(0, X9 - 1, 3)

                        End If 'End checking X9

                    Loop Until X9 >= 0 And X9 <= 14

                Loop Until X9 = 0 And (T3 > 98 And T3 < 101)

                Call SaveTeamDraft(draftNew, tmYears_DRAFT$(0), tmName_DRAFT$(0), teamPosn)

        End Select

    Loop Until I1 = 2

    Color 15, 0
    Cls

    Call box0(6, 8, 8, 70, 2, 15, 1)

    Locate 7

    U$ = "DRAFT ANOTHER TEAM? (Y/N)"

    Do
        Locate 7, (80 - Len(U$)) / 2
        Print U$
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    If UCase$(I$) = "N" Then
        doneDrafting = 1
    End If

End Sub


'----------------------------------------
'       SaveTeamDraft Subroutine
'----------------------------------------
'This subroutine updates the source team
'with the newly assigned player / stat
'values after the draft.
'
'draftNew
'  0 = Existing Team
'  1 = New Team
Sub SaveTeamDraft (draftNew, teamYear$, teamName$, teamPosn)

    Shared plyrNames_DRAFT$(), posn_DRAFT$(), tmName_DRAFT$()
    Shared mascots_DRAFT$(), coaches_DRAFT$(), stadiums_DRAFT$()

    Shared plyrOff_DRAFT!(), tmAtt_DRAFT&(), dFGPctAdj_DRAFT!(), plyrDef_DRAFT!()
    Shared plyrStat_DRAFT!(), plyrRat_DRAFT!(), stamina_DRAFT!()

    Shared leagRat_DRAFT(), teamRat_DRAFT(), teamStats_DRAFT!()
    Shared avgRat_DRAFT(), d3FG_Adj(), d3FGA_Adj()

    Cls

    numPlayers = 0

    For I = 0 To 13

        If plyrNames_DRAFT$(0, I) <> "XXX" Then

            numPlayers = numPlayers + 1
            stamina_DRAFT!(0) = stamina_DRAFT!(0) + avgRat_DRAFT(0, I, 1)
            dFGPctAdj_DRAFT!(0) = dFGPctAdj_DRAFT!(0) + avgRat_DRAFT(0, I, 2)
            d3FGA_Adj(0) = d3FGA_Adj(0) + avgRat_DRAFT(0, I, 3)
            d3FG_Adj(0) = d3FG_Adj(0) + avgRat_DRAFT(0, I, 4)
            leagRat_DRAFT(0, 0) = leagRat_DRAFT(0, 0) + avgRat_DRAFT(0, I, 5)
            leagRat_DRAFT(0, 2) = leagRat_DRAFT(0, 2) + avgRat_DRAFT(0, I, 6)
            leagRat_DRAFT(0, 3) = leagRat_DRAFT(0, 3) + avgRat_DRAFT(0, I, 7)
            teamRat_DRAFT(0, 3) = teamRat_DRAFT(0, 3) + avgRat_DRAFT(0, I, 9)
            teamRat_DRAFT(0, 4) = teamRat_DRAFT(0, 4) + avgRat_DRAFT(0, I, 10)
            teamRat_DRAFT(0, 5) = teamRat_DRAFT(0, 5) + avgRat_DRAFT(0, I, 11)
            teamRat_DRAFT(0, 6) = teamRat_DRAFT(0, 6) + avgRat_DRAFT(0, I, 12)

        End If

    Next

    stamina_DRAFT!(0) = stamina_DRAFT!(0) / (numPlayers - 1)
    dFGPctAdj_DRAFT!(0) = dFGPctAdj_DRAFT!(0) / (numPlayers - 1)

    d3FGA_Adj(0) = d3FGA_Adj(0) / (numPlayers - 1)
    If d3FGA_Adj(0) > 20 Then d3FGA_Adj(0) = 100 - d3FGA_Adj(0)

    d3FG_Adj(0) = d3FG_Adj(0) / (numPlayers - 1)
    If d3FG_Adj(0) > 20 Then d3FG_Adj(0) = 100 - d3FG_Adj(0)

    leagRat_DRAFT(0, 0) = leagRat_DRAFT(0, 0) / (numPlayers - 1)
    If leagRat_DRAFT(0, 0) > 20 Then leagRat_DRAFT(0, 0) = 100 - leagRat_DRAFT(0, 0)

    leagRat_DRAFT(0, 2) = leagRat_DRAFT(0, 2) / (numPlayers - 1)
    If leagRat_DRAFT(0, 2) > 20 Then leagRat_DRAFT(0, 2) = 100 - leagRat_DRAFT(0, 2)

    leagRat_DRAFT(0, 3) = leagRat_DRAFT(0, 3) / (numPlayers - 1)
    If leagRat_DRAFT(0, 3) > 20 Then leagRat_DRAFT(0, 3) = 100 - leagRat_DRAFT(0, 3)

    leagRat_DRAFT(0, 4) = 99
    teamRat_DRAFT(0, 3) = teamRat_DRAFT(0, 3) / (numPlayers - 1)
    teamRat_DRAFT(0, 4) = teamRat_DRAFT(0, 4) / (numPlayers - 1)
    teamRat_DRAFT(0, 5) = teamRat_DRAFT(0, 5) / (numPlayers - 1)
    teamRat_DRAFT(0, 6) = teamRat_DRAFT(0, 6) / (numPlayers - 1)

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + teamYear$ For Random As #1 Len = TEAM_SIZE_BYTES

    fileLength& = LOF(1)

    If draftNew = 1 Then 
        teamPosn = (fileLength& / TEAM_SIZE_BYTES) + 1
    End If

    Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 2 As Q$(16), 2 As Q$(17), 2 As Q$(18), 2 As Q$(19), 602 As X$
    For I = 0 To 13
        Field #1, 53 + I * 43 As X$, 15 As Q$(I * 15 + 20), 2 As Q$(I * 15 + 21), 2 As Q$(I * 15 + 22), 2 As Q$(I * 15 + 23), 2 As Q$(I * 15 + 24), 2 As Q$(I * 15 + 25), 2 As Q$(I * 15 + 26), 2 As Q$(I * 15 + 27), 2 As Q$(I * 15 + 28), 2 As Q$(I * 15 + 29), 2 As Q$(I * 15 + 30), 2 As Q$(I * 15 + 31), 2 As Q$(I * 15 + 32), 2 As Q$(I * 15 + 33), 2 As Q$(I * 15 + 34), TEAM_SIZE_BYTES - 53 - 43 - I * 43 As X$
    Next

    LSet Q$(0) = teamName$
    LSet Q$(1) = MKI$(stamina_DRAFT!(0))
    LSet Q$(2) = MKI$(dFGPctAdj_DRAFT!(0))
    LSet Q$(3) = MKI$(d3FG_Adj(0))
    LSet Q$(4) = MKI$(d3FGA_Adj(0))

    For I = 0 To 4:
        LSet Q$(I + 5) = MKI$(leagRat_DRAFT(0, I))
    Next

    teamRat_DRAFT(0, 2) = 999

    For I = 0 To 9:
        LSet Q$(I + 10) = MKI$(teamRat_DRAFT(0, I))
    Next

    For I = 0 To 13

        LSet Q$(I * 15 + 20) = plyrNames_DRAFT$(0, I)

        For I1 = 0 To 6
            LSet Q$(I * 15 + I1 + 21) = MKI$(plyrOff_DRAFT!(0, I, I1))
            LSet Q$(I * 15 + I1 + 28) = MKI$(plyrRat_DRAFT!(0, I, I1))
        Next

    Next

    Put #1, teamPosn

    Close #1

    If draftNew = 0 And Not _FileExists(diskPaths$(0) + DATA_FILE_NAME$ + "." + teamYear$) Then

        MsgText$ = "WARNING!!! NO DATA FILE WAS FOUND FOR THIS TEAM. PRESS ANY KEY TO CONTINUE"
        BoxWidth = 20
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, PASS)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, PASS)

    Else

        Open diskPaths$(0) + DATA_FILE_NAME$ + "." + teamYear$ For Random As #2 Len = DATA_SIZE_BYTES

        Field #2, 15 As Q$(0), 40 As Q$(1), 15 As Q$(2), 4 As Q$(3)

        For I = 0 To 13
            Field #2, (74 + I * 32) As X$, 2 As Q$(I * 16 + 4), 2 As Q$(I * 16 + 5), 2 As Q$(I * 16 + 6), 2 As Q$(I * 16 + 7), 2 As Q$(I * 16 + 8), 2 As Q$(I * 16 + 9), 2 As Q$(I * 16 + 10), 2 As Q$(I * 16 + 11), 2 As Q$(I * 16 + 12), 2 As Q$(I * 16 + 13), 2 As Q$(I * 16 + 14), 2 As Q$(I * 16 + 15), 2 As Q$(I * 16 + 16), 2 As Q$(I * 16 + 17), 2 As Q$(I * 16 + 18), 2 As Q$(I * 16 + 19)
        Next

        Field #2, 522 As X$, 2 As Q$(228), 2 As Q$(229), 2 As Q$(230), 2 As Q$(231), 2 As Q$(232), 2 As Q$(233), 2 As Q$(234), 2 As Q$(235), 2 As Q$(236), 2 As Q$(237), 2 As Q$(238), 2 As Q$(239), 2 As Q$(240), 2 As Q$(241), 2 As Q$(242), 2 As Q$(243), 2 As Q$(244), 2 As Q$(245), 2 As Q$(246), 2 As Q$(247), 2 As Q$(248), 2 As Q$(249), 2 As Q$(250), 2 As Q$(251), 2 As Q$(252), 2 As Q$(253)

        LSet Q$(0) = coaches_DRAFT$(0)
        LSet Q$(1) = stadiums_DRAFT$(0)
        LSet Q$(2) = mascots_DRAFT$(0)
        LSet Q$(3) = MKL$(tmAtt_DRAFT&(0))

        For I = 0 To 13

            LSet Q$(I * 16 + 4) = MKI$(plyrDef_DRAFT!(0, I))
            LSet Q$(I * 16 + 5) = posn_DRAFT$(0, I)

            For I1 = 0 To 13
                LSet Q$(I * 16 + I1 + 6) = MKI$(plyrStat_DRAFT!(0, I, I1))
            Next

        Next

        For I = 0 To 24
            LSet Q$(228 + I) = MKI$(teamStats_DRAFT!(0, I))
        Next

        LSet Q$(253) = MKI$(999)

        Put #2, teamPosn

        Close #2

    End If

End Sub


'----------------------------------------
'     ChooseDraftTeam Subroutine
'----------------------------------------
'This subroutine provides an interface for
'choosing the team to draft players from
Sub ChooseDraftTeam (yearNumber$, numberTeams, chosenIdx, teamName$, draftIdx)

    Shared previousYear$

    Shared teamRat_DRAFT()

    Do

        chosenIdx = 0

        Color 15, 0
        Cls

        Call MenuBox(6, 8, 61, 7, 15, 4)

        Locate 7, 32

        Locate , 12: Print "ENTER DISK ID: "
        Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Print
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
        Locate 8,

        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then

            If I$ <> Chr$(13) Then

                Locate 7, 27: Print I$;

                NY$ = ""

                Do Until NY$ = Chr$(13)

                    NY$ = GetKeyPress$

                    If NY$ = Chr$(8) And Len(I$) >= 1 Then
                        col% = Pos(0)
                        I$ = Left$(I$, (Len(I$) - 1))
                        Locate , col% - 1
                        Print " ";
                        Locate , col% - 1
                    Else
                        I$ = I$ + NY$
                        Print NY$;
                    End If

                Loop

                I$ = Left$(I$, (Len(I$) - 1))

                If Not (I$ = "" And yearNumber$ <> "") Then

                    yearNumber$ = RTrim$(I$)
                    Erase teamNames$
                    numberTeams = 0

                    Cls

                    If Not _FileExists(diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$) Then

                        MsgText$ = "NO TEAM FILES COULD BE FOUND MATCHING THIS ID. PRESS ANY KEY TO RETURN TO THE MAIN MENU"
                        BoxWidth = 24
                        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                        Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                        Call CSMsgBox(MsgText$, BoxWidth, PASS)
                        I$ = GetKeyPress$
                        Call CSMsgBox("", 0, PASS)
                        Exit Sub

                    Else

                        Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$ For Random As #1 Len = TEAM_SIZE_BYTES

                        fileLength& = LOF(1)

                        Field #1, 15 As Q$(0)

                        numberTeams = fileLength& / TEAM_SIZE_BYTES

                        For teamIdx = 1 To numberTeams
                            Get #1, teamIdx
                            'numberTeams = numberTeams + 1
                            teamNames$(teamIdx) = Q$(0)
                            teamIndex(teamIdx) = teamIdx
                        Next teamIdx

                        Close #1

                    End If 'Done checking if file exists

                End If 'Done checking for blank year number

                previousYear$ = yearNumber$

            Else

                yearNumber$ = previousYear$

            End If 'Done checking for ENTER key

            'Locate 2, 33
            'Color 31: Print "LOADING..."

            Do

                Call SelectDraftTeam(numberTeams, chosenIdx, ExitCode%, draftIdx)

                If ExitCode% <> 27 And numberTeams <> 0 And chosenIdx > 0 Then

                    Do
                        YCoord = Int((80 - Len(teamName$)) / 2)
                        Locate 2, YCoord
                        Color teamRat_DRAFT(draftIdx, 0), teamRat_DRAFT(draftIdx, 1): Print teamName$

                        Color 15, 4
                        Locate 3, 32: Print "ANY CHANGE? (Y/N)";

                        J$ = GetKeyPress$

                    Loop Until J$ = Chr$(27) Or UCase$(J$) = "Y" Or UCase$(J$) = "N"

                End If

            Loop Until ExitCode% = 27 Or J$ = Chr$(27) Or UCase$(J$) = "N" Or chosenIdx = -1

        End If 'Done checking for ESC

    Loop Until UCase$(J$) = "N" Or I$ = Chr$(27) 'Or J$ = Chr$(27)

    'If I$ = Chr$(27) Or J$ = Chr$(27) Then chosenIdx = 0

End Sub


'----------------------------------------
'       SelectDraftTeam Subroutine
'----------------------------------------
'This subroutine provides an interface for
'selecting the team to draft players from
Sub SelectDraftTeam (numberTeams, selectedIdx, ExitCode%, draftIdx)

    ExitCode% = -1
    validData = 0

    If numberTeams > 0 And CheckString%(teamNames$(numberTeams)) = 0 Then

        SELECTION% = 1
        Count% = numberTeams
        Start% = 1
        FileFGColor = 15: FileBGColor = 4
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Rows% = LAST_ROW - 6
        Columns% = 4
        Spacing% = 6
        Row% = 5
        Column% = 1

        teamNumber = draftIdx + 1

        Color , 4
        Cls

        Color 14
        Locate , 33: Print "TEAM SELECTION"
        Locate LAST_ROW, 1
        Print Tab(3); "PG DN";
        Print Tab(18); "ENTER";
        Color 15: Print "-SELECTS TEAM";
        Color 14
        Print Tab(39); "ESC";
        Color 15: Print "-SELECT NEW ID";
        Color 14
        Print Tab(72); "PG UP";

        _MouseShow "DEFAULT"
        MAMenu teamNames$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Rows%, Columns%, Spacing%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            selectedIdx = teamIndex(SELECTION%)

            'Locate 2, 33
            'Color 31: Print "LOADING..."

            Call ReadDraftTeam(selectedIdx, draftIdx)
            Call ReadDraftExtraData(selectedIdx, draftIdx, validData)

        End If

    End If

    If validData = 0 And ExitCode% <> 27 Then

        selectedIdx = -1
        Locate 9,
        Call CSMsgBox("THERE WAS AN ISSUE READING TEAM DATA. YOU MAY HAVE CHOSEN A TEAM FROM 4.0... PRESS ANY KEY AND TRY ANOTHER DISK ID", 20, PASS)
        K$ = GetKeyPress$
        Call CSMsgBox("", 0, PASS)

    End If

End Sub


'----------------------------------------
'       ReadDraftTeam SubRoutine
'----------------------------------------
'This subroutine reads team data into
'arrays for the selected team
Sub ReadDraftTeam (teamIdx, draftIdx)

    Shared tmAtt_DRAFT&(), CK!()
    Shared dFGPctAdj_DRAFT!(), d3FG_Adj(), d3FGA_Adj()
    Shared leagRat_DRAFT(), avgRat_DRAFT()
    Shared plyrOff_DRAFT!(), plyrDef_DRAFT!()
    Shared plyrRat_DRAFT!(), plyrStat_DRAFT!(), stamina_DRAFT!()

    Shared teamRat_DRAFT(), teamStats_DRAFT!()

    Shared plyrNames_DRAFT$(), tmName_DRAFT$(), posn_DRAFT$(), tmYears_DRAFT$()
    Shared mascots_DRAFT$(), coaches_DRAFT$(), stadiums_DRAFT$()

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + tmYears_DRAFT$(draftIdx) For Random As #1 Len = TEAM_SIZE_BYTES

    Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 2 As Q$(16), 2 As Q$(17), 2 As Q$(18), 2 As Q$(19), 602 As X$

    For I = 0 To 13
        Field #1, 53 + I * 43 As X$, 15 As Q$(I * 15 + 20), 2 As Q$(I * 15 + 21), 2 As Q$(I * 15 + 22), 2 As Q$(I * 15 + 23), 2 As Q$(I * 15 + 24), 2 As Q$(I * 15 + 25), 2 As Q$(I * 15 + 26), 2 As Q$(I * 15 + 27), 2 As Q$(I * 15 + 28), 2 As Q$(I * 15 + 29), 2 As Q$(I * 15 + 30), 2 As Q$(I * 15 + 31), 2 As Q$(I * 15 + 32), 2 As Q$(I * 15 + 33), 2 As Q$(I * 15 + 34), TEAM_SIZE_BYTES - 53 - 43 - I * 43 As X$
    Next

    Get #1, teamIdx

    tmName_DRAFT$(draftIdx) = RTrim$(Q$(0))
    stamina_DRAFT!(draftIdx) = CVI(Q$(1))
    dFGPctAdj_DRAFT!(draftIdx) = CVI(Q$(2))
    d3FG_Adj(draftIdx) = CVI(Q$(3))
    d3FGA_Adj(draftIdx) = CVI(Q$(4))

    For I = 0 To 4:
        leagRat_DRAFT(draftIdx, I) = CVI(Q$(I + 5))
    Next

    For I = 0 To 9:
        teamRat_DRAFT(draftIdx, I) = CVI(Q$(I + 10))
    Next

    For I = 0 To 13

        'Player name
        plyrNames_DRAFT$(draftIdx, I) = RTrim$(Q$(I * 15 + 20))

        'Player ratings
        For I1 = 0 To 6
            plyrOff_DRAFT!(draftIdx, I, I1) = CVI(Q$(I * 15 + I1 + 21))
            plyrRat_DRAFT!(draftIdx, I, I1) = CVI(Q$(I * 15 + I1 + 28))
        Next

    Next

    'Close team file
    Close #1

    If d3FG_Adj(draftIdx) > 20 Then d3FG_Adj(draftIdx) = 100 - d3FG_Adj(draftIdx)
    If d3FGA_Adj(draftIdx) > 20 Then d3FGA_Adj(draftIdx) = 100 - d3FGA_Adj(draftIdx)
    If leagRat_DRAFT(draftIdx, 0) > 20 Then leagRat_DRAFT(draftIdx, 0) = 100 - leagRat_DRAFT(draftIdx, 0)
    If leagRat_DRAFT(draftIdx, 2) > 20 Then leagRat_DRAFT(draftIdx, 2) = 100 - leagRat_DRAFT(draftIdx, 2)
    If leagRat_DRAFT(draftIdx, 3) > 20 Then leagRat_DRAFT(draftIdx, 3) = 100 - leagRat_DRAFT(draftIdx, 3)

    For I = 0 To 13
        avgRat_DRAFT(draftIdx, I, 1) = stamina_DRAFT!(draftIdx)
        avgRat_DRAFT(draftIdx, I, 2) = dFGPctAdj_DRAFT!(draftIdx)
        avgRat_DRAFT(draftIdx, I, 3) = d3FGA_Adj(draftIdx)
        avgRat_DRAFT(draftIdx, I, 4) = d3FG_Adj(draftIdx)
        avgRat_DRAFT(draftIdx, I, 5) = leagRat_DRAFT(draftIdx, 0)
        avgRat_DRAFT(draftIdx, I, 6) = leagRat_DRAFT(draftIdx, 2)
        avgRat_DRAFT(draftIdx, I, 7) = leagRat_DRAFT(draftIdx, 3)
        avgRat_DRAFT(draftIdx, I, 8) = 99
        avgRat_DRAFT(draftIdx, I, 9) = teamRat_DRAFT(draftIdx, 3)
        avgRat_DRAFT(draftIdx, I, 10) = teamRat_DRAFT(draftIdx, 4)
        avgRat_DRAFT(draftIdx, I, 11) = teamRat_DRAFT(draftIdx, 5)
        avgRat_DRAFT(draftIdx, I, 12) = teamRat_DRAFT(draftIdx, 6)
    Next


End Sub


'----------------------------------------
'     ReadDraftExtraData Subroutine
'----------------------------------------
'Reads in extra data for the selected team,
' given it's index in the team file.
'This extra file usually includes info.
' related to stadiums, coaches, etc.
'This routine is highly customized TO
' the specific game for which it has
' been included for.
Sub ReadDraftExtraData (teamIdx, draftIdx, dataOK)

    Shared CK!(), tmAtt_DRAFT&()

    Shared coaches_DRAFT$(), stadiums_DRAFT$(), mascots_DRAFT$()

    Shared plyrDef_DRAFT!(), plyrStat_DRAFT!(), teamStats_DRAFT!()

    Shared posn_DRAFT$(), tmYears_DRAFT$()

    If Not _FileExists(diskPaths$(0) + DATA_FILE_NAME$ + "." + tmYears_DRAFT$(draftIdx)) Then

        MsgText$ = "WARNING!!! NO DATA FILE WAS FOUND FOR THIS TEAM. PRESS ANY KEY TO CONTINUE"
        BoxWidth = 20
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, PASS)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, PASS)

    Else

        Open diskPaths$(0) + DATA_FILE_NAME$ + "." + tmYears_DRAFT$(draftIdx) For Random As #2 Len = DATA_SIZE_BYTES

        Field #2, 15 As Q$(0), 40 As Q$(1), 15 As Q$(2), 4 As Q$(3)

        For I = 0 To 13
            Field #2, 74 + I * 32 As X$, 2 As Q$(I * 16 + 4), 2 As Q$(I * 16 + 5), 2 As Q$(I * 16 + 6), 2 As Q$(I * 16 + 7), 2 As Q$(I * 16 + 8), 2 As Q$(I * 16 + 9), 2 As Q$(I * 16 + 10), 2 As Q$(I * 16 + 11), 2 As Q$(I * 16 + 12), 2 As Q$(I * 16 + 13), 2 As Q$(I * 16 + 14), 2 As Q$(I * 16 + 15), 2 As Q$(I * 16 + 16), 2 As Q$(I * 16 + 17), 2 As Q$(I * 16 + 18), 2 As Q$(I * 16 + 19)
        Next

        Field #2, 522 As X$, 2 As Q$(228), 2 As Q$(229), 2 As Q$(230), 2 As Q$(231), 2 As Q$(232), 2 As Q$(233), 2 As Q$(234), 2 As Q$(235), 2 As Q$(236), 2 As Q$(237), 2 As Q$(238), 2 As Q$(239), 2 As Q$(240), 2 As Q$(241), 2 As Q$(242), 2 As Q$(243), 2 As Q$(244), 2 As Q$(245), 2 As Q$(246), 2 As Q$(247), 2 As Q$(248), 2 As Q$(249), 2 As Q$(250), 2 As Q$(251), 2 As Q$(252), 2 As Q$(253)

        Get #2, teamIdx

        coaches_DRAFT$(draftIdx) = RTrim$(Q$(0))
        stadiums_DRAFT$(draftIdx) = RTrim$(Q$(1))
        mascots_DRAFT$(draftIdx) = RTrim$(Q$(2))
        tmAtt_DRAFT&(draftIdx) = CVL(Q$(3))

        For I = 0 To 13

            plyrDef_DRAFT!(draftIdx, I) = CVI(Q$(I * 16 + 4))
            posn_DRAFT$(draftIdx, I) = Q$(I * 16 + 5)

            For I1 = 0 To 13
                plyrStat_DRAFT!(draftIdx, I, I1) = CVI(Q$(I * 16 + I1 + 6))
            Next

        Next

        For I = 0 To 20
            teamStats_DRAFT!(draftIdx, I) = CVI(Q$(228 + I))
        Next

        If tmAtt_DRAFT&(draftIdx) > 100000 Or ((CheckString%(stadiums_DRAFT$(draftIdx)) = 1) And stadiums_DRAFT$(draftIdx) <> "" ) Then
        'If attendance is abnormally high, we ARE
        'clearly not reading accurate data

            dataOK = 0

        Else

            For I = 21 To 24
                teamStats_DRAFT!(draftIdx, I) = CVI(Q$(228 + I))
            Next

            CK!(draftIdx) = CVI(Q$(253))

            If CK!(draftIdx) <> 999 Then

                coaches_DRAFT$(draftIdx) = ""
                stadiums_DRAFT$(draftIdx) = ""
                mascots_DRAFT$(draftIdx) = ""
                tmAtt_DRAFT&(draftIdx) = 0

                For I = 0 To 13:

                    plyrDef_DRAFT!(draftIdx, I) = 5
                    posn_DRAFT$(draftIdx, I) = ""

                    For I1 = 0 To 13
                        plyrStat_DRAFT!(draftIdx, I, I1) = 0
                    Next

                Next

                For I = 0 To 24
                    teamStats_DRAFT!(draftIdx, I) = 0
                Next

            End If

            dataOK = 1

        End If

        Close #2

    End If

End Sub
