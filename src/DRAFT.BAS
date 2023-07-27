Sub DRAFT ()

    _Title "Courtside College Basketball - Team Draft"

    Shared d3FG_Adj(), d3FGA_Adj()
    Shared dFGPctAdj_DRAFT!()
    
    Shared lgRat_DRAFT()
    Shared plyrOff_DRAFT!(), plyrRat_DRAFT!()
    Shared stamina_DRAFT!(), teamRat_DRAFT()

    Shared teamYears$(), plyrNames_DRAFT$(), tmName_DRAFT$()

    ReDim menuChoices$(1 To 3)

    Do

        Call TITLE

        menuChoices$(1) = "DRAFT A NEW TEAM         "
        menuChoices$(2) = "DRAFT TO AN EXISTING TEAM"
        menuChoices$(3) = "EXIT TO MAIN MENU        "
        Selection% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 11, 50, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1:

                    For I = 0 To 1

                        tmName_DRAFT$(I) = "XXX"
                        stamina_DRAFT!(I) = 0
                        dFGPctAdj_DRAFT!(I) = 0
                        d3FG_Adj(I) = 0
                        d3FGA_Adj(I) = 0

                        For I1 = 0 To 4:
                            lgRat_DRAFT(I, I1) = 0
                        Next

                        For I1 = 0 To 9:
                            teamRat_DRAFT(I, I1) = 0
                        Next

                        For I1 = 0 To 13
                            plyrNames_DRAFT$(I, I1) = "XXX"
                            For I2 = 0 To 6
                                plyrOff_DRAFT!(I, I1, I2) = 0
                                plyrRat_DRAFT!(I, I1, I2) = 0
                            Next
                        Next

                    Next

                    draftIdx = 0
                    Call DraftNewTeam(teamYears$(draftIdx), tmName_DRAFT$(draftIdx), teamIdx)
                    Call DraftChoices(1, doneDraft, teamIdx)

                Case 2:
                    draftIdx = 0
                    Call ChooseDraftTeam(teamYears$(draftIdx), NTMS, teamIdx, tmName_DRAFT$(draftIdx), draftIdx)
                    Call DraftChoices(0, doneDraft, teamIdx)

            End Select

        End If

        If doneDraft = 1 Then Selection% = 3

    Loop Until Selection% = 3 Or ExitCode% = 27

    '1 Exit Sub
End Sub

'----------------------------------------
'       DraftNewTeam Subroutine
'----------------------------------------
'...explanation...
Sub DraftNewTeam (draftYear$, teamName$, teamIdx)

    Shared teamRat_DRAFT()

    Color 15, 0
    Cls

    Color 14
    Print "TEAM INFORMATION"
    Color 15

    Locate 3: Input " 1. TEAM NUMBER (0 = AUTO)"; teamIdx
    Locate 4: Input " 2. DISK ID "; draftYear$
    Locate 5: Input " 3. TEAM NAME "; teamName$
    Locate 6: Input " 4. HOMECOURT FACTOR (0-6) "; teamRat_DRAFT(0, 6)
    Locate 7: Input " 5. TEXT COLOR (0-15) "; teamRat_DRAFT(0, 0)
    Locate 8: Input " 6. BACKGROUND (0-7) "; teamRat_DRAFT(0, 1)
    Locate 10, 1
    Print "TEAM APPEARANCE";
    Color teamRat_DRAFT(0, 0), teamRat_DRAFT(0, 1)
    Print teamName$
    Color 15, 0

    Do

        Do
            Locate 20
            Input "# TO CHANGE (0 TO CONTINUE) "; B9
        Loop Until B9 >= 0 And B9 <= 6

        If B9 <> 0 Then

            Select Case B9

                Case 1
                    Locate 3: Input " 1. TEAM NUMBER (0 = AUTO)"; teamIdx

                Case 2
                    Locate 4: Input " 2. DISK ID "; draftYear$

                Case 3
                    Locate 5: Input " 3. TEAM NAME "; teamName$

                Case 4
                    Locate 6: Input " 4. HOMECOURT FACTOR (0-6) "; teamRat_DRAFT(0, 6)

                Case 5
                    Locate 7: Input " 5. TEXT COLOR (0-15) "; teamRat_DRAFT(0, 0)
                    Print "TEAM APPEARANCE";
                    Color teamRat_DRAFT(0, 0), teamRat_DRAFT(0, 1)
                    Print teamName$
                    Color 15, 0

                Case 6
                    Locate 8: Input " 6. BACKGROUND (0-7) "; teamRat_DRAFT(0, 1)
                    Print "TEAM APPEARANCE";
                    Color teamRat_DRAFT(0, 0), teamRat_DRAFT(0, 1)
                    Print teamName$
                    Color 15, 0

            End Select

        End If

    Loop Until B9 = 0

    If teamRat_DRAFT(0, 6) < 0 Then teamRat_DRAFT(0, 6) = 0
    If teamRat_DRAFT(0, 6) > 6 Then teamRat_DRAFT(0, 6) = 6

End Sub


'----------------------------------------
'     DraftChoices Subroutine
'----------------------------------------
'...explanation...
'draftNew
'0 = Existing Team
'1 = New Team
Sub DraftChoices (draftNew, doneDrafting, teamPosn)

    Shared teamRat_DRAFT(), miscRat_DRAFT()

    Shared plyrDef_DRAFT!(), plyrStat_DRAFT!()
    Shared plyrOff_DRAFT!(), plyrRat_DRAFT!()

    Shared posn_DRAFT$(), teamYears$()
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
                    Input B
                Loop Until B >= 0 And B <= 14

                draftIdx = 1
                Call ChooseDraftTeam(teamYears$(draftIdx), NTMS, destIdx, tmName_DRAFT$(draftIdx), draftIdx)

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
                    Input B1
                Loop Until B1 >= 0 And B1 <= 13

                plyrNames_DRAFT$(0, B) = plyrNames_DRAFT$(1, B1)
                plyrDef_DRAFT!(0, B) = plyrDef_DRAFT!(1, B1)
                posn_DRAFT$(0, B) = posn_DRAFT$(1, B1)

                For I = 0 To 13
                    plyrStat_DRAFT!(0, B, I) = plyrStat_DRAFT!(1, B1, I)
                Next

                For I = 1 To 12
                    miscRat_DRAFT(0, B, I) = miscRat_DRAFT(1, B1, I)
                Next

                For I = 0 To 6
                    plyrOff_DRAFT!(0, B, I) = plyrOff_DRAFT!(1, B1, I)
                    plyrRat_DRAFT!(0, B, I) = plyrRat_DRAFT!(1, B1, I)
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

                        Locate 21: Print "SELECT PLAYER TO CHANGE 1-14 (0 - NO MORE CHANGE) ";
                        Input X9

                        If X9 = 0 Then

                            If T3 <= 98 Or T3 >= 101 Then

                                Cls

                                JJ = 4

                                Locate 6, 8: Color 15, 1

                                Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";

                                For II = 1 To JJ: Locate 6 + II, 8
                                    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
                                Next

                                Locate 7 + JJ, 8

                                Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";

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

                Call SaveTeamDraft(draftNew, teamYears$(0), tmName_DRAFT$(0), teamPosn)

        End Select

    Loop Until I1 = 2

    Cls

    JJ = 1

    Locate 6, 8: Color 15, 1

    Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";

    For II = 1 To JJ: Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next

    Locate 7 + JJ, 8
    Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";

    Locate 7

    U$ = "DRAFT ANOTHER TEAM (YN)"

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
'...explanation...
Sub SaveTeamDraft (draftNew, teamYear$, teamName$, teamPosn)

    Shared plyrNames_DRAFT$(), posn_DRAFT$(), tmName_DRAFT$()
    Shared mascots_DRAFT$(), coaches_DRAFT$(), stadiums_DRAFT$()

    Shared plyrOff_DRAFT!(), att_DRAFT&(), dFGPctAdj_DRAFT!(), plyrDef_DRAFT!()
    Shared plyrStat_DRAFT!(), plyrRat_DRAFT!(), stamina_DRAFT!()

    Shared lgRat_DRAFT(), teamRat_DRAFT(), teamStats_DRAFT()
    Shared miscRat_DRAFT(), d3FG_Adj(), d3FGA_Adj()

    Cls

    NP% = 0

    For I = 0 To 13

        If plyrNames_DRAFT$(0, I) <> "XXX" Then

            NP% = NP% + 1
            stamina_DRAFT!(0) = stamina_DRAFT!(0) + miscRat_DRAFT(0, I, 1)
            dFGPctAdj_DRAFT!(0) = dFGPctAdj_DRAFT!(0) + miscRat_DRAFT(0, I, 2)
            d3FGA_Adj(0) = d3FGA_Adj(0) + miscRat_DRAFT(0, I, 3)
            d3FG_Adj(0) = d3FG_Adj(0) + miscRat_DRAFT(0, I, 4)
            lgRat_DRAFT(0, 0) = lgRat_DRAFT(0, 0) + miscRat_DRAFT(0, I, 5)
            lgRat_DRAFT(0, 2) = lgRat_DRAFT(0, 2) + miscRat_DRAFT(0, I, 6)
            lgRat_DRAFT(0, 3) = lgRat_DRAFT(0, 3) + miscRat_DRAFT(0, I, 7)
            teamRat_DRAFT(0, 3) = teamRat_DRAFT(0, 3) + miscRat_DRAFT(0, I, 9)
            teamRat_DRAFT(0, 4) = teamRat_DRAFT(0, 4) + miscRat_DRAFT(0, I, 10)
            teamRat_DRAFT(0, 5) = teamRat_DRAFT(0, 5) + miscRat_DRAFT(0, I, 11)
            teamRat_DRAFT(0, 6) = teamRat_DRAFT(0, 6) + miscRat_DRAFT(0, I, 12)

        End If

    Next

    stamina_DRAFT!(0) = stamina_DRAFT!(0) / (NP% - 1)
    dFGPctAdj_DRAFT!(0) = dFGPctAdj_DRAFT!(0) / (NP% - 1)

    d3FGA_Adj(0) = d3FGA_Adj(0) / (NP% - 1)
    If d3FGA_Adj(0) > 20 Then d3FGA_Adj(0) = 100 - d3FGA_Adj(0)

    d3FG_Adj(0) = d3FG_Adj(0) / (NP% - 1)
    If d3FG_Adj(0) > 20 Then d3FG_Adj(0) = 100 - d3FG_Adj(0)

    lgRat_DRAFT(0, 0) = lgRat_DRAFT(0, 0) / (NP% - 1)
    If lgRat_DRAFT(0, 0) > 20 Then lgRat_DRAFT(0, 0) = 100 - lgRat_DRAFT(0, 0)

    lgRat_DRAFT(0, 2) = lgRat_DRAFT(0, 2) / (NP% - 1)
    If lgRat_DRAFT(0, 2) > 20 Then lgRat_DRAFT(0, 2) = 100 - lgRat_DRAFT(0, 2)

    lgRat_DRAFT(0, 3) = lgRat_DRAFT(0, 3) / (NP% - 1)
    If lgRat_DRAFT(0, 3) > 20 Then lgRat_DRAFT(0, 3) = 100 - lgRat_DRAFT(0, 3)

    lgRat_DRAFT(0, 4) = 99
    teamRat_DRAFT(0, 3) = teamRat_DRAFT(0, 3) / (NP% - 1)
    teamRat_DRAFT(0, 4) = teamRat_DRAFT(0, 4) / (NP% - 1)
    teamRat_DRAFT(0, 5) = teamRat_DRAFT(0, 5) / (NP% - 1)
    teamRat_DRAFT(0, 6) = teamRat_DRAFT(0, 6) / (NP% - 1)

    Open diskPaths$(0) + "BASK." + teamYear$ For Random As #1 Len = 655

    fileLength& = LOF(1)

    If draftNew = 1 Then teamPosn = fileLength& / 655 + 1

    Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 2 As Q$(16), 2 As Q$(17), 2 As Q$(18), 2 As Q$(19), 602 As X$
    For I = 0 To 13
        Field #1, 53 + I * 43 As X$, 15 As Q$(I * 15 + 20), 2 As Q$(I * 15 + 21), 2 As Q$(I * 15 + 22), 2 As Q$(I * 15 + 23), 2 As Q$(I * 15 + 24), 2 As Q$(I * 15 + 25), 2 As Q$(I * 15 + 26), 2 As Q$(I * 15 + 27), 2 As Q$(I * 15 + 28), 2 As Q$(I * 15 + 29), 2 As Q$(I * 15 + 30), 2 As Q$(I * 15 + 31), 2 As Q$(I * 15 + 32), 2 As Q$(I * 15 + 33), 2 As Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 As X$
    Next

    LSet Q$(0) = teamName$
    LSet Q$(1) = MKI$(stamina_DRAFT!(0))
    LSet Q$(2) = MKI$(dFGPctAdj_DRAFT!(0))
    LSet Q$(3) = MKI$(d3FG_Adj(0))
    LSet Q$(4) = MKI$(d3FGA_Adj(0))

    For I = 0 To 4: 
        LSet Q$(I + 5) = MKI$(lgRat_DRAFT(0, I))
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

    Close 1

    Open diskPaths$(0) + "COLBBTMS." + teamYear$ For Random As #1 Len = 566

    Field #1, 15 As Q$(0), 40 As Q$(1), 15 As Q$(2), 4 As Q$(3)

    For I = 0 To 13
        Field #1, (74 + I * 32) As X$, 2 As Q$(I * 16 + 4), 2 As Q$(I * 16 + 5), 2 As Q$(I * 16 + 6), 2 As Q$(I * 16 + 7), 2 As Q$(I * 16 + 8), 2 As Q$(I * 16 + 9), 2 As Q$(I * 16 + 10), 2 As Q$(I * 16 + 11), 2 As Q$(I * 16 + 12), 2 As Q$(I * 16 + 13), 2 As Q$(I * 16 + 14), 2 As Q$(I * 16 + 15), 2 As Q$(I * 16 + 16), 2 As Q$(I * 16 + 17), 2 As Q$(I * 16 + 18), 2 As Q$(I * 16 + 19)
        Field #1, 522 As X$, 2 As Q$(228), 2 As Q$(229), 2 As Q$(230), 2 As Q$(231), 2 As Q$(232), 2 As Q$(233), 2 As Q$(234), 2 As Q$(235), 2 As Q$(236), 2 As Q$(237), 2 As Q$(238), 2 As Q$(239), 2 As Q$(240), 2 As Q$(241), 2 As Q$(242), 2 As Q$(243), 2 As Q$(244), 2 As Q$(245), 2 As Q$(246), 2 As Q$(247), 2 As Q$(248), 2 As Q$(249)
    Next

    LSet Q$(0) = coaches_DRAFT$(0)
    LSet Q$(1) = stadiums_DRAFT$(0)
    LSet Q$(2) = mascots_DRAFT$(0)
    LSet Q$(3) = MKL$(att_DRAFT&(0))

    For I = 0 To 13

        LSet Q$(I * 16 + 4) = MKI$(plyrDef_DRAFT!(0, I))
        LSet Q$(I * 16 + 5) = posn_DRAFT$(0, I)

        For I1 = 0 To 13
            LSet Q$(I * 16 + I1 + 6) = MKI$(plyrStat_DRAFT!(0, I, I1))
        Next

    Next

    For I = 0 To 20
        LSet Q$(228 + I) = MKI$(teamStats_DRAFT(0, I))
    Next

    LSet Q$(249) = MKI$(999)

    Put #1, teamPosn

    Close 1

End Sub


'----------------------------------------
'     ChooseDraftTeam Subroutine
'----------------------------------------
'...explanation...
Sub ChooseDraftTeam (yearNumber$, numberTeams, chosenIdx, teamName$, draftIdx)

    Shared teamRat_DRAFT()

    Shared previousYear$

    Do

        Color 15, 0
        Cls

        JJ = 7

        Locate 6, 8
        Color 15, 4

        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

        For II = 1 To JJ
            Locate 6 + II, 8
            Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
        Next

        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

        Locate 7, 32

        Locate , 12: Print "ENTER DISK ID: "
        Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Print
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"

        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then

            If I$ <> Chr$(13) Then

                Locate 7, 27: Print I$
                Locate 7, 28: Line Input NY$
                NY$ = I$ + NY$
                Color 15, 0

                If Not (NY$ = "" And yearNumber$ <> "") Then

                    yearNumber$ = RTrim$(NY$)
                    Erase teamNames$
                    numberTeams = 0

                    Cls

                    If Not _FileExists(diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$) Then

                        Locate 10, 8

                        Call CSMsgBox("NO TEAMS WERE FOUND MATCHING THAT ID... PRESS ANY KEY TO SELECT ANOTHER ID", 20, PASS)
                        I$ = GetKeyPress$
                        Call CSMsgBox("", 0, PASS)

                    Else

                        Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$ For Random As #1 Len = TEAM_SIZE_BYTES

                        fileLength& = LOF(1)

                        Field #1, 15 As Q$(0)

                        numberTeams = fileLength& / TEAM_SIZE_BYTES

                        For teamIdx = 1 To numberTeams
                            Get #1, teamIdx
                            'numberTeams = numberTeams + 1
                            teamNames$(teamIdx) = Q$(0)
                            teamIndex%(teamIdx) = teamIdx
                        Next teamIdx

                        Close 1

                    End If 'Done checking if file exists

                End If 'Done checking for blank year number

                previousYear$ = yearNumber$

            Else

                yearNumber$ = previousYear$

            End If 'Done checking for ENTER key

            'Locate 2, 33
            'Color 31: Print "LOADING..."

            Do

                Call SelectDraftTeam(numberTeams, yearNumber$, chosenIdx, teamName$, ExitCode%, draftIdx)

                If ExitCode% <> 27 Then

                    Do
                        Locate 2, 33
                        Color teamRat_DRAFT(draftIdx, 0), teamRat_DRAFT(draftIdx, 1): Print teamName$

                        Color 15, 4
                        Locate 3, 33: Print "ANY CHANGE? (YN)";

                        J$ = GetKeyPress$

                    Loop Until J$ = Chr$(27) Or UCase$(J$) = "Y" Or UCase$(J$) = "N"

                End If

            Loop Until ExitCode% = 27 Or J$ = Chr$(27) Or UCase$(J$) = "N"

        End If 'Done checking for ESC

    Loop Until UCase$(J$) = "N" Or I$ = Chr$(27) 'Or J$ = Chr$(27)

    If I$ = Chr$(27) Or J$ = Chr$(27) Then chosenIdx = 0

End Sub


'----------------------------------------
'       SelectDraftTeam Subroutine
'----------------------------------------
Sub SelectDraftTeam (numberTeams, yearNumber$, selectedIdx, teamName$, ExitCode%, draftIdx)

    If numberTeams > 0 Then

        Selection% = 1
        Count% = numberTeams
        Start% = 1
        FileFGColr% = 15: FileBGColr% = 4
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Rows% = 20
        Columns% = 4
        Spacing% = 6
        Row% = 4
        Column% = 1

        Color , 4
        Cls
        Color 14
        teamNumber = draftIdx + 1
        Print Using "TEAM # SELECTION"; teamNumber
        Locate 25, 1
        Color 14: Print "ESC";: Color 15: Print "-SELECT NEW ID  PGDN  PGUP  ";
        Color 14: Print "ENTER";: Color 15: Print "-SELECTS TEAM";

        _MouseShow "DEFAULT"
        MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            selectedIdx = teamIndex%(Selection%)

            Locate 2, 33: Color 3
            Print "LOADING.."

            Call ReadDraftTeam(yearNumber$, teamName$, selectedIdx, draftIdx)

        Else
            BO% = 1
        End If

    End If

End Sub


'----------------------------------------
'       ReadDraftTeam SubRoutine
'----------------------------------------
'Reads team data into arrays for the
'selected team
Sub ReadDraftTeam (teamYear$, targetTeam$, teamIdx, draftIdx)

    Shared att_DRAFT&(), CK!()
    Shared dFGPctAdj_DRAFT!(), d3FG_Adj(), d3FGA_Adj()
    Shared lgRat_DRAFT(), miscRat_DRAFT()
    Shared plyrOff_DRAFT!(), plyrDef_DRAFT!()
    Shared plyrRat_DRAFT!(), plyrStat_DRAFT!(), stamina_DRAFT!()

    Shared teamRat_DRAFT(), teamStats_DRAFT()

    Shared plyrNames_DRAFT$(), tmName_DRAFT$(), posn_DRAFT$(), teamYears$()
    Shared mascots_DRAFT$(), coaches_DRAFT$(), stadiums_DRAFT$()

    Open diskPaths$(0) + "BASK." + teamYears$(draftIdx) For Random As #1 Len = 655

    Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 2 As Q$(16), 2 As Q$(17), 2 As Q$(18), 2 As Q$(19), 602 As X$

    For I = 0 To 13
        Field #1, 53 + I * 43 As X$, 15 As Q$(I * 15 + 20), 2 As Q$(I * 15 + 21), 2 As Q$(I * 15 + 22), 2 As Q$(I * 15 + 23), 2 As Q$(I * 15 + 24), 2 As Q$(I * 15 + 25), 2 As Q$(I * 15 + 26), 2 As Q$(I * 15 + 27), 2 As Q$(I * 15 + 28), 2 As Q$(I * 15 + 29), 2 As Q$(I * 15 + 30), 2 As Q$(I * 15 + 31), 2 As Q$(I * 15 + 32), 2 As Q$(I * 15 + 33), 2 As Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 As X$
    Next

    Get #1, teamIdx

    tmName_DRAFT$(draftIdx) = RTrim$(Q$(0))
    stamina_DRAFT!(draftIdx) = CVI(Q$(1))
    dFGPctAdj_DRAFT!(draftIdx) = CVI(Q$(2))
    d3FG_Adj(draftIdx) = CVI(Q$(3))
    d3FGA_Adj(draftIdx) = CVI(Q$(4))

    For I = 0 To 4:
        lgRat_DRAFT(draftIdx, I) = CVI(Q$(I + 5))
    Next

    For I = 0 To 9:
        teamRat_DRAFT(draftIdx, I) = CVI(Q$(I + 10))
    Next

    For I = 0 To 13
        plyrNames_DRAFT$(draftIdx, I) = RTrim$(Q$(I * 15 + 20))

        For I1 = 0 To 6
            plyrOff_DRAFT!(draftIdx, I, I1) = CVI(Q$(I * 15 + I1 + 21))
            plyrRat_DRAFT!(draftIdx, I, I1) = CVI(Q$(I * 15 + I1 + 28))
        Next
    Next

    Close 1

    If d3FG_Adj(draftIdx) > 20 Then d3FG_Adj(draftIdx) = 100 - d3FG_Adj(draftIdx)
    If d3FGA_Adj(draftIdx) > 20 Then d3FGA_Adj(draftIdx) = 100 - d3FGA_Adj(draftIdx)
    If lgRat_DRAFT(draftIdx, 0) > 20 Then lgRat_DRAFT(draftIdx, 0) = 100 - lgRat_DRAFT(draftIdx, 0)
    If lgRat_DRAFT(draftIdx, 2) > 20 Then lgRat_DRAFT(draftIdx, 2) = 100 - lgRat_DRAFT(draftIdx, 2)
    If lgRat_DRAFT(draftIdx, 3) > 20 Then lgRat_DRAFT(draftIdx, 3) = 100 - lgRat_DRAFT(draftIdx, 3)

    Open diskPaths$(0) + "COLBBTMS." + teamYears$(draftIdx) For Random As #1 Len = 566

    Field #1, 15 As Q$(0), 40 As Q$(1), 15 As Q$(2), 4 As Q$(3)

    For I = 0 To 13
        Field #1, 74 + I * 32 As X$, 2 As Q$(I * 16 + 4), 2 As Q$(I * 16 + 5), 2 As Q$(I * 16 + 6), 2 As Q$(I * 16 + 7), 2 As Q$(I * 16 + 8), 2 As Q$(I * 16 + 9), 2 As Q$(I * 16 + 10), 2 As Q$(I * 16 + 11), 2 As Q$(I * 16 + 12), 2 As Q$(I * 16 + 13), 2 As Q$(I * 16 + 14), 2 As Q$(I * 16 + 15), 2 As Q$(I * 16 + 16), 2 As Q$(I * 16 + 17), 2 As Q$(I * 16 + 18), 2 As Q$(I * 16 + 19)
        Field #1, 522 As X$, 2 As Q$(228), 2 As Q$(229), 2 As Q$(230), 2 As Q$(231), 2 As Q$(232), 2 As Q$(233), 2 As Q$(234), 2 As Q$(235), 2 As Q$(236), 2 As Q$(237), 2 As Q$(238), 2 As Q$(239), 2 As Q$(240), 2 As Q$(241), 2 As Q$(242), 2 As Q$(243), 2 As Q$(244), 2 As Q$(245), 2 As Q$(246), 2 As Q$(247), 2 As Q$(248), 2 As Q$(249)
    Next

    Get #1, teamIdx

    coaches_DRAFT$(draftIdx) = RTrim$(Q$(0))
    stadiums_DRAFT$(draftIdx) = RTrim$(Q$(1))
    mascots_DRAFT$(draftIdx) = RTrim$(Q$(2))
    att_DRAFT&(draftIdx) = CVL(Q$(3))

    For I = 0 To 13

        plyrDef_DRAFT!(draftIdx, I) = CVI(Q$(I * 16 + 4))
        posn_DRAFT$(draftIdx, I) = Q$(I * 16 + 5)

        For I1 = 0 To 13
            plyrStat_DRAFT!(draftIdx, I, I1) = CVI(Q$(I * 16 + I1 + 6))
        Next

    Next

    For I = 0 To 20
        teamStats_DRAFT(draftIdx, I) = CVI(Q$(228 + I))
    Next

    CK!(draftIdx) = CVI(Q$(249))

    Close 1

    For I = 0 To 13
        miscRat_DRAFT(draftIdx, I, 1) = stamina_DRAFT!(draftIdx)
        miscRat_DRAFT(draftIdx, I, 2) = dFGPctAdj_DRAFT!(draftIdx)
        miscRat_DRAFT(draftIdx, I, 3) = d3FGA_Adj(draftIdx)
        miscRat_DRAFT(draftIdx, I, 4) = d3FG_Adj(draftIdx)
        miscRat_DRAFT(draftIdx, I, 5) = lgRat_DRAFT(draftIdx, 0)
        miscRat_DRAFT(draftIdx, I, 6) = lgRat_DRAFT(draftIdx, 2)
        miscRat_DRAFT(draftIdx, I, 7) = lgRat_DRAFT(draftIdx, 3)
        miscRat_DRAFT(draftIdx, I, 8) = 99
        miscRat_DRAFT(draftIdx, I, 9) = teamRat_DRAFT(draftIdx, 3)
        miscRat_DRAFT(draftIdx, I, 10) = teamRat_DRAFT(draftIdx, 4)
        miscRat_DRAFT(draftIdx, I, 11) = teamRat_DRAFT(draftIdx, 5)
        miscRat_DRAFT(draftIdx, I, 12) = teamRat_DRAFT(draftIdx, 6)
    Next

    If CK!(draftIdx) <> 999 Then

        coaches_DRAFT$(draftIdx) = ""
        stadiums_DRAFT$(draftIdx) = ""
        mascots_DRAFT$(draftIdx) = ""
        att_DRAFT&(draftIdx) = 0

        For I = 0 To 13:

            plyrDef_DRAFT!(draftIdx, I) = 5
            posn_DRAFT$(draftIdx, I) = ""

            For I1 = 0 To 13
                plyrStat_DRAFT!(draftIdx, I, I1) = 0
            Next

        Next

        For I = 0 To 20:
            teamStats_DRAFT(draftIdx, I) = 0
        Next

    End If

End Sub

