'----------------------------------------
'            CNGTM Subroutine
'----------------------------------------
'This subroutine presents a menu of all 
'the options related to team rosters
Sub CNGTM (userChoice)

    _Title "Courtside College Basketball - Change Team"

    ReDim menuChoices$(1 To 4)

    Do

        Cls

        Call TITLE
        menuChoices$(1) = "EDIT AN EXISTING TEAM"
        menuChoices$(2) = "PRINT A TEAM ROSTER  "
        menuChoices$(3) = "VIEW A TEAM ROSTER   "
        menuChoices$(4) = "EXIT TO MAIN MENU    "
        Selection% = 1
        Count% = 4
        Start% = 1
        ExitCode% = 4
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        If (userChoice <> 0) Then
            Selection% = userChoice
        Else
            Call box0(7, 24, 12, 46, 2, 15, 1)

            _MouseShow "DEFAULT"
            MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
            _MouseHide

            Color , 0
        End If

        If ExitCode% <> 27 Then

            Call ChooseTeam(YN$, NTMS, teamNbr, team$)

            If teamNbr <> 0 Then

                Select Case Selection%

                    Case 1:
                        Call EditTeam(team$, T1, YN$)

                    Case 2:
                        Call PrintRoster(team$, T1)

                    Case 3:
                        Call ViewRoster(team$, T1)

                End Select

                userChoice = 0
                
            End If

        End If

    Loop Until Selection% = 4 Or ExitCode% = 27

End Sub

'----------------------------------------
'          EditTeam Subroutine
'----------------------------------------
'This subroutine steps the user through 
'screens for adjusting team data and players
'
'NOTE: <ENTER> key is valued as 0 for when
'a user is prompted to press keys to CONTINUE
'This improvement over the original makes it
'faster and easier to proceed through screens
Sub EditTeam (teamName$, teamIdx, yearNumber$)

    Shared def3FG_Adj, def3FGA_Adj
    Shared teamAttendance%, defFGPctAdj%, staminaRating%
    Shared mascot$, coach$, arenaName$

    Shared leagueRatings%(), teamRatings%(), teamStats()
    Shared playerNames$(), position$()
    Shared playerOff!(), playerDef!(), playerStats!(), playerRatings!()

    Color 15, 0
    Cls

    Color 14
    Print Tab(29); "CHANGE TEAM INFORMATION"
    Color 15

    Print " 1. TEAM NUMBER "; teamIdx; Tab(37); "20. NICKNAME "; mascot$
    Print " 2. DISK ID "; yearNumber$; Tab(37); "21. COACH "; coach$
    Print " 3. TEAM NAME "; teamName$; Tab(37); "22. AVG ATTENDANCE "; teamAttendance%
    Print " 4. TEAM STAMINA RATING "; staminaRating%; Tab(37); "23. WINS "; teamRatings%(7)
    Print " 5. DEF FG% ADJ "; defFGPctAdj%; Tab(37); "24. LOSSES "; teamRatings%(8)
    Print " 6. DEF 3FGA/FGA ADJ "; def3FGA_Adj; Tab(37);"25. POWER RATING "; teamRatings%(9)
    Print " 7. DEF 3FG% ADJ "; def3FG_Adj
    Print " 8. OFF STEAL RATING "; leagueRatings%(0)
    Print " 9. OFF TURNOVER RATING "; leagueRatings%(1)
    Print "10. DEF TURNOVER ADJ "; leagueRatings%(2)
    Print "11. DEF FOUL ADJ "; leagueRatings%(3)
    Print "12. ENTER 99 IF USING 6-9 "; leagueRatings%(4)
    Print "13. TEXT COLOR (0-15) "; teamRatings%(0)
    Print "14. BACKGROUND COLOR (0-7) "; teamRatings%(1)
    Print "15. TEAM FGA/G "; teamRatings%(3)
    Print "16. LEAGUE FGA/G "; teamRatings%(4)
    Print "17. TEAM GAMES PLAYED "; teamRatings%(5)
    Print "18. HOMECOURT FACTOR "; teamRatings%(6)
    Print "19. ARENA NAME "; arenaName$

    If teamRatings%(0) >= 0 And teamRatings%(1) >= 0 Then
        Locate 21, 1
        Print "TEAM APPEARANCE: ";
        Color teamRatings%(0), teamRatings%(1)
        Print RTrim$(teamName$ + " " + mascot$)
        Color 15, 0
    End If

    Do

        Do
            Locate 23
            Input "SELECT # TO CHANGE 1-25 (0 TO CONTINUE) "; B9
        Loop Until B9 >= 0 And B9 <= 25

        Select Case B9

            Case 1
                Locate 2, 17: Print Space$(5)
                Locate 2: Input " 1. TEAM NUMBER "; teamIdx

            Case 2
                Locate 3: Input " 2. DISK ID "; yearNumber$

            Case 3
                Locate 4: Input " 3. TEAM NAME "; teamName$

            Case 4
                Locate 2, 25: Print Space$(5)
                Locate 5: Input " 4. TEAM STAMINA RATING "; staminaRating%

            Case 5
                Locate 2, 17: Print Space$(5)
                Locate 6: Input " 5. DEF FG% ADJ "; defFGPctAdj%

            Case 6
                Locate 2, 22: Print Space$(5)
                Locate 7: Input " 6. DEF 3FGA/FGA ADJ "; def3FGA_Adj

            Case 7
                Locate 2, 18: Print Space$(5)
                Locate 8: Input " 7. DEF 3FG% ADJ "; def3FG_Adj

            Case 8
                Locate 2, 22: Print Space$(5)
                Locate 9: Input " 8. OFF STEAL RATING "; leagueRatings%(0)

            Case 9
                Locate 2, 25: Print Space$(5)
                Locate 10: Input " 9. OFF TURNOVER RATING "; leagueRatings%(1)

            Case 10
                Locate 2, 22: Print Space$(5)
                Locate 11: Input "10. DEF TURNOVER ADJ "; leagueRatings%(2)

            Case 11
                Locate 2, 18: Print Space$(5)
                Locate 12: Input "11. DEF FOUL ADJ "; leagueRatings%(3)

            Case 12
                Locate 2, 27: Print Space$(5)
                Locate 13: Input "12. ENTER 99 IF USING 6-9 "; leagueRatings%(4)

            Case 13
                Locate 2, 23: Print Space$(5)
                Locate 14: Input "13. TEXT COLOR (0-15) "; teamRatings%(0)

                If teamRatings%(0) >= 0 And teamRatings%(1) >= 0 Then
                    Locate 21, 1: Color teamRatings%(0), teamRatings%(1)
                    Print teamName$ + " " + mascot$
                    Color 15, 0
                End If

            Case 14
                Locate 2, 28: Print Space$(5)
                Locate 15: Input "14. BACKGROUND COLOR (0-7) "; teamRatings%(1)

                If teamRatings%(0) >= 0 And teamRatings%(1) >= 0 Then
                    Locate 21, 1
                    Locate 21, 1
                    Print "TEAM APPEARANCE: ";
                    Color teamRatings%(0), teamRatings%(1)
                    Print teamName$ + " " + mascot$
                    Color 15, 0
                End If

            Case 15
                Locate 2, 16: Print Space$(5)
                Locate 16: Input "15. TEAM FGA/G "; teamRatings%(3)

            Case 16
                Locate 2, 18: Print Space$(5)
                Locate 17: Input "16. LEAGUE FGA/G "; teamRatings%(4)

            Case 17
                Locate 2, 23: Print Space$(5)
                Locate 18: Input "17. TEAM GAMES PLAYED "; teamRatings%(5)

            Case 18
                Locate 2, 22: Print Space$(5)
                Locate 19: Input "18. HOMECOURT FACTOR "; teamRatings%(6)

            Case 19
                Locate 20: Input "19. ARENA NAME "; arenaName$

            Case 20
                Locate 2, 37: Input "20. NICKNAME "; mascot$

            Case 21
                Locate 3, 37: Input "21. COACH "; coach$

            Case 22
                Locate 2, 20: Print Space$(5)
                Locate 4, 37: Input "22. AVG ATTENDANCE "; teamAttendance%

            Case 23
                Locate 2, 10: Print Space$(5)
                Locate 5, 37: Input "23. WINS "; teamRatings%(7)

            Case 24
                Locate 2, 12: Print Space$(5)
                Locate 6, 37: Input "24. LOSSES "; teamRatings%(8)

            Case 25
                Locate 2, 18: Print Space$(5)
                Locate 7, 37: Input "25. POWER RATING "; teamRatings%(9)

        End Select

    Loop Until B9 = 0


    '---------------
    'Change Players
    '---------------
    If teamRatings%(6) < 0 Then teamRatings%(6) = 0
    If teamRatings%(6) > 6 Then teamRatings%(6) = 6

    Do

        Cls
        Color 14
        Print Tab(32); "CHANGE PLAYERS"
        Color 15

        For I = 0 To 6
            Locate I + 3, 5
            Print Using "##"; I + 1;
            Print " "; playerNames$(I); Tab(37);
            Print Using "##"; I + 8;
            Print " "; playerNames$(I + 7)
        Next

        Print

        Do
            Locate 11
            Input "# TO CHANGE (1-13) (0 TO CONTINUE) "; idx
        Loop Until idx >= 0 And idx <= 14

        If idx > 0 Then

            I = idx - 1

            Cls
            Color 14
            Print Tab(35); "PLAYER - "; idx
            Print Tab(31); "CHANGE PLAYER STATS"
            Color 15

            Print " 1. NAME "; playerNames$(I); Tab(37); "17. GAMES "; playerStats!(I, 0)
            Print " 2. POSITION "; position$(I); Tab(37); "18. MINUTES "; playerStats!(I, 1)
            Print " 3. GAMES PLAYED "; playerRatings!(I, 0); Tab(37); "19. FGM "; playerStats!(I, 3)
            Print " 4. ADJ FG% "; playerOff!(I, 0); Tab(37); "20. FGA "; playerStats!(I, 2)
            Print " 5. FT% "; playerOff!(I, 1); Tab(37); "21. FTM "; playerStats!(I, 5)
            Print " 6. OFF RB/40 MIN X 10 "; playerRatings!(I, 4); Tab(37); "22. FTA "; playerStats!(I, 4)
            Print " 7. DEF RB/40 MIN X 10 "; playerOff!(I, 2); Tab(37); "23. OFF REB "; playerStats!(I, 6)
            Print " 8. ASSIST RATING "; playerOff!(I, 4); Tab(37); "24. TOTAL REB "; playerStats!(I, 7)
            Print " 9. STEAL RATING "; playerOff!(I, 5); Tab(37); "25. PER FOULS "; playerStats!(I, 8)
            Print "10. BLOCK RATING "; playerOff!(I, 6); Tab(37); "26. ASSISTS "; playerStats!(I, 9)
            Print "11. FGA "; playerRatings!(I, 1); Tab(37); "27. BLOCKS "; playerStats!(I, 10)
            Print "12. FOUL COMMIT RATING "; playerRatings!(I, 5); Tab(37); "28. STEALS "; playerStats!(I, 11)
            Print "13. FOUL DRAWING RATING "; playerRatings!(I, 6); Tab(37); "29. 3 FGM "; playerStats!(I, 13)
            Print "14. 3FG% "; playerRatings!(I, 2); Tab(37); "30. 3 FGA "; playerStats!(I, 12)
            Print "15. 3FGA/TOT FGA "; playerRatings!(I, 3)
            Print "16. DEFENSIVE RATING (1-9) "; playerDef!(I)

            Do

                Do
                    Locate 22, 1
                    Input "# TO CHANGE (0 TO CONTINUE) "; B9
                Loop Until B9 >= 0 And B9 <= 30

                Select Case B9

                    Case 1
                        Locate 2: Input " 1. NAME "; playerNames$(I)

                    Case 2
                        Locate 3: Input " 2. POSITION "; position$(I)

                    Case 3
                        Locate 4, 18: Print Space$(5)
                        Locate 4: Input " 3. GAMES PLAYED "; playerRatings!(I, 0)

                    Case 4
                        Locate 5, 13: Print Space$(5)
                        Locate 5: Input " 4. ADJ FG% "; playerOff!(I, 0)

                    Case 5
                        Locate 6, 9: Print Space$(5)
                        Locate 6: Input " 5. FT% "; playerOff!(I, 1)

                    Case 6
                        Locate 7, 24: Print Space$(5)
                        Locate 7: Input " 6. OFF RB/40 MIN X 10 "; playerRatings!(I, 4)

                    Case 7
                        Locate 8, 24: Print Space$(5)
                        Locate 8: Input " 7. DEF RB/40 MIN X 10 "; playerOff!(I, 2)

                    Case 8
                        Locate 9, 19: Print Space$(5)
                        Locate 9: Input " 8. ASSIST RATING "; playerOff!(I, 4)

                    Case 9
                        Locate 10, 18: Print Space$(5)
                        Locate 10: Input " 9. STEAL RATING "; playerOff!(I, 5)

                    Case 10
                        Locate 11, 18: Print Space$(5)
                        Locate 11: Input "10. BLOCK RATING "; playerOff!(I, 6)

                    Case 11
                        Locate 12, 9: Print Space$(5)
                        Locate 12: Input "11. FGA "; playerRatings!(I, 1)

                    Case 12
                        Locate 13, 25: Print Space$(5)
                        Locate 13: Input "12. FOUL COMMIT RATING "; playerRatings!(I, 5)

                    Case 13
                        Locate 14, 25: Print Space$(5)
                        Locate 14: Input "13. FOUL DRAWING RATING "; playerRatings!(I, 6)

                    Case 14
                        Locate 15, 10: Print Space$(5)
                        Locate 15: Input "14. 3FG% "; playerRatings!(I, 2)

                    Case 15
                        Locate 16, 18: Print Space$(5)
                        Locate 16: Input "15. 3FGA/TOT FGA "; playerRatings!(I, 3)

                    Case 16
                        Locate 17, 28: Print Space$(5)
                        Locate 17: Input "16. DEFENSIVE RATING (1-9) "; playerDef!(I)

                    Case 17
                        Locate 2, 48: Print Space$(5)
                        Locate 2, 37: Input "17. GAMES "; playerStats!(I, 0)

                    Case 18
                        Locate 3, 50: Print Space$(5)
                        Locate 3, 37: Input "18. MINUTES "; playerStats!(I, 1)

                    Case 19
                        Locate 4, 46: Print Space$(5)
                        Locate 4, 37: Input "19. FGM "; playerStats!(I, 3)

                    Case 20
                        Locate 5, 46: Print Space$(5)
                        Locate 5, 37: Input "20. FGA "; playerStats!(I, 2)

                    Case 21
                        Locate 6, 46: Print Space$(5)
                        Locate 6, 37: Input "21. FTM "; playerStats!(I, 5)

                    Case 22
                        Locate 7, 46: Print Space$(5)
                        Locate 7, 37: Input "22. FTA "; playerStats!(I, 4)

                    Case 23
                        Locate 8, 50: Print Space$(5)
                        Locate 8, 37: Input "23. OFF REB "; playerStats!(I, 6)

                    Case 24
                        Locate 9, 52: Print Space$(5)
                        Locate 9, 37: Input "24. TOTAL REB "; playerStats!(I, 7)

                    Case 25
                        Locate 10, 52: Print Space$(5)
                        Locate 10, 37: Input "25. PER FOULS "; playerStats!(I, 8)

                    Case 26
                        Locate 11, 50: Print Space$(5)
                        Locate 11, 37: Input "26. ASSISTS "; playerStats!(I, 9)

                    Case 27
                        Locate 12, 49: Print Space$(5)
                        Locate 12, 37: Input "27. BLOCKS "; playerStats!(I, 10)

                    Case 28
                        Locate 13, 49: Print Space$(5)
                        Locate 13, 37: Input "28. STEALS "; playerStats!(I, 11)

                    Case 29
                        Locate 14, 48: Print Space$(5)
                        Locate 14, 37: Input "29. 3 FGM "; playerStats!(I, 13)

                    Case 30
                        Locate 15, 48: Print Space$(5)
                        Locate 15, 37: Input "30. 3 FGA "; playerStats!(I, 12)

                End Select

            Loop Until B9 = 0

        End If

    Loop Until idx = 0

    '---------------
    'Player Contributions
    '---------------
    Do

        Cls

        Color teamRatings%(0), teamRatings%(1)
        Print teamName$

        Color 14, 0
        Print "PLAYER          CONTRIBUTION"
        Print "                PERCENT"

        Color 15, 0

        totalCX = 0

        For I = 0 To 13
            Print Using "## "; I + 1;
            Print playerNames$(I);
            Locate , 17
            Print playerOff!(I, 3)
            Locate , 1
            totalCX = totalCX + playerOff!(I, 3)
        Next I

        Print
        Color 14: Print "TOTAL CP ";

        Locate , 17
        Color 15, 0: Print totalCX

        Do
            Locate 21
            Input "SELECT PLAYER TO CHANGE 1-14 (0-NO MORE CHANGE) "; X9
        Loop Until X9 >= 0 And X9 <= 14

        If X9 <> 0 Then
            'Subtract one because our array starts at 0
            X9 = X9 - 1

            Locate 22, 18
            Print playerNames$(X9)
            Locate , 1
            Locate 23, 1
            Input "ENTER CP: "; playerOff!(X9, 3)

        Else

            Cls

            If totalCX > 98 And totalCX < 102 Then

                Call SaveChangedTeam(0, yearNumber$, teamName$, teamIdx)

            Else
                'We need to warn the user that
                'our total doesn't add up!!
                JJ = 4

                Locate 6, 8
                Color 15, 1

                Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";

                For II = 1 To JJ:
                    Locate 6 + II, 8
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

            End If

        End If

    Loop Until X9 = 0

End Sub


'----------------------------------------
'         PrintRoster Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'team roster to the default printer
Sub PrintRoster (A$, T1)

    Shared def3FG_Adj, def3FGA_Adj
    Shared defFGPctAdj%, staminaRating%

    Shared mascot$, coach$, arenaName$

    Shared leagueRatings%(), teamRatings%(), teamStats()

    Shared playerOff!(), playerDef!(), playerRatings!()

    Shared playerNames$(), position$()

    Cls

    LPrint "TEAM ROSTER FOR "; A$
    LPrint
    LPrint "AVG SHOTS PER GAME = "; teamRatings%(3); "      DEF FG ADJ = "; defFGPctAdj%
    LPrint "OPP PCT OF 3FG ATT ADJ = "; def3FGA_Adj; "  OPP 3FG PCT ADJ = "; def3FG_Adj
    LPrint
    LPrint "DEF STEAL ADJ "; leagueRatings%(0)
    LPrint "OFF TURNOVER ADJ "; leagueRatings%(1)
    LPrint "DEF TURNOVER ADJ "; leagueRatings%(2)
    LPrint "OFF FOUL ADJ "; leagueRatings%(3)
    LPrint
    LPrint "NAME           FG  FT ORB DRB  CX PS  D  B FC FD  3FG 3FG%":
    LPrint

    For I = 0 To 13
        If playerNames$(I) <> "XXX" Then
            LPrint playerNames$(I); Tab(15);
            LPrint Using "### ### ### ### ### "; playerOff!(I, 0); playerOff!(I, 1); playerRatings!(I, 4); playerOff!(I, 2); playerOff!(I, 3);
            LPrint Using "## ## ## ## ## ### ###"; playerOff!(I, 4); playerOff!(I, 5); playerOff!(I, 6); playerRatings!(I, 5); playerRatings!(I, 6); playerRatings!(I, 3); playerRatings!(I, 2)
        End If
    Next I

    LPrint
    LPrint "FG- FIELD GOAL %                 B- BLOCK SHOT RATING"
    LPrint "FT- FREE THROW %                FC- FOUL COMMIT RATING"
    LPrint "RB- REBOUNDING RATING           FD- FOUL DRAWING RATING"
    LPrint "CX- CONTRIBUTION FACTOR        3FG- 3FGA/TOTAL FGA"
    LPrint "PS- PASSING RATING            3FG%- THREE POINT GOAL %"
    LPrint " D- DEFENSIVE RATING"

    LPrint Chr$(12)

End Sub


'----------------------------------------
'         ViewRoster Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'team roster to the user's display
Sub ViewRoster (A$, T1)

    Shared def3FG_Adj, def3FGA_Adj
    Shared defFGPctAdj%, staminaRating%

    Shared mascot$, coach$, arenaName$

    Shared leagueRatings%(), teamRatings%(), teamStats()

    Shared playerOff!(), playerDef!(), playerRatings!()

    Shared playerNames$(), position$()

    Color 15, 0
    Cls

    Print "TEAM ROSTER FOR ";
    Color teamRatings%(0), teamRatings%(1): Print A$

    Color 14, 0
    Print "    AVG SHOTS PER GAME = ";
    Color 15, 0
    Print teamRatings%(3);

    Color 14, 0
    Print " DEF FG ADJ = ";
    Color 15, 0: Print defFGPctAdj%

    Color 14, 0: Print "OPP PCT OF 3FG ATT ADJ = ";
    Color 15, 0: Print def3FGA_Adj;

    Color 14, 0: Print "  OPP 3FG PCT ADJ = ";
    Color 15, 0: Print def3FG_Adj;

    Color 14, 0: Print "   DEF STEAL ADJ ";
    Color 15, 0: Print leagueRatings%(0)

    Color 14, 0: Print "OFF TURNOVER ADJ ";
    Color 15, 0: Print leagueRatings%(1);

    Color 14, 0: Print "    DEF TURNOVER ADJ ";
    Color 15, 0: Print leagueRatings%(2);

    Color 14, 0: Print "    OFF FOUL ADJ ";
    Color 15, 0: Print leagueRatings%(3)
    Print

    Color 2, 0: Print "NAME           FG% FT% ORB DRB  CX PS  D  B  FC FD  3FG 3FG%"
    Color 15, 0

    For I = 0 To 13
        If playerNames$(I) <> "XXX" Then
            Print playerNames$(I); Tab(16);
            Print Using "### ### ### ### ### "; playerOff!(I, 0); playerOff!(I, 1); playerRatings!(I, 4); playerOff!(I, 2); playerOff!(I, 3);
            Print Using "## ## ## ### ##  ###  ###"; playerOff!(I, 4); playerOff!(I, 5); playerOff!(I, 6); playerRatings!(I, 5); playerRatings!(I, 6); playerRatings!(I, 3); playerRatings!(I, 2)
        End If
    Next I

    Print " B- BLOCK SHOT RATING           FC- FOUL COMMIT RATING"
    Print "RB- REBOUNDING RATING           FD- FOUL DRAWING RATING"
    Print "CX- CONTRIBUTION FACTOR        3FG- 3FGA/TOTAL FGA"
    Print "PS- PASSING RATING               D- DEFENSIVE RATING"

    I$ = GetKeyPress$

End Sub


'----------------------------------------
'       SaveChangedTeam Subroutine
'----------------------------------------
'This routine saves the modified data back
'to the original team file
Sub SaveChangedTeam (newID%, yearNumber$, teamName$, teamIdx)

    Shared def3FG_Adj, def3FGA_Adj
    Shared teamAttendance%, defFGPctAdj%, staminaRating%
    Shared mascot$, coach$, arenaName$

    Shared leagueRatings%(), teamRatings%(), teamStats()
    Shared playerNames$(), position$()
    Shared playerOff!(), playerDef!(), playerStats!(), playerRatings!()

    teamYear$ = RTrim$(yearNumber$)

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + teamYear$ For Random As #1 Len = TEAM_SIZE_BYTES

    fileLength& = LOF(1)

    If newID% = 1 Or teamIdx = 0 Then teamIdx = fileLength& / TEAM_FILE_BYTES + 1

    Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 2 As Q$(16), 2 As Q$(17), 2 As Q$(18), 2 As Q$(19), 602 As X$

    For I = 0 To 13
        Field #1, 53 + I * 43 As X$, 15 As Q$(I * 15 + 20), 2 As Q$(I * 15 + 21), 2 As Q$(I * 15 + 22), 2 As Q$(I * 15 + 23), 2 As Q$(I * 15 + 24), 2 As Q$(I * 15 + 25), 2 As Q$(I * 15 + 26), 2 As Q$(I * 15 + 27), 2 As Q$(I * 15 + 28), 2 As Q$(I * 15 + 29), 2 As Q$(I * 15 + 30), 2 As Q$(I * 15 + 31), 2 As Q$(I * 15 + 32), 2 As Q$(I * 15 + 33), 2 As Q$(I * 15 + 34), TEAM_SIZE_BYTES - 53 - 43 - I * 43 As X$
    Next

    LSet Q$(0) = RTrim$(teamName$)
    LSet Q$(1) = MKI$(staminaRating%)
    LSet Q$(2) = MKI$(defFGPctAdj%)
    LSet Q$(3) = MKI$(def3FG_Adj)
    LSet Q$(4) = MKI$(def3FGA_Adj)

    For I = 0 To 4: LSet Q$(I + 5) = MKI$(leagueRatings%(I)): Next

    teamRatings%(2) = 999

    For I = 0 To 9: LSet Q$(I + 10) = MKI$(teamRatings%(I)): Next

    'Players
    For I = 0 To 13
        LSet Q$(I * 15 + 20) = playerNames$(I)
        For I1 = 0 To 6
            LSet Q$(I * 15 + I1 + 21) = MKI$(playerOff!(I, I1))
            LSet Q$(I * 15 + I1 + 28) = MKI$(playerRatings!(I, I1))
        Next
    Next

    Put #1, teamIdx

    Close 1

    Open diskPaths$(0) + DATA_FILE_NAME$ + "." + teamYear$ For Random As #1 Len = DATA_SIZE_BYTES

    Field #1, 15 As Q$(0), 40 As Q$(1), 15 As Q$(2), 4 As Q$(3)

    For I = 0 To 13
        Field #1, 74 + I * 32 As X$, 2 As Q$(I * 16 + 4), 2 As Q$(I * 16 + 5), 2 As Q$(I * 16 + 6), 2 As Q$(I * 16 + 7), 2 As Q$(I * 16 + 8), 2 As Q$(I * 16 + 9), 2 As Q$(I * 16 + 10), 2 As Q$(I * 16 + 11), 2 As Q$(I * 16 + 12), 2 As Q$(I * 16 + 13), 2 As Q$(I * 16 + 14), 2 As Q$(I * 16 + 15), 2 As Q$(I * 16 + 16), 2 As Q$(I * 16 + 17), 2 As Q$(I * 16 + 18), 2 As Q$(I * 16 + 19)
        Field #1, 522 As X$, 2 As Q$(228), 2 As Q$(229), 2 As Q$(230), 2 As Q$(231), 2 As Q$(232), 2 As Q$(233), 2 As Q$(234), 2 As Q$(235), 2 As Q$(236), 2 As Q$(237), 2 As Q$(238), 2 As Q$(239), 2 As Q$(240), 2 As Q$(241), 2 As Q$(242), 2 As Q$(243), 2 As Q$(244), 2 As Q$(245), 2 As Q$(246), 2 As Q$(247), 2 As Q$(248), 2 As Q$(249)
    Next

    LSet Q$(0) = RTrim$(coach$)
    LSet Q$(1) = RTrim$(arenaName$)
    LSet Q$(2) = RTrim$(mascot$)
    LSet Q$(3) = MKL$(teamAttendance%)

    For I = 0 To 13
        LSet Q$(I * 16 + 4) = MKI$(playerDef!(I))
        LSet Q$(I * 16 + 5) = position$(I)
        For I1 = 0 To 13
            LSet Q$(I * 16 + I1 + 6) = MKI$(playerStats!(I, I1))
        Next
    Next

    For I = 0 To 20
        LSet Q$(228 + I) = MKI$(teamStats(I))
    Next

    LSet Q$(249) = MKI$(999)

    Put #1, teamIdx

    Close 1

End Sub
