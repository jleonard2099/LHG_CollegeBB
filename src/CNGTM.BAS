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
        Color 15, 0

        Call TITLE

        menuChoices$(1) = "EDIT AN EXISTING TEAM"
        menuChoices$(2) = "PRINT A TEAM ROSTER  "
        menuChoices$(3) = "VIEW A TEAM ROSTER   "
        menuChoices$(4) = "EXIT TO MAIN MENU    "
        SELECTION% = 1
        Count% = 4
        Start% = 1
        ExitCode% = 4
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        If (userChoice <> 0) Then
            SELECTION% = userChoice
        Else

            Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

            _MouseShow "DEFAULT"
            MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
            _MouseHide

            Color , 0
        End If

        If ExitCode% <> 27 And SELECTION% <> 4 Then

            teamNbr = 0
            Call ChooseTeam(YN$, NTMS, teamNbr, team$)

            If teamNbr <> 0 Then

                Select Case SELECTION%

                    Case 1:
                        Call EditTeam(team$, YN$, teamNbr)

                    Case 2:
                        userKey$ = GetPrintOptions$

                        If userKey$ <> Chr$(27) Then

                            Select Case userKey$

                                Case "F"
                                    userFile$ = _SaveFileDialog$("Save Report File", _CWD$ + "\Report", "*.txt", "Text File")
                                    If userFile$ <> "" Then
                                        Call Roster_Report(0, userFile$, team$)
                                    End If

                                Case "P"
                                    Call Roster_Report(1, "REPORT", team$)

                            End Select

                        End If

                    Case 3:
                        Call ViewRoster(team$)

                End Select

            End If

        End If

        'Reset "user choice" after first call
        'We're at the menu now, no purpose in forcing shortcuts
        userChoice = 0

    Loop Until SELECTION% = 4 Or ExitCode% = 27

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
Sub EditTeam (teamName$, yearNumber$, teamIdx)

    Shared def3FG_Adj, defthreeFGA_Adj
    Shared teamAttendance&, defFGPctAdj!, staminaRating!
    Shared mascot$, coach$, arenaName$

    Shared leagueRatings(), teamRatings(), teamStats!()
    Shared playerNames$(), position$()
    Shared playerOff!(), playerDef!(), playerStats!(), playerRatings!()

    offStRat = leagueRatings(0)
    If offStRat > 20 Then
        offStRat = leagueRatings(0) - 100
    End If

    defTOAdj = leagueRatings(2)
    If defTOAdj > 20 Then
        defTOAdj = leagueRatings(2) - 100
    End If

    offFoulAdj = leagueRatings(3)
    If offFoulAdj > 20 Then
        offFoulAdj = leagueRatings(3) - 100
    End If

    d3FGAdj = def3FG_Adj
    If d3FGAdj > 20 Then
        d3FGAdj = def3FG_Adj - 100
    End If

    dthreeFGAAdj = defthreeFGA_Adj
    If dthreeFGAAdj > 20 Then
        dthreeFGAAdj = defthreeFGA_Adj - 100
    End If

    Color 15, 0
    Cls

    Color 14
    Locate 2
    Print Tab(29); "CHANGE TEAM INFORMATION"
    Color 15

    Locate 4
    Print " 1. TEAM NUMBER  "; teamIdx
    Print " 2. DISK ID  "; yearNumber$
    Print " 3. TEAM NAME   "; teamName$
    Print " 4. TEAM STAMINA RATING "; staminaRating!
    Print " 5. DEF FG% ADJ "; defFGPctAdj!
    Print " 6. DEF 3FGA/FGA ADJ "; dthreeFGAAdj
    Print " 7. DEF 3FG% ADJ "; d3FGAdj
    Print " 8. OFF STEAL RATING "; offStRat
    Print " 9. OFF TURNOVER RATING "; leagueRatings(1)
    Print "10. DEF TURNOVER ADJ "; defTOAdj
    Print "11. OFF FOUL ADJ "; offFoulAdj
    Print "12. ENTER 99 IF USING 6-9 "; leagueRatings(4)
    Print "13. TEXT COLOR (0-15) "; teamRatings(0)
    Print "14. BACKGROUND COLOR (0-7) "; teamRatings(1)
    Print "15. TEAM FGA/G "; teamRatings(3)
    Print "16. LEAGUE FGA/G "; teamRatings(4)
    Print "17. TEAM GAMES PLAYED "; teamRatings(5)
    Print "18. HOMECOURT FACTOR "; teamRatings(6)
    Print "19. ARENA NAME  "; arenaName$
    Locate 4, 40
    Print "20. NICKNAME  "; mascot$
    Locate 5, 40
    Print "21. COACH  "; coach$
    Locate 6, 40
    Print "22. AVG ATTENDANCE "; teamAttendance&
    Locate 7, 40
    Print "23. WINS "; teamRatings(7)
    Locate 8, 40
    Print "24. LOSSES "; teamRatings(8)
    Locate 9, 40
    Print "25. POWER RATING "; teamRatings(9)

    If teamRatings(0) >= 0 And teamRatings(1) >= 0 Then
        Locate 24, 1
        Print "TEAM APPEARANCE: ";
        Color teamRatings(0), teamRatings(1)
        Print RTrim$(teamName$ + " " + mascot$)
        Color 15, 0
    End If

    Do

        Do
            Locate LAST_ROW - 2, 25
            Input "# TO CHANGE (0 TO CONTINUE) "; B9
        Loop Until B9 >= 0 And B9 <= 25

        Select Case B9

            Case 1
                Locate 4, 17: Print Space$(10)
                Locate 4, 1: Input " 1. TEAM NUMBER "; teamIdx
                Locate 4, 17: Print " "

            Case 2
                Locate 5, 12: Print Space$(10)
                Locate 5, 1: Input " 2. DISK ID"; yearNumber$
                Locate 5, 12: Print " "

            Case 3
                Locate 6, 15: Print Space$(20)
                Locate 6, 1: Input " 3. TEAM NAME "; teamName$
                Locate 6, 15: Print " "

            Case 4
                Locate 7, 24: Print Space$(10)
                Locate 7, 1: Input " 4. TEAM STAMINA RATING"; staminaRating!
                Locate 7, 24: Print " "

            Case 5
                Locate 8, 16: Print Space$(10)
                Locate 8, 1: Input " 5. DEF FG% ADJ"; defFGPctAdj!
                Locate 8, 16: Print " "

            Case 6
                Locate 9, 21: Print Space$(10)
                Locate 9, 1: Input " 6. DEF 3FGA/FGA ADJ"; defthreeFGA_Adj
                Locate 9, 21: Print " "

            Case 7
                Locate 10, 17: Print Space$(10)
                Locate 10, 1: Input " 7. DEF 3FG% ADJ"; def3FG_Adj
                Locate 10, 17: Print " "

            Case 8
                Locate 11, 21: Print Space$(10)
                Locate 11, 1: Input " 8. OFF STEAL RATING"; leagueRatings(0)
                Locate 11, 21: Print " "

            Case 9
                Locate 12, 24: Print Space$(10)
                Locate 12, 1: Input " 9. OFF TURNOVER RATING"; leagueRatings(1)
                Locate 12, 24: Print " "

            Case 10
                Locate 13, 21: Print Space$(10)
                Locate 13, 1: Input "10. DEF TURNOVER ADJ"; leagueRatings(2)
                Locate 13, 21: Print " "

            Case 11
                Locate 14, 17: Print Space$(10)
                Locate 14, 1: Input "11. OFF FOUL ADJ"; leagueRatings(3)
                Locate 14, 17: Print " "

            Case 12
                Locate 15, 26: Print Space$(10)
                Locate 15, 1: Input "12. ENTER 99 IF USING 6-9"; leagueRatings(4)
                Locate 15, 26: Print " "

            Case 13
                Locate 16, 22: Print Space$(10)
                Locate 16, 1: Input "13. TEXT COLOR (0-15)"; teamRatings(0)
                Locate 16, 22: Print " "

                If teamRatings(0) >= 0 And teamRatings(1) >= 0 Then
                    Locate 24, 1
                    Print "TEAM APPEARANCE: ";
                    Color teamRatings(0), teamRatings(1)
                    Print RTrim$(teamName$) + " " + RTrim$(mascot$)
                    Color 15, 0
                End If

            Case 14
                Locate 17, 27: Print Space$(10)
                Locate 17, 1: Input "14. BACKGROUND COLOR (0-7)"; teamRatings(1)
                Locate 17, 27: Print " "

                If teamRatings(0) >= 0 And teamRatings(1) >= 0 Then
                    Locate 24, 1
                    Print "TEAM APPEARANCE: ";
                    Color teamRatings(0), teamRatings(1)
                    Print RTrim$(teamName$) + " " + RTrim$(mascot$)
                    Color 15, 0
                End If

            Case 15
                Locate 18, 15: Print Space$(10)
                Locate 18, 1: Input "15. TEAM FGA/G"; teamRatings(3)
                Locate 18, 15: Print " "

            Case 16
                Locate 19, 17: Print Space$(10)
                Locate 19, 1: Input "16. LEAGUE FGA/G"; teamRatings(4)
                Locate 19, 17: Print " "

            Case 17
                Locate 20, 22: Print Space$(10)
                Locate 20, 1: Input "17. TEAM GAMES PLAYED"; teamRatings(5)
                Locate 20, 22: Print " "

            Case 18
                Locate 21, 21: Print Space$(10)
                Locate 21, 1: Input "18. HOMECOURT FACTOR"; teamRatings(6)
                Locate 21, 21: Print " "

                If teamRatings(6) < 0 Then teamRatings(6) = 0
                If teamRatings(6) > 6 Then teamRatings(6) = 6

            Case 19
                Locate 22, 16: Print Space$(40)
                Locate 22, 1: Input "19. ARENA NAME "; arenaName$
                Locate 22, 16: Print " "

            Case 20
                Locate 4, 53: Print Space$(15)
                Locate 4, 40: Input "20. NICKNAME "; mascot$
                Locate 4, 53: Print " "

            Case 21
                Locate 5, 50: Print Space$(20)
                Locate 5, 40: Input "21. COACH "; coach$
                Locate 5, 50: Print " "

            Case 22
                Locate 6, 58: Print Space$(10)
                Locate 6, 40: Input "22. AVG ATTENDANCE"; teamAttendance&
                Locate 6, 58: Print " "

            Case 23
                Locate 7, 48: Print Space$(10)
                Locate 7, 40: Input "23. WINS"; teamRatings(7)
                Locate 7, 48: Print " "

            Case 24
                Locate 8, 50: Print Space$(10)
                Locate 8, 40: Input "24. LOSSES"; teamRatings(8)
                Locate 8, 50: Print " "

            Case 25
                Locate 9, 56: Print Space$(10)
                Locate 9, 40: Input "25. POWER RATING"; teamRatings(9)
                Locate 9, 56: Print " "

        End Select

        Locate LAST_ROW - 2, 53
        Print Space$(10)

    Loop Until B9 = 0

    If defthreeFGA_Adj < 0 Then
        defthreeFGA_Adj = 100 + defthreeFGA_Adj
    End If

    If def3FG_Adj < 0 Then
        def3FG_Adj = 100 + def3FG_Adj
    End If

    If leagueRatings(0) < 0 Then
        leagueRatings(0) = 100 + leagueRatings(0)
    End If

    If leagueRatings(2) < 0 Then
        leagueRatings(2) = 100 + leagueRatings(0)
    End If

    If leagueRatings(3) < 0 Then
        leagueRatings(3) = 100 + leagueRatings(0)
    End If

    Color 15, 0
    Cls

    Color 14
    Locate 2, 30: Print "STRENGTH OF SCHEDULE"
    Color 15

    newSos = -99

    Do

        Locate 4, 5: Print "DO YOU WANT TO CHANGE THIS TEAM'S STRENGTH OF SCHEDULE? (Y/N)"
        Locate 5, 5: Print "(THIS ALSO REQUIRES OPP 2FG% AND SEASON FG%)"
        I$ = GetKeyPress$

    Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y" Or I$ = Chr$(27)

    If UCase$(I$) = "Y" Then

        Locate 7, 1: Input "1. OPP 2FG % "; newOpp2FGPct!
        Locate 7, 14: Print " "
        Locate 8, 1: Input "2. SEASON FG % "; newLeagFGPct!
        Locate 8, 16: Print " "
        Locate 9, 1: Input "3. STRENGTH OF SCHEDULE "; newSos
        Locate 9, 25: Print " "

        Do

            Do
                Locate LAST_ROW - 2, 25
                Input "# TO CHANGE (0 TO CONTINUE) "; B9
            Loop Until B9 >= 0 And B9 <= 3

            Select Case B9

                Case 1
                    Locate 7, 13: Print Space$(10)
                    Locate 7, 1: Input "1. OPP 2FG %"; newOpp2FGPct!
                    Locate 7, 13: Print " "

                Case 2
                    Locate 8, 15: Print Space$(10)
                    Locate 8, 1: Input "2. SEASON FG %"; newLeagFGPct!
                    Locate 8, 15: Print " "

                Case 3
                    Locate 9, 24: Print Space$(10)
                    Locate 9, 1: Input "3. STRENGTH OF SCHEDULE"; newSos
                    Locate 9, 24: Print " "

            End Select

        Loop Until B9 = 0

    End If

    Color 15, 0
    Cls

    Color 14
    Locate 2
    Print Tab(30); "CHANGE ACTUAL STATS"
    Color 15

    teamStats!(0) = teamRatings(5)

    Color 14
    Locate 4, 15: Print " TEAM"
    Locate 4, 30: Print " OPPONENTS"
    Color 15

    Print " 1. FGA "; Tab(15); teamStats!(1); Tab(30); teamStats!(2)
    Print " 2. FG % "; Tab(15); teamStats!(3); Tab(30); teamStats!(4)
    Print " 3. 3 FGA "; Tab(15); teamStats!(21); Tab(30); teamStats!(19)
    Print " 4. 3 FG % "; Tab(15); teamStats!(22); Tab(30); teamStats!(20)
    Print " 5. FTA "; Tab(15); teamStats!(5); Tab(30); teamStats!(6)
    Print " 6. FT % "; Tab(15); teamStats!(7); Tab(30); teamStats!(8)
    Print " 7. RB "; Tab(15); teamStats!(9); Tab(30); teamStats!(10)
    Print " 8. AST "; Tab(15); teamStats!(11); Tab(30); teamStats!(12)
    Print " 9. STLS "; Tab(15); teamStats!(17); Tab(30); teamStats!(18)
    Print "10. BLOCKS "; Tab(15); teamStats!(23); Tab(30); teamStats!(24)
    Print "11. TO "; Tab(15); teamStats!(15); Tab(30); teamStats!(16)
    Print "12. FOULS "; Tab(15); teamStats!(13); Tab(30); teamStats!(14)

    Do

        Do
            Locate 23, 25
            Input "# TO CHANGE (0 TO CONTINUE)"; B9
        Loop Until B9 >= 0 And B9 <= 12

        Select Case B9

            Case 1
                Locate 5, 1: Print " 1. FGA ";
                Locate 5, 14: Print Space$(10)
                Locate 5, 14: Input teamStats!(1)
                Locate 5, 14: Print " "
                Locate 5, 29: Print Space$(10)
                Locate 5, 29: Input teamStats!(2)
                Locate 5, 29: Print " "

            Case 2
                Locate 6, 1: Print " 2. FG % ";
                Locate 6, 14: Print Space$(10)
                Locate 6, 14: Input teamStats!(3)
                Locate 6, 14: Print " "
                Locate 6, 29: Print Space$(10)
                Locate 6, 29: Input teamStats!(4)
                Locate 6, 29: Print " "

            Case 3
                Locate 7, 1: Print " 3. 3 FGA ";
                Locate 7, 14: Print Space$(10)
                Locate 7, 14: Input teamStats!(21)
                Locate 7, 14: Print " "
                Locate 7, 29: Print Space$(10)
                Locate 7, 29: Input teamStats!(19)
                Locate 7, 29: Print " "

            Case 4
                Locate 8, 1: Print " 4. 3 FG % ";
                Locate 8, 14: Print Space$(10)
                Locate 8, 14: Input teamStats!(22)
                Locate 8, 14: Print " "
                Locate 8, 29: Print Space$(10)
                Locate 8, 29: Input teamStats!(20)
                Locate 8, 29: Print " "

            Case 5
                Locate 9, 1: Print " 5. FTA ";
                Locate 9, 14: Print Space$(10)
                Locate 9, 14: Input teamStats!(5)
                Locate 9, 14: Print " "
                Locate 9, 29: Print Space$(10)
                Locate 9, 29: Input teamStats!(6)
                Locate 9, 29: Print " "

            Case 6
                Locate 10, 1: Print " 6. FT % "
                Locate 10, 14: Print Space$(10)
                Locate 10, 14: Input teamStats!(7)
                Locate 10, 14: Print " "
                Locate 10, 29: Print Space$(10)
                Locate 10, 29: Input teamStats!(8)
                Locate 10, 29: Print " "

            Case 7
                Locate 11, 1: Print " 7. RB ";
                Locate 11, 14: Print Space$(10)
                Locate 11, 14: Input teamStats!(9)
                Locate 11, 14: Print " "
                Locate 11, 29: Print Space$(10)
                Locate 11, 29: Input teamStats!(10)
                Locate 11, 29: Print " "

            Case 8
                Locate 12, 1: Print " 8. AST ";
                Locate 12, 14: Print Space$(10)
                Locate 12, 14: Input teamStats!(11)
                Locate 12, 14: Print " "
                Locate 12, 29: Print Space$(10)
                Locate 12, 29: Input teamStats!(12)
                Locate 12, 29: Print " "

            Case 9
                Locate 13, 1: Print " 9. STLS ";
                Locate 13, 14: Print Space$(10)
                Locate 13, 14: Input teamStats!(17)
                Locate 13, 14: Print " "
                Locate 13, 29: Print Space$(10)
                Locate 13, 29: Input teamStats!(18)
                Locate 13, 29: Print " "

            Case 10
                Locate 14, 1: Print "10. BLOCKS ";
                Locate 14, 14: Print Space$(10)
                Locate 14, 14: Input teamStats!(23)
                Locate 14, 14: Print " "
                Locate 14, 29: Print Space$(10)
                Locate 14, 29: Input teamStats!(24)
                Locate 14, 29: Print " "

            Case 11
                Locate 15, 1: Print "11. TO ";
                Locate 15, 14: Print Space$(10)
                Locate 15, 14: Input teamStats!(15)
                Locate 15, 14: Print " "
                Locate 15, 29: Print Space$(10)
                Locate 15, 29: Input teamStats!(16)
                Locate 15, 29: Print " "

            Case 12
                Locate 16, 1: Print "12. FOULS ";
                Locate 16, 14: Print Space$(10)
                Locate 16, 14: Input teamStats!(13)
                Locate 16, 14: Print " "
                Locate 16, 29: Print Space$(10)
                Locate 16, 29: Input teamStats!(14)
                Locate 16, 29: Print " "

        End Select

        Locate 23, 40
        Print Space$(10)

    Loop Until B9 = 0


    '---------------
    'Change Players
    '---------------

    Do

        Cls
        Color 14
        Locate 2, 32: Print "CHANGE PLAYERS"
        Color 15

        For I = 0 To 6
            Locate I + 4, 5
            Print Using "##"; I + 1;
            Print " "; playerNames$(I); Tab(37);
            Print Using "##"; I + 8;
            Print " "; playerNames$(I + 7)
        Next

        Print

        Do
            Locate 12, 37
            Print Space$(10)
            Locate 12, 9
            Input "# TO CHANGE (1-13) (0 TO CONTINUE) "; idx
        Loop Until idx >= 0 And idx <= 14

        If idx > 0 Then

            I = idx - 1

            Cls
            Color 14
            Locate 2
            Print Tab(35); "PLAYER - "; idx
            Locate 4
            Print Tab(31); "CHANGE PLAYER STATS"
            Color 15
            Print

            Print " 1. NAME   "; playerNames$(I); Tab(37); "16. GAMES  "; playerStats!(I, 0)
            Print " 2. POSITION   "; position$(I); Tab(37); "17. MINUTES  "; playerStats!(I, 1)
            Print " 3. GAMES PLAYED  "; playerRatings!(I, 0); Tab(37); "18. FGM  "; playerStats!(I, 3)
            Print " 4. ADJ FG%  "; playerOff!(I, 0); Tab(37); "19. FGA  "; playerStats!(I, 2)
            Print " 5. FT%  "; playerOff!(I, 1); Tab(37); "20. FTM  "; playerStats!(I, 5)
            Print " 6. OFF RB/40 MIN X 10  "; playerRatings!(I, 4); Tab(37); "21. FTA  "; playerStats!(I, 4)
            Print " 7. DEF RB/40 MIN X 10  "; playerOff!(I, 2); Tab(37); "22. OFF REB  "; playerStats!(I, 6)
            Print " 8. ASSIST RATING  "; playerOff!(I, 4); Tab(37); "23. TOTAL REB  "; playerStats!(I, 7)
            Print " 9. STEAL RATING  "; playerOff!(I, 5); Tab(37); "24. PER FOULS  "; playerStats!(I, 8)
            Print "10. BLOCK RATING  "; playerOff!(I, 6); Tab(37); "25. ASSISTS  "; playerStats!(I, 9)
            Print "11. FGA  "; playerRatings!(I, 1); Tab(37); "26. BLOCKS  "; playerStats!(I, 10)
            Print "12. FOUL COMMIT RATING  "; playerRatings!(I, 5); Tab(37); "27. STEALS  "; playerStats!(I, 11)
            Print "13. FOUL DRAWING RATING  "; playerRatings!(I, 6); Tab(37); "28. 3 FGM  "; playerStats!(I, 13)
            Print "14. 3FG%  "; playerRatings!(I, 2); Tab(37); "29. 3 FGA  "; playerStats!(I, 12)
            Print "15. 3FGA/TOT FGA  "; playerRatings!(I, 3)
            'Print "16. DEFENSIVE RATING (1-9)  "; playerDef!(I)

            Do

                Do
                    Locate 22, 25
                    Input "# TO CHANGE (0 TO CONTINUE) "; B9
                Loop Until B9 >= 0 And B9 <= 29

                Select Case B9

                    Case 1
                        Locate 6, 10: Print Space$(20)
                        Locate 6, 1: Input " 1. NAME "; playerNames$(I)
                        Locate 6, 10: Print " "

                    Case 2
                        Locate 7, 14: Print Space$(10)
                        Locate 7, 1: Input " 2. POSITION "; position$(I)
                        Locate 7, 14: Print " "

                    Case 3
                        Locate 8, 17: Print Space$(10)
                        Locate 8, 1: Input " 3. GAMES PLAYED"; playerRatings!(I, 0)
                        Locate 8, 17: Print " "

                    Case 4
                        Locate 9, 12: Print Space$(10)
                        Locate 9, 1: Input " 4. ADJ FG%"; playerOff!(I, 0)
                        Locate 9, 12: Print " "

                    Case 5
                        Locate 10, 8: Print Space$(10)
                        Locate 10, 1: Input " 5. FT%"; playerOff!(I, 1)
                        Locate 10, 8: Print " "

                    Case 6
                        Locate 11, 23: Print Space$(10)
                        Locate 11, 1: Input " 6. OFF RB/40 MIN X 10"; playerRatings!(I, 4)
                        Locate 11, 23: Print " "

                    Case 7
                        Locate 12, 23: Print Space$(10)
                        Locate 12, 1: Input " 7. DEF RB/40 MIN X 10"; playerOff!(I, 2)
                        Locate 12, 23: Print " "

                    Case 8
                        Locate 13, 18: Print Space$(10)
                        Locate 13, 1: Input " 8. ASSIST RATING"; playerOff!(I, 4)
                        Locate 13, 18: Print " "

                    Case 9
                        Locate 14, 17: Print Space$(10)
                        Locate 14, 1: Input " 9. STEAL RATING"; playerOff!(I, 5)
                        Locate 14, 17: Print " "

                    Case 10
                        Locate 15, 17: Print Space$(10)
                        Locate 15, 1: Input "10. BLOCK RATING"; playerOff!(I, 6)
                        Locate 15, 17: Print " "

                    Case 11
                        Locate 16, 8: Print Space$(10)
                        Locate 16, 1: Input "11. FGA"; playerRatings!(I, 1)
                        Locate 16, 8: Print " "

                    Case 12
                        Locate 17, 24: Print Space$(10)
                        Locate 17, 1: Input "12. FOUL COMMIT RATING"; playerRatings!(I, 5)
                        Locate 17, 24: Print " "

                    Case 13
                        Locate 18, 24: Print Space$(10)
                        Locate 18, 1: Input "13. FOUL DRAWING RATING"; playerRatings!(I, 6)
                        Locate 18, 24: Print " "

                    Case 14
                        Locate 19, 9: Print Space$(10)
                        Locate 19, 1: Input "14. 3FG%"; playerRatings!(I, 2)
                        Locate 19, 9: Print " "

                    Case 15
                        Locate 20, 17: Print Space$(10)
                        Locate 20, 1: Input "15. 3FGA/TOT FGA"; playerRatings!(I, 3)
                        Locate 20, 17: Print " "

                        'Case 16
                        'Locate 18, 28: Print Space$(10)
                        'Locate 18, 1: Input "16. DEFENSIVE RATING (1-9) "; playerDef!(I)
                        'Locate 18, 28: Print " "

                    Case 16
                        Locate 6, 46: Print Space$(10)
                        Locate 6, 37: Input "17. GAMES"; playerStats!(I, 0)
                        Locate 6, 46: Print " "

                    Case 17
                        Locate 7, 48: Print Space$(10)
                        Locate 7, 37: Input "18. MINUTES"; playerStats!(I, 1)
                        Locate 7, 48: Print " "

                    Case 18
                        Locate 8, 44: Print Space$(10)
                        Locate 8, 37: Input "19. FGM"; playerStats!(I, 3)
                        Locate 8, 44: Print " "

                    Case 19
                        Locate 9, 44: Print Space$(10)
                        Locate 9, 37: Input "20. FGA"; playerStats!(I, 2)
                        Locate 9, 44: Print " "

                    Case 20
                        Locate 10, 44: Print Space$(10)
                        Locate 10, 37: Input "21. FTM"; playerStats!(I, 5)
                        Locate 10, 44: Print " "

                    Case 21
                        Locate 11, 44: Print Space$(10)
                        Locate 11, 37: Input "22. FTA"; playerStats!(I, 4)
                        Locate 11, 44: Print " "

                    Case 22
                        Locate 12, 48: Print Space$(10)
                        Locate 12, 37: Input "23. OFF REB"; playerStats!(I, 6)
                        Locate 12, 48: Print " "

                    Case 23
                        Locate 13, 50: Print Space$(10)
                        Locate 13, 37: Input "24. TOTAL REB"; playerStats!(I, 7)
                        Locate 13, 50: Print " "

                    Case 24
                        Locate 14, 50: Print Space$(10)
                        Locate 14, 37: Input "25. PER FOULS"; playerStats!(I, 8)
                        Locate 14, 50: Print " "

                    Case 25
                        Locate 15, 48: Print Space$(10)
                        Locate 15, 37: Input "26. ASSISTS"; playerStats!(I, 9)
                        Locate 15, 48: Print " "

                    Case 26
                        Locate 16, 47: Print Space$(10)
                        Locate 16, 37: Input "27. BLOCKS"; playerStats!(I, 10)
                        Locate 16, 47: Print " "

                    Case 27
                        Locate 17, 47: Print Space$(10)
                        Locate 17, 37: Input "28. STEALS"; playerStats!(I, 11)
                        Locate 17, 47: Print " "

                    Case 28
                        Locate 18, 46: Print Space$(10)
                        Locate 18, 37: Input "29. 3 FGM"; playerStats!(I, 13)
                        Locate 18, 46: Print " "

                    Case 29
                        Locate 19, 46: Print Space$(10)
                        Locate 19, 37: Input "30. 3 FGA"; playerStats!(I, 12)
                        Locate 19, 46: Print " "

                End Select

                Locate 22, 53
                Print Space$(10)

            Loop Until B9 = 0

        End If

    Loop Until idx = 0


    'Adjust ratings based on new SOS
    If newSos <> -99 Then
        Call CalcSosAdjustments(newOpp2FGPct!, newLeagFGPct!, newSos)
    End If


    '---------------
    'Player Contributions
    '---------------
    Do

        Cls

        Color teamRatings(0), teamRatings(1)
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
            Locate 21, 46
            Print Space$(10);
            Locate 21, 1
            Input "SELECT PLAYER # TO CHANGE (0-NO MORE CHANGE) "; X9
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

            If (totalCX > 99 And totalCX < 101) Or X9 = 0 Then

                Call SaveChangedTeam(0, yearNumber$, teamName$, teamIdx)

            Else
                'We need to warn the user that
                'our total doesn't add up!!
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

            End If

        End If

    Loop Until X9 = 0

End Sub


'----------------------------------------
'         Roster_Report Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'team roster to the default printer
Sub Roster_Report (printDest, destFile$, teamName$)

    Shared def3FG_Adj, defthreeFGA_Adj
    Shared defFGPctAdj!, staminaRating!

    Shared mascot$, coach$, arenaName$

    Shared leagueRatings(), teamRatings(), teamStats!()

    Shared playerOff!(), playerDef!(), playerRatings!()

    Shared playerNames$(), position$()

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    XCoord = (80 - 16 - Len(targetTeam$)) / 2

    Print #1, Tab(XCoord); "TEAM ROSTER FOR "; teamName$
    Print #1, Tab(20);
    Print #1, "TEXT COLOR:"; teamRatings(0); Tab(38);
    Print #1, "BACKGROUND COLOR:"; teamRatings(1)
    Print #1,

    offStRat = leagueRatings(0)
    If offStRat > 20 Then
        offStRat = leagueRatings(0) - 100
    End If

    defTOAdj = leagueRatings(2)
    If defTOAdj > 20 Then
        defTOAdj = leagueRatings(2) - 100
    End If

    offFoulAdj = leagueRatings(3)
    If offFoulAdj > 20 Then
        offFoulAdj = leagueRatings(3) - 100
    End If

    d3FGAdj = def3FG_Adj
    If d3FGAdj > 20 Then
        d3FGAdj = def3FG_Adj - 100
    End If

    dthreeFGAAdj = defthreeFGA_Adj
    If dthreeFGAAdj > 20 Then
        dthreeFGAAdj = defthreeFGA_Adj - 100
    End If

    Print #1, "AVG SHOTS / GAME       = "; teamRatings(3);
    Print #1, Tab(30); "DEF FG ADJ    = "; defFGPctAdj!
    Print #1, "OPP PCT OF 3FG ATT ADJ = "; dthreeFGAAdj;
    Print #1, Tab(30); "DEF STEAL ADJ = "; offStRat;
    Print #1, Tab(50); "OFF TURNOVER ADJ = "; leagueRatings(1)
    Print #1, "OPP 3FG PCT ADJ        = "; d3FGAdj;
    Print #1, Tab(30); "OFF FOUL ADJ  = "; offFoulAdj;
    Print #1, Tab(50); "DEF TURNOVER ADJ = "; defTOAdj

    Print #1,
    Print #1, "NAME           FG% FT% ORB DRB  CX PS  D  B  FC FD  3FG 3FG%"

    For I = 0 To 13
        If playerNames$(I) <> "XXX" Then
            Print #1, playerNames$(I); Tab(16);
            'FG% --> CX
            Print #1, Using "### ### ### ### ### "; playerOff!(I, 0); playerOff!(I, 1); playerRatings!(I, 4); playerOff!(I, 2); playerOff!(I, 3);
            'PS --> FD
            Print #1, Using "## ## ## ### ##  ###  ###"; playerOff!(I, 4); playerOff!(I, 5); playerOff!(I, 6); playerRatings!(I, 5); playerRatings!(I, 6); playerRatings!(I, 3); playerRatings!(I, 2)
        End If
    Next I

    Print #1,
    Print #1, "RB - REBOUNDING RATING                 B - BLOCK SHOT RATING"
    Print #1, "CX - CONTRIBUTION FACTOR               FC - FOUL COMMIT RATING"
    Print #1, "PS - PASSING RATING                    FD - FOUL DRAWING RATING"
    Print #1, " D - DEFENSIVE RATING                  3FG - 3FGA/TOTAL FGA"

    If printDest = 1 Then

        Open destFile$ For Input As #2

        Do While Not EOF(2)
            Line Input #2, X$
            LPrint X$
        Loop

        Close #2

        LPrint Chr$(12)

        Kill destFile$

    End If

End Sub


'----------------------------------------
'         ViewRoster Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'team roster to the user's display
Sub ViewRoster (targetTeam$)

    Shared def3FG_Adj, defthreeFGA_Adj
    Shared defFGPctAdj!, staminaRating!

    Shared mascot$, coach$, arenaName$

    Shared leagueRatings(), teamRatings(), teamStats!()

    Shared playerOff!(), playerDef!(), playerRatings!()

    Shared playerNames$(), position$()

    Color 15, 0
    Cls

    offStRat = leagueRatings(0)
    If offStRat > 20 Then
        offStRat = leagueRatings(0) - 100
    End If

    defTOAdj = leagueRatings(2)
    If defTOAdj > 20 Then
        defTOAdj = leagueRatings(2) - 100
    End If

    offFoulAdj = leagueRatings(3)
    If offFoulAdj > 20 Then
        offFoulAdj = leagueRatings(3) - 100
    End If

    d3FGAdj = def3FG_Adj
    If d3FGAdj > 20 Then
        d3FGAdj = def3FG_Adj - 100
    End If

    dthreeFGAAdj = defthreeFGA_Adj
    If dthreeFGAAdj > 20 Then
        dthreeFGAAdj = defthreeFGA_Adj - 100
    End If

    XCoord = (80 - 16 - Len(targetTeam$)) / 2
    Locate 2, XCoord

    Print "TEAM ROSTER FOR ";
    Color teamRatings(0), teamRatings(1): Print targetTeam$
    Color 15, 0
    Locate , 20
    Print "TEXT COLOR:"; teamRatings(0); Tab(38);
    Print "BACKGROUND COLOR:"; teamRatings(1)

    Locate 5, 1
    Color 11, 0: Print "AVG SHOTS / GAME       = ";
    Color 15: Print teamRatings(3);

    Color 11: Locate , 30: Print "DEF FG ADJ    = ";
    Color 15: Print defFGPctAdj!

    Color 11: Print "OPP PCT OF 3FG ATT ADJ = ";
    Color 15: Print dthreeFGAAdj;

    Color 11: Locate , 30: Print "DEF STEAL ADJ = ";
    Color 15: Print offStRat;

    Color 11: Locate , 50: Print "OFF TURNOVER ADJ = ";
    Color 15: Print leagueRatings(1)

    Color 11: Print "OPP 3FG PCT ADJ        = ";
    Color 15: Print d3FGAdj;

    Color 11: Locate , 30: Print "OFF FOUL ADJ  = ";
    Color 15: Print offFoulAdj;

    Color 11: Locate , 50: Print "DEF TURNOVER ADJ = ";
    Color 15: Print defTOAdj

    Locate 9, 1
    Color 14: Print "NAME           FG% FT% ORB DRB  CX PS  D  B  FC FD  3FG 3FG%"
    Color 15

    For I = 0 To 13
        If playerNames$(I) <> "XXX" Then
            Print playerNames$(I); Tab(16);
            'FG% --> CX
            Print Using "### ### ### ### ### "; playerOff!(I, 0); playerOff!(I, 1); playerRatings!(I, 4); playerOff!(I, 2); playerOff!(I, 3);
            'PS --> FD
            Print Using "## ## ## ### ##  ###  ###"; playerOff!(I, 4); playerOff!(I, 5); playerOff!(I, 6); playerRatings!(I, 5); playerRatings!(I, 6); playerRatings!(I, 3); playerRatings!(I, 2)
        End If
    Next I

    Print
    Color 2
    Print "RB";
    Color 15
    Print " - REBOUNDING RATING";

    Locate , 32
    Color 2
    Print " B";
    Color 15
    Print " - BLOCK SHOT RATING"

    ' -- New Line
    Color 2
    Print "CX";
    Color 15
    Print " - CONTRIBUTION FACTOR";

    Locate , 32
    Color 2
    Print "FC";
    Color 15
    Print " - FOUL COMMIT RATING"

    ' -- New Line
    Color 2
    Print "PS";
    Color 15
    Print " - PASSING RATING";

    Locate , 32
    Color 2
    Print "FD";
    Color 15
    Print " - FOUL DRAWING RATING"

    ' -- New Line
    Color 2
    Print "D";
    Color 15
    Print " - DEFENSIVE RATING";

    Locate , 32
    Color 2
    Print "3FG";
    Color 15
    Print " - 3FGA/TOTAL FGA"

    Locate LAST_ROW - 1, 30
    Color 14: Print "HIT ANY KEY TO EXIT";
    I$ = GetKeyPress$

End Sub


'----------------------------------------
'     CalcSosAdjustments Subroutine
'----------------------------------------
'This routine makes adjustments to the
'team and player ratings based on the
'SOS that was provided
Sub CalcSosAdjustments (opp2FGPct!, seasonFGPct!, sosVal)

    Shared defFGPctAdj!

    Shared playerOff!(), teamStats!()

    sosAdj = (sosVal / 2) / ((teamStats!(1) + teamStats!(2)) / (teamStats!(0) * 2))
    sosAdj = CInt((sosAdj * 100))

    sosVal = Int(sosAdj / 2)
    sosDupe = Int(sosAdj / 2)
    coin = Int(Rnd(1) * 2)

    If sosAdj - sosVal - sosDupe > 0 Then
        If coin = 0 Then
            sosVal = sosVal + 1
        Else
            sosDupe = sosDupe + 1
        End If
    End If

    'Def FG % Adj
    lgFgPctAdj = Int((46 - seasonFGPct!) / 2)
    defFGPctAdj! = opp2FGPct! - seasonFGPct! + sosDupe - lgFgPctAdj

    For I = 0 To 13

        If playerNames$(I) <> "XXX" Then

            'Adjust player FG% by SOS and lgFgPctAdj (how far league FG% is from 46)
            playerOff!(I, 0) = playerOff!(I, 0) - sosVal + lgFgPctAdj

            X! = ((playerOff!(I, 9) * playerOff!(I, 0)) / 100)
            MI = (playerOff!(I, 8) / playerOff!(I, 7)) / 40

            If playerOff!(I, 8) = 0 Then
                MI = (playerOff!(I, 9) + playerOff!(I, 10)) / (X9! / 5)
            End If

            X! = X! * .1

            If MI > 1 And playerOff!(I, 8) = 0 Then
                MI = 1
            End If

            X! = X! + ((playerOff!(I, 7) * 1.64) * MI)
            X! = ((playerOff!(I, 16) - X!) / 2)
            X! = CInt(((X! / (playerOff!(I, 9) + X!)) * 100))

            If X! < 0 Then X! = 0

            'Foul Draw rating
            playerRatings!(I, 6) = X!

        End If

    Next I

End Sub


'----------------------------------------
'       SaveChangedTeam Subroutine
'----------------------------------------
'This routine saves the modified data back
'to the original team file
Sub SaveChangedTeam (newID, yearNumber$, teamName$, teamIdx)

    Shared def3FG_Adj, defthreeFGA_Adj
    Shared teamAttendance&, defFGPctAdj!, staminaRating!
    Shared mascot$, coach$, arenaName$

    Shared leagueRatings(), teamRatings(), teamStats!()
    Shared playerNames$(), position$()
    Shared playerOff!(), playerDef!(), playerStats!(), playerRatings!()

    Call CSMsgBox("SAVING TEAM FILES", 20, PASS)

    teamYear$ = RTrim$(yearNumber$)

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
    LSet Q$(4) = MKI$(defthreeFGA_Adj)

    For I = 0 To 4:
        LSet Q$(I + 5) = MKI$(leagueRatings(I))
    Next

    teamRatings(2) = 999

    For I = 0 To 9:
        LSet Q$(I + 10) = MKI$(teamRatings(I))
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

    Open diskPaths$(0) + DATA_FILE_NAME$ + "." + teamYear$ For Random As #2 Len = DATA_SIZE_BYTES

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

    Call CSMsgBox("", 0, PASS)

End Sub
