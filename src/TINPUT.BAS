'----------------------------------------
'           InputTeam Subroutine
'----------------------------------------
'This subroutine assists the user with the
'steps for entering a new team.
Sub InputTeam ()

    _Title "Courtside College Basketball - Team Input"

    Shared nbrGmsPlayed!, opp3FGA!, opp3FGAPct!, oppAsst!, oppBlocks!
    Shared oppFGPct!, oppFouls!, oppFGA!, oppFTA!, oppFTPct!
    Shared oppReb!, oppSteal!, oppTO!
    Shared tm3FGA!, tm3FGAPct!, tmAsst!, tmBlocks!, tmFouls!
    Shared tmFGA!, tmFGPct!, tmFTA!, tmFTPct!
    Shared tmReb!, tmSteal!, tmTO!

    Shared def3FG_Adj, def3FGA_Adj
    Shared teamAttendance&, defFGPctAdj!, staminaRating!
    Shared mascot$, coach$, arenaName$

    Dim inputRatings(16)
    Dim inputQQ(3), inputX$(3)

    Shared teamRatings%()

    Shared playerOff!(), playerDef!(), leagueRatings()
    Shared playerStats!(), playerRatings!()

    Shared playerNames$(), position$()

    inputX$(0) = "COLLEGE BASKETBALL"
    inputX$(1) = "H S BASKETBALL"

    Cls

    Do

        Color 12: Print "SELECT TYPE OF TEAMS YOU ARE ENTERING"

        Print
        Color 14: Print "(0) ";
        Color 15: Print "COLLEGE BASKETBALL"

        Print
        Color 14: Print "(1) ";
        Color 15: Print "HIGH SCHOOL BASKETBALL"

        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then
            choice = Val(I$)
        End If

    Loop Until I$ = Chr$(27) Or choice = 0 Or choice = 1

    If I$ <> Chr$(27) Then

        'L6 = average assists / team ???
        'L7 is not used; average steals / game?
        If choice = 0 Then
            'College BB
            L7 = 6
            L6 = 14
            tmFouls! = 20
        Else
            'High School BB
            L7 = 5
            L6 = 11
            tmFouls! = 16
        End If

        '---------------
        ' Season Stats
        '---------------
        Cls

        Color 14
        Print inputX$(choice); Tab(31); "LEAGUE INFORMATION"
        Color 15

        Locate 3, 1: Input "1. SEASON FGA/G "; leagFGAPerGame!
        'Locate 3, 17: Print " "

        Locate 4, 1: Input "2. SEASON REBOUNDS/GAME "; leagRebPerGame!
        'Locate 4, 25: Print " "

        Locate 5, 1: Input "3. SEASON FG % "; leagFGPct!
        'Locate 5, 16: Print " "

        Locate 6, 1: Input "4. SEASON STEALS/GAME "; leagStlsPerGame!
        'Locate 6, 23: Print " "

        Locate 7, 1: Input "5. SEASON TURNOVERS/GAME "; leagTOPerGame!
        'Locate 7, 26: Print " "

        Locate 8, 1: Input "6. SEASON 3 FGA/FGA % "; leag3FGAPct!
        'Locate 8, 23: Print " "

        Locate 9, 1: Input "7. SEASON 3 FG % "; leag3FGPct!
        'Locate 9, 18: Print " "

        teamRatings%(2) = 999

        Do

            Do
                Locate 22, 1
                Input "# TO CHANGE (0 TO CONTINUE)"; B9
            Loop Until B9 >= 0 And B9 <= 7

            Select Case B9

                Case 1:
                    Locate 3, 17: Print Space$(10)
                    Locate 3, 1: Input "1. SEASON FGA/G "; leagFGAPerGame!
                    Locate 3, 17: Print " "

                Case 2:
                    Locate 4, 25: Print Space$(10)
                    Locate 4, 1: Input "2. SEASON REBOUNDS/GAME "; leagRebPerGame!
                    Locate 4, 25: Print " "

                Case 3:
                    Locate 5, 16: Print Space$(10)
                    Locate 5, 1: Input "3. SEASON FG % "; leagFGPct!
                    Locate 5, 16: Print " "

                Case 4
                    Locate 6, 23: Print Space$(10)
                    Locate 6, 1: Input "4. SEASON STEALS/GAME "; leagStlsPerGame!
                    Locate 6, 23: Print " "

                Case 5
                    Locate 7, 26: Print Space$(10)
                    Locate 7, 1: Input "5. SEASON TURNOVERS/GAME "; leagTOPerGame!
                    Locate 7, 26: Print " "

                Case 6
                    Locate 8, 23: Print Space$(10)
                    Locate 8, 1: Input "6. SEASON 3 FGA/FGA % "; leag3FGAPct!
                    Locate 8, 23: Print " "

                Case 7
                    Locate 9, 18: Print Space$(10)
                    Locate 9, 1: Input "7. SEASON 3 FG % "; leag3FGPct!
                    Locate 9, 18: Print " "

            End Select

            Locate 22, 27
            Print Space$(10)

        Loop Until B9 = 0


        '---------------
        ' Required Stats
        '---------------
        Do

            Cls

            Print "YOU MUST HAVE THE FOLLOWING TEAM &"
            Print "INDIVIDUAL STATS: "
            Print "FGA, FG %, FTA, FT %, REBOUNDS"
            Print
            Print "IF YOU ARE INPUTTING A TEAM WITH ANY "
            Print "OF THE FOLLOWING STATS MISSING,"
            Print "SIMPLY TYPE 'N' AND THE PROGRAM WILL"
            Print "ALLOW YOU TO INPUT RATINGS DIRECTLY"
            Print
            Color 12: Print "DO YOU HAVE THE FOLLOWING INFORMATION :"

            Print
            Color 14: Print "(1) ";
            Color 15: Print "TEAM/INDIVID ASSISTS? (Y/N): ";

            Do
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = Chr$(27)

            If I$ <> Chr$(27) Then

                Print UCase$(I$)

                If UCase$(I$) = "Y" Then
                    inputQQ(0) = 1
                End If
                If UCase$(I$) = "N" Then
                    inputQQ(0) = 0
                End If

                Print
                Color 14: Print "(2) ";
                Color 15: Print "PERSONAL FOULS? (Y/N): ";

                Do
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = Chr$(27)

                If I$ <> Chr$(27) Then

                    Print UCase$(I$)

                    If UCase$(I$) = "Y" Then
                        inputQQ(1) = 1
                    End If
                    If UCase$(I$) = "N" Then
                        inputQQ(1) = 0
                    End If

                    Print
                    Color 14: Print "(3) ";
                    Color 15: Print "STEALS & TURNOVERS? (Y/N): ";

                    Do
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = Chr$(27)

                    If I$ <> Chr$(27) Then

                        Print UCase$(I$)

                        If UCase$(I$) = "Y" Then
                            inputQQ(2) = 1
                        End If
                        If UCase$(I$) = "N" Then
                            inputQQ(2) = 0
                        End If

                        Print
                        Color 14: Print "(4) ";
                        Color 15: Print "TEAM / INDIVIDUAL BLOCKS? (Y/N): ";

                        Do
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = Chr$(27)

                        If I$ <> Chr$(27) Then

                            Print UCase$(I$)

                            If UCase$(I$) = "Y" Then
                                inputQQ(3) = 1
                            End If
                            If UCase$(I$) = "N" Then
                                inputQQ(3) = 1
                            End If

                            Print

                            Do

                                Print "ANY CHANGE? (Y/N)";
                                I$ = GetKeyPress$

                            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or I$ = Chr$(27)

                        End If

                    End If

                End If

            End If

            If I$ = Chr$(27) Then Exit Sub

        Loop Until UCase$(I$) = "N"

        '---------------
        ' Team Stats
        '---------------
        Cls

        Locate 1
        Color 14
        Print inputX$(choice); Tab(32); "TEAM INFORMATION"
        Color 15

        Locate 2
        Input " 0. TEAM # (BLANK TO AUTO-ASSIGN)"; teamNumber

        Locate 3: Input " 1. DISK ID "; yearNumber$
        Locate 4: Input " 2. TEAM NAME "; teamName$
        Locate 5: Input " 3. TEAM NICKNAME "; mascot$
        Locate 6: Input " 4. STRENGTH OF SCHEDULE ADJ "; sosAdj
        Locate 7: Input " 5. HOMECOURT FACTOR (0-6) "; teamRatings%(6)
        Locate 8: Input " 6. TEXT COLOR (0-15) "; teamRatings%(0)
        Locate 9: Input " 7. BACKGROUND COLOR (0-7) "; teamRatings%(1)
        'Locate 8, 40
        'Print "TEAM APPEARANCE: ";
        Locate 9, 40
        Color teamRatings%(0), teamRatings%(1)
        Print teamName$ + " " + mascot$
        Color 15, 0

        Locate 2, 40: Input "21. GAMES PLAYED "; nbrGmsPlayed!
        Locate 3, 40: Input "22. WINS "; teamRatings%(7)
        Locate 4, 40: Input "23. LOSSES "; teamRatings%(8)
        Locate 5, 40: Input "24. POWER RATING "; teamRatings%(9)
        Locate 6, 40: Input "25. COACH "; coach$
        Locate 7, 40: Input "26. ARENA "; arenaName$
        Locate 8, 40: Input "27. AVG ATTENDANCE "; teamAttendance&

        Color 14
        Locate 10, 16: Print "TEAM"
        Locate 10, 30: Print "OPPONENTS"
        Color 15
        Locate 11, 1: Print " 8. FGA ";
        Locate 11, 14: Input tmFGA!
        Locate 11, 29: Input oppFGA!
        Locate 12, 1: Print " 9. FG % ";
        Locate 12, 14: Input tmFGPct!
        Locate 12, 29: Input oppFGPct!
        Locate 13, 1: Print "10. FTA";
        Locate 13, 14: Input tmFTA!
        Locate 13, 29: Input oppFTA!
        Locate 14, 1: Print "11. FT %";
        Locate 14, 14: Input tmFTPct!
        Locate 14, 29: Input oppFTPct!
        Locate 15, 1: Print "12. RB";
        Locate 15, 14: Input tmReb!
        Locate 15, 29: Input oppReb!
        Locate 16, 1: Print "13. AST";
        Locate 16, 14: Input tmAsst!
        Locate 16, 29: Input oppAsst!
        Locate 17, 1: Print "14. FOULS";
        Locate 17, 14: Input tmFouls!
        Locate 17, 29: Input oppFouls!
        Locate 18, 1: Print "15. TO";
        Locate 18, 14: Input tmTO!
        Locate 18, 29: Input oppTO!
        Locate 19, 1: Print "16. STLS";
        Locate 19, 14: Input tmSteal!
        Locate 19, 29: Input oppSteal!
        Locate 20, 1: Print "17. BLOCKS";
        Locate 20, 14: Input tmBlocks!
        Locate 20, 29: Input oppBlocks!
        Locate 21, 1: Print "18. 3 FGA ";
        Locate 21, 14: Input tm3FGA!
        Locate 21, 29: Input opp3FGA!
        Locate 22, 1: Print "19. 3 FGA % ";
        Locate 22, 14: Input tm3FGAPct!
        Locate 22, 29: Input opp3FGAPct!

        Locate 23: Input "20. OPP 2FG%"; opp2FGPct!

        'FOULS
        'Average 20 / game if none entered
        If oppFouls! = 0 Then oppFouls! = 20 * nbrGmsPlayed!
        If tmFouls! = 0 Then tmFouls! = 20 * nbrGmsPlayed!

        Do

            Do
                Locate 23, 40
                Input "# TO CHANGE (0 TO CONTINUE)"; B9
            Loop Until B9 >= 0 And B9 <= 27

            Select Case B9

                Case 1
                    Locate 3, 1: Input " 1. DISK ID "; yearNumber$
                    Locate 3, 13: Print " "

                Case 2
                    Locate 4, 1: Input " 2. TEAM NAME "; teamName$
                    Locate 4, 15: Print " "

                Case 3
                    Locate 5, 1: Input " 3. TEAM NICKNAME "; mascot$
                    Locate 5, 19: Print " "

                Case 4
                    Locate 6, 30: Print Space$(10)
                    Locate 6, 1: Input " 4. STRENGTH OF SCHEDULE ADJ "; sosAdj
                    Locate 6, 30: Print " "

                Case 5
                    Locate 7, 28: Print Space$(10)
                    Locate 7, 1: Input " 5. HOMECOURT FACTOR (0-6) "; teamRatings%(6)
                    Locate 7, 28: Print " "

                Case 6
                    Locate 8, 23: Print Space$(10)
                    Locate 8, 1: Input " 6. TEXT COLOR (0-15) "; teamRatings%(0)
                    Locate 8, 23: Print " "

                    If teamRatings%(0) >= 0 And teamRatings%(1) >= 0 Then
                        'Locate 8, 40
                        'Print "TEAM APPEARANCE: ";
                        Locate 9, 40
                        Color teamRatings%(0), teamRatings%(1)
                        Print teamName$ + " " + mascot$
                        Color 15, 0
                    End If

                Case 7
                    Locate 9, 28: Print Space$(10)
                    Locate 9, 1: Input " 7. BACKGROUND COLOR (0-7) "; teamRatings%(1)
                    Locate 9, 28: Print " "

                    If teamRatings%(0) >= 0 And teamRatings%(1) >= 0 Then
                        'Locate 8, 37
                        'Print "TEAM APPEARANCE: ";
                        Locate 9, 40
                        Color teamRatings%(0), teamRatings%(1)
                        Print teamName$ + " " + mascot$
                        Color 15, 0
                    End If

                Case 8
                    Locate 11, 1: Print " 8. FGA";

                    Locate 11, 15: Print Space$(10)
                    Locate 11, 14: Input tmFGA!
                    Locate 11, 15: Print " "

                    Locate 11, 30: Print Space$(10)
                    Locate 11, 29: Input oppFGA!
                    Locate 11, 30: Print " "

                Case 9
                    Locate 12, 1: Print " 9. FG %";

                    Locate 12, 15: Print Space$(10)
                    Locate 12, 14: Input tmFGPct!
                    Locate 12, 15: Print " "

                    Locate 12, 30: Print Space$(10)
                    Locate 12, 29: Input oppFGPct!
                    Locate 12, 30: Print " "

                Case 10
                    Locate 13, 1: Print "10. FTA";

                    Locate 13, 15: Print Space$(10)
                    Locate 13, 14: Input tmFTA!
                    Locate 13, 15: Print " "

                    Locate 13, 30: Print Space$(10)
                    Locate 13, 29: Input oppFTA!
                    Locate 13, 30: Print " "

                Case 11
                    Locate 14, 1: Print "11. FT %";

                    Locate 14, 15: Print Space$(10)
                    Locate 14, 14: Input tmFTPct!
                    Locate 14, 15: Print " "

                    Locate 14, 30: Print Space$(10)
                    Locate 14, 29: Input oppFTPct!
                    Locate 14, 30: Print " "

                Case 12
                    Locate 15, 1: Print "12. RB";

                    Locate 15, 15: Print Space$(10)
                    Locate 15, 14: Input tmReb!
                    Locate 15, 15: Print " "

                    Locate 15, 30: Print Space$(10)
                    Locate 15, 29: Input oppReb!
                    Locate 15, 30: Print " "

                Case 13
                    Locate 16, 1: Print "13. AST";

                    Locate 16, 15: Print Space$(10)
                    Locate 16, 14: Input tmAsst!
                    Locate 16, 15: Print " "

                    Locate 16, 30: Print Space$(10)
                    Locate 16, 29: Input oppAsst!
                    Locate 16, 30: Print " "

                Case 14
                    Locate 17, 1: Print "14. FOULS";

                    Locate 17, 15: Print Space$(10)
                    Locate 17, 14: Input tmFouls!
                    Locate 17, 15: Print " "

                    Locate 17, 30: Print Space$(10)
                    Locate 17, 29: Input oppFouls!
                    Locate 17, 30: Print " "

                Case 15
                    Locate 18, 1: Print "15. TO";

                    Locate 18, 15: Print Space$(10)
                    Locate 18, 14: Input tmTO!
                    Locate 18, 15: Print " "

                    Locate 18, 30: Print Space$(10)
                    Locate 18, 29: Input oppTO!
                    Locate 18, 30: Print " "

                Case 16
                    Locate 19, 1: Print "16. STLS";

                    Locate 19, 15: Print Space$(10)
                    Locate 19, 14: Input tmSteal!
                    Locate 19, 15: Print " "

                    Locate 19, 30: Print Space$(10)
                    Locate 19, 29: Input oppSteal!
                    Locate 19, 30: Print " "

                Case 17
                    Locate 20, 1: Print "17. BLOCKS";

                    Locate 20, 15: Print Space$(10)
                    Locate 20, 14: Input tmBlocks!
                    Locate 20, 15: Print " "

                    Locate 20, 30: Print Space$(10)
                    Locate 20, 29: Input oppBlocks!
                    Locate 20, 30: Print " "

                Case 18
                    Locate 21, 1: Print "18. 3 FGA";
                    Locate 21, 14: Input tm3FGA!
                    Locate 21, 30: Print Space$(10)
                    Locate 21, 29: Input opp3FGA!
                    Locate 21, 30: Print " "

                Case 19
                    Locate 22, 1: Print "19. 3 FGA %";
                    Locate 22, 14: Input tm3FGAPct!
                    Locate 22, 29: Input opp3FGAPct!

                Case 20
                    'Try Locate 24, 40 ???
                    Locate 23, 9: Print Space$(10)
                    Locate 23, 1: Input "OPP 2FG% "; opp2FGPct!
                    Locate 23, 9: Print " "

                Case 21
                    Locate 2, 57: Print Space$(10)
                    Locate 2, 40: Input "21. GAMES PLAYED "; nbrGmsPlayed!
                    Locate 2, 57: Print " "

                Case 22
                    Locate 3, 49: Print Space$(10)
                    Locate 3, 40: Input "22. WINS "; teamRatings%(7)
                    Locate 3, 49: Print " "

                Case 23
                    Locate 4, 51: Print Space$(10)
                    Locate 4, 40: Input "23. LOSSES "; teamRatings%(8)
                    Locate 4, 51: Print " "

                Case 24
                    Locate 5, 57: Print Space$(10)
                    Locate 5, 40: Input "24. POWER RATING "; teamRatings%(9)
                    Locate 5, 57: Print " "

                Case 25
                    Locate 6, 40: Input "25. COACH "; coach$
                    Locate 6, 50: Print " "

                Case 26
                    Locate 7, 40: Input "26. ARENA "; arenaName$
                    Locate 7, 50: Print " "
                    
                Case 27
                    Locate 8, 60: Print Space$(10)
                    Locate 8, 40: Input "27. AVG ATTENDANCE "; teamAttendance&
                    Locate 8, 60: Print " "

            End Select

            Locate 23, 40
            Print Space$(10)

        Loop Until B9 = 0


        '---------------
        ' Players
        '---------------
        'Average FGA/G for this team (Off + Def)
        teamRatings%(3) = Int(((tmFGA! + oppFGA!) / (nbrGmsPlayed! * 2)) + .5)
        teamRatings%(4) = leagFGAPerGame!
        teamRatings%(5) = nbrGmsPlayed!

        If choice = 1 Then
            F3 = 32
        Else
            F3 = 40
        End If

        For I = 0 To 13

            Cls
            Color 14
            Print inputX$(choice); Tab(35); "PLAYER - "; I + 1
            Print "ENTER PLAYER STATS"
            Color 15

            Locate 3
            Input " 1. NAME (XXX TO QUIT) "; playerNames$(I)

            If playerNames$(I) <> "XXX" Then

                Locate 4, 1: Input " 2. POSITION"; position$(I)
                Locate 5, 1: Input " 3. GAMES"; playerOff!(I, 7)
                playerRatings!(I, 0) = playerOff!(I, 7)

                Locate 6, 1: Input " 4. MP"; playerOff!(I, 8)
                Locate 7, 1: Input " 5. FGM"; playerStats!(I, 3)

                Locate 8, 1: Input " 6. FGA"; playerOff!(I, 9)
                playerRatings!(I, 1) = playerOff!(I, 9)

                Locate 9, 1: Input " 7. FTM"; playerStats!(I, 5)
                Locate 10, 1: Input " 8. FTA"; playerOff!(I, 16)
                Locate 11, 1: Input " 9. ORB"; playerOff!(I, 19)
                Locate 12, 1: Input "10. TOT RB"; playerOff!(I, 10)
                Locate 13, 1: Input "11. PF"; playerOff!(I, 12)
                Locate 14, 1: Input "12. AST"; playerOff!(I, 11)
                Locate 15, 1: Input "13. BLK"; playerOff!(I, 14)
                Locate 16, 1: Input "14. STL"; playerOff!(I, 13)
                Locate 17, 1: Input "15. 3 FGM"; playerStats!(I, 13)
                Locate 18, 1: Input "16. 3 FGA"; playerOff!(I, 17)
                'Locate 19, 1: Input "17. DEFENSE (1-9)"; playerDef!(I)

                Do

                    Do
                        Locate 22, 1
                        Input "# TO CHANGE (0 TO CONTINUE) "; B9
                    Loop Until B9 >= 0 And B9 <= 17

                    If B9 <> 0 Then

                        Select Case B9

                            Case 1
                                Locate 3, 1: Input " 1. NAME (XXX TO QUIT) "; playerNames$(I)
                                Locate 3, 24: Print " "

                            Case 2
                                Locate 4, 1: Input " 2. POSITION "; position$(I)
                                Locate 4, 14: Print " "

                            Case 3
                                Locate 5, 12: Print Space$(10)
                                Locate 5, 1: Input " 3. GAMES "; playerOff!(I, 7)
                                Locate 5, 12: Print " "

                                playerRatings!(I, 0) = playerOff!(I, 7)

                            Case 4
                                Locate 6, 9: Print Space$(10)
                                Locate 6, 1: Input " 4. MP "; playerOff!(I, 8)
                                Locate 6, 9: Print " "

                            Case 5
                                Locate 7, 10: Print Space$(10)
                                Locate 7, 1: Input " 5. FGM "; playerStats!(I, 3)
                                Locate 7, 10: Print " "

                            Case 6
                                Locate 8, 10: Print Space$(10)
                                Locate 8, 1: Input " 6. FGA "; playerOff!(I, 9)
                                playerRatings!(I, 1) = playerOff!(I, 9)
                                Locate 8, 10: Print " "

                            Case 7
                                Locate 9, 10: Print Space$(10)
                                Locate 9, 1: Input " 7. FTM "; playerStats!(I, 5)
                                Locate 9, 10: Print " "

                            Case 8
                                Locate 10, 10: Print Space$(10)
                                Locate 10, 1: Input " 8. FTA "; playerOff!(I, 16)
                                Locate 10, 10: Print " "

                            Case 9
                                Locate 11, 10: Print Space$(10)
                                Locate 11, 1: Input " 9. ORB "; playerOff!(I, 19)
                                Locate 11, 10: Print " "

                            Case 10
                                Locate 12, 13: Print Space$(10)
                                Locate 12, 1: Input "10. TOT RB "; playerOff!(I, 10)
                                Locate 12, 13: Print " "

                            Case 11
                                Locate 13, 9: Print Space$(10)
                                Locate 13, 1: Input "11. PF "; playerOff!(I, 12)
                                Locate 13, 9: Print " "

                            Case 12
                                Locate 14, 10: Print Space$(10)
                                Locate 14, 1: Input "12. AST "; playerOff!(I, 11)
                                Locate 14, 10: Print " "

                            Case 13
                                Locate 15, 10: Print Space$(10)
                                Locate 15, 1: Input "13. BLK "; playerOff!(I, 14)
                                Locate 15, 10: Print " "

                            Case 14
                                Locate 16, 10: Print Space$(10)
                                Locate 16, 1: Input "14. STL "; playerOff!(I, 13)
                                Locate 16, 10: Print " "

                            Case 15
                                Locate 17, 12: Print Space$(10)
                                Locate 17, 1: Input "15. 3 FGM "; playerStats!(I, 13)
                                Locate 17, 12: Print " "

                            Case 16
                                Locate 18, 12: Print Space$(10)
                                Locate 18, 1: Input "16. 3 FGA "; playerOff!(I, 17)
                                Locate 18, 12: Print " "

                            Case 17
                                'Locate 19, 20: Print Space$(10)
                                'Locate 19, 1: Input "17. DEFENSE (1-9) "; playerDef!(I)
                                'Locate 19, 20: Print " "

                        End Select

                        Locate 22, 29
                        Print Space$(10)

                    End If

                    playerStats!(I, 0) = playerOff!(I, 7)
                    playerStats!(I, 1) = playerOff!(I, 8)
                    playerStats!(I, 2) = playerOff!(I, 9)

                    If playerStats!(I, 2) = 0 Then
                        playerOff!(I, 0) = 0
                    Else
                        playerOff!(I, 0) = Int(playerStats!(I, 3) / playerStats!(I, 2) * 100 + .5)
                    End If

                    playerStats!(I, 4) = playerOff!(I, 16)

                    If playerStats!(I, 4) = 0 Then
                        playerOff!(I, 1) = 0
                    Else
                        playerOff!(I, 1) = Int(playerStats!(I, 5) / playerStats!(I, 4) * 100 + .5)
                    End If

                    playerStats!(I, 6) = playerOff!(I, 19)
                    playerStats!(I, 7) = playerOff!(I, 10)
                    playerStats!(I, 8) = playerOff!(I, 12)
                    playerStats!(I, 9) = playerOff!(I, 11)
                    
                    'Blocks
                    playerStats!(I, 10) = playerOff!(I, 14)

                    playerStats!(I, 11) = playerOff!(I, 13)
                    playerStats!(I, 12) = playerOff!(I, 17)

                    If playerStats!(I, 12) = 0 Then
                        playerOff!(I, 18) = 0
                        playerRatings!(I, 2) = 0
                    Else
                        playerOff!(I, 18) = Int(playerStats!(I, 13) / playerStats!(I, 12) * 100 + .5)
                        
                        
                        Locate 17, 1: Input "15. 3 FGM"; playerStats!(I, 13)
                        Locate 18, 1: Input "16. 3 FGA"; playerOff!(I, 17)

                        playerRatings!(I, 2) = playerOff!(I, 18)
                    End If

                Loop Until B9 = 0

            Else

                For I1 = I To 13:
                    playerNames$(I1) = "XXX"
                Next

                Exit For

            End If 'Done checking for XXX

        Next I

        X1 = Val(Mid$(teamName$, 1, 2))

        'Adjusts team and player FTA
        'based on FT rules changes
        '-- original code was between 1 and 72
        '-- but this would impact years
        '-- between 2000 and 2052
        If (X1 >= 39 And X1 <= 72) Then

            If X1 >= 53 And X1 <= 57 Then
                '1953 - 1957
                '70% of entered value
                X2! = .7
            Else
                '1939 - 1952, 1958 - 1972
                '83% of entered value
                X2! = .83
            End If

            tmFTA! = Int((tmFTA! * X2!) + .5)
            oppFTA! = Int((oppFTA! * X2!) + .5)

            For I = 0 To 13:
                playerOff!(I, 16) = Int((playerOff!(I, 16) * X2) + .5)
            Next I

        End If

        X9! = 0

        X1! = oppFGA! - ((oppFGPct! * oppFGA!) / 100) + oppFTA! - ((oppFTA! * oppFTPct!) / 100)
        X2! = tmFGA! - ((tmFGA! * tmFGPct!) / 100) + tmFTA! - ((tmFTA! * tmFTPct!) / 100)
        RD! = (tmReb! - X1! + X2! - oppReb!) / (nbrGmsPlayed! * 2)
        HS! = tmReb! - X1! + X2! - oppReb!
        M7! = ((35 + RD!) / leagRebPerGame!)

        If choice = 2 Then 
            M7! = ((44 + RD!) / leagRebPerGame!)
        End If

        For I = 0 To 13

            If playerNames$(I) <> "XXX" Then
                'Sum of all player Blocks + Assists
                X9! = X9! + playerOff!(I, 10) + playerOff!(I, 9)
            End If

        Next I

        For I = 0 To 13

            If playerNames$(I) <> "XXX" Then

                'M is a factor of how often a player
                'contributes in a single game
                If playerOff!(I, 8) <> 0 Then
                    If choice = 0 Then 
                        'College
                        M! = 40 / (playerOff!(I, 8) / playerOff!(I, 7))
                    End If
                    If choice = 1 Then 
                        'High School
                        M! = 32 / (playerOff!(I, 8) / playerOff!(I, 7))
                    End If
                    plyrMinAdj! = M!
                Else
                    M! = (X9! / 5) / (playerOff!(I, 9) + playerOff!(I, 10))
                    MI! = (playerOff!(I, 9) + playerOff!(I, 10)) / (X9! / 5)
                    If M! < 1.1 Then plyrMinAdj! = 1.1
                End If

                'Tot Reb / 40 Min
                playerOff!(I, 2) = Int((((playerOff!(I, 10) / playerOff!(I, 7)) * plyrMinAdj!) * M7!) + .5)

                '(Tot Reb / 40 Min) * 10
                X = Int(((((playerOff!(I, 10) / playerOff!(I, 7)) * plyrMinAdj!) * M7!) * 10) + .5)

                '(Off Reb / 40 Min) * 10
                If playerOff!(I, 19) = 0 Then
                    playerRatings!(I, 4) = Int((X * .35) + .5)
                Else
                    playerRatings!(I, 4) = Int((X * (playerOff!(I, 19) / playerOff!(I, 10))) + .5)
                End If

                '(Def Reb / 40 Min) x 10
                playerOff!(I, 2) = X - playerRatings!(I, 4)

                'Assist Rating (based on Assits / Game)
                If inputQQ(0) = 1 Then
                    'Adjust entered assists / game based on time contribution and team
                    playerOff!(I, 4) = Int(((playerOff!(I, 11) / playerOff!(I, 7)) * plyrMinAdj!) * (L6 / (tmAsst! / nbrGmsPlayed!)) + .5)
                End If

                'Steal Rating (based on Steals / Game)
                If inputQQ(2) = 1 Then
                    'Adjust entered steals / game based on time contribution
                    playerSteals = Int(((playerOff!(I, 13) / playerOff!(I, 7)) * plyrMinAdj!) + .5)
                    playerOff!(I, 5) = playerSteals
                    playerDef!(I) = playerSteals
                End If

                'Block Rating (based on Blocks / Game)
                If inputQQ(3) = 1 Then
                    playerOff!(I, 6) = Int(((playerOff!(I, 14) / playerOff!(I, 7)) * plyrMinAdj!) + .5)
                End If

            End If

        Next I

        X! = leagFGAPerGame! + leagRebPerGame! + 20
        inputY! = (tmFGA! + oppFGA! + tmReb! + oppReb! + tmFouls! + oppFouls!) / (2 * nbrGmsPlayed!)
        staminaRating! = Int(((2 * inputY!) - X!) + .5)

        'Strength of Schedule
        X1! = (tmFGA! + oppFGA!) / (nbrGmsPlayed! * 2)
        X3 = (sosAdj / 2) / X1!
        X3 = Int((X3 * 100) + .5)
        sosAdj = Int(X3 / 2)
        BY = Int(X3 / 2)
        X4 = Int(Rnd(1) * 2)

        If X3 - sosAdj - BY > 0 And X4 = 0 Then
            sosAdj = sosAdj + 1
        End If

        If X3 - sosAdj - BY > 0 And X4 = 1 Then
            BY = BY + 1
        End If

        U7 = Int((46 - leagFGPct) / 2)
        defFGPctAdj! = opp2FGPct! - leagFGPct! + BY - U7

        V% = Int(((opp3FGA! / oppFGA!) + .005) * 100)
        def3FGA_Adj = V% - leag3FGAPct!

        'DEF 3FGA/3FGA ADJ
        '-- Here is where 100 is being added to the value
        '-- This is why our team files and viewer later
        '-- will show values as 95-99
        If def3FGA_Adj < 0 Then def3FGA_Adj = 100 + def3FGA_Adj

        def3FG_Adj = opp3FGAPct! - leag3FGPct!

        'DEF 3FG %
        '-- Here is where 100 is being added to the value
        '-- This is why our team files and viewer later
        '-- will show values as 95-99
        If def3FG_Adj < 0 Then def3FG_Adj = 100 + def3FG_Adj


        For I = 0 To 13

            If playerNames$(I) <> "XXX" Then

                'Contribution Pct
                playerOff!(I, 3) = (playerOff!(I, 9) + playerOff!(I, 10)) / X9!
                playerOff!(I, 3) = Int((100 * playerOff!(I, 3)) + .5)

                '3FGA / TOT FGA
                If playerOff!(I, 9) <> 0 Then 
                    playerRatings!(I, 3) = Int(playerOff!(I, 17) / playerOff!(I, 9) * 100 + .5)
                End If

                If M! < 1 Then plyrMinAdj! = 1

                'Fouls Rating (adjusted for player time contrib)
                B3 = (playerOff!(I, 12) / playerOff!(I, 7)) * plyrMinAdj!
                B3 = Int((B3 * 10) + .5)
                playerRatings!(I, 5) = B3

                'FGA * FG%
                B3 = playerOff!(I, 9) * (playerOff!(I, 0) / 100)
                
                '3FGA * 3FG%
                B4 = playerOff!(I, 17) * (playerOff!(I, 18) / 100)

                'Adjusted FG%
                If playerOff!(I, 9) - playerOff!(I, 17) <> 0 Then 
                    playerOff!(I, 0) = ((B3 - B4) / (playerOff!(I, 9) - playerOff!(I, 17))) + .005
                End If

                playerOff!(I, 0) = Int((playerOff!(I, 0) * 100) + .5)

            End If

        Next I

        For I = 0 To 13

            If playerNames$(I) <> "XXX" Then
                'Adjust player FG% by SOS and U7 (how far league FG% is from 46)
                playerOff!(I, 0) = playerOff!(I, 0) - sosAdj + U7
            End If

        Next I

        leagueRatings(4) = 99

        For I = 0 To 13

            If playerNames$(I) <> "XXX" Then

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
                X! = Int(((X! / (playerOff!(I, 9) + X!)) * 100) + .5)

                If X! < 0 Then X! = 0
                
                'Foul Draw rating
                playerRatings!(I, 6) = X!

            End If

        Next I

        'Off Steal Rating
        inputY! = oppSteal! / nbrGmsPlayed!
        X! = (inputY! - 7) / .4
        X! = Int(X! + .5)

        If X! < 0 Then leagueRatings(0) = 100 + X!
        If X! > 0 Then leagueRatings(0) = X!

        'Off TO Rating
        R! = 1200 - (((tmFGA! + oppFGA!) / (nbrGmsPlayed! * 2)) * 4)
        R! = R! / 6
        inputY! = (tmTO! - oppSteal!) / nbrGmsPlayed!
        X! = (inputY! / R!) - .01
        X! = Int((X! * 100) + .5)
        leagueRatings(1) = X!

        'Def TO Adj
        inputY! = (oppTO! - tmSteal!) / nbrGmsPlayed!
        X! = (inputY! / R!) - .01
        X! = Int((X! * 100) + .5)
        X! = X! - 4

        If X! < 0 Then leagueRatings(2) = 100 + X!
        If X! > 0 Then leagueRatings(2) = X!

        'Def Foul Adj
        homeScoreTeam! = Int(((oppFGA! * oppFGPct!) / 100) + .5)
        X! = (homeScoreTeam! * .1) + (nbrGmsPlayed! * 8.2)
        X! = (oppFTA! - X!) / 2
        X! = (X! / (oppFGA! + X!)) - .09
        X! = Int((X! * 100) + .5)

        If X! > 0 Then leagueRatings(3) = X!
        If X! < 0 Then leagueRatings(3) = X! + 100

        '---------------
        ' Assists
        '---------------
        If inputQQ(0) = 0 Then

            Cls
            Color 14
            Print "INPUT ASSIST RATINGS"
            Color 15
            Print "AA SHOULD = APPROX. 3 WHEN FINISHED"
            Print "FORWARDS,CENTERS ARE USUALLY 0,1,2,3 ; GUARDS ARE USUALLY 3,4,5,6"

            Print

            For I = 0 To 13
                Locate I + 6, 1
                Print I; " "; playerNames$(I);
                Locate I + 6, 20
                Input inputRatings(I)
            Next I

            AA = 0

            For I1 = 0 To 13
                AA = AA + inputRatings(I1) * Int(playerOff!(I1, 3))
            Next I1

            Locate 20, 1
            Print "AA = "; AA / 100

            Do

                Do
                    Locate 21, 1
                    Print "ANY CHANGE (0-13) (N=NO CHANGE) "
                    Input I$
                    B9 = Val(I$)
                Loop Until B9 >= 0 And B9 <= 13 Or UCase$(I$) = "N"

                If UCase$(I$) <> "N" Then
                    rating = B9
                    Locate rating + 6, 20
                    Input inputRatings(rating)
                End If

            Loop Until UCase$(I$) = "N"

            For I = 0 To 13
                playerOff!(I, 4) = playerOff!(I, 4) + inputRatings(I)
            Next I

        End If

        '---------------
        ' Foul Commit
        '---------------
        If inputQQ(1) = 0 Then

            Cls
            Color 14
            Print "INPUT FOUL COMMITING RATINGS"
            Color 15
            Print "PF SHOULD = APPROX. 40 WHEN FINISHED"

            Print "AVG. RATING = 40 ; RANGE = 15 TO 99"
            Print

            For I = 0 To 13
                Locate I + 6, 1
                Print I; " "; playerNames$(I);
                Locate I + 6, 20
                Input inputRatings(I)
            Next I

            PF = 0
            TP = 0

            For I1 = 0 To 13
                If playerNames$(I1) <> "XXX" Then
                    TP = TP + 1
                    PF = PF + inputRatings(I1)
                End If
            Next I1

            Locate 20, 1
            Print "PF = "; PF / TP

            Do

                Do
                    Locate 21, 1
                    Print "ANY CHANGE (0-13) (N=NO CHANGE) "
                    Input I$
                    B9 = Val(I$)
                Loop Until B9 >= 0 And B9 <= 13 Or UCase$(I$) = "N"

                If UCase$(I$) <> "N" Then
                    rating = B9
                    Locate rating + 6, 20
                    Input inputRatings(rating)
                End If

            Loop Until UCase$(I$) = "N"

            For I = 0 To 13
                playerRatings!(I, 5) = inputRatings(I)
            Next I

        End If

        '---------------
        ' Steals & TOs
        '---------------
        If inputQQ(2) = 0 Then

            HQ = tmFGA! - oppFGA!
            HR = tmFTA! - oppFTA!

            HW = HR / 350
            If HW > 1 Then HW = 1

            HW = ((HW * 35) + 32) / 100
            If HW < 0 Then HW = HW * -1

            HR = HR * HW
            HS = HS / 2
            HX = Int((HR + HS - HQ) + .5)
            HX = Int(((HX / nbrGmsPlayed!) * 10) + .5)

            Cls
            leagueRatings(4) = 99

            Color 14
            Print "INPUT STEALS & TURNOVERS RATINGS."
            Color 15
            Print "THIS TEAMS ST SHOULD = APPROX. "; HX / 10
            Print "IND. STL RATINGS: AVG=1 RANGE = 0 TO 4"
            Print "TM OFF STL ADJ: AVG=0 RANGE=-10 TO 10"
            Print "TM TURNOVR RATING: AVG=4 RANGE = 1 TO 7"
            Print "TM DEF TRNOVR ADJ: AVG=0 RANGE -4 TO 4"

            For I = 0 To 13
                Locate I + 7, 1
                Print I; " "; playerNames$(I);
                Locate I + 7, 20
                Input inputRatings(I)
            Next I

            Locate 21, 1: Input "X-TM OFF STL ADJ   "; inputRatings(14)
            Locate 22, 1: Input "Y-TM TURNOVER ADJ  "; inputRatings(15)
            Locate 23, 1: Input "Z-TM DEF TURNOVR ADJ"; inputRatings(16)

            ST = 0

            For I = 0 To 13:
                ST = ST + inputRatings(I) * Int(playerOff!(I, 3))
            Next I

            ST = ST / 20
            SY = 7 + inputRatings(14) * .4
            SV = (R * inputRatings(15)) / 100
            SW = ((4 + inputRatings(16)) * R) / 100
            ST = ST - SY - SV + SW

            Locate 10, 26
            Print "ST = "; ST;

            Do

                Do
                    Locate 23, 1
                    Print "SELECT # (0-Z) TO CHANGE (N = NO CHANGE) ";
                    Input I$
                    rating = Val(I$)
                    If UCase$(I$) = "X" Then rating = 14
                    If UCase$(I$) = "Y" Then rating = 15
                    If UCase$(I$) = "Z" Then rating = 16
                Loop Until rating >= 0 And rating <= 16 Or UCase$(I$) = "N"

                If UCase$(I$) <> "N" Then

                    If rating <= 13 Then

                        Locate rating + 7, 20
                        Input inputRatings(rating)

                    Else

                        Select Case UCase$(I$)

                            Case "X":
                                Locate 20, 1: Input "X-TM OFF STL ADJ   "; inputRatings(14)

                            Case "Y":
                                Locate 21, 1: Input "Y-TM TURNOVER ADJ  "; inputRatings(15)

                            Case "Z":
                                Locate 22, 1: Input "Z-TM DEF TURNOVR ADJ"; inputRatings(16)

                        End Select

                    End If

                End If

            Loop Until UCase$(I$) = "N"

            For I = 0 To 13
                playerOff!(I, 5) = playerOff!(I, 5) + inputRatings(I)
            Next I

            If inputRatings(14) < 0 Then inputRatings(14) = 100 + inputRatings(14)
            If inputRatings(16) < 0 Then inputRatings(16) = 100 + inputRatings(16)

            For I = 0 To 2
                leagueRatings(I) = inputRatings(I + 14)
            Next

            I$ = GetKeyPress$

        End If

        '---------------
        ' Block Ratings
        '---------------
        If inputQQ(3) = 0 Then

            Cls
            Color 14
            Print "INPUT BLOCK RATINGS"
            Color 15
            Print "GUARDS ARE USUALLY 0,1 ; FORWARDS,CENTERS ARE USUALLY 0,1,2,3"
            Print "THE GREAT SHOT BLOCKERS ARE USUALLY 5,6,7"
            Print

            For I = 0 To 13
                Locate I + 6, 1
                Print I; " "; playerNames$(I);
                Locate I + 6, 20
                Input inputRatings(I)
            Next I

            Do
                Do
                    Locate 20, 1
                    Print "ANY CHANGE (0-13) (N=NO CHANGE) "
                    Input I$
                    B9 = Val(I$)
                Loop Until B9 >= 0 And B9 <= 13 Or UCase$(I$) = "N"

                If UCase$(I$) <> "N" Then
                    rating = B9
                    Locate rating + 6, 20
                    Input inputRatings(rating)
                End If

            Loop Until UCase$(I$) = "N"

        End If

        For I = 0 To 13:
            playerOff!(I, 6) = playerOff!(I, 6) + inputRatings(I)
        Next I

        Call SaveTeam(1, teamName$, yearNumber$, teamNumber)

    End If 'We ESC'd out of the first menu

End Sub


'----------------------------------------
'          SaveTeam Subroutine
'----------------------------------------
'This subroutine saves the modifed team
'data (from corresponding arrays) to an
'appropriate spot in the select team file
Sub SaveTeam (newID, teamName$, yearNumber$, teamIdx)

    Shared nbrGmsPlayed!, opp3FGA!, opp3FGAPct!, oppAsst!, oppBlocks!
    Shared oppFGPct!, oppFouls!, oppFGA!, oppFTA!, oppFTPct!
    Shared oppReb!, oppSteal!, oppTO!
    Shared tm3FGA!, tm3FGAPct!, tmAsst!, tmBlocks!, tmFouls!
    Shared tmFGA!, tmFGPct!, tmFTA!, tmFTPct!
    Shared tmReb!, tmSteal!, tmTO!

    Shared def3FG_Adj, def3FGA_Adj
    Shared teamAttendance&, defFGPctAdj!, staminaRating!
    Shared mascot$, coach$, arenaName$

    Shared teamRatings%()

    Shared playerOff!(), playerDef!(), leagueRatings()
    Shared playerStats!(), playerRatings!()

    Shared playerNames$(), position$()

    Call CSMsgBox("SAVING TEAM FILES", 20, PASS)

    teamYear$ = RTrim$(yearNumber$)

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$ For Random As #1 Len = TEAM_SIZE_BYTES

    fileLength& = LOF(1)

    If newID = 1 Or teamIdx = 0 Then 
        teamIdx = (fileLength& / TEAM_SIZE_BYTES) + 1
    End If

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
        LSet Q$(I + 5) = MKI$(leagueRatings(I))
    Next

    teamRatings%(2) = 999

    For I = 0 To 9:
        LSet Q$(I + 10) = MKI$(teamRatings%(I))
    Next

    For I = 0 To 13

        LSet Q$(I * 15 + 20) = playerNames$(I)

        For I1 = 0 To 6
            LSet Q$(I * 15 + I1 + 21) = MKI$(playerOff!(I, I1))
            LSet Q$(I * 15 + I1 + 28) = MKI$(playerRatings!(I, I1))
        Next

    Next

    Put #1, teamIdx

    Close #1

    Open diskPaths$(0) + DATA_FILE_NAME$ + "." + yearNumber$ For Random As #2 Len = DATA_SIZE_BYTES

    Field #2, 15 As Q$(0), 40 As Q$(1), 15 As Q$(2), 4 As Q$(3)

    For I = 0 To 13
        Field #2, 74 + I * 32 As X$, 2 As Q$(I * 16 + 4), 2 As Q$(I * 16 + 5), 2 As Q$(I * 16 + 6), 2 As Q$(I * 16 + 7), 2 As Q$(I * 16 + 8), 2 As Q$(I * 16 + 9), 2 As Q$(I * 16 + 10), 2 As Q$(I * 16 + 11), 2 As Q$(I * 16 + 12), 2 As Q$(I * 16 + 13), 2 As Q$(I * 16 + 14), 2 As Q$(I * 16 + 15), 2 As Q$(I * 16 + 16), 2 As Q$(I * 16 + 17), 2 As Q$(I * 16 + 18), 2 As Q$(I * 16 + 19)
    Next

    Field #2, 522 As X$, 2 As Q$(228), 2 As Q$(229), 2 As Q$(230), 2 As Q$(231), 2 As Q$(232), 2 As Q$(233), 2 As Q$(234), 2 As Q$(235), 2 As Q$(236), 2 As Q$(237), 2 As Q$(238), 2 As Q$(239), 2 As Q$(240), 2 As Q$(241), 2 As Q$(242), 2 As Q$(243), 2 As Q$(244), 2 As Q$(245), 2 As Q$(246), 2 As Q$(247), 2 As Q$(248), 2 As Q$(249), 2 As Q$(250), 2 As Q$(251), 2 As Q$(252), 2 As Q$(253)

    LSet Q$(0) = coach$
    LSet Q$(1) = arenaName$
    LSet Q$(2) = mascot$
    LSet Q$(3) = MKL$(teamAttendance&)

    For I = 0 To 13

        LSet Q$(I * 16 + 4) = MKI$(playerDef!(I))
        LSet Q$(I * 16 + 5) = position$(I)

        For I1 = 0 To 13
            LSet Q$(I * 16 + I1 + 6) = MKI$(playerStats!(I, I1))
        Next

    Next

    LSet Q$(228) = MKI$(nbrGmsPlayed!)
    LSet Q$(229) = MKI$(tmFGA!)
    LSet Q$(230) = MKI$(oppFGA!)
    LSet Q$(231) = MKI$(tmFGPct!)
    LSet Q$(232) = MKI$(oppFGPct!)
    LSet Q$(233) = MKI$(tmFTA!)
    LSet Q$(234) = MKI$(oppFTA!)
    LSet Q$(235) = MKI$(tmFTPct!)
    LSet Q$(236) = MKI$(oppFTPct!)
    LSet Q$(237) = MKI$(tmReb!)
    LSet Q$(238) = MKI$(oppReb!)
    LSet Q$(239) = MKI$(tmAsst!)
    LSet Q$(240) = MKI$(oppAsst!)
    LSet Q$(241) = MKI$(tmFouls!)
    LSet Q$(242) = MKI$(oppFouls!)
    LSet Q$(243) = MKI$(tmTO!)
    LSet Q$(244) = MKI$(oppTO!)
    LSet Q$(245) = MKI$(tmSteal!)
    LSet Q$(246) = MKI$(oppSteal!)
    LSet Q$(247) = MKI$(opp3FGA!)
    LSet Q$(248) = MKI$(opp3FGAPct!)
    LSet Q$(249) = MKI$(tm3FGA!)
    LSet Q$(250) = MKI$(tm3FGAPct!)
    LSet Q$(251) = MKI$(tmBlocks!)
    LSet Q$(252) = MKI$(oppBlocks!)

    LSet Q$(253) = MKI$(999)

    Put #2, teamIdx

    Close #2

    Call CSMsgBox("", 0, PASS)

    tmSteal! = 0

    Erase playerDef!, playerOff!, playerRatings!, playerStats!
    Erase position$

    mascot$ = ""
    arenaName$ = ""
    coach$ = ""
    teamAttendance& = 0

End Sub
