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
                                    userFile$ = _SaveFileDialog$("Save Report File",_CWD$ + "\Report","*.txt","Text File")
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

    Color 15, 0
    Cls

    Color 14
    Print Tab(29); "CHANGE TEAM INFORMATION"
    Color 15

    Print " 1. TEAM NUMBER  "; teamIdx
    Print " 2. DISK ID  "; yearNumber$
    Print " 3. TEAM NAME   "; teamName$
    Print " 4. TEAM STAMINA RATING "; staminaRating!
    Print " 5. DEF FG% ADJ "; defFGPctAdj!
    Print " 6. DEF 3FGA/FGA ADJ "; defthreeFGA_Adj
    Print " 7. DEF 3FG% ADJ "; def3FG_Adj
    Print " 8. OFF STEAL RATING "; leagueRatings(0)
    Print " 9. OFF TURNOVER RATING "; leagueRatings(1)
    Print "10. DEF TURNOVER ADJ "; leagueRatings(2)
    Print "11. DEF FOUL ADJ "; leagueRatings(3)
    Print "12. ENTER 99 IF USING 6-9 "; leagueRatings(4)
    Print "13. TEXT COLOR (0-15) "; teamRatings(0)
    Print "14. BACKGROUND COLOR (0-7) "; teamRatings(1)
    Print "15. TEAM FGA/G "; teamRatings(3)
    Print "16. LEAGUE FGA/G "; teamRatings(4)
    Print "17. TEAM GAMES PLAYED "; teamRatings(5)
    Print "18. HOMECOURT FACTOR "; teamRatings(6)
    Print "19. ARENA NAME   "; arenaName$
    Print "20. NICKNAME   "; mascot$
    Print "21. COACH   "; coach$
    Print "22. AVG ATTENDANCE "; teamAttendance&
    Print "23. WINS "; teamRatings(7)
    Print "24. LOSSES "; teamRatings(8)
    Print "25. POWER RATING "; teamRatings(9)

    If teamRatings(0) >= 0 And teamRatings(1) >= 0 Then
        Locate 21, 1
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
                Locate 2, 17: Print Space$(10)
                Locate 2, 1: Input " 1. TEAM NUMBER "; teamIdx
                Locate 2, 17: Print " "

            Case 2
                Locate 3, 1: Input " 2. DISK ID "; yearNumber$
                Locate 3, 13: Print " "

            Case 3
                Locate 4, 1: Input " 3. TEAM NAME "; teamName$
                Locate 4, 15: Print " "

            Case 4
                Locate 5, 24: Print Space$(10)
                Locate 5, 1: Input " 4. TEAM STAMINA RATING"; staminaRating!
                Locate 5, 24: Print " "

            Case 5
                Locate 6, 16: Print Space$(10)
                Locate 6, 1: Input " 5. DEF FG% ADJ"; defFGPctAdj!
                Locate 6, 16: Print " "

            Case 6
                Locate 7, 21: Print Space$(10)
                Locate 7, 1: Input " 6. DEF 3FGA/FGA ADJ"; defthreeFGA_Adj
                Locate 7, 21: Print " "

            Case 7
                Locate 8, 17: Print Space$(10)
                Locate 8, 1: Input " 7. DEF 3FG% ADJ"; def3FG_Adj
                Locate 8, 17: Print " "

            Case 8
                Locate 9, 21: Print Space$(10)
                Locate 9, 1: Input " 8. OFF STEAL RATING"; leagueRatings(0)
                Locate 9, 21: Print " "

            Case 9
                Locate 10, 24: Print Space$(10)
                Locate 10, 1: Input " 9. OFF TURNOVER RATING"; leagueRatings(1)
                Locate 10, 24: Print " "

            Case 10
                Locate 11, 21: Print Space$(10)
                Locate 11, 1: Input "10. DEF TURNOVER ADJ"; leagueRatings(2)
                Locate 11, 21: Print " "

            Case 11
                Locate 12, 17: Print Space$(10)
                Locate 12, 1: Input "11. DEF FOUL ADJ"; leagueRatings(3)
                Locate 12, 17: Print " "

            Case 12
                Locate 13, 26: Print Space$(10)
                Locate 13, 1: Input "12. ENTER 99 IF USING 6-9"; leagueRatings(4)
                Locate 13, 26: Print " "

            Case 13
                Locate 14, 22: Print Space$(10)
                Locate 14, 1: Input "13. TEXT COLOR (0-15)"; teamRatings(0)
                Locate 14, 22: Print " "

                If teamRatings(0) >= 0 And teamRatings(1) >= 0 Then
                    Locate 21, 1
                    Print "TEAM APPEARANCE: ";
                    Color teamRatings(0), teamRatings(1)
                    Print RTrim$(teamName$) + " " + RTrim$(mascot$)
                    Color 15, 0
                End If

            Case 14
                Locate 15, 27: Print Space$(10)
                Locate 15, 1: Input "14. BACKGROUND COLOR (0-7)"; teamRatings(1)
                Locate 15, 27: Print " "

                If teamRatings(0) >= 0 And teamRatings(1) >= 0 Then
                    Locate 21, 1
                    Print "TEAM APPEARANCE: ";
                    Color teamRatings(0), teamRatings(1)
                    Print RTrim$(teamName$) + " " + RTrim$(mascot$)
                    Color 15, 0
                End If

            Case 15
                Locate 16, 16: Print Space$(10)
                Locate 16, 1: Input "15. TEAM FGA/G "; teamRatings(3)
                Locate 16, 16: Print " "

            Case 16
                Locate 17, 18: Print Space$(10)
                Locate 17, 1: Input "16. LEAGUE FGA/G "; teamRatings(4)
                Locate 17, 18: Print " "

            Case 17
                Locate 18, 23: Print Space$(10)
                Locate 18, 1: Input "17. TEAM GAMES PLAYED "; teamRatings(5)
                Locate 18, 23: Print " "

            Case 18
                Locate 19, 22: Print Space$(10)
                Locate 19, 1: Input "18. HOMECOURT FACTOR "; teamRatings(6)
                Locate 19, 22: Print " "

                If teamRatings(6) < 0 Then teamRatings(6) = 0
                If teamRatings(6) > 6 Then teamRatings(6) = 6

            Case 19
                Locate 20, 1: Input "19. ARENA NAME "; arenaName$
                Locate 20, 16: Print " "

            Case 20
                Locate 21, 1: Input "20. NICKNAME "; mascot$
                Locate 21, 15: Print " "

            Case 21
                Locate 22, 1: Input "21. COACH "; coach$
                Locate 22, 12: Print " "

            Case 22
                Locate 23, 21: Print Space$(10)
                Locate 23, 1: Input "22. AVG ATTENDANCE "; teamAttendance&
                Locate 23, 21: Print " "

            Case 23
                Locate 24, 11: Print Space$(10)
                Locate 24, 1: Input "23. WINS "; teamRatings(7)
                Locate 24, 11: Print " "

            Case 24
                Locate 25, 13: Print Space$(10)
                Locate 25, 1: Input "24. LOSSES "; teamRatings(8)
                Locate 25, 13: Print " "

            Case 25
                Locate 26, 19: Print Space$(10)
                Locate 26, 1: Input "25. POWER RATING "; teamRatings(9)
                Locate 26, 19: Print " "

        End Select

        Locate LAST_ROW - 2, 53
        Print Space$(10)
            
    Loop Until B9 = 0


    Color 15, 0
    Cls

    Color 14
    Print Tab(30); "CHANGE ACTUAL STATS"
    Color 15

    teamStats!(0) = teamRatings(5)

    Color 14
    Locate 3, 15: Print "TEAM"
    Locate 3, 30: Print "OPPONENTS"
    Color 15
    Locate 4, 1: Print " 1. FGA ";
    Locate 4, 14: Print teamStats!(1)
    Locate 4, 29: Print teamStats!(2)
    Locate 5, 1: Print " 2. FG % ";
    Locate 5, 14: Print teamStats!(3)
    Locate 5, 29: Print teamStats!(4)
    Locate 6, 1: Print " 3. FTA";
    Locate 6, 14: Print teamStats!(5)
    Locate 6, 29: Print teamStats!(6)
    Locate 7, 1: Print " 4. FT %";
    Locate 7, 14: Print teamStats!(7)
    Locate 7, 29: Print teamStats!(8)
    Locate 8, 1: Print " 5. RB";
    Locate 8, 14: Print teamStats!(9)
    Locate 8, 29: Print teamStats!(10)
    Locate 9, 1: Print " 6. AST";
    Locate 9, 14: Print teamStats!(11)
    Locate 9, 29: Print teamStats!(12)
    Locate 10, 1: Print " 7. FOULS";
    Locate 10, 14: Print teamStats!(13)
    Locate 10, 29: Print teamStats!(14)
    Locate 11, 1: Print " 8. TO";
    Locate 11, 14: Print teamStats!(15)
    Locate 11, 29: Print teamStats!(16)
    Locate 12, 1: Print " 9. STLS";
    Locate 12, 14: Print teamStats!(17)
    Locate 12, 29: Print teamStats!(18)
    Locate 13, 1: Print "10. BLOCKS";
    Locate 13, 14: Print teamStats!(23);
    Locate 13, 29: Print teamStats!(24)
    Locate 14, 1: Print "11. 3 FGA";
    Locate 14, 14: Print teamStats!(21);
    Locate 14, 29: Print teamStats!(19)
    Locate 15, 1: Print "12. 3 FG %";
    Locate 15, 14: Print teamStats!(22);
    Locate 15, 29: Print teamStats!(20)

    Do

        Do
            Locate 23, 25
            Input "# TO CHANGE (0 TO CONTINUE)"; B9
        Loop Until B9 >= 0 And B9 <= 12

        Select Case B9

            Case 1
                Locate 4, 1: Print " 1. FGA";

                Locate 4, 15: Print Space$(10)
                Locate 4, 13: Input teamStats!(1)
                Locate 4, 13: Print " "

                Locate 4, 30: Print Space$(10)
                Locate 4, 28: Input teamStats!(2)
                Locate 4, 28: Print " "

            Case 2
                Locate 5, 1: Print " 2. FG %";

                Locate 5, 15: Print Space$(10)
                Locate 5, 13: Input teamStats!(3)
                Locate 5, 13: Print " "

                Locate 5, 30: Print Space$(10)
                Locate 5, 28: Input teamStats!(4)
                Locate 5, 28: Print " "

            Case 3
                Locate 6, 1: Print " 3. FTA";

                Locate 6, 15: Print Space$(10)
                Locate 6, 13: Input teamStats!(5)
                Locate 6, 13: Print " "

                Locate 6, 30: Print Space$(10)
                Locate 6, 28: Input teamStats!(6)
                Locate 6, 28: Print " "

            Case 4
                Locate 7, 1: Print " 4. FT %"

                Locate 7, 15: Print Space$(10)
                Locate 7, 13: Input teamStats!(7)
                Locate 7, 13: Print " "

                Locate 7, 30: Print Space$(10)
                Locate 7, 28: Input teamStats!(8)
                Locate 7, 28: Print " "

            Case 5
                Locate 8, 1: Print " 5. RB";

                Locate 8, 15: Print Space$(10)
                Locate 8, 13: Input teamStats!(9)
                Locate 8, 13: Print " "

                Locate 8, 30: Print Space$(10)
                Locate 8, 28: Input teamStats!(10)
                Locate 8, 28: Print " "

            Case 6
                Locate 9, 1: Print " 6. AST";

                Locate 9, 15: Print Space$(10)
                Locate 9, 13: Input teamStats!(11)
                Locate 9, 13: Print " "

                Locate 9, 30: Print Space$(10)
                Locate 9, 28: Input teamStats!(12)
                Locate 9, 28: Print " "

            Case 7
                Locate 10, 1: Print " 7. FOULS";

                Locate 10, 15: Print Space$(10)
                Locate 10, 13: Input teamStats!(13)
                Locate 10, 13: Print " "

                Locate 10, 30: Print Space$(10)
                Locate 10, 28: Input teamStats!(14)
                Locate 10, 28: Print " "

            Case 8
                Locate 11, 1: Print " 8. TO";

                Locate 11, 15: Print Space$(10)
                Locate 11, 13: Input teamStats!(15)
                Locate 11, 13: Print " "

                Locate 11, 30: Print Space$(10)
                Locate 11, 28: Input teamStats!(16)
                Locate 11, 28: Print " "

            Case 9
                Locate 12, 1: Print " 9. STLS";

                Locate 12, 15: Print Space$(10)
                Locate 12, 13: Input teamStats!(17)
                Locate 12, 13: Print " "

                Locate 12, 30: Print Space$(10)
                Locate 12, 28: Input teamStats!(18)
                Locate 12, 28: Print " "

            Case 10
                Locate 13, 15: Print Space$(10)
                Locate 13, 1: Print "10. BLOCKS";
                Locate 13, 13: Input teamStats!(23)
                Locate 13, 13: Print " "

                Locate 13, 30: Print Space$(10)
                Locate 13, 28: Input teamStats!(24)
                Locate 13, 28: Print " "

            Case 11
                Locate 14, 15: Print Space$(10)
                Locate 14, 1: Print "11. 3 FGA";
                Locate 14, 13: Input teamStats!(21)
                Locate 14, 13: Print " "

                Locate 14, 30: Print Space$(10)
                Locate 14, 28: Input teamStats!(19)
                Locate 14, 28: Print " "

            Case 12
                Locate 15, 15: Print Space$(10)
                Locate 15, 1: Print "12. 3 FG %";
                Locate 15, 13: Input teamStats!(22)
                Locate 15, 13: Print " "

                Locate 15, 30: Print Space$(10)
                Locate 15, 28: Input teamStats!(20)
                Locate 15, 28: Print " "

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
            Locate 11, 37
            Print Space$(10)
            Locate 11, 1
            Input "# TO CHANGE (1-13) (0 TO CONTINUE) "; idx
        Loop Until idx >= 0 And idx <= 14

        If idx > 0 Then

            I = idx - 1

            Cls
            Color 14
            Print Tab(35); "PLAYER - "; idx
            Print Tab(31); "CHANGE PLAYER STATS"
            Color 15

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
                        Locate 3, 1: Input " 1. NAME "; playerNames$(I)
                        Locate 3, 10: Print " "

                    Case 2
                        Locate 4, 1: Input " 2. POSITION "; position$(I)
                        Locate 3, 14: Print " "

                    Case 3
                        Locate 5, 17: Print Space$(10)
                        Locate 5, 1: Input " 3. GAMES PLAYED"; playerRatings!(I, 0)
                        Locate 5, 17: Print " "

                    Case 4
                        Locate 6, 12: Print Space$(10)
                        Locate 6, 1: Input " 4. ADJ FG%"; playerOff!(I, 0)
                        Locate 6, 12: Print " "

                    Case 5
                        Locate 7, 8: Print Space$(10)
                        Locate 7, 1: Input " 5. FT%"; playerOff!(I, 1)
                        Locate 7, 8: Print " "

                    Case 6
                        Locate 8, 23: Print Space$(10)
                        Locate 8, 1: Input " 6. OFF RB/40 MIN X 10"; playerRatings!(I, 4)
                        Locate 8, 23: Print " "

                    Case 7
                        Locate 9, 23: Print Space$(10)
                        Locate 9, 1: Input " 7. DEF RB/40 MIN X 10"; playerOff!(I, 2)
                        Locate 9, 23: Print " "

                    Case 8
                        Locate 10, 18: Print Space$(10)
                        Locate 10, 1: Input " 8. ASSIST RATING"; playerOff!(I, 4)
                        Locate 10, 18: Print " "

                    Case 9
                        Locate 11, 17: Print Space$(10)
                        Locate 11, 1: Input " 9. STEAL RATING"; playerOff!(I, 5)
                        Locate 11, 17: Print " "

                    Case 10
                        Locate 12, 17: Print Space$(10)
                        Locate 12, 1: Input "10. BLOCK RATING"; playerOff!(I, 6)
                        Locate 12, 17: Print " "

                    Case 11
                        Locate 13, 8: Print Space$(10)
                        Locate 13, 1: Input "11. FGA"; playerRatings!(I, 1)
                        Locate 13, 8: Print " "

                    Case 12
                        Locate 14, 24: Print Space$(10)
                        Locate 14, 1: Input "12. FOUL COMMIT RATING"; playerRatings!(I, 5)
                        Locate 14, 24: Print " "

                    Case 13
                        Locate 15, 24: Print Space$(10)
                        Locate 15, 1: Input "13. FOUL DRAWING RATING"; playerRatings!(I, 6)
                        Locate 15, 24: Print " "

                    Case 14
                        Locate 16, 9: Print Space$(10)
                        Locate 16, 1: Input "14. 3FG%"; playerRatings!(I, 2)
                        Locate 16, 9: Print " "

                    Case 15
                        Locate 17, 17: Print Space$(10)
                        Locate 17, 1: Input "15. 3FGA/TOT FGA"; playerRatings!(I, 3)
                        Locate 17, 17: Print " "

                        'Case 16
                        'Locate 18, 28: Print Space$(10)
                        'Locate 18, 1: Input "16. DEFENSIVE RATING (1-9) "; playerDef!(I)
                        'Locate 18, 28: Print " "

                    Case 16
                        Locate 3, 46: Print Space$(10)
                        Locate 3, 37: Input "17. GAMES"; playerStats!(I, 0)
                        Locate 3, 46: Print " "

                    Case 17
                        Locate 4, 48: Print Space$(10)
                        Locate 4, 37: Input "18. MINUTES"; playerStats!(I, 1)
                        Locate 4, 48: Print " "

                    Case 18
                        Locate 5, 44: Print Space$(10)
                        Locate 5, 37: Input "19. FGM"; playerStats!(I, 3)
                        Locate 5, 44: Print " "

                    Case 19
                        Locate 6, 44: Print Space$(10)
                        Locate 6, 37: Input "20. FGA"; playerStats!(I, 2)
                        Locate 6, 44: Print " "

                    Case 20
                        Locate 7, 44: Print Space$(10)
                        Locate 7, 37: Input "21. FTM"; playerStats!(I, 5)
                        Locate 7, 44: Print " "

                    Case 21
                        Locate 8, 44: Print Space$(10)
                        Locate 8, 37: Input "22. FTA"; playerStats!(I, 4)
                        Locate 8, 44: Print " "

                    Case 22
                        Locate 9, 48: Print Space$(10)
                        Locate 9, 37: Input "23. OFF REB"; playerStats!(I, 6)
                        Locate 9, 48: Print " "

                    Case 23
                        Locate 10, 50: Print Space$(10)
                        Locate 10, 37: Input "24. TOTAL REB"; playerStats!(I, 7)
                        Locate 10, 50: Print " "

                    Case 24
                        Locate 11, 50: Print Space$(10)
                        Locate 11, 37: Input "25. PER FOULS"; playerStats!(I, 8)
                        Locate 11, 50: Print " "

                    Case 25
                        Locate 12, 48: Print Space$(10)
                        Locate 12, 37: Input "26. ASSISTS"; playerStats!(I, 9)
                        Locate 12, 48: Print " "

                    Case 26
                        Locate 13, 47: Print Space$(10)
                        Locate 13, 37: Input "27. BLOCKS"; playerStats!(I, 10)
                        Locate 13, 47: Print " "

                    Case 27
                        Locate 14, 47: Print Space$(10)
                        Locate 14, 37: Input "28. STEALS"; playerStats!(I, 11)
                        Locate 14, 47: Print " "

                    Case 28
                        Locate 15, 46: Print Space$(10)
                        Locate 15, 37: Input "29. 3 FGM"; playerStats!(I, 13)
                        Locate 15, 46: Print " "

                    Case 29
                        Locate 16, 46: Print Space$(10)
                        Locate 16, 37: Input "30. 3 FGA"; playerStats!(I, 12)
                        Locate 16, 46: Print " "

                End Select

                Locate 22, 53
                Print Space$(10)

            Loop Until B9 = 0

        End If

    Loop Until idx = 0

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

    Print #1, "TEAM ROSTER FOR "; teamName$
    Print #1,
    Print #1, "AVG SHOTS PER GAME = "; teamRatings(3); "      DEF FG ADJ = "; defFGPctAdj!
    Print #1, "OPP PCT OF 3FG ATT ADJ = "; defthreeFGA_Adj; "  OPP 3FG PCT ADJ = "; def3FG_Adj
    Print #1,
    Print #1, "DEF STEAL ADJ "; leagueRatings(0)
    Print #1, "OFF TURNOVER ADJ "; leagueRatings(1)
    Print #1, "DEF TURNOVER ADJ "; leagueRatings(2)
    Print #1, "OFF FOUL ADJ "; leagueRatings(3)
    Print #1,
    Print #1, "NAME           FG  FT ORB DRB  CX PS  D  B FC FD  3FG 3FG%":
    Print #1,

    For I = 0 To 13
        If playerNames$(I) <> "XXX" Then
            Print #1, playerNames$(I); Tab(15);
            Print #1, Using "### ### ### ### ### "; playerOff!(I, 0); playerOff!(I, 1); playerRatings!(I, 4); playerOff!(I, 2); playerOff!(I, 3);
            Print #1, Using "## ## ## ## ## ### ###"; playerOff!(I, 4); playerOff!(I, 5); playerOff!(I, 6); playerRatings!(I, 5); playerRatings!(I, 6); playerRatings!(I, 3); playerRatings!(I, 2)
        End If
    Next I

    Print #1,
    Print #1, "FG - FIELD GOAL %                 B - BLOCK SHOT RATING"
    Print #1, "FT - FREE THROW %                FC - FOUL COMMIT RATING"
    Print #1, "RB - REBOUNDING RATING           FD - FOUL DRAWING RATING"
    Print #1, "CX - CONTRIBUTION FACTOR        3FG - 3FGA/TOTAL FGA"
    Print #1, "PS - PASSING RATING            3FG% - THREE POINT GOAL %"
    Print #1, " D - DEFENSIVE RATING"

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
Sub ViewRoster (teamName$)

    Shared def3FG_Adj, defthreeFGA_Adj
    Shared defFGPctAdj!, staminaRating!

    Shared mascot$, coach$, arenaName$

    Shared leagueRatings(), teamRatings(), teamStats!()

    Shared playerOff!(), playerDef!(), playerRatings!()

    Shared playerNames$(), position$()

    Color 15, 0
    Cls

    Locate 2, 1
    Print "TEAM ROSTER FOR ";
    Color teamRatings(0), teamRatings(1): Print teamName$
    
    Locate 4, 1
    Color 11, 0: Print "AVG SHOTS / GAME       = ";
    Color 15: Print teamRatings(3);

    Color 11: Locate , 30: Print "DEF FG ADJ    = ";
    Color 15: Print defFGPctAdj!

    Color 11: Print "OPP PCT OF 3FG ATT ADJ = ";
    Color 15: Print defthreeFGA_Adj;

    Color 11: Locate , 30: Print "DEF STEAL ADJ = ";
    Color 15: Print leagueRatings(0);

    Color 11: Locate , 50: Print "OFF TURNOVER ADJ = ";
    Color 15: Print leagueRatings(1)

    Color 11: Print "OPP 3FG PCT ADJ        = ";
    Color 15: Print def3FG_Adj;

    Color 11: Locate , 30: Print "OFF FOUL ADJ  = ";
    Color 15: Print leagueRatings(3);

    Color 11: Locate , 50: Print "DEF TURNOVER ADJ = ";
    Color 15: Print leagueRatings(2)

    Locate 8, 1
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
    Print " B";
    Color 15
    Print " - BLOCK SHOT RATING";
    Locate , 32
    Color 2
    Print "FC";
    Color 15
    Print " - FOUL COMMIT RATING"

    Color 2
    Print "RB";
    Color 15
    Print " - REBOUNDING RATING";
    Locate , 32
    Color 2
    Print "FD";
    Color 15
    Print " - FOUL DRAWING RATING"

    Color 2
    Print "CX";
    Color 15
    Print " - CONTRIBUTION FACTOR";
    Locate , 32
    Color 2
    Print "3FG";
    Color 15
    Print " - 3FGA/TOTAL FGA"

    Color 2
    Print "PS";
    Color 15
    Print " - PASSING RATING";
    Locate , 32
    Color 2
    Print "D";
    Color 15
    Print " - DEFENSIVE RATING"

    Locate LAST_ROW - 1, 30
    Color 15: Print "HIT ANY KEY TO EXIT";
    I$ = GetKeyPress$

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
