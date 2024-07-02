'----------------------------------------
'           COMPARE Subroutine
'----------------------------------------
'This subroutine loads in existing stats
'for a team and provides different options
'to the end user for formatting stats
Sub COMPARE ()

    _Title "Courtside College Basketball - Stats Comparison"

    Shared teamScore(), oppScore(), teamRatings%()
    Shared replayLosses, replayWins

    Shared statsA!(), statsW0!(), statsW1!()
    Shared statsZ!(), statsZ1!()

    Shared position$()
    Shared statPlyrNames$(), locIndicator$(), oppName$()

    Dim posn$(0 To 13)

    Do

        teamNbr = 0
        Call ChooseTeam(YN$, NTMS, teamNbr, team$)

        If teamNbr <> 0 Then
        
            haveStats = 1

            statFile$ = nameStatFile$(team$)
        
            If Not _FileExists(diskPaths$(1) + statFile$) Then

                haveStats = 0

                MsgText$ = "NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU"
                BoxWidth = 24
                BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                Call CSMsgBox(MsgText$, BoxWidth, PASS)
                I$ = GetKeyPress$
                Call CSMsgBox("", 0, PASS)

            Else

                Open diskPaths$(1) + statFile$ For Input As #1

                Input #1, statTeam$

                For I = 0 To 13:

                    Input #1, statPlyrNames$(I)

                    For J = 0 To 13
                        Input #1, statsA!(I, J)
                    Next

                    Input #1, statsW0!(I)

                    Input #1, statsW1!(I)

                Next

                For L = 0 To 15:
                    Input #1, statsZ!(L)
                Next

                For L = 0 To 15:
                    Input #1, statsZ1!(L)
                Next

                Close #1

                For I = 0 To 13
                    If position$(I) = "F " Then posn$(I) = "FORWARD"
                    If position$(I) = "C " Then posn$(I) = "CENTER"
                    If position$(I) = "G " Then posn$(I) = "GUARD"
                    If position$(I) = "FC" Then posn$(I) = "FORWARD/GUARD"
                    If position$(I) = "FG" Then posn$(I) = "FORWARD/CENTER"
                    If position$(I) = "GF" Then posn$(I) = "GUARD/FORWARD"
                    If position$(I) = "CF" Then posn$(I) = "CENTER/FORWARD"
                Next

                Open diskPaths$(1) + statFile$ + "S" For Input As #2

                Input #2, numberGames

                For J = 1 To MAX_SCHED_STATS: 
                    Input #2, teamScore(J)
                Next

                For J = 1 To MAX_SCHED_STATS: 
                    Input #2, oppName$(J)
                Next

                For J = 1 To MAX_SCHED_STATS: 
                    Input #2, oppScore(J)
                Next

                replayLosses = 0
                replayWins = 0

                For currGame = 1 To numberGames
                    If teamScore(currGame) < oppScore(currGame) Then
                        replayLosses = replayLosses + 1
                    End If
                    If teamScore(currGame) > oppScore(currGame) Then
                        replayWins = replayWins + 1
                    End If
                Next

                userKey$ = GetOutputOptions$

                If userKey$ <> Chr$(27) Then

                    backToMenu = 0

                    Select Case userKey$

                        Case "F":
                            userFile$ = _SAVEFILEDIALOG$("Save Comparison Report",_CWD$ + "\Comparison","*.txt","Text File")
                            If userFile$ <> "" Then
                                Call Comparisons_Report(0, userFile$, team$, posn$())
                            End If

                        Case "P":
                            Call Comparisons_Report(1, "COMPARISON", team$, posn$())

                        Case "S":
                            Call Comparisons_Screen(team$, backToMenu, posn$())

                    End Select

                End If
                
            End If 'Done checking for valid stat file

        End If 'Done checking for valid team

    Loop Until backToMenu = 0 Or userKey$ = Chr$(27) Or teamNbr = 0 Or haveStats = 0

End Sub


'----------------------------------------
'     Comparisons_Screen Subroutine
'----------------------------------------
'This subroutine provides a brief menu of
'player comparisons that can be done, and
'then formats stats for those options for
'use with the user display
Sub Comparisons_Screen (teamName$, backToMenu, posn$())

    Shared teamRatings%(), teamStats!()

    Shared statsW0!(), statsW1!()
    
    Shared statsZ!(), statsZ1!()

    Shared statPlyrNames$()

    Shared replayLosses, replayWins

    Cls

    For dc = 1 To 22
        Color 15, 0
        Locate dc
        Print String$(79, 177)
    Next

    Call TeamComparisons_Screen(teamName$)

    Locate 23, 28
    Color 15, 0: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Do

        Color 0, 0
        Cls

        Color 9
        Locate 3, 12: Print "STAT COMPARISONS FOR ";
        Color 15: Print teamName$

        'Print names of all 14 characters
        For I = 0 To 13
            Color 12
            Locate I + 5, 15
            Print Using "##"; I + 1;
            Print ". ";
            Color 7: Print statPlyrNames$(I)
        Next

        Color 12
        Locate 5, 15: Print "15. ";
        Color 3: Print "View Players In Sequence"
        Color 12
        Locate 6, 15: Print "16. ";
        Color 3: Print "View Another Team"
        Color 12
        Locate , 15: Print "17. ";
        Color 3: Print "Return To Main Menu"

        Do

            Color 15
            Locate 23, 15
            Input "ENTER # TO VIEW "; B9

            Select Case B9

                Case 1 To 14:
                    playerIdx = B9 - 1

                    Cls

                    For dc = 1 To 19
                        Color 15, 0: Locate dc
                        Print String$(78, 177)
                    Next

                    Call PlayerComparisons_Screen(playerIdx, posn$(playerIdx), teamName$)

                    Color 15, 0: Locate 22: Print "HIT ENTER TO CONTINUE"
                    I$ = GetKeyPress$

                Case 15:
                    Cls

                    For dc = 1 To 19
                        Color 15, 0: Locate dc
                        Print String$(78, 177)
                    Next

                    menuExit = 0

                    playerIdx = 0

                    Do

                        If statPlyrNames$(playerIdx) <> "XXX" And statPlyrNames$(playerIdx) <> "END" And statPlyrNames$(playerIdx) <> "" Then

                            Call PlayerComparisons_Screen(playerIdx, posn$(playerIdx), teamName$)

                            Color 15, 0
                            Locate 22: Print "HIT ENTER TO CONTINUE"
                            Locate 22, 23: Print "OR ESC TO RETURN TO MENU"
                            I$ = GetKeyPress$

                            If I$ = Chr$(27) Then menuExit = 1

                        End If 'done checking for null player

                        playerIdx = playerIdx + 1

                    Loop Until playerIdx = 14 Or menuExit = 1

                Case 16:
                    backToMenu = 1

                Case 17:
                    Exit Sub

            End Select

            Locate 23, 30
            Print Space$(10)

        Loop Until B9 >= 1 And B9 <= 17 Or menuExit = 1

    Loop Until B9 = 17 Or backToMenu = 1

    If backToMenu <> 1 Then
        Color 15, 0
        Locate 23, 28: Print "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$
    End If

End Sub


'----------------------------------------
'   TeamComparisons_Screen Subroutine
'----------------------------------------
'This subroutine focuses on the formatting
'of team stats for comparison. The stats
'are output to the end user display
Sub TeamComparisons_Screen (teamName$)

    Shared teamRatings%(), teamStats!()
    Shared statsZ!(), statsZ1!()

    Shared replayLosses, replayWins

    Locate 11, 31: Print Space$(18)

    team$ = _Trim$(teamName$)
    teamLen% = Len(team$)

    Color 11
    Locate 11, Int((80 - teamLen%) / 2): Print teamName$

    Color 15, 2
    Locate 5, 30: Print "              W - L ";
    Locate 6, 30: Print "  ACTUAL   ";
    Locate 7, 30: Print "  REPLAY   ";

    Color 15, 1
    Locate 2, 3: Print "   OFFENSIVE STATS    "
    Locate 3, 3: Print "       ACTUAL   REPLAY"
    Locate 4, 3: Print " FGM   ";
    Locate 5, 3: Print " FGA   ";
    Locate 6, 3: Print " FG PCT";
    Locate 7, 3: Print " FTM   ";
    Locate 8, 3: Print " FTA   ";
    Locate 9, 3: Print " FT PCT";
    Locate 10, 3: Print " O.RB  ";
    Locate 11, 3: Print " D.RB  ";
    Locate 12, 3: Print " T.REB ";
    Locate 13, 3: Print " ASS'T ";
    Locate 14, 3: Print " FLS   ";
    Locate 15, 3: Print "  ST   ";
    Locate 16, 3: Print "  TO   ";
    Locate 17, 3: Print " BLK   ";
    Locate 18, 3: Print " 3-FGM ";
    Locate 19, 3: Print " 3-FGA ";
    Locate 20, 3: Print " 3-PCT ";

    Color 15, 12
    Locate 2, 55: Print "   DEFENSIVE STATS   "
    Locate 3, 55: Print "       ACTUAL  REPLAY"
    Locate 4, 55: Print " FGM   ";
    Locate 5, 55: Print " FGA   ";
    Locate 6, 55: Print " FG PCT";
    Locate 7, 55: Print " FTM   ";
    Locate 8, 55: Print " FTA   ";
    Locate 9, 55: Print " FT PCT";
    Locate 10, 55: Print " O.RB  ";
    Locate 11, 55: Print " D.RB  ";
    Locate 12, 55: Print " T.REB ";
    Locate 13, 55: Print " ASS'T ";
    Locate 14, 55: Print " FLS   ";
    Locate 15, 55: Print "  ST   ";
    Locate 16, 55: Print "  TO   ";
    Locate 17, 55: Print " BLK   ";
    Locate 18, 55: Print " 3-FGM ";
    Locate 19, 55: Print " 3-FGA ";
    Locate 20, 55: Print " 3-PCT ";

    For I = 3 To 21:
        Locate I, 25
        Color 7, 0
        Print String$(2, 177)
    Next

    For I = 3 To 21:
        Locate I, 76
        Print String$(2, 177)
    Next

    Locate 21, 5: Print String$(20, 177)
    Locate 21, 57: Print String$(20, 177)

    For I = 6 To 8:
        Locate I, 50
        Print String$(2, 177)
    Next

    Locate 8, 32: Print String$(18, 177)

    Color 3, 0
    Locate 6, 41: Print Using "####"; teamRatings%(7);
    Locate 6, 45: Print Using "#### "; teamRatings%(8);

    Color 14, 0
    Locate 7, 41: Print Using "####"; replayWins;
    Locate 7, 45: Print Using "#### "; replayLosses;

    ' *** ACTUAL VALUES ***

    'Print out all ACTUAL values from COLBBTMS

    '--> OFFENSE

    Color 3, 0

    'FGM
    Locate 4, 10: Print Using "######"; teamStats!(1) / 100 * teamStats!(3);

    'FGA
    Locate 5, 10: Print Using "######"; teamStats!(1);

    'FG PCT
    Locate 6, 10
    If teamStats!(1) = 0 Then 
        Print "  .000";
    End If
    If teamStats!(1) > 0 Then 
        statPCT! = teamStats!(3) / 100
        Print "  ";
        Print Using ".###"; statPCT!;
    End If

    'FTM
    Locate 7, 10: Print Using "######"; teamStats!(5) / 100 * teamStats!(7);
    
    'FTA
    Locate 8, 10: Print Using "######"; teamStats!(5);

    'FT Pct
    Locate 9, 10
    If teamStats!(5) = 0 Then 
        Print "  .000";
    End If
    If teamStats!(5) > 0 Then 
        statPCT! = teamStats!(7) / 100
        Print "  ";
        Print Using ".###"; statPCT!;
    End If

    'Rebounds / Assists / Fouls / Steals / TOs
    Locate 10, 10: Print Using "######"; .35 * teamStats!(9);
    Locate 11, 10: Print Using "######"; .65 * teamStats!(9);
    Locate 12, 10: Print Using "######"; teamStats!(9);
    Locate 13, 10: Print Using "######"; teamStats!(11);
    Locate 14, 10: Print Using "######"; teamStats!(13);
    Locate 15, 10: Print Using "######"; teamStats!(17);
    Locate 16, 10: Print Using "######"; teamStats!(15);
    
    'Blocks
    Locate 17, 10
    If teamStats!(21) = 0 Then
        Print "   ---";
    Else
        Print Using "######"; teamStats!(21);
    End If

    '3FGM
    Locate 18, 10    
    If teamStats!(23) = 0 Then
        Print "   ---"
    Else
        Print Using "######"; teamStats!(23) * teamStats!(24) / 100;
    End If

    '3FGA
    Locate 19, 10
    If teamStats!(23) = 0 Then
        Print "   ---"
    Else
        Print Using "######"; teamStats!(23);
    End If

    '3FG Pct
    Locate 20, 10
    If teamStats!(24) = 0 Then 
        Print "  .000";
    End If
    If teamStats!(24) > 0 Then 
        statPCT! = teamStats!(24) / 100
        Print "  ";
        Print Using ".###"; statPCT!;
    End If

    '--> DEFENSE

    Color 3, 0

    'FGM
    Locate 4, 62: Print Using "######"; teamStats!(2) / 100 * teamStats!(4);

    'FGA
    Locate 5, 62: Print Using "######"; teamStats!(2);

    'FG PCT
    Locate 6, 62
    If teamRatings%(2) = 0 Then 
        Print "  .000";
    End If
    If teamRatings%(2) > 0 Then
        statPCT! = teamStats!(4) / 100
        Print "  ";
        Print Using ".###"; statPCT!;
    End If

    'FTM
    Locate 7, 62: Print Using "######"; teamStats!(6) / 100 * teamStats!(8);
    
    'FTA
    Locate 8, 62: Print Using "######"; teamStats!(6);

    'FT Pct
    Locate 9, 62
    If teamRatings%(6) = 0 Then 
        Print "  .000";
    End If
    If teamRatings%(6) > 0 Then
        statPCT! = teamStats!(8) / 100
        Print "  ";
        Print Using ".###"; statPCT!;
    End if

    'Rebounds / Assists / Fouls / Steals / TOs
    Locate 10, 62: Print Using "######"; .35 * teamStats!(10);
    Locate 11, 62: Print Using "######"; .65 * teamStats!(10);
    Locate 12, 62: Print Using "######"; teamStats!(10);
    Locate 13, 62: Print Using "######"; teamStats!(12);
    Locate 14, 62: Print Using "######"; teamStats!(14);
    Locate 15, 62: Print Using "######"; teamStats!(18);
    Locate 16, 62: Print Using "######"; teamStats!(16);

    ' Blocks
    Locate 17, 62
    If teamStats!(22) = 0 Then
        Print "   ---"
    Else
        Print Using "   ###"; teamStats!(22);
    End If

    '3FGM
    Locate 18, 62: Print Using "######"; teamStats!(19) * teamStats!(20) / 100;
    
    '3FGA
    Locate 19, 62: Print Using "######"; teamStats!(19);

    '3FG Pct
    Locate 20, 62
    If teamRatings%(9) = 0 Then
        Print "  .000";
    End If
    If teamRatings%(9) > 0 Then
        statPCT! = teamStats!(20) / 100
        Print "  ";
        Print Using ".###"; statPCT!;
    End If


    ' *** REPLAY VALUES ***
    
    'Print out all REPLAY values from stat files

    '--> OFFENSE
    
    'FGM
    Locate 4, 16: Print Using "#########"; statsZ!(2);

    'FGA
    Locate 5, 16: Print Using "#########"; statsZ!(1);

    'FG PCT
    Locate 6, 16
    If statsZ!(1) = 0 Then 
        Print "     .000";
    End If
    If statsZ!(1) > 0 Then 
        statPCT! = (statsZ!(2) / statsZ!(1))
        Print "     ";
        Print Using ".###"; statPCT!;
    End If

    'FTM
    Locate 7, 16: Print Using "#########"; statsZ!(4);

    'FTA
    Locate 8, 16: Print Using "#########"; statsZ!(3);

    'FT PCT
    Locate 9, 16
    If statsZ!(3) = 0 Then 
        Print "     .000";
    End If
    If statsZ!(3) > 0 Then 
        statPCT! = (statsZ!(4) / statsZ!(3))
        Print "     ";: Print Using ".###"; statPCT!;
    End If

    'Rebounds / Assists / Fouls / Steals / TOs
    Locate 10, 16: Print Using "#########"; statsZ!(12);
    Locate 11, 16: Print Using "#########"; statsZ!(5) - statsZ!(12) - statsZ!(15);
    Locate 12, 16: Print Using "#########"; statsZ!(5) - statsZ!(15);
    Locate 13, 16: Print Using "#########"; statsZ!(8);
    Locate 14, 16: Print Using "#########"; statsZ!(6);
    Locate 15, 16: Print Using "#########"; statsZ!(7);
    Locate 16, 16: Print Using "#########"; statsZ!(11);
    
    'Blocks
    Locate 17, 16: Print Using "#########"; statsZ!(9);

    '3 FGM
    Locate 18, 16: Print Using "#########"; statsZ!(13);

    '3 FGA
    Locate 19, 16: Print Using "#########"; statsZ!(14);

    '3 FG PCT
    Locate 20, 16
    If statsZ!(13) = 0 Then
        Print "     .000";
    End If
    If statsZ!(13) > 0 Then
        statPCT! = (statsZ!(13) / statsZ!(14))
        Print "     ";
        Print Using ".###"; statPCT!;
    End If


    '--> DEFENSE

    'FGM
    Locate 4, 68: Print Using "########"; statsZ1!(2);
    
    'FGA
    Locate 5, 68: Print Using "########"; statsZ1!(1);

    'FG PCT
    Locate 6, 68
    If statsZ1!(1) = 0 Then 
        Print "    .000";
    End If
    If statsZ1!(1) > 0 Then 
        statPCT! = (statsZ1!(2) / statsZ1!(1))
        Print Using "    .###"; statPCT!;
    End If

    'FTM
    Locate 7, 68: Print Using "########"; statsZ1!(4);
    
    'FTA
    Locate 8, 68: Print Using "########"; statsZ1!(3);

    'FT PCT
    Locate 9, 68
    If statsZ1!(3) = 0 Then
        Print "    .000";
    End If
    If statsZ1!(3) > 0 Then 
        statPCT! = (statsZ1!(4) / statsZ1!(3))
        Print Using "    .###"; statPCT!;
    End If

    'Rebounds / Assists / Fouls / Steals / TOs
    Locate 10, 68: Print Using "########"; statsZ1!(12);
    Locate 11, 68: Print Using "########"; statsZ1!(5) - statsZ1!(12) - statsZ1!(15);
    Locate 12, 68: Print Using "########"; statsZ1!(5) - statsZ1!(15);
    Locate 13, 68: Print Using "########"; statsZ1!(8);
    Locate 14, 68: Print Using "########"; statsZ1!(6);
    Locate 15, 68: Print Using "########"; statsZ1!(7);
    Locate 16, 68: Print Using "########"; statsZ1!(11);
    Locate 17, 68: Print Using "########"; statsZ1!(9);

    '3 FGM
    Locate 18, 68: Print Using "########"; statsZ1!(13);

    '3 FGA
    Locate 19, 68: Print Using "########"; statsZ1!(14);

    '3 FGA PCT
    If statsZ1!(13) = 0 Then
        Locate 20, 68: Print "    .000";
    End If
    If statsZ1!(13) > 0 Then
        Locate 20, 68: statPCT! = (statsZ1!(13) / statsZ1!(14))
        Print "    ";: Print Using ".###"; statPCT!;
    End If

End Sub


'----------------------------------------
'  PlayerComparisons_Screen Subroutine
'----------------------------------------
'This subroutine focuses on the formatting
'of team stats for comparison. The stats
'are output to the end user display
Sub PlayerComparisons_Screen (idx, position$, teamName$)

    Shared statsA!(), playerStats!()
    Shared statsW0!(), statsW1!()

    Shared statPlyrNames$()

    Color 11
    Locate 1, 2: Print teamName$

    Color 15, 0
    Locate 1, 14: Print String$(34, 177)

    Color 14

    'Player Name / Position
    Locate 2, 14: Print statPlyrNames$(idx);
    Print " - ";
    Color 11: Print position$

    Color 12
    Locate 5, 2: Print "ACTUAL  "
    Locate 6, 2: Print "REPLAY  ";
    
    Color 7, 0: Print String$(2, 177)
    Locate 7, 4: Print String$(8, 177)

    Color 0, 11
    Locate 4, 14: Print "  G ";
    Locate , 18: Print " MIN ";
    Locate , 23: Print " FGM ";
    Locate , 28: Print " FGA ";
    Locate , 33: Print " FG-PCT ";
    Locate , 41: Print " FTM ";
    Locate , 46: Print " FTA ";
    Locate , 51: Print " FT-PCT ";
    Locate , 59: Print " O.REB ";
    Locate , 65: Print "  O.REB/G"

    Color 12, 0
    Locate 10, 2: Print "ACTUAL  "
    Locate 11, 2: Print "REPLAY  ";

    Color 7, 0: Print String$(2, 177)
    Locate 12, 4: Print String$(8, 177)

    Color 0, 11
    Locate 9, 14: Print " D.REB ";
    Locate , 21: Print " D.REB/G ";
    Locate , 30: Print " ASST ";
    Locate , 36: Print " ASST/G ";
    Locate , 44: Print " FOULS ";
    Locate , 51: Print " F/G ";
    Locate , 56: Print " TREB ";
    Locate , 62: Print " STL ";
    Locate , 67: Print " STL/G"

    Color 12, 0
    Locate 15, 2: Print "ACTUAL  "
    Locate 16, 2: Print "REPLAY  ";

    Color 7, 0: Print String$(2, 177)
    Locate 12, 4: Print String$(8, 177)

    Color 0, 11
    Locate 14, 14: Print " BLK ";
    Locate , 19: Print " BLK/G ";
    Locate , 26: Print " 3-FGM ";
    Locate , 33: Print " 3-FGA ";
    Locate , 40: Print " 3-FG-PCT ";
    Locate , 50: Print "  RB/G ";
    Locate , 57: Print "  PTS";
    Locate , 62: Print " PTS/G ";
    Locate , 69: Print " MIN/G"

    Color 1, 11
    Locate 5, 14: Print Using "###"; playerStats!(idx, 0);
    Locate , 17: Print Using "#####"; playerStats!(idx, 1);
    Locate , 22: Print Using "#####"; playerStats!(idx, 3);
    Locate , 27: Print Using "#####"; playerStats!(idx, 2);

    Locate , 32
    If playerStats!(idx, 2) = 0 Then 
        Print "   --.- %";
    End If
    If playerStats!(idx, 2) > 0 Then
        Print Using "#####.# %"; (playerStats!(idx, 3) / playerStats!(idx, 2)) * 100;
    End If

    Locate , 40: Print Using "#####"; playerStats!(idx, 5);
    Locate , 45: Print Using "#####"; playerStats!(idx, 4);

    Locate , 50
    If playerStats!(idx, 4) = 0 Then 
        Print "   --.- %";
    End If
    If playerStats!(idx, 4) > 0 Then
        Print Using "#####.# %"; (playerStats!(idx, 5) / playerStats!(idx, 4)) * 100;
    End If

    Locate , 58: Print Using "#######"; playerStats!(idx, 6);

    Locate , 65
    If playerStats!(idx, 0) = 0 Or playerStats!(idx, 6) = 0 Or (playerStats!(idx, 0) > 0 And playerStats!(idx, 6) = 0) Then 
        Print "      0.0";
    End If
    If playerStats!(idx, 0) > 0 And playerStats!(idx, 6) > 0 Then 
        Print Using "#######.#"; playerStats!(idx, 6) / playerStats!(idx, 0);
    End If

    Color 7, 0: Print String$(2, 177)
    Color 1, 11

    Locate 10, 14: Print Using "######"; playerStats!(idx, 7) - playerStats!(idx, 6);

    Locate , 20
    If playerStats!(idx, 0) = 0 Or playerStats!(idx, 7) - playerStats!(idx, 6) = 0 Or (playerStats!(idx, 0) > 0 And playerStats!(idx, 7) - playerStats!(idx, 6) = 0) Then 
        Print "      0.0";
    End If
    If playerStats!(idx, 0) > 0 And playerStats!(idx, 7) - playerStats!(idx, 6) > 0 Then 
        Print Using "#######.#"; (playerStats!(idx, 7) - playerStats!(idx, 6)) / playerStats!(idx, 0);
    End If

    Locate , 29: Print Using "######"; playerStats!(idx, 9);

    Locate , 35
    If playerStats!(idx, 0) = 0 Or playerStats!(idx, 9) = 0 Or (playerStats!(idx, 0) > 0 And playerStats!(idx, 9) = 0) Then 
        Print "     0.0";
    End If
    If playerStats!(idx, 0) > 0 And playerStats!(idx, 9) > 0 Then 
        Print Using "######.#"; playerStats!(idx, 9) / playerStats!(idx, 0);
    End If

    Locate , 43: Print Using "#######"; playerStats!(idx, 8);

    Locate , 50
    If playerStats!(idx, 0) = 0 Or playerStats!(idx, 8) = 0 Or (playerStats!(idx, 0) > 0 And playerStats!(idx, 8) = 0) Then 
        Print "  0.0";
    End If
    If playerStats!(idx, 0) > 0 And playerStats!(idx, 8) > 0 Then 
        Print Using "###.#"; playerStats!(idx, 8) / playerStats!(idx, 0);
    End If

    Locate , 55: Print Using "######"; playerStats!(idx, 7);
    Locate , 61: Print Using "#####"; playerStats!(idx, 11);

    Locate , 66
    If playerStats!(idx, 0) = 0 Or playerStats!(idx, 11) = 0 Or (playerStats!(idx, 0) > 0 And playerStats!(idx, 11) = 0) Then 
        Print "    0.0";
    End If
    If playerStats!(idx, 0) > 0 And playerStats!(idx, 11) > 0 Then 
        Print Using "#####.#"; playerStats!(idx, 11) / playerStats!(idx, 0);
    End If

    Color 7, 0: Print String$(2, 177)
    Color 1, 11

    Locate 15, 14: Print Using "####"; playerStats!(idx, 10);

    Locate , 18:
    If playerStats!(idx, 0) = 0 Or playerStats!(idx, 10) = 0 Or (playerStats!(idx, 0) > 0 And playerStats!(idx, 10) = 0) Then 
        Print "    0.0";
    End If
    If playerStats!(idx, 0) > 0 And playerStats!(idx, 12) > 0 Then 
        Print Using "#####.#"; playerStats!(idx, 10) / playerStats!(idx, 0);
    End If

    Locate , 25: Print Using "#######"; playerStats!(idx, 13);
    Locate , 32: Print Using "#######"; playerStats!(idx, 12);

    Locate , 39
    If playerStats!(idx, 13) = 0 Then 
        Print "     --.- %";
    End If
    If playerStats!(idx, 12) > 0 Then
        Print Using "#######.# %"; (playerStats!(idx, 13) / playerStats!(idx, 12)) * 100;
    End If

    Locate , 49: If playerStats!(idx, 0) <> 0 Then Print Using "   ##.#"; playerStats!(idx, 7) / playerStats!(idx, 0);
    Locate , 56: Print Using "######"; (playerStats!(idx, 3) * 2) + playerStats!(idx, 5) + playerStats!(idx, 13);

    Locate , 62
    If playerStats!(idx, 0) = 0 Then 
        Print "  --.-";
    End If
    If playerStats!(idx, 0) > 0 Then 
        Print Using "####.#"; (playerStats!(idx, 3) * 2) + (playerStats!(idx, 5) + playerStats!(idx, 13)) / playerStats!(idx, 0);
    End If

    Locate , 68
    If playerStats!(idx, 0) = 0 Then 
        Print "   --.-";
    End If
    If playerStats!(idx, 0) > 0 Then 
        Print Using "#####.#"; playerStats!(idx, 1) / playerStats!(idx, 0);
    End If

    Color 7, 0: Print String$(2, 177)
    Color 14, 11

    Locate 6, 14: Print Using "###"; statsA!(idx, 0);

    Locate , 17
    If statsA!(idx, 10) = 0 Then 
        Print "    0";
    End If
    If statsA!(idx, 10) > 0 Then 
        Print Using "#####"; (statsA!(idx, 10) + 30) / 60;
    End If

    Locate , 22: Print Using "#####"; statsA!(idx, 2);
    Locate , 27: Print Using "#####"; statsA!(idx, 1);

    Locate , 32
    If statsA!(idx, 2) = 0 Then 
        Print "    0.0%";
    End If
    If statsA!(idx, 2) > 0 Then
        Print Using "#####.#%"; (statsA!(idx, 2) / statsA!(idx, 1)) * 100;
    End If

    Locate 6, 40: Print Using "#####"; statsA!(idx, 4);
    Locate 6, 45: Print Using "#####"; statsA!(idx, 3);

    Locate , 50
    If statsA!(idx, 4) = 0 Then 
        Print "    0.0%";
    End If
    If statsA!(idx, 4) > 0 Then
        Print Using "#####.#%"; statsA!(idx, 4) / statsA!(idx, 3) * 100;
    End If

    Locate , 58: Print Using "#######"; statsA!(idx, 12);

    Locate , 65
    If statsA!(idx, 0) = 0 Or statsA!(idx, 12) = 0 Or (statsA!(idx, 0) > 0 And statsA!(idx, 12) = 0) Then 
        Print "      0.0";
    End If
    If statsA!(idx, 0) > 0 And statsA!(idx, 12) > 0 Then 
        Print Using "#######.#"; statsA!(idx, 12) / statsA!(idx, 0);
    End If

    Color 7, 0: Print String$(2, 177)
    Locate , 16: Print String$(60, 177)

    Color 14, 11
    Locate 11, 14: Print Using "######"; statsA!(idx, 5) - statsA!(idx, 12);

    Locate , 20
    If statsA!(idx, 0) = 0 Or (statsA!(idx, 5) - statsA!(idx, 12)) = 0 Or (statsA!(idx, 0) > 0 And (statsA!(idx, 5) - statsA!(idx, 12)) = 0) Then 
        Print "      0.0";
    End If
    If statsA!(idx, 0) > 0 And (statsA!(idx, 5) - statsA!(idx, 12)) > 0 Then 
        Print Using "#######.#"; (statsA!(idx, 5) - statsA!(idx, 12)) / statsA!(idx, 0);
    End If

    Locate , 29: Print Using "######"; statsA!(idx, 8);

    Locate , 35
    If statsA!(idx, 0) = 0 Or statsA!(idx, 8) = 0 Or (statsA!(idx, 0) > 0 And statsA!(idx, 8) = 0) Then 
        Print "     0.0";
    End If
    If statsA!(idx, 0) > 0 And statsA!(idx, 8) > 0 Then 
        Print Using "######.#"; statsA!(idx, 8) / statsA!(idx, 0);
    End If

    Locate , 43: Print Using "#######"; statsA!(idx, 6);

    Locate , 50
    If statsA!(idx, 0) = 0 Or statsA!(idx, 6) = 0 Or (statsA!(idx, 0) > 0 And statsA!(idx, 6) = 0) Then 
        Print "  0.0";
    End If
    If statsA!(idx, 0) > 0 And statsA!(idx, 6) > 0 Then 
        Print Using "###.#"; statsA!(idx, 6) / statsA!(idx, 0);
    End If

    Locate , 55: Print Using "######"; statsA!(idx, 5);
    Locate , 61: Print Using "#####"; statsA!(idx, 7);

    Locate , 66
    If statsA!(idx, 0) = 0 Or statsA!(idx, 7) = 0 Or (statsA!(idx, 0) > 0 And statsA!(idx, 7) = 0) Then 
        Print "    0.0";
    End If
    If statsA!(idx, 0) > 0 And statsA!(idx, 7) > 0 Then 
        Print Using "#####.#"; statsA!(idx, 7) / statsA!(idx, 0);
    End If

    Color 7, 0: Print String$(2, 177)
    Locate , 16: Print String$(59, 177)

    Color 14, 11
    Locate 16, 14: Print Using "####"; statsA!(idx, 9);

    Locate , 18
    If statsA!(idx, 0) = 0 Or statsA!(idx, 9) = 0 Or (statsA!(idx, 0) > 0 And statsA!(idx, 9) = 0) Then 
        Print "    0.0";
    End If
    If statsA!(idx, 0) > 0 And statsA!(idx, 9) > 0 Then 
        Print Using "#####.#"; statsA!(idx, 9) / statsA!(idx, 0);
    End If

    Locate , 25: Print Using "#######"; statsW0!(idx);
    Locate , 32: Print Using "#######"; statsW1!(idx);

    Locate , 39
    If statsW0!(idx) = 0 Then 
        Print "      0.0%";
    End If
    If statsW0!(idx) > 0 Then
        Print Using "#######.#%"; (statsW0!(idx) / statsW1!(idx)) * 100;
    End If

    statVal = statsA!(idx, 5) / statsA!(idx, 0)
    Locate , 49: Print Using "#####.#"; statVal;
    Locate , 56: Print Using "######"; (statsA!(idx, 2) * 2) + statsA!(idx, 4) + statsW0!(idx);

    Locate , 62
    If (statsA!(idx, 2) * 2) + statsA!(idx, 4) + statsW0!(idx) = 0 Then 
        Print "   0.0";
    End If
    If (statsA!(idx, 2) * 2) + statsA!(idx, 4) + statsW0!(idx) > 0 Then 
        Print Using "####.#"; ((statsA!(idx, 2) * 2) + statsA!(idx, 4) + statsW0!(idx)) / statsA!(idx, 0);
    End If

    Locate , 68
    If statsA!(idx, 10) = 0 Then 
        Print "    0.0";
    End If
    If statsA!(idx, 10) > 0 Then 
        Print Using "#####.#"; ((statsA!(idx, 10) + 30) / 60) / statsA!(idx, 0);
    End If

    Color 7, 0: Print String$(2, 177)
    Locate , 16: Print String$(61, 177)

End Sub


'----------------------------------------
'   Comparisons_Report Subroutine
'----------------------------------------
'This subroutine focuses on the formatting
'of team stats for comparison. The stats
'are output to the end user display
Sub Comparisons_Report (printDest, destFile$, teamName$, posn$())

    Shared replayLosses, replayWins

    Shared teamRatings%(), teamStats!()

    Shared playerStats!()

    Shared statsA!()
    Shared statsZ!(), statsZ1!()
    Shared statsW0!(), statsW1!()

    Shared statPlyrNames$()

    Color 15, 0
    Cls

    '--------------------
    '  Team Comparisons
    '--------------------

    Open destFile$ For Output As #1

    Print #1,
    Print #1, Tab(33); teamName$
    Print #1,

    'Locate 2
    Print #1, Tab(3); "   OFFENSIVE STATS    ";
    Print #1, Tab(55); "   DEFENSIVE STATS   "
    Print #1,

    'Locate 3
    Print #1, Tab(3); "       ACTUAL   REPLAY";
    Print #1, Tab(55); "       ACTUAL  REPLAY"

    'Locate 4
    Print #1, Tab(3); " FGM   ";
    Print #1, Tab(16); Using "#########"; statsZ!(2);

    Print #1, Tab(55); " FGM   ";
    Print #1, Tab(62); Using "######"; teamStats!(2) / 100 * teamStats!(4);
    Print #1, Tab(68); Using "########"; statsZ1!(2)

    'Locate 5
    Print #1, Tab(3); " FGA   ";
    Print #1, Tab(16); Using "#########"; statsZ!(1);

    Print #1, Tab(30); "              W - L ";

    Print #1, Tab(55); " FGA   ";
    Print #1, Tab(62); Using "######"; teamStats!(2);
    Print #1, Tab(68); Using "########"; statsZ1!(1)

    'Locate 6
    Print #1, Tab(3); " FG PCT";

    If teamStats!(1) = 0 Then 
        Print #1, Tab(10); "  .000";
    End If
    If teamStats!(1) > 0 Then
        statPCT! = teamStats!(3) / 100
        Print #1, Tab(10); "  "; Using ".###"; statPCT!;
    End If

    If statsZ!(1) = 0 Then 
        Print #1, Tab(16); "     .000";
    End If
    If statsZ!(1) > 0 Then
        statPCT! = (statsZ!(2) / statsZ!(1))
        Print #1, Tab(16); "     "; Using ".###"; statPCT!;
    End If

    Print #1, Tab(30); "  ACTUAL   ";
    Print #1, Tab(41); Using "####"; teamStats!(7);
    Print #1, Tab(45); Using "####"; teamStats!(8);

    Print #1, Tab(55); " FG PCT";

    If teamStats!(2) = 0 Then
        Print #1, Tab(62); "  .000";
    End If
    If teamStats!(2) > 0 Then
        statPCT! = teamStats!(4) / 100
        Print #1, Tab(62); "  "; Using ".###"; statPCT!;
    End If

    If statsZ1!(1) = 0 Then
        Print #1, Tab(68); "    .000"
    End If
    If statsZ1!(1) > 0 Then
        statPCT! = (statsZ1!(2) / statsZ1!(1))
        Print #1, Tab(68); "    "; Using ".###"; statPCT!
    End If

    'Locate 7
    Print #1, Tab(3); " FTM   ";
    Print #1, Tab(10); Using "######"; teamStats!(5) / 100 * teamStats!(7);
    Print #1, Tab(16); Using "#########"; statsZ!(4);

    Print #1, Tab(30); "  REPLAY   ";
    Print #1, Tab(41); Using "####"; replayWins;
    Print #1, Tab(45); Using "####"; replayLosses;

    Print #1, Tab(55); " FTM   ";
    Print #1, Tab(62); Using "######"; teamStats!(6) / 100 * teamStats!(8);
    Print #1, Tab(68); Using "########"; statsZ1!(4)

    'Locate 8
    Print #1, Tab(3); " FTA   ";
    Print #1, Tab(10); Using "######"; teamStats!(5);
    Print #1, Tab(16); Using "#########"; statsZ!(3);

    Print #1, Tab(55); " FTA   ";
    Print #1, Tab(62); Using "######"; teamStats!(6);
    Print #1, Tab(68); Using "########"; statsZ1!(3)

    'Locate 9
    Print #1, Tab(3); " FT PCT";
    If teamStats!(5) = 0 Then
        Print #1, Tab(10); "  .000";
    End If
    If teamStats!(5) > 0 Then
        statPCT! = teamStats!(7) / 100
        Print #1, Tab(10); "  "; Using ".###"; statPCT!;
    End If

    If statsZ!(3) = 0 Then
        Print #1, Tab(16); "     .000";
    End If
    If statsZ!(3) > 0 Then
        statPCT! = (statsZ!(4) / statsZ!(3))
        Print #1, Tab(16); "     "; Using ".###"; statPCT!;
    End If

    Print #1, Tab(55); " FT PCT";

    If teamStats!(6) = 0 Then
        Print #1, Tab(62); "  .000";
    End If
    If teamStats!(6) > 0 Then
        statPCT! = teamStats!(8) / 100
        Print #1, Tab(62); "  "; Using ".###"; statPCT!;
    End If

    If statsZ1!(3) = 0 Then
        Print #1, Tab(68); "    .000"
    End If
    If statsZ1!(3) > 0 Then
        statPCT! = (statsZ1!(4) / statsZ1!(3))
        Print #1, Tab(68); "    "; Using ".###"; statPCT!
    End If

    'Locate 10
    Print #1, Tab(3); " O.RB  ";
    Print #1, Tab(10); Using "######"; .35 * teamStats!(9);
    Print #1, Tab(16); Using "#########"; statsZ!(12);

    Print #1, Tab(55); " O.RB  ";
    Print #1, Tab(62); Using "######"; .35 * teamStats!(10);
    Print #1, Tab(68); Using "########"; statsZ1!(12)

    'Locate 11
    Print #1, Tab(3); " D.RB  ";
    Print #1, Tab(10); Using "######"; .65 * teamStats!(9);
    Print #1, Tab(16); Using "#########"; statsZ!(5) - statsZ!(12) - statsZ!(15);

    Print #1, Tab(55); " D.RB  ";
    Print #1, Tab(62); Using "######"; .65 * teamStats!(10);
    Print #1, Tab(68); Using "########"; statsZ1!(5) - statsZ1!(12) - statsZ1!(15);

    'Locate 12
    Print #1, Tab(3); " T.REB ";
    Print #1, Tab(10); Using "######"; teamStats!(9);
    Print #1, Tab(16); Using "#########"; statsZ!(5) - statsZ!(15);

    Print #1, Tab(55); " T.REB ";
    Print #1, Tab(62); Using "######"; teamStats!(10);
    Print #1, Tab(68); Using "########"; statsZ1!(5) - statsZ1!(15)

    'Locate 13
    Print #1, Tab(3); " ASS'T ";
    Print #1, Tab(10); Using "######"; teamStats!(11);
    Print #1, Tab(16); Using "#########"; statsZ!(8);

    Print #1, Tab(55); " ASS'T ";
    Print #1, Tab(62); Using "######"; teamStats!(12);
    Print #1, Tab(68); Using "########"; statsZ1!(8)

    'Locate 14
    Print #1, Tab(3); " FLS   ";
    Print #1, Tab(10); Using "######"; teamStats!(13);
    Print #1, Tab(16); Using "#########"; statsZ!(6);

    Print #1, Tab(55); " FLS   ";
    Print #1, Tab(62); Using "######"; teamStats!(14);
    Print #1, Tab(68); Using "########"; statsZ1!(6)

    'Locate 15
    Print #1, Tab(3); "  ST   ";
    Print #1, Tab(10); Using "######"; teamStats!(17);
    Print #1, Tab(16); Using "#########"; statsZ!(7);

    Print #1, Tab(55); "  ST   ";
    Print #1, Tab(62); Using "######"; teamStats!(18);
    Print #1, Tab(68); Using "########"; statsZ1!(7)

    'Locate 16
    Print #1, Tab(3); "  TO   ";
    Print #1, Tab(10); Using "######"; teamStats!(15);
    Print #1, Tab(16); Using "#########"; statsZ!(11);

    Print #1, Tab(55); "  TO   ";
    Print #1, Tab(62); Using "######"; teamStats!(16);
    Print #1, Tab(68); Using "########"; statsZ1!(11)

    'Locate 17
    Print #1, Tab(3); " BLK   ";
    Print #1, Tab(10); Using "######"; teamStats!(21);
    Print #1, Tab(16); Using "#########"; statsZ!(9);

    Print #1, Tab(55); " BLK   ";
    Print #1, Tab(62); Using "######"; teamStats!(22);
    Print #1, Tab(68); Using "########"; statsZ1!(9)

    'Locate 18
    Print #1, Tab(3); " 3-FGM ";
    Print #1, Tab(10); Using "######"; teamStats!(23) * teamStats!(24) / 100;
    Print #1, Tab(16); Using "#########"; statsZ!(13);

    Print #1, Tab(55); " 3-FGM ";
    Print #1, Tab(62); Using "######"; teamStats!(19) * teamStats!(20) / 100;
    Print #1, Tab(68); Using "########"; statsZ1!(13)

    'Locate 19
    Print #1, Tab(3); " 3-FGA ";
    Print #1, Tab(10); Using "######"; teamStats!(23);
    Print #1, Tab(16); Using "#########"; statsZ!(14);

    Print #1, Tab(55); " 3-FGA ";
    Print #1, Tab(62); Using "######"; teamStats!(19);
    Print #1, Tab(68); Using "########"; statsZ1!(14)

    'Actual
    Print #1, Tab(3); " 3-PCT ";

    If teamStats!(23) = 0 Then
        Print #1, Tab(10); "  .000";
    End If
    If teamStats!(23) > 0 Then
        statPCT! = teamStats!(24) / 100
        Print #1, Tab(10); "  "; Using ".###"; statPCT!;
    End If

    If statsZ!(13) = 0 Then
        Print #1, Tab(16); "     .000";
    End If
    If statsZ!(13) > 0 Then
        statPCT! = (statsZ!(13) / statsZ!(14))
        Print #1, Tab(16); "     "; Using ".###"; statPCT!;
    End If

    'Replay
    Print #1, Tab(55); " 3-PCT ";

    If teamStats!(9) = 0 Then
        Print #1, Tab(62); "  .000";
    End If
    If teamStats!(9) > 0 Then
        statPCT! = teamStats!(20) / 100
        Print #1, Tab(62); "  "; Using ".###"; statPCT!;
    End If

    If statsZ1!(13) = 0 Then
        Print #1, Tab(68); "    .000"
    End If
    If statsZ1!(13) > 0 Then
        statPCT! = (statsZ1!(13) / statsZ1!(14))
        Print #1, Tab(68); Using "    .###"; statPCT!
    End If

    Print #1, Chr$(12)


    '--------------------
    ' Player Comparisons
    '--------------------

    playerIdx = 0

    For playerIdx = 0 To 14

        If statPlyrNames$(playerIdx) <> "XXX" And statPlyrNames$(playerIdx) <> "END" And statPlyrNames$(playerIdx) <> "" Then

            If playerIdx Mod 3 = 0 Then
                Print #1,
                Print #1, Tab(22); "STAT COMPARISONS FOR "; teamName$
                Print #1,
            End If

            'Player Name
            Print #1, statPlyrNames$(idx); " - "; posn$(idx)
            Print #1,

            Print #1, Tab(14); "  G ";
            Print #1, Tab(18); " MIN ";
            Print #1, Tab(23); " FGM ";
            Print #1, Tab(28); " FGA ";
            Print #1, Tab(33); " FG-PCT ";
            Print #1, Tab(41); " FTM ";
            Print #1, Tab(46); " FTA ";
            Print #1, Tab(51); " FT-PCT ";
            Print #1, Tab(59); " O.REB ";
            Print #1, Tab(66); " O.REB/G"

            'Locate 5
            Print #1, " ACTUAL  ";
            Print #1, Tab(14); Using "###"; playerStats!(idx, 0);
            Print #1, Tab(17); Using "#####"; playerStats!(idx, 1);
            Print #1, Tab(22); Using "#####"; playerStats!(idx, 3);
            Print #1, Tab(27); Using "#####"; playerStats!(idx, 2);

            Print #1, Tab(32);
            If playerStats!(idx, 2) = 0 Then
                Print #1, "   --.- %";
            End If
            If playerStats!(idx, 2) > 0 Then
                Print #1, Using "#####.#%"; (playerStats!(idx, 3) / playerStats!(idx, 2)) * 100;
            End If

            Print #1, Tab(40); Using "#####"; playerStats!(idx, 5);
            Print #1, Tab(45); Using "#####"; playerStats!(idx, 4);

            Print #1, Tab(50);
            If playerStats!(idx, 4) = 0 Then
                Print #1, "   --.- %";
            End If
            If playerStats!(idx, 4) > 0 Then
                Print #1, Using "#####.#%"; (playerStats!(idx, 5) / playerStats!(idx, 4)) * 100;
            End If

            Print #1, Tab(58); Using "#######"; playerStats!(idx, 6);

            Print #1, Tab(65);
            If playerStats!(idx, 0) = 0 Or playerStats!(idx, 6) = 0 Or (playerStats!(idx, 0) > 0 And playerStats!(idx, 6) = 0) Then 
                Print #1, "      0.0"
            End If
            If playerStats!(idx, 0) > 0 And playerStats!(idx, 6) > 0 Then 
                Print #1, Using "#######.#"; playerStats!(idx, 6) / playerStats!(idx, 0)
            End If

            '-----

            'Locate 6
            Print #1, " REPLAY  ";
            Print #1, Tab(14); Using "###"; statsA!(idx, 0);

            Print #1, Tab(17);
            If statsA!(idx, 10) = 0 Then 
                Print #1, "    0";
            End If
            If statsA!(idx, 10) > 0 Then 
                Print #1, Using "#####"; (statsA!(idx, 10) + 30) / 60;
            End If

            Print #1, Tab(22); Using "#####"; statsA!(idx, 2);
            Print #1, Tab(27); Using "#####"; statsA!(idx, 1);

            Print #1, Tab(32);
            If statsA!(idx, 2) = 0 Then
                Print #1, "    0.0%";
            End If
            If statsA!(idx, 2) > 0 Then
                Print #1, Using "#####.#"; (statsA!(idx, 2) / statsA!(idx, 1)) * 100;
                Print #1, "%";
            End If

            Print #1, Tab(40); Using "#####"; statsA!(idx, 4);
            Print #1, Tab(45); Using "#####"; statsA!(idx, 3);

            Print #1, Tab(50);
            If statsA!(idx, 4) = 0 Then
                Print #1, "    0.0%";
            End If
            If statsA!(idx, 4) > 0 Then
                Print #1, Using "#####.#%"; statsA!(idx, 4) / statsA!(idx, 3) * 100;
            End If

            Print #1, Tab(58); Using "#######"; statsA!(idx, 12);

            Print #1, Tab(65);
            If statsA!(idx, 0) = 0 Or statsA!(idx, 12) = 0 Or (statsA!(idx, 0) > 0 And statsA!(idx, 12) = 0) Then 
                Print #1, "      0.0";
            End If
            If statsA!(idx, 0) > 0 And statsA!(idx, 12) > 0 Then 
                Print #1, Using "#######.#"; statsA!(idx, 12) / statsA!(idx, 0);
            End If

            Print #1,
            Print #1,

            '-----

            Print #1, Tab(14); " D.REB ";
            Print #1, Tab(21); " D.REB/G ";
            Print #1, Tab(30); " ASST ";
            Print #1, Tab(36); " ASST/G ";
            Print #1, Tab(44); " FOULS ";
            Print #1, Tab(51); " F/G ";
            Print #1, Tab(56); " TREB ";
            Print #1, Tab(62); " STL ";
            Print #1, Tab(67); " STL/G"

            '-----

            'Locate 10
            Print #1, " ACTUAL ";

            'Def Reb
            Print #1, Tab(14); Using "######"; playerStats!(idx, 7) - playerStats!(idx, 6);

            'Def Reb / Game
            Print #1, Tab(20);
            If playerStats!(idx, 0) = 0 Or playerStats!(idx, 7) - playerStats!(idx, 6) = 0 Or (playerStats!(idx, 0) > 0 And playerStats!(idx, 7) - playerStats!(idx, 6) = 0) Then
                Print #1, "      0.0";
            End If
            If playerStats!(idx, 0) > 0 And playerStats!(idx, 7) - playerStats!(idx, 6) > 0 Then 
                Print #1, Using "#######.#"; (playerStats!(idx, 7) - playerStats!(idx, 6)) / playerStats!(idx, 0);
            End If

            'Assist
            Print #1, Tab(29); Using "######"; playerStats!(idx, 9);

            'Assist / Game
            Print #1, Tab(35);
            If playerStats!(idx, 0) = 0 Or playerStats!(idx, 9) = 0 Or (playerStats!(idx, 0) > 0 And playerStats!(idx, 9) = 0) Then 
                Print #1, "     0.0";
            End If
            If playerStats!(idx, 0) > 0 And playerStats!(idx, 9) > 0 Then 
                Print #1, Using "######.#"; playerStats!(idx, 9) / playerStats!(idx, 0);
            End If

            Print #1, Tab(43); Using "#######"; playerStats!(idx, 8);

            Print #1, Tab(50);
            If playerStats!(idx, 0) = 0 Or playerStats!(idx, 8) = 0 Or (playerStats!(idx, 0) > 0 And playerStats!(idx, 8) = 0) Then 
                Print #1, "  0.0";
            End If
            If playerStats!(idx, 0) > 0 And playerStats!(idx, 8) > 0 Then 
                Print #1, Using "###.#"; playerStats!(idx, 8) / playerStats!(idx, 0);
            End If

            Print #1, Tab(55);
            Print #1, Using "######"; playerStats!(idx, 7);
            Print #1, Tab(61);
            Print #1, Using "#####"; playerStats!(idx, 11);

            Print #1, Tab(66);
            If playerStats!(idx, 0) = 0 Or playerStats!(idx, 11) = 0 Or (playerStats!(idx, 0) > 0 And playerStats!(idx, 11) = 0) Then 
                Print #1, "    0.0"
            End If
            If playerStats!(idx, 0) > 0 And playerStats!(idx, 11) > 0 Then 
                Print #1, Using "#####.#"; playerStats!(idx, 11) / playerStats!(idx, 0)
            End If

            '-----

            'Locate 11
            Print #1, " REPLAY  ";
            Print #1, Tab(14); Using "######"; statsA!(idx, 5) - statsA!(idx, 12);

            Print #1, Tab(20);
            If statsA!(idx, 0) = 0 Or (statsA!(idx, 5) - statsA!(idx, 12)) = 0 Or (statsA!(idx, 0) > 0 And (statsA!(idx, 5) - statsA!(idx, 12)) = 0) Then 
                Print #1, "      0.0";
            End If
            If statsA!(idx, 0) > 0 And (statsA!(idx, 5) - statsA!(idx, 12)) > 0 Then 
                Print #1, Using "#######.#"; (statsA!(idx, 5) - statsA!(idx, 12)) / statsA!(idx, 0);
            End If

            Print #1, Tab(29);
            Print #1, Using "######"; statsA!(idx, 8);

            Print #1, Tab(35);
            If statsA!(idx, 0) = 0 Or statsA!(idx, 8) = 0 Or (statsA!(idx, 0) > 0 And statsA!(idx, 8) = 0) Then 
                Print #1, "     0.0";
            End If
            If statsA!(idx, 0) > 0 And statsA!(idx, 8) > 0 Then 
                Print #1, Using "######.#"; statsA!(idx, 8) / statsA!(idx, 0);
            End If

            Print #1, Tab(43);
            Print #1, Using "#######"; statsA!(idx, 6);

            Print #1, Tab(50);
            If statsA!(idx, 0) = 0 Or statsA!(idx, 6) = 0 Or (statsA!(idx, 0) > 0 And statsA!(idx, 6) = 0) Then 
                Print #1, "  0.0";
            End If
            If statsA!(idx, 0) > 0 And statsA!(idx, 6) > 0 Then 
                Print #1, Using "###.#"; statsA!(idx, 6) / statsA!(idx, 0);
            End If

            Print #1, Tab(55);
            Print #1, Using "######"; statsA!(idx, 5);
            Print #1, Tab(61);
            Print #1, Using "#####"; statsA!(idx, 7);

            Print #1, Tab(66);
            If statsA!(idx, 0) = 0 Or statsA!(idx, 7) = 0 Or (statsA!(idx, 0) > 0 And statsA!(idx, 7) = 0) Then 
                Print #1, "    0.0"
            End If
            If statsA!(idx, 0) > 0 And statsA!(idx, 7) > 0 Then 
                Print #1, Using "#####.#"; statsA!(idx, 7) / statsA!(idx, 0);
            End If

            '-----

            Print #1,
            Print #1,

            'Locate 14
            Print #1, Tab(14); " BLK ";
            Print #1, Tab(19); " BLK/G ";
            Print #1, Tab(26); " 3-FGM ";
            Print #1, Tab(33); " 3-FGA ";
            Print #1, Tab(40); " 3-FG-PCT ";
            Print #1, Tab(50); "  RB/G ";
            Print #1, Tab(57); "  PTS";
            Print #1, Tab(62); " PTS/G ";
            Print #1, Tab(69); " MIN/G"

            '-----

            'Locate 15
            Print #1, " ACTUAL  ";
            Print #1, Tab(14); Using "####"; playerStats!(idx, 10);

            Print #1, Tab(18);
            If playerStats!(idx, 0) = 0 Or playerStats!(idx, 10) = 0 Or (playerStats!(idx, 0) > 0 And playerStats!(idx, 10) = 0) Then 
                Print #1, "    0.0";
            End If
            If playerStats!(idx, 0) > 0 And playerStats!(idx, 12) > 0 Then 
                Print #1, Using "#####.#"; playerStats!(idx, 10) / playerStats!(idx, 0);
            End If

            Print #1, Tab(25);
            Print #1, Using "#######"; playerStats!(idx, 13);

            Print #1, Tab(32);
            Print #1, Using "#######"; playerStats!(idx, 12);

            Print #1, Tab(39);
            If playerStats!(idx, 13) = 0 Then
                Print #1, "     --.- %";
            End If
            If playerStats!(idx, 12) > 0 Then
                Print #1, Using "#######.#%"; (playerStats!(idx, 13) / playerStats!(idx, 12)) * 100;
            End If

            Print #1, Tab(49);
            If playerStats!(idx, 0) <> 0 Then 
                Print #1, Using "   ##.#"; playerStats!(idx, 7) / playerStats!(idx, 0);
            End If
            Print #1, Tab(56);
            Print #1, Using "######"; (playerStats!(idx, 3) * 2) + playerStats!(idx, 5) + playerStats!(idx, 13);

            Print #1, Tab(62);
            If playerStats!(idx, 0) = 0 Then 
                Print #1, "  --.-";
            End If
            If playerStats!(idx, 0) > 0 Then 
                Print #1, Using "####.#"; (playerStats!(idx, 3) * 2) + (playerStats!(idx, 5) + playerStats!(idx, 13)) / playerStats!(idx, 0);
            End If

            Print #1, Tab(68);
            If playerStats!(idx, 0) = 0 Then 
                Print #1, "   --.-"
            End If
            If playerStats!(idx, 0) > 0 Then 
                Print #1, Using "#####.#"; playerStats!(idx, 1) / playerStats!(idx, 0)
            End If

            '-----

            'Locate 16
            Print #1, " REPLAY  ";
            'Blocks
            Print #1, Tab(14); Using "####"; statsA!(idx, 9);

            'Blocks / Game
            Print #1, Tab(18);
            If statsA!(idx, 0) = 0 Or statsA!(idx, 9) = 0 Or (statsA!(idx, 0) > 0 And statsA!(idx, 9) = 0) Then 
                Print #1, "    0.0";
            End If
            If statsA!(idx, 0) > 0 And statsA!(idx, 9) > 0 Then 
                Print #1, Using "#####.#"; statsA!(idx, 9) / statsA!(idx, 0);
            End If

            '3 FGM
            Print #1, Tab(25);
            Print #1, Using "#######"; statsW0!(I);

            '3 FGA
            Print #1, Tab(32);
            Print #1, Using "#######"; statsW1!(I);

            Print #1, Tab(39);
            If statsW0!(I) = 0 Then
                Print #1, "      0.0%";
            End If
            If statsW0!(I) > 0 Then
                Print #1, Using "#######.#%"; (statsW0!(I) / statsW1!(I)) * 100;
            End If

            statVal = statsA!(idx, 5) / statsA!(idx, 0)
            Print #1, Tab(49);
            Print #1, Using "#####.#"; statVal;
            Print #1, Tab(56);
            Print #1, Using "######"; (statsA!(idx, 2) * 2) + statsA!(idx, 4) + statsW0!(I);

            Print #1, Tab(62);
            If (statsA!(idx, 2) * 2) + statsA!(idx, 4) + statsW0!(I) = 0 Then 
                Print #1, "   0.0";
            End If
            If (statsA!(idx, 2) * 2) + statsA!(idx, 4) + statsW0!(I) > 0 Then 
                Print #1, Using "####.#"; ((statsA!(idx, 2) * 2) + statsA!(idx, 4) + statsW0!(I)) / statsA!(idx, 0);
            End If

            Print #1, Tab(68);
            If statsA!(idx, 10) = 0 Then 
                Print #1, "    0.0"
            End If
            If statsA!(idx, 10) > 0 Then 
                Print #1, Using "#####.#"; ((statsA!(idx, 10) + 30) / 60) / statsA!(idx, 0)
            End If

            Print #1,
            Print #1,

            If playerIdx Mod 3 = 2 Then
                Print #1, Chr$(12)
            End If

        End If 'done checking for null player

    Next playerIdx

    Close #1

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
