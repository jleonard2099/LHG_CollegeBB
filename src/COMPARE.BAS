'----------------------------------------
'           COMPARE Subroutine
'----------------------------------------
'This subroutine loads in existing stats
'for a team and provides different options 
'to the end user for formatting stats 
Sub COMPARE ()

    _Title "Courtside College Basketball - Stats Comparison"

    Shared teamRatings%(), HP%(), OP%()

    Shared compareA!(), statsW0!(), statsW1!()
    Shared statsZ!(), statsZ1!()

    Shared statPlyrNames$(), HP$(), OP$()
    Shared position$(), T$()

    Dim posn$(0 To 13)
    
    Call ChooseTeam(YN$, NTMS, teamNbr, team$)
    statFile$ = nameStatFile$(team$)

    If Not _FileExists(diskPaths$(1) + statFile$) Then
        MsgText$ = "NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU"
        BoxWidth = 24
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)
    Else

        Open diskPaths$(1) + statFile$ For Input As #1

        'We don't actually use T$
        Input #1, T$

        For I = 0 To 13:

            Input #1, statPlyrNames$(I)

            For J = 0 To 13
                Input #1, compareA!(I, J)
            Next

            Input #1, statsW0!(I)

            Input #1, statsW1!(I)

        Next

        For L = 0 To 15: Input #1, statsZ!(L): Next

        For L = 0 To 15: Input #1, statsZ1!(L): Next

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

        Input #2, Y

        For J = 1 To NUM_STATRECORDS: Input #2, OP%(J): Next

        For J = 1 To NUM_STATRECORDS: Input #2, OP$(J): Next

        For J = 1 To NUM_STATRECORDS: Input #2, HP%(J): Next

        For J = 1 To NUM_STATRECORDS: Input #2, HP$(J): Next

        Close #2

        replayLosses% = 0
        replayWins% = 0

        For I = 1 To Y
            If OP%(I) < HP%(I) Then replayLosses% = replayLosses% + 1
            If OP%(I) > HP%(I) Then replayWins% = replayWins% + 1
        Next

        Color 15, 0
        Cls

        JJ = 4

        Locate 8, 8: Color 14, 1

        Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";

        For II = 1 To JJ:
            Locate 8 + II, 8
            Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
        Next

        Locate 9 + JJ, 8

        Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";

        Do
            Locate 10
            Color 7, 1

            Locate , 30: Print "STATS TO ";
            Color 15: Print "S";: Color 7: Print "CREEN"
            Locate , 30: Print "STATS TO ";
            Color 15: Print "P";: Color 7: Print "RINTER"
            J$ = GetKeyPress$

        Loop Until UCase$(J$) = "P" Or UCase$(J$) = "S" Or J$ = Chr$(27)

        backToMenu = 0

        If J$ <> Chr$(27) Then

            Select Case UCase$(J$)

                Case "P":
                    Call Comparisons_Printer(team$)

                Case "S":
                    Call Comparisons_Screen(team$, backToMenu, posn$())

            End Select

        End If

    End If 'Done checking for ESC

End Sub


'----------------------------------------
'     Comparisons_Printer Subroutine
'----------------------------------------
'This subroutine formats team stats for
'output to the default printer
Sub Comparisons_Printer (teamName$)

End Sub


'----------------------------------------
'     Comparisons_Screen Subroutine
'----------------------------------------
'This subroutine provides a brief menu of
'player comparisons that can be done, and 
'then formats stats for those options for
'use with the user display
Sub Comparisons_Screen (teamName$, backToMenu, posn$())

    Shared teamRatings%(), teamStats()

    Shared compareA!(), playerStats!(), X1!()
    Shared statsW0!(), statsW1!()
    Shared statsZ!(), statsZ1!()

    Shared statPlyrNames$(), T$()

    Shared replayLosses%, replayWins%

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
        Locate , 15: Print "15. ";
        Color 3: Print "View Players In Sequence"
        Color 12
        Locate , 15: Print "16. ";
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

                    Call PlayerComparisons_Screen(playerIdx, posn$(playerIdx), menuExit)

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

                            Call PlayerComparisons_Screen(playerIdx, posn$(playerIdx), menuExit)

                            Color 15, 0: Locate 22: Print "HIT ENTER TO CONTINUE"
                            Locate 22, 23: Print "OR ESC TO RETURN TO MENU" 
                            I$ = GetKeyPress$

                            If I$ = Chr$(27) Then menuExit = 1

                        End If 'done chceking for null player

                        playerIdx = playerIdx + 1

                    Loop Until playerIdx = 14 Or menuExit = 1

                Case 16:
                    backToMenu = 1

                Case 17:
                    Exit Sub

            End Select

        Loop Until B9 >= 1 And B9 <= 17 Or menuExit = 1

    Loop Until B9 = 17 Or backToMenu = 1

    If backToMenu <> 1 Then
        Color 15, 0
        Locate 23, 28: Print "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$
    End If

End Sub


'----------------------------------------
'    TeamComparisons_Screen SubRoutine
'----------------------------------------
'This subroutine focuses on the formatting
'of team stats for comparison. The stats
'are output to the end user display
Sub TeamComparisons_Screen (T$)

    Shared teamRatings%(), teamStats()
    Shared statsZ!(), statsZ1!()

    Shared replayLosses%, replayWins%

    Locate 11, 31: Print Space$(18)

    Color 11
    Locate 11, 33: Print T$

    'Color 15, 12
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

    For I = 3 To 21: Locate I, 25: Color 7, 0: Print String$(2, 177): Next
    For I = 3 To 21: Locate I, 76: Print String$(2, 177): Next

    Locate 21, 5: Print String$(20, 177)
    Locate 21, 57: Print String$(20, 177)

    For I = 6 To 8: Locate I, 50: Print String$(2, 177): Next

    Locate 8, 32: Print String$(18, 177)

    Color 3, 0
    Locate 6, 41: Print Using "####"; teamRatings%(7);
    Locate 6, 45: Print Using "####"; teamRatings%(8);: Print " ";

    Color 14, 0
    Locate 7, 41: Print Using "####"; replayWins%;
    Locate 7, 45: Print Using "####"; replayLosses%;: Print " ";

    Color 3, 0
    Locate 4, 10: Print Using "######"; teamStats(1) / 100 * teamStats(3);
    Locate 5, 10: Print Using "######"; teamStats(1);

    If teamStats(1) = 0 Then Locate 6, 10: Print "  .000";
    If teamStats(1) > 0 Then statPCT! = teamStats(3) / 100: Locate 6, 10: Print "  ";: Print Using ".###"; statPCT!;

    Locate 7, 10: Print Using "######"; teamStats(5) / 100 * teamStats(7);
    Locate 8, 10: Print Using "######"; teamStats(5);

    If teamStats(5) = 0 Then Locate 9, 10: Print "  .000";
    If teamStats(5) > 0 Then statPCT! = teamStats(7) / 100: Locate 9, 10: Print "  ";: Print Using ".###"; statPCT!;

    Locate 10, 10: Print Using "######"; .35 * teamStats(9);
    Locate 11, 10: Print Using "######"; .65 * teamStats(9);
    Locate 12, 10: Print Using "######"; teamStats(9);
    Locate 13, 10: Print Using "######"; teamStats(11);
    Locate 14, 10: Print Using "######"; teamStats(13);
    Locate 15, 10: Print Using "######"; teamStats(17);
    Locate 16, 10: Print Using "######"; teamStats(15);
    Locate 17, 10: Print "   ---"
    Locate 18, 10: Print "   ---"
    Locate 19, 10: Print "   ---"
    Locate 20, 10: Print "   ---"

    Color 3, 0
    Locate 4, 62: Print Using "######"; teamStats(2) / 100 * teamStats(4);
    Locate 5, 62: Print Using "######"; teamStats(2);

    If teamRatings%(2) = 0 Then Locate 6, 62: Print "  .000";
    If teamRatings%(2) > 0 Then statPCT! = teamStats(4) / 100: Locate 6, 62: Print "  ";: Print Using ".###"; statPCT!;

    Locate 7, 62: Print Using "######"; teamStats(6) / 100 * teamStats(8);
    Locate 8, 62: Print Using "######"; teamStats(6);

    If teamRatings%(6) = 0 Then Locate 9, 62: Print "  .000";
    If teamRatings%(6) > 0 Then statPCT! = teamStats(8) / 100: Locate 9, 62: Print "  ";: Print Using ".###"; statPCT!;

    Locate 10, 62: Print Using "######"; .35 * teamStats(10);
    Locate 11, 62: Print Using "######"; .65 * teamStats(10);
    Locate 12, 62: Print Using "######"; teamStats(10);
    Locate 13, 62: Print Using "######"; teamStats(12);
    Locate 14, 62: Print Using "######"; teamStats(14);
    Locate 15, 62: Print Using "######"; teamStats(18);
    Locate 16, 62: Print Using "######"; teamStats(16);
    Locate 17, 62: Print "   ---"
    Locate 18, 62: Print Using "######"; teamStats(19) * teamStats(20) / 100;
    Locate 19, 62: Print Using "######"; teamStats(19);

    If teamRatings%(9) = 0 Then Locate 20, 62: Print "  .000";
    If teamRatings%(9) > 0 Then statPCT! = teamStats(20) / 100: Locate 20, 62: Print "  ";: Print Using ".###"; statPCT!;

    Locate 4, 16: Print Using "#########"; statsZ!(2);
    Locate 5, 16: Print Using "#########"; statsZ!(1);

    If statsZ!(1) = 0 Then Locate 6, 16: Print "     .000";
    If statsZ!(1) > 0 Then statPCT! = (statsZ!(2) / statsZ!(1)): Locate 6, 16: Print "     ";: Print Using ".###"; statPCT!;

    Locate 7, 16: Print Using "#########"; statsZ!(4);
    Locate 8, 16: Print Using "#########"; statsZ!(3);

    If statsZ!(3) = 0 Then Locate 9, 16: Print "     .000";
    If statsZ!(3) > 0 Then statPCT! = (statsZ!(4) / statsZ!(3)): Locate 9, 16: Print "     ";: Print Using ".###"; statPCT!;

    Locate 10, 16: Print Using "#########"; statsZ!(12);
    Locate 11, 16: Print Using "#########"; statsZ!(5) - statsZ!(12) - statsZ!(15);
    Locate 12, 16: Print Using "#########"; statsZ!(5) - statsZ!(15);
    Locate 13, 16: Print Using "#########"; statsZ!(8);
    Locate 14, 16: Print Using "#########"; statsZ!(6);
    Locate 15, 16: Print Using "#########"; statsZ!(7);
    Locate 16, 16: Print Using "#########"; statsZ!(11);
    Locate 17, 16: Print Using "#########"; statsZ!(9);
    Locate 18, 16: Print Using "#########"; statsZ!(13);
    Locate 19, 16: Print Using "#########"; statsZ!(14);
    Locate 4, 68: Print Using "########"; statsZ1!(2);
    Locate 5, 68: Print Using "########"; statsZ1!(1);

    If statsZ1!(1) = 0 Then Locate 6, 68: Print "    .000";
    If statsZ1!(1) > 0 Then statPCT! = (statsZ1!(2) / statsZ1!(1)): Locate 6, 68: Print "    ";: Print Using ".###"; statPCT!;

    Locate 7, 68: Print Using "########"; statsZ1!(4);
    Locate 8, 68: Print Using "########"; statsZ1!(3);

    If statsZ1!(3) = 0 Then Locate 9, 68: Print "    .000";
    If statsZ1!(3) > 0 Then statPCT! = (statsZ1!(4) / statsZ1!(3)): Locate 9, 68: Print "    ";: Print Using ".###"; statPCT!;

    Locate 10, 68: Print Using "########"; statsZ1!(12);
    Locate 11, 68: Print Using "########"; statsZ1!(5) - statsZ1!(12) - statsZ1!(15);
    Locate 12, 68: Print Using "########"; statsZ1!(5) - statsZ1!(15);
    Locate 13, 68: Print Using "########"; statsZ1!(8);
    Locate 14, 68: Print Using "########"; statsZ1!(6);
    Locate 15, 68: Print Using "########"; statsZ1!(7);
    Locate 16, 68: Print Using "########"; statsZ1!(11);
    Locate 17, 68: Print Using "########"; statsZ1!(9);
    Locate 18, 68: Print Using "########"; statsZ1!(13);
    Locate 19, 68: Print Using "########"; statsZ1!(14);

    If statsZ1!(13) = 0 Then Locate 20, 68: Print "    .000";
    If statsZ1!(13) > 0 Then statPCT! = (statsZ1!(13) / statsZ1!(14)): Locate 20, 68: Print "    ";: Print Using ".###"; statPCT!;

    If statsZ!(13) = 0 Then Locate 20, 16: Print "     .000";
    If statsZ!(13) > 0 Then statPCT! = (statsZ!(13) / statsZ!(14)): Locate 20, 16: Print "     ";: Print Using ".###"; statPCT!;

End Sub


'----------------------------------------
'  PlayerComparisons_Screen Subroutine
'----------------------------------------
'This subroutine focuses on the formatting
'of team stats for comparison. The stats
'are output to the end user display
Sub PlayerComparisons_Screen (idx, position$, menuExit)

    Shared compareA!(), playerStats!()
    Shared statsW0!(), statsW1!(), X1!()

    Shared statPlyrNames$(), T$()

    Color 11
    Locate 2, 2: Print T$(0)
    Color 15, 0
    Locate 2, 14: Print String$(34, 177)

    Color 14

    'Player Name
    Locate 2, 14: Print statPlyrNames$(idx);

    'Player Position
    Print " - ";: Color 11: Print position$
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

    Color 7, 0
    Print String$(2, 177)
    Locate 17, 4
    Print String$(8, 177)

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

    Color 1, 11: Locate 5, 14: Print Using "###"; X1!(idx, 0);
    Locate , 17: Print Using "#####"; playerStats!(idx, 1);
    Locate , 22: Print Using "#####"; playerStats!(idx, 3);
    Locate , 27: Print Using "#####"; playerStats!(idx, 2);

    If playerStats!(idx, 2) = 0 Then Locate , 32: Print "   --.- %";
    If playerStats!(idx, 2) > 0 Then Locate , 32: Print Using "#####.#";  (playerStats!(idx, 3) / playerStats!(idx, 2)) * 100;: Print " %";

    Locate , 40: Print Using "#####"; playerStats!(idx, 5);
    Locate , 45: Print Using "#####"; playerStats!(idx, 4);

    If playerStats!(idx, 4) = 0 Then Locate , 50: Print "   --.-";: Print " %";
    If playerStats!(idx, 4) > 0 Then Locate , 50: Print Using "#####.#";  (playerStats!(idx, 5) / playerStats!(idx, 4)) * 100;: Print " %";

    Locate , 58: Print Using "#######"; playerStats!(idx, 6);

    If playerStats!(idx, 0) = 0 Or playerStats!(idx, 6) = 0 Then Locate , 65: Print "      0.0";
    If playerStats!(idx, 0) > 0 And playerStats!(idx, 6) = 0 Then Locate , 65: Print "      0.0";
    If playerStats!(idx, 0) > 0 And playerStats!(idx, 6) > 0 Then Locate , 65: Print Using "#######.#"; playerStats!(idx, 6) / playerStats!(idx, 0);

    Color 7, 0: Print String$(2, 177)
    Color 1, 11
    Locate 10, 14: Print Using "######"; playerStats!(idx, 7) - playerStats!(idx, 6);

    If playerStats!(idx, 0) = 0 Or playerStats!(idx, 7) - playerStats!(idx, 6) = 0 Then Locate , 20: Print "      0.0";
    If playerStats!(idx, 0) > 0 And playerStats!(idx, 7) - playerStats!(idx, 6) = 0 Then Locate , 20: Print "      0.0";
    If playerStats!(idx, 0) > 0 And playerStats!(idx, 7) - playerStats!(idx, 6) > 0 Then Locate , 20: Print Using "#######.#";  (playerStats!(idx, 7) - playerStats!(idx, 6)) / playerStats!(idx, 0);

    Locate , 29: Print Using "######"; playerStats!(idx, 9);

    If playerStats!(idx, 0) = 0 Or playerStats!(idx, 9) = 0 Then Locate , 35: Print "     0.0";
    If playerStats!(idx, 0) > 0 And playerStats!(idx, 9) = 0 Then Locate , 35: Print "     0.0";
    If playerStats!(idx, 0) > 0 And playerStats!(idx, 9) > 0 Then Locate , 35: Print Using "######.#"; playerStats!(idx, 9) / playerStats!(idx, 0);

    Locate , 43: Print Using "#######"; playerStats!(idx, 8);

    If playerStats!(idx, 0) = 0 Or playerStats!(idx, 8) = 0 Then Locate , 50: Print "  0.0";
    If playerStats!(idx, 0) > 0 And playerStats!(idx, 8) = 0 Then Locate , 50: Print "  0.0";
    If playerStats!(idx, 0) > 0 And playerStats!(idx, 8) > 0 Then Locate , 50: Print Using "###.#"; playerStats!(idx, 8) / playerStats!(idx, 0);

    Locate , 55: Print Using "######"; playerStats!(idx, 7);
    Locate , 61: Print Using "#####"; playerStats!(idx, 11);

    If playerStats!(idx, 0) = 0 Or playerStats!(idx, 11) = 0 Then Locate , 66: Print "    0.0";
    If playerStats!(idx, 0) > 0 And playerStats!(idx, 11) = 0 Then Locate , 66: Print "    0.0";
    If playerStats!(idx, 0) > 0 And playerStats!(idx, 11) > 0 Then Locate , 66: Print Using "#####.#"; playerStats!(idx, 11) / playerStats!(idx, 0);

    Color 7, 0: Print String$(2, 177)
    Color 1, 11
    Locate 15, 14: Print Using "####"; playerStats!(idx, 10);

    If playerStats!(idx, 0) = 0 Or playerStats!(idx, 10) = 0 Then Locate , 18: Print "    0.0";
    If playerStats!(idx, 0) > 0 And playerStats!(idx, 10) = 0 Then Locate , 18: Print "    0.0";
    If playerStats!(idx, 0) > 0 And playerStats!(idx, 12) > 0 Then Locate , 18: Print Using "#####.#"; playerStats!(idx, 10) / playerStats!(idx, 0);

    Locate , 25: Print Using "#######"; playerStats!(idx, 13);
    Locate , 32: Print Using "#######"; playerStats!(idx, 12);

    If playerStats!(idx, 13) = 0 Then Locate , 39: Print "     --.-";: Print " %";
    If playerStats!(idx, 12) > 0 Then Locate , 39: Print Using "#######.#";  (playerStats!(idx, 13) / playerStats!(idx, 12)) * 100;: Print " %";

    Locate , 49: If playerStats!(idx, 0) <> 0 Then Print Using "   ##.#"; playerStats!(idx, 7) / playerStats!(idx, 0);
    Locate , 56: Print Using "######"; (playerStats!(idx, 3) * 2) + playerStats!(idx, 5) + playerStats!(idx, 13);

    If playerStats!(idx, 0) = 0 Then Locate , 62: Print "  --.-";
    If playerStats!(idx, 0) > 0 Then Locate , 62: Print Using "####.#"; (playerStats!(idx, 3) * 2) + (playerStats!(idx, 5) + playerStats!(idx, 13)) / playerStats!(idx, 0);
    If playerStats!(idx, 0) = 0 Then Locate , 68: Print "   --.-";
    If playerStats!(idx, 0) > 0 Then Locate , 68: Print Using "#####.#"; playerStats!(idx, 1) / playerStats!(idx, 0);

    Color 7, 0: Print String$(2, 177)
    Color 14, 11
    Locate 6, 14: Print Using "###"; compareA!(idx, 0);

    If compareA!(idx, 10) = 0 Then Locate , 17: Print "    0";
    If compareA!(idx, 10) > 0 Then Locate , 17: Print Using "#####"; (compareA!(idx, 10) + 30) / 60;

    Locate , 22: Print Using "#####"; compareA!(idx, 2);
    Locate , 27: Print Using "#####"; compareA!(idx, 1);

    If compareA!(idx, 2) = 0 Then Locate , 32: Print "    0.0";: Print "%";
    If compareA!(idx, 2) > 0 Then Locate , 32: Print Using "#####.#"; (compareA!(idx, 2) / compareA!(idx, 1)) * 100;: Print "%";

    Locate 6, 40: Print Using "#####"; compareA!(idx, 4);
    Locate 6, 45: Print Using "#####"; compareA!(idx, 3);

    If compareA!(idx, 4) = 0 Then Locate , 50: Print "    0.0";: Print "%";
    If compareA!(idx, 4) > 0 Then Locate , 50: Print Using "#####.#"; compareA!(idx, 4) / compareA!(idx, 3) * 100;: Print "%";

    Locate , 58: Print Using "#######"; compareA!(idx, 12);

    If compareA!(idx, 0) = 0 Or compareA!(idx, 12) = 0 Then Locate , 65: Print "      0.0";
    If compareA!(idx, 0) > 0 And compareA!(idx, 12) = 0 Then Locate , 65: Print "      0.0";
    If compareA!(idx, 0) > 0 And compareA!(idx, 12) > 0 Then Locate , 65: Print Using "#######.#"; compareA!(idx, 12) / compareA!(idx, 0);

    Color 7, 0: Print String$(2, 177)
    Locate , 16: Print String$(60, 177)
    Color 14, 11
    Locate 11, 14: Print Using "######"; compareA!(idx, 5) - compareA!(idx, 12);

    If compareA!(idx, 0) = 0 Or (compareA!(idx, 5) - compareA!(idx, 12)) = 0 Then Locate , 20: Print "      0.0";
    If compareA!(idx, 0) > 0 And (compareA!(idx, 5) - compareA!(idx, 12)) = 0 Then Locate , 20: Print "      0.0";
    If compareA!(idx, 0) > 0 And (compareA!(idx, 5) - compareA!(idx, 12)) > 0 Then Locate , 20: Print Using "#######.#"; (compareA!(idx, 5) - compareA!(idx, 12)) / compareA!(idx, 0);

    Locate , 29: Print Using "######"; compareA!(idx, 8);

    If compareA!(idx, 0) = 0 Or compareA!(idx, 8) = 0 Then Locate , 35: Print "     0.0";
    If compareA!(idx, 0) > 0 And compareA!(idx, 8) = 0 Then Locate , 35: Print "     0.0";
    If compareA!(idx, 0) > 0 And compareA!(idx, 8) > 0 Then Locate , 35: Print Using "######.#"; compareA!(idx, 8) / compareA!(idx, 0);

    Locate , 43: Print Using "#######"; compareA!(idx, 6);

    If compareA!(idx, 0) = 0 Or compareA!(idx, 6) = 0 Then Locate , 50: Print "  0.0";
    If compareA!(idx, 0) > 0 And compareA!(idx, 6) = 0 Then Locate , 50: Print "  0.0";
    If compareA!(idx, 0) > 0 And compareA!(idx, 6) > 0 Then Locate , 50: Print Using "###.#"; compareA!(idx, 6) / compareA!(idx, 0);

    Locate , 55: Print Using "######"; compareA!(idx, 5);
    Locate , 61: Print Using "#####"; compareA!(idx, 7);

    If compareA!(idx, 0) = 0 Or compareA!(idx, 7) = 0 Then Locate , 66: Print "    0.0";
    If compareA!(idx, 0) > 0 And compareA!(idx, 7) = 0 Then Locate , 66: Print "    0.0";
    If compareA!(idx, 0) > 0 And compareA!(idx, 7) > 0 Then Locate , 66: Print Using "#####.#"; compareA!(idx, 7) / compareA!(idx, 0);

    Color 7, 0: Print String$(2, 177)
    Locate , 16: Print String$(59, 177)
    Color 14, 11
    Locate 16, 14: Print Using "####"; compareA!(idx, 9);

    If compareA!(idx, 0) = 0 Or compareA!(idx, 9) = 0 Then Locate , 18: Print "    0.0";
    If compareA!(idx, 0) > 0 And compareA!(idx, 9) = 0 Then Locate , 18: Print "    0.0";
    If compareA!(idx, 0) > 0 And compareA!(idx, 9) > 0 Then Locate , 18: Print Using "#####.#"; compareA!(idx, 9) / compareA!(idx, 0);

    Locate , 25: Print Using "#######"; statsW0!(I);
    Locate , 32: Print Using "#######"; statsW1!(I);

    If statsW0!(I) = 0 Then Locate , 39: Print "      0.0";: Print "%";
    If statsW0!(I) > 0 Then Locate , 39: Print Using "#######.#"; (statsW0!(I) / statsW1!(I)) * 100;: Print "%";

    statVal = compareA!(idx, 5) / compareA!(idx, 0)
    Locate , 49: Print Using "#####.#"; statVal;
    Locate , 56: Print Using "######"; (compareA!(idx, 2) * 2) + compareA!(idx, 4) + statsW0!(I);

    If (compareA!(idx, 2) * 2) + compareA!(idx, 4) + statsW0!(I) = 0 Then Locate , 62: Print "   0.0";
    If (compareA!(idx, 2) * 2) + compareA!(idx, 4) + statsW0!(I) > 0 Then Locate , 62: Print Using "####.#"; ((compareA!(idx, 2) * 2) + compareA!(idx, 4) + statsW0!(I)) / compareA!(idx, 0);
    If compareA!(idx, 10) = 0 Then Locate , 68: Print "    0.0";
    If compareA!(idx, 10) > 0 Then Locate , 68: Print Using "#####.#"; ((compareA!(idx, 10) + 30) / 60) / compareA!(idx, 0);


    Color 7, 0: Print String$(2, 177)
    Locate , 16: Print String$(61, 177)

End Sub
