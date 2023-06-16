Sub COMPARE ()

    _Title "Courtside College Basketball - Stats Comparison"

    Shared compareT%(), HP%(), OP%()

    Shared compareA!(), statsW0!(), statsW1!()
    Shared statsZ!(), statsZ1!()

    Shared A$(), HP$(), OP$()
    Shared P$(), T$()

    Dim posn$(0 To 13)

    Do

        Color 15, 0
        Cls

        JJ = 7

        Color 15, 4
        Locate 6, 8

        'Print box top
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

        'Print box sides
        For II = 1 To JJ
            Locate 6 + II, 8
            Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
        Next

        'Print box bottom
        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

        Locate 7, 32
        Locate , 12: Print "ENTER DISK ID: ": Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
        Locate 7, 27: Input NY$
        If NY$ = "" And yearNumber$ <> "" Then
            Call SelectTeam_COMPARE(yearNumber$, numberTeams, T$)
        Else
            yearNumber$ = RTrim$(NY$)
            Erase teamNames$
            numberTeams = 0

            If Not _FileExists(diskPaths$(0) + "BASK." + yearNumber$) Then
                Locate 10, 8
                Call CSMsgBox("NO TEAM FILES WERE FOUND MATCHING THIS ID.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, PASS)
                I$ = GetKeyPress$
                Call CSMsgBox("", 0, PASS)
            Else

                Open diskPaths$(0) + "BASK." + yearNumber$ For Random As #1 Len = 655

                fileLength& = LOF(1)

                Field #1, 15 As Q$(0), 640 As X$

                For I1 = 1 To fileLength& / 655
                    Get #1, I1
                    teamNames$(I1) = Q$(0)
                    teamIndex%(I1) = I1
                Next

                Close 1

                numberTeams = fileLength& / 655

                Call SelectTeam_COMPARE(yearNumber$, numberTeams, T$)

            End If 'Done checking for teams file

        End If 'Done checking INPUT

        If BO% = 1 Then
            BO% = 0
        Else
            Locate 2, 33
            Color compareT%(0), compareT%(1): Print T$

            Do
                Color 15, 4
                Locate 3, 33: Print "ANY CHANGE (YN)";
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

        If UCase$(I$) = "N" Then

            'Determine stat file for team
            For XX = 4 To 11
                If Mid$(T$, XX, 1) = " " Or Mid$(T$, XX, 1) = "." Then Mid$(T$, XX, 1) = "_"
            Next XX

            For XX = 12 To 14
                If Mid$(T$, XX, 2) = "ST" Then Mid$(T$, 9, 1) = "_": Mid$(T$, 10, 2) = "ST"
            Next XX

            QQ = 8

            For XX = 11 To 4 Step -1
                If Mid$(T$, XX, 1) = "_" Then
                    QQ = XX - 4
                Else
                    Exit For
                End If
            Next XX

            Z$ = Mid$(T$, 4, QQ) + "." + Left$(T$, 2)

            If Not _FileExists(diskPaths$(1) + Z$) Then
                Locate 10, 8
                Call CSMsgBox("NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, PASS)
                I$ = GetKeyPress$
                Call CSMsgBox("", 0, PASS)
            Else

                Open diskPaths$(1) + Z$ For Input As #1

                Input #1, T$

                For I = 0 To 13:

                    Input #1, A$(I)

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
                    If P$(I) = "F " Then posn$(I) = "FORWARD"
                    If P$(I) = "C " Then posn$(I) = "CENTER"
                    If P$(I) = "G " Then posn$(I) = "GUARD"
                    If P$(I) = "FC" Then posn$(I) = "FORWARD/GUARD"
                    If P$(I) = "FG" Then posn$(I) = "FORWARD/CENTER"
                    If P$(I) = "GF" Then posn$(I) = "GUARD/FORWARD"
                    If P$(I) = "CF" Then posn$(I) = "CENTER/FORWARD"
                Next

                Open diskPaths$(1) + Z$ + "S" For Input As #1

                Input #1, Y

                For J = 1 To 100: Input #1, OP%(J): Next
                For J = 1 To 100: Input #1, OP$(J): Next
                For J = 1 To 100: Input #1, HP%(J): Next
                For J = 1 To 100: Input #1, HP$(J): Next

                Close #1

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
                            Call Comparisons_Printer(T$)

                        Case "S":
                            Call Comparisons_Screen(T$, backToMenu, posn$())

                    End Select

                End If

            End If 'Done checking file count

        End If 'Done checking for ESC

    Loop Until (UCase$(I$) = "N" And backToMenu = 0) Or I$ = Chr$(27)

End Sub


'----------------------------------------
'     Comparisons_Screen Subroutine
'----------------------------------------
'...explanation...
Sub Comparisons_Screen (teamName$, backToMenu, posn$())

    Shared compareT%(), compT1%()

    Shared compareA!(), RS!(), X1!()
    Shared statsW0!(), statsW1!()
    Shared statsZ!(), statsZ1!()

    Shared A$(), T$()

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
            Color 7: Print A$(I)
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

                        If A$(playerIdx) <> "XXX" And A$(playerIdx) <> "END" And A$(playerIdx) <> "" Then

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
'...explanation...
Sub TeamComparisons_Screen (T$)

    Shared compareT%(), compT1%()
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
    Locate 6, 41: Print Using "####"; compareT%(7);
    Locate 6, 45: Print Using "####"; compareT%(8);: Print " ";

    Color 14, 0
    Locate 7, 41: Print Using "####"; replayWins%;
    Locate 7, 45: Print Using "####"; replayLosses%;: Print " ";

    Color 3, 0
    Locate 4, 10: Print Using "######"; compT1%(1) / 100 * compT1%(3);
    Locate 5, 10: Print Using "######"; compT1%(1);

    If compT1%(1) = 0 Then Locate 6, 10: Print "  .000";
    If compT1%(1) > 0 Then statPCT! = compT1%(3) / 100: Locate 6, 10: Print "  ";: Print Using ".###"; statPCT!;

    Locate 7, 10: Print Using "######"; compT1%(5) / 100 * compT1%(7);
    Locate 8, 10: Print Using "######"; compT1%(5);

    If compT1%(5) = 0 Then Locate 9, 10: Print "  .000";
    If compT1%(5) > 0 Then statPCT! = compT1%(7) / 100: Locate 9, 10: Print "  ";: Print Using ".###"; statPCT!;

    Locate 10, 10: Print Using "######"; .35 * compT1%(9);
    Locate 11, 10: Print Using "######"; .65 * compT1%(9);
    Locate 12, 10: Print Using "######"; compT1%(9);
    Locate 13, 10: Print Using "######"; compT1%(11);
    Locate 14, 10: Print Using "######"; compT1%(13);
    Locate 15, 10: Print Using "######"; compT1%(17);
    Locate 16, 10: Print Using "######"; compT1%(15);
    Locate 17, 10: Print "   ---"
    Locate 18, 10: Print "   ---"
    Locate 19, 10: Print "   ---"
    Locate 20, 10: Print "   ---"

    Color 3, 0
    Locate 4, 62: Print Using "######"; compT1%(2) / 100 * compT1%(4);
    Locate 5, 62: Print Using "######"; compT1%(2);

    If compareT%(2) = 0 Then Locate 6, 62: Print "  .000";
    If compareT%(2) > 0 Then statPCT! = compT1%(4) / 100: Locate 6, 62: Print "  ";: Print Using ".###"; statPCT!;

    Locate 7, 62: Print Using "######"; compT1%(6) / 100 * compT1%(8);
    Locate 8, 62: Print Using "######"; compT1%(6);

    If compareT%(6) = 0 Then Locate 9, 62: Print "  .000";
    If compareT%(6) > 0 Then statPCT! = compT1%(8) / 100: Locate 9, 62: Print "  ";: Print Using ".###"; statPCT!;

    Locate 10, 62: Print Using "######"; .35 * compT1%(10);
    Locate 11, 62: Print Using "######"; .65 * compT1%(10);
    Locate 12, 62: Print Using "######"; compT1%(10);
    Locate 13, 62: Print Using "######"; compT1%(12);
    Locate 14, 62: Print Using "######"; compT1%(14);
    Locate 15, 62: Print Using "######"; compT1%(18);
    Locate 16, 62: Print Using "######"; compT1%(16);
    Locate 17, 62: Print "   ---"
    Locate 18, 62: Print Using "######"; compT1%(19) * compT1%(20) / 100;
    Locate 19, 62: Print Using "######"; compT1%(19);

    If compareT%(19) = 0 Then Locate 20, 62: Print "  .000";
    If compareT%(19) > 0 Then statPCT! = compT1%(20) / 100: Locate 20, 62: Print "  ";: Print Using ".###"; statPCT!;

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
'   PlayerComparisons_Screen SubRoutine
'----------------------------------------
'...explanation...
Sub PlayerComparisons_Screen (idx, position$, menuExit)

    Shared compareA!(), RS!()
    Shared statsW0!(), statsW1!(), X1!()

    Shared A$(), T$()

    Color 11
    Locate 2, 2: Print T$(0)
    Color 15, 0
    Locate 2, 14: Print String$(34, 177)

    Color 14

    'Player Name
    Locate 2, 14: Print A$(idx);

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
    Locate , 17: Print Using "#####"; RS!(idx, 1);
    Locate , 22: Print Using "#####"; RS!(idx, 3);
    Locate , 27: Print Using "#####"; RS!(idx, 2);

    If RS!(idx, 2) = 0 Then Locate , 32: Print "   --.- %";
    If RS!(idx, 2) > 0 Then Locate , 32: Print Using "#####.#"; (RS!(idx, 3) / RS!(idx, 2)) * 100;: Print " %";

    Locate , 40: Print Using "#####"; RS!(idx, 5);
    Locate , 45: Print Using "#####"; RS!(idx, 4);

    If RS!(idx, 4) = 0 Then Locate , 50: Print "   --.-";: Print " %";
    If RS!(idx, 4) > 0 Then Locate , 50: Print Using "#####.#"; (RS!(idx, 5) / RS!(idx, 4)) * 100;: Print " %";

    Locate , 58: Print Using "#######"; RS!(idx, 6);

    If RS!(idx, 0) = 0 Or RS!(idx, 6) = 0 Then Locate , 65: Print "      0.0";
    If RS!(idx, 0) > 0 And RS!(idx, 6) = 0 Then Locate , 65: Print "      0.0";
    If RS!(idx, 0) > 0 And RS!(idx, 6) > 0 Then Locate , 65: Print Using "#######.#"; RS!(idx, 6) / RS!(idx, 0);

    Color 7, 0: Print String$(2, 177)
    Color 1, 11
    Locate 10, 14: Print Using "######"; RS!(idx, 7) - RS!(idx, 6);

    If RS!(idx, 0) = 0 Or RS!(idx, 7) - RS!(idx, 6) = 0 Then Locate , 20: Print "      0.0";
    If RS!(idx, 0) > 0 And RS!(idx, 7) - RS!(idx, 6) = 0 Then Locate , 20: Print "      0.0";
    If RS!(idx, 0) > 0 And RS!(idx, 7) - RS!(idx, 6) > 0 Then Locate , 20: Print Using "#######.#"; (RS!(idx, 7) - RS!(idx, 6)) / RS!(idx, 0);

    Locate , 29: Print Using "######"; RS!(idx, 9);

    If RS!(idx, 0) = 0 Or RS!(idx, 9) = 0 Then Locate , 35: Print "     0.0";
    If RS!(idx, 0) > 0 And RS!(idx, 9) = 0 Then Locate , 35: Print "     0.0";
    If RS!(idx, 0) > 0 And RS!(idx, 9) > 0 Then Locate , 35: Print Using "######.#"; RS!(idx, 9) / RS!(idx, 0);

    Locate , 43: Print Using "#######"; RS!(idx, 8);

    If RS!(idx, 0) = 0 Or RS!(idx, 8) = 0 Then Locate , 50: Print "  0.0";
    If RS!(idx, 0) > 0 And RS!(idx, 8) = 0 Then Locate , 50: Print "  0.0";
    If RS!(idx, 0) > 0 And RS!(idx, 8) > 0 Then Locate , 50: Print Using "###.#"; RS!(idx, 8) / RS!(idx, 0);

    Locate , 55: Print Using "######"; RS!(idx, 7);
    Locate , 61: Print Using "#####"; RS!(idx, 11);

    If RS!(idx, 0) = 0 Or RS!(idx, 11) = 0 Then Locate , 66: Print "    0.0";
    If RS!(idx, 0) > 0 And RS!(idx, 11) = 0 Then Locate , 66: Print "    0.0";
    If RS!(idx, 0) > 0 And RS!(idx, 11) > 0 Then Locate , 66: Print Using "#####.#"; RS!(idx, 11) / RS!(idx, 0);

    Color 7, 0: Print String$(2, 177)
    Color 1, 11
    Locate 15, 14: Print Using "####"; RS!(idx, 10);

    If RS!(idx, 0) = 0 Or RS!(idx, 10) = 0 Then Locate , 18: Print "    0.0";
    If RS!(idx, 0) > 0 And RS!(idx, 10) = 0 Then Locate , 18: Print "    0.0";
    If RS!(idx, 0) > 0 And RS!(idx, 12) > 0 Then Locate , 18: Print Using "#####.#"; RS!(idx, 10) / RS!(idx, 0);

    Locate , 25: Print Using "#######"; RS!(idx, 13);
    Locate , 32: Print Using "#######"; RS!(idx, 12);

    If RS!(idx, 13) = 0 Then Locate , 39: Print "     --.-";: Print " %";
    If RS!(idx, 12) > 0 Then Locate , 39: Print Using "#######.#"; (RS!(idx, 13) / RS!(idx, 12)) * 100;: Print " %";

    Locate , 49: If RS!(idx, 0) <> 0 Then Print Using "   ##.#"; RS!(idx, 7) / RS!(idx, 0);
    Locate , 56: Print Using "######"; (RS!(idx, 3) * 2) + RS!(idx, 5) + RS!(idx, 13);

    If RS!(idx, 0) = 0 Then Locate , 62: Print "  --.-";
    If RS!(idx, 0) > 0 Then Locate , 62: Print Using "####.#"; ((RS!(idx, 3) * 2) + RS!(idx, 5) + RS!(idx, 13)) / RS!(idx, 0);
    If RS!(idx, 0) = 0 Then Locate , 68: Print "   --.-";
    If RS!(idx, 0) > 0 Then Locate , 68: Print Using "#####.#"; RS!(idx, 1) / RS!(idx, 0);

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
    If statsW0!(I) > 0 Then Locate , 39: Print Using "#######.#"; statsW0!(I) / statsW1!(I) * 100;: Print "%";

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


'----------------------------------------
'   Comparisons_Printer Subroutine
'----------------------------------------
'...explanation...
Sub Comparisons_Printer (teamName$)

End Sub


'----------------------------------------
'   SelectTeam_COMPARE SubRoutine
'----------------------------------------
'...explanation...
Sub SelectTeam_COMPARE (yearNumber$, numberTeams, teamName$)

    Shared mascot$, coach$, stadium$
    Shared CK, defFGPctAdj%, replayLosses%, replayWins%
    Shared staminaRating%, teamAttendance%
    Shared V1%, V2%

    Shared HP%(), OP%(), compareL%(), compareT%(), compT1%()

    Shared A1!(), compareA!(), DF!(), RS!(), X1!()
    Shared statsW0!(), statsW1!()
    Shared statsZ!(), statsZ1!()

    Shared A$(), HP$(), OP$(), P$()

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
    Color , 4: Cls: Locate , 33: Color 14
    Print "TEAM SELECTION": Color 15
    Locate 25, 1: Color 14: Print "ESC";: Color 15
    Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
    Print "ENTER";: Color 15: Print "-SELECTS TEAM";

    _MouseShow "DEFAULT"
    MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    If ExitCode% <> 27 Then

        t1 = teamIndex%(Selection%)
        teamYear$ = RTrim$(yearNumber$)

        Open diskPaths$(0) + "BASK." + teamYear$ For Random As #1 Len = 655

        Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 2 As Q$(16), 2 As Q$(17), 2 As Q$(18), 2 As Q$(19), 602 As X$

        For I = 0 To 13
            Field #1, 53 + I * 43 As X$, 15 As Q$(I * 15 + 20), 2 As Q$(I * 15 + 21), 2 As Q$(I * 15 + 22), 2 As Q$(I * 15 + 23), 2 As Q$(I * 15 + 24), 2 As Q$(I * 15 + 25), 2 As Q$(I * 15 + 26), 2 As Q$(I * 15 + 27), 2 As Q$(I * 15 + 28), 2 As Q$(I * 15 + 29), 2 As Q$(I * 15 + 30), 2 As Q$(I * 15 + 31), 2 As Q$(I * 15 + 32), 2 As Q$(I * 15 + 33), 2 As Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 As X$
        Next

        Get #1, t1

        teamName$ = RTrim$(Q$(0))

        staminaRating% = CVI(Q$(1))
        defFGPctAdj% = CVI(Q$(2))
        V1% = CVI(Q$(3))
        V2% = CVI(Q$(4))

        For I = 0 To 4: compareL%(I) = CVI(Q$(I + 5)): Next
        For I = 0 To 9: compareT%(I) = CVI(Q$(I + 10)): Next

        For I = 0 To 13
            A$(I) = RTrim$(Q$(I * 15 + 20))
            For I1 = 0 To 6
                A1!(I, I1) = CVI(Q$(I * 15 + I1 + 21))
                X1!(I, I1) = CVI(Q$(I * 15 + I1 + 28))
            Next
        Next

        Close 1

        Open diskPaths$(0) + "COLBBTMS." + yearNumber$ For Random As #1 Len = 566

        Field #1, 15 As Q$(0), 40 As Q$(1), 15 As Q$(2), 4 As Q$(3)

        For I = 0 To 13
            Field #1, 74 + I * 32 As X$, 2 As Q$(I * 16 + 4), 2 As Q$(I * 16 + 5), 2 As Q$(I * 16 + 6), 2 As Q$(I * 16 + 7), 2 As Q$(I * 16 + 8), 2 As Q$(I * 16 + 9), 2 As Q$(I * 16 + 10), 2 As Q$(I * 16 + 11), 2 As Q$(I * 16 + 12), 2 As Q$(I * 16 + 13), 2 As Q$(I * 16 + 14), 2 As Q$(I * 16 + 15), 2 As Q$(I * 16 + 16), 2 As Q$(I * 16 + 17), 2 As Q$(I * 16 + 18), 2 As Q$(I * 16 + 19)
            Field #1, 522 As X$, 2 As Q$(228), 2 As Q$(229), 2 As Q$(230), 2 As Q$(231), 2 As Q$(232), 2 As Q$(233), 2 As Q$(234), 2 As Q$(235), 2 As Q$(236), 2 As Q$(237), 2 As Q$(238), 2 As Q$(239), 2 As Q$(240), 2 As Q$(241), 2 As Q$(242), 2 As Q$(243), 2 As Q$(244), 2 As Q$(245), 2 As Q$(246), 2 As Q$(247), 2 As Q$(248), 2 As Q$(249)
        Next

        Get #1, t1

        coach$ = RTrim$(Q$(0))
        stadium$ = RTrim$(Q$(1))
        mascot$ = RTrim$(Q$(2))
        teamAttendance% = CVL(Q$(3))

        For I = 0 To 13
            DF!(I) = CVI(Q$(I * 16 + 4))
            P$(I) = Q$(I * 16 + 5)
            For I1 = 0 To 13
                RS!(I, I1) = CVI(Q$(I * 16 + I1 + 6))
            Next
        Next

        For I = 0 To 20
            compT1%(I) = CVI(Q$(228 + I))
        Next

        CK = CVI(Q$(249))

        Close 1

        '-- Everything from here down needs to be in the calling routine
        If CK <> 999 Then

            coach$ = ""
            stadium$ = ""
            mascot$ = ""
            teamAttendance% = 0

            For I = 0 To 13

                DF!(I) = 5
                P$(I) = ""

                For I1 = 0 To 13
                    RS!(I, I1) = 0
                Next

            Next

            For I = 0 To 20:
                compT1%(I) = 0
            Next

        End If

    Else
        BO% = 1
    End If 'Done checking for ESC

End Sub
