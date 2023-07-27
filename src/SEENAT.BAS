Sub SEENAT ()

    _Title "Courtside College Basketball - View National Stats"

    Dim organizationLeaderFiles$(0 To MAX_FILES)

    Do
        'Display list of files...
        Spec$ = diskPaths$(1) + "*.NLD"
        Count% = FCount%(Spec$)

        If Count% = 0 Then
            Call CSMsgBox("NO ORGANIZATION FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        Else
            ReDim organizationLeaderFiles$(0 To Count%)
            For X = 1 To Count%
                organizationLeaderFiles$(X) = Space$(12)
            Next

            organizationLeaderFiles$(0) = Spec$
            Call ReadFile(organizationLeaderFiles$())

            Do
                Color , 1

                Cls

                Locate , 28
                Color 14
                Print "SELECT ORGANIZATION TO VIEW": Color 15
                Selection% = 1
                START% = 1
                FileFGColr% = 15: FileBGColr% = 1
                HiLiteFGColr% = 0: HiLiteBGColr% = 7
                Rows% = 20
                Columns% = 4
                Spacing% = 6
                Row% = 4
                Column% = 1
                Locate 25, 1
                Color 14
                Print "ENTER";: Color 15
                Print "-SELECTS ORGANIZATION";

                _MouseShow "DEFAULT"
                MAMenu organizationLeaderFiles$(), Selection%, START%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                If ExitCode% <> 27 Then

                    Color 15, 1
                    targetFile$ = organizationLeaderFiles$(Selection%)
                    Locate 2, 33: Print targetFile$

                    Do
                        Locate 3, 33: Print "ANY CHANGE (YN)";
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    Color 15, 0

                End If

            Loop Until UCase$(I$) = "N" Or ExitCode% = 27

            If UCase$(I$) = "N" Then

                targetFile$ = RTrim$(targetFile$)
                WE$ = Left$(targetFile$, Len(targetFile$) - 4)
                targetFile$ = Left$(WE$, 8)

                Call LoadLeaderFiles_Nat(NRT, targetFile$)
                Call StatOptions_Nat (NRT, targetFile$)

            End If 'Done checking for ESC

        End If 'Done checking file count

        '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0

End Sub


'----------------------------------------
'     LoadLeaderFiles_Nat Subroutine
'----------------------------------------
'...explanation...
Sub LoadLeaderFiles_Nat (NRT, leagueFile$)

    Shared DL$(), OL$(), PR$()
    Shared DL!(), OL!()
    Shared seenatZ!()
    Shared seenatZ1$(), seenatZ2$(), seenatZ3$(), T$()
    Shared PT#()

    Shared O1!(), O2!(), O3!(), O4!(), O5!(), O6!()

    Cls
    Locate 9, 1: Print "      PLEASE WAIT WHILE THE LEADER FILES ARE LOADING!!"

    Open diskPaths$(1) + leagueFile$ + ".NLD" For Input As #2

    For X = 1 To 30: Input #2, T$(X), O1!(X), O2!(X), O3!(X), O4!(X), O5!(X), O6!(X): Next

    For X = 1 To 250: Input #2, seenatZ2$(X), seenatZ3$(X), seenatZ!(X), seenatZ1$(X): Next

    For X = 1 To 40: Input #2, OL$(X): For M = 1 To 20: Input #2, OL!(X, M): Next: Next

    For X = 1 To 40: Input #2, DL$(X): For M = 1 To 20: Input #2, DL!(X, M): Next: Next

    Close #2

    Open diskPaths$(1) + leagueFile$ + ".POL" For Input As #2

    Input #2, NRT

    For X = 1 To NRT
        Input #2, PR$(X), PT#(X, 1), PT#(X, 2), PT#(X, 3), PT#(X, 4)
    Next

    Close 2

End Sub


'----------------------------------------
'         StatOptions_Nat Subroutine
'----------------------------------------
'...explanation...
Sub StatOptions_Nat (NRT, leagueFile$)

    Do

        Cls
        Call TITLE_STATS

        J = 5

        Locate 8, 8: Color 15, 1

        'Print box top
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

        'Print box sides
        For II = 1 To J
            Locate 8 + II, 8
            Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
        Next

        'Print box bottom
        Locate 9 + J, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

        Locate 7, 32

        Do

            Locate 10, 15
            Color 7, 1

            Locate , 30: Print "STATS TO ";
            Color 15: Print "S";: Color 7: Print "CREEN"
            Locate , 30: Print "STATS TO ";
            Color 15: Print "P";: Color 7: Print "RINTER"
            Locate , 30: Print "STATS TO ";
            Color 15: Print "F";: Color 7: Print "ILE"
            I$ = GetKeyPress$

            Color 14, 0

        Loop Until UCase$(I$) = "F" Or UCase$(I$) = "P" Or UCase$(I$) = "S"

        If I$ <> Chr$(27) Then

            Select Case UCase$(I$)

                Case "F":
                    Call ReportFilePrompt_Nat(SF$)
                    Call SavePrintNatLeaders(reportFile$, leagueFile$, NRT, 1)

                Case "P":
                    Call SavePrintNatLeaders(reportFile$, leagueFile$, NRT, 0)

                Case "S":
                    Call LeaderStatsNat_Screen(leagueFile$, NRT)

            End Select

            Color 15, 0
            Cls

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

            Do
                Color 15, 1
                Locate 8
                U$ = "VIEW LEAGUE STATS AGAIN (YN)"
                Locate , (80 - Len(U$)) / 2
                Print U$
                Color 15, 0
                A$ = GetKeyPress$
            Loop Until UCase$(A$) = "Y" Or UCase$(A$) = "N"

        End If

    Loop Until UCase$(A$) = "N" Or A$ = Chr$(27)

End Sub


'----------------------------------------
'     ReportFilePrompt_Nat Subroutine
'----------------------------------------
'...explanation...
Sub ReportFilePrompt_Nat (reportFile$)

    Do

        Color 15, 0
        Cls

        JJ = 7

        Locate 6, 8
        Color 15, 1

        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

        For II = 1 To JJ:
            Locate 6 + II, 8
            Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
        Next

        Locate 7 + JJ, 8

        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

        Locate 7, 32

        Locate , 12: Print "ENTER NAME FOR FILE: ": Print
        Locate , 12: Print "THE NAME SHOULD HAVE NO PERIODS OR SPACES"
        Locate , 12: Print "DO NOT ADD A FILE EXTENSION; THE PROGRAM"
        Locate , 12: Print "WILL AUTOMATICALLY ADD .RPT"
        Locate 7, 37: Input reportFile$

        If reportFile$ <> "" Then
            Do
                Locate 13, 12: Print "ANY CHANGE (YN) "
                F$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"
        End If

    Loop Until UCase$(F$) = "N" Or reportFile$ <> ""

    Color 15, 0

End Sub


'----------------------------------------
'     LeaderStatsNat_Screen Subroutine
'----------------------------------------
'...explanation...
Sub LeaderStatsNat_Screen (orgName$, NRT)

    Shared DL$(), OL$(), PR$()
    Shared DL!(), OL!(), seenatZ!()
    Shared seenatZ1$(), seenatZ2$(), seenatZ3$()

    Shared PT#()

    Color 15, 0
    Cls

    Color 12
    Print orgName$ + " OVERALL RANKINGS"

    Locate , 1
    Color 14
    Print "                    RATING     W    L    STRENGTH OF SCHEDULE"
    Color 15

    For P = 1 To NRT

        Print Using "###  "; P;
        Print PR$(P); Tab(22); Using "###.#    ##   ##          ###.#"; PT#(P, 1); PT#(P, 2); PT#(P, 3); PT#(P, 4)

        If P / 20 = Int(P / 20) Then

            Color 12
            'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
            Color 15
            QK$ = GetKeyPress$

            Cls

            Color 12
            Print orgName$ + " OVERALL RANKINGS"

            Locate , 1
            Color 14: Print "                    RATING     W    L    STRENGTH OF SCHEDULE"
            Color 15

        End If

    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "FIELD GOAL % (MIN 5 FGA/G)"
    Print "                            PCT   FGA"
    Color 15

    For P = 41 To 60
        If seenatZ2$(P) <> "" Then
            Print Left$(seenatZ2$(P), 10) + ", " + seenatZ3$(P);
            Locate , 28
            Print Using "###.#  "; seenatZ!(P) * 100;
            Print "  " + seenatZ1$(P)
        End If
    Next

    Color 14
    Locate 1, 41
    Print "FREE THROW % (MIN 2 FTA/G)"
    Locate , 41
    Print "                            PCT   FTA"
    Color 15

    For P = 61 To 80
        If seenatZ2$(P) <> "" Then
            Locate , 41
            Print Left$(seenatZ2$(P), 10) + ", " + seenatZ3$(P);
            Locate , 68
            Print Using "###.#  "; seenatZ!(P) * 100;
            Print "  " + seenatZ1$(P)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "SCORING"
    Print "                                PTS/G"
    Color 15

    For P = 1 To 20
        If seenatZ2$(P) <> "" Then
            Print seenatZ2$(P) + ", " + seenatZ3$(P);
            Locate , 34
            Print Using "##.#"; seenatZ!(P)
        End If
    Next

    Color 14
    Locate 1, 41
    Print "TOTAL POINTS"
    Locate , 41
    Print "                                  PTS"
    Color 15

    For P = 21 To 40
        If seenatZ2$(P) <> "" Then
            Locate , 41: Print seenatZ2$(P) + ", " + seenatZ3$(P);
            Locate , 74
            Print Using "####"; seenatZ!(P)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "STEALS PER GAME"
    Color 15

    For P = 81 To 90
        If seenatZ2$(P) <> "" Then
            Print seenatZ2$(P) + ", " + seenatZ3$(P);
            Locate , 34
            Print Using "##.#"; seenatZ!(P)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "STEALS"
    Color 15

    For P = 91 To 100
        If seenatZ2$(P) <> "" Then
            Locate , 41: Print seenatZ2$(P) + ", " + seenatZ3$(P);
            Locate , 74
            Print Using "####"; seenatZ!(P)
        End If
    Next

    Locate 13
    Color 14
    Print "ASSISTS PER GAME"
    Color 15

    For P = 101 To 110
        If seenatZ2$(P) <> "" Then
            Print seenatZ2$(P) + ", " + seenatZ3$(P);
            Locate , 34
            Print Using "##.#"; seenatZ!(P)
        End If
    Next

    Locate 13, 41
    Color 14
    Print "ASSISTS"
    Color 15

    For P = 111 To 120
        If seenatZ2$(P) <> "" Then
            Locate , 41: Print seenatZ2$(P) + ", " + seenatZ3$(P);
            Locate , 74
            Print Using "####"; seenatZ!(P)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "THREE FIELD GOAL% (MIN 1.5 3FGM/G)"
    Color 15

    For P = 121 To 130

        If seenatZ2$(P) <> "" Then
            Print seenatZ2$(P) + ", " + seenatZ3$(P);
            Locate , 33
            Print Using "###.#"; seenatZ!(P) * 100
        End If

    Next P

    Locate 1, 41
    Color 14
    Print "MINUTES PER GAME"
    Color 15

    For P = 141 To 150
        If seenatZ2$(P) <> "" Then
            Locate , 41: Print seenatZ2$(P) + ", " + seenatZ3$(P);
            Locate , 74
            Print Using "##.#"; seenatZ!(P)
        End If
    Next P

    Locate 13
    Color 14
    Print "THREE POINT FIELD GOALS MADE"
    Color 15

    For P = 131 To 140
        If seenatZ2$(P) <> "" Then
            Print seenatZ2$(P) + ", " + seenatZ3$(P);
            Locate , 34
            Print Using "####"; seenatZ!(P)
        End If
    Next

    Locate 13, 41
    Color 14
    Print "TOTAL MINUTES"
    Color 15

    For P = 151 To 160
        If seenatZ2$(P) <> "" Then
            Locate , 41: Print seenatZ2$(P) + ", " + seenatZ3$(P);
            Locate , 74
            Print Using "####"; seenatZ!(P)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "REBOUNDS PER GAME   "
    Color 15

    For P = 161 To 170
        If seenatZ2$(P) <> "" Then
            Print seenatZ2$(P) + ", " + seenatZ3$(P);
            Locate , 34
            Print Using "##.#"; seenatZ!(P)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "REBOUNDS"
    Color 15

    For P = 171 To 180
        If seenatZ2$(P) <> "" Then
            Locate , 41: Print seenatZ2$(P) + ", " + seenatZ3$(P);
            Locate , 74
            Print Using "####"; seenatZ!(P)
        End If
    Next

    Locate 13
    Color 14
    Print "BLOCKS PER GAME"
    Color 15

    For P = 181 To 190
        If seenatZ2$(P) <> "" Then
            Print seenatZ2$(P) + ", " + seenatZ3$(P);
            Locate , 34
            Print Using "##.#"; seenatZ!(P)
        End If
    Next

    Locate 13, 41
    Color 14
    Print "BLOCKS"
    Color 15

    For P = 191 To 200
        If seenatZ2$(P) <> "" Then
            Locate , 41: Print seenatZ2$(P) + ", " + seenatZ3$(P);
            Locate , 74
            Print Using "####"; seenatZ!(P)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "FIELD GOALS MADE "
    Color 15

    For P = 201 To 210
        If seenatZ2$(P) <> "" Then
            Print seenatZ2$(P) + ", " + seenatZ3$(P);: Locate , 34
            Print Using "####"; seenatZ!(P)
        End If
    Next

    Color 14
    Locate 1, 41: Print "FREE THROWS MADE"
    Color 15

    For P = 211 To 220
        If seenatZ2$(P) <> "" Then
            Locate , 41: Print seenatZ2$(P) + ", " + seenatZ3$(P);
            Locate , 74
            Print Using "####"; seenatZ!(P)
        End If
    Next

    Locate 13
    Color 14
    Print "FOULS PER GAME"
    Color 15

    For P = 221 To 230
        If seenatZ2$(P) <> "" Then
            Print seenatZ2$(P) + ", " + seenatZ3$(P);: Locate , 34
            Print Using "##.#"; seenatZ!(P)
        End If
    Next

    Locate 13, 41
    Color 14
    Print "FOULS"
    Color 15

    For P = 231 To 240
        If seenatZ2$(P) <> "" Then
            Locate , 41: Print seenatZ2$(P) + ", " + seenatZ3$(P);
            Locate , 74
            Print Using "####"; seenatZ!(P)
        End If

    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "MOST VALUABLE PLAYER POINTS"
    Color 15

    For P = 241 To 250
        If seenatZ2$(P) <> "" Then
            Print seenatZ2$(P) + ", " + seenatZ3$(P);
            Locate , 34
            Print Using "####"; seenatZ!(P)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 12
    Print "                         OFFENSIVE TOTALS"
    Color 14
    Print "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG"
    Color 15

    For X = 1 To 20

        If OL$(X) <> "" And OL$(X) <> "*" And OL$(X) <> " " Then
            Print Left$(OL$(X), 11); Tab(13);: Print Using "##"; OL!(X, 16);: Print Tab(16);: Print Using "####"; OL!(X, 2);: Print Tab(21);: Print Using "####"; OL!(X, 3);: Print Tab(26);
            Print Using ".###"; OL!(X, 17);: Print Tab(31);: Print Using "#### #### .###"; OL!(X, 4); OL!(X, 5); OL!(X, 18);
            Print Tab(46);: Print Using "####"; OL!(X, 6);: Print Tab(51);: Print Using "####"; OL!(X, 9);: Print Tab(56);: Print Using "####"; OL!(X, 7);: Print Tab(61);
            Print Using "###"; OL!(X, 8);: Print Tab(65);: Print Using "###"; OL!(X, 10);: Print Tab(69);: Print Using "#####"; OL!(X, 20);
            Print Tab(75);: Print Using "###.#"; OL!(X, 19)
        End If

    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 12
    Print "                         DEFENSIVE TOTALS"
    Color 14
    Print "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG"
    Color 15

    For X = 1 To 20

        If DL$(X) <> "" And DL$(X) <> "*" And DL$(X) <> " " Then
            Print Left$(DL$(X), 11); Tab(13);: Print Using "##"; DL!(X, 16);: Print Tab(16);: Print Using "####"; DL!(X, 2);: Print Tab(21);: Print Using "####"; DL!(X, 3);: Print Tab(26);
            Print Using ".###"; DL!(X, 17);: Print Tab(31);: Print Using "#### #### .###"; DL!(X, 4); DL!(X, 5); DL!(X, 18);
            Print Tab(46);: Print Using "#### ####"; DL!(X, 6); DL!(X, 9);: Print Tab(56);: Print Using "####"; DL!(X, 7);: Print Tab(61);
            Print Using "###"; DL!(X, 8);: Print Tab(65);: Print Using "###"; DL!(X, 10);: Print Tab(69);: Print Using "#####"; DL!(X, 20);
            Print Tab(75);: Print Using "###.#"; DL!(X, 19)
        End If

    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

End Sub

'----------------------------------------
'     SavePrintNatLeaders Subroutine
'----------------------------------------
'This routine opens/creates the target
'   report file and writes data to it.
'   At the end, if specified, this file
'   is sent to the printer.
'
'This saves the effort of needing to
'   have an LPrint and Print for
'   everything
'
'We should do this with other routines
'   and simply use a temp file

Sub SavePrintNatLeaders (reportFile$, orgName$, NRT, destination)

    Shared DL$(), OL$(), PR$()
    Shared DL!(), OL!(), seenatZ!()
    Shared seenatZ1$(), seenatZ2$(), seenatZ3$()
    Shared PT#()

    If destination <> 1 Then
        Open "REPORT" For Output As #2
    Else
        Open reportFile$ + ".RPT" For Output As #2
    End If

    Color 0, 15
    Cls

    Print #2, orgName$ + " OVERALL RANKINGS"
    Print #2, "                    RATING     W    L    STRENGTH OF SCHEDULE"

    For P = 1 To NRT
        Print #2, Using "###  "; P;
        Print #2, PR$(P); Tab(22); Using "###.#    ##   ##          ###.#"; PT#(P, 1); PT#(P, 2); PT#(P, 3); PT#(P, 4)
    Next

    Print #2, Chr$(12)
    Print #2, "FIELD GOAL % (MIN 5 FGM/G)"; Tab(41); "FREE THROW % (MIN 2.5 FTM/G)"
    Print #2,
    Print #2, "                            PCT   FGA"; Tab(41); "                            PCT   FTA"

    For P = 41 To 60

        If seenatZ2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, Left$(seenatZ2$(P), 10) + ", " + seenatZ3$(P);
            Print #2, Tab(28);
            Print #2, Using "##.#  ####"; seenatZ!(P) * 100; seenatZ1$(P);
            Print #2, Tab(41);
        End If

        If seenatZ2$(P + 20) = "" Then
            Print #2,
        Else
            Print #2, Left$(seenatZ2$(P + 20), 10) + ", " + seenatZ3$(P + 20);
            Print #2, Tab(67);
            Print #2, Using "###.#  ####"; seenatZ!(P + 20) * 100; seenatZ1$(P)
        End If

    Next

    Print #2,
    Print #2, "SCORING"; Tab(41); "TOTAL POINTS"

    For P = 1 To 20

        If seenatZ2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, seenatZ2$(P) + ", " + seenatZ3$(P);
            Print #2, Tab(34);
            Print #2, Using "##.#"; seenatZ!(P);
            Print #2, Tab(41);
        End If

        If seenatZ2$(P + 20) = "" Then
            Print #2,
        Else
            Print #2, seenatZ2$(P + 20) + ", " + seenatZ3$(P + 20);
            Print #2, Tab(74);
            Print #2, Using "####"; seenatZ!(P + 20)
        End If

    Next

    Print #2, Chr$(12)
    Print #2, "STEALS PER GAME"; Tab(41); "STEALS"

    For P = 81 To 90

        If seenatZ2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, seenatZ2$(P) + ", " + seenatZ3$(P);
            Print #2, Tab(34);
            Print #2, Using "##.#"; seenatZ!(P);
            Print #2, Tab(41);
        End If
        If seenatZ2$(P + 10) = "" Then
            Print #2,
        Else
            Print #2, seenatZ2$(P + 10) + ", " + seenatZ3$(P + 10);
            Print #2, Tab(74);
            Print #2, Using "####"; seenatZ!(P + 10)
        End If

    Next

    Print #2,
    Print #2, "ASSISTS PER GAME"; Tab(41); "ASSISTS"

    For P = 101 To 110

        If seenatZ2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, seenatZ2$(P) + ", " + seenatZ3$(P);
            Print #2, Tab(34);
            Print #2, Using "##.#"; seenatZ!(P);
            Print #2, Tab(41);
        End If

        If seenatZ2$(P + 10) = "" Then
            Print #2,
        Else
            Print #2, seenatZ2$(P + 10) + ", " + seenatZ3$(P + 10);
            Print #2, Tab(74);
            Print #2, Using "####"; seenatZ!(P + 10)
        End If

    Next

    Print #2,
    Print #2, "THREE FIELD GOAL % (MIN 1.5 3FGM/G)"; Tab(41); "MINUTES PER GAME"

    For P = 121 To 130

        If seenatZ2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, seenatZ2$(P) + ", " + seenatZ3$(P);
            Print #2, Tab(33);
            Print #2, Using "###.#"; seenatZ!(P) * 100;
            Print #2, Tab(41);
        End If

        If seenatZ2$(P + 20) = "" Then
            Print #2,
        Else
            Print #2, seenatZ2$(P + 20) + ", " + seenatZ3$(P + 20);
            Print #2, Tab(74);
            Print #2, Using "##.#"; seenatZ!(P + 20)
        End If

    Next

    Print #2,
    Print #2, "THREE POINT FIELD GOALS MADE"; Tab(41); "MINUTES"

    For P = 131 To 140

        If seenatZ2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, seenatZ2$(P) + ", " + seenatZ3$(P);
            Print #2, Tab(34);
            Print #2, Using "####"; seenatZ!(P);
            Print #2, Tab(41);
        End If

        If seenatZ2$(P + 20) = "" Then
            Print #2,
        Else
            Print #2, seenatZ2$(P + 20) + ", " + seenatZ3$(P + 20);
            Print #2, Tab(74);
            Print #2, Using "####"; seenatZ!(P + 20)
        End If

    Next

    Print #2,
    Print #2, Chr$(12)
    Print #2, "REBOUNDS PER GAME"; Tab(41); "REBOUNDS"

    For P = 161 To 170

        If seenatZ2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, seenatZ2$(P) + ", " + seenatZ3$(P);
            Print #2, Tab(34);
            Print #2, Using "##.#"; seenatZ!(P);
            Print #2, Tab(41);
        End If

        If seenatZ2$(P + 10) = "" Then
            Print #2,
        Else
            Print #2, seenatZ2$(P + 10) + ", " + seenatZ3$(P + 10);
            Print #2, Tab(74);
            Print #2, Using "####"; seenatZ!(P + 10)
        End If

    Next

    Print #2,
    Print #2, "BLOCKS PER GAME"; Tab(41); "BLOCKS"

    For P = 181 To 190

        If seenatZ2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, seenatZ2$(P) + ", " + seenatZ3$(P);
            Print #2, Tab(34);
            Print #2, Using "##.#"; seenatZ!(P);
            Print #2, Tab(41);
        End If

        If seenatZ2$(P + 10) = "" Then
            Print #2,
        Else
            Print #2, seenatZ2$(P + 10) + ", " + seenatZ3$(P + 10);
            Print #2, Tab(74);
            Print #2, Using "####"; seenatZ!(P + 20)
        End If

    Next

    Print #2,
    Print #2, "FIELD GOALS MADE"; Tab(41); "FREE THROWS MADE"

    For P = 201 To 210

        If seenatZ2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, seenatZ2$(P) + ", " + seenatZ3$(P);
            Print #2, Tab(34);
            Print #2, Using "####"; seenatZ!(P);
            Print #2, Tab(41);
        End If

        If seenatZ2$(P + 10) = "" Then
            Print #2,
        Else
            Print #2, seenatZ2$(P + 10) + ", " + seenatZ3$(P + 10);
            Print #2, Tab(74);
            Print #2, Using "####"; seenatZ!(P + 10)
        End If

    Next

    Print #2,
    Print #2, "FOULS PER GAME"; Tab(41); "FOULS"

    For P = 221 To 230

        If seenatZ2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, seenatZ2$(P) + ", " + seenatZ3$(P);
            Print #2, Tab(34);
            Print #2, Using "##.#"; seenatZ!(P);
            Print #2, Tab(41);
        End If

        If seenatZ2$(P + 10) = "" Then
            Print #2,
        Else
            Print #2, seenatZ2$(P + 10) + ", " + seenatZ3$(P + 10);
            Print #2, Tab(74);
            Print #2, Using "####"; seenatZ!(P + 10)
        End If

    Next

    Print #2,
    Print #2, "MOST VALUABLE PLAYER"

    For P = 241 To 250

        If seenatZ2$(P) <> "" Then
            Print #2, seenatZ2$(P) + ", " + seenatZ3$(P);
            Print #2, Tab(34);
            Print #2, Using "####"; seenatZ!(P)
        End If

    Next

    Print #2,
    Print #2, Chr$(12)
    Print #2, "                         OFFENSIVE TOTALS"
    Print #2, "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG"

    For X = 1 To 20

        If OL$(X) <> "" And OL$(X) <> "*" And OL$(X) <> " " Then
            Print #2, Left$(OL$(X), 11); Tab(13);: Print #2, Using "##"; OL!(X, 16);: Print #2, Tab(16);: Print #2, Using "####"; OL!(X, 2);: Print #2, Tab(21);: Print #2, Using "####"; OL!(X, 3);: Print #2, Tab(26);
            Print #2, Using ".###"; OL!(X, 17);: Print #2, Tab(31);: Print #2, Using "#### #### .###"; OL!(X, 4); OL!(X, 5); OL!(X, 18);
            Print #2, Tab(46);: Print #2, Using "####"; OL!(X, 6);: Print #2, Tab(51);: Print #2, Using "####"; OL!(X, 9);: Print #2, Tab(56);: Print #2, Using "####"; OL!(X, 7);: Print #2, Tab(61);
            Print #2, Using "###"; OL!(X, 8);: Print #2, Tab(65);: Print #2, Using "###"; OL!(X, 10);: Print #2, Tab(69);: Print #2, Using "#####"; OL!(X, 20);
            Print #2, Tab(75);: Print #2, Using "###.#"; OL!(X, 19)
        End If

    Next

    Print #2,
    Print #2, "                         DEFENSIVE TOTALS"
    Print #2, "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG": Color 15

    For X = 1 To 20

        If DL$(X) <> "" And DL$(X) <> "*" And DL$(X) <> " " Then
            Print #2, Left$(DL$(X), 11); Tab(13);: Print #2, Using "##"; DL!(X, 16);: Print #2, Tab(16);: Print #2, Using "####"; DL!(X, 2);: Print #2, Tab(21);: Print #2, Using "####"; DL!(X, 3);: Print #2, Tab(26);
            Print #2, Using ".###"; DL!(X, 17);: Print #2, Tab(31);: Print #2, Using "#### #### .###"; DL!(X, 4); DL!(X, 5); DL!(X, 18);
            Print #2, Tab(46);: Print #2, Using "#### ####"; DL!(X, 6); DL!(X, 9);: Print #2, Tab(56);: Print #2, Using "####"; DL!(X, 7);: Print #2, Tab(61);
            Print #2, Using "###"; DL!(X, 8);: Print #2, Tab(65);: Print #2, Using "###"; DL!(X, 10);: Print #2, Tab(69);: Print #2, Using "#####"; DL!(X, 20);
            Print #2, Tab(75);: Print #2, Using "###.#"; DL!(X, 19)
        End If

    Next

    Close 2

    If destination <> 1 Then

        Open "REPORT" For Input As #2

        Do While Not EOF(2)
            Line Input #2, X$
            LPrint X$
        Loop

        LPrint Chr$(12)

        Close 2

    End If

End Sub
