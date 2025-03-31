'----------------------------------------
'          HD2HDCON Subroutine
'----------------------------------------
'This subroutine reads in stats from the
'selected team and then presents the user
'a menu of options for viewing Head-to-Head
'team comparisons.
Sub HD2HDCON ()

    _Title "Courtside College Basketball - Head-to-Head Stats"

    Shared teamScore(), oppScore()

    Shared awayWins(), awayLosses()
    Shared homeWins(), homeLosses()
    Shared awayScoreTeam!(), awayScoreOpp!()
    Shared homeScoreTeam!(), homeScoreOpp!()
    Shared totAwayWins(), totAwayLosses()
    Shared totHomeWins(), totHomeLosses()
    Shared totAwayScoreTeam!(), totAwayScoreOpp!()
    Shared totHomeScoreTeam!(), totHomeScoreOpp!()

    Shared locIndicator$(), oppName$()
    Shared memberName$(), memberYear$()

    Shared conferenceName$
    Shared div1Name$, div2Name$

    newTeam = 1

    Do

        If newTeam = 1 Then

            teamNbr = 0
            Call ChooseTeam(YN$, NTMS, teamNbr, team$)

            If teamNbr > 0 Then

                statFile$ = nameStatFile$(team$)

                If Not _FileExists(diskPaths$(1) + statFile$ + "S") Then

                    MsgText$ = "NO COMPILED STATS COULD BE FOUND FOR THIS TEAM. PRESS ANY KEY TO RETURN TO THE MAIN MENU"
                    BoxWidth = 24
                    BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                    Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                    Call CSMsgBox(MsgText$, BoxWidth, PASS)
                    I$ = GetKeyPress$
                    Call CSMsgBox("", 0, PASS)

                Else

                    Call ReadScheduleStats (statFile$, nbrGames)

                    'Check if same conference
                    If sameConf <> 1 Then
                        conferenceFile$ = ChooseFile$(diskPaths$(2) + "*.TMI", "CONFERENCE", ExitCode%)
                    End If

                End If

            Else
                'If a team wasn't chosen, treat it like ESC
                ExitCode% = 27

            End If 'Done checking for valid team

        End If 'Done checking for new team

        Color 7, 0
        Cls

        If teamNbr > 0 And ExitCode% <> 27 And ExitCode% <> 0 Then

            userKey$ = GetOutputOptions$

            If userKey$ <> Chr$(27) Then

                Select Case userKey$

                    Case "F"
                        printChoice = 0
                        VPP = 0
                    Case "P"
                        printChoice = 1
                        VPP = 0
                    Case "S"
                        VPP = 1

                End Select

                Call ReadConferenceIndex (conferenceFile$)

                Cls
                Color 15
                Locate 18, 18: Print "COMPUTING W/L/T RECORDS . . . PLEASE WAIT"

                'Initialization for 1st league
                Erase awayWins, awayLosses
                Erase homeWins, homeLosses
                Erase awayScoreTeam!, awayScoreOpp!
                Erase homeScoreTeam!, homeScoreOpp!
                Erase totAwayWins, totAwayLosses
                Erase totHomeWins, totHomeLosses
                Erase totAwayScoreTeam!, totAwayScoreOpp!
                Erase totHomeScoreTeam!, totHomeScoreOpp!

                'Calculations for 1st league
                For K = 1 To TEAMS_PER_CONFERENCE

                    If memberName$(K) <> "XXX" Then

                        For I = 1 To nbrGames

                            If Left$(locIndicator$(I), 1) = "V" And oppName$(I) = memberName$(K) Then

                                If teamScore(I) > oppScore(I) Then
                                    awayWins(K) = awayWins(K) + 1
                                End If

                                If teamScore(I) < oppScore(I) Then
                                    awayLosses(K) = awayLosses(K) + 1
                                End If

                                awayScoreTeam!(K) = awayScoreTeam!(K) + teamScore(I)
                                awayScoreOpp!(K) = awayScoreOpp!(K) + oppScore(I)

                            End If

                            If (Left$(locIndicator$(I), 1) = "H" Or Left$(locIndicator$(I), 1) = "N") And oppName$(I) = memberName$(K) Then

                                If teamScore(I) > oppScore(I) Then
                                    homeWins(K) = homeWins(K) + 1
                                End If

                                If teamScore(I) < oppScore(I) Then
                                    homeLosses(K) = homeLosses(K) + 1
                                End If

                                homeScoreTeam!(K) = homeScoreTeam!(K) + teamScore(I)
                                homeScoreOpp!(K) = homeScoreOpp!(K) + oppScore(I)

                            End If

                        Next I

                    End If

                Next K

                'Generate output
                If VPP <> 1 Then
                    Call Hd2HdColl_Report(team$, printChoice)
                Else
                    Call Hd2HdColl_Screen(team$)
                End If

                Cls

                Call TITLE_UTILS

                Call MenuBox(8, 15, 44, 6, 15, 1)

                Do

                    Locate 10, 15

                    Color 3: Locate , 22: Print "SELECT OPTION:"
                    Color 7: Locate , 22: Print "VIEW ";
                    Color 15: Print "S";
                    Color 7: Print "AME TEAM AGAIN"
                    Color 7: Locate , 22: Print "VIEW ";
                    Color 15: Print "A";
                    Color 7: Print "NOTHER TEAM (SAME CONFERENCE)"
                    Color 15: Locate , 22: Print "R";
                    Color 7: Print "ETURN TO OPTIONS"

                    K$ = GetKeyPress$

                    Select Case UCase$(K$)

                        Case "A"
                            sameConf = 1
                            newTeam = 1
                            RDD = 0

                        Case "R"
                            sameConf = 0
                            newTeam = 0
                            RDD = 0

                        Case "S"
                            sameConf = 1
                            newTeam = 0
                            RDD = 0

                            'Re-initialize for the next loop
                            Erase totAwayLosses, totAwayWins
                            Erase totHomeLosses, totHomeWins
                            Erase totAwayScoreTeam!, totAwayScoreOpp!
                            Erase totHomeScoreTeam!, totHomeScoreOpp!

                    End Select

                Loop Until UCase$(K$) = "A" Or UCase$(K$) = "R" Or UCase$(K$) = "S" Or K$ = Chr$(27)

            End If 'Done checking for stats file

        End If

    Loop Until UCase$(K$) = "R" Or K$ = Chr$(27) Or userKey$ = Chr$(27) Or ExitCode% = 27

End Sub


'----------------------------------------
'      Hd2HdColl_Report Subroutine
'----------------------------------------
'This subroutine outputs the calculated
'comparison values to the default printer
Sub Hd2HdColl_Report (teamName$, printDest)

    Shared awayWins(), awayLosses()
    Shared homeWins(), homeLosses()
    Shared awayScoreTeam!(), awayScoreOpp!()
    Shared homeScoreTeam!(), homeScoreOpp!()
    Shared totAwayWins(), totAwayLosses()
    Shared totHomeWins(), totHomeLosses()
    Shared totAwayScoreTeam!(), totAwayScoreOpp!()
    Shared totHomeScoreTeam!(), totHomeScoreOpp!()

    Shared memberName$()

    Shared div1Name$, div2Name$

    If printDest = 0 Then
        destFile$ = _SaveFileDialog$("Save Comparison Report", _CWD$ + "\Comparison", "*.txt", "Text File")
    Else
        destFile$ = "Comparison"
    End If

    Dim divisionNames$(1 To 2)

    divisionNames$(1) = div1Name$
    divisionNames$(2) = div2Name$

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    div = 0

    Do

        RDD = div + 1

        If divisionNames$(RDD) <> "XXX" And divisionNames$(RDD) <> "*" And divisionNames$(RDD) <> "" Then

            Print #1, Tab(50); teamName$
            Print #1,

            Print #1, Tab(22); "--------- HOME ----------"; Tab(54); "--------- AWAY ----------"; Tab(86); "---------- TOTALS ----------"
            Print #1, Tab(22); "W"; Tab(26); "L"; Tab(30); "PCT."; Tab(36); "PF/G"; Tab(43); "PA/G"; 
            Print #1, Tab(54); "W"; Tab(58); "L"; Tab(62); "PCT."; Tab(68); "PF/G"; Tab(75); "PA/G"; 
            Print #1, Tab(86); "W"; Tab(91); "L"; Tab(97); "PCT."; Tab(103); "PF/G"; Tab(110); "PA/G"

            '-- Not TEAMS_PER_DIV ???
            DD = ((RDD - 1) * 10) + 1
            EE = RDD * 10

            Print #1, Tab(2); divisionNames$(RDD)

            For K = DD To EE

                If memberName$(K) <> "XXX" And memberName$(K) <> "*" And memberName$(K) <> teamName$ Then

                    Print #1, memberName$(K);
                    Print #1, Tab(22); Using "#"; homeWins(K);
                    Print #1, Tab(26); Using "#"; homeLosses(K);

                    If homeWins(K) = homeWins(K) + homeLosses(K) And homeWins(K) <> 0 Then
                        Print #1, Tab(28); "1.000";
                    End If
                    If homeWins(K) = 0 Then
                        Print #1, Tab(29); ".000";
                    End If
                    If homeWins(K) > 0 And homeWins(K) < homeWins(K) + homeLosses(K) Then
                        statPCT! = (homeWins(K) / (homeWins(K) + homeLosses(K)))
                        Print #1, Tab(29); Using ".###"; statPCT!;
                    End If

                    If homeScoreTeam!(K) = 0 Then
                        Print #1, Tab(35); "  0.0";
                    End If
                    If homeScoreTeam!(K) > 0 Then
                        Print #1, Tab(35); Using "###.#"; homeScoreTeam!(K) / (homeWins(K) + homeLosses(K));
                    End If

                    If homeScoreOpp!(K) = 0 Then
                        Print #1, Tab(42); "  0.0";
                    End If
                    If homeScoreOpp!(K) > 0 Then
                        Print #1, Tab(42); Using "###.#"; homeScoreOpp!(K) / (homeWins(K) + homeLosses(K));
                    End If

                    Print #1, Tab(54); Using "#"; awayWins(K);
                    Print #1, Tab(58); Using "#"; awayLosses(K);

                    If awayWins(K) = awayWins(K) + awayLosses(K) And awayWins(K) <> 0 Then
                        Print #1, Tab(60); "1.000";
                    End If
                    If awayWins(K) = 0 Then
                        Print #1, Tab(61); ".000";
                    End If
                    If awayWins(K) > 0 And awayWins(K) < awayWins(K) + awayLosses(K) Then
                        statPCT! = (awayWins(K) / (awayWins(K) + awayLosses(K)))
                        Print #1, Tab(61); Using ".###"; statPCT!;
                    End If

                    If awayScoreTeam!(K) = 0 Then
                        Print #1, Tab(67); "  0.0";
                    End If
                    If awayScoreTeam!(K) > 0 Then
                        Print #1, Tab(67); Using "###.#"; awayScoreTeam!(K) / (awayWins(K) + awayLosses(K));
                    End If

                    If awayScoreOpp!(K) = 0 Then
                        Print #1, Tab(74); "  0.0";
                    End If
                    If awayScoreOpp!(K) > 0 Then
                        Print #1, Tab(74); Using "###.#"; awayScoreOpp!(K) / (awayWins(K) + awayLosses(K));
                    End If

                    Print #1, Tab(86); Using "##"; homeWins(K) + awayWins(K);
                    Print #1, Tab(91); Using "##"; homeLosses(K) + awayLosses(K);

                    If homeWins(K) + awayWins(K) = homeWins(K) + awayWins(K) + homeLosses(K) + awayLosses(K) And homeWins(K) + awayWins(K) <> 0 Then
                        Print #1, Tab(95); "1.000";
                    End If
                    If homeWins(K) + awayWins(K) = 0 Then
                        Print #1, Tab(96); ".000";
                    End If
                    If homeWins(K) + awayWins(K) > 0 And (homeWins(K) + awayWins(K)) < (homeWins(K) + awayWins(K) + homeLosses(K) + awayLosses(K)) Then
                        statPCT! = ((homeWins(K) + awayWins(K)) / (homeWins(K) + awayWins(K) + homeLosses(K) + awayLosses(K)))
                        Print #1, Tab(96); Using ".###"; statPCT!;
                    End If

                    If homeScoreTeam!(K) + awayScoreTeam!(K) = 0 Then
                        Print #1, Tab(102); "  0.0";
                    End If
                    If homeScoreTeam!(K) + awayScoreTeam!(K) > 0 Then
                        Print #1, Tab(102); Using "###.#"; (homeScoreTeam!(K) + awayScoreTeam!(K)) / (homeWins(K) + homeLosses(K) + awayWins(K) + awayLosses(K));
                    End If

                    If homeScoreOpp!(K) + awayScoreOpp!(K) = 0 Then
                        Print #1, Tab(109); "  0.0"
                    End If
                    If homeScoreOpp!(K) + awayScoreOpp!(K) > 0 Then
                        Print #1, Tab(109); Using "###.#"; (homeScoreOpp!(K) + awayScoreOpp!(K)) / (homeWins(K) + homeLosses(K) + awayWins(K) + awayLosses(K))
                    End If

                    totHomeWins(RDD) = totHomeWins(RDD) + homeWins(K)
                    totHomeLosses(RDD) = totHomeLosses(RDD) + homeLosses(K)
                    totAwayWins(RDD) = totAwayWins(RDD) + awayWins(K)
                    totAwayLosses(RDD) = totAwayLosses(RDD) + awayLosses(K)
                    totHomeScoreTeam!(RDD) = totHomeScoreTeam!(RDD) + homeScoreTeam!(K)
                    totHomeScoreOpp!(RDD) = totHomeScoreOpp!(RDD) + homeScoreOpp!(K)
                    totAwayScoreTeam!(RDD) = totAwayScoreTeam!(RDD) + awayScoreTeam!(K)
                    totAwayScoreOpp!(RDD) = totAwayScoreOpp!(RDD) + awayScoreOpp!(K)

                End If

            Next K

            ' ** DIVISION TOTALS
            Print #1, Tab(5); "TOTALS";
            Print #1, Tab(21); Using "##"; totHomeWins(RDD);
            Print #1, Tab(25); Using "##"; totHomeLosses(RDD);

            If totHomeWins(RDD) = totHomeWins(RDD) + totHomeLosses(RDD) And totHomeWins(RDD) <> 0 Then
                Print #1, Tab(28); "1.000";
            End If
            If totHomeWins(RDD) = 0 Then
                Print #1, Tab(29); ".000";
            End If
            If totHomeWins(RDD) > 0 And totHomeWins(RDD) < totHomeWins(RDD) + totHomeLosses(RDD) Then
                statPCT! = (totHomeWins(RDD) / (totHomeWins(RDD) + totHomeLosses(RDD)))
                Print #1, Tab(29); Using ".###"; statPCT!;
            End If

            If totHomeScoreTeam!(RDD) = 0 Then
                Print #1, Tab(35); "  0.0";
            End If
            If totHomeScoreTeam!(RDD) > 0 Then
                Print #1, Tab(35); Using "###.#"; totHomeScoreTeam!(RDD) / (totHomeWins(RDD) + totHomeLosses(RDD));
            End If

            If totHomeScoreOpp!(RDD) = 0 Then
                Print #1, Tab(42); "  0.0";
            End If
            If totHomeScoreOpp!(RDD) > 0 Then
                Print #1, Tab(42); Using "###.#"; totHomeScoreOpp!(RDD) / (totHomeWins(RDD) + totHomeLosses(RDD));
            End If

            Print #1, Tab(53); Using "##"; totAwayWins(RDD);
            Print #1, Tab(57); Using "##"; totAwayLosses(RDD);

            If totAwayWins(RDD) = totAwayWins(RDD) + totAwayLosses(RDD) And totAwayWins(RDD) <> 0 Then
                Print #1, Tab(60); "1.000";
            End If
            If totAwayWins(RDD) = 0 Then
                Print #1, Tab(61); ".000";
            End If
            If totAwayWins(RDD) > 0 And totAwayWins(RDD) < totAwayWins(RDD) + totAwayLosses(RDD) Then
                statPCT! = (totAwayWins(RDD) / (totAwayWins(RDD) + totAwayLosses(RDD)))
                Print #1, Tab(61); Using ".###"; statPCT!;
            End If

            If totAwayScoreTeam!(RDD) = 0 Then
                Print #1, Tab(67); "  0.0";
            End If
            If totAwayScoreTeam!(RDD) > 0 Then
                Print #1, Tab(67); Using "###.#"; totAwayScoreTeam!(RDD) / (totAwayWins(RDD) + totAwayLosses(RDD));
            End If

            If totAwayScoreOpp!(RDD) = 0 Then
                Print #1, Tab(74); "  0.0";
            End If
            If totAwayScoreOpp!(RDD) > 0 Then
                Print #1, Tab(74); Using "###.#"; totAwayScoreOpp!(RDD) / (totAwayWins(RDD) + totAwayLosses(RDD));
            End If

            Print #1, Tab(84); Using "###"; totHomeWins(RDD) + totAwayWins(RDD);
            Print #1, Tab(89); Using "###"; totHomeLosses(RDD) + totAwayLosses(RDD);

            If totHomeWins(RDD) + totAwayWins(RDD) = totHomeWins(RDD) + totAwayWins(RDD) + totHomeLosses(RDD) + totAwayLosses(RDD) And totHomeWins(RDD) + totAwayWins(RDD) <> 0 Then
                Print #1, Tab(95); "1.000";
            End If
            If totHomeWins(RDD) + totAwayWins(RDD) = 0 Then
                Print #1, Tab(96); ".000";
            End If
            If totHomeWins(RDD) + totAwayWins(RDD) > 0 And totHomeWins(RDD) + totAwayWins(RDD) < totHomeWins(RDD) + totAwayWins(RDD) + totHomeLosses(RDD) + totAwayLosses(RDD) Then
                statPCT! = ((totHomeWins(RDD) + totAwayWins(RDD)) / (totHomeWins(RDD) + totAwayWins(RDD) + totHomeLosses(RDD) + totAwayLosses(RDD)))
                Print #1, Tab(96); Using ".###"; statPCT!;
            End If

            If totHomeScoreTeam!(RDD) + totAwayScoreTeam!(RDD) = 0 Then
                Print #1, Tab(102); "  0.0";
            End If
            If totHomeScoreTeam!(RDD) + totAwayScoreTeam!(RDD) > 0 Then
                Print #1, Tab(102); Using "###.#"; (totHomeScoreTeam!(RDD) + totAwayScoreTeam!(RDD)) / (totHomeWins(RDD) + totHomeLosses(RDD) + totAwayWins(RDD) + totAwayLosses(RDD));
            End If

            If totHomeScoreOpp!(RDD) + totAwayScoreOpp!(RDD) = 0 Then
                Print #1, Tab(109); "  0.0"
            End If
            If totHomeScoreOpp!(RDD) + totAwayScoreOpp!(RDD) > 0 Then
                Print #1, Tab(109); Using "###.#"; (totHomeScoreOpp!(RDD) + totAwayScoreOpp!(RDD)) / (totHomeWins(RDD) + totHomeLosses(RDD) + totAwayWins(RDD) + totAwayLosses(RDD))
            End If

            Print #1,

        End If

        div = div + 1

    Loop Until div > DIVS_PER_CONFERENCE Or divisionNames$(div) = "XXX" Or divisionNames$(div) = "*" Or divisionNames$(div) = ""

    ' ** CONFERENCE TOTALS **
    Print #1,
    Print #1,
    Print #1, Tab(22); "--------- HOME ----------"; Tab(54); "--------- AWAY ----------"; Tab(86); "---------- TOTALS ----------"
    Print #1, Tab(22); "W"; Tab(26); "L"; Tab(30); "PCT."; Tab(36); "PF/G"; Tab(43); "PA/G"; 
    Print #1, Tab(54); "W"; Tab(58); "L"; Tab(62); "PCT."; Tab(68); "PF/G"; Tab(75); "PA/G"; 
    Print #1, Tab(86); "W"; Tab(91); "L"; Tab(97); "PCT."; Tab(103); "PF/G"; Tab(110); "PA/G"
    Print #1,

    For W = 1 To DIVS_PER_CONFERENCE

        If divisionNames$(W) <> "XXX" And divisionNames$(W) <> "*" And divisionNames$(W) <> "" Then

            Print #1, Tab(2); divisionNames$(W);
            Print #1, Tab(21); Using "##"; totHomeWins(W);
            Print #1, Tab(25); Using "##"; totHomeLosses(W);

            If totHomeWins(W) = totHomeWins(W) + totHomeLosses(W) And totHomeWins(W) <> 0 Then
                Print #1, Tab(28); "1.000";
            End If
            If totHomeWins(W) = 0 Then
                Print #1, Tab(29); ".000";
            End If
            If totHomeWins(W) > 0 And totHomeWins(W) < totHomeWins(W) + totHomeLosses(W) Then
                statPCT! = (totHomeWins(W) / (totHomeWins(W) + totHomeLosses(W)))
                Print #1, Tab(29); Using ".###"; statPCT!;
            End If

            If totHomeScoreTeam!(W) = 0 Then
                Print #1, Tab(35); "  0.0";
            End If
            If totHomeScoreTeam!(W) > 0 Then
                Print #1, Tab(35); Using "###.#"; totHomeScoreTeam!(W) / (totHomeWins(W) + totHomeLosses(W));
            End If

            If totHomeScoreOpp!(W) = 0 Then
                Print #1, Tab(42); "  0.0";
            End If
            If totHomeScoreOpp!(W) > 0 Then
                Print #1, Tab(42); Using "###.#"; totHomeScoreOpp!(W) / (totHomeWins(W) + totHomeLosses(W));
            End If

            Print #1, Tab(53); Using "##"; totAwayWins(W);
            Print #1, Tab(57); Using "##"; totAwayLosses(W);

            If totAwayWins(W) = totAwayWins(W) + totAwayLosses(W) And totAwayWins(W) <> 0 Then
                Print #1, Tab(60); "1.000";
            End If
            If totAwayWins(W) = 0 Then
                Print #1, Tab(61); ".000";
            End If
            If totAwayWins(W) > 0 And totAwayWins(W) < totAwayWins(W) + totAwayLosses(W) Then
                statPCT! = (totAwayWins(W) / (totAwayWins(W) + totAwayLosses(W)))
                Print #1, Tab(61); Using ".###"; statPCT!;
            End If

            If totAwayScoreTeam!(W) = 0 Then
                Print #1, Tab(67); "  0.0";
            End If
            If totAwayScoreTeam!(W) > 0 Then
                Print #1, Tab(67); Using "###.#"; totAwayScoreTeam!(W) / (totAwayWins(W) + totAwayLosses(W));
            End If

            If totAwayScoreOpp!(W) = 0 Then
                Print #1, Tab(74); "  0.0";
            End If
            If totAwayScoreOpp!(W) > 0 Then
                Print #1, Tab(74); Using "###.#"; totAwayScoreOpp!(W) / (totAwayWins(W) + totAwayLosses(W));
            End If

            Print #1, Tab(84); Using "###"; totHomeWins(W) + totAwayWins(W);
            Print #1, Tab(89); Using "###"; totHomeLosses(W) + totAwayLosses(W);

            If totHomeWins(W) + totAwayWins(W) = totHomeWins(W) + totAwayWins(W) + totHomeLosses(W) + totAwayLosses(W) And totHomeWins(W) + totAwayWins(W) <> 0 Then
                Print #1, Tab(95); "1.000";
            End If
            If (totHomeWins(W) + totAwayWins(W)) = 0 Then
                Print #1, Tab(96); ".000";
            End If
            If (totHomeWins(W) + totAwayWins(W)) > 0 And totHomeWins(W) + totAwayWins(W) < totHomeWins(W) + totAwayWins(W) + totHomeLosses(W) + totAwayLosses(W) Then
                statPCT! = ((totHomeWins(W) + totAwayWins(W)) / (totHomeWins(W) + totAwayWins(W) + totHomeLosses(W) + totAwayLosses(W)))
                Print #1, Tab(96); Using ".###"; statPCT!;
            End If

            If totHomeScoreTeam!(W) + totAwayScoreTeam!(W) = 0 Then
                Print #1, Tab(102); "  0.0";
            End If
            If totHomeScoreTeam!(W) + totAwayScoreTeam!(W) > 0 Then
                Print #1, Tab(102); Using "###.#"; (totHomeScoreTeam!(W) + totAwayScoreTeam!(W)) / (totHomeWins(W) + totHomeLosses(W) + totAwayWins(W) + totAwayLosses(W));
            End If

            If totHomeScoreOpp!(W) + totAwayScoreOpp!(W) = 0 Then
                Print #1, Tab(109); "  0.0";
            End If
            If totHomeScoreOpp!(W) + totAwayScoreOpp!(W) > 0 Then
                Print #1, Tab(109); Using "###.#"; (totHomeScoreOpp!(W) + totAwayScoreOpp!(W)) / (totHomeWins(W) + totHomeLosses(W) + totAwayWins(W) + totAwayLosses(W))
            End If

        End If

    Next W

    THWW = 0
    THLL = 0
    TAWW = 0
    TALL = 0

    For I = 1 To DIVS_PER_CONFERENCE
        THWW = THWW + totHomeWins(I)
        THLL = THLL + totHomeLosses(I)
        TAWW = TAWW + totAwayWins(I)
        TALL = TALL + totAwayLosses(I)
    Next

    TR11 = 0
    TR22 = 0
    TR33 = 0
    TR44 = 0

    For I = 1 To DIVS_PER_CONFERENCE
        TR11 = TR11 + totHomeScoreTeam!(I)
        TR22 = TR22 + totHomeScoreOpp!(I)
        TR33 = TR33 + totAwayScoreTeam!(I)
        TR44 = TR44 + totAwayScoreOpp!(I)
    Next

    Print #1, Tab(5); "TOTALS";
    Print #1, Tab(20); Using "###"; THWW;
    Print #1, Tab(24); Using "###"; THLL;

    If THWW = THWW + THLL And THWW <> 0 Then
        Print #1, Tab(28); "1.000";
    End If
    If THWW = 0 Then
        Print #1, Tab(29); ".000";
    End If
    If THWW > 0 And THWW < THWW + THLL Then
        statPCT! = THWW / (THWW + THLL)
        Print #1, Tab(29); Using ".###"; statPCT!;
    End If

    If TR11 = 0 Then
        Print #1, Tab(35); "  0.0";
    End If
    If TR11 > 0 Then
        Print #1, Tab(35); Using "###.#"; TR11 / (THWW + THLL);
    End If
    If TR22 = 0 Then
        Print #1, Tab(42); "  0.0";
    End If
    If TR22 > 0 Then
        Print #1, Tab(42); Using "###.#"; TR22 / (THWW + THLL);
    End If

    Print #1, Tab(52); Using "###"; TAWW;
    Print #1, Tab(56); Using "###"; TALL;

    If TAWW = TAWW + TALL And TAWW <> 0 Then
        Print #1, Tab(60); "1.000";
    End If
    If TAWW = 0 Then
        Print #1, Tab(61); ".000";
    End If
    If TAWW > 0 And TAWW < TAWW + TALL Then
        statPCT! = TAWW / (TAWW + TALL)
        Print #1, Tab(61); Using ".###"; statPCT!;
    End If

    If TR33 = 0 Then
        Print #1, Tab(67); "  0.0";
    End If
    If TR33 > 0 Then
        Print #1, Tab(67); Using "###.#"; TR33 / (TAWW + TALL);
    End If
    If TR44 = 0 Then
        Print #1, Tab(74); "  0.0";
    End If
    If TR44 > 0 Then
        Print #1, Tab(74); Using "###.#"; TR44 / (TAWW + TALL);
    End If

    Print #1, Tab(84); Using "###"; THWW + TAWW;
    Print #1, Tab(89); Using "###"; THLL + TALL;

    If THWW + TAWW = THWW + TAWW + THLL + TALL And THWW + TAWW <> 0 Then
        Print #1, Tab(95); "1.000";
    End If
    If THWW + TAWW = 0 Then
        Print #1, Tab(96); ".000";
    End If
    If THWW + TAWW > 0 And THWW + TAWW < THWW + TAWW + THLL + TALL Then
        statPCT! = (THWW + TAWW) / (THWW + TAWW + THLL + TALL)
        Print #1, Tab(96); Using ".###"; statPCT!;
    End If

    If TR11 + TR33 = 0 Then
        Print #1, Tab(102); "  0.0";
    End If
    If TR11 + TR33 > 0 Then
        Print #1, Tab(102); Using "###.#"; (TR11 + TR33) / (THWW + THLL + TAWW + TALL);
    End If
    If TR22 + TR44 = 0 Then
        Print #1, Tab(109); "  0.0";
    End If
    If TR22 + TR44 > 0 Then
        Print #1, Tab(109); Using "###.#"; (TR22 + TR44) / (THWW + THLL + TAWW + TALL)
    End If

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
'      Hd2HdColl_Screen Subroutine
'----------------------------------------
'This subroutine outputs the calculated
'comparison values to the end user display
Sub Hd2HdColl_Screen (teamName$)

    Shared teamRatings()

    Shared awayWins(), awayLosses()
    Shared homeWins(), homeLosses()
    Shared awayScoreTeam!(), awayScoreOpp!()
    Shared homeScoreTeam!(), homeScoreOpp!()
    Shared totAwayWins(), totAwayLosses()
    Shared totHomeWins(), totHomeLosses()
    Shared totAwayScoreTeam!(), totAwayScoreOpp!()
    Shared totHomeScoreTeam!(), totHomeScoreOpp!()

    Shared memberName$()

    Shared div1Name$, div2Name$

    Dim divisionNames$(1 To 4)

    Color 15, 0

    divisionNames$(1) = div1Name$
    divisionNames$(2) = div2Name$

    div = 0

    Do

        RDD = div + 1

        If divisionNames$(RDD) <> "XXX" And divisionNames$(RDD) <> "*" And divisionNames$(RDD) <> "" Then

            Cls

            Locate 2, 35
            Color teamRatings(0), teamRatings(1): Print teamName$
            Color 3, 0

            'Table header
            Locate 4
            Print Tab(15); String$(7, 196); " HOME "; String$(8, 196); Tab(36); Chr$(194); String$(7, 196); " AWAY "; String$(8, 196); Tab(58); Chr$(194); String$(6, 196); " TOTALS "; String$(7, 196)
            Locate 5
            Print Tab(15); "W"; Tab(17); "L"; Tab(21); "PCT."; Tab(26); "PF/G"; Tab(32); "PA/G"; Tab(36); Chr$(179); 
            Print Tab(37); "W"; Tab(39); "L"; Tab(43); "PCT."; Tab(48); "PF/G"; Tab(54); "PA/G"; Tab(58); Chr$(179); 
            Print Tab(59); "W"; Tab(61); "L"; Tab(65); "PCT."; Tab(70); "PF/G"; Tab(76); "PA/G"

            Color 11
            Print Left$(divisionNames$(RDD), 15);

            Color 3
            Print Tab(36); Chr$(179); Tab(58); Chr$(179)

            DD = ((RDD - 1) * TEAMS_PER_DIV) + 1
            EE = RDD * TEAMS_PER_DIV

            For K = DD To EE

                If memberName$(K) <> "XXX" And memberName$(K) <> "*" And memberName$(K) <> teamName$ Then

                    Color 15
                    Locate , 1: Print Left$(memberName$(K), 13);

                    Color 7
                    Locate , 15: Print Using "#"; homeWins(K);
                    Locate , 17: Print Using "#"; homeLosses(K);

                    If homeWins(K) = homeWins(K) + homeLosses(K) And homeWins(K) <> 0 Then
                        Locate , 19: Print "1.000";
                    End If
                    If homeWins(K) = 0 Then
                        Locate , 20: Print ".000";
                    End If
                    If homeWins(K) > 0 And homeWins(K) < homeWins(K) + homeLosses(K) Then
                        statPCT! = homeWins(K) / (homeWins(K) + homeLosses(K))
                        Locate , 20: Print Using ".###"; statPCT!;
                    End If

                    If homeScoreTeam!(K) = 0 Then
                        Locate , 25: Print "  0.0";
                    End If
                    If homeScoreTeam!(K) > 0 Then
                        Locate , 25: Print Using "###.#"; homeScoreTeam!(K) / (homeWins(K) + homeLosses(K));
                    End If

                    If homeScoreOpp!(K) = 0 Then
                        Locate , 31: Print "  0.0";
                    End If
                    If homeScoreOpp!(K) > 0 Then
                        Locate , 31: Print Using "###.#"; homeScoreOpp!(K) / (homeWins(K) + homeLosses(K));
                    End If

                    Color 3
                    Print Tab(36); Chr$(179);

                    Color 7
                    Locate , 37: Print Using "#"; awayWins(K);
                    Locate , 39: Print Using "#"; awayLosses(K);

                    If awayWins(K) = awayWins(K) + awayLosses(K) And awayWins(K) <> 0 Then
                        Locate , 41: Print "1.000";
                    End If
                    If awayWins(K) = 0 Then
                        Locate , 42: Print ".000";
                    End If
                    If awayWins(K) > 0 And awayWins(K) < awayWins(K) + awayLosses(K) Then
                        statPCT! = (awayWins(K) / (awayWins(K) + awayLosses(K)))
                        Locate , 42: Print Using ".###"; statPCT!;
                    End If

                    If awayScoreTeam!(K) = 0 Then
                        Locate , 47: Print "  0.0";
                    End If
                    If awayScoreTeam!(K) > 0 Then
                        Locate , 47: Print Using "###.#"; awayScoreTeam!(K) / (awayWins(K) + awayLosses(K));
                    End If

                    If awayScoreOpp!(K) = 0 Then
                        Locate , 53: Print "  0.0";
                    End If
                    If awayScoreOpp!(K) > 0 Then
                        Locate , 53: Print Using "###.#"; awayScoreOpp!(K) / (awayWins(K) + awayLosses(K));
                    End If

                    Color 3
                    Print Tab(58); Chr$(179);

                    Color 7
                    Locate , 59: Print Using "##"; homeWins(K) + awayWins(K);
                    Locate , 61: Print Using "##"; homeLosses(K) + awayLosses(K);

                    If (homeWins(K) + awayWins(K)) = (homeWins(K) + awayWins(K) + homeLosses(K) + awayLosses(K)) And (homeWins(K) + awayWins(K)) <> 0 Then
                        Locate , 63: Print "1.000";
                    End If
                    If (homeWins(K) + awayWins(K)) = 0 Then
                        Locate , 64: Print ".000";
                    End If
                    If (homeWins(K) + awayWins(K)) > 0 And (homeWins(K) + awayWins(K)) < (homeWins(K) + awayWins(K) + homeLosses(K) + awayLosses(K)) Then
                        statPCT! = (homeWins(K) + awayWins(K)) / (homeWins(K) + awayWins(K) + homeLosses(K) + awayLosses(K))
                        Locate , 64: Print Using ".###"; statPCT!;
                    End If

                    If homeScoreTeam!(K) + awayScoreTeam!(K) = 0 Then
                        Locate , 69: Print "  0.0";
                    End If
                    If homeScoreTeam!(K) + awayScoreTeam!(K) > 0 Then
                        Locate , 69: Print Using "###.#"; (homeScoreTeam!(K) + awayScoreTeam!(K)) / (homeWins(K) + homeLosses(K) + awayWins(K) + awayLosses(K));
                    End If

                    If homeScoreOpp!(K) + awayScoreOpp!(K) = 0 Then
                        Locate , 75: Print "  0.0"
                    End If
                    If homeScoreOpp!(K) + awayScoreOpp!(K) > 0 Then
                        Locate , 75: Print Using "###.#"; (homeScoreOpp!(K) + awayScoreOpp!(K)) / (homeWins(K) + homeLosses(K) + awayWins(K) + awayLosses(K))
                    End If

                    Color 3
                    Print Tab(36); Chr$(179); Tab(58); Chr$(179);

                    totHomeWins(RDD) = totHomeWins(RDD) + homeWins(K)
                    totHomeLosses(RDD) = totHomeLosses(RDD) + homeLosses(K)
                    totAwayWins(RDD) = totAwayWins(RDD) + awayWins(K)
                    totAwayLosses(RDD) = totAwayLosses(RDD) + awayLosses(K)
                    totHomeScoreTeam!(RDD) = totHomeScoreTeam!(RDD) + homeScoreTeam!(K)
                    totHomeScoreOpp!(RDD) = totHomeScoreOpp!(RDD) + homeScoreOpp!(K)
                    totAwayScoreTeam!(RDD) = totAwayScoreTeam!(RDD) + awayScoreTeam!(K)
                    totAwayScoreOpp!(RDD) = totAwayScoreOpp!(RDD) + awayScoreOpp!(K)

                End If

            Next K

            ' ** DIVISION TOTALS
            Print

            Color 11
            Locate , 14: Print Using "##"; totHomeWins(RDD);

            If totHomeWins(RDD) = totHomeWins(RDD) + totHomeLosses(RDD) And totHomeWins(RDD) <> 0 Then
                Locate , 19: Print "1.000";
            End If
            If totHomeWins(RDD) = 0 Then
                Locate , 20: Print ".000";
            End If
            If totHomeWins(RDD) > 0 And totHomeWins(RDD) < totHomeWins(RDD) + totHomeLosses(RDD) Then
                statPCT! = (totHomeWins(RDD) / (totHomeWins(RDD) + totHomeLosses(RDD)))
                Locate , 20: Print Using ".###"; statPCT!;
            End If

            If totHomeScoreOpp!(RDD) < 1 Then
                Locate , 32: Print " 0.0";
            End If
            If totHomeScoreOpp!(RDD) > 0 Then
                Locate , 31: Print Using "###.#"; totHomeScoreOpp!(RDD) / (totHomeWins(RDD) + totHomeLosses(RDD));
            End If

            Color 11
            Locate , 38: Print Using "##"; totAwayLosses(RDD);

            If totAwayScoreTeam!(RDD) < 1 Then
                Locate , 48: Print " 0.0";
            End If
            If totAwayScoreTeam!(RDD) > 0 Then
                Locate , 47: Print Using "###.#"; totAwayScoreTeam!(RDD) / (totAwayWins(RDD) + totAwayLosses(RDD));
            End If

            Locate , 57: Print Using "###"; totHomeWins(RDD) + totAwayWins(RDD);
            Color 11

            If totHomeWins(RDD) + totAwayWins(RDD) = totHomeWins(RDD) + totHomeLosses(RDD) + totAwayWins(RDD) + totAwayLosses(RDD) And totHomeWins(RDD) + totAwayWins(RDD) <> 0 Then
                Locate , 63: Print "1.000";
            End If
            If totHomeWins(RDD) + totAwayWins(RDD) = 0 Then
                Locate , 64: Print ".000";
            End If
            If totHomeWins(RDD) + totAwayWins(RDD) > 0 And totHomeWins(RDD) + totAwayWins(RDD) < totHomeWins(RDD) + totAwayWins(RDD) + totHomeLosses(RDD) + totAwayLosses(RDD) Then
                statPCT! = (totHomeWins(RDD) + totAwayWins(RDD)) / (totHomeWins(RDD) + totAwayWins(RDD) + totHomeLosses(RDD) + totAwayLosses(RDD))
                Locate , 64: Print Using ".###"; statPCT!;
            End If

            If totHomeScoreOpp!(RDD) + totAwayScoreOpp!(RDD) < 1 Then
                Locate , 76: Print " 0.0"
            End If
            If totHomeScoreOpp!(RDD) + totAwayScoreOpp!(RDD) > 0 Then
                Locate , 75: Print Using "###.#"; (totHomeScoreOpp!(RDD) + totAwayScoreOpp!(RDD)) / (totHomeWins(RDD) + totHomeLosses(RDD) + totAwayWins(RDD) + totAwayLosses(RDD))
            End If

            Locate , 5: Print "TOTALS";

            If totHomeScoreTeam!(RDD) < 1 Then
                Locate , 26: Print " 0.0";
            End If
            If totHomeScoreTeam!(RDD) > 0 Then
                Locate , 25: Print Using "###.#"; totHomeScoreTeam!(RDD) / (totHomeWins(RDD) + totHomeLosses(RDD));
            End If

            Color 11

            If totAwayWins(RDD) = totAwayWins(RDD) + totAwayLosses(RDD) And totAwayWins(RDD) <> 0 Then
                Locate , 41: Print "1.000";
            End If
            If totAwayWins(RDD) = 0 Then
                Locate , 42: Print ".000";
            End If
            If totAwayWins(RDD) > 0 And totAwayWins(RDD) < totAwayWins(RDD) + totAwayLosses(RDD) Then
                statPCT! = (totAwayWins(RDD) / (totAwayWins(RDD) + totAwayLosses(RDD)))
                Locate , 42: Print Using ".###"; statPCT!;
            End If

            If totAwayScoreOpp!(RDD) < 1 Then
                Locate , 54: Print " 0.0";
            End If
            If totAwayScoreOpp!(RDD) > 0 Then
                statPCT! = totAwayScoreOpp!(RDD) / (totAwayWins(RDD) + totAwayLosses(RDD))
                Locate , 53: Print Using "###.#"; statPCT!;
            End If

            If totHomeScoreTeam!(RDD) + totAwayScoreTeam!(RDD) = 0 Then
                Locate , 70: Print " 0.0";
            End If
            If totHomeScoreTeam!(RDD) + totAwayScoreTeam!(RDD) > 0 Then
                statPCT! = (totHomeScoreTeam!(RDD) + totAwayScoreTeam!(RDD)) / (totHomeWins(RDD) + totHomeLosses(RDD) + totAwayWins(RDD) + totAwayLosses(RDD))
                Locate , 69: Print Using "###.#"; statPCT!;
            End If

            Locate , 16: Print Using "##"; totHomeLosses(RDD);
            Locate , 36: Print Using "##"; totAwayWins(RDD);
            Locate , 59: Print Using "###"; totHomeLosses(RDD) + totAwayLosses(RDD)

            Color 14
            Locate LAST_ROW - 2, 5: Print "HIT ANY KEY TO CONTINUE"

            I$ = GetKeyPress$
            
        End If

        div = div + 1

    Loop Until div > DIVS_PER_CONFERENCE Or divisionNames$(div) = "XXX" Or divisionNames$(div) = "*" Or divisionNames$(div) = ""

    ' ** CONFERENCE TOTALS **
    Cls

    Locate 2, 35
    Color teamRatings(0), teamRatings(1): Print teamName$
    Color 3, 0

    Locate 4
    Print Tab(9); String$(8, 196); " HOME "; String$(9, 196); Tab(32); Chr$(194); String$(8, 196); " AWAY "; String$(9, 196); Tab(56); Chr$(194); String$(7, 196); " TOTALS "; String$(8, 196)
    Locate 5
    Print Tab(10); "W"; Tab(13); "L"; Tab(17); "PCT."; Tab(22); "PF/G"; Tab(28); "PA/G"; Tab(32); Chr$(179); 
    Print Tab(34); "W"; Tab(37); "L"; Tab(41); "PCT."; Tab(46); "PF/G"; Tab(52); "PA/G"; Tab(56); Chr$(179); 
    Print Tab(58); "W"; Tab(61); "L"; Tab(65); "PCT."; Tab(70); "PF/G"; Tab(76); "PA/G"

    Color 3
    Print Tab(32); Chr$(179); Tab(56); Chr$(179)

    For W = 1 To DIVS_PER_CONFERENCE

        If divisionNames$(W) <> "XXX" And divisionNames$(W) <> "*" And divisionNames$(W) <> "" Then

            Color 15

            'We have to limit this to 8 because we start stats on column 7
            Locate , 1: Print Left$(divisionNames$(W), 8);

            Color 7
            Locate , 9: Print Using "##"; totHomeWins(W);
            Locate , 12: Print Using "##"; totHomeLosses(W);

            If totHomeWins(W) = totHomeWins(W) + totHomeLosses(W) And totHomeWins(W) <> 0 Then
                Locate , 15: Print "1.000";
            End If
            If totHomeWins(W) = 0 Then
                Locate , 16: Print ".000";
            End If
            If totHomeWins(W) > 0 And totHomeWins(W) < totHomeWins(W) + totHomeLosses(W) Then
                statPCT! = (totHomeWins(W) / (totHomeWins(W) + totHomeLosses(W)))
                Locate , 16: Print Using ".###"; statPCT!;
            End If

            If totHomeScoreTeam!(W) = 0 Then
                Locate , 21: Print "  0.0";
            End If
            If totHomeScoreTeam!(W) > 0 Then
                Locate , 21: Print Using "###.#"; totHomeScoreTeam!(W) / (totHomeWins(W) + totHomeLosses(W));
            End If

            If totHomeScoreOpp!(W) = 0 Then
                Locate , 27: Print "  0.0";
            End If
            If totHomeScoreOpp!(W) > 0 Then
                Locate , 27: Print Using "###.#"; totHomeScoreOpp!(W) / (totHomeWins(W) + totHomeLosses(W));
            End If

            Color 3
            Print Tab(32); Chr$(179);

            Color 7
            Locate , 33: Print Using "##"; totAwayWins(W);
            Locate , 36: Print Using "##"; totAwayLosses(W);

            If totAwayWins(W) = totAwayWins(W) + totAwayLosses(W) And totAwayWins(W) <> 0 Then
                Locate , 39: Print "1.000";
            End If
            If totAwayWins(W) = 0 Then
                Locate , 40: Print ".000";
            End If
            If totAwayWins(W) > 0 And totAwayWins(W) < totAwayWins(W) + totAwayLosses(W) Then
                statPCT! = (totAwayWins(W) / (totAwayWins(W) + totAwayLosses(W)))
                Locate , 40: Print Using ".###"; statPCT!;
            End If

            If totAwayScoreTeam!(W) = 0 Then
                Locate , 45: Print "  0.0";
            End If
            If totAwayScoreTeam!(W) > 0 Then
                Locate , 45: Print Using "###.#"; totAwayScoreTeam!(W) / (totAwayWins(W) + totAwayLosses(W));
            End If

            If totAwayScoreOpp!(W) = 0 Then
                Locate , 51: Print "  0.0";
            End If
            If totAwayScoreOpp!(W) > 0 Then
                Locate , 51: Print Using "###.#"; totAwayScoreOpp!(W) / (totAwayWins(W) + totAwayLosses(W));
            End If

            Color 3
            Print Tab(56); Chr$(179);

            Color 7
            Locate , 57: Print Using "##"; totHomeWins(W) + totAwayWins(W);
            Locate , 60: Print Using "##"; totHomeLosses(W) + totAwayLosses(W);

            If totHomeWins(W) + totAwayWins(W) = totHomeWins(W) + totAwayWins(W) + totHomeLosses(W) + totAwayLosses(W) And totHomeWins(W) + totAwayWins(W) <> 0 Then
                Locate , 63: Print "1.000";
            End If
            If (totHomeWins(W) + totAwayWins(W)) = 0 Then
                Locate , 64: Print ".000";
            End If
            If (totHomeWins(W) + totAwayWins(W)) > 0 And totHomeWins(W) + totAwayWins(W) < totHomeWins(W) + totAwayWins(W) + totHomeLosses(W) + totAwayLosses(W) Then
                statPCT! = ((totHomeWins(W) + totAwayWins(W)) / (totHomeWins(W) + totAwayWins(W) + totHomeLosses(W) + totAwayLosses(W)))
                Locate , 64: Print Using ".###"; statPCT!;
            End If

            If totHomeScoreTeam!(W) + totAwayScoreTeam!(W) = 0 Then
                Locate , 69: Print "  0.0";
            End If
            If totHomeScoreTeam!(W) + totAwayScoreTeam!(W) > 0 Then
                Locate , 69: Print Using "###.#"; (totHomeScoreTeam!(W) + totAwayScoreTeam!(W)) / (totHomeWins(W) + totHomeLosses(W) + totAwayWins(W) + totAwayLosses(W));
            End If

            If totHomeScoreOpp!(W) + totAwayScoreOpp!(W) = 0 Then
                Locate , 75: Print "  0.0"
            End If
            If totHomeScoreOpp!(W) + totAwayScoreOpp!(W) > 0 Then
                Locate , 75: Print Using "###.#"; (totHomeScoreOpp!(W) + totAwayScoreOpp!(W)) / (totHomeWins(W) + totHomeLosses(W) + totAwayWins(W) + totAwayLosses(W))
            End If

            Color 3
            Print Tab(32); Chr$(179); Tab(56); Chr$(179);

        End If

    Next W

    'Calculate TOTALS for Conference
    THWW = 0
    THLL = 0
    TAWW = 0
    TALL = 0

    For I = 1 To DIVS_PER_CONFERENCE
        THWW = THWW + totHomeWins(I)
        THLL = THLL + totHomeLosses(I)
        TAWW = TAWW + totAwayWins(I)
        TALL = TALL + totAwayLosses(I)
    Next

    TR11 = 0
    TR22 = 0
    TR33 = 0
    TR44 = 0

    For I = 1 To DIVS_PER_CONFERENCE
        TR11 = TR11 + totHomeScoreTeam!(I)
        TR22 = TR22 + totHomeScoreOpp!(I)
        TR33 = TR33 + totAwayScoreTeam!(I)
        TR44 = TR44 + totAwayScoreOpp!(I)
    Next

    'Division Totals
    Color 11
    Print "TOTALS";

    Locate , 9: Print Using "##"; THWW;
    Locate , 12: Print Using "##"; THLL;

    If THWW = THWW + THLL And THWW <> 0 Then
        Locate , 15: Print "1.000";
    End If
    If THWW = 0 Then
        Locate , 16: Print ".000";
    End If
    If THWW > 0 And THWW < THWW + THLL Then
        statPCT! = THWW / (THWW + THLL)
        Locate , 16: Print Using ".###"; statPCT!
    End If

    If TR11 = 0 Then
        Locate , 21: Print "  0.0";
    End If
    If TR11 > 0 Then
        Locate , 21: Print Using "###.#"; TR11 / (THWW + THLL);
    End If

    If TR22 = 0 Then
        Locate , 27: Print "  0.0";
    End If
    If TR22 > 0 Then
        Locate , 27: Print Using "###.#"; TR22 / (THWW + THLL);
    End If

    Color 3
    Print Tab(32); Chr$(179);

    Color 11
    Locate , 33: Print Using "##"; TAWW;
    Locate , 36: Print Using "##"; TALL;

    If TAWW = TAWW + TALL And TAWW <> 0 Then
        Locate , 39: Print "1.000";
    End If
    If TAWW = 0 Then
        Locate , 40: Print ".000";
    End If
    If TAWW > 0 And TAWW < TAWW + TALL Then
        statPCT! = TAWW / (TAWW + TALL)
        Locate , 40: Print Using ".###"; statPCT!;
    End If

    If TR33 = 0 Then
        Locate , 45: Print "  0.0";
    End If
    If TR33 > 0 Then
        Locate , 45: Print Using "###.#"; TR33 / (TAWW + TALL);
    End If
    If TR44 = 0 Then
        Locate , 51: Print "  0.0";
    End If
    If TR44 > 0 Then
        Locate , 51: Print Using "###.#"; TR44 / (TAWW + TALL);
    End If

    Color 3
    Print Tab(56); Chr$(179);

    Color 11
    Locate , 57: Print Using "##"; THWW + TAWW;
    Locate , 60: Print Using "##"; THLL + TALL;

    If THWW + TAWW = THWW + TAWW + THLL + TALL And THWW + TAWW <> 0 Then
        Locate , 63: Print "1.000";
    End If
    If THWW + TAWW = 0 Then
        Locate , 64: Print ".000";
    End If
    If THWW + TAWW > 0 And THWW + TAWW < THWW + TAWW + THLL + TALL Then
        statPCT! = (THWW + TAWW) / (THWW + TAWW + THLL + TALL)
        Locate , 64: Print Using ".###"; statPCT!;
    End If

    If TR11 + TR33 = 0 Then
        Locate , 69: Print "  0.0";
    End If
    If TR11 + TR33 > 0 Then
        Locate , 69: Print Using "###.#"; (TR11 + TR33) / (THWW + THLL + TAWW + TALL);
    End If
    If TR22 + TR44 = 0 Then
        Locate , 75: Print "  0.0"
    End If
    If TR22 + TR44 > 0 Then
        Locate , 75: Print Using "###.#"; (TR22 + TR44) / (THWW + THLL + TAWW + TALL)
    End If

    Color 14
    Locate LAST_ROW - 2, 5: Print "HIT ANY KEY TO CONTINUE";

    I$ = GetKeyPress$

End Sub

