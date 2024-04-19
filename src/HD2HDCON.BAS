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

    Shared AL!(), AW!(), HL!(), HW!()
    Shared R1!(), R2!(), hd2hdR3!(), R4!()
    Shared TAL!(), TAW!(), THL!(), THW!()
    Shared TR1!(), TR2!(), TR3!(), TR4!()

    Shared locIndicator$(), oppName$(), Z1$()

    Dim Z1_HD2HD(MAX_CONFERENCES), Z2_HD2HD$(MAX_CONFERENCES)

    Shared D1$, D2$

    newTeam = 1

    Do

        If newTeam = 1 Then

            teamNbr = 0
            Call ChooseTeam(YN$, NTMS, teamNbr, team$)

            If teamNbr > 0 Then

                statFile$ = nameStatFile$(team$)

                If Not _FileExists(diskPaths$(1) + statFile$ + "S") Then

                    MsgText$ = "NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU"
                    BoxWidth = 24
                    BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                    Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                    Call CSMsgBox(MsgText$, BoxWidth, PASS)
                    I$ = GetKeyPress$
                    Call CSMsgBox("", 0, PASS)

                Else

                    Open diskPaths$(1) + statFile$ + "S" For Input As #1

                    Input #1, nbrGames

                    For J = 1 To MAX_SCHED_STATS:
                        Input #1, teamScore(J)
                    Next

                    For J = 1 To MAX_SCHED_STATS:
                        Input #1, oppName$(J)
                    Next

                    For J = 1 To MAX_SCHED_STATS:
                        Input #1, oppScore(J)
                    Next

                    For J = 1 To MAX_SCHED_STATS:
                        Input #1, locIndicator$(J)
                    Next

                    Close #1

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

            Call MenuBox(8, 25, 24, 5, 15, 1)

            Do

                Locate 10
                Color 7, 1

                Locate , 32: Print "SEND TO ";
                Color 15: Print "F";
                Color 7: Print "ILE"
                Locate , 32: Print "SEND TO ";
                Color 15: Print "P";
                Color 7: Print "RINTER"
                Locate , 32: Print "SEND TO ";
                Color 15: Print "S";
                Color 7: Print "CREEN"

                J$ = GetKeyPress$

                Color 14, 0

            Loop Until UCase$(J$) = "F" Or UCase$(J$) = "P" Or UCase$(J$) = "S" Or J$ = Chr$(27)

            If J$ <> Chr$(27) Then

                If UCase$(J$) = "F" Then
                    printChoice = 0
                End If
                If UCase$(J$) = "P" Then
                    printChoice = 1
                End If
                If UCase$(J$) = "S" Then
                    VPP = 1
                Else
                    VPP = 0
                End If

                Open diskPaths$(2) + conferenceFile$ + ".TMI" For Input As #2

                Input #2, X
                Input #2, conf$
                Input #2, D1$
                Input #2, D2$

                For X = 1 To TEAMS_PER_CONFERENCE
                    Input #2, Z1$(X)
                    Input #2, Z1_HD2HD(X)
                    Input #2, Z2_HD2HD$(X)
                Next

                Close #2

                Color 15
                Locate 18, 18: Print "COMPUTING W/L/T RECORDS . . . PLEASE WAIT"

                'Initialization for 1st league
                Erase AL!, AW!, HL!, HW!
                Erase R1!, R2!, hd2hdR3!, R4!
                Erase TAL!, TAW!, THL!, THW!, TR1!, TR2!, TR3!, TR4!

                'Calculations for 1st league
                For K = 1 To TEAMS_PER_CONFERENCE

                    If Z1$(K) <> "XXX" Then

                        For I = 1 To nbrGames

                            If Left$(locIndicator$(I), 1) = "V" And oppName$(I) = Z1$(K) Then

                                If teamScore(I) > oppScore(I) Then
                                    AW!(K) = AW!(K) + 1
                                End If

                                If teamScore(I) < oppScore(I) Then
                                    AL!(K) = AL!(K) + 1
                                End If

                                hd2hdR3!(K) = hd2hdR3!(K) + teamScore(I)
                                R4!(K) = R4!(K) + oppScore(I)

                            End If

                            If (Left$(locIndicator$(I), 1) = "H" Or Left$(locIndicator$(I), 1) = "N") And oppName$(I) = Z1$(K) Then

                                If teamScore(I) > oppScore(I) Then
                                    HW!(K) = HW!(K) + 1
                                End If

                                If teamScore(I) < oppScore(I) Then
                                    HL!(K) = HL!(K) + 1
                                End If

                                R1!(K) = R1!(K) + teamScore(I)
                                R2!(K) = R2!(K) + oppScore(I)

                            End If

                        Next I

                    End If

                Next K

                If VPP <> 1 Then
                    Call Hd2HdColl_Report(team$, printChoice)
                Else
                    Call Hd2HdColl_Screen(team$)
                End If

                Cls

                Call TITLE_UTILS

                'Call MenuBox(8, 23, 39, 5, 15, 1)
                Call MenuBox(8, 15, 44, 6, 15, 1)
                'Call MenuBox(8, 17, 40, 6, 15, 1)

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
                            Erase TAL!, TAW!, THL!, THW!
                            Erase TR1!, TR2!, TR3!, TR4!

                    End Select

                Loop Until UCase$(K$) = "A" Or UCase$(K$) = "R" Or UCase$(K$) = "S" Or K$ = Chr$(27) Or J$ = Chr$(27)

            End If 'Done checking for stats file

        End If

    Loop Until UCase$(K$) = "R" Or K$ = Chr$(27) Or J$ = Chr$(27) Or ExitCode% = 27

End Sub


'----------------------------------------
'      Hd2HdColl_Report Subroutine
'----------------------------------------
'This subroutine outputs the calculated
'comparison values to the default printer
Sub Hd2HdColl_Report (teamName$, printDest)

    Shared D1$, D2$

    Shared teamRatings%()

    Shared Z1$()

    Shared AL!(), AW!(), HL!(), HW!()
    Shared R1!(), R2!(), hd2hdR3!(), R4!()
    Shared TAL!(), TAW!(), THL!(), THW!()
    Shared TR1!(), TR2!(), TR3!(), TR4!()

    If printDest = 0 Then
        destFile$ = _SaveFileDialog$("Save Comparison Report", _CWD$ + "\Comparison", "*.txt", "Text File")
    Else
        destFile$ = "Comparison"
    End If

    DV$(1) = D1$
    DV$(2) = D2$

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    RDD = 1

    Do

        Print #1, Tab(55); teamName$
        Print #1,
        Print #1, Tab(22); "--------- HOME ----------"; Tab(54); "--------- AWAY ----------"; Tab(86); "---------- TOTALS ----------"

        Print #1, Tab(22); "W"; Tab(26); "L"; Tab(30); "PCT."; Tab(36); "PF/G"; Tab(43); "PA/G"; Tab(54); "W"; Tab(58); "L"; Tab(62); "PCT."; Tab(68); "PF/G"; Tab(75); "PA/G"; Tab(86); "W"; Tab(91); "L"; Tab(97); "PCT."; Tab(103); "PF/G"; Tab(110); "PA/G"

        Print #1, Tab(2); DV$(RDD)

        DD = ((RDD - 1) * 10) + 1
        EE = RDD * 10

        'If DV$(RDD) <> "XXX" And DV$(RDD) <> "" Then

        For K = DD To EE

            If Z1$(K) <> "XXX" And Z1$(K) <> teamName$ Then

                Print #1, Z1$(K);
                Print #1, Tab(22); Using "#"; HW!(K);
                Print #1, Tab(26); Using "#"; HL!(K);

                If HW!(K) = HW!(K) + HL!(K) And HW!(K) <> 0 Then Print #1, Tab(28); "1.000";
                If HW!(K) = 0 Then Print #1, Tab(29); ".000";
                If HW!(K) > 0 And HW!(K) < HW!(K) + HL!(K) Then 
                    statPCT! = (HW!(K) / (HW!(K) + HL!(K)))
                    Print #1, Tab(29); Using ".###"; statPCT!;
                End If

                If R1!(K) = 0 Then Print #1, Tab(35); "  0.0";
                If R1!(K) > 0 Then Print #1, Tab(35); Using "###.#"; R1!(K) / (HW!(K) + HL!(K));
                If R2!(K) = 0 Then Print #1, Tab(42); "  0.0";
                If R2!(K) > 0 Then Print #1, Tab(42); Using "###.#"; R2!(K) / (HW!(K) + HL!(K));

                Print #1, Tab(54); Using "#"; AW!(K);
                Print #1, Tab(58); Using "#"; AL!(K);

                If AW!(K) = AW!(K) + AL!(K) And AW!(K) <> 0 Then Print #1, Tab(60); "1.000";
                If AW!(K) = 0 Then Print #1, Tab(61); ".000";
                If AW!(K) > 0 And AW!(K) < AW!(K) + AL!(K) Then 
                    statPCT! = (AW!(K) / (AW!(K) + AL!(K)))
                    Print #1, Tab(61); Using ".###"; statPCT!;
                End If

                If hd2hdR3!(K) = 0 Then Print #1, Tab(67); "  0.0";
                If hd2hdR3!(K) > 0 Then Print #1, Tab(67); Using "###.#"; hd2hdR3!(K) / (AW!(K) + AL!(K));
                If R4!(K) = 0 Then Print #1, Tab(74); "  0.0";
                If R4!(K) > 0 Then Print #1, Tab(74); Using "###.#"; R4!(K) / (AW!(K) + AL!(K));

                Print #1, Tab(86); Using "##"; HW!(K) + AW!(K);
                Print #1, Tab(91); Using "##"; HL!(K) + AL!(K);

                If HW!(K) + AW!(K) = HW!(K) + AW!(K) + HL!(K) + AL!(K) And HW!(K) + AW!(K) <> 0 Then Print #1, Tab(95); "1.000";
                If (HW!(K) + AW!(K)) = 0 Then Print #1, Tab(96); ".000";
                If (HW!(K) + AW!(K)) > 0 And (HW!(K) + AW!(K)) < (HW!(K) + AW!(K) + HL!(K) + AL!(K)) Then 
                    statPCT! = ((HW!(K) + AW!(K)) / (HW!(K) + AW!(K) + HL!(K) + AL!(K)))
                    Print #1, Tab(96); Using ".###"; statPCT!;
                End If
                
                If R1!(K) + hd2hdR3!(K) = 0 Then Print #1, Tab(102); "  0.0";
                If R1!(K) + hd2hdR3!(K) > 0 Then Print #1, Tab(102); Using "###.#"; (R1!(K) + hd2hdR3!(K)) / (HW!(K) + HL!(K) + AW!(K) + AL!(K));
                If R2!(K) + R4!(K) = 0 Then Print #1, Tab(109); "  0.0"
                If R2!(K) + R4!(K) > 0 Then Print #1, Tab(109); Using "###.#"; (R2!(K) + R4!(K)) / (HW!(K) + HL!(K) + AW!(K) + AL!(K))

                THW!(RDD) = THW!(RDD) + HW!(K)
                THL!(RDD) = THL!(RDD) + HL!(K)
                TAW!(RDD) = TAW!(RDD) + AW!(K)
                TAL!(RDD) = TAL!(RDD) + AL!(K)
                TR1!(RDD) = TR1!(RDD) + R1!(K)
                TR2!(RDD) = TR2!(RDD) + R2!(K)
                TR3!(RDD) = TR3!(RDD) + hd2hdR3!(K)
                TR4!(RDD) = TR4!(RDD) + R4!(K)

            End If

        Next K

        Print #1, Tab(5); "TOTALS";
        Print #1, Tab(21); Using "##"; THW!(RDD);
        Print #1, Tab(25); Using "##"; THL!(RDD);

        If THW!(RDD) = THW!(RDD) + THL!(RDD) And THW!(RDD) <> 0 Then Print #1, Tab(28); "1.000";
        If THW!(RDD) = 0 Then Print #1, Tab(29); ".000";
        If THW!(RDD) > 0 And THW!(RDD) < THW!(RDD) + THL!(RDD) Then Print #1, Tab(29); Using ".###"; (THW!(RDD) / (THW!(RDD) + THL!(RDD)));

        If TR1!(RDD) = 0 Then Print #1, Tab(35); "  0.0";
        If TR1!(RDD) > 0 Then Print #1, Tab(35); Using "###.#"; TR1!(RDD) / (THW!(RDD) + THL!(RDD));
        If TR2!(RDD) = 0 Then Print #1, Tab(42); "  0.0";
        If TR2!(RDD) > 0 Then Print #1, Tab(42); Using "###.#"; TR2!(RDD) / (THW!(RDD) + THL!(RDD));

        Print #1, Tab(53); Using "##"; TAW!(RDD);
        Print #1, Tab(57); Using "##"; TAL!(RDD);

        If TAW!(RDD) = TAW!(RDD) + TAL!(RDD) And TAW!(RDD) <> 0 Then Print #1, Tab(60); "1.000";
        If TAW!(RDD) = 0 Then Print #1, Tab(61); ".000";
        If TAW!(RDD) > 0 And TAW!(RDD) < TAW!(RDD) + TAL!(RDD) Then 
            statPCT! = (TAW!(RDD) / (TAW!(RDD) + TAL!(RDD)))
            Print #1, Tab(61); Using ".###"; statPCT!;
        End If
        
        If TR3!(RDD) = 0 Then Print #1, Tab(67); "  0.0";
        If TR3!(RDD) > 0 Then Print #1, Tab(67); Using "###.#"; TR3!(RDD) / (TAW!(RDD) + TAL!(RDD));
        If TR4!(RDD) = 0 Then Print #1, Tab(74); "  0.0";
        If TR4!(RDD) > 0 Then Print #1, Tab(74); Using "###.#"; TR4!(RDD) / (TAW!(RDD) + TAL!(RDD));

        Print #1, Tab(84); Using "###"; THW!(RDD) + TAW!(RDD);
        Print #1, Tab(89); Using "###"; THL!(RDD) + TAL!(RDD);

        If THW!(RDD) + TAW!(RDD) = THW!(RDD) + TAW!(RDD) + THL!(RDD) + TAL!(RDD) And THW!(RDD) + TAW!(RDD) <> 0 Then Print #1, Tab(95); "1.000";
        If THW!(RDD) + TAW!(RDD) = 0 Then Print #1, Tab(96); ".000";
        If THW!(RDD) + TAW!(RDD) > 0 And THW!(RDD) + TAW!(RDD) < THW!(RDD) + TAW!(RDD) + THL!(RDD) + TAL!(RDD) Then 
            statPCT! = ((THW!(RDD) + TAW!(RDD)) / (THW!(RDD) + TAW!(RDD) + THL!(RDD) + TAL!(RDD)))
            Print #1, Tab(96); Using ".###"; statPCT!;
        End If

        If TR1!(RDD) + TR3!(RDD) = 0 Then Print #1, Tab(102); "  0.0";
        If TR1!(RDD) + TR3!(RDD) > 0 Then Print #1, Tab(102); Using "###.#"; (TR1!(RDD) + TR3!(RDD)) / (THW!(RDD) + THL!(RDD) + TAW!(RDD) + TAL!(RDD));
        If TR2!(RDD) + TR4!(RDD) = 0 Then Print #1, Tab(109); "  0.0"
        If TR2!(RDD) + TR4!(RDD) > 0 Then Print #1, Tab(109); Using "###.#"; (TR2!(RDD) + TR4!(RDD)) / (THW!(RDD) + THL!(RDD) + TAW!(RDD) + TAL!(RDD))

        Print #1,

        RDD = RDD + 1

    Loop Until RDD > DIVS_PER_CONFERENCE Or DV$(RDD) = "XXX" Or DV$(RDD) = ""

    Print #1,
    Print #1,
    Print #1, Tab(22); "--------- HOME ----------"; Tab(54); "--------- AWAY ----------"; Tab(86); "---------- TOTALS ----------"
    Print #1, Tab(22); "W"; Tab(26); "L"; Tab(30); "PCT."; Tab(36); "PF/G"; Tab(43); "PA/G"; Tab(54); "W"; Tab(58); "L"; Tab(62); "PCT."; Tab(68); "PF/G"; Tab(75); "PA/G"; Tab(86); "W"; Tab(91); "L"; Tab(97); "PCT."; Tab(103); "PF/G"; Tab(110); "PA/G"
    Print #1,

    For W = 1 To DIVS_PER_CONFERENCE

        If DV$(W) <> "XXX" Then

            Print #1, Tab(2); DV$(W);
            Print #1, Tab(21); Using "##"; THW!(W);
            Print #1, Tab(25); Using "##"; THL!(W);

            If THW!(W) = THW!(W) + THL!(W) And THW!(W) <> 0 Then Print #1, Tab(28); "1.000";
            If THW!(W) = 0 Then Print #1, Tab(29); ".000";
            If THW!(W) > 0 And THW!(W) < THW!(W) + THL!(W) Then 
                statPCT! = (THW!(W) / (THW!(W) + THL!(W)))
                Print #1, Tab(29); Using ".###"; statPCT!;
            End If

            If TR1!(W) = 0 Then Print #1, Tab(35); "  0.0";
            If TR1!(W) > 0 Then Print #1, Tab(35); Using "###.#"; TR1!(W) / (THW!(W) + THL!(W));
            If TR2!(W) = 0 Then Print #1, Tab(42); "  0.0";
            If TR2!(W) > 0 Then Print #1, Tab(42); Using "###.#"; TR2!(W) / (THW!(W) + THL!(W));

            Print #1, Tab(53); Using "##"; TAW!(W);
            Print #1, Tab(57); Using "##"; TAL!(W);

            If TAW!(W) = TAW!(W) + TAL!(W) And TAW!(W) <> 0 Then Print #1, Tab(60); "1.000";
            If TAW!(W) = 0 Then Print #1, Tab(61); ".000";
            If TAW!(W) > 0 And TAW!(W) < TAW!(W) + TAL!(W) Then 
                statPCT! = (TAW!(W) / (TAW!(W) + TAL!(W)))
                Print #1, Tab(61); Using ".###"; statPCT!;
            End If

            If TR3!(W) = 0 Then Print #1, Tab(67); "  0.0";
            If TR3!(W) > 0 Then Print #1, Tab(67); Using "###.#"; TR3!(W) / (TAW!(W) + TAL!(W));
            If TR4!(W) = 0 Then Print #1, Tab(74); "  0.0";
            If TR4!(W) > 0 Then Print #1, Tab(74); Using "###.#"; TR4!(W) / (TAW!(W) + TAL!(W));

            Print #1, Tab(84); Using "###"; THW!(W) + TAW!(W);
            Print #1, Tab(89); Using "###"; THL!(W) + TAL!(W);

            If THW!(W) + TAW!(W) = THW!(W) + TAW!(W) + THL!(W) + TAL!(W) And THW!(W) + TAW!(W) <> 0 Then Print #1, Tab(95); "1.000";
            If (THW!(W) + TAW!(W)) = 0 Then Print #1, Tab(96); ".000";
            If (THW!(W) + TAW!(W)) > 0 And THW!(W) + TAW!(W) < THW!(W) + TAW!(W) + THL!(W) + TAL!(W) Then 
                statPCT! = ((THW!(W) + TAW!(W)) / (THW!(W) + TAW!(W) + THL!(W) + TAL!(W)))
                Print #1, Tab(96); Using ".###"; statPCT!;
            End If

            If TR1!(W) + TR3!(W) = 0 Then Print #1, Tab(102); "  0.0";
            If TR1!(W) + TR3!(W) > 0 Then Print #1, Tab(102); Using "###.#"; (TR1!(W) + TR3!(W)) / (THW!(W) + THL!(W) + TAW!(W) + TAL!(W));
            If TR2!(W) + TR4!(W) = 0 Then Print #1, Tab(109); "  0.0";
            If TR2!(W) + TR4!(W) > 0 Then Print #1, Tab(109); Using "###.#"; (TR2!(W) + TR4!(W)) / (THW!(W) + THL!(W) + TAW!(W) + TAL!(W))

        End If

    Next W

    THWW = 0
    THLL = 0
    TAWW = 0
    TALL = 0

    For I = 1 To 4
        THWW = THWW + THW!(I)
        THLL = THLL + THL!(I)
        TAWW = TAWW + TAW!(I)
        TALL = TALL + TAL!(I)
    Next

    TR11 = 0
    TR22 = 0
    TR33 = 0
    TR44 = 0

    For I = 1 To 4
        TR11 = TR11 + TR1!(I)
        TR22 = TR22 + TR2!(I)
        TR33 = TR33 + TR3!(I)
        TR44 = TR44 + TR4!(I)
    Next

    Print #1, Tab(5); "TOTALS";
    Print #1, Tab(20); Using "###"; THWW;
    Print #1, Tab(24); Using "###"; THLL;

    If THWW = THWW + THLL And THWW <> 0 Then Print #1, Tab(28); "1.000";
    If THWW = 0 Then Print #1, Tab(29); ".000";
    If THWW > 0 And THWW < THWW + THLL Then 
        statPCT! = THWW / (THWW + THLL)
        Print #1, Tab(29); Using ".###"; statPCT!;
    End If

    If TR11 = 0 Then Print #1, Tab(35); "  0.0";
    If TR11 > 0 Then Print #1, Tab(35); Using "###.#"; TR11 / (THWW + THLL);
    If TR22 = 0 Then Print #1, Tab(42); "  0.0";
    If TR22 > 0 Then Print #1, Tab(42); Using "###.#"; TR22 / (THWW + THLL);

    Print #1, Tab(52); Using "###"; TAWW;
    Print #1, Tab(56); Using "###"; TALL;

    If TAWW = TAWW + TALL And TAWW <> 0 Then Print #1, Tab(60); "1.000";
    If TAWW = 0 Then Print #1, Tab(61); ".000";
    If TAWW > 0 And TAWW < TAWW + TALL Then 
        statPCT! = TAWW / (TAWW + TALL)
        Print #1, Tab(61); Using ".###"; statPCT!;
    End If

    If TR33 = 0 Then Print #1, Tab(67); "  0.0";
    If TR33 > 0 Then Print #1, Tab(67); Using "###.#"; TR33 / (TAWW + TALL);
    If TR44 = 0 Then Print #1, Tab(74); "  0.0";
    If TR44 > 0 Then Print #1, Tab(74); Using "###.#"; TR44 / (TAWW + TALL);

    Print #1, Tab(84); Using "###"; THWW + TAWW;
    Print #1, Tab(89); Using "###"; THLL + TALL;

    If THWW + TAWW = THWW + TAWW + THLL + TALL And THWW + TAWW <> 0 Then Print #1, Tab(95); "1.000";
    If THWW + TAWW = 0 Then Print #1, Tab(96); ".000";
    If THWW + TAWW > 0 And THWW + TAWW < THWW + TAWW + THLL + TALL Then 
        statPCT! = (THWW + TAWW) / (THWW + TAWW + THLL + TALL)
        Print #1, Tab(96); Using ".###"; statPCT!;
    End If

    If TR11 + TR33 = 0 Then Print #1, Tab(102); "  0.0";
    If TR11 + TR33 > 0 Then Print #1, Tab(102); Using "###.#"; (TR11 + TR33) / (THWW + THLL + TAWW + TALL);
    If TR22 + TR44 = 0 Then Print #1, Tab(109); "  0.0";
    If TR22 + TR44 > 0 Then Print #1, Tab(109); Using "###.#"; (TR22 + TR44) / (THWW + THLL + TAWW + TALL)

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

    Dim DV$(1 To 4)

    Shared D1$, D2$

    Shared teamRatings%()

    Shared Z1$()

    Shared AL!(), AW!(), HL!(), HW!()
    Shared R1!(), R2!(), hd2hdR3!(), R4!()
    Shared TAL!(), TAW!(), THL!(), THW!()
    Shared TR1!(), TR2!(), TR3!(), TR4!()

    Color 15, 0

    DV$(1) = D1$
    DV$(2) = D2$

    RDD = 0

    Do

        Cls

        Locate 2, 35
        Color teamRatings%(0), teamRatings%(1): Print teamName$
        Color 3, 0

        'Table header
        Locate 4
        Print Tab(15); String$(7, 196); " HOME "; String$(8, 196); Tab(36); Chr$(194); String$(7, 196); " AWAY "; String$(8, 196); Tab(58); Chr$(194); String$(6, 196); " TOTALS "; String$(7, 196)
        Locate 5
        Print Tab(15); "W"; Tab(17); "L"; Tab(21); "PCT."; Tab(26); "PF/G"; Tab(32); "PA/G"; Tab(36); Chr$(179); Tab(37); "W"; Tab(39); "L"; Tab(43); "PCT."; Tab(48); "PF/G"; Tab(54); "PA/G"; Tab(58); Chr$(179); Tab(59); "W"; Tab(61); "L"; Tab(65); "PCT."; Tab(70); "PF/G"; Tab(76); "PA/G"

        DD = ((RDD - 1) * TEAMS_PER_DIV) + 1
        EE = RDD * TEAMS_PER_DIV

        'If DV$(RDD) <> "XXX" And DV$(RDD) <> "" Then
        Color 11
        Print Left$(DV$(RDD), 15);
        'End If

        Color 3: Print Tab(36); Chr$(179); Tab(58); Chr$(179)

        For K = DD To EE

            If Z1$(K) <> "XXX" And Z1$(K) <> "*" And Z1$(K) <> teamName$ Then

                Color 15
                Locate , 1: Print Left$(Z1$(K), 13);

                Color 7
                Locate , 15: Print Using "#"; HW!(K);
                Locate , 17: Print Using "#"; HL!(K);

                If HW!(K) = HW!(K) + HL!(K) And HW!(K) <> 0 Then Locate , 19: Print "1.000";
                If HW!(K) = 0 Then Locate , 20: Print ".000";
                If HW!(K) > 0 And HW!(K) < HW!(K) + HL!(K) Then 
                    statPCT! = (HW!(K) / (HW!(K) + HL!(K)))
                    Locate , 20: Print Using ".###"; statPCT!;
                End If

                If R1!(K) = 0 Then Locate , 25: Print "  0.0";
                If R1!(K) > 0 Then Locate , 25: Print Using "###.#"; R1!(K) / (HW!(K) + HL!(K));
                If R2!(K) = 0 Then Locate , 31: Print "  0.0";
                If R2!(K) > 0 Then Locate , 31: Print Using "###.#"; R2!(K) / (HW!(K) + HL!(K));

                Color 3: Print Tab(36); Chr$(179);

                Color 7
                Locate , 37: Print Using "#"; AW!(K);
                Locate , 39: Print Using "#"; AL!(K);

                If AW!(K) = AW!(K) + AL!(K) And AW!(K) <> 0 Then Locate , 41: Print "1.000";
                If AW!(K) = 0 Then Locate , 42: Print ".000";
                If AW!(K) > 0 And AW!(K) < AW!(K) + AL!(K) Then 
                    statPCT! = (AW!(K) / (AW!(K) + AL!(K)))
                    Locate , 42: Print Using ".###"; statPCT!;
                End If

                If hd2hdR3!(K) = 0 Then Locate , 47: Print "  0.0";
                If hd2hdR3!(K) > 0 Then Locate , 47: Print Using "###.#"; hd2hdR3!(K) / (AW!(K) + AL!(K));
                If R4!(K) = 0 Then Locate , 53: Print "  0.0";
                If R4!(K) > 0 Then Locate , 53: Print Using "###.#"; R4!(K) / (AW!(K) + AL!(K));

                Color 3: Print Tab(58); Chr$(179);

                Color 7
                Locate , 59: Print Using "##"; HW!(K) + AW!(K);
                Locate , 61: Print Using "##"; HL!(K) + AL!(K);

                If (HW!(K) + AW!(K)) = (HW!(K) + AW!(K) + HL!(K) + AL!(K)) And (HW!(K) + AW!(K)) <> 0 Then Locate , 63: Print "1.000";
                If (HW!(K) + AW!(K)) = 0 Then Locate , 64: Print ".000";
                If (HW!(K) + AW!(K)) > 0 And (HW!(K) + AW!(K)) < (HW!(K) + AW!(K) + HL!(K) + AL!(K)) Then 
                    statPCT! = ((HW!(K) + AW!(K)) / (HW!(K) + AW!(K) + HL!(K) + AL!(K)))
                    Locate , 64: Print Using ".###"; statPCT!;
                End If

                If R1!(K) + hd2hdR3!(K) = 0 Then Locate , 69: Print "  0.0";
                If R1!(K) + hd2hdR3!(K) > 0 Then Locate , 69: Print Using "###.#"; (R1!(K) + hd2hdR3!(K)) / (HW!(K) + HL!(K) + AW!(K) + AL!(K));
                If R2!(K) + R4!(K) = 0 Then Locate , 75: Print "  0.0"
                If R2!(K) + R4!(K) > 0 Then Locate , 75: Print Using "###.#"; (R2!(K) + R4!(K)) / (HW!(K) + HL!(K) + AW!(K) + AL!(K))

                Color 3: Print Tab(36); Chr$(179); Tab(58); Chr$(179);

                THW!(RDD) = THW!(RDD) + HW!(K)
                THL!(RDD) = THL!(RDD) + HL!(K)
                TAW!(RDD) = TAW!(RDD) + AW!(K)
                TAL!(RDD) = TAL!(RDD) + AL!(K)
                TR1!(RDD) = TR1!(RDD) + R1!(K)
                TR2!(RDD) = TR2!(RDD) + R2!(K)
                TR3!(RDD) = TR3!(RDD) + hd2hdR3!(K)
                TR4!(RDD) = TR4!(RDD) + R4!(K)

            End If

        Next K

        'Division Team Totals
        Print

        Color 11
        Locate , 14: Print Using "##"; THW!(RDD);

        If THW!(RDD) = THW!(RDD) + THL!(RDD) And THW!(RDD) <> 0 Then Locate , 19: Print "1.000";
        If THW!(RDD) = 0 Then Locate , 20: Print ".000";
        If THW!(RDD) > 0 And THW!(RDD) < THW!(RDD) + THL!(RDD) Then 
            statPCT! = (THW!(RDD) / (THW!(RDD) + THL!(RDD)))
            Locate , 20: Print Using ".###"; statPCT!;
        End If

        If TR2!(RDD) < 1 Then Locate , 32: Print " 0.0";
        If TR2!(RDD) > 0 Then Locate , 31: Print Using "###.#"; TR2!(RDD) / (THW!(RDD) + THL!(RDD));

        Locate , 38: Print Using "##"; TAL!(RDD);

        If TR3!(RDD) < 1 Then Locate , 48: Print " 0.0";
        If TR3!(RDD) > 0 Then Locate , 47: Print Using "###.#"; TR3!(RDD) / (TAW!(RDD) + TAL!(RDD));

        Locate , 57: Print Using "###"; THW!(RDD) + TAW!(RDD);

        Color 11

        If THW!(RDD) + TAW!(RDD) = THW!(RDD) + THL!(RDD) + TAW!(RDD) + TAL!(RDD) And THW!(RDD) + TAW!(RDD) <> 0 Then Locate , 63: Print "1.000";
        If THW!(RDD) + TAW!(RDD) = 0 Then Locate , 64: Print ".000";
        If THW!(RDD) + TAW!(RDD) > 0 And THW!(RDD) + TAW!(RDD) < THW!(RDD) + TAW!(RDD) + THL!(RDD) + TAL!(RDD) Then 
            statPCT! = (THW!(RDD) + TAW!(RDD)) / (THW!(RDD) + TAW!(RDD) + THL!(RDD) + TAL!(RDD))
            Locate , 64: Print Using ".###"; statPCT!;
        End If

        If TR2!(RDD) + TR4!(RDD) < 1 Then Locate , 76: Print " 0.0"
        If TR2!(RDD) + TR4!(RDD) > 0 Then Locate , 75: Print Using "###.#"; (TR2!(RDD) + TR4!(RDD)) / (THW!(RDD) + THL!(RDD) + TAW!(RDD) + TAL!(RDD))

        Locate , 5: Print "TOTALS";

        If TR1!(RDD) < 1 Then Locate , 26: Print " 0.0";
        If TR1!(RDD) > 0 Then Locate , 25: Print Using "###.#"; TR1!(RDD) / (THW!(RDD) + THL!(RDD));

        Color 11

        If TAW!(RDD) = TAW!(RDD) + TAL!(RDD) And TAW!(RDD) <> 0 Then Locate , 41: Print "1.000";
        If TAW!(RDD) = 0 Then Locate , 42: Print ".000";
        If TAW!(RDD) > 0 And TAW!(RDD) < TAW!(RDD) + TAL!(RDD) Then 
            statPCT! = (TAW!(RDD) / (TAW!(RDD) + TAL!(RDD)))
            Locate , 42: Print Using ".###"; statPCT!;
        End If

        If TR4!(RDD) < 1 Then Locate , 54: Print " 0.0";
        If TR4!(RDD) > 0 Then Locate , 53: Print Using "###.#"; TR4!(RDD) / (TAW!(RDD) + TAL!(RDD));
        If TR1!(RDD) + TR3!(RDD) = 0 Then Locate , 70: Print " 0.0";
        If TR1!(RDD) + TR3!(RDD) > 0 Then Locate , 69: Print Using "###.#"; (TR1!(RDD) + TR3!(RDD)) / (THW!(RDD) + THL!(RDD) + TAW!(RDD) + TAL!(RDD));

        Locate , 16: Print Using "##"; THL!(RDD);
        Locate , 36: Print Using "##"; TAW!(RDD);
        Locate , 59: Print Using "###"; THL!(RDD) + TAL!(RDD)

        Color 14
        Locate 23, 5: Print "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$

        RDD = RDD + 1

    Loop Until RDD > DIVS_PER_CONFERENCE Or DV$(RDD) = "XXX" Or DV$(RDD) = ""

    'Stats for Division Totals
    Cls

    Locate 2, 35
    Color teamRatings%(0), teamRatings%(1): Print teamName$
    Color 3, 0

    Locate 4
    Print Tab(9); String$(8, 196); " HOME "; String$(9, 196); Tab(32); Chr$(194); String$(8, 196); " AWAY "; String$(9, 196); Tab(56); Chr$(194); String$(7, 196); " TOTALS "; String$(8, 196)
    Locate 5
    Print Tab(10); "W"; Tab(13); "L"; Tab(17); "PCT."; Tab(22); "PF/G"; Tab(28); "PA/G"; Tab(32); Chr$(179); Tab(34); "W"; Tab(37); "L"; Tab(41); "PCT."; Tab(46); "PF/G"; Tab(52); "PA/G"; Tab(56); Chr$(179); Tab(58); "W"; Tab(61); "L"; Tab(65); "PCT."; Tab(70); "PF/G"; Tab(76); "PA/G"

    Color 3: Print Tab(32); Chr$(179); Tab(56); Chr$(179)

    For W = 1 To DIVS_PER_CONFERENCE

        If DV$(W) <> "XXX" Then

            Color 15
            'We have to limit this to 8 because we start stats on column 7
            Locate , 1: Print Left$(DV$(W), 8);

            Color 7
            Locate , 9: Print Using "##"; THW!(W);
            Locate , 12: Print Using "##"; THL!(W);

            If THW!(W) = THW!(W) + THL!(W) And THW!(W) <> 0 Then Locate , 15: Print "1.000";
            If THW!(W) = 0 Then Locate , 16: Print ".000";
            If THW!(W) > 0 And THW!(W) < THW!(W) + THL!(W) Then 
                statPCT! = (THW!(W) / (THW!(W) + THL!(W)))
                Locate , 16: Print Using ".###"; statPCT!;
            End If

            If TR1!(W) = 0 Then Locate , 21: Print "  0.0";
            If TR1!(W) > 0 Then Locate , 21: Print Using "###.#"; TR1!(W) / (THW!(W) + THL!(W));
            If TR2!(W) = 0 Then Locate , 27: Print "  0.0";
            If TR2!(W) > 0 Then Locate , 27: Print Using "###.#"; TR2!(W) / (THW!(W) + THL!(W));

            Color 3: Print Tab(32); Chr$(179);

            Color 7
            Locate , 33: Print Using "##"; TAW!(W);
            Locate , 36: Print Using "##"; TAL!(W);

            If TAW!(W) = TAW!(W) + TAL!(W) And TAW!(W) <> 0 Then Locate , 39: Print "1.000";
            If TAW!(W) = 0 Then Locate , 40: Print ".000";
            If TAW!(W) > 0 And TAW!(W) < TAW!(W) + TAL!(W) Then 
                statPCT! = (TAW!(W) / (TAW!(W) + TAL!(W)))
                Locate , 40: Print Using ".###"; statPCT!;
            End If

            If TR3!(W) = 0 Then Locate , 45: Print "  0.0";
            If TR3!(W) > 0 Then Locate , 45: Print Using "###.#"; TR3!(W) / (TAW!(W) + TAL!(W));
            If TR4!(W) = 0 Then Locate , 51: Print "  0.0";
            If TR4!(W) > 0 Then Locate , 51: Print Using "###.#"; TR4!(W) / (TAW!(W) + TAL!(W));

            Color 3: Print Tab(56); Chr$(179);

            Color 7
            Locate , 57: Print Using "##"; THW!(W) + TAW!(W);
            Locate , 60: Print Using "##"; THL!(W) + TAL!(W);

            If THW!(W) + TAW!(W) = THW!(W) + TAW!(W) + THL!(W) + TAL!(W) And THW!(W) + TAW!(W) <> 0 Then Locate , 63: Print "1.000";
            If (THW!(W) + TAW!(W)) = 0 Then Locate , 64: Print ".000";
            If (THW!(W) + TAW!(W)) > 0 And THW!(W) + TAW!(W) < THW!(W) + TAW!(W) + THL!(W) + TAL!(W) Then
                statPCT! = ((THW!(W) + TAW!(W)) / (THW!(W) + TAW!(W) + THL!(W) + TAL!(W)))
                Locate , 64: Print Using ".###"; statPCT!;
            End If

            If TR1!(W) + TR3!(W) = 0 Then Locate , 69: Print "  0.0";
            If TR1!(W) + TR3!(W) > 0 Then Locate , 69: Print Using "###.#"; (TR1!(W) + TR3!(W)) / (THW!(W) + THL!(W) + TAW!(W) + TAL!(W));
            If TR2!(W) + TR4!(W) = 0 Then Locate , 75: Print "  0.0"
            If TR2!(W) + TR4!(W) > 0 Then Locate , 75: Print Using "###.#"; (TR2!(W) + TR4!(W)) / (THW!(W) + THL!(W) + TAW!(W) + TAL!(W))

            Color 3: Print Tab(32); Chr$(179); Tab(56); Chr$(179);

        End If

    Next W

    THWW = 0
    THLL = 0
    TAWW = 0
    TALL = 0

    For I = 1 To 4
        THWW = THWW + THW!(I)
        THLL = THLL + THL!(I)
        TAWW = TAWW + TAW!(I)
        TALL = TALL + TAL!(I)
    Next

    TR11 = 0
    TR22 = 0
    TR33 = 0
    TR44 = 0

    For I = 1 To 4
        TR11 = TR11 + TR1!(I)
        TR22 = TR22 + TR2!(I)
        TR33 = TR33 + TR3!(I)
        TR44 = TR44 + TR4!(I)
    Next

    'Division Totals
    Color 11
    Print "TOTALS";

    Locate , 9: Print Using "##"; THWW;
    Locate , 12: Print Using "##"; THLL;

    If THWW = THWW + THLL And THWW <> 0 Then Locate , 15: Print "1.000";
    If THWW = 0 Then Locate , 16: Print ".000";
    If THWW > 0 And THWW < THWW + THLL Then Locate , 16: Print Using ".###"; THWW / (THWW + THLL);

    If TR11 = 0 Then Locate , 21: Print "  0.0";
    If TR11 > 0 Then Locate , 21: Print Using "###.#"; TR11 / (THWW + THLL);
    If TR22 = 0 Then Locate , 27: Print "  0.0";
    If TR22 > 0 Then Locate , 27: Print Using "###.#"; TR22 / (THWW + THLL);

    Color 3: Print Tab(32); Chr$(179);

    Color 11
    Locate , 33
    Print Using "##"; TAWW;
    Locate , 36
    Print Using "##"; TALL;

    If TAWW = TAWW + TALL And TAWW <> 0 Then Locate , 39: Print "1.000";
    If TAWW = 0 Then Locate , 40: Print ".000";
    If TAWW > 0 And TAWW < TAWW + TALL Then 
        statPCT! = TAWW / (TAWW + TALL)
        Locate , 40: Print Using ".###"; statPCT!;
    End If

    If TR33 = 0 Then Locate , 45: Print "  0.0";
    If TR33 > 0 Then Locate , 45: Print Using "###.#"; TR33 / (TAWW + TALL);
    If TR44 = 0 Then Locate , 51: Print "  0.0";
    If TR44 > 0 Then Locate , 51: Print Using "###.#"; TR44 / (TAWW + TALL);

    Color 3: Print Tab(56); Chr$(179);

    Color 11
    Locate , 57
    Print Using "##"; THWW + TAWW;
    Locate , 60
    Print Using "##"; THLL + TALL;

    If THWW + TAWW = THWW + TAWW + THLL + TALL And THWW + TAWW <> 0 Then Locate , 63: Print "1.000";
    If THWW + TAWW = 0 Then Locate , 64: Print ".000";
    If THWW + TAWW > 0 And THWW + TAWW < THWW + TAWW + THLL + TALL Then 
        statPCT! = (THWW + TAWW) / (THWW + TAWW + THLL + TALL)
        Locate , 64: Print Using ".###"; statPCT!;
    End If

    If TR11 + TR33 = 0 Then Locate , 69: Print "  0.0";
    If TR11 + TR33 > 0 Then Locate , 69: Print Using "###.#"; (TR11 + TR33) / (THWW + THLL + TAWW + TALL);
    If TR22 + TR44 = 0 Then Locate , 75: Print "  0.0"
    If TR22 + TR44 > 0 Then Locate , 75: Print Using "###.#"; (TR22 + TR44) / (THWW + THLL + TAWW + TALL)

    Color 14
    Locate 23, 5: Print "HIT ANY KEY TO CONTINUE"

    I$ = GetKeyPress$

End Sub
