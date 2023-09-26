'----------------------------------------
'            LOOKY Subroutine
'----------------------------------------
'This subroutine guides the user through
'choosing a team file and then calls other
'routines to load and view the stats
Sub LOOKY ()

    _Title "Courtside College Basketball - Stats Viewer"

    Shared teamRatings%()

    '-- we probably don't need this anymore
    '-- unless we're going to loop around this
    menuExit = 0

    Color 15, 0

    Call ChooseTeam(YN$, NTMS, teamNbr, team$)
    statFile$ = nameStatFile$(team$)

    If Not _FileExists(diskPaths$(1) + statFile$) Then

        MsgText$ = "NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU"
        BoxWidth = 24
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, PASS)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, PASS)

    Else

        Call LoadTeamStats(statFile$, team$, totalGames%)
        Call ViewStatSummary(statFile$, team$, totalGames%, menuExit)

    End If

End Sub


'----------------------------------------
'        LoadTeamStats Subroutine
'----------------------------------------
'This subroutine loads the selected team's
'stats into arrays
Sub LoadTeamStats (statFile$, teamName$, totalGames%)

    Dim lookyCRD(NUM_STATRECORDS)
    Dim lookyAP%(NUM_STATRECORDS, 1), P%(NUM_STATRECORDS)

    Dim lookySites$(NUM_STATRECORDS)

    'Do these need shared based on the below???
    'All are assigned 0s and read in from file
    Shared HL%, HW%, NL%, NW%, VL%, VW%
    Shared L%, W%

    Shared mergeT%(), mergeO%()

    Shared lookyA!()
    Shared statsW0!(), statsW1!()
    Shared statsZ!(), statsZ1!()

    Shared statPlyrNames$(), homeTeam$(), compO$()

    ' *** READ IN TEAM STATS ***

    P%(0) = 0
    P%(1) = 0
    L% = 0
    W% = 0
    X% = 0
    HW% = 0
    VW% = 0
    NW% = 0
    HL% = 0
    VL% = 0
    NL% = 0

    Open diskPaths$(1) + statFile$ + "S" For Input As #1

    Input #1, totalGames%

    For I = 1 To NUM_STATRECORDS: Input #1, mergeO%(I): Next I

    For I = 1 To NUM_STATRECORDS: Input #1, compO$(I): Next I

    For I = 1 To NUM_STATRECORDS: Input #1, mergeT%(I): Next I

    For I = 1 To NUM_STATRECORDS: Input #1, homeTeam$(I): Next I

    For I = 1 To NUM_STATRECORDS
        For I1 = 0 To 1: Input #1, lookyAP%(I, I1): Next
    Next

    For I = 1 To NUM_STATRECORDS: Input #1, lookyCRD(I): Next I

    For I = 1 To NUM_STATRECORDS: Input #1, lookySites$(I): Next I

    Close #1

    For I = 1 To totalGames%
        If mergeO%(I) > mergeT%(I) Then W% = W% + 1
        If mergeO%(I) > mergeT%(I) And Left$(homeTeam$(I), 1) = "H" Then HW% = HW% + 1
        If mergeO%(I) > mergeT%(I) And Left$(homeTeam$(I), 1) = "V" Then VW% = VW% + 1
        If mergeO%(I) > mergeT%(I) And Left$(homeTeam$(I), 1) = "N" Then NW% = NW% + 1

        If mergeO%(I) < mergeT%(I) Then L% = L% + 1
        If mergeO%(I) < mergeT%(I) And Left$(homeTeam$(I), 1) = "H" Then HL% = HL% + 1
        If mergeO%(I) < mergeT%(I) And Left$(homeTeam$(I), 1) = "V" Then VL% = VL% + 1
        If mergeO%(I) < mergeT%(I) And Left$(homeTeam$(I), 1) = "N" Then NL% = NL% + 1

        If mergeO%(I) = mergeT%(I) Then X% = X% + 1
        P%(0) = P%(0) + mergeO%(I)
        P%(1) = P%(1) + mergeT%(I)
    Next

    Open diskPaths$(1) + statFile$ For Input As #2

    Input #2, teamName$

    For I = 0 To 13:

        Input #2, statPlyrNames$(I)

        For J = 0 To 13:
            Input #2, lookyA!(I, J)
        Next

        Input #2, statsW0!(I)
        Input #2, statsW1!(I)

    Next

    For K = 0 To 15: Input #2, statsZ!(K): Next

    For M = 0 To 15: Input #2, statsZ1!(M): Next

    Close #2

    For I = 0 To 13
        If lookyA!(I, 0) <> 0 Then lookyA!(I, 14) = lookyA!(I, 10) / lookyA!(I, 0): lookyA!(I, 20) = (lookyA!(I, 2) * 2 + lookyA!(I, 4) + statsW0!(I)) / lookyA!(I, 0): lookyA!(I, 21) = lookyA!(I, 5) / lookyA!(I, 0)
        lookyA!(I, 22) = lookyA!(I, 2) * 2 + lookyA!(I, 4) + statsW0!(I)
        lookyA!(I, 23) = lookyA!(I, 5) - lookyA!(I, 12)
        If lookyA!(I, 1) <> 0 Then lookyA!(I, 15) = lookyA!(I, 2) / lookyA!(I, 1)
        lookyA!(I, 18) = statsW0!(I): lookyA!(I, 19) = statsW1!(I)
        If lookyA!(I, 19) <> 0 Then lookyA!(I, 16) = lookyA!(I, 18) / lookyA!(I, 19)
        If lookyA!(I, 3) <> 0 Then lookyA!(I, 17) = lookyA!(I, 4) / lookyA!(I, 3)
    Next

End Sub


'----------------------------------------
'       ViewStatSummary Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'selected team's record to the display,
'then provides routines for output of
'the other stat categories
Sub ViewStatSummary (statFile$, teamName$, totalGames%, menuExit)

    'Do these need shared based on the below???
    'All are assigned 0s and read in from file
    Shared HL%, HW%, NL%, NW%, VL%, VW%
    Shared L%, W%

    Shared lookyA!()
    Shared statsZ!(), statsZ1!()

    Shared mergeT%(), mergeO%()

    Shared statPlyrNames$(), homeTeam$(), compO$()

    Color , 0
    Cls

    Locate 1, 1
    Color 14, 1

    Print Tab(80);
    Xcoord = Len(teamName$)
    Xcoord = Int((80 - Xcoord) / 2)
    Locate , Xcoord
    Print teamName$

    Locate 25, 1: Print Tab(80);
    Locate 2, 1
    Color 15, 0

    For I = 1 To totalGames%

        Color 7

        If I / 2 = Int(I / 2) Then Color 15

        If I > 36 Then
            Locate I - 35, 53: Print Using "###"; mergeO%(I);
            Print Tab(57); Left$(compO$(I), 14);
            Print Tab(72); Using "###"; mergeT%(I);
            Print Tab(76); homeTeam$(I)
        End If

        If I > 18 Then
            Locate I - 17, 27: Print Using "###"; mergeO%(I);
            Print Tab(31); Left$(compO$(I), 14);
            Print Tab(46); Using "###"; mergeT%(I);
            Print Tab(50); homeTeam$(I)
        End If

        If I > 54 Then Exit For

        Print Using "###"; mergeO%(I);
        Print Tab(5); Left$(compO$(I), 14);
        Print Tab(20); Using "###"; mergeT%(I);
        Print Tab(24); homeTeam$(I)

    Next I

    Print

    Locate 21, 27
    Color 14: Print "OVERALL RECORD ";
    Color 15: Print "("; W%; "-"; L%; ")"
    Locate , 27
    Color 14: Print "HOME RECORD    ";
    Color 15: Print "("; HW%; "-"; HL%; ")"
    Locate , 27
    Color 14: Print "ROAD RECORD    ";
    Color 15: Print "("; VW%; "-"; VL%; ")"
    Locate , 27
    Color 14: Print "NEUTRAL RECORD ";
    Color 15: Print "("; NW%; "-"; NL%; ")";

    Color 3
    Locate 25, 1
    Color 15, 1: Print "INDIVIDUAL STATS ";
    Color 14: Print "[Pg DN]  ";
    Color 15: Print "SELECT ANOTHER TEAM ";
    Color 14: Print "[ESC]  ";
    Color 15: Print "PRINT STATS ";
    Color 14: Print "[P]           ";
    Locate 3, 1

    Do

        I$ = GetKeyPress$

        Select Case UCase$(I$)

            'Page Down
            Case Chr$(0) + "Q":
                Color , 0
                Cls
                Call ExtendedStatSummary(statFile$, teamName$, totalGames%)

            Case "D":
                Kill diskPaths$(1) + statFile$
                Kill diskPaths$(1) + statFile$ + "S"

            Case "P":
                Call PrintStatsSchedule(teamName$, totalGames%)

        End Select

    Loop Until I$ = Chr$(0) + "Q" Or I$ = Chr$(27)

    If I$ = Chr$(27) Then menuExit = 1

End Sub


'----------------------------------------
'       ViewPlayerStats Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'team's player stats to the user display
Sub ViewPlayerStats (statFile$, teamName$, CP%, totalGames%, menuExit)

    Shared lookyA!()
    Shared statsW0!(), statsW1!()
    Shared statsZ!(), statsZ1!()

    Shared statPlyrNames$(), CP$()

    CP% = 1

    Do

        Call TITLE_STATS

        Color , 1

        Locate 23, 1

        Color 15, 1: Print "SCHEDULE ";
        Color 14: Print "[Pg UP]  ";
        Color 15: Print "MORE STATS ";
        Color 14: Print "[Pg DN]  ";
        Color 15: Print "NEW TEAM ";
        Color 14: Print "[ESC]  ";
        Color 15: Print "SORT ";
        Color 14: Print "<-,-> and [ENTER]    "
        Color 15: Print "PRINT ";
        Color 14: Print "[P]  ";
        Color 15: Print "DELETE STATS ";
        Color 14: Print "[D]";
        Print Space$(53);

        Color 12, 0
        Locate 2, 1: Print "SORT BY: "; CP$(CP%)

        Color 14: Print teamName$

        Print "NAME"; Tab(15);
        Print " G  MIN AVG   FGM  FGA  PCT  3FGM 3FGA  PCT   FTM  FTA  PCT"

        For I = 0 To 13

            If statPlyrNames$(I) <> "XXX" Then

                Color 7

                If I / 2 = Int(I / 2) Then Color 15

                Print Left$(statPlyrNames$(I), 13); Tab(15);
                Print Using "## "; lookyA!(I, 0);

                If lookyA!(I, 10) > 0 Then Locate , 18: Print Using "####"; Int((lookyA!(I, 10) + 30) / 60);: Locate , 24: Print Using "##"; Int(((lookyA!(I, 10) / 60) / lookyA!(I, 0)) + .5);

                Locate , 28: Print Using "#### ####"; lookyA!(I, 2); lookyA!(I, 1);

                Locate , 38
                If lookyA!(I, 1) = 0 Then
                    Print ".---";
                Else
                    If lookyA!(I, 2) = lookyA!(I, 1) Then
                        Print "1.00";
                    Else
                        If lookyA!(I, 2) = 0 Then
                            Print ".000";
                        Else
                            statPCT! = lookyA!(I, 2) / lookyA!(I, 1)
                            Print Using ".###"; statPCT!;
                        End If
                    End If
                End If

                Locate , 44: Print Using "#### ####"; statsW0!(I); statsW1!(I);: Locate , 54

                If statsW1!(I) = 0 Then
                    Print ".---";
                Else
                    If statsW1!(I) = statsW0!(I) Then
                        Print "1.00";
                    Else
                        If statsW0!(I) = 0 Then
                            Print ".000";
                        Else
                            statPCT! = statsW0!(I) / statsW1!(I)
                            Print Using ".###"; statPCT!;
                        End If
                    End If
                End If

                Locate , 60: Print Using "#### ####"; lookyA!(I, 4); lookyA!(I, 3);: Locate , 70

                If lookyA!(I, 3) = 0 Then
                    Print ".---"
                Else
                    If lookyA!(I, 3) = lookyA!(I, 4) Then
                        Print "1.00"
                    Else
                        If lookyA!(I, 4) = 0 Then
                            Print ".000"
                        Else
                            statPCT! = lookyA!(I, 4) / lookyA!(I, 3)
                            Print Using ".###"; statPCT!
                        End If
                    End If
                End If
            End If

        Next 'done looping through all players

        Color 12
        Print
        Print "TOTALS";
        Locate , 15: Print Using "##"; totalGames%;

        Locate , 28: Print Using "#### ####"; statsZ!(2); statsZ!(1);
        If statsZ!(1) <> 0 Then statPCT! = statsZ!(2) / statsZ!(1): Locate , 38: Print Using ".###"; statPCT!;

        Locate , 44: Print Using "#### ####"; statsZ!(13); statsZ!(14);
        If statsZ!(14) <> 0 Then statPCT! = statsZ!(13) / statsZ!(14): Locate , 54: Print Using ".###"; statPCT!;

        Locate , 60: Print Using "#### ####"; statsZ!(4); statsZ!(3);
        If statsZ!(3) <> 0 Then statPCT! = statsZ!(4) / statsZ!(3): Locate , 70: Print Using ".###"; statPCT!

        Color 12
        Locate , 15: Print Using "##"; totalGames%;

        Locate , 28: Print Using "#### ####"; statsZ1!(2); statsZ1!(1);
        If statsZ1!(1) <> 0 Then statPCT! = statsZ1!(2) / statsZ1!(1): Locate , 38: Print Using ".###"; statPCT!;

        Locate , 44: Print Using "#### ####"; statsZ1!(13); statsZ1!(14);
        If statsZ1!(14) <> 0 Then statPCT! = statsZ1!(13) / statsZ1!(14): Locate , 54: Print Using ".###"; statPCT!;

        Locate , 60: Print Using "#### ####"; statsZ1!(4); statsZ1!(3);
        If statsZ1!(3) <> 0 Then statPCT! = statsZ1!(4) / statsZ1!(3): Locate , 70: Print Using ".###"; statPCT!

        Do
            I$ = GetKeyPress$

            Select Case I$

                'Pg Down
                Case Chr$(0) + "Q"
                    Call ViewTeamStats(statFile$, teamName$, CP%, totalGames%, menuExit)

                    'Pg Up
                Case Chr$(0) + "I"
                    Cls
                    Call ExtendedStatSummary(statFile$, teamName$, totalGames%)

                    'Right Arrow
                Case Chr$(0) + "M"
                    CP% = CP% + 1
                    If CP% > 13 Then CP% = 1
                    Locate 2, 1: Print "SORT BY: "; CP$(CP%); Tab(40);

                    'Left Arrow
                Case Chr$(0) + "K"
                    CP% = CP% - 1
                    If CP% < 1 Then CP% = 13
                    Locate 2, 1: Print "SORT BY: "; CP$(CP%); Tab(40);

                Case Chr$(13)
                    Call SortStats(CP%)

                Case "d", "D"
                    Kill diskPaths$(1) + statFile$
                    Kill diskPaths$(1) + statFile$ + "S"

                Case "p", "P"
                    Call PrintStatsSchedule(teamName$, totalGames%)

            End Select

        Loop Until I$ = Chr$(0) + "Q" Or I$ = Chr$(27) Or I$ = Chr$(13)

        If I$ = Chr$(27) Then menuExit = 1

    Loop Until I$ <> Chr$(13)

End Sub

'----------------------------------------
'       ViewTeamStats Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'team's OFF/DEF stats to the user display
Sub ViewTeamStats (statFile$, teamName$, CP%, totalGames%, menuExit)

    Shared lookyA!()

    Shared statsW0!(), statsW1!()
    Shared statsZ!(), statsZ1!()

    Shared statPlyrNames$(), CP$()

    CP% = 14

    Do

        Cls

        Call TITLE_STATS

        Color , 1
        Locate 23, 1

        Color 15: Print "MORE STATS ";
        Color 14: Print "[Pg UP]  ";
        Color 15: Print "SELECT NEW TEAM ";
        Color 14: Print "[ESC]  ";
        Color 15: Print "SORT ";
        Color 14: Print "<-,-> and [ENTER]  ";
        Color 15: Print "PRINT ";
        Color 14: Print "[P]    ";
        Color 15: Print "DELETE STATS ";
        Color 14: Print "[D]";
        Print Space$(64);

        Color 3, 0
        Locate 2, 1: Print "SORT BY: "; CP$(CP%); Tab(40);

        Color 14, 0
        Locate 3, 1: Print teamName$;
        Locate , 15
        Print "               OFF  DEF  TOT": Print "NAME"; Tab(15);
        Print " PTS   AVG HI  REB  REB  REB   AVG   PF  AST  BLK  STL   TO"

        For I = 0 To 13

            If statPlyrNames$(I) <> "XXX" Then

                Color 7
                If I / 2 = Int(I / 2) Then Color 15

                Print Left$(statPlyrNames$(I), 13);: Locate , 15
                Print Using "####"; lookyA!(I, 2) * 2 + lookyA!(I, 4) + statsW0!(I);

                If lookyA!(I, 0) > 0 Then Locate , 21: Print Using "##.#"; (lookyA!(I, 2) * 2 + lookyA!(I, 4) + statsW0!(I)) / lookyA!(I, 0);

                Locate , 26: Print Using "## #### #### ####"; lookyA!(I, 13); lookyA!(I, 12); lookyA!(I, 5) - lookyA!(I, 12); lookyA!(I, 5);

                If lookyA!(I, 0) > 0 Then Locate , 45: Print Using "##.#"; lookyA!(I, 5) / lookyA!(I, 0);

                Locate , 51: Print Using "### ####  ###  ###  ###"; lookyA!(I, 6); lookyA!(I, 8); lookyA!(I, 9); lookyA!(I, 7); lookyA!(I, 11)

            End If

        Next I

        Print "TEAM";: Locate , 39: Print Using "####"; statsZ!(15)
        Color 12: Print "TOTALS";
        statVal! = statsZ!(10) / totalGames%

        Locate , 14: Print Using "##### ###.#"; statsZ!(10); statVal!;
        statVal! = statsZ!(5) / totalGames%

        Locate , 29: Print Using "#### #### ####  ##.#"; statsZ!(12); statsZ!(5) - statsZ!(12) - statsZ!(15); statsZ!(5); statVal!;
        Print Using " #### #### #### #### ####"; statsZ!(6); statsZ!(8); statsZ!(9); statsZ!(7); statsZ!(11)
        statVal = statsZ!(10) / totalGames%

        Locate , 14: Print Using "##### ###.#"; statsZ1!(10); statVal;
        statVal! = statsZ1!(5) / totalGames%

        Locate , 29: Print Using "#### #### ####  ##.#"; statsZ1!(12); statsZ1!(5) - statsZ1!(12) - statsZ1!(15); statsZ1!(5); statVal!;
        Print Using " #### #### #### #### ####"; statsZ1!(6); statsZ1!(8); statsZ1!(9); statsZ1!(7); statsZ1!(11)

        Do
            I$ = GetKeyPress$

            Select Case UCase$(I$)

                'Page Up
                Case Chr$(0) + "I"
                    Cls
                    Call ViewPlayerStats(statFile$, teamName$, CP%, totalGames%, menuExit)

                Case Chr$(0) + "M"
                    CP% = CP% + 1
                    If CP% > 25 Then CP% = 14
                    Color 3
                    Locate 2, 1: Print "SORT BY: "; CP$(CP%); Tab(40);

                Case Chr$(0) + "K"
                    CP% = CP% - 1
                    If CP% < 14 Then CP% = 25
                    Color 3
                    Locate 2, 1: Print "SORT BY: "; CP$(CP%); Tab(40);

                Case Chr$(13)
                    'SORT BASED ON CATEGORY
                    Call SortStats(CP%)

                Case "d", "D"
                    Kill diskPaths$(1) + statFile$
                    Kill diskPaths$(1) + statFile$ + "S"

                Case "p", "P"
                    Call PrintStatsSchedule(teamName$, totalGames%)

            End Select

        Loop Until I$ = Chr$(27) Or I$ = Chr$(13)

        If I$ = Chr$(27) Then menuExit = 1

        'Loop Until I$ = Chr$(27) Or I$ = Chr$(0) + "I"
    Loop Until I$ <> Chr$(13)

End Sub


'----------------------------------------
'     ExtendedStatSummary Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'team's schedule stats to the  user's display
Sub ExtendedStatSummary (statFile$, teamName$, totalGames%)

    Shared HL%, HW%, NL%, NW%, VL%, VW%
    Shared L%, W%

    Shared mergeO%(), mergeT%()

    Shared compO$()

    Shared statsW0!(), statsW1!()
    Shared statsZ!(), statsZ1!()
    Shared mergeT%()
    Shared homeTeam$()

    Color , 0
    Cls

    Locate 1, 1
    Color 14, 1

    Print Tab(80);
    Xcoord = Len(teamName$)
    Xcoord = Int((80 - Xcoord) / 2)
    Locate , Xcoord
    Print teamName$

    Color 15, 0

    Locate 25, 1: Print Tab(80);
    Locate 2, 1
    Color , 0

    For I = 1 To totalGames%

        Color 7

        If I / 2 = Int(I / 2) Then Color 15

        If I >= 55 Then

            If I <= 72 Then
                Print Using "###"; mergeO%(I);
                Print Tab(5); Left$(compO$(I), 14);
                Print Tab(20); Using "###"; mergeT%(I);
                Print Tab(24); homeTeam$(I)
            Else
                Locate I - 71, 27: Print Using "###"; mergeO%(I);
                Print Tab(31); Left$(compO$(I), 14);
                Print Tab(46); Using "###"; mergeT%(I);
                Print Tab(50); homeTeam$(I)

            End If

        End If

    Next I

    Print
    Locate 21, 27
    Color 14: Print "OVERALL RECORD ";
    Color 15: Print "("; W%; "-"; L%; ")"
    Locate , 27: Color 14: Print "HOME RECORD    ";
    Color 15: Print "("; HW%; "-"; HL%; ")"
    Locate , 27: Color 14: Print "ROAD RECORD    ";
    Color 15: Print "("; VW%; "-"; VL%; ")"
    Locate , 27: Color 14: Print "NEUTRAL RECORD ";
    Color 15: Print "("; NW%; "-"; NL%; ")";
    Color 3
    Locate 25, 1
    Color 15, 1: Print "INDIVIDUAL STATS ";
    Color 14: Print "[Pg DN]  ";
    Color 15: Print "SELECT ANOTHER TEAM ";
    Color 14: Print "[ESC]  ";
    Color 15: Print "PRINT STATS ";
    Color 14: Print "[P]           ";
    Locate 3, 1

    Do
        I$ = GetKeyPress$

        Select Case I$
            'Page Down
            Case Chr$(0) + "Q"
                Color , 0
                Cls
                Call ViewPlayerStats(statFile$, teamName$, CP%, totalGames%, menuExit)

                'Page Up
            Case Chr$(0) + "I"
                Color , 0
                Cls
                Call ViewStatSummary(statFile$, teamName$, totalGames%, menuExit)

            Case "d", "D":
                Kill diskPaths$(1) + statFile$
                Kill diskPaths$(1) + statFile$ + "S"

            Case "p", "P":
                Call PrintStatsSchedule(teamName$, totalGames%)

        End Select

    Loop Until I$ = Chr$(27) Or I$ = Chr$(0) + "Q" Or menuExit = 1

End Sub

'----------------------------------------
'     PrintStatsSchedule Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'team's schedule results to the default printer
Sub PrintStatsSchedule (teamName$, totalGames%)

    Shared HL%, HW%, NL%, NW%, VL%, VW%
    Shared L%, W%

    Shared lookyA!(), statsW0!(), statsW1!(), statsZ!(), statsZ1!()

    Shared mergeT%(), mergeO%()
    Shared statPlyrNames$(), homeTeam$(), compO$()

    LPrint "SCHEDULE RESULTS FOR "; teamName$
    LPrint

    For I = 1 To 28
        LPrint Using "###"; mergeO%(I);
        LPrint Tab(5); Left$(compO$(I), 14);
        LPrint Tab(20); Using "###"; mergeT%(I);
        LPrint Tab(24); homeTeam$(I); Tab(27);

        If I + 28 <= totalGames% Then
            LPrint Using "###"; mergeO%(I + 28);
            LPrint Tab(31); Left$(compO$(I + 28), 14);
            LPrint Tab(46); Using "###"; mergeT%(I + 28);
            LPrint Tab(50); homeTeam$(I + 28); Tab(53);

            If I + 56 <= totalGames% Then
                LPrint Using "###"; mergeO%(I + 56);
                LPrint Tab(57); Left$(compO$(I + 56), 14);
                LPrint Tab(72); Using "###"; mergeT%(I + 56);
                LPrint Tab(76); homeTeam$(I + 56)
            Else
                LPrint
            End If
        Else
            LPrint
        End If
    Next I

    LPrint
    LPrint Tab(27); "OVERALL RECORD ("; W%; "-"; L%; ")"
    LPrint Tab(27); "HOME RECORD    ("; HW%; "-"; HL%; ")"
    LPrint Tab(27); "ROAD RECORD    ("; VW%; "-"; VL%; ")"
    LPrint Tab(27); "NEUTRAL RECORD ("; NW%; "-"; NL%; ")"

    LPrint Chr$(12)

    LPrint "STATISTICS SUMMARY FOR "; teamName$
    LPrint "NAME"; Tab(15);
    LPrint " G  MIN AVG   FGM  FGA  PCT  3FGM 3FGA  PCT   FTM  FTA  PCT"

    For I = 0 To 13
        LPrint Left$(statPlyrNames$(I), 13); Tab(15);
        LPrint Using "## "; lookyA!(I, 0);

        If lookyA!(I, 10) > 0 Then LPrint Tab(18); Using "####"; Int((lookyA!(I, 10) + 30) / 60);: LPrint Tab(24); Using "##"; Int(((lookyA!(I, 10) / 60) / lookyA!(I, 0)) + .5);

        LPrint Tab(28); Using "#### ####"; lookyA!(I, 2); lookyA!(I, 1);
        LPrint Tab(38);

        If lookyA!(I, 1) = 0 Then
            LPrint ".---";
        Else
            If lookyA!(I, 2) = lookyA!(I, 1) Then
                LPrint "1.00";
            Else
                If lookyA!(I, 2) = 0 Then
                    LPrint ".000";
                Else
                    LPrint Using ".###"; lookyA!(I, 2) / lookyA!(I, 1);
                End If
            End If
        End If

        LPrint Tab(44); Using "#### ####"; statsW0!(I); statsW1!(I);: LPrint Tab(54);
        If statsW1!(I) = 0 Then
            LPrint ".---";
        Else
            If statsW1!(I) = statsW0!(I) Then
                LPrint "1.00";
            Else
                If statsW0!(I) = 0 Then
                    LPrint ".000";
                Else
                    statPCT! = statsW0!(I) / statsW1!(I)
                    LPrint Using ".###"; statPCT!;
                End If
            End If
        End If

        LPrint Tab(60); Using "#### ####"; lookyA!(I, 4); lookyA!(I, 3);: LPrint Tab(70);

        If lookyA!(I, 3) = 0 Then
            LPrint ".---"
        Else
            If lookyA!(I, 3) = lookyA!(I, 4) Then
                LPrint "1.00"
            Else
                If lookyA!(I, 4) = 0 Then
                    LPrint ".000"
                Else
                    statPCT! = lookyA!(I, 4) / lookyA!(I, 3)
                    LPrint Using ".###"; statPCT!
                End If
            End If
        End If

    Next 'done looping through players

    LPrint: LPrint "TOTALS"; Tab(15);
    LPrint Using "##"; totalGames%;

    '1st Row
    LPrint Tab(28); Using "#### ####"; statsZ!(2); statsZ!(1);
    If statsZ!(1) <> 0 Then statPCT! = statsZ!(2) / statsZ!(1): LPrint Tab(38); Using ".###"; statPCT!;

    LPrint Tab(44); Using "#### ####"; statsZ!(13); statsZ!(14);
    If statsZ!(14) <> 0 Then statPCT! = statsZ!(13) / statsZ!(14): LPrint Tab(54); Using ".###"; statPCT!;

    LPrint Tab(60); Using "#### ####"; statsZ!(4); statsZ!(3);
    If statsZ!(3) <> 0 Then statPCT! = statsZ!(4) / statsZ!(3): LPrint Tab(70); Using ".###"; statPCT!

    '2nd Row (Opponents?)
    LPrint Tab(15); Using "##"; totalGames%;

    LPrint Tab(28); Using "#### ####"; statsZ1!(2); statsZ1!(1);
    If statsZ1!(1) <> 0 Then statPCT! = statsZ1!(2) / statsZ1!(1): LPrint Tab(38); Using ".###"; statPCT!;

    LPrint Tab(44); Using "#### ####"; statsZ1!(13); statsZ1!(14);
    If statsZ1!(14) <> 0 Then statPCT! = statsZ1!(13) / statsZ1!(14): LPrint Tab(54); Using ".###"; statPCT!;

    LPrint Tab(60); Using "#### ####"; statsZ1!(4); statsZ1!(3);
    If statsZ1!(3) <> 0 Then statPCT! = statsZ1!(4) / statsZ1!(3): LPrint Tab(70); Using ".###"; statPCT!

    LPrint
    LPrint Tab(15);
    LPrint "               OFF  DEF  TOT": LPrint Tab(15);
    LPrint " PTS   AVG HI  REB  REB  REB   AVG   PF  AST  BLK  STL   TO"

    For I = 0 To 13
        LPrint Left$(statPlyrNames$(I), 13);: LPrint Tab(15);
        LPrint Using "####"; lookyA!(I, 2) * 2 + lookyA!(I, 4) + statsW0!(I);
        If lookyA!(I, 0) > 0 Then LPrint Tab(21); Using "##.#"; (lookyA!(I, 2) * 2 + lookyA!(I, 4) + statsW0!(I)) / lookyA!(I, 0);
        LPrint Tab(26); Using "## #### #### ####"; lookyA!(I, 13); lookyA!(I, 12); lookyA!(I, 5) - lookyA!(I, 12); lookyA!(I, 5);
        If lookyA!(I, 0) > 0 Then LPrint Tab(45); Using "##.#"; lookyA!(I, 5) / lookyA!(I, 0);
        LPrint Tab(51); Using "### ####  ###  ###  ###"; lookyA!(I, 6); lookyA!(I, 8); lookyA!(I, 9); lookyA!(I, 7); lookyA!(I, 11)
    Next I

    LPrint "TEAM";: LPrint Tab(39); Using "####"; statsZ!(15)
    LPrint
    LPrint "TOTALS";
    LPrint Tab(14); Using "##### ###.#"; statsZ!(10); statsZ!(10) / totalGames%;
    LPrint Tab(29); Using "#### #### ####  ##.#"; statsZ!(12); statsZ!(5) - statsZ!(12) - statsZ!(15); statsZ!(5); statsZ!(5) / totalGames%;
    LPrint Using " #### #### #### #### ####"; statsZ!(6); statsZ!(8); statsZ!(9); statsZ!(7); statsZ!(11)
    LPrint Tab(14); Using "##### ###.#"; statsZ1!(10); statsZ1!(10) / totalGames%;
    LPrint Tab(29); Using "#### #### ####  ##.#"; statsZ1!(12); statsZ1!(5) - statsZ1!(12) - statsZ1!(15); statsZ1!(5); statsZ1!(5) / totalGames%;
    LPrint Using " #### #### #### #### ####"; statsZ1!(6); statsZ1!(8); statsZ1!(9); statsZ1!(7); statsZ1!(11)

    LPrint Chr$(12)

End Sub


'----------------------------------------
'       SortStats Subroutine
'----------------------------------------
'This subroutine sorts the associate stats
'based on the chosen category
Sub SortStats (CP%)

    Shared lookyA!()
    Shared statsW0!(), statsW0!()
    Shared statsW1!(), statsW1!()

    Shared statPlyrNames$(), statPlyrNames$()

    If CP% = 1 Then SG% = 0
    If CP% = 2 Then SG% = 10
    If CP% = 3 Then SG% = 14
    If CP% = 4 Then SG% = 2
    If CP% = 5 Then SG% = 1
    If CP% = 6 Then SG% = 15
    If CP% = 7 Then SG% = 18
    If CP% = 8 Then SG% = 19
    If CP% = 9 Then SG% = 16
    If CP% = 10 Then SG% = 4
    If CP% = 11 Then SG% = 3
    If CP% = 12 Then SG% = 17
    If CP% = 14 Then SG% = 22
    If CP% = 15 Then SG% = 20
    If CP% = 17 Then SG% = 12
    If CP% = 16 Then SG% = 13
    If CP% = 18 Then SG% = 23
    If CP% = 19 Then SG% = 5
    If CP% = 20 Then SG% = 21
    If CP% = 21 Then SG% = 6
    If CP% = 22 Then SG% = 8
    If CP% = 23 Then SG% = 9
    If CP% = 24 Then SG% = 7
    If CP% = 25 Then SG% = 11

    For Z = 0 To 12

        For Z1 = Z + 1 To 13

            If lookyA!(Z, SG%) < lookyA!(Z1, SG%) Then

                For I = 0 To 24: Swap lookyA!(Z, I), lookyA!(Z1, I): Next

                Swap statsW0!(Z), statsW0!(Z1)
                Swap statsW1!(Z), statsW1!(Z1)
                Swap statPlyrNames$(Z), statPlyrNames$(Z1)

            End If

        Next Z1

    Next Z


End Sub


'----------------------------------------
'       ResetStatPrompt Subroutine
'----------------------------------------
'This subroutine prompts the user with
'a choice to reset stats. If they chose,
'and confirm, it will then proceed to
'delete the files.
Sub ResetStatPrompt (statFile$)

    Cls

    Call MenuBox(6, 8, 61, 1, 15, 1)

    Locate 7

    Do
        U$ = "RESET COMPILED STATS TO 0? (Y/N)"
        Locate , (80 - Len(U$)) / 2
        Print U$
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Color 15, 0

    If UCase$(I$) = "Y" Then

        Call MenuBox(6, 8, 61, 1, 15, 1)

        Locate 7

        Do
            U$ = "ARE YOU SURE? (Y/N)"
            Locate , (80 - Len(U$)) / 2
            Print U$
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        Color 15, 0

        If UCase$(I$) = "Y" Then
            Kill diskPaths$(1) + statFile$
            Kill diskPaths$(1) + statFile$ + "S"
            If _FileExists(diskPaths$(1) + statFile$ + "R") Then Kill diskPaths$(1) + statFile$ + "R"
        End If

        'Loop Until UCase$(I$) = "N"

    End If 'Done checking if reset stats

End Sub

