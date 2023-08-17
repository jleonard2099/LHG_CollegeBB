'----------------------------------------
'
'----------------------------------------
'
Sub GameRecords (menuChoice)

    _Title "Courtside College Basketball - Game Records"

    Shared REC!(), TREC!()
    
    Shared recordsA$(), RC$(), TRC$()

    Open diskPaths$(1) + "RECORDS.GAM" For Input As #1

    For I = 0 To 48
        For I1 = 0 To 2
            Input #1, REC!(I, I1)
        Next
        For I1 = 0 To 4
            Input #1, RC$(I, I1)
        Next
    Next

    For I = 0 To 125
        For I1 = 0 To 2
            Input #1, TREC!(I, I1)
        Next
        For I1 = 0 To 3
            Input #1, TRC$(I, I1)
        Next
    Next

    Close 1

    Select Case menuChoice

        Case 1: 
            Call ViewGameRecords

        Case 2: 
            Call PrintGameRecords

    End Select

End Sub

'----------------------------------------
'       ViewGameRecords Subroutine
'----------------------------------------
'...explanation...
Sub ViewGameRecords

    Shared REC!(), TREC!()
    Shared recordsA$(), RC$(), TRC$()

    Dim HALF$(2), QTR$(6)

    HALF$(1) = " (1ST HALF)"
    HALF$(2) = " (2ND HALF)"
    QTR$(3) = " (1ST OT)"
    QTR$(4) = " (2ND OT)"
    QTR$(5) = " (3RD OT)"
    QTR$(6) = " (4TH OT)"

    Color 15, 1
    Cls

    Color 12: Print Tab(31); "INDIVIDUAL RECORDS"

    Color 14: Print "MOST POINTS, GAME"
    Color 15
    Print REC!(0, 0); "- "; RC$(0, 0); ", "; RC$(0, 1) + " " + RC$(0, 2) + " " + RC$(0, 3) + ", " + RC$(0, 4)
    Print

    Color 14: Print "MOST POINTS, ONE HALF"
    Color 15
    Print REC!(1, 0); "- "; RC$(1, 0); ", "; RC$(1, 1) + " " + RC$(1, 2) + " " + RC$(1, 3) + ", " + RC$(1, 4); HALF$(REC!(1, 2))
    Print

    Color 14: Print "MOST POINTS, OVERTIME PERIOD"
    Color 15
    If REC!(3, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print REC!(3, 0); "- "; RC$(3, 0); ", "; RC$(3, 1) + " " + RC$(3, 2) + " " + RC$(3, 3) + ", " + RC$(3, 4)
    End If
    Print

    Color 14: Print "HIGHEST FIELD GOAL PERCENTAGE, GAME (MINIMUM 15 FIELD GOALS)"
    Color 15
    If REC!(4, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        If REC!(4, 0) = 1 Then
            Print "1.000 ";
        Else
            Print Using " .### "; REC!(4, 0);
        End If
        Print "- "; RC$(4, 0); ", "; RC$(4, 1) + " " + RC$(4, 2) + " " + RC$(4, 3) + ", " + RC$(4, 4);
        Print Using " (##/##)"; REC!(4, 1); REC!(4, 2)
    End If

    Print

    Color 14: Print "MOST FIELD GOALS, NO MISSES, GAME"
    Color 15
    If REC!(5, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print REC!(5, 0); "- "; RC$(5, 0); ", "; RC$(5, 1) + " " + RC$(5, 2) + " " + RC$(5, 3) + ", " + RC$(5, 4)
    End If
    Print

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, NONE MADE, GAME"
    Color 15
    If REC!(6, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print REC!(6, 0); "- "; RC$(6, 0); ", "; RC$(6, 1) + " " + RC$(6, 2) + " " + RC$(6, 3) + ", " + RC$(6, 4)
    End If

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls

    Color 12: Print Tab(31); "INDIVIDUAL RECORDS"

    Color 14: Print "MOST FIELD GOALS, GAME"
    Color 15
    Print REC!(7, 0); "- "; RC$(7, 0); ", "; RC$(7, 1) + " " + RC$(7, 2) + " " + RC$(7, 3) + ", " + RC$(7, 4)
    Print
    
    Color 14: Print "MOST FIELD GOALS, ONE HALF"
    Color 15
    Print REC!(8, 0); "- "; RC$(8, 0); ", "; RC$(8, 1) + " " + RC$(8, 2) + " " + RC$(8, 3) + ", " + RC$(8, 4); HALF$(REC!(8, 2))
    Print

    Color 14: Print "MOST FIELD GOALS, OVERTIME"
    Color 15
    If REC!(10, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print REC!(10, 0); "- "; RC$(10, 0); ", "; RC$(10, 1) + " " + RC$(10, 2) + " " + RC$(10, 3) + ", " + RC$(10, 4)
    End If
    Print

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, GAME"
    Color 15
    Print REC!(11, 0); "- "; RC$(11, 0); ", "; RC$(11, 1) + " " + RC$(11, 2) + " " + RC$(11, 3) + ", " + RC$(11, 4)
    Print

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, ONE HALF"
    Color 15
    Print REC!(12, 0); "- "; RC$(12, 0); ", "; RC$(12, 1) + " " + RC$(12, 2) + " " + RC$(12, 3) + ", " + RC$(12, 4); HALF$(REC!(12, 2))
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls

    Color 12: Print Tab(31); "INDIVIDUAL RECORDS"

    Color 14: Print "MOST THREE-POINT FIELD GOALS, NO MISSES, GAME"
    Color 15
    If REC!(15, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print REC!(15, 0); "- "; RC$(15, 0); ", "; RC$(15, 1) + " " + RC$(15, 2) + " " + RC$(15, 3) + ", " + RC$(15, 4)
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, NONE MADE, GAME"
    Color 15
    If REC!(16, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print REC!(16, 0); "- "; RC$(16, 0); ", "; RC$(16, 1) + " " + RC$(16, 2) + " " + RC$(16, 3) + ", " + RC$(16, 4)
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS, GAME"
    Color 15
    If REC!(17, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print REC!(17, 0); "- "; RC$(17, 0); ", "; RC$(17, 1) + " " + RC$(17, 2) + " " + RC$(17, 3) + ", " + RC$(17, 4)
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS MADE, ONE HALF"
    Color 15
    If REC!(18, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print REC!(18, 0); "- "; RC$(18, 0); ", "; RC$(18, 1) + " " + RC$(18, 2) + " " + RC$(18, 3) + ", " + RC$(18, 4); HALF$(REC!(18, 2))
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS MADE, OVERTIME PERIOD"
    Color 15
    If REC!(20, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print REC!(20, 0); "- "; RC$(20, 0); ", "; RC$(20, 1) + " " + RC$(20, 2) + " " + RC$(20, 3) + ", " + RC$(20, 4); QTR$(REC!(20, 2))
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, GAME"
    Color 15
    If REC!(21, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print REC!(21, 0); "- "; RC$(21, 0); ", "; RC$(21, 1) + " " + RC$(21, 2) + " " + RC$(21, 3) + ", " + RC$(21, 4)
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, ONE HALF"
    Color 15
    If REC!(22, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print REC!(22, 0); "- "; RC$(22, 0); ", "; RC$(22, 1) + " " + RC$(22, 2) + " " + RC$(22, 3) + ", " + RC$(22, 4); HALF$(REC!(22, 2))
    End If
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls
    Color 12: Print Tab(31); "INDIVIDUAL RECORDS"

    Color 14: Print "MOST FREE THROWS MADE, NO MISSES, GAME"
    Color 15
    If REC!(23, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print REC!(23, 0); "- "; RC$(23, 0); ", "; RC$(23, 1) + " " + RC$(23, 2) + " " + RC$(23, 3) + ", " + RC$(23, 4)
    End If
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, NONE MADE, GAME"
    Color 15
    If REC!(24, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print REC!(24, 0); "- "; RC$(24, 0); ", "; RC$(24, 1) + " " + RC$(24, 2) + " " + RC$(24, 3) + ", " + RC$(24, 4)
    End If
    Print

    Color 14: Print "MOST FREE THROWS MADE, GAME"
    Color 15
    If REC!(25, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print REC!(25, 0); "- "; RC$(25, 0); ", "; RC$(25, 1) + " " + RC$(25, 2) + " " + RC$(25, 3) + ", " + RC$(25, 4)
    End If
    Print

    Color 14: Print "MOST FREE THROWS MADE, ONE HALF"
    Color 15
    If REC!(26, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print REC!(26, 0); "- "; RC$(26, 0); ", "; RC$(26, 1) + " " + RC$(26, 2) + " " + RC$(26, 3) + ", " + RC$(26, 4); HALF$(REC!(26, 2))
    End If
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, GAME"
    Color 15
    If REC!(28, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print REC!(28, 0); "- "; RC$(28, 0); ", "; RC$(28, 1) + " " + RC$(28, 2) + " " + RC$(28, 3) + ", " + RC$(28, 4)
    End If
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, ONE HALF"
    Color 15
    If REC!(29, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print REC!(29, 0); "- "; RC$(29, 0); ", "; RC$(29, 1) + " " + RC$(29, 2) + " " + RC$(29, 3) + ", " + RC$(29, 4); HALF$(REC!(29, 2))
    End If
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls
    
    Color 12: Print Tab(31); "INDIVIDUAL RECORDS"

    Color 14: Print "MOST REBOUNDS, GAME"
    Color 15
    If REC!(31, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print REC!(31, 0); "- "; RC$(31, 0); ", "; RC$(31, 1) + " " + RC$(31, 2) + " " + RC$(31, 3) + ", " + RC$(31, 4)
    End If
    Print

    Color 14: Print "MOST REBOUNDS, ONE HALF"
    Color 15
    If REC!(32, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print REC!(32, 0); "- "; RC$(32, 0); ", "; RC$(32, 1) + " " + RC$(32, 2) + " " + RC$(32, 3) + ", " + RC$(32, 4); HALF$(REC!(32, 2))
    End If
    Print

    Color 14: Print "MOST OFFENSIVE REBOUNDS, GAME"
    Color 15
    If REC!(34, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print REC!(34, 0); "- "; RC$(34, 0); ", "; RC$(34, 1) + " " + RC$(34, 2) + " " + RC$(34, 3) + ", " + RC$(34, 4)
    End If
    Print

    Color 14: Print "MOST OFFENSIVE REBOUNDS, ONE HALF"
    Color 15
    If REC!(35, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print REC!(35, 0); "- "; RC$(35, 0); ", "; RC$(35, 1) + " " + RC$(35, 2) + " " + RC$(35, 3) + ", " + RC$(35, 4); HALF$(REC!(35, 2))
    End If
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls

    Color 12: Print Tab(31); "INDIVIDUAL RECORDS"

    Color 14: Print "MOST DEFENSIVE REBOUNDS, GAME"
    Color 15
    If REC!(37, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print REC!(37, 0); "- "; RC$(37, 0); ", "; RC$(37, 1) + " " + RC$(37, 2) + " " + RC$(37, 3) + ", " + RC$(37, 4)
    End If
    Print

    Color 14: Print "MOST DEFENSIVE REBOUNDS, ONE HALF"
    Color 15
    If REC!(38, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print REC!(38, 0); "- "; RC$(38, 0); ", "; RC$(38, 1) + " " + RC$(38, 2) + " " + RC$(38, 3) + ", " + RC$(38, 4); HALF$(REC!(38, 2))
    End If
    Print

    Color 14: Print "MOST ASSISTS, GAME"
    Color 15
    If REC!(40, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print REC!(40, 0); "- "; RC$(40, 0); ", "; RC$(40, 1) + " " + RC$(40, 2) + " " + RC$(40, 3) + ", " + RC$(40, 4)
    End If
    Print

    Color 14: Print "MOST ASSISTS, ONE HALF"
    Color 15
    If REC!(41, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print REC!(41, 0); "- "; RC$(41, 0); ", "; RC$(41, 1) + " " + RC$(41, 2) + " " + RC$(41, 3) + ", " + RC$(41, 4); HALF$(REC!(41, 2))
    End If
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls
    Color 12: Print Tab(31); "INDIVIDUAL RECORDS"

    Color 14: Print "MOST STEALS, GAME"
    Color 15
    If REC!(43, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print REC!(43, 0); "- "; RC$(43, 0); ", "; RC$(43, 1) + " " + RC$(43, 2) + " " + RC$(43, 3) + ", " + RC$(43, 4)
    End If
    Print

    Color 14: Print "MOST STEALS, ONE HALF"
    Color 15
    If REC!(44, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print REC!(44, 0); "- "; RC$(44, 0); ", "; RC$(44, 1) + " " + RC$(44, 2) + " " + RC$(44, 3) + ", " + RC$(44, 4); HALF$(REC!(44, 2))
    End If
    Print

    Color 14: Print "MOST BLOCKS, GAME"
    Color 15
    If REC!(46, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print REC!(46, 0); "- "; RC$(46, 0); ", "; RC$(46, 1) + " " + RC$(46, 2) + " " + RC$(46, 3) + ", " + RC$(46, 4)
    End If
    Print

    Color 14: Print "MOST BLOCKS, ONE HALF"
    Color 15
    If REC!(47, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print REC!(47, 0); "- "; RC$(47, 0); ", "; RC$(47, 1) + " " + RC$(47, 2) + " " + RC$(47, 3) + ", " + RC$(47, 4); HALF$(REC!(47, 2))
    End If
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST OVERTIME PERIODS"
    Color 15
    If TREC!(0, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print TREC!(0, 0); "- "; TRC$(0, 1);
        Print Using " (###) AT "; TREC!(0, 1);
        Print TRC$(0, 2);: Print Using " (###)"; TREC!(0, 2);: Print ", " + TRC$(0, 3)
    End If
    Print

    Color 14: Print "MOST POINTS, GAME"
    Color 15
    Print TREC!(1, 0); "- "; TRC$(1, 0) + " " + TRC$(1, 1) + " " + TRC$(1, 2) + ", " + TRC$(1, 3)
    Print

    Color 14: Print "FEWEST POINTS, GAME"
    Color 15
    Print TREC!(2, 0); "- "; TRC$(2, 0) + " " + TRC$(2, 1) + " " + TRC$(2, 2) + ", " + TRC$(2, 3)
    Print

    Color 14: Print "MOST POINTS, BOTH TEAMS, GAME"
    Color 15
    Print TREC!(3, 0); "- "; TRC$(3, 0);
    Print Using " (###) "; TREC!(3, 1);: Print TRC$(3, 1) + " ";
    Print TRC$(3, 2);: Print Using " (###)"; TREC!(3, 2);: Print ", " + TRC$(3, 3)
    Print

    Color 14: Print "FEWEST POINTS, BOTH TEAMS, GAME"
    Color 15
    Print TREC!(4, 0); "- "; TRC$(4, 0);
    Print Using " (###) "; TREC!(4, 1);: Print TRC$(4, 1) + " ";
    Print TRC$(4, 2);: Print Using " (###)"; TREC!(4, 2);: Print ", " + TRC$(4, 3)
    Print

    Color 14: Print "MOST POINTS, FIRST HALF"
    Color 15
    Print TREC!(6, 0); "- "; TRC$(6, 0) + " " + TRC$(6, 1) + " " + TRC$(6, 2) + ", " + TRC$(6, 3)
    Print

    Color 14: Print "FEWEST POINTS, FIRST HALF"
    Color 15
    Print TREC!(7, 0); "- "; TRC$(7, 0) + " " + TRC$(7, 1) + " " + TRC$(7, 2) + ", " + TRC$(7, 3)
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST POINTS, BOTH TEAMS, FIRST HALF"
    Color 15
    Print TREC!(8, 0); "- "; TRC$(8, 0);
    Print Using " (###) "; TREC!(8, 1);: Print TRC$(8, 1) + " ";
    Print TRC$(8, 2);: Print Using " (###)"; TREC!(8, 2);: Print ", " + TRC$(8, 3)
    Print

    Color 14: Print "FEWEST POINTS, BOTH TEAMS, FIRST HALF"
    Color 15
    Print TREC!(9, 0); "- "; TRC$(9, 0);
    Print Using " (###) "; TREC!(9, 1);: Print TRC$(9, 1) + " ";
    Print TRC$(9, 2);: Print Using " (###)"; TREC!(9, 2);: Print ", " + TRC$(9, 3)
    Print

    Color 14: Print "MOST POINTS, SECOND HALF"
    Color 15
    Print TREC!(10, 0); "- "; TRC$(10, 0) + " " + TRC$(10, 1) + " " + TRC$(10, 2) + ", " + TRC$(10, 3)
    Print

    Color 14: Print "FEWEST POINTS, SECOND HALF"
    Color 15
    Print TREC!(11, 0); "- "; TRC$(11, 0) + " " + TRC$(11, 1) + " " + TRC$(11, 2) + ", " + TRC$(11, 3)
    Print

    Color 14: Print "MOST POINTS, BOTH TEAMS, SECOND HALF"
    Color 15
    Print TREC!(12, 0); "- "; TRC$(12, 0);
    Print Using " (###) "; TREC!(12, 1);: Print TRC$(12, 1) + " ";
    Print TRC$(12, 2);: Print Using " (###)"; TREC!(12, 2);: Print ", " + TRC$(12, 3)
    Print

    Color 14: Print "FEWEST POINTS, BOTH TEAMS, SECOND HALF"
    Color 15
    Print TREC!(13, 0); "- "; TRC$(13, 0);
    Print Using " (###) "; TREC!(13, 1);: Print TRC$(13, 1) + " ";
    Print TRC$(13, 2);: Print Using " (###)"; TREC!(13, 2);: Print ", " + TRC$(13, 3)
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST POINTS, OVERTIME"
    Color 15
    If TREC!(14, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print TREC!(14, 0); "- "; TRC$(14, 0) + " " + TRC$(14, 1) + " " + TRC$(14, 2) + ", " + TRC$(14, 3)
    End If
    Print

    Color 14: Print "FEWEST POINTS, OVERTIME"
    Color 15
    If TREC!(15, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print TREC!(15, 0); "- "; TRC$(15, 0) + " " + TRC$(15, 1) + " " + TRC$(15, 2) + ", " + TRC$(15, 3)
    End If
    Print

    Color 14: Print "MOST POINTS, BOTH TEAMS, OVERTIME"
    Color 15
    If TREC!(16, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print TREC!(16, 0); "- "; TRC$(16, 0);
        Print Using " (###) "; TREC!(16, 1);: Print TRC$(16, 1) + " ";
        Print TRC$(16, 2);: Print Using " (###)"; TREC!(16, 2);: Print ", " + TRC$(16, 3)
    End If
    Print

    Color 14: Print "FEWEST POINTS, BOTH TEAMS, OVERTIME"
    Color 15
    If TREC!(17, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print TREC!(17, 0); "- "; TRC$(17, 0);
        Print Using " (###) "; TREC!(17, 1);: Print TRC$(17, 1) + " ";
        Print TRC$(17, 2);: Print Using " (###)"; TREC!(17, 2);: Print ", " + TRC$(17, 3)
    End If
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "HIGHEST FIELD GOAL PERCENTAGE, GAME"
    Color 15
    Print Using ".### "; TREC!(30, 0);: Print "- "; TRC$(30, 0) + " " + TRC$(30, 1) + " " + TRC$(30, 2) + ", " + TRC$(30, 3);: Print Using "###/###"; TREC!(30, 1); TREC!(30, 2)
    Print

    Color 14: Print "LOWEST FIELD GOAL PERCENTAGE, GAME"
    Color 15
    Print Using ".### "; TREC!(31, 0);: Print "- "; TRC$(31, 0) + " " + TRC$(31, 1) + " " + TRC$(31, 2) + ", " + TRC$(31, 3);: Print Using "###/###"; TREC!(31, 1); TREC!(31, 2)
    Print

    Color 14: Print "HIGHEST FIELD GOAL PERCENTAGE, BOTH TEAMS, GAME"
    Color 15
    Print Using ".### "; TREC!(32, 0);: Print "- "; TRC$(32, 0);
    Print Using " (.###) "; TREC!(32, 1);: Print TRC$(32, 1) + " ";
    Print TRC$(32, 2);: Print Using " (.###)"; TREC!(32, 2);: Print ", " + TRC$(32, 3)
    Print

    Color 14: Print "LOWEST FIELD GOAL PERCENTAGE, BOTH TEAMS, GAME"
    Color 15
    Print Using ".### "; TREC!(33, 0);: Print "- "; TRC$(33, 0);
    Print Using " (.###) "; TREC!(33, 1);: Print TRC$(33, 1) + " ";
    Print TRC$(33, 2);: Print Using " (.###)"; TREC!(33, 2);: Print ", " + TRC$(33, 3)
    Print

    Color 14: Print "MOST FIELD GOALS, GAME"
    Color 15
    Print TREC!(34, 0); "- "; TRC$(34, 0) + " " + TRC$(34, 1) + " " + TRC$(34, 2) + ", " + TRC$(34, 3)
    Print

    Color 14: Print "FEWEST FIELD GOALS, GAME"
    Color 15
    Print TREC!(35, 0); "- "; TRC$(35, 0) + " " + TRC$(35, 1) + " " + TRC$(35, 2) + ", " + TRC$(35, 3)
    Print

    Color 14: Print "MOST FIELD GOALS, BOTH TEAMS, GAME"
    Color 15
    Print TREC!(36, 0); "- "; TRC$(36, 0);
    Print Using " (###) "; TREC!(36, 1);: Print TRC$(36, 1) + " ";
    Print TRC$(36, 2);: Print Using " (###)"; TREC!(36, 2);: Print ", " + TRC$(36, 3)
    Print

    Color 14: Print "FEWEST FIELD GOALS, BOTH TEAMS, GAME"
    Color 15
    Print TREC!(37, 0); "- "; TRC$(37, 0);
    Print Using " (###) "; TREC!(37, 1);: Print TRC$(37, 1) + " ";
    Print TRC$(37, 2);: Print Using " (###)"; TREC!(37, 2);: Print ", " + TRC$(37, 3);

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST FIELD GOALS, ONE HALF"
    Color 15
    Print TREC!(38, 0); "- "; TRC$(38, 0) + " " + TRC$(38, 1) + " " + TRC$(38, 2) + ", " + TRC$(38, 3); HALF$(TREC!(38, 1))
    Print

    Color 14: Print "MOST FIELD GOALS, BOTH TEAMS, ONE HALF"
    Color 15
    Print TREC!(39, 0); "- "; TRC$(39, 0); " "; TRC$(39, 1); " "; TRC$(39, 2); ", "; TRC$(39, 3); HALF$(TREC!(39, 1))
    Print

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, GAME"
    Color 15
    Print TREC!(42, 0); "- "; TRC$(42, 0) + " " + TRC$(42, 1) + " " + TRC$(42, 2) + ", " + TRC$(42, 3)
    Print

    Color 14: Print "FEWEST FIELD GOAL ATTEMPTS, GAME"
    Color 15
    Print TREC!(43, 0); "- "; TRC$(43, 0) + " " + TRC$(43, 1) + " " + TRC$(43, 2) + ", " + TRC$(43, 3)
    Print

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
    Color 15
    Print TREC!(44, 0); "- "; TRC$(44, 0);
    Print Using " (###) "; TREC!(44, 1);: Print TRC$(44, 1) + " ";
    Print TRC$(44, 2);: Print Using " (###)"; TREC!(44, 2);: Print ", " + TRC$(44, 3)
    Print

    Color 14: Print "FEWEST FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
    Color 15
    Print TREC!(45, 0); "- "; TRC$(45, 0);
    Print Using " (###) "; TREC!(45, 1);: Print TRC$(45, 1) + " ";
    Print TRC$(45, 2);: Print Using " (###)"; TREC!(45, 2);: Print ", " + TRC$(45, 3);

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, ONE HALF"
    Color 15
    Print TREC!(46, 0); "- "; TRC$(46, 0) + " " + TRC$(46, 1) + " " + TRC$(46, 2) + ", " + TRC$(46, 3); HALF$(TREC!(46, 1))
    Print

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, ONE HALF"
    Color 15
    Print TREC!(47, 0); "- "; TRC$(47, 0); " "; TRC$(47, 1); " "; TRC$(47, 2); ", "; TRC$(47, 3); HALF$(TREC!(47, 1))
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS, NO MISSES, GAME"
    Color 15
    If TREC!(50, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print TREC!(50, 0); "- "; TRC$(50, 0) + " " + TRC$(50, 1) + " " + TRC$(50, 2) + ", " + TRC$(50, 3)
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS, NO MISSES, BOTH TEAMS, GAME"
    Color 15
    If TREC!(51, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print TREC!(51, 0); "- "; TRC$(51, 0);
        Print Using " (###) "; TREC!(51, 1);: Print TRC$(51, 1) + " ";
        Print TRC$(51, 2);: Print Using " (###)"; TREC!(51, 2);: Print ", " + TRC$(51, 3)
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, NONE MADE, GAME"
    Color 15
    If TREC!(52, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print TREC!(52, 0); "- "; TRC$(52, 0) + " " + TRC$(52, 1) + " " + TRC$(52, 2) + ", " + TRC$(52, 3)
    End If
    Print
    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST THREE-POINT FIELD GOALS, GAME"
    Color 15
    Print TREC!(53, 0); "- "; TRC$(53, 0) + " " + TRC$(53, 1) + " " + TRC$(53, 2) + ", " + TRC$(53, 3)
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS, BOTH TEAMS, GAME"
    Color 15
    Print TREC!(54, 0); "- "; TRC$(54, 0);
    Print Using " (###) "; TREC!(54, 1);: Print TRC$(54, 1) + " ";
    Print TRC$(54, 2);: Print Using " (###)"; TREC!(54, 2);: Print ", " + TRC$(54, 3)
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS, ONE HALF"
    Color 15
    Print TREC!(55, 0); "- "; TRC$(55, 0) + " " + TRC$(55, 1) + " " + TRC$(55, 2) + ", " + TRC$(55, 3); HALF$(TREC!(55, 1))
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, GAME"
    Color 15
    Print TREC!(57, 0); "- "; TRC$(57, 0) + " " + TRC$(57, 1) + " " + TRC$(57, 2) + ", " + TRC$(57, 3)
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
    Color 15
    Print TREC!(58, 0); "- "; TRC$(58, 0);
    Print Using " (###) "; TREC!(58, 1);: Print TRC$(58, 1) + " ";
    Print TRC$(58, 2);: Print Using " (###)"; TREC!(58, 2);: Print ", " + TRC$(58, 3)
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, ONE HALF"
    Color 15
    Print TREC!(59, 0); "- "; TRC$(59, 0) + " " + TRC$(59, 1) + " " + TRC$(59, 2) + ", " + TRC$(59, 3); HALF$(TREC!(59, 1))

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls

    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST FREE THROWS MADE, NO MISSES, GAME"
    Color 15
    If TREC!(60, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print TREC!(60, 0); "- "; TRC$(60, 0) + " " + TRC$(60, 1) + " " + TRC$(60, 2) + ", " + TRC$(60, 3)
    End If
    Print

    Color 14: Print "LOWEST FREE THROW PERCENTAGE, GAME"
    Color 15
    Print Using ".### "; TREC!(61, 0);: Print "- "; TRC$(61, 0) + " " + TRC$(61, 1) + " " + TRC$(61, 2) + ", " + TRC$(61, 3);: Print Using "###/###"; TREC!(61, 1); TREC!(61, 2)
    Print

    Color 14: Print "HIGHEST FREE THROW PERCENTAGE, BOTH TEAMS, GAME"
    Color 15
    Print Using ".### "; TREC!(62, 0);: Print "- "; TRC$(62, 0);
    Print Using " (.###) "; TREC!(62, 1);: Print TRC$(62, 1) + " ";
    Print TRC$(62, 2);: Print Using " (.###)"; TREC!(62, 2);: Print ", " + TRC$(62, 3)
    Print

    Color 14: Print "LOWEST FREE THROW PERCENTAGE, BOTH TEAMS, GAME"
    Color 15
    Print Using ".### "; TREC!(63, 0);: Print "- "; TRC$(63, 0);
    Print Using " (.###) "; TREC!(63, 1);: Print TRC$(63, 1) + " ";
    Print TRC$(63, 2);: Print Using " (.###)"; TREC!(63, 2);: Print ", " + TRC$(63, 3)
    Print

    Color 14: Print "MOST FREE THROWS MADE, GAME"
    Color 15
    Print TREC!(64, 0); "- "; TRC$(64, 0) + " " + TRC$(64, 1) + " " + TRC$(64, 2) + ", " + TRC$(64, 3)
    Print

    Color 14: Print "FEWEST FREE THROWS MADE, GAME"
    Color 15
    Print TREC!(65, 0); "- "; TRC$(65, 0) + " " + TRC$(65, 1) + " " + TRC$(65, 2) + ", " + TRC$(65, 3)
    Print

    Color 14: Print "MOST FREE THROWS MADE, BOTH TEAMS, GAME"
    Color 15
    Print TREC!(66, 0); "- "; TRC$(66, 0);
    Print Using " (###) "; TREC!(66, 1);: Print TRC$(66, 1) + " ";
    Print TRC$(66, 2);: Print Using " (###)"; TREC!(66, 2);: Print ", " + TRC$(66, 3)
    Print

    Color 14: Print "FEWEST FREE THROWS MADE, BOTH TEAMS, GAME"
    Color 15
    Print TREC!(67, 0); "- "; TRC$(67, 0);
    Print Using " (###) "; TREC!(67, 1);: Print TRC$(67, 1) + " ";
    Print TRC$(67, 2);: Print Using " (###)"; TREC!(67, 2);: Print ", " + TRC$(67, 3);

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls

    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST FREE THROWS MADE, ONE HALF"
    Color 15
    Print TREC!(68, 0); "- "; TRC$(68, 0) + " " + TRC$(68, 1) + " " + TRC$(68, 2) + ", " + TRC$(68, 3); HALF$(TREC!(68, 1))
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, GAME"
    Color 15
    Print TREC!(72, 0); "- "; TRC$(72, 0) + " " + TRC$(72, 1) + " " + TRC$(72, 2) + ", " + TRC$(72, 3)
    Print

    Color 14: Print "FEWEST FREE THROW ATTEMPTS, GAME"
    Color 15
    Print TREC!(73, 0); "- "; TRC$(73, 0) + " " + TRC$(73, 1) + " " + TRC$(73, 2) + ", " + TRC$(73, 3)
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, BOTH TEAMS, GAME"
    Color 15
    Print TREC!(74, 0); "- "; TRC$(74, 0);
    Print Using " (###) "; TREC!(74, 1);: Print TRC$(74, 1) + " ";
    Print TRC$(74, 2);: Print Using " (###)"; TREC!(74, 2);: Print ", " + TRC$(74, 3)
    Print

    Color 14: Print "FEWEST FREE THROW ATTEMPTS, BOTH TEAMS, GAME"
    Color 15
    Print TREC!(75, 0); "- "; TRC$(75, 0);
    Print Using " (###) "; TREC!(75, 1);: Print TRC$(75, 1) + " ";
    Print TRC$(75, 2);: Print Using " (###)"; TREC!(75, 2);: Print ", " + TRC$(75, 3)
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, ONE HALF"
    Color 15
    Print TREC!(76, 0); "- "; TRC$(76, 0) + " " + TRC$(76, 1) + " " + TRC$(76, 2) + ", " + TRC$(76, 3); HALF$(TREC!(76, 1))
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, BOTH TEAMS, HALF"
    Color 15
    Print TREC!(77, 0); "- "; TRC$(77, 0);
    Print Using " (###) "; TREC!(77, 1);: Print TRC$(77, 1) + " ";
    Print TRC$(77, 2);: Print Using " (###)"; TREC!(77, 2);: Print ", " + TRC$(77, 3)

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls

    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST REBOUNDS, GAME"
    Color 15
    Print TREC!(80, 0); "- "; TRC$(80, 0) + " " + TRC$(80, 1) + " " + TRC$(80, 2) + ", " + TRC$(80, 3)
    Print

    Color 14: Print "FEWEST REBOUNDS, GAME"
    Color 15
    Print TREC!(81, 0); "- "; TRC$(81, 0) + " " + TRC$(81, 1) + " " + TRC$(81, 2) + ", " + TRC$(81, 3)
    Print

    Color 14: Print "MOST REBOUNDS, BOTH TEAMS, GAME"
    Color 15
    Print TREC!(82, 0); "- "; TRC$(82, 0);
    Print Using " (###) "; TREC!(82, 1);: Print TRC$(82, 1) + " ";
    Print TRC$(82, 2);: Print Using " (###)"; TREC!(82, 2);: Print ", " + TRC$(82, 3)
    Print

    Color 14: Print "FEWEST REBOUNDS, BOTH TEAMS, GAME"
    Color 15
    Print TREC!(83, 0); "- "; TRC$(83, 0);
    Print Using " (###) "; TREC!(83, 1);: Print TRC$(83, 1) + " ";
    Print TRC$(83, 2);: Print Using " (###)"; TREC!(83, 2);: Print ", " + TRC$(83, 3)
    Print

    Color 14: Print "MOST REBOUNDS, ONE HALF"
    Color 15
    Print TREC!(84, 0); "- "; TRC$(84, 0) + " " + TRC$(84, 1) + " " + TRC$(84, 2) + ", " + TRC$(84, 3); HALF$(TREC!(84, 1))
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls

    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST OFFENSIVE REBOUNDS, GAME"
    Color 15
    Print TREC!(86, 0); "- "; TRC$(86, 0) + " " + TRC$(86, 1) + " " + TRC$(86, 2) + ", " + TRC$(86, 3)
    Print

    Color 14: Print "FEWEST OFFENSIVE REBOUNDS, GAME"
    Color 15
    Print TREC!(87, 0); "- "; TRC$(87, 0) + " " + TRC$(87, 1) + " " + TRC$(87, 2) + ", " + TRC$(87, 3)
    Print

    Color 14: Print "MOST OFFENSIVE REBOUNDS, BOTH TEAMS, GAME"
    Color 15
    Print TREC!(88, 0); "- "; TRC$(88, 0);
    Print Using " (###) "; TREC!(88, 1);: Print TRC$(88, 1) + " ";
    Print TRC$(88, 2);: Print Using " (###)"; TREC!(88, 2);: Print ", " + TRC$(88, 3)
    Print

    Color 14: Print "FEWEST OFFENSIVE REBOUNDS, BOTH TEAMS, GAME"
    Color 15
    Print TREC!(89, 0); "- "; TRC$(89, 0);
    Print Using " (###) "; TREC!(89, 1);: Print TRC$(89, 1) + " ";
    Print TRC$(89, 2);: Print Using " (###)"; TREC!(89, 2);: Print ", " + TRC$(89, 3)
    Print

    Color 14: Print "MOST OFFENSIVE REBOUNDS, ONE HALF"
    Color 15
    Print TREC!(90, 0); "- "; TRC$(90, 0) + " " + TRC$(90, 1) + " " + TRC$(90, 2) + ", " + TRC$(90, 3); HALF$(TREC!(90, 1))
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls

    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST ASSISTS, GAME"
    Color 15
    Print TREC!(96, 0); "- "; TRC$(96, 0) + " " + TRC$(96, 1) + " " + TRC$(96, 2) + ", " + TRC$(96, 3)
    Print

    Color 14: Print "FEWEST ASSISTS, GAME"
    Color 15
    Print TREC!(97, 0); "- "; TRC$(97, 0) + " " + TRC$(97, 1) + " " + TRC$(97, 2) + ", " + TRC$(97, 3)
    Print

    Color 14: Print "MOST ASSISTS, BOTH TEAMS, GAME"
    Color 15
    Print TREC!(98, 0); "- "; TRC$(98, 0);
    Print Using " (###) "; TREC!(98, 1);: Print TRC$(98, 1) + " ";
    Print TRC$(98, 2);: Print Using " (###)"; TREC!(98, 2);: Print ", " + TRC$(98, 3)
    Print

    Color 14: Print "FEWEST ASSISTS, BOTH TEAMS, GAME"
    Color 15
    Print TREC!(99, 0); "- "; TRC$(99, 0);
    Print Using " (###) "; TREC!(99, 1);: Print TRC$(99, 1) + " ";
    Print TRC$(99, 2);: Print Using " (###)"; TREC!(99, 2);: Print ", " + TRC$(99, 3)
    Print

    Color 14: Print "MOST ASSISTS, ONE HALF"
    Color 15
    Print TREC!(100, 0); "- "; TRC$(100, 0) + " " + TRC$(100, 1) + " " + TRC$(100, 2) + ", " + TRC$(100, 3); HALF$(TREC!(100, 1))
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"
        
    Color 14: Print "MOST PERSONAL FOULS, GAME"
    Color 15
    Print TREC!(104, 0); "- "; TRC$(104, 0) + " " + TRC$(104, 1) + " " + TRC$(104, 2) + ", " + TRC$(104, 3)
    Print

    Color 14: Print "FEWEST PERSONAL FOULS, GAME"
    Color 15
    Print TREC!(105, 0); "- "; TRC$(105, 0) + " " + TRC$(105, 1) + " " + TRC$(105, 2) + ", " + TRC$(105, 3)
    Print

    Color 14: Print "MOST PERSONAL FOULS, BOTH TEAMS, GAME"
    Color 15
    Print TREC!(106, 0); "- "; TRC$(106, 0);
    Print Using " (###) "; TREC!(106, 1);: Print TRC$(106, 1) + " ";
    Print TRC$(106, 2);: Print Using " (###)"; TREC!(106, 2);: Print ", " + TRC$(106, 3)
    Print

    Color 14: Print "FEWEST PERSONAL FOULS, BOTH TEAMS, GAME"
    Color 15
    Print TREC!(107, 0); "- "; TRC$(107, 0);
    Print Using " (###) "; TREC!(107, 1);: Print TRC$(107, 1) + " ";
    Print TRC$(107, 2);: Print Using " (###)"; TREC!(107, 2);: Print ", " + TRC$(107, 3)
    Print

    Color 14: Print "MOST PERSONAL FOULS, ONE HALF"
    Color 15
    Print TREC!(108, 0); "- "; TRC$(108, 0) + " " + TRC$(108, 1) + " " + TRC$(108, 2) + ", " + TRC$(108, 3); HALF$(TREC!(108, 1))
    Print

    Color 14: Print "MOST PERSONAL FOULS, BOTH TEAMS, ONE HALF"
    Color 15
    Print TREC!(109, 0); "- "; TRC$(109, 0); " "; TRC$(109, 1); " "; TRC$(109, 2); ", "; TRC$(109, 3); HALF$(TREC!(109, 1))
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls

    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST STEALS, GAME"
    Color 15
    Print TREC!(112, 0); "- "; TRC$(112, 0) + " " + TRC$(112, 1) + " " + TRC$(112, 2) + ", " + TRC$(112, 3)
    Print

    Color 14: Print "MOST STEALS, BOTH TEAMS, GAME"
    Color 15
    Print TREC!(114, 0); "- "; TRC$(114, 0);
    Print Using " (###) "; TREC!(114, 1);: Print TRC$(114, 1) + " ";
    Print TRC$(114, 2);: Print Using " (###)"; TREC!(114, 2);: Print ", " + TRC$(114, 3)
    Print

    Color 14: Print "MOST STEALS, ONE HALF"
    Color 15
    Print TREC!(116, 0); "- "; TRC$(116, 0) + " " + TRC$(116, 1) + " " + TRC$(116, 2) + ", " + TRC$(116, 3); HALF$(TREC!(116, 1))
    Print

    Color 14: Print "MOST BLOCKS, GAME"
    Color 15
    Print TREC!(118, 0); "- "; TRC$(118, 0) + " " + TRC$(118, 1) + " " + TRC$(118, 2) + ", " + TRC$(118, 3)
    Print

    Color 14: Print "MOST BLOCKS, BOTH TEAMS, GAME"
    Color 15
    Print TREC!(119, 0); "- "; TRC$(119, 0);
    Print Using " (###) "; TREC!(119, 1);: Print TRC$(119, 1) + " ";
    Print TRC$(119, 2);: Print Using " (###)"; TREC!(119, 2);: Print ", " + TRC$(119, 3)
    Print

    Color 14: Print "MOST BLOCKS, HALF"
    Color 15
    Print TREC!(121, 0); "- "; TRC$(121, 0) + " " + TRC$(121, 1) + " " + TRC$(121, 2) + ", " + TRC$(121, 3); HALF$(TREC!(121, 1))
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls

    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST TURNOVERS, GAME"
    Color 15
    Print TREC!(122, 0); "- "; TRC$(122, 0) + " " + TRC$(122, 1) + " " + TRC$(122, 2) + ", " + TRC$(122, 3)
    Print

    Color 14: Print "FEWEST TURNOVERS, GAME"
    Color 15
    Print TREC!(123, 0); "- "; TRC$(123, 0) + " " + TRC$(123, 1) + " " + TRC$(123, 2) + ", " + TRC$(123, 3)
    Print

    Color 14: Print "MOST TURNOVERS, BOTH TEAMS, GAME"
    Color 15
    Print TREC!(124, 0); "- "; TRC$(124, 0);
    Print Using " (###) "; TREC!(124, 1);: Print TRC$(124, 1) + " ";
    Print TRC$(124, 2);: Print Using " (###)"; TREC!(124, 2);: Print ", " + TRC$(124, 3)
    Print

    Color 14: Print "FEWEST TURNOVERS, BOTH TEAMS, GAME"
    Color 15
    Print TREC!(125, 0); "- "; TRC$(125, 0);
    Print Using " (###) "; TREC!(125, 1);: Print TRC$(125, 1) + " ";
    Print TRC$(125, 2);: Print Using " (###)"; TREC!(125, 2);: Print ", " + TRC$(125, 3)
    Print

    I$ = GetKeyPress$

End Sub

'----------------------------------------
'       PrintGameRecords Subroutine
'----------------------------------------
'...explanation...
Sub PrintGameRecords ()

    Shared REC!(), TREC!()
    Shared recordsA$(), RC$(), TRC$()

    Dim HALF$(2), QTR$(6)

    HALF$(1) = " (1ST HALF)"
    HALF$(2) = " (2ND HALF)"
    QTR$(3) = " (1ST OT)"
    QTR$(4) = " (2ND OT)"
    QTR$(5) = " (3RD OT)"
    QTR$(6) = " (4TH OT)"

    'PRINT ROUTINE
    Open "RECORDS" For Output As #2
    Print #2, "                            INDIVIDUAL RECORDS"
    Print #2,
    
    Print #2, "MOST POINTS, GAME"
    Print #2, REC!(0, 0); "- "; RC$(0, 0); ", "; RC$(0, 1) + " " + RC$(0, 2) + " " + RC$(0, 3) + ", " + RC$(0, 4)
    Print #2,
    
    Print #2, "MOST POINTS, ONE HALF"
    Print #2, REC!(1, 0); "- "; RC$(1, 0); ", "; RC$(1, 1) + " " + RC$(1, 2) + " " + RC$(1, 3) + ", " + RC$(1, 4); HALF$(REC!(1, 2))
    Print #2,
    
    Print #2, "MOST POINTS, OVERTIME PERIOD"
    If REC!(3, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, REC!(3, 0); "- "; RC$(3, 0); ", "; RC$(3, 1) + " " + RC$(3, 2) + " " + RC$(3, 3) + ", " + RC$(3, 4)
    End If
    Print #2,
    
    Print #2, "HIGHEST FIELD GOAL PERCENTAGE, GAME (MINIMUM 15 FIELD GOALS)"
    If REC!(4, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        If REC!(4, 0) = 1 Then
            Print #2, "1.000 ";
        Else
            Print #2, Using " .### "; REC!(4, 0);
        End If

        Print #2, "- "; RC$(4, 0); ", "; RC$(4, 1) + " " + RC$(4, 2) + " " + RC$(4, 3) + ", " + RC$(4, 4);
        Print #2, Using " (##/##)"; REC!(4, 1); REC!(4, 2)
    End If
    Print #2,

    Print #2, "MOST FIELD GOALS, NO MISSES, GAME"
    If REC!(5, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, REC!(5, 0); "- "; RC$(5, 0); ", "; RC$(5, 1) + " " + RC$(5, 2) + " " + RC$(5, 3) + ", " + RC$(5, 4)
    End If
    Print #2,
    
    Print #2, "MOST FIELD GOAL ATTEMPTS, NONE MADE, GAME"
    If REC!(6, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, REC!(6, 0); "- "; RC$(6, 0); ", "; RC$(6, 1) + " " + RC$(6, 2) + " " + RC$(6, 3) + ", " + RC$(6, 4)
    End If
    Print #2,
    
    Print #2, "MOST FIELD GOALS, GAME"
    Print #2, REC!(7, 0); "- "; RC$(7, 0); ", "; RC$(7, 1) + " " + RC$(7, 2) + " " + RC$(7, 3) + ", " + RC$(7, 4)
    Print #2,

    Print #2, "MOST FIELD GOALS, ONE HALF"
    Print #2, REC!(8, 0); "- "; RC$(8, 0); ", "; RC$(8, 1) + " " + RC$(8, 2) + " " + RC$(8, 3) + ", " + RC$(8, 4); HALF$(REC!(8, 2))
    Print #2,

    Print #2, "MOST FIELD GOALS, OVERTIME"
    If REC!(10, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, REC!(10, 0); "- "; RC$(10, 0); ", "; RC$(10, 1) + " " + RC$(10, 2) + " " + RC$(10, 3) + ", " + RC$(10, 4)
    End If
    Print #2,
    
    Print #2, "MOST FIELD GOAL ATTEMPTS, GAME"
    Print #2, REC!(11, 0); "- "; RC$(11, 0); ", "; RC$(11, 1) + " " + RC$(11, 2) + " " + RC$(11, 3) + ", " + RC$(11, 4)
    Print #2,
    
    Print #2, "MOST FIELD GOAL ATTEMPTS, ONE HALF"
    Print #2, REC!(12, 0); "- "; RC$(12, 0); ", "; RC$(12, 1) + " " + RC$(12, 2) + " " + RC$(12, 3) + ", " + RC$(12, 4); HALF$(REC!(12, 2))
    Print #2,
    Print #2,
    
    Print #2, "MOST THREE-POINT FIELD GOALS, NO MISSES, GAME"
    If REC!(15, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, REC!(15, 0); "- "; RC$(15, 0); ", "; RC$(15, 1) + " " + RC$(15, 2) + " " + RC$(15, 3) + ", " + RC$(15, 4)
    End If
    Print #2,
    
    Print #2, "MOST THREE-POINT FIELD GOAL ATTEMPTS, NONE MADE, GAME"
    If REC!(16, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, REC!(16, 0); "- "; RC$(16, 0); ", "; RC$(16, 1) + " " + RC$(16, 2) + " " + RC$(16, 3) + ", " + RC$(16, 4)
    End If

    Print #2,
    
    Print #2, "MOST THREE-POINT FIELD GOALS, GAME"
    If REC!(17, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, REC!(17, 0); "- "; RC$(17, 0); ", "; RC$(17, 1) + " " + RC$(17, 2) + " " + RC$(17, 3) + ", " + RC$(17, 4)
    End If
    Print #2,
    
    Print #2, "MOST THREE-POINT FIELD GOALS MADE, ONE HALF"
    If REC!(18, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, REC!(18, 0); "- "; RC$(18, 0); ", "; RC$(18, 1) + " " + RC$(18, 2) + " " + RC$(18, 3) + ", " + RC$(18, 4); HALF$(REC!(18, 2))
    End If
    Print #2,
    
    Print #2, "MOST THREE-POINT FIELD GOALS MADE, OVERTIME PERIOD"
    If REC!(20, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, REC!(20, 0); "- "; RC$(20, 0); ", "; RC$(20, 1) + " " + RC$(20, 2) + " " + RC$(20, 3) + ", " + RC$(20, 4); QTR$(REC!(20, 2))
    End If
    Print #2,
    
    Print #2, "MOST THREE-POINT FIELD GOAL ATTEMPTS, GAME"
    If REC!(21, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, REC!(21, 0); "- "; RC$(21, 0); ", "; RC$(21, 1) + " " + RC$(21, 2) + " " + RC$(21, 3) + ", " + RC$(21, 4)
    End If
    Print #2,
    
    Print #2, "MOST THREE-POINT FIELD GOAL ATTEMPTS, ONE HALF"
    If REC!(22, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, REC!(22, 0); "- "; RC$(22, 0); ", "; RC$(22, 1) + " " + RC$(22, 2) + " " + RC$(22, 3) + ", " + RC$(22, 4); HALF$(REC!(22, 2))
    End If
    Print #2,
    
    Print #2, "MOST FREE THROWS MADE, NO MISSES, GAME"
    If REC!(23, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, REC!(23, 0); "- "; RC$(23, 0); ", "; RC$(23, 1) + " " + RC$(23, 2) + " " + RC$(23, 3) + ", " + RC$(23, 4)
    End If
    Print #2,
    
    Print #2, "MOST FREE THROW ATTEMPTS, NONE MADE, GAME"
    If REC!(24, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, REC!(24, 0); "- "; RC$(24, 0); ", "; RC$(24, 1) + " " + RC$(24, 2) + " " + RC$(24, 3) + ", " + RC$(24, 4)
    End If
    Print #2,
    
    Print #2, "MOST FREE THROWS MADE, GAME"
    If REC!(25, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, REC!(25, 0); "- "; RC$(25, 0); ", "; RC$(25, 1) + " " + RC$(25, 2) + " " + RC$(25, 3) + ", " + RC$(25, 4)
    End If
    Print #2,
    
    Print #2, "MOST FREE THROWS MADE, ONE HALF"
    If REC!(26, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, REC!(26, 0); "- "; RC$(26, 0); ", "; RC$(26, 1) + " " + RC$(26, 2) + " " + RC$(26, 3) + ", " + RC$(26, 4); HALF$(REC!(26, 2))
    End If
    Print #2,
    
    Print #2, "MOST FREE THROW ATTEMPTS, GAME"
    If REC!(28, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, REC!(28, 0); "- "; RC$(28, 0); ", "; RC$(28, 1) + " " + RC$(28, 2) + " " + RC$(28, 3) + ", " + RC$(28, 4)
    End If
    Print #2,
    
    Print #2, "MOST FREE THROW ATTEMPTS, ONE HALF"
    If REC!(29, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, REC!(29, 0); "- "; RC$(29, 0); ", "; RC$(29, 1) + " " + RC$(29, 2) + " " + RC$(29, 3) + ", " + RC$(29, 4); HALF$(REC!(29, 2))
    End If
    Print #2,
    Print #2,
    
    Print #2, "MOST REBOUNDS, GAME"
    If REC!(31, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, REC!(31, 0); "- "; RC$(31, 0); ", "; RC$(31, 1) + " " + RC$(31, 2) + " " + RC$(31, 3) + ", " + RC$(31, 4)
    End If
    Print #2,
    
    Print #2, "MOST REBOUNDS, ONE HALF"
    If REC!(32, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, REC!(32, 0); "- "; RC$(32, 0); ", "; RC$(32, 1) + " " + RC$(32, 2) + " " + RC$(32, 3) + ", " + RC$(32, 4); HALF$(REC!(32, 2))
    End If
    Print #2,
    
    Print #2, "MOST OFFENSIVE REBOUNDS, GAME"
    If REC!(34, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, REC!(34, 0); "- "; RC$(34, 0); ", "; RC$(34, 1) + " " + RC$(34, 2) + " " + RC$(34, 3) + ", " + RC$(34, 4)
    End If
    Print #2,
    
    Print #2, "MOST OFFENSIVE REBOUNDS, ONE HALF"
    If REC!(35, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, REC!(35, 0); "- "; RC$(35, 0); ", "; RC$(35, 1) + " " + RC$(35, 2) + " " + RC$(35, 3) + ", " + RC$(35, 4); HALF$(REC!(35, 2))
    End If
    Print #2,
    
    Print #2, "MOST DEFENSIVE REBOUNDS, GAME"
    If REC!(37, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, REC!(37, 0); "- "; RC$(37, 0); ", "; RC$(37, 1) + " " + RC$(37, 2) + " " + RC$(37, 3) + ", " + RC$(37, 4)
    End If
    Print #2,
    
    Print #2, "MOST DEFENSIVE REBOUNDS, ONE HALF"
    If REC!(38, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, REC!(38, 0); "- "; RC$(38, 0); ", "; RC$(38, 1) + " " + RC$(38, 2) + " " + RC$(38, 3) + ", " + RC$(38, 4); HALF$(REC!(38, 2))
    End If
    Print #2,
    
    Print #2, "MOST ASSISTS, GAME"
    If REC!(40, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, REC!(40, 0); "- "; RC$(40, 0); ", "; RC$(40, 1) + " " + RC$(40, 2) + " " + RC$(40, 3) + ", " + RC$(40, 4)
    End If
    Print #2,
    
    Print #2, "MOST ASSISTS, ONE HALF"
    If REC!(41, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, REC!(41, 0); "- "; RC$(41, 0); ", "; RC$(41, 1) + " " + RC$(41, 2) + " " + RC$(41, 3) + ", " + RC$(41, 4); HALF$(REC!(41, 2))
    End If
    Print #2,
    
    Print #2, "MOST STEALS, GAME"
    If REC!(43, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, REC!(43, 0); "- "; RC$(43, 0); ", "; RC$(43, 1) + " " + RC$(43, 2) + " " + RC$(43, 3) + ", " + RC$(43, 4)
    End If
    Print #2,
    
    Print #2, "MOST STEALS, ONE HALF"
    If REC!(44, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, REC!(44, 0); "- "; RC$(44, 0); ", "; RC$(44, 1) + " " + RC$(44, 2) + " " + RC$(44, 3) + ", " + RC$(44, 4); HALF$(REC!(44, 2))
    End If
    Print #2,
    
    Print #2, "MOST BLOCKS, GAME"
    If REC!(46, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, REC!(46, 0); "- "; RC$(46, 0); ", "; RC$(46, 1) + " " + RC$(46, 2) + " " + RC$(46, 3) + ", " + RC$(46, 4)
    End If
    Print #2,
    
    Print #2, "MOST BLOCKS, ONE HALF"
    If REC!(47, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, REC!(47, 0); "- "; RC$(47, 0); ", "; RC$(47, 1) + " " + RC$(47, 2) + " " + RC$(47, 3) + ", " + RC$(47, 4); HALF$(REC!(47, 2))
    End If
    Print #2,
    
    Print #2, Chr$(12)
    Print #2, "                            TEAM RECORDS"
    Print #2,

    Print #2, "MOST OVERTIME PERIODS"
    If TREC!(0, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, TREC!(0, 0); "- "; TRC$(0, 1);
        Print #2, Using " (###) AT "; TREC!(0, 1);
        Print #2, TRC$(0, 2);: Print #2, Using " (###)"; TREC!(0, 2);: Print #2, ", " + TRC$(0, 3)
    End If
    Print #2,

    Print #2, "MOST POINTS, GAME"
    Print #2, TREC!(1, 0); "- "; TRC$(1, 0) + " " + TRC$(1, 1) + " " + TRC$(1, 2) + ", " + TRC$(1, 3)
    Print #2,

    Print #2, "FEWEST POINTS, GAME"
    Print #2, TREC!(2, 0); "- "; TRC$(2, 0) + " " + TRC$(2, 1) + " " + TRC$(2, 2) + ", " + TRC$(2, 3)
    Print #2,

    Print #2, "MOST POINTS, BOTH TEAMS, GAME"
    Print #2, TREC!(3, 0); "- "; TRC$(3, 0);
    Print #2, Using " (###) "; TREC!(3, 1);: Print #2, TRC$(3, 1) + " ";
    Print #2, TRC$(3, 2);: Print #2, Using " (###)"; TREC!(3, 2);: Print #2, ", " + TRC$(3, 3)
    Print #2,

    Print #2, "FEWEST POINTS, BOTH TEAMS, GAME"
    Print #2, TREC!(4, 0); "- "; TRC$(4, 0);
    Print #2, Using " (###) "; TREC!(4, 1);: Print #2, TRC$(4, 1) + " ";
    Print #2, TRC$(4, 2);: Print #2, Using " (###)"; TREC!(4, 2);: Print #2, ", " + TRC$(4, 3)
    Print #2,

    Print #2, "MOST POINTS, FIRST HALF"
    Print #2, TREC!(6, 0); "- "; TRC$(6, 0) + " " + TRC$(6, 1) + " " + TRC$(6, 2) + ", " + TRC$(6, 3)
    Print #2,

    Print #2, "FEWEST POINTS, FIRST HALF"
    Print #2, TREC!(7, 0); "- "; TRC$(7, 0) + " " + TRC$(7, 1) + " " + TRC$(7, 2) + ", " + TRC$(7, 3)
    Print #2,
    Print #2,

    Print #2, "MOST POINTS, BOTH TEAMS, FIRST HALF"
    Print #2, TREC!(8, 0); "- "; TRC$(8, 0);
    Print #2, Using " (###) "; TREC!(8, 1);: Print #2, TRC$(8, 1) + " ";
    Print #2, TRC$(8, 2);: Print #2, Using " (###)"; TREC!(8, 2);: Print #2, ", " + TRC$(8, 3)
    Print #2,

    Print #2, "FEWEST POINTS, BOTH TEAMS, FIRST HALF"
    Print #2, TREC!(9, 0); "- "; TRC$(9, 0);
    Print #2, Using " (###) "; TREC!(9, 1);: Print #2, TRC$(9, 1) + " ";
    Print #2, TRC$(9, 2);: Print #2, Using " (###)"; TREC!(9, 2);: Print #2, ", " + TRC$(9, 3)
    Print #2,

    Print #2, "MOST POINTS, SECOND HALF"
    Print #2, TREC!(10, 0); "- "; TRC$(10, 0) + " " + TRC$(10, 1) + " " + TRC$(10, 2) + ", " + TRC$(10, 3)
    Print #2,

    Print #2, "FEWEST POINTS, SECOND HALF"
    Print #2, TREC!(11, 0); "- "; TRC$(11, 0) + " " + TRC$(11, 1) + " " + TRC$(11, 2) + ", " + TRC$(11, 3)
    Print #2,

    Print #2, "MOST POINTS, BOTH TEAMS, SECOND HALF"
    Print #2, TREC!(12, 0); "- "; TRC$(12, 0);
    Print #2, Using " (###) "; TREC!(12, 1);: Print #2, TRC$(12, 1) + " ";
    Print #2, TRC$(12, 2);: Print #2, Using " (###)"; TREC!(12, 2);: Print #2, ", " + TRC$(12, 3)
    Print #2,

    Print #2, "FEWEST POINTS, BOTH TEAMS, SECOND HALF"
    Print #2, TREC!(13, 0); "- "; TRC$(13, 0);
    Print #2, Using " (###) "; TREC!(13, 1);: Print #2, TRC$(13, 1) + " ";
    Print #2, TRC$(13, 2);: Print #2, Using " (###)"; TREC!(13, 2);: Print #2, ", " + TRC$(13, 3)
    Print #2,

    Print #2, "MOST POINTS, OVERTIME"
    If TREC!(14, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, TREC!(14, 0); "- "; TRC$(14, 0) + " " + TRC$(14, 1) + " " + TRC$(14, 2) + ", " + TRC$(14, 3)
    End If
    Print #2,

    Print #2, "FEWEST POINTS, OVERTIME"
    If TREC!(15, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, TREC!(15, 0); "- "; TRC$(15, 0) + " " + TRC$(15, 1) + " " + TRC$(15, 2) + ", " + TRC$(15, 3)
    End If
    Print #2,

    Print #2, "MOST POINTS, BOTH TEAMS, OVERTIME"
    If TREC!(16, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, TREC!(16, 0); "- "; TRC$(16, 0);
        Print #2, Using " (###) "; TREC!(16, 1);: Print #2, TRC$(16, 1) + " ";
        Print #2, TRC$(16, 2);: Print #2, Using " (###)"; TREC!(16, 2);: Print #2, ", " + TRC$(16, 3)
    End If
    Print #2,

    Print #2, "FEWEST POINTS, BOTH TEAMS, OVERTIME"
    If TREC!(17, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, TREC!(17, 0); "- "; TRC$(17, 0);
        Print #2, Using " (###) "; TREC!(17, 1);: Print #2, TRC$(17, 1) + " ";
        Print #2, TRC$(17, 2);: Print #2, Using " (###)"; TREC!(17, 2);: Print #2, ", " + TRC$(17, 3)
    End If
    Print #2,

    Print #2, "HIGHEST FIELD GOAL PERCENTAGE, GAME"
    Print #2, Using ".### "; TREC!(30, 0);: Print #2, "- "; TRC$(30, 0) + " " + TRC$(30, 1) + " " + TRC$(30, 2) + ", " + TRC$(30, 3);: Print #2, Using "###/###"; TREC!(30, 1); TREC!(30, 2)
    Print #2,

    Print #2, "LOWEST FIELD GOAL PERCENTAGE, GAME"
    Print #2, Using ".### "; TREC!(31, 0);: Print #2, "- "; TRC$(31, 0) + " " + TRC$(31, 1) + " " + TRC$(31, 2) + ", " + TRC$(31, 3);: Print #2, Using "###/###"; TREC!(31, 1); TREC!(31, 2)
    Print #2,

    Print #2, "HIGHEST FIELD GOAL PERCENTAGE, BOTH TEAMS, GAME"
    Print #2, Using ".### "; TREC!(32, 0);: Print #2, "- "; TRC$(32, 0);
    Print #2, Using " (.###) "; TREC!(32, 1);: Print #2, TRC$(32, 1) + " ";
    Print #2, TRC$(32, 2);: Print #2, Using " (.###)"; TREC!(32, 2);: Print #2, ", " + TRC$(32, 3)
    Print #2,

    Print #2, "LOWEST FIELD GOAL PERCENTAGE, BOTH TEAMS, GAME"
    Print #2, Using ".### "; TREC!(33, 0);: Print #2, "- "; TRC$(33, 0);
    Print #2, Using " (.###) "; TREC!(33, 1);: Print #2, TRC$(33, 1) + " ";
    Print #2, TRC$(33, 2);: Print #2, Using " (.###)"; TREC!(33, 2);: Print #2, ", " + TRC$(33, 3)
    Print #2,

    Print #2, "MOST FIELD GOALS, GAME"
    Print #2, TREC!(34, 0); "- "; TRC$(34, 0) + " " + TRC$(34, 1) + " " + TRC$(34, 2) + ", " + TRC$(34, 3)
    Print #2,

    Print #2, "FEWEST FIELD GOALS, GAME"
    Print #2, TREC!(35, 0); "- "; TRC$(35, 0) + " " + TRC$(35, 1) + " " + TRC$(35, 2) + ", " + TRC$(35, 3)
    Print #2,

    Print #2, "MOST FIELD GOALS, BOTH TEAMS, GAME"
    Print #2, TREC!(36, 0); "- "; TRC$(36, 0);
    Print #2, Using " (###) "; TREC!(36, 1);: Print #2, TRC$(36, 1) + " ";
    Print #2, TRC$(36, 2);: Print #2, Using " (###)"; TREC!(36, 2);: Print #2, ", " + TRC$(36, 3)
    Print #2,

    Print #2, "FEWEST FIELD GOALS, BOTH TEAMS, GAME"
    Print #2, TREC!(37, 0); "- "; TRC$(37, 0);
    Print #2, Using " (###) "; TREC!(37, 1);: Print #2, TRC$(37, 1) + " ";
    Print #2, TRC$(37, 2);: Print #2, Using " (###)"; TREC!(37, 2);: Print #2, ", " + TRC$(37, 3)
    Print #2,

    Print #2, "MOST FIELD GOALS, ONE HALF"
    Print #2, TREC!(38, 0); "- "; TRC$(38, 0) + " " + TRC$(38, 1) + " " + TRC$(38, 2) + ", " + TRC$(38, 3); HALF$(TREC!(38, 1))
    Print #2,

    Print #2, "MOST FIELD GOALS, BOTH TEAMS, ONE HALF"
    Print #2, TREC!(39, 0); "- "; TRC$(39, 0); " "; TRC$(39, 1); " "; TRC$(39, 2); ", "; TRC$(39, 3); HALF$(TREC!(39, 1))
    Print #2,

    Print #2, "MOST FIELD GOAL ATTEMPTS, GAME"
    Print #2, TREC!(42, 0); "- "; TRC$(42, 0) + " " + TRC$(42, 1) + " " + TRC$(42, 2) + ", " + TRC$(42, 3)
    Print #2,

    Print #2, "FEWEST FIELD GOAL ATTEMPTS, GAME"
    Print #2, TREC!(43, 0); "- "; TRC$(43, 0) + " " + TRC$(43, 1) + " " + TRC$(43, 2) + ", " + TRC$(43, 3)
    Print #2,

    Print #2, "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
    Print #2, TREC!(44, 0); "- "; TRC$(44, 0);
    Print #2, Using " (###) "; TREC!(44, 1);: Print #2, TRC$(44, 1) + " ";
    Print #2, TRC$(44, 2);: Print #2, Using " (###)"; TREC!(44, 2);: Print #2, ", " + TRC$(44, 3)
    Print #2,

    Print #2, "FEWEST FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
    Print #2, TREC!(45, 0); "- "; TRC$(45, 0);
    Print #2, Using " (###) "; TREC!(45, 1);: Print #2, TRC$(45, 1) + " ";
    Print #2, TRC$(45, 2);: Print #2, Using " (###)"; TREC!(45, 2);: Print #2, ", " + TRC$(45, 3)
    Print #2,

    Print #2, "MOST FIELD GOAL ATTEMPTS, ONE HALF"
    Print #2, TREC!(46, 0); "- "; TRC$(46, 0) + " " + TRC$(46, 1) + " " + TRC$(46, 2) + ", " + TRC$(46, 3); HALF$(TREC!(46, 1))
    Print #2,

    Print #2, "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, ONE HALF"
    Print #2, TREC!(47, 0); "- "; TRC$(47, 0); " "; TRC$(47, 1); " "; TRC$(47, 2); ", "; TRC$(47, 3); HALF$(TREC!(47, 1))
    Print #2,

    Print #2, "MOST THREE-POINT FIELD GOALS, NO MISSES, GAME"
    If TREC!(50, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, TREC!(50, 0); "- "; TRC$(50, 0) + " " + TRC$(50, 1) + " " + TRC$(50, 2) + ", " + TRC$(50, 3)
    End If
    Print #2,

    Print #2, "MOST THREE-POINT FIELD GOALS, NO MISSES, BOTH TEAMS, GAME"
    If TREC!(51, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, TREC!(51, 0); "- "; TRC$(51, 0);
        Print #2, Using " (###) "; TREC!(51, 1);: Print #2, TRC$(51, 1) + " ";
        Print #2, TRC$(51, 2);: Print #2, Using " (###)"; TREC!(51, 2);: Print #2, ", " + TRC$(51, 3)
    End If
    Print #2,

    Print #2, "MOST THREE-POINT FIELD GOAL ATTEMPTS, NONE MADE, GAME"
    If TREC!(52, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, TREC!(52, 0); "- "; TRC$(52, 0) + " " + TRC$(52, 1) + " " + TRC$(52, 2) + ", " + TRC$(52, 3)
    End If
    Print #2,
 
    Print #2, "MOST THREE-POINT FIELD GOALS, GAME"
    Print #2, TREC!(53, 0); "- "; TRC$(53, 0) + " " + TRC$(53, 1) + " " + TRC$(53, 2) + ", " + TRC$(53, 3)
    Print #2,

    Print #2, "MOST THREE-POINT FIELD GOALS, BOTH TEAMS, GAME"
    Print #2, TREC!(54, 0); "- "; TRC$(54, 0);
    Print #2, Using " (###) "; TREC!(54, 1);: Print #2, TRC$(54, 1) + " ";
    Print #2, TRC$(54, 2);: Print #2, Using " (###)"; TREC!(54, 2);: Print #2, ", " + TRC$(54, 3)
    Print #2,
    
    Print #2, "MOST THREE-POINT FIELD GOALS, ONE HALF"
    Print #2, TREC!(55, 0); "- "; TRC$(55, 0) + " " + TRC$(55, 1) + " " + TRC$(55, 2) + ", " + TRC$(55, 3); HALF$(TREC!(55, 1))
    Print #2,
    
    Print #2, "MOST THREE-POINT FIELD GOAL ATTEMPTS, GAME"
    Print #2, TREC!(57, 0); "- "; TRC$(57, 0) + " " + TRC$(57, 1) + " " + TRC$(57, 2) + ", " + TRC$(57, 3)
    Print #2,
    
    Print #2, "MOST THREE-POINT FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
    Print #2, TREC!(58, 0); "- "; TRC$(58, 0);
    Print #2, Using " (###) "; TREC!(58, 1);: Print #2, TRC$(58, 1) + " ";
    Print #2, TRC$(58, 2);: Print #2, Using " (###)"; TREC!(58, 2);: Print #2, ", " + TRC$(58, 3)
    Print #2,
    
    Print #2, "MOST THREE-POINT FIELD GOAL ATTEMPTS, ONE HALF"
    Print #2, TREC!(59, 0); "- "; TRC$(59, 0) + " " + TRC$(59, 1) + " " + TRC$(59, 2) + ", " + TRC$(59, 3); HALF$(TREC!(59, 1))
    Print #2,

    Print #2, "MOST FREE THROWS MADE, NO MISSES, GAME"
    If TREC!(60, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, TREC!(60, 0); "- "; TRC$(60, 0) + " " + TRC$(60, 1) + " " + TRC$(60, 2) + ", " + TRC$(60, 3)
    End If
    Print #2,

    Print #2, "LOWEST FREE THROW PERCENTAGE, GAME"
    Print #2, Using ".### "; TREC!(61, 0);: Print #2, "- "; TRC$(61, 0) + " " + TRC$(61, 1) + " " + TRC$(61, 2) + ", " + TRC$(61, 3);: Print #2, Using "###/###"; TREC!(61, 1); TREC!(61, 2)
    Print #2,

    Print #2, "HIGHEST FREE THROW PERCENTAGE, BOTH TEAMS, GAME"
    Print #2, Using ".### "; TREC!(62, 0);: Print #2, "- "; TRC$(62, 0);
    Print #2, Using " (.###) "; TREC!(62, 1);: Print #2, TRC$(62, 1) + " ";
    Print #2, TRC$(62, 2);: Print #2, Using " (.###)"; TREC!(62, 2);: Print #2, ", " + TRC$(62, 3)
    Print #2,

    Print #2, "LOWEST FREE THROW PERCENTAGE, BOTH TEAMS, GAME"
    Print #2, Using ".### "; TREC!(63, 0);: Print #2, "- "; TRC$(63, 0);
    Print #2, Using " (.###) "; TREC!(63, 1);: Print #2, TRC$(63, 1) + " ";
    Print #2, TRC$(63, 2);: Print #2, Using " (.###)"; TREC!(63, 2);: Print #2, ", " + TRC$(63, 3)
    Print #2,

    Print #2, "MOST FREE THROWS MADE, GAME"
    Print #2, TREC!(64, 0); "- "; TRC$(64, 0) + " " + TRC$(64, 1) + " " + TRC$(64, 2) + ", " + TRC$(64, 3)
    Print #2,

    Print #2, "FEWEST FREE THROWS MADE, GAME"
    Print #2, TREC!(65, 0); "- "; TRC$(65, 0) + " " + TRC$(65, 1) + " " + TRC$(65, 2) + ", " + TRC$(65, 3)
    Print #2,

    Print #2, "MOST FREE THROWS MADE, BOTH TEAMS, GAME"
    Print #2, TREC!(66, 0); "- "; TRC$(66, 0);
    Print #2, Using " (###) "; TREC!(66, 1);: Print #2, TRC$(66, 1) + " ";
    Print #2, TRC$(66, 2);: Print #2, Using " (###)"; TREC!(66, 2);: Print #2, ", " + TRC$(66, 3)
    Print #2,

    Print #2, "FEWEST FREE THROWS MADE, BOTH TEAMS, GAME"
    Print #2, TREC!(67, 0); "- "; TRC$(67, 0);
    Print #2, Using " (###) "; TREC!(67, 1);: Print #2, TRC$(67, 1) + " ";
    Print #2, TRC$(67, 2);: Print #2, Using " (###)"; TREC!(67, 2);: Print #2, ", " + TRC$(67, 3)
    Print #2,

    Print #2, "MOST FREE THROWS MADE, ONE HALF"
    Print #2, TREC!(68, 0); "- "; TRC$(68, 0) + " " + TRC$(68, 1) + " " + TRC$(68, 2) + ", " + TRC$(68, 3); HALF$(TREC!(68, 1))
    Print #2,

    Print #2, "MOST FREE THROW ATTEMPTS, GAME"
    Print #2, TREC!(72, 0); "- "; TRC$(72, 0) + " " + TRC$(72, 1) + " " + TRC$(72, 2) + ", " + TRC$(72, 3)
    Print #2,

    Print #2, "FEWEST FREE THROW ATTEMPTS, GAME"
    Print #2, TREC!(73, 0); "- "; TRC$(73, 0) + " " + TRC$(73, 1) + " " + TRC$(73, 2) + ", " + TRC$(73, 3)
    Print #2,

    Print #2, "MOST FREE THROW ATTEMPTS, BOTH TEAMS, GAME"
    Print #2, TREC!(74, 0); "- "; TRC$(74, 0);
    Print #2, Using " (###) "; TREC!(74, 1);: Print #2, TRC$(74, 1) + " ";
    Print #2, TRC$(74, 2);: Print #2, Using " (###)"; TREC!(74, 2);: Print #2, ", " + TRC$(74, 3)
    Print #2,

    Print #2, "FEWEST FREE THROW ATTEMPTS, BOTH TEAMS, GAME"
    Print #2, TREC!(75, 0); "- "; TRC$(75, 0);
    Print #2, Using " (###) "; TREC!(75, 1);: Print #2, TRC$(75, 1) + " ";
    Print #2, TRC$(75, 2);: Print #2, Using " (###)"; TREC!(75, 2);: Print #2, ", " + TRC$(75, 3)
    Print #2,

    Print #2, "MOST FREE THROW ATTEMPTS, ONE HALF"
    Print #2, TREC!(76, 0); "- "; TRC$(76, 0) + " " + TRC$(76, 1) + " " + TRC$(76, 2) + ", " + TRC$(76, 3); HALF$(TREC!(76, 1))
    Print #2,

    Print #2, "MOST FREE THROW ATTEMPTS, BOTH TEAMS, HALF"
    Print #2, TREC!(77, 0); "- "; TRC$(77, 0);
    Print #2, Using " (###) "; TREC!(77, 1);: Print #2, TRC$(77, 1) + " ";
    Print #2, TRC$(77, 2);: Print #2, Using " (###)"; TREC!(77, 2);: Print #2, ", " + TRC$(77, 3)
    Print #2,

    Print #2, "MOST REBOUNDS, GAME"
    Print #2, TREC!(80, 0); "- "; TRC$(80, 0) + " " + TRC$(80, 1) + " " + TRC$(80, 2) + ", " + TRC$(80, 3)
    Print #2,

    Print #2, "FEWEST REBOUNDS, GAME"
    Print #2, TREC!(81, 0); "- "; TRC$(81, 0) + " " + TRC$(81, 1) + " " + TRC$(81, 2) + ", " + TRC$(81, 3)
    Print #2,

    Print #2, "MOST REBOUNDS, BOTH TEAMS, GAME"
    Print #2, TREC!(82, 0); "- "; TRC$(82, 0);
    Print #2, Using " (###) "; TREC!(82, 1);: Print #2, TRC$(82, 1) + " ";
    Print #2, TRC$(82, 2);: Print #2, Using " (###)"; TREC!(82, 2);: Print #2, ", " + TRC$(82, 3)
    Print #2,

    Print #2, "FEWEST REBOUNDS, BOTH TEAMS, GAME"
    Print #2, TREC!(83, 0); "- "; TRC$(83, 0);
    Print #2, Using " (###) "; TREC!(83, 1);: Print #2, TRC$(83, 1) + " ";
    Print #2, TRC$(83, 2);: Print #2, Using " (###)"; TREC!(83, 2);: Print #2, ", " + TRC$(83, 3)
    Print #2,

    Print #2, "MOST REBOUNDS, ONE HALF"
    Print #2, TREC!(84, 0); "- "; TRC$(84, 0) + " " + TRC$(84, 1) + " " + TRC$(84, 2) + ", " + TRC$(84, 3); HALF$(TREC!(84, 1))
    Print #2,

    Print #2, "MOST OFFENSIVE REBOUNDS, GAME"
    Print #2, TREC!(86, 0); "- "; TRC$(86, 0) + " " + TRC$(86, 1) + " " + TRC$(86, 2) + ", " + TRC$(86, 3)
    Print #2,

    Print #2, "FEWEST OFFENSIVE REBOUNDS, GAME"
    Print #2, TREC!(87, 0); "- "; TRC$(87, 0) + " " + TRC$(87, 1) + " " + TRC$(87, 2) + ", " + TRC$(87, 3)
    Print #2,

    Print #2, "MOST OFFENSIVE REBOUNDS, BOTH TEAMS, GAME"
    Print #2, TREC!(88, 0); "- "; TRC$(88, 0);
    Print #2, Using " (###) "; TREC!(88, 1);: Print #2, TRC$(88, 1) + " ";
    Print #2, TRC$(88, 2);: Print #2, Using " (###)"; TREC!(88, 2);: Print #2, ", " + TRC$(88, 3)
    Print #2,

    Print #2, "FEWEST OFFENSIVE REBOUNDS, BOTH TEAMS, GAME"
    Print #2, TREC!(89, 0); "- "; TRC$(89, 0);
    Print #2, Using " (###) "; TREC!(89, 1);: Print #2, TRC$(89, 1) + " ";
    Print #2, TRC$(89, 2);: Print #2, Using " (###)"; TREC!(89, 2);: Print #2, ", " + TRC$(89, 3)
    Print #2,

    Print #2, "MOST OFFENSIVE REBOUNDS, ONE HALF"
    Print #2, TREC!(90, 0); "- "; TRC$(90, 0) + " " + TRC$(90, 1) + " " + TRC$(90, 2) + ", " + TRC$(90, 3); HALF$(TREC!(90, 1))
    Print #2,

    Print #2, "MOST ASSISTS, GAME"
    Print #2, TREC!(96, 0); "- "; TRC$(96, 0) + " " + TRC$(96, 1) + " " + TRC$(96, 2) + ", " + TRC$(96, 3)
    Print #2,

    Print #2, "FEWEST ASSISTS, GAME"
    Print #2, TREC!(97, 0); "- "; TRC$(97, 0) + " " + TRC$(97, 1) + " " + TRC$(97, 2) + ", " + TRC$(97, 3)
    Print #2,

    Print #2, "MOST ASSISTS, BOTH TEAMS, GAME"
    Print #2, TREC!(98, 0); "- "; TRC$(98, 0);
    Print #2, Using " (###) "; TREC!(98, 1);: Print #2, TRC$(98, 1) + " ";
    Print #2, TRC$(98, 2);: Print #2, Using " (###)"; TREC!(98, 2);: Print #2, ", " + TRC$(98, 3)
    Print #2,

    Print #2, "FEWEST ASSISTS, BOTH TEAMS, GAME"
    Print #2, TREC!(99, 0); "- "; TRC$(99, 0);
    Print #2, Using " (###) "; TREC!(99, 1);: Print #2, TRC$(99, 1) + " ";
    Print #2, TRC$(99, 2);: Print #2, Using " (###)"; TREC!(99, 2);: Print #2, ", " + TRC$(99, 3)
    Print #2,

    Print #2, "MOST ASSISTS, ONE HALF"
    Print #2, TREC!(100, 0); "- "; TRC$(100, 0) + " " + TRC$(100, 1) + " " + TRC$(100, 2) + ", " + TRC$(100, 3); HALF$(TREC!(100, 1))
    Print #2,

    Print #2, "MOST PERSONAL FOULS, GAME"
    Print #2, TREC!(104, 0); "- "; TRC$(104, 0) + " " + TRC$(104, 1) + " " + TRC$(104, 2) + ", " + TRC$(104, 3)
    Print #2,

    Print #2, "FEWEST PERSONAL FOULS, GAME"
    Print #2, TREC!(105, 0); "- "; TRC$(105, 0) + " " + TRC$(105, 1) + " " + TRC$(105, 2) + ", " + TRC$(105, 3)
    Print #2,

    Print #2, "MOST PERSONAL FOULS, BOTH TEAMS, GAME"
    Print #2, TREC!(106, 0); "- "; TRC$(106, 0);
    Print #2, Using " (###) "; TREC!(106, 1);: Print #2, TRC$(106, 1) + " ";
    Print #2, TRC$(106, 2);: Print #2, Using " (###)"; TREC!(106, 2);: Print #2, ", " + TRC$(106, 3)
    Print #2,

    Print #2, "FEWEST PERSONAL FOULS, BOTH TEAMS, GAME"
    Print #2, TREC!(107, 0); "- "; TRC$(107, 0);
    Print #2, Using " (###) "; TREC!(107, 1);: Print #2, TRC$(107, 1) + " ";
    Print #2, TRC$(107, 2);: Print #2, Using " (###)"; TREC!(107, 2);: Print #2, ", " + TRC$(107, 3)
    Print #2,

    Print #2, "MOST PERSONAL FOULS, ONE HALF"
    Print #2, TREC!(108, 0); "- "; TRC$(108, 0) + " " + TRC$(108, 1) + " " + TRC$(108, 2) + ", " + TRC$(108, 3); HALF$(TREC!(108, 1))
    Print #2,

    Print #2, "MOST PERSONAL FOULS, BOTH TEAMS, ONE HALF"
    Print #2, TREC!(109, 0); "- "; TRC$(109, 0); " "; TRC$(109, 1); " "; TRC$(109, 2); ", "; TRC$(109, 3); HALF$(TREC!(109, 1))
    Print #2,

    Print #2, "MOST STEALS, GAME"
    Print #2, TREC!(112, 0); "- "; TRC$(112, 0) + " " + TRC$(112, 1) + " " + TRC$(112, 2) + ", " + TRC$(112, 3)
    Print #2,

    Print #2, "MOST STEALS, BOTH TEAMS, GAME"
    Print #2, TREC!(114, 0); "- "; TRC$(114, 0);
    Print #2, Using " (###) "; TREC!(114, 1);: Print #2, TRC$(114, 1) + " ";
    Print #2, TRC$(114, 2);: Print #2, Using " (###)"; TREC!(114, 2);: Print #2, ", " + TRC$(114, 3)
    Print #2,

    Print #2, "MOST STEALS, ONE HALF"
    Print #2, TREC!(116, 0); "- "; TRC$(116, 0) + " " + TRC$(116, 1) + " " + TRC$(116, 2) + ", " + TRC$(116, 3); HALF$(TREC!(116, 1))
    Print #2,

    Print #2, "MOST BLOCKS, GAME"
    Print #2, TREC!(118, 0); "- "; TRC$(118, 0) + " " + TRC$(118, 1) + " " + TRC$(118, 2) + ", " + TRC$(118, 3)
    Print #2,

    Print #2, "MOST BLOCKS, BOTH TEAMS, GAME"
    Print #2, TREC!(119, 0); "- "; TRC$(119, 0);
    Print #2, Using " (###) "; TREC!(119, 1);: Print #2, TRC$(119, 1) + " ";
    Print #2, TRC$(119, 2);: Print #2, Using " (###)"; TREC!(119, 2);: Print #2, ", " + TRC$(119, 3)
    Print #2,

    Print #2, "MOST BLOCKS, HALF"
    Print #2, TREC!(121, 0); "- "; TRC$(121, 0) + " " + TRC$(121, 1) + " " + TRC$(121, 2) + ", " + TRC$(121, 3); HALF$(TREC!(121, 1))
    Print #2,

    Print #2, "MOST TURNOVERS, GAME"
    Print #2, TREC!(122, 0); "- "; TRC$(122, 0) + " " + TRC$(122, 1) + " " + TRC$(122, 2) + ", " + TRC$(122, 3)
    Print #2,

    Print #2, "FEWEST TURNOVERS, GAME"
    Print #2, TREC!(123, 0); "- "; TRC$(123, 0) + " " + TRC$(123, 1) + " " + TRC$(123, 2) + ", " + TRC$(123, 3)
    Print #2,

    Print #2, "MOST TURNOVERS, BOTH TEAMS, GAME"
    Print #2, TREC!(124, 0); "- "; TRC$(124, 0);
    Print #2, Using " (###) "; TREC!(124, 1);: Print #2, TRC$(124, 1) + " ";
    Print #2, TRC$(124, 2);: Print #2, Using " (###)"; TREC!(124, 2);: Print #2, ", " + TRC$(124, 3)
    Print #2,

    Print #2, "FEWEST TURNOVERS, BOTH TEAMS, GAME"
    Print #2, TREC!(125, 0); "- "; TRC$(125, 0);
    Print #2, Using " (###) "; TREC!(125, 1);: Print #2, TRC$(125, 1) + " ";
    Print #2, TRC$(125, 2);: Print #2, Using " (###)"; TREC!(125, 2);: Print #2, ", " + TRC$(125, 3)

    Close 2

    Open "RECORDS" For Input As #2
    
    Do While Not EOF(2)
        Line Input #2, X$
        LPrint X$
    Loop

    LPrint Chr$(12)
    
    Close 2

END SUB
