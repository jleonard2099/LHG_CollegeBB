'----------------------------------------
'         GameRecords Subroutine
'----------------------------------------
'This subroutine reads in the record file
'and gives the user an option of either
'viewing or printing the records.
Sub GameRecords (menuChoice)

    _Title "Courtside College Basketball - Game Records"

    Shared indRec!(), teamRec!()
    
    Shared indRecDesc$(), teamRecDesc$()

    If Not _FileExists (diskPaths$(1) + "RECORDS.GAM") Then

        MsgText$ = "UNABLE TO LOCATE RECORDS PRESS ANY KEY TO RETURN TO MAIN MENU"
        BoxWidth = 24
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)
    
    Else

        Open diskPaths$(1) + "RECORDS.GAM" For Input As #1

        For I = 0 To 48
            For I1 = 0 To 2
                Input #1, indRec!(I, I1)
            Next
            For I1 = 0 To 4
                Input #1, indRecDesc$(I, I1)
            Next
        Next

        For I = 0 To 125
            For I1 = 0 To 2
                Input #1, teamRec!(I, I1)
            Next
            For I1 = 0 To 3
                Input #1, teamRecDesc$(I, I1)
            Next
        Next

        Close #1

        Select Case menuChoice

            Case 1: 
                Call ViewGameRecords

            Case 2: 
                Call PrintGameRecords

        End Select

    End If

End Sub

'----------------------------------------
'       ViewGameRecords Subroutine
'----------------------------------------
'This subroutine formats the records For
'output to screen and displays them.
Sub ViewGameRecords

    Shared indRec!(), teamRec!()
    Shared indRecDesc$(), teamRecDesc$()

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
    Print indRec!(0, 0); "- "; indRecDesc$(0, 0); ", "; indRecDesc$(0, 1) + " " + indRecDesc$(0, 2) + " " + indRecDesc$(0, 3) + ", " + indRecDesc$(0, 4)
    Print

    Color 14: Print "MOST POINTS, ONE HALF"
    Color 15
    Print indRec!(1, 0); "- "; indRecDesc$(1, 0); ", "; indRecDesc$(1, 1) + " " + indRecDesc$(1, 2) + " " + indRecDesc$(1, 3) + ", " + indRecDesc$(1, 4); HALF$(indRec!(1, 2))
    Print

    Color 14: Print "MOST POINTS, OVERTIME PERIOD"
    Color 15
    If indRec!(3, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRec!(3, 0); "- "; indRecDesc$(3, 0); ", "; indRecDesc$(3, 1) + " " + indRecDesc$(3, 2) + " " + indRecDesc$(3, 3) + ", " + indRecDesc$(3, 4)
    End If
    Print

    Color 14: Print "HIGHEST FIELD GOAL PERCENTAGE, GAME (MINIMUM 15 FIELD GOALS)"
    Color 15
    If indRec!(4, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        If indRec!(4, 0) = 1 Then
            Print "1.000 ";
        Else
            Print Using " .### "; indRec!(4, 0);
        End If
        Print "- "; indRecDesc$(4, 0); ", "; indRecDesc$(4, 1) + " " + indRecDesc$(4, 2) + " " + indRecDesc$(4, 3) + ", " + indRecDesc$(4, 4);
        Print Using " (##/##)"; indRec!(4, 1); indRec!(4, 2)
    End If

    Print

    Color 14: Print "MOST FIELD GOALS, NO MISSES, GAME"
    Color 15
    If indRec!(5, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRec!(5, 0); "- "; indRecDesc$(5, 0); ", "; indRecDesc$(5, 1) + " " + indRecDesc$(5, 2) + " " + indRecDesc$(5, 3) + ", " + indRecDesc$(5, 4)
    End If
    Print

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, NONE MADE, GAME"
    Color 15
    If indRec!(6, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRec!(6, 0); "- "; indRecDesc$(6, 0); ", "; indRecDesc$(6, 1) + " " + indRecDesc$(6, 2) + " " + indRecDesc$(6, 3) + ", " + indRecDesc$(6, 4)
    End If

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls

    Color 12: Print Tab(31); "INDIVIDUAL RECORDS"

    Color 14: Print "MOST FIELD GOALS, GAME"
    Color 15
    Print indRec!(7, 0); "- "; indRecDesc$(7, 0); ", "; indRecDesc$(7, 1) + " " + indRecDesc$(7, 2) + " " + indRecDesc$(7, 3) + ", " + indRecDesc$(7, 4)
    Print
    
    Color 14: Print "MOST FIELD GOALS, ONE HALF"
    Color 15
    Print indRec!(8, 0); "- "; indRecDesc$(8, 0); ", "; indRecDesc$(8, 1) + " " + indRecDesc$(8, 2) + " " + indRecDesc$(8, 3) + ", " + indRecDesc$(8, 4); HALF$(indRec!(8, 2))
    Print

    Color 14: Print "MOST FIELD GOALS, OVERTIME"
    Color 15
    If indRec!(10, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRec!(10, 0); "- "; indRecDesc$(10, 0); ", "; indRecDesc$(10, 1) + " " + indRecDesc$(10, 2) + " " + indRecDesc$(10, 3) + ", " + indRecDesc$(10, 4)
    End If
    Print

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, GAME"
    Color 15
    Print indRec!(11, 0); "- "; indRecDesc$(11, 0); ", "; indRecDesc$(11, 1) + " " + indRecDesc$(11, 2) + " " + indRecDesc$(11, 3) + ", " + indRecDesc$(11, 4)
    Print

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, ONE HALF"
    Color 15
    Print indRec!(12, 0); "- "; indRecDesc$(12, 0); ", "; indRecDesc$(12, 1) + " " + indRecDesc$(12, 2) + " " + indRecDesc$(12, 3) + ", " + indRecDesc$(12, 4); HALF$(indRec!(12, 2))
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls

    Color 12: Print Tab(31); "INDIVIDUAL RECORDS"

    Color 14: Print "MOST THREE-POINT FIELD GOALS, NO MISSES, GAME"
    Color 15
    If indRec!(15, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRec!(15, 0); "- "; indRecDesc$(15, 0); ", "; indRecDesc$(15, 1) + " " + indRecDesc$(15, 2) + " " + indRecDesc$(15, 3) + ", " + indRecDesc$(15, 4)
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, NONE MADE, GAME"
    Color 15
    If indRec!(16, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRec!(16, 0); "- "; indRecDesc$(16, 0); ", "; indRecDesc$(16, 1) + " " + indRecDesc$(16, 2) + " " + indRecDesc$(16, 3) + ", " + indRecDesc$(16, 4)
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS, GAME"
    Color 15
    If indRec!(17, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRec!(17, 0); "- "; indRecDesc$(17, 0); ", "; indRecDesc$(17, 1) + " " + indRecDesc$(17, 2) + " " + indRecDesc$(17, 3) + ", " + indRecDesc$(17, 4)
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS MADE, ONE HALF"
    Color 15
    If indRec!(18, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRec!(18, 0); "- "; indRecDesc$(18, 0); ", "; indRecDesc$(18, 1) + " " + indRecDesc$(18, 2) + " " + indRecDesc$(18, 3) + ", " + indRecDesc$(18, 4); HALF$(indRec!(18, 2))
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS MADE, OVERTIME PERIOD"
    Color 15
    If indRec!(20, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRec!(20, 0); "- "; indRecDesc$(20, 0); ", "; indRecDesc$(20, 1) + " " + indRecDesc$(20, 2) + " " + indRecDesc$(20, 3) + ", " + indRecDesc$(20, 4); QTR$(indRec!(20, 2))
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, GAME"
    Color 15
    If indRec!(21, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRec!(21, 0); "- "; indRecDesc$(21, 0); ", "; indRecDesc$(21, 1) + " " + indRecDesc$(21, 2) + " " + indRecDesc$(21, 3) + ", " + indRecDesc$(21, 4)
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, ONE HALF"
    Color 15
    If indRec!(22, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRec!(22, 0); "- "; indRecDesc$(22, 0); ", "; indRecDesc$(22, 1) + " " + indRecDesc$(22, 2) + " " + indRecDesc$(22, 3) + ", " + indRecDesc$(22, 4); HALF$(indRec!(22, 2))
    End If
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls
    Color 12: Print Tab(31); "INDIVIDUAL RECORDS"

    Color 14: Print "MOST FREE THROWS MADE, NO MISSES, GAME"
    Color 15
    If indRec!(23, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRec!(23, 0); "- "; indRecDesc$(23, 0); ", "; indRecDesc$(23, 1) + " " + indRecDesc$(23, 2) + " " + indRecDesc$(23, 3) + ", " + indRecDesc$(23, 4)
    End If
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, NONE MADE, GAME"
    Color 15
    If indRec!(24, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRec!(24, 0); "- "; indRecDesc$(24, 0); ", "; indRecDesc$(24, 1) + " " + indRecDesc$(24, 2) + " " + indRecDesc$(24, 3) + ", " + indRecDesc$(24, 4)
    End If
    Print

    Color 14: Print "MOST FREE THROWS MADE, GAME"
    Color 15
    If indRec!(25, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRec!(25, 0); "- "; indRecDesc$(25, 0); ", "; indRecDesc$(25, 1) + " " + indRecDesc$(25, 2) + " " + indRecDesc$(25, 3) + ", " + indRecDesc$(25, 4)
    End If
    Print

    Color 14: Print "MOST FREE THROWS MADE, ONE HALF"
    Color 15
    If indRec!(26, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRec!(26, 0); "- "; indRecDesc$(26, 0); ", "; indRecDesc$(26, 1) + " " + indRecDesc$(26, 2) + " " + indRecDesc$(26, 3) + ", " + indRecDesc$(26, 4); HALF$(indRec!(26, 2))
    End If
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, GAME"
    Color 15
    If indRec!(28, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRec!(28, 0); "- "; indRecDesc$(28, 0); ", "; indRecDesc$(28, 1) + " " + indRecDesc$(28, 2) + " " + indRecDesc$(28, 3) + ", " + indRecDesc$(28, 4)
    End If
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, ONE HALF"
    Color 15
    If indRec!(29, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRec!(29, 0); "- "; indRecDesc$(29, 0); ", "; indRecDesc$(29, 1) + " " + indRecDesc$(29, 2) + " " + indRecDesc$(29, 3) + ", " + indRecDesc$(29, 4); HALF$(indRec!(29, 2))
    End If
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls
    
    Color 12: Print Tab(31); "INDIVIDUAL RECORDS"

    Color 14: Print "MOST REBOUNDS, GAME"
    Color 15
    If indRec!(31, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRec!(31, 0); "- "; indRecDesc$(31, 0); ", "; indRecDesc$(31, 1) + " " + indRecDesc$(31, 2) + " " + indRecDesc$(31, 3) + ", " + indRecDesc$(31, 4)
    End If
    Print

    Color 14: Print "MOST REBOUNDS, ONE HALF"
    Color 15
    If indRec!(32, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRec!(32, 0); "- "; indRecDesc$(32, 0); ", "; indRecDesc$(32, 1) + " " + indRecDesc$(32, 2) + " " + indRecDesc$(32, 3) + ", " + indRecDesc$(32, 4); HALF$(indRec!(32, 2))
    End If
    Print

    Color 14: Print "MOST OFFENSIVE REBOUNDS, GAME"
    Color 15
    If indRec!(34, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRec!(34, 0); "- "; indRecDesc$(34, 0); ", "; indRecDesc$(34, 1) + " " + indRecDesc$(34, 2) + " " + indRecDesc$(34, 3) + ", " + indRecDesc$(34, 4)
    End If
    Print

    Color 14: Print "MOST OFFENSIVE REBOUNDS, ONE HALF"
    Color 15
    If indRec!(35, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRec!(35, 0); "- "; indRecDesc$(35, 0); ", "; indRecDesc$(35, 1) + " " + indRecDesc$(35, 2) + " " + indRecDesc$(35, 3) + ", " + indRecDesc$(35, 4); HALF$(indRec!(35, 2))
    End If
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls

    Color 12: Print Tab(31); "INDIVIDUAL RECORDS"

    Color 14: Print "MOST DEFENSIVE REBOUNDS, GAME"
    Color 15
    If indRec!(37, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRec!(37, 0); "- "; indRecDesc$(37, 0); ", "; indRecDesc$(37, 1) + " " + indRecDesc$(37, 2) + " " + indRecDesc$(37, 3) + ", " + indRecDesc$(37, 4)
    End If
    Print

    Color 14: Print "MOST DEFENSIVE REBOUNDS, ONE HALF"
    Color 15
    If indRec!(38, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRec!(38, 0); "- "; indRecDesc$(38, 0); ", "; indRecDesc$(38, 1) + " " + indRecDesc$(38, 2) + " " + indRecDesc$(38, 3) + ", " + indRecDesc$(38, 4); HALF$(indRec!(38, 2))
    End If
    Print

    Color 14: Print "MOST ASSISTS, GAME"
    Color 15
    If indRec!(40, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRec!(40, 0); "- "; indRecDesc$(40, 0); ", "; indRecDesc$(40, 1) + " " + indRecDesc$(40, 2) + " " + indRecDesc$(40, 3) + ", " + indRecDesc$(40, 4)
    End If
    Print

    Color 14: Print "MOST ASSISTS, ONE HALF"
    Color 15
    If indRec!(41, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRec!(41, 0); "- "; indRecDesc$(41, 0); ", "; indRecDesc$(41, 1) + " " + indRecDesc$(41, 2) + " " + indRecDesc$(41, 3) + ", " + indRecDesc$(41, 4); HALF$(indRec!(41, 2))
    End If
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls
    Color 12: Print Tab(31); "INDIVIDUAL RECORDS"

    Color 14: Print "MOST STEALS, GAME"
    Color 15
    If indRec!(43, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRec!(43, 0); "- "; indRecDesc$(43, 0); ", "; indRecDesc$(43, 1) + " " + indRecDesc$(43, 2) + " " + indRecDesc$(43, 3) + ", " + indRecDesc$(43, 4)
    End If
    Print

    Color 14: Print "MOST STEALS, ONE HALF"
    Color 15
    If indRec!(44, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRec!(44, 0); "- "; indRecDesc$(44, 0); ", "; indRecDesc$(44, 1) + " " + indRecDesc$(44, 2) + " " + indRecDesc$(44, 3) + ", " + indRecDesc$(44, 4); HALF$(indRec!(44, 2))
    End If
    Print

    Color 14: Print "MOST BLOCKS, GAME"
    Color 15
    If indRec!(46, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRec!(46, 0); "- "; indRecDesc$(46, 0); ", "; indRecDesc$(46, 1) + " " + indRecDesc$(46, 2) + " " + indRecDesc$(46, 3) + ", " + indRecDesc$(46, 4)
    End If
    Print

    Color 14: Print "MOST BLOCKS, ONE HALF"
    Color 15
    If indRec!(47, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRec!(47, 0); "- "; indRecDesc$(47, 0); ", "; indRecDesc$(47, 1) + " " + indRecDesc$(47, 2) + " " + indRecDesc$(47, 3) + ", " + indRecDesc$(47, 4); HALF$(indRec!(47, 2))
    End If
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST OVERTIME PERIODS"
    Color 15
    If teamRec!(0, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print teamRec!(0, 0); "- "; teamRecDesc$(0, 1);
        Print Using " (###) AT "; teamRec!(0, 1);
        Print teamRecDesc$(0, 2);: Print Using " (###)"; teamRec!(0, 2);: Print ", " + teamRecDesc$(0, 3)
    End If
    Print

    Color 14: Print "MOST POINTS, GAME"
    Color 15
    Print teamRec!(1, 0); "- "; teamRecDesc$(1, 0) + " " + teamRecDesc$(1, 1) + " " + teamRecDesc$(1, 2) + ", " + teamRecDesc$(1, 3)
    Print

    Color 14: Print "FEWEST POINTS, GAME"
    Color 15
    Print teamRec!(2, 0); "- "; teamRecDesc$(2, 0) + " " + teamRecDesc$(2, 1) + " " + teamRecDesc$(2, 2) + ", " + teamRecDesc$(2, 3)
    Print

    Color 14: Print "MOST POINTS, BOTH TEAMS, GAME"
    Color 15
    Print teamRec!(3, 0); "- "; teamRecDesc$(3, 0);
    Print Using " (###) "; teamRec!(3, 1);: Print teamRecDesc$(3, 1) + " ";
    Print teamRecDesc$(3, 2);: Print Using " (###)"; teamRec!(3, 2);: Print ", " + teamRecDesc$(3, 3)
    Print

    Color 14: Print "FEWEST POINTS, BOTH TEAMS, GAME"
    Color 15
    Print teamRec!(4, 0); "- "; teamRecDesc$(4, 0);
    Print Using " (###) "; teamRec!(4, 1);: Print teamRecDesc$(4, 1) + " ";
    Print teamRecDesc$(4, 2);: Print Using " (###)"; teamRec!(4, 2);: Print ", " + teamRecDesc$(4, 3)
    Print

    Color 14: Print "MOST POINTS, FIRST HALF"
    Color 15
    Print teamRec!(6, 0); "- "; teamRecDesc$(6, 0) + " " + teamRecDesc$(6, 1) + " " + teamRecDesc$(6, 2) + ", " + teamRecDesc$(6, 3)
    Print

    Color 14: Print "FEWEST POINTS, FIRST HALF"
    Color 15
    Print teamRec!(7, 0); "- "; teamRecDesc$(7, 0) + " " + teamRecDesc$(7, 1) + " " + teamRecDesc$(7, 2) + ", " + teamRecDesc$(7, 3)
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST POINTS, BOTH TEAMS, FIRST HALF"
    Color 15
    Print teamRec!(8, 0); "- "; teamRecDesc$(8, 0);
    Print Using " (###) "; teamRec!(8, 1);: Print teamRecDesc$(8, 1) + " ";
    Print teamRecDesc$(8, 2);: Print Using " (###)"; teamRec!(8, 2);: Print ", " + teamRecDesc$(8, 3)
    Print

    Color 14: Print "FEWEST POINTS, BOTH TEAMS, FIRST HALF"
    Color 15
    Print teamRec!(9, 0); "- "; teamRecDesc$(9, 0);
    Print Using " (###) "; teamRec!(9, 1);: Print teamRecDesc$(9, 1) + " ";
    Print teamRecDesc$(9, 2);: Print Using " (###)"; teamRec!(9, 2);: Print ", " + teamRecDesc$(9, 3)
    Print

    Color 14: Print "MOST POINTS, SECOND HALF"
    Color 15
    Print teamRec!(10, 0); "- "; teamRecDesc$(10, 0) + " " + teamRecDesc$(10, 1) + " " + teamRecDesc$(10, 2) + ", " + teamRecDesc$(10, 3)
    Print

    Color 14: Print "FEWEST POINTS, SECOND HALF"
    Color 15
    Print teamRec!(11, 0); "- "; teamRecDesc$(11, 0) + " " + teamRecDesc$(11, 1) + " " + teamRecDesc$(11, 2) + ", " + teamRecDesc$(11, 3)
    Print

    Color 14: Print "MOST POINTS, BOTH TEAMS, SECOND HALF"
    Color 15
    Print teamRec!(12, 0); "- "; teamRecDesc$(12, 0);
    Print Using " (###) "; teamRec!(12, 1);: Print teamRecDesc$(12, 1) + " ";
    Print teamRecDesc$(12, 2);: Print Using " (###)"; teamRec!(12, 2);: Print ", " + teamRecDesc$(12, 3)
    Print

    Color 14: Print "FEWEST POINTS, BOTH TEAMS, SECOND HALF"
    Color 15
    Print teamRec!(13, 0); "- "; teamRecDesc$(13, 0);
    Print Using " (###) "; teamRec!(13, 1);: Print teamRecDesc$(13, 1) + " ";
    Print teamRecDesc$(13, 2);: Print Using " (###)"; teamRec!(13, 2);: Print ", " + teamRecDesc$(13, 3)
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST POINTS, OVERTIME"
    Color 15
    If teamRec!(14, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print teamRec!(14, 0); "- "; teamRecDesc$(14, 0) + " " + teamRecDesc$(14, 1) + " " + teamRecDesc$(14, 2) + ", " + teamRecDesc$(14, 3)
    End If
    Print

    Color 14: Print "FEWEST POINTS, OVERTIME"
    Color 15
    If teamRec!(15, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print teamRec!(15, 0); "- "; teamRecDesc$(15, 0) + " " + teamRecDesc$(15, 1) + " " + teamRecDesc$(15, 2) + ", " + teamRecDesc$(15, 3)
    End If
    Print

    Color 14: Print "MOST POINTS, BOTH TEAMS, OVERTIME"
    Color 15
    If teamRec!(16, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print teamRec!(16, 0); "- "; teamRecDesc$(16, 0);
        Print Using " (###) "; teamRec!(16, 1);: Print teamRecDesc$(16, 1) + " ";
        Print teamRecDesc$(16, 2);: Print Using " (###)"; teamRec!(16, 2);: Print ", " + teamRecDesc$(16, 3)
    End If
    Print

    Color 14: Print "FEWEST POINTS, BOTH TEAMS, OVERTIME"
    Color 15
    If teamRec!(17, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print teamRec!(17, 0); "- "; teamRecDesc$(17, 0);
        Print Using " (###) "; teamRec!(17, 1);: Print teamRecDesc$(17, 1) + " ";
        Print teamRecDesc$(17, 2);: Print Using " (###)"; teamRec!(17, 2);: Print ", " + teamRecDesc$(17, 3)
    End If
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "HIGHEST FIELD GOAL PERCENTAGE, GAME"
    Color 15
    Print Using ".### "; teamRec!(30, 0);: Print "- "; teamRecDesc$(30, 0) + " " + teamRecDesc$(30, 1) + " " + teamRecDesc$(30, 2) + ", " + teamRecDesc$(30, 3);: Print Using "###/###"; teamRec!(30, 1); teamRec!(30, 2)
    Print

    Color 14: Print "LOWEST FIELD GOAL PERCENTAGE, GAME"
    Color 15
    Print Using ".### "; teamRec!(31, 0);: Print "- "; teamRecDesc$(31, 0) + " " + teamRecDesc$(31, 1) + " " + teamRecDesc$(31, 2) + ", " + teamRecDesc$(31, 3);: Print Using "###/###"; teamRec!(31, 1); teamRec!(31, 2)
    Print

    Color 14: Print "HIGHEST FIELD GOAL PERCENTAGE, BOTH TEAMS, GAME"
    Color 15
    Print Using ".### "; teamRec!(32, 0);: Print "- "; teamRecDesc$(32, 0);
    Print Using " (.###) "; teamRec!(32, 1);: Print teamRecDesc$(32, 1) + " ";
    Print teamRecDesc$(32, 2);: Print Using " (.###)"; teamRec!(32, 2);: Print ", " + teamRecDesc$(32, 3)
    Print

    Color 14: Print "LOWEST FIELD GOAL PERCENTAGE, BOTH TEAMS, GAME"
    Color 15
    Print Using ".### "; teamRec!(33, 0);: Print "- "; teamRecDesc$(33, 0);
    Print Using " (.###) "; teamRec!(33, 1);: Print teamRecDesc$(33, 1) + " ";
    Print teamRecDesc$(33, 2);: Print Using " (.###)"; teamRec!(33, 2);: Print ", " + teamRecDesc$(33, 3)
    Print

    Color 14: Print "MOST FIELD GOALS, GAME"
    Color 15
    Print teamRec!(34, 0); "- "; teamRecDesc$(34, 0) + " " + teamRecDesc$(34, 1) + " " + teamRecDesc$(34, 2) + ", " + teamRecDesc$(34, 3)
    Print

    Color 14: Print "FEWEST FIELD GOALS, GAME"
    Color 15
    Print teamRec!(35, 0); "- "; teamRecDesc$(35, 0) + " " + teamRecDesc$(35, 1) + " " + teamRecDesc$(35, 2) + ", " + teamRecDesc$(35, 3)
    Print

    Color 14: Print "MOST FIELD GOALS, BOTH TEAMS, GAME"
    Color 15
    Print teamRec!(36, 0); "- "; teamRecDesc$(36, 0);
    Print Using " (###) "; teamRec!(36, 1);: Print teamRecDesc$(36, 1) + " ";
    Print teamRecDesc$(36, 2);: Print Using " (###)"; teamRec!(36, 2);: Print ", " + teamRecDesc$(36, 3)
    Print

    Color 14: Print "FEWEST FIELD GOALS, BOTH TEAMS, GAME"
    Color 15
    Print teamRec!(37, 0); "- "; teamRecDesc$(37, 0);
    Print Using " (###) "; teamRec!(37, 1);: Print teamRecDesc$(37, 1) + " ";
    Print teamRecDesc$(37, 2);: Print Using " (###)"; teamRec!(37, 2);: Print ", " + teamRecDesc$(37, 3);

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST FIELD GOALS, ONE HALF"
    Color 15
    Print teamRec!(38, 0); "- "; teamRecDesc$(38, 0) + " " + teamRecDesc$(38, 1) + " " + teamRecDesc$(38, 2) + ", " + teamRecDesc$(38, 3); HALF$(teamRec!(38, 1))
    Print

    Color 14: Print "MOST FIELD GOALS, BOTH TEAMS, ONE HALF"
    Color 15
    Print teamRec!(39, 0); "- "; teamRecDesc$(39, 0); " "; teamRecDesc$(39, 1); " "; teamRecDesc$(39, 2); ", "; teamRecDesc$(39, 3); HALF$(teamRec!(39, 1))
    Print

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, GAME"
    Color 15
    Print teamRec!(42, 0); "- "; teamRecDesc$(42, 0) + " " + teamRecDesc$(42, 1) + " " + teamRecDesc$(42, 2) + ", " + teamRecDesc$(42, 3)
    Print

    Color 14: Print "FEWEST FIELD GOAL ATTEMPTS, GAME"
    Color 15
    Print teamRec!(43, 0); "- "; teamRecDesc$(43, 0) + " " + teamRecDesc$(43, 1) + " " + teamRecDesc$(43, 2) + ", " + teamRecDesc$(43, 3)
    Print

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
    Color 15
    Print teamRec!(44, 0); "- "; teamRecDesc$(44, 0);
    Print Using " (###) "; teamRec!(44, 1);: Print teamRecDesc$(44, 1) + " ";
    Print teamRecDesc$(44, 2);: Print Using " (###)"; teamRec!(44, 2);: Print ", " + teamRecDesc$(44, 3)
    Print

    Color 14: Print "FEWEST FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
    Color 15
    Print teamRec!(45, 0); "- "; teamRecDesc$(45, 0);
    Print Using " (###) "; teamRec!(45, 1);: Print teamRecDesc$(45, 1) + " ";
    Print teamRecDesc$(45, 2);: Print Using " (###)"; teamRec!(45, 2);: Print ", " + teamRecDesc$(45, 3);

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, ONE HALF"
    Color 15
    Print teamRec!(46, 0); "- "; teamRecDesc$(46, 0) + " " + teamRecDesc$(46, 1) + " " + teamRecDesc$(46, 2) + ", " + teamRecDesc$(46, 3); HALF$(teamRec!(46, 1))
    Print

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, ONE HALF"
    Color 15
    Print teamRec!(47, 0); "- "; teamRecDesc$(47, 0); " "; teamRecDesc$(47, 1); " "; teamRecDesc$(47, 2); ", "; teamRecDesc$(47, 3); HALF$(teamRec!(47, 1))
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS, NO MISSES, GAME"
    Color 15
    If teamRec!(50, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print teamRec!(50, 0); "- "; teamRecDesc$(50, 0) + " " + teamRecDesc$(50, 1) + " " + teamRecDesc$(50, 2) + ", " + teamRecDesc$(50, 3)
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS, NO MISSES, BOTH TEAMS, GAME"
    Color 15
    If teamRec!(51, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print teamRec!(51, 0); "- "; teamRecDesc$(51, 0);
        Print Using " (###) "; teamRec!(51, 1);: Print teamRecDesc$(51, 1) + " ";
        Print teamRecDesc$(51, 2);: Print Using " (###)"; teamRec!(51, 2);: Print ", " + teamRecDesc$(51, 3)
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, NONE MADE, GAME"
    Color 15
    If teamRec!(52, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print teamRec!(52, 0); "- "; teamRecDesc$(52, 0) + " " + teamRecDesc$(52, 1) + " " + teamRecDesc$(52, 2) + ", " + teamRecDesc$(52, 3)
    End If
    Print
    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST THREE-POINT FIELD GOALS, GAME"
    Color 15
    Print teamRec!(53, 0); "- "; teamRecDesc$(53, 0) + " " + teamRecDesc$(53, 1) + " " + teamRecDesc$(53, 2) + ", " + teamRecDesc$(53, 3)
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS, BOTH TEAMS, GAME"
    Color 15
    Print teamRec!(54, 0); "- "; teamRecDesc$(54, 0);
    Print Using " (###) "; teamRec!(54, 1);: Print teamRecDesc$(54, 1) + " ";
    Print teamRecDesc$(54, 2);: Print Using " (###)"; teamRec!(54, 2);: Print ", " + teamRecDesc$(54, 3)
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS, ONE HALF"
    Color 15
    Print teamRec!(55, 0); "- "; teamRecDesc$(55, 0) + " " + teamRecDesc$(55, 1) + " " + teamRecDesc$(55, 2) + ", " + teamRecDesc$(55, 3); HALF$(teamRec!(55, 1))
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, GAME"
    Color 15
    Print teamRec!(57, 0); "- "; teamRecDesc$(57, 0) + " " + teamRecDesc$(57, 1) + " " + teamRecDesc$(57, 2) + ", " + teamRecDesc$(57, 3)
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
    Color 15
    Print teamRec!(58, 0); "- "; teamRecDesc$(58, 0);
    Print Using " (###) "; teamRec!(58, 1);: Print teamRecDesc$(58, 1) + " ";
    Print teamRecDesc$(58, 2);: Print Using " (###)"; teamRec!(58, 2);: Print ", " + teamRecDesc$(58, 3)
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, ONE HALF"
    Color 15
    Print teamRec!(59, 0); "- "; teamRecDesc$(59, 0) + " " + teamRecDesc$(59, 1) + " " + teamRecDesc$(59, 2) + ", " + teamRecDesc$(59, 3); HALF$(teamRec!(59, 1))

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls

    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST FREE THROWS MADE, NO MISSES, GAME"
    Color 15
    If teamRec!(60, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print teamRec!(60, 0); "- "; teamRecDesc$(60, 0) + " " + teamRecDesc$(60, 1) + " " + teamRecDesc$(60, 2) + ", " + teamRecDesc$(60, 3)
    End If
    Print

    Color 14: Print "LOWEST FREE THROW PERCENTAGE, GAME"
    Color 15
    Print Using ".### "; teamRec!(61, 0);: Print "- "; teamRecDesc$(61, 0) + " " + teamRecDesc$(61, 1) + " " + teamRecDesc$(61, 2) + ", " + teamRecDesc$(61, 3);: Print Using "###/###"; teamRec!(61, 1); teamRec!(61, 2)
    Print

    Color 14: Print "HIGHEST FREE THROW PERCENTAGE, BOTH TEAMS, GAME"
    Color 15
    Print Using ".### "; teamRec!(62, 0);: Print "- "; teamRecDesc$(62, 0);
    Print Using " (.###) "; teamRec!(62, 1);: Print teamRecDesc$(62, 1) + " ";
    Print teamRecDesc$(62, 2);: Print Using " (.###)"; teamRec!(62, 2);: Print ", " + teamRecDesc$(62, 3)
    Print

    Color 14: Print "LOWEST FREE THROW PERCENTAGE, BOTH TEAMS, GAME"
    Color 15
    Print Using ".### "; teamRec!(63, 0);: Print "- "; teamRecDesc$(63, 0);
    Print Using " (.###) "; teamRec!(63, 1);: Print teamRecDesc$(63, 1) + " ";
    Print teamRecDesc$(63, 2);: Print Using " (.###)"; teamRec!(63, 2);: Print ", " + teamRecDesc$(63, 3)
    Print

    Color 14: Print "MOST FREE THROWS MADE, GAME"
    Color 15
    Print teamRec!(64, 0); "- "; teamRecDesc$(64, 0) + " " + teamRecDesc$(64, 1) + " " + teamRecDesc$(64, 2) + ", " + teamRecDesc$(64, 3)
    Print

    Color 14: Print "FEWEST FREE THROWS MADE, GAME"
    Color 15
    Print teamRec!(65, 0); "- "; teamRecDesc$(65, 0) + " " + teamRecDesc$(65, 1) + " " + teamRecDesc$(65, 2) + ", " + teamRecDesc$(65, 3)
    Print

    Color 14: Print "MOST FREE THROWS MADE, BOTH TEAMS, GAME"
    Color 15
    Print teamRec!(66, 0); "- "; teamRecDesc$(66, 0);
    Print Using " (###) "; teamRec!(66, 1);: Print teamRecDesc$(66, 1) + " ";
    Print teamRecDesc$(66, 2);: Print Using " (###)"; teamRec!(66, 2);: Print ", " + teamRecDesc$(66, 3)
    Print

    Color 14: Print "FEWEST FREE THROWS MADE, BOTH TEAMS, GAME"
    Color 15
    Print teamRec!(67, 0); "- "; teamRecDesc$(67, 0);
    Print Using " (###) "; teamRec!(67, 1);: Print teamRecDesc$(67, 1) + " ";
    Print teamRecDesc$(67, 2);: Print Using " (###)"; teamRec!(67, 2);: Print ", " + teamRecDesc$(67, 3);

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls

    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST FREE THROWS MADE, ONE HALF"
    Color 15
    Print teamRec!(68, 0); "- "; teamRecDesc$(68, 0) + " " + teamRecDesc$(68, 1) + " " + teamRecDesc$(68, 2) + ", " + teamRecDesc$(68, 3); HALF$(teamRec!(68, 1))
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, GAME"
    Color 15
    Print teamRec!(72, 0); "- "; teamRecDesc$(72, 0) + " " + teamRecDesc$(72, 1) + " " + teamRecDesc$(72, 2) + ", " + teamRecDesc$(72, 3)
    Print

    Color 14: Print "FEWEST FREE THROW ATTEMPTS, GAME"
    Color 15
    Print teamRec!(73, 0); "- "; teamRecDesc$(73, 0) + " " + teamRecDesc$(73, 1) + " " + teamRecDesc$(73, 2) + ", " + teamRecDesc$(73, 3)
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, BOTH TEAMS, GAME"
    Color 15
    Print teamRec!(74, 0); "- "; teamRecDesc$(74, 0);
    Print Using " (###) "; teamRec!(74, 1);: Print teamRecDesc$(74, 1) + " ";
    Print teamRecDesc$(74, 2);: Print Using " (###)"; teamRec!(74, 2);: Print ", " + teamRecDesc$(74, 3)
    Print

    Color 14: Print "FEWEST FREE THROW ATTEMPTS, BOTH TEAMS, GAME"
    Color 15
    Print teamRec!(75, 0); "- "; teamRecDesc$(75, 0);
    Print Using " (###) "; teamRec!(75, 1);: Print teamRecDesc$(75, 1) + " ";
    Print teamRecDesc$(75, 2);: Print Using " (###)"; teamRec!(75, 2);: Print ", " + teamRecDesc$(75, 3)
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, ONE HALF"
    Color 15
    Print teamRec!(76, 0); "- "; teamRecDesc$(76, 0) + " " + teamRecDesc$(76, 1) + " " + teamRecDesc$(76, 2) + ", " + teamRecDesc$(76, 3); HALF$(teamRec!(76, 1))
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, BOTH TEAMS, HALF"
    Color 15
    Print teamRec!(77, 0); "- "; teamRecDesc$(77, 0);
    Print Using " (###) "; teamRec!(77, 1);: Print teamRecDesc$(77, 1) + " ";
    Print teamRecDesc$(77, 2);: Print Using " (###)"; teamRec!(77, 2);: Print ", " + teamRecDesc$(77, 3)

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls

    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST REBOUNDS, GAME"
    Color 15
    Print teamRec!(80, 0); "- "; teamRecDesc$(80, 0) + " " + teamRecDesc$(80, 1) + " " + teamRecDesc$(80, 2) + ", " + teamRecDesc$(80, 3)
    Print

    Color 14: Print "FEWEST REBOUNDS, GAME"
    Color 15
    Print teamRec!(81, 0); "- "; teamRecDesc$(81, 0) + " " + teamRecDesc$(81, 1) + " " + teamRecDesc$(81, 2) + ", " + teamRecDesc$(81, 3)
    Print

    Color 14: Print "MOST REBOUNDS, BOTH TEAMS, GAME"
    Color 15
    Print teamRec!(82, 0); "- "; teamRecDesc$(82, 0);
    Print Using " (###) "; teamRec!(82, 1);: Print teamRecDesc$(82, 1) + " ";
    Print teamRecDesc$(82, 2);: Print Using " (###)"; teamRec!(82, 2);: Print ", " + teamRecDesc$(82, 3)
    Print

    Color 14: Print "FEWEST REBOUNDS, BOTH TEAMS, GAME"
    Color 15
    Print teamRec!(83, 0); "- "; teamRecDesc$(83, 0);
    Print Using " (###) "; teamRec!(83, 1);: Print teamRecDesc$(83, 1) + " ";
    Print teamRecDesc$(83, 2);: Print Using " (###)"; teamRec!(83, 2);: Print ", " + teamRecDesc$(83, 3)
    Print

    Color 14: Print "MOST REBOUNDS, ONE HALF"
    Color 15
    Print teamRec!(84, 0); "- "; teamRecDesc$(84, 0) + " " + teamRecDesc$(84, 1) + " " + teamRecDesc$(84, 2) + ", " + teamRecDesc$(84, 3); HALF$(teamRec!(84, 1))
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub 'CALL RECCON

    Cls

    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST OFFENSIVE REBOUNDS, GAME"
    Color 15
    Print teamRec!(86, 0); "- "; teamRecDesc$(86, 0) + " " + teamRecDesc$(86, 1) + " " + teamRecDesc$(86, 2) + ", " + teamRecDesc$(86, 3)
    Print

    Color 14: Print "FEWEST OFFENSIVE REBOUNDS, GAME"
    Color 15
    Print teamRec!(87, 0); "- "; teamRecDesc$(87, 0) + " " + teamRecDesc$(87, 1) + " " + teamRecDesc$(87, 2) + ", " + teamRecDesc$(87, 3)
    Print

    Color 14: Print "MOST OFFENSIVE REBOUNDS, BOTH TEAMS, GAME"
    Color 15
    Print teamRec!(88, 0); "- "; teamRecDesc$(88, 0);
    Print Using " (###) "; teamRec!(88, 1);: Print teamRecDesc$(88, 1) + " ";
    Print teamRecDesc$(88, 2);: Print Using " (###)"; teamRec!(88, 2);: Print ", " + teamRecDesc$(88, 3)
    Print

    Color 14: Print "FEWEST OFFENSIVE REBOUNDS, BOTH TEAMS, GAME"
    Color 15
    Print teamRec!(89, 0); "- "; teamRecDesc$(89, 0);
    Print Using " (###) "; teamRec!(89, 1);: Print teamRecDesc$(89, 1) + " ";
    Print teamRecDesc$(89, 2);: Print Using " (###)"; teamRec!(89, 2);: Print ", " + teamRecDesc$(89, 3)
    Print

    Color 14: Print "MOST OFFENSIVE REBOUNDS, ONE HALF"
    Color 15
    Print teamRec!(90, 0); "- "; teamRecDesc$(90, 0) + " " + teamRecDesc$(90, 1) + " " + teamRecDesc$(90, 2) + ", " + teamRecDesc$(90, 3); HALF$(teamRec!(90, 1))
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls

    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST ASSISTS, GAME"
    Color 15
    Print teamRec!(96, 0); "- "; teamRecDesc$(96, 0) + " " + teamRecDesc$(96, 1) + " " + teamRecDesc$(96, 2) + ", " + teamRecDesc$(96, 3)
    Print

    Color 14: Print "FEWEST ASSISTS, GAME"
    Color 15
    Print teamRec!(97, 0); "- "; teamRecDesc$(97, 0) + " " + teamRecDesc$(97, 1) + " " + teamRecDesc$(97, 2) + ", " + teamRecDesc$(97, 3)
    Print

    Color 14: Print "MOST ASSISTS, BOTH TEAMS, GAME"
    Color 15
    Print teamRec!(98, 0); "- "; teamRecDesc$(98, 0);
    Print Using " (###) "; teamRec!(98, 1);: Print teamRecDesc$(98, 1) + " ";
    Print teamRecDesc$(98, 2);: Print Using " (###)"; teamRec!(98, 2);: Print ", " + teamRecDesc$(98, 3)
    Print

    Color 14: Print "FEWEST ASSISTS, BOTH TEAMS, GAME"
    Color 15
    Print teamRec!(99, 0); "- "; teamRecDesc$(99, 0);
    Print Using " (###) "; teamRec!(99, 1);: Print teamRecDesc$(99, 1) + " ";
    Print teamRecDesc$(99, 2);: Print Using " (###)"; teamRec!(99, 2);: Print ", " + teamRecDesc$(99, 3)
    Print

    Color 14: Print "MOST ASSISTS, ONE HALF"
    Color 15
    Print teamRec!(100, 0); "- "; teamRecDesc$(100, 0) + " " + teamRecDesc$(100, 1) + " " + teamRecDesc$(100, 2) + ", " + teamRecDesc$(100, 3); HALF$(teamRec!(100, 1))
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"
        
    Color 14: Print "MOST PERSONAL FOULS, GAME"
    Color 15
    Print teamRec!(104, 0); "- "; teamRecDesc$(104, 0) + " " + teamRecDesc$(104, 1) + " " + teamRecDesc$(104, 2) + ", " + teamRecDesc$(104, 3)
    Print

    Color 14: Print "FEWEST PERSONAL FOULS, GAME"
    Color 15
    Print teamRec!(105, 0); "- "; teamRecDesc$(105, 0) + " " + teamRecDesc$(105, 1) + " " + teamRecDesc$(105, 2) + ", " + teamRecDesc$(105, 3)
    Print

    Color 14: Print "MOST PERSONAL FOULS, BOTH TEAMS, GAME"
    Color 15
    Print teamRec!(106, 0); "- "; teamRecDesc$(106, 0);
    Print Using " (###) "; teamRec!(106, 1);: Print teamRecDesc$(106, 1) + " ";
    Print teamRecDesc$(106, 2);: Print Using " (###)"; teamRec!(106, 2);: Print ", " + teamRecDesc$(106, 3)
    Print

    Color 14: Print "FEWEST PERSONAL FOULS, BOTH TEAMS, GAME"
    Color 15
    Print teamRec!(107, 0); "- "; teamRecDesc$(107, 0);
    Print Using " (###) "; teamRec!(107, 1);: Print teamRecDesc$(107, 1) + " ";
    Print teamRecDesc$(107, 2);: Print Using " (###)"; teamRec!(107, 2);: Print ", " + teamRecDesc$(107, 3)
    Print

    Color 14: Print "MOST PERSONAL FOULS, ONE HALF"
    Color 15
    Print teamRec!(108, 0); "- "; teamRecDesc$(108, 0) + " " + teamRecDesc$(108, 1) + " " + teamRecDesc$(108, 2) + ", " + teamRecDesc$(108, 3); HALF$(teamRec!(108, 1))
    Print

    Color 14: Print "MOST PERSONAL FOULS, BOTH TEAMS, ONE HALF"
    Color 15
    Print teamRec!(109, 0); "- "; teamRecDesc$(109, 0); " "; teamRecDesc$(109, 1); " "; teamRecDesc$(109, 2); ", "; teamRecDesc$(109, 3); HALF$(teamRec!(109, 1))
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls

    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST STEALS, GAME"
    Color 15
    Print teamRec!(112, 0); "- "; teamRecDesc$(112, 0) + " " + teamRecDesc$(112, 1) + " " + teamRecDesc$(112, 2) + ", " + teamRecDesc$(112, 3)
    Print

    Color 14: Print "MOST STEALS, BOTH TEAMS, GAME"
    Color 15
    Print teamRec!(114, 0); "- "; teamRecDesc$(114, 0);
    Print Using " (###) "; teamRec!(114, 1);: Print teamRecDesc$(114, 1) + " ";
    Print teamRecDesc$(114, 2);: Print Using " (###)"; teamRec!(114, 2);: Print ", " + teamRecDesc$(114, 3)
    Print

    Color 14: Print "MOST STEALS, ONE HALF"
    Color 15
    Print teamRec!(116, 0); "- "; teamRecDesc$(116, 0) + " " + teamRecDesc$(116, 1) + " " + teamRecDesc$(116, 2) + ", " + teamRecDesc$(116, 3); HALF$(teamRec!(116, 1))
    Print

    Color 14: Print "MOST BLOCKS, GAME"
    Color 15
    Print teamRec!(118, 0); "- "; teamRecDesc$(118, 0) + " " + teamRecDesc$(118, 1) + " " + teamRecDesc$(118, 2) + ", " + teamRecDesc$(118, 3)
    Print

    Color 14: Print "MOST BLOCKS, BOTH TEAMS, GAME"
    Color 15
    Print teamRec!(119, 0); "- "; teamRecDesc$(119, 0);
    Print Using " (###) "; teamRec!(119, 1);: Print teamRecDesc$(119, 1) + " ";
    Print teamRecDesc$(119, 2);: Print Using " (###)"; teamRec!(119, 2);: Print ", " + teamRecDesc$(119, 3)
    Print

    Color 14: Print "MOST BLOCKS, HALF"
    Color 15
    Print teamRec!(121, 0); "- "; teamRecDesc$(121, 0) + " " + teamRecDesc$(121, 1) + " " + teamRecDesc$(121, 2) + ", " + teamRecDesc$(121, 3); HALF$(teamRec!(121, 1))
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls

    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST TURNOVERS, GAME"
    Color 15
    Print teamRec!(122, 0); "- "; teamRecDesc$(122, 0) + " " + teamRecDesc$(122, 1) + " " + teamRecDesc$(122, 2) + ", " + teamRecDesc$(122, 3)
    Print

    Color 14: Print "FEWEST TURNOVERS, GAME"
    Color 15
    Print teamRec!(123, 0); "- "; teamRecDesc$(123, 0) + " " + teamRecDesc$(123, 1) + " " + teamRecDesc$(123, 2) + ", " + teamRecDesc$(123, 3)
    Print

    Color 14: Print "MOST TURNOVERS, BOTH TEAMS, GAME"
    Color 15
    Print teamRec!(124, 0); "- "; teamRecDesc$(124, 0);
    Print Using " (###) "; teamRec!(124, 1);: Print teamRecDesc$(124, 1) + " ";
    Print teamRecDesc$(124, 2);: Print Using " (###)"; teamRec!(124, 2);: Print ", " + teamRecDesc$(124, 3)
    Print

    Color 14: Print "FEWEST TURNOVERS, BOTH TEAMS, GAME"
    Color 15
    Print teamRec!(125, 0); "- "; teamRecDesc$(125, 0);
    Print Using " (###) "; teamRec!(125, 1);: Print teamRecDesc$(125, 1) + " ";
    Print teamRecDesc$(125, 2);: Print Using " (###)"; teamRec!(125, 2);: Print ", " + teamRecDesc$(125, 3)
    Print

    I$ = GetKeyPress$

End Sub


'----------------------------------------
'      PrintGameRecords Subroutine
'----------------------------------------
'This subroutine formats and prints the 
'records on the default printer
Sub PrintGameRecords ()

    Shared indRec!(), teamRec!()
    Shared indRecDesc$(), teamRecDesc$()

    Dim HALF$(2), QTR$(6)

    HALF$(1) = " (1ST HALF)"
    HALF$(2) = " (2ND HALF)"
    QTR$(3) = " (1ST OT)"
    QTR$(4) = " (2ND OT)"
    QTR$(5) = " (3RD OT)"
    QTR$(6) = " (4TH OT)"

    'PRINT ROUTINE
    Open "RECORDS" For Output As #1
    Print #1, "                            INDIVIDUAL RECORDS"
    Print #1,
    
    Print #1, "MOST POINTS, GAME"
    Print #1, indRec!(0, 0); "- "; indRecDesc$(0, 0); ", "; indRecDesc$(0, 1) + " " + indRecDesc$(0, 2) + " " + indRecDesc$(0, 3) + ", " + indRecDesc$(0, 4)
    Print #1,
    
    Print #1, "MOST POINTS, ONE HALF"
    Print #1, indRec!(1, 0); "- "; indRecDesc$(1, 0); ", "; indRecDesc$(1, 1) + " " + indRecDesc$(1, 2) + " " + indRecDesc$(1, 3) + ", " + indRecDesc$(1, 4); HALF$(indRec!(1, 2))
    Print #1,
    
    Print #1, "MOST POINTS, OVERTIME PERIOD"
    If indRec!(3, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRec!(3, 0); "- "; indRecDesc$(3, 0); ", "; indRecDesc$(3, 1) + " " + indRecDesc$(3, 2) + " " + indRecDesc$(3, 3) + ", " + indRecDesc$(3, 4)
    End If
    Print #1,
    
    Print #1, "HIGHEST FIELD GOAL PERCENTAGE, GAME (MINIMUM 15 FIELD GOALS)"
    If indRec!(4, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        If indRec!(4, 0) = 1 Then
            Print #1, "1.000 ";
        Else
            Print #1, Using " .### "; indRec!(4, 0);
        End If

        Print #1, "- "; indRecDesc$(4, 0); ", "; indRecDesc$(4, 1) + " " + indRecDesc$(4, 2) + " " + indRecDesc$(4, 3) + ", " + indRecDesc$(4, 4);
        Print #1, Using " (##/##)"; indRec!(4, 1); indRec!(4, 2)
    End If
    Print #1,

    Print #1, "MOST FIELD GOALS, NO MISSES, GAME"
    If indRec!(5, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRec!(5, 0); "- "; indRecDesc$(5, 0); ", "; indRecDesc$(5, 1) + " " + indRecDesc$(5, 2) + " " + indRecDesc$(5, 3) + ", " + indRecDesc$(5, 4)
    End If
    Print #1,
    
    Print #1, "MOST FIELD GOAL ATTEMPTS, NONE MADE, GAME"
    If indRec!(6, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRec!(6, 0); "- "; indRecDesc$(6, 0); ", "; indRecDesc$(6, 1) + " " + indRecDesc$(6, 2) + " " + indRecDesc$(6, 3) + ", " + indRecDesc$(6, 4)
    End If
    Print #1,
    
    Print #1, "MOST FIELD GOALS, GAME"
    Print #1, indRec!(7, 0); "- "; indRecDesc$(7, 0); ", "; indRecDesc$(7, 1) + " " + indRecDesc$(7, 2) + " " + indRecDesc$(7, 3) + ", " + indRecDesc$(7, 4)
    Print #1,

    Print #1, "MOST FIELD GOALS, ONE HALF"
    Print #1, indRec!(8, 0); "- "; indRecDesc$(8, 0); ", "; indRecDesc$(8, 1) + " " + indRecDesc$(8, 2) + " " + indRecDesc$(8, 3) + ", " + indRecDesc$(8, 4); HALF$(indRec!(8, 2))
    Print #1,

    Print #1, "MOST FIELD GOALS, OVERTIME"
    If indRec!(10, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRec!(10, 0); "- "; indRecDesc$(10, 0); ", "; indRecDesc$(10, 1) + " " + indRecDesc$(10, 2) + " " + indRecDesc$(10, 3) + ", " + indRecDesc$(10, 4)
    End If
    Print #1,
    
    Print #1, "MOST FIELD GOAL ATTEMPTS, GAME"
    Print #1, indRec!(11, 0); "- "; indRecDesc$(11, 0); ", "; indRecDesc$(11, 1) + " " + indRecDesc$(11, 2) + " " + indRecDesc$(11, 3) + ", " + indRecDesc$(11, 4)
    Print #1,
    
    Print #1, "MOST FIELD GOAL ATTEMPTS, ONE HALF"
    Print #1, indRec!(12, 0); "- "; indRecDesc$(12, 0); ", "; indRecDesc$(12, 1) + " " + indRecDesc$(12, 2) + " " + indRecDesc$(12, 3) + ", " + indRecDesc$(12, 4); HALF$(indRec!(12, 2))
    Print #1,
    Print #1,
    
    Print #1, "MOST THREE-POINT FIELD GOALS, NO MISSES, GAME"
    If indRec!(15, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRec!(15, 0); "- "; indRecDesc$(15, 0); ", "; indRecDesc$(15, 1) + " " + indRecDesc$(15, 2) + " " + indRecDesc$(15, 3) + ", " + indRecDesc$(15, 4)
    End If
    Print #1,
    
    Print #1, "MOST THREE-POINT FIELD GOAL ATTEMPTS, NONE MADE, GAME"
    If indRec!(16, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRec!(16, 0); "- "; indRecDesc$(16, 0); ", "; indRecDesc$(16, 1) + " " + indRecDesc$(16, 2) + " " + indRecDesc$(16, 3) + ", " + indRecDesc$(16, 4)
    End If

    Print #1,
    
    Print #1, "MOST THREE-POINT FIELD GOALS, GAME"
    If indRec!(17, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRec!(17, 0); "- "; indRecDesc$(17, 0); ", "; indRecDesc$(17, 1) + " " + indRecDesc$(17, 2) + " " + indRecDesc$(17, 3) + ", " + indRecDesc$(17, 4)
    End If
    Print #1,
    
    Print #1, "MOST THREE-POINT FIELD GOALS MADE, ONE HALF"
    If indRec!(18, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRec!(18, 0); "- "; indRecDesc$(18, 0); ", "; indRecDesc$(18, 1) + " " + indRecDesc$(18, 2) + " " + indRecDesc$(18, 3) + ", " + indRecDesc$(18, 4); HALF$(indRec!(18, 2))
    End If
    Print #1,
    
    Print #1, "MOST THREE-POINT FIELD GOALS MADE, OVERTIME PERIOD"
    If indRec!(20, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRec!(20, 0); "- "; indRecDesc$(20, 0); ", "; indRecDesc$(20, 1) + " " + indRecDesc$(20, 2) + " " + indRecDesc$(20, 3) + ", " + indRecDesc$(20, 4); QTR$(indRec!(20, 2))
    End If
    Print #1,
    
    Print #1, "MOST THREE-POINT FIELD GOAL ATTEMPTS, GAME"
    If indRec!(21, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRec!(21, 0); "- "; indRecDesc$(21, 0); ", "; indRecDesc$(21, 1) + " " + indRecDesc$(21, 2) + " " + indRecDesc$(21, 3) + ", " + indRecDesc$(21, 4)
    End If
    Print #1,
    
    Print #1, "MOST THREE-POINT FIELD GOAL ATTEMPTS, ONE HALF"
    If indRec!(22, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRec!(22, 0); "- "; indRecDesc$(22, 0); ", "; indRecDesc$(22, 1) + " " + indRecDesc$(22, 2) + " " + indRecDesc$(22, 3) + ", " + indRecDesc$(22, 4); HALF$(indRec!(22, 2))
    End If
    Print #1,
    
    Print #1, "MOST FREE THROWS MADE, NO MISSES, GAME"
    If indRec!(23, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRec!(23, 0); "- "; indRecDesc$(23, 0); ", "; indRecDesc$(23, 1) + " " + indRecDesc$(23, 2) + " " + indRecDesc$(23, 3) + ", " + indRecDesc$(23, 4)
    End If
    Print #1,
    
    Print #1, "MOST FREE THROW ATTEMPTS, NONE MADE, GAME"
    If indRec!(24, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRec!(24, 0); "- "; indRecDesc$(24, 0); ", "; indRecDesc$(24, 1) + " " + indRecDesc$(24, 2) + " " + indRecDesc$(24, 3) + ", " + indRecDesc$(24, 4)
    End If
    Print #1,
    
    Print #1, "MOST FREE THROWS MADE, GAME"
    If indRec!(25, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRec!(25, 0); "- "; indRecDesc$(25, 0); ", "; indRecDesc$(25, 1) + " " + indRecDesc$(25, 2) + " " + indRecDesc$(25, 3) + ", " + indRecDesc$(25, 4)
    End If
    Print #1,
    
    Print #1, "MOST FREE THROWS MADE, ONE HALF"
    If indRec!(26, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRec!(26, 0); "- "; indRecDesc$(26, 0); ", "; indRecDesc$(26, 1) + " " + indRecDesc$(26, 2) + " " + indRecDesc$(26, 3) + ", " + indRecDesc$(26, 4); HALF$(indRec!(26, 2))
    End If
    Print #1,
    
    Print #1, "MOST FREE THROW ATTEMPTS, GAME"
    If indRec!(28, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRec!(28, 0); "- "; indRecDesc$(28, 0); ", "; indRecDesc$(28, 1) + " " + indRecDesc$(28, 2) + " " + indRecDesc$(28, 3) + ", " + indRecDesc$(28, 4)
    End If
    Print #1,
    
    Print #1, "MOST FREE THROW ATTEMPTS, ONE HALF"
    If indRec!(29, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRec!(29, 0); "- "; indRecDesc$(29, 0); ", "; indRecDesc$(29, 1) + " " + indRecDesc$(29, 2) + " " + indRecDesc$(29, 3) + ", " + indRecDesc$(29, 4); HALF$(indRec!(29, 2))
    End If
    Print #1,
    Print #1,
    
    Print #1, "MOST REBOUNDS, GAME"
    If indRec!(31, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRec!(31, 0); "- "; indRecDesc$(31, 0); ", "; indRecDesc$(31, 1) + " " + indRecDesc$(31, 2) + " " + indRecDesc$(31, 3) + ", " + indRecDesc$(31, 4)
    End If
    Print #1,
    
    Print #1, "MOST REBOUNDS, ONE HALF"
    If indRec!(32, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRec!(32, 0); "- "; indRecDesc$(32, 0); ", "; indRecDesc$(32, 1) + " " + indRecDesc$(32, 2) + " " + indRecDesc$(32, 3) + ", " + indRecDesc$(32, 4); HALF$(indRec!(32, 2))
    End If
    Print #1,
    
    Print #1, "MOST OFFENSIVE REBOUNDS, GAME"
    If indRec!(34, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRec!(34, 0); "- "; indRecDesc$(34, 0); ", "; indRecDesc$(34, 1) + " " + indRecDesc$(34, 2) + " " + indRecDesc$(34, 3) + ", " + indRecDesc$(34, 4)
    End If
    Print #1,
    
    Print #1, "MOST OFFENSIVE REBOUNDS, ONE HALF"
    If indRec!(35, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRec!(35, 0); "- "; indRecDesc$(35, 0); ", "; indRecDesc$(35, 1) + " " + indRecDesc$(35, 2) + " " + indRecDesc$(35, 3) + ", " + indRecDesc$(35, 4); HALF$(indRec!(35, 2))
    End If
    Print #1,
    
    Print #1, "MOST DEFENSIVE REBOUNDS, GAME"
    If indRec!(37, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRec!(37, 0); "- "; indRecDesc$(37, 0); ", "; indRecDesc$(37, 1) + " " + indRecDesc$(37, 2) + " " + indRecDesc$(37, 3) + ", " + indRecDesc$(37, 4)
    End If
    Print #1,
    
    Print #1, "MOST DEFENSIVE REBOUNDS, ONE HALF"
    If indRec!(38, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRec!(38, 0); "- "; indRecDesc$(38, 0); ", "; indRecDesc$(38, 1) + " " + indRecDesc$(38, 2) + " " + indRecDesc$(38, 3) + ", " + indRecDesc$(38, 4); HALF$(indRec!(38, 2))
    End If
    Print #1,
    
    Print #1, "MOST ASSISTS, GAME"
    If indRec!(40, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRec!(40, 0); "- "; indRecDesc$(40, 0); ", "; indRecDesc$(40, 1) + " " + indRecDesc$(40, 2) + " " + indRecDesc$(40, 3) + ", " + indRecDesc$(40, 4)
    End If
    Print #1,
    
    Print #1, "MOST ASSISTS, ONE HALF"
    If indRec!(41, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRec!(41, 0); "- "; indRecDesc$(41, 0); ", "; indRecDesc$(41, 1) + " " + indRecDesc$(41, 2) + " " + indRecDesc$(41, 3) + ", " + indRecDesc$(41, 4); HALF$(indRec!(41, 2))
    End If
    Print #1,
    
    Print #1, "MOST STEALS, GAME"
    If indRec!(43, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRec!(43, 0); "- "; indRecDesc$(43, 0); ", "; indRecDesc$(43, 1) + " " + indRecDesc$(43, 2) + " " + indRecDesc$(43, 3) + ", " + indRecDesc$(43, 4)
    End If
    Print #1,
    
    Print #1, "MOST STEALS, ONE HALF"
    If indRec!(44, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRec!(44, 0); "- "; indRecDesc$(44, 0); ", "; indRecDesc$(44, 1) + " " + indRecDesc$(44, 2) + " " + indRecDesc$(44, 3) + ", " + indRecDesc$(44, 4); HALF$(indRec!(44, 2))
    End If
    Print #1,
    
    Print #1, "MOST BLOCKS, GAME"
    If indRec!(46, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRec!(46, 0); "- "; indRecDesc$(46, 0); ", "; indRecDesc$(46, 1) + " " + indRecDesc$(46, 2) + " " + indRecDesc$(46, 3) + ", " + indRecDesc$(46, 4)
    End If
    Print #1,
    
    Print #1, "MOST BLOCKS, ONE HALF"
    If indRec!(47, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRec!(47, 0); "- "; indRecDesc$(47, 0); ", "; indRecDesc$(47, 1) + " " + indRecDesc$(47, 2) + " " + indRecDesc$(47, 3) + ", " + indRecDesc$(47, 4); HALF$(indRec!(47, 2))
    End If
    Print #1,
    
    Print #1, Chr$(12)
    Print #1, "                            TEAM RECORDS"
    Print #1,

    Print #1, "MOST OVERTIME PERIODS"
    If teamRec!(0, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, teamRec!(0, 0); "- "; teamRecDesc$(0, 1);
        Print #1, Using " (###) AT "; teamRec!(0, 1);
        Print #1, teamRecDesc$(0, 2);: Print #1, Using " (###)"; teamRec!(0, 2);: Print #1, ", " + teamRecDesc$(0, 3)
    End If
    Print #1,

    Print #1, "MOST POINTS, GAME"
    Print #1, teamRec!(1, 0); "- "; teamRecDesc$(1, 0) + " " + teamRecDesc$(1, 1) + " " + teamRecDesc$(1, 2) + ", " + teamRecDesc$(1, 3)
    Print #1,

    Print #1, "FEWEST POINTS, GAME"
    Print #1, teamRec!(2, 0); "- "; teamRecDesc$(2, 0) + " " + teamRecDesc$(2, 1) + " " + teamRecDesc$(2, 2) + ", " + teamRecDesc$(2, 3)
    Print #1,

    Print #1, "MOST POINTS, BOTH TEAMS, GAME"
    Print #1, teamRec!(3, 0); "- "; teamRecDesc$(3, 0);
    Print #1, Using " (###) "; teamRec!(3, 1);: Print #1, teamRecDesc$(3, 1) + " ";
    Print #1, teamRecDesc$(3, 2);: Print #1, Using " (###)"; teamRec!(3, 2);: Print #1, ", " + teamRecDesc$(3, 3)
    Print #1,

    Print #1, "FEWEST POINTS, BOTH TEAMS, GAME"
    Print #1, teamRec!(4, 0); "- "; teamRecDesc$(4, 0);
    Print #1, Using " (###) "; teamRec!(4, 1);: Print #1, teamRecDesc$(4, 1) + " ";
    Print #1, teamRecDesc$(4, 2);: Print #1, Using " (###)"; teamRec!(4, 2);: Print #1, ", " + teamRecDesc$(4, 3)
    Print #1,

    Print #1, "MOST POINTS, FIRST HALF"
    Print #1, teamRec!(6, 0); "- "; teamRecDesc$(6, 0) + " " + teamRecDesc$(6, 1) + " " + teamRecDesc$(6, 2) + ", " + teamRecDesc$(6, 3)
    Print #1,

    Print #1, "FEWEST POINTS, FIRST HALF"
    Print #1, teamRec!(7, 0); "- "; teamRecDesc$(7, 0) + " " + teamRecDesc$(7, 1) + " " + teamRecDesc$(7, 2) + ", " + teamRecDesc$(7, 3)
    Print #1,
    Print #1,

    Print #1, "MOST POINTS, BOTH TEAMS, FIRST HALF"
    Print #1, teamRec!(8, 0); "- "; teamRecDesc$(8, 0);
    Print #1, Using " (###) "; teamRec!(8, 1);: Print #1, teamRecDesc$(8, 1) + " ";
    Print #1, teamRecDesc$(8, 2);: Print #1, Using " (###)"; teamRec!(8, 2);: Print #1, ", " + teamRecDesc$(8, 3)
    Print #1,

    Print #1, "FEWEST POINTS, BOTH TEAMS, FIRST HALF"
    Print #1, teamRec!(9, 0); "- "; teamRecDesc$(9, 0);
    Print #1, Using " (###) "; teamRec!(9, 1);: Print #1, teamRecDesc$(9, 1) + " ";
    Print #1, teamRecDesc$(9, 2);: Print #1, Using " (###)"; teamRec!(9, 2);: Print #1, ", " + teamRecDesc$(9, 3)
    Print #1,

    Print #1, "MOST POINTS, SECOND HALF"
    Print #1, teamRec!(10, 0); "- "; teamRecDesc$(10, 0) + " " + teamRecDesc$(10, 1) + " " + teamRecDesc$(10, 2) + ", " + teamRecDesc$(10, 3)
    Print #1,

    Print #1, "FEWEST POINTS, SECOND HALF"
    Print #1, teamRec!(11, 0); "- "; teamRecDesc$(11, 0) + " " + teamRecDesc$(11, 1) + " " + teamRecDesc$(11, 2) + ", " + teamRecDesc$(11, 3)
    Print #1,

    Print #1, "MOST POINTS, BOTH TEAMS, SECOND HALF"
    Print #1, teamRec!(12, 0); "- "; teamRecDesc$(12, 0);
    Print #1, Using " (###) "; teamRec!(12, 1);: Print #1, teamRecDesc$(12, 1) + " ";
    Print #1, teamRecDesc$(12, 2);: Print #1, Using " (###)"; teamRec!(12, 2);: Print #1, ", " + teamRecDesc$(12, 3)
    Print #1,

    Print #1, "FEWEST POINTS, BOTH TEAMS, SECOND HALF"
    Print #1, teamRec!(13, 0); "- "; teamRecDesc$(13, 0);
    Print #1, Using " (###) "; teamRec!(13, 1);: Print #1, teamRecDesc$(13, 1) + " ";
    Print #1, teamRecDesc$(13, 2);: Print #1, Using " (###)"; teamRec!(13, 2);: Print #1, ", " + teamRecDesc$(13, 3)
    Print #1,

    Print #1, "MOST POINTS, OVERTIME"
    If teamRec!(14, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, teamRec!(14, 0); "- "; teamRecDesc$(14, 0) + " " + teamRecDesc$(14, 1) + " " + teamRecDesc$(14, 2) + ", " + teamRecDesc$(14, 3)
    End If
    Print #1,

    Print #1, "FEWEST POINTS, OVERTIME"
    If teamRec!(15, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, teamRec!(15, 0); "- "; teamRecDesc$(15, 0) + " " + teamRecDesc$(15, 1) + " " + teamRecDesc$(15, 2) + ", " + teamRecDesc$(15, 3)
    End If
    Print #1,

    Print #1, "MOST POINTS, BOTH TEAMS, OVERTIME"
    If teamRec!(16, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, teamRec!(16, 0); "- "; teamRecDesc$(16, 0);
        Print #1, Using " (###) "; teamRec!(16, 1);: Print #1, teamRecDesc$(16, 1) + " ";
        Print #1, teamRecDesc$(16, 2);: Print #1, Using " (###)"; teamRec!(16, 2);: Print #1, ", " + teamRecDesc$(16, 3)
    End If
    Print #1,

    Print #1, "FEWEST POINTS, BOTH TEAMS, OVERTIME"
    If teamRec!(17, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, teamRec!(17, 0); "- "; teamRecDesc$(17, 0);
        Print #1, Using " (###) "; teamRec!(17, 1);: Print #1, teamRecDesc$(17, 1) + " ";
        Print #1, teamRecDesc$(17, 2);: Print #1, Using " (###)"; teamRec!(17, 2);: Print #1, ", " + teamRecDesc$(17, 3)
    End If
    Print #1,

    Print #1, "HIGHEST FIELD GOAL PERCENTAGE, GAME"
    Print #1, Using ".### "; teamRec!(30, 0);: Print #1, "- "; teamRecDesc$(30, 0) + " " + teamRecDesc$(30, 1) + " " + teamRecDesc$(30, 2) + ", " + teamRecDesc$(30, 3);: Print #1, Using "###/###"; teamRec!(30, 1); teamRec!(30, 2)
    Print #1,

    Print #1, "LOWEST FIELD GOAL PERCENTAGE, GAME"
    Print #1, Using ".### "; teamRec!(31, 0);: Print #1, "- "; teamRecDesc$(31, 0) + " " + teamRecDesc$(31, 1) + " " + teamRecDesc$(31, 2) + ", " + teamRecDesc$(31, 3);: Print #1, Using "###/###"; teamRec!(31, 1); teamRec!(31, 2)
    Print #1,

    Print #1, "HIGHEST FIELD GOAL PERCENTAGE, BOTH TEAMS, GAME"
    Print #1, Using ".### "; teamRec!(32, 0);: Print #1, "- "; teamRecDesc$(32, 0);
    Print #1, Using " (.###) "; teamRec!(32, 1);: Print #1, teamRecDesc$(32, 1) + " ";
    Print #1, teamRecDesc$(32, 2);: Print #1, Using " (.###)"; teamRec!(32, 2);: Print #1, ", " + teamRecDesc$(32, 3)
    Print #1,

    Print #1, "LOWEST FIELD GOAL PERCENTAGE, BOTH TEAMS, GAME"
    Print #1, Using ".### "; teamRec!(33, 0);: Print #1, "- "; teamRecDesc$(33, 0);
    Print #1, Using " (.###) "; teamRec!(33, 1);: Print #1, teamRecDesc$(33, 1) + " ";
    Print #1, teamRecDesc$(33, 2);: Print #1, Using " (.###)"; teamRec!(33, 2);: Print #1, ", " + teamRecDesc$(33, 3)
    Print #1,

    Print #1, "MOST FIELD GOALS, GAME"
    Print #1, teamRec!(34, 0); "- "; teamRecDesc$(34, 0) + " " + teamRecDesc$(34, 1) + " " + teamRecDesc$(34, 2) + ", " + teamRecDesc$(34, 3)
    Print #1,

    Print #1, "FEWEST FIELD GOALS, GAME"
    Print #1, teamRec!(35, 0); "- "; teamRecDesc$(35, 0) + " " + teamRecDesc$(35, 1) + " " + teamRecDesc$(35, 2) + ", " + teamRecDesc$(35, 3)
    Print #1,

    Print #1, "MOST FIELD GOALS, BOTH TEAMS, GAME"
    Print #1, teamRec!(36, 0); "- "; teamRecDesc$(36, 0);
    Print #1, Using " (###) "; teamRec!(36, 1);: Print #1, teamRecDesc$(36, 1) + " ";
    Print #1, teamRecDesc$(36, 2);: Print #1, Using " (###)"; teamRec!(36, 2);: Print #1, ", " + teamRecDesc$(36, 3)
    Print #1,

    Print #1, "FEWEST FIELD GOALS, BOTH TEAMS, GAME"
    Print #1, teamRec!(37, 0); "- "; teamRecDesc$(37, 0);
    Print #1, Using " (###) "; teamRec!(37, 1);: Print #1, teamRecDesc$(37, 1) + " ";
    Print #1, teamRecDesc$(37, 2);: Print #1, Using " (###)"; teamRec!(37, 2);: Print #1, ", " + teamRecDesc$(37, 3)
    Print #1,

    Print #1, "MOST FIELD GOALS, ONE HALF"
    Print #1, teamRec!(38, 0); "- "; teamRecDesc$(38, 0) + " " + teamRecDesc$(38, 1) + " " + teamRecDesc$(38, 2) + ", " + teamRecDesc$(38, 3); HALF$(teamRec!(38, 1))
    Print #1,

    Print #1, "MOST FIELD GOALS, BOTH TEAMS, ONE HALF"
    Print #1, teamRec!(39, 0); "- "; teamRecDesc$(39, 0); " "; teamRecDesc$(39, 1); " "; teamRecDesc$(39, 2); ", "; teamRecDesc$(39, 3); HALF$(teamRec!(39, 1))
    Print #1,

    Print #1, "MOST FIELD GOAL ATTEMPTS, GAME"
    Print #1, teamRec!(42, 0); "- "; teamRecDesc$(42, 0) + " " + teamRecDesc$(42, 1) + " " + teamRecDesc$(42, 2) + ", " + teamRecDesc$(42, 3)
    Print #1,

    Print #1, "FEWEST FIELD GOAL ATTEMPTS, GAME"
    Print #1, teamRec!(43, 0); "- "; teamRecDesc$(43, 0) + " " + teamRecDesc$(43, 1) + " " + teamRecDesc$(43, 2) + ", " + teamRecDesc$(43, 3)
    Print #1,

    Print #1, "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
    Print #1, teamRec!(44, 0); "- "; teamRecDesc$(44, 0);
    Print #1, Using " (###) "; teamRec!(44, 1);: Print #1, teamRecDesc$(44, 1) + " ";
    Print #1, teamRecDesc$(44, 2);: Print #1, Using " (###)"; teamRec!(44, 2);: Print #1, ", " + teamRecDesc$(44, 3)
    Print #1,

    Print #1, "FEWEST FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
    Print #1, teamRec!(45, 0); "- "; teamRecDesc$(45, 0);
    Print #1, Using " (###) "; teamRec!(45, 1);: Print #1, teamRecDesc$(45, 1) + " ";
    Print #1, teamRecDesc$(45, 2);: Print #1, Using " (###)"; teamRec!(45, 2);: Print #1, ", " + teamRecDesc$(45, 3)
    Print #1,

    Print #1, "MOST FIELD GOAL ATTEMPTS, ONE HALF"
    Print #1, teamRec!(46, 0); "- "; teamRecDesc$(46, 0) + " " + teamRecDesc$(46, 1) + " " + teamRecDesc$(46, 2) + ", " + teamRecDesc$(46, 3); HALF$(teamRec!(46, 1))
    Print #1,

    Print #1, "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, ONE HALF"
    Print #1, teamRec!(47, 0); "- "; teamRecDesc$(47, 0); " "; teamRecDesc$(47, 1); " "; teamRecDesc$(47, 2); ", "; teamRecDesc$(47, 3); HALF$(teamRec!(47, 1))
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOALS, NO MISSES, GAME"
    If teamRec!(50, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, teamRec!(50, 0); "- "; teamRecDesc$(50, 0) + " " + teamRecDesc$(50, 1) + " " + teamRecDesc$(50, 2) + ", " + teamRecDesc$(50, 3)
    End If
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOALS, NO MISSES, BOTH TEAMS, GAME"
    If teamRec!(51, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, teamRec!(51, 0); "- "; teamRecDesc$(51, 0);
        Print #1, Using " (###) "; teamRec!(51, 1);: Print #1, teamRecDesc$(51, 1) + " ";
        Print #1, teamRecDesc$(51, 2);: Print #1, Using " (###)"; teamRec!(51, 2);: Print #1, ", " + teamRecDesc$(51, 3)
    End If
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOAL ATTEMPTS, NONE MADE, GAME"
    If teamRec!(52, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, teamRec!(52, 0); "- "; teamRecDesc$(52, 0) + " " + teamRecDesc$(52, 1) + " " + teamRecDesc$(52, 2) + ", " + teamRecDesc$(52, 3)
    End If
    Print #1,
 
    Print #1, "MOST THREE-POINT FIELD GOALS, GAME"
    Print #1, teamRec!(53, 0); "- "; teamRecDesc$(53, 0) + " " + teamRecDesc$(53, 1) + " " + teamRecDesc$(53, 2) + ", " + teamRecDesc$(53, 3)
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOALS, BOTH TEAMS, GAME"
    Print #1, teamRec!(54, 0); "- "; teamRecDesc$(54, 0);
    Print #1, Using " (###) "; teamRec!(54, 1);: Print #1, teamRecDesc$(54, 1) + " ";
    Print #1, teamRecDesc$(54, 2);: Print #1, Using " (###)"; teamRec!(54, 2);: Print #1, ", " + teamRecDesc$(54, 3)
    Print #1,
    
    Print #1, "MOST THREE-POINT FIELD GOALS, ONE HALF"
    Print #1, teamRec!(55, 0); "- "; teamRecDesc$(55, 0) + " " + teamRecDesc$(55, 1) + " " + teamRecDesc$(55, 2) + ", " + teamRecDesc$(55, 3); HALF$(teamRec!(55, 1))
    Print #1,
    
    Print #1, "MOST THREE-POINT FIELD GOAL ATTEMPTS, GAME"
    Print #1, teamRec!(57, 0); "- "; teamRecDesc$(57, 0) + " " + teamRecDesc$(57, 1) + " " + teamRecDesc$(57, 2) + ", " + teamRecDesc$(57, 3)
    Print #1,
    
    Print #1, "MOST THREE-POINT FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
    Print #1, teamRec!(58, 0); "- "; teamRecDesc$(58, 0);
    Print #1, Using " (###) "; teamRec!(58, 1);: Print #1, teamRecDesc$(58, 1) + " ";
    Print #1, teamRecDesc$(58, 2);: Print #1, Using " (###)"; teamRec!(58, 2);: Print #1, ", " + teamRecDesc$(58, 3)
    Print #1,
    
    Print #1, "MOST THREE-POINT FIELD GOAL ATTEMPTS, ONE HALF"
    Print #1, teamRec!(59, 0); "- "; teamRecDesc$(59, 0) + " " + teamRecDesc$(59, 1) + " " + teamRecDesc$(59, 2) + ", " + teamRecDesc$(59, 3); HALF$(teamRec!(59, 1))
    Print #1,

    Print #1, "MOST FREE THROWS MADE, NO MISSES, GAME"
    If teamRec!(60, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, teamRec!(60, 0); "- "; teamRecDesc$(60, 0) + " " + teamRecDesc$(60, 1) + " " + teamRecDesc$(60, 2) + ", " + teamRecDesc$(60, 3)
    End If
    Print #1,

    Print #1, "LOWEST FREE THROW PERCENTAGE, GAME"
    Print #1, Using ".### "; teamRec!(61, 0);: Print #1, "- "; teamRecDesc$(61, 0) + " " + teamRecDesc$(61, 1) + " " + teamRecDesc$(61, 2) + ", " + teamRecDesc$(61, 3);: Print #1, Using "###/###"; teamRec!(61, 1); teamRec!(61, 2)
    Print #1,

    Print #1, "HIGHEST FREE THROW PERCENTAGE, BOTH TEAMS, GAME"
    Print #1, Using ".### "; teamRec!(62, 0);: Print #1, "- "; teamRecDesc$(62, 0);
    Print #1, Using " (.###) "; teamRec!(62, 1);: Print #1, teamRecDesc$(62, 1) + " ";
    Print #1, teamRecDesc$(62, 2);: Print #1, Using " (.###)"; teamRec!(62, 2);: Print #1, ", " + teamRecDesc$(62, 3)
    Print #1,

    Print #1, "LOWEST FREE THROW PERCENTAGE, BOTH TEAMS, GAME"
    Print #1, Using ".### "; teamRec!(63, 0);: Print #1, "- "; teamRecDesc$(63, 0);
    Print #1, Using " (.###) "; teamRec!(63, 1);: Print #1, teamRecDesc$(63, 1) + " ";
    Print #1, teamRecDesc$(63, 2);: Print #1, Using " (.###)"; teamRec!(63, 2);: Print #1, ", " + teamRecDesc$(63, 3)
    Print #1,

    Print #1, "MOST FREE THROWS MADE, GAME"
    Print #1, teamRec!(64, 0); "- "; teamRecDesc$(64, 0) + " " + teamRecDesc$(64, 1) + " " + teamRecDesc$(64, 2) + ", " + teamRecDesc$(64, 3)
    Print #1,

    Print #1, "FEWEST FREE THROWS MADE, GAME"
    Print #1, teamRec!(65, 0); "- "; teamRecDesc$(65, 0) + " " + teamRecDesc$(65, 1) + " " + teamRecDesc$(65, 2) + ", " + teamRecDesc$(65, 3)
    Print #1,

    Print #1, "MOST FREE THROWS MADE, BOTH TEAMS, GAME"
    Print #1, teamRec!(66, 0); "- "; teamRecDesc$(66, 0);
    Print #1, Using " (###) "; teamRec!(66, 1);: Print #1, teamRecDesc$(66, 1) + " ";
    Print #1, teamRecDesc$(66, 2);: Print #1, Using " (###)"; teamRec!(66, 2);: Print #1, ", " + teamRecDesc$(66, 3)
    Print #1,

    Print #1, "FEWEST FREE THROWS MADE, BOTH TEAMS, GAME"
    Print #1, teamRec!(67, 0); "- "; teamRecDesc$(67, 0);
    Print #1, Using " (###) "; teamRec!(67, 1);: Print #1, teamRecDesc$(67, 1) + " ";
    Print #1, teamRecDesc$(67, 2);: Print #1, Using " (###)"; teamRec!(67, 2);: Print #1, ", " + teamRecDesc$(67, 3)
    Print #1,

    Print #1, "MOST FREE THROWS MADE, ONE HALF"
    Print #1, teamRec!(68, 0); "- "; teamRecDesc$(68, 0) + " " + teamRecDesc$(68, 1) + " " + teamRecDesc$(68, 2) + ", " + teamRecDesc$(68, 3); HALF$(teamRec!(68, 1))
    Print #1,

    Print #1, "MOST FREE THROW ATTEMPTS, GAME"
    Print #1, teamRec!(72, 0); "- "; teamRecDesc$(72, 0) + " " + teamRecDesc$(72, 1) + " " + teamRecDesc$(72, 2) + ", " + teamRecDesc$(72, 3)
    Print #1,

    Print #1, "FEWEST FREE THROW ATTEMPTS, GAME"
    Print #1, teamRec!(73, 0); "- "; teamRecDesc$(73, 0) + " " + teamRecDesc$(73, 1) + " " + teamRecDesc$(73, 2) + ", " + teamRecDesc$(73, 3)
    Print #1,

    Print #1, "MOST FREE THROW ATTEMPTS, BOTH TEAMS, GAME"
    Print #1, teamRec!(74, 0); "- "; teamRecDesc$(74, 0);
    Print #1, Using " (###) "; teamRec!(74, 1);: Print #1, teamRecDesc$(74, 1) + " ";
    Print #1, teamRecDesc$(74, 2);: Print #1, Using " (###)"; teamRec!(74, 2);: Print #1, ", " + teamRecDesc$(74, 3)
    Print #1,

    Print #1, "FEWEST FREE THROW ATTEMPTS, BOTH TEAMS, GAME"
    Print #1, teamRec!(75, 0); "- "; teamRecDesc$(75, 0);
    Print #1, Using " (###) "; teamRec!(75, 1);: Print #1, teamRecDesc$(75, 1) + " ";
    Print #1, teamRecDesc$(75, 2);: Print #1, Using " (###)"; teamRec!(75, 2);: Print #1, ", " + teamRecDesc$(75, 3)
    Print #1,

    Print #1, "MOST FREE THROW ATTEMPTS, ONE HALF"
    Print #1, teamRec!(76, 0); "- "; teamRecDesc$(76, 0) + " " + teamRecDesc$(76, 1) + " " + teamRecDesc$(76, 2) + ", " + teamRecDesc$(76, 3); HALF$(teamRec!(76, 1))
    Print #1,

    Print #1, "MOST FREE THROW ATTEMPTS, BOTH TEAMS, HALF"
    Print #1, teamRec!(77, 0); "- "; teamRecDesc$(77, 0);
    Print #1, Using " (###) "; teamRec!(77, 1);: Print #1, teamRecDesc$(77, 1) + " ";
    Print #1, teamRecDesc$(77, 2);: Print #1, Using " (###)"; teamRec!(77, 2);: Print #1, ", " + teamRecDesc$(77, 3)
    Print #1,

    Print #1, "MOST REBOUNDS, GAME"
    Print #1, teamRec!(80, 0); "- "; teamRecDesc$(80, 0) + " " + teamRecDesc$(80, 1) + " " + teamRecDesc$(80, 2) + ", " + teamRecDesc$(80, 3)
    Print #1,

    Print #1, "FEWEST REBOUNDS, GAME"
    Print #1, teamRec!(81, 0); "- "; teamRecDesc$(81, 0) + " " + teamRecDesc$(81, 1) + " " + teamRecDesc$(81, 2) + ", " + teamRecDesc$(81, 3)
    Print #1,

    Print #1, "MOST REBOUNDS, BOTH TEAMS, GAME"
    Print #1, teamRec!(82, 0); "- "; teamRecDesc$(82, 0);
    Print #1, Using " (###) "; teamRec!(82, 1);: Print #1, teamRecDesc$(82, 1) + " ";
    Print #1, teamRecDesc$(82, 2);: Print #1, Using " (###)"; teamRec!(82, 2);: Print #1, ", " + teamRecDesc$(82, 3)
    Print #1,

    Print #1, "FEWEST REBOUNDS, BOTH TEAMS, GAME"
    Print #1, teamRec!(83, 0); "- "; teamRecDesc$(83, 0);
    Print #1, Using " (###) "; teamRec!(83, 1);: Print #1, teamRecDesc$(83, 1) + " ";
    Print #1, teamRecDesc$(83, 2);: Print #1, Using " (###)"; teamRec!(83, 2);: Print #1, ", " + teamRecDesc$(83, 3)
    Print #1,

    Print #1, "MOST REBOUNDS, ONE HALF"
    Print #1, teamRec!(84, 0); "- "; teamRecDesc$(84, 0) + " " + teamRecDesc$(84, 1) + " " + teamRecDesc$(84, 2) + ", " + teamRecDesc$(84, 3); HALF$(teamRec!(84, 1))
    Print #1,

    Print #1, "MOST OFFENSIVE REBOUNDS, GAME"
    Print #1, teamRec!(86, 0); "- "; teamRecDesc$(86, 0) + " " + teamRecDesc$(86, 1) + " " + teamRecDesc$(86, 2) + ", " + teamRecDesc$(86, 3)
    Print #1,

    Print #1, "FEWEST OFFENSIVE REBOUNDS, GAME"
    Print #1, teamRec!(87, 0); "- "; teamRecDesc$(87, 0) + " " + teamRecDesc$(87, 1) + " " + teamRecDesc$(87, 2) + ", " + teamRecDesc$(87, 3)
    Print #1,

    Print #1, "MOST OFFENSIVE REBOUNDS, BOTH TEAMS, GAME"
    Print #1, teamRec!(88, 0); "- "; teamRecDesc$(88, 0);
    Print #1, Using " (###) "; teamRec!(88, 1);: Print #1, teamRecDesc$(88, 1) + " ";
    Print #1, teamRecDesc$(88, 2);: Print #1, Using " (###)"; teamRec!(88, 2);: Print #1, ", " + teamRecDesc$(88, 3)
    Print #1,

    Print #1, "FEWEST OFFENSIVE REBOUNDS, BOTH TEAMS, GAME"
    Print #1, teamRec!(89, 0); "- "; teamRecDesc$(89, 0);
    Print #1, Using " (###) "; teamRec!(89, 1);: Print #1, teamRecDesc$(89, 1) + " ";
    Print #1, teamRecDesc$(89, 2);: Print #1, Using " (###)"; teamRec!(89, 2);: Print #1, ", " + teamRecDesc$(89, 3)
    Print #1,

    Print #1, "MOST OFFENSIVE REBOUNDS, ONE HALF"
    Print #1, teamRec!(90, 0); "- "; teamRecDesc$(90, 0) + " " + teamRecDesc$(90, 1) + " " + teamRecDesc$(90, 2) + ", " + teamRecDesc$(90, 3); HALF$(teamRec!(90, 1))
    Print #1,

    Print #1, "MOST ASSISTS, GAME"
    Print #1, teamRec!(96, 0); "- "; teamRecDesc$(96, 0) + " " + teamRecDesc$(96, 1) + " " + teamRecDesc$(96, 2) + ", " + teamRecDesc$(96, 3)
    Print #1,

    Print #1, "FEWEST ASSISTS, GAME"
    Print #1, teamRec!(97, 0); "- "; teamRecDesc$(97, 0) + " " + teamRecDesc$(97, 1) + " " + teamRecDesc$(97, 2) + ", " + teamRecDesc$(97, 3)
    Print #1,

    Print #1, "MOST ASSISTS, BOTH TEAMS, GAME"
    Print #1, teamRec!(98, 0); "- "; teamRecDesc$(98, 0);
    Print #1, Using " (###) "; teamRec!(98, 1);: Print #1, teamRecDesc$(98, 1) + " ";
    Print #1, teamRecDesc$(98, 2);: Print #1, Using " (###)"; teamRec!(98, 2);: Print #1, ", " + teamRecDesc$(98, 3)
    Print #1,

    Print #1, "FEWEST ASSISTS, BOTH TEAMS, GAME"
    Print #1, teamRec!(99, 0); "- "; teamRecDesc$(99, 0);
    Print #1, Using " (###) "; teamRec!(99, 1);: Print #1, teamRecDesc$(99, 1) + " ";
    Print #1, teamRecDesc$(99, 2);: Print #1, Using " (###)"; teamRec!(99, 2);: Print #1, ", " + teamRecDesc$(99, 3)
    Print #1,

    Print #1, "MOST ASSISTS, ONE HALF"
    Print #1, teamRec!(100, 0); "- "; teamRecDesc$(100, 0) + " " + teamRecDesc$(100, 1) + " " + teamRecDesc$(100, 2) + ", " + teamRecDesc$(100, 3); HALF$(teamRec!(100, 1))
    Print #1,

    Print #1, "MOST PERSONAL FOULS, GAME"
    Print #1, teamRec!(104, 0); "- "; teamRecDesc$(104, 0) + " " + teamRecDesc$(104, 1) + " " + teamRecDesc$(104, 2) + ", " + teamRecDesc$(104, 3)
    Print #1,

    Print #1, "FEWEST PERSONAL FOULS, GAME"
    Print #1, teamRec!(105, 0); "- "; teamRecDesc$(105, 0) + " " + teamRecDesc$(105, 1) + " " + teamRecDesc$(105, 2) + ", " + teamRecDesc$(105, 3)
    Print #1,

    Print #1, "MOST PERSONAL FOULS, BOTH TEAMS, GAME"
    Print #1, teamRec!(106, 0); "- "; teamRecDesc$(106, 0);
    Print #1, Using " (###) "; teamRec!(106, 1);: Print #1, teamRecDesc$(106, 1) + " ";
    Print #1, teamRecDesc$(106, 2);: Print #1, Using " (###)"; teamRec!(106, 2);: Print #1, ", " + teamRecDesc$(106, 3)
    Print #1,

    Print #1, "FEWEST PERSONAL FOULS, BOTH TEAMS, GAME"
    Print #1, teamRec!(107, 0); "- "; teamRecDesc$(107, 0);
    Print #1, Using " (###) "; teamRec!(107, 1);: Print #1, teamRecDesc$(107, 1) + " ";
    Print #1, teamRecDesc$(107, 2);: Print #1, Using " (###)"; teamRec!(107, 2);: Print #1, ", " + teamRecDesc$(107, 3)
    Print #1,

    Print #1, "MOST PERSONAL FOULS, ONE HALF"
    Print #1, teamRec!(108, 0); "- "; teamRecDesc$(108, 0) + " " + teamRecDesc$(108, 1) + " " + teamRecDesc$(108, 2) + ", " + teamRecDesc$(108, 3); HALF$(teamRec!(108, 1))
    Print #1,

    Print #1, "MOST PERSONAL FOULS, BOTH TEAMS, ONE HALF"
    Print #1, teamRec!(109, 0); "- "; teamRecDesc$(109, 0); " "; teamRecDesc$(109, 1); " "; teamRecDesc$(109, 2); ", "; teamRecDesc$(109, 3); HALF$(teamRec!(109, 1))
    Print #1,

    Print #1, "MOST STEALS, GAME"
    Print #1, teamRec!(112, 0); "- "; teamRecDesc$(112, 0) + " " + teamRecDesc$(112, 1) + " " + teamRecDesc$(112, 2) + ", " + teamRecDesc$(112, 3)
    Print #1,

    Print #1, "MOST STEALS, BOTH TEAMS, GAME"
    Print #1, teamRec!(114, 0); "- "; teamRecDesc$(114, 0);
    Print #1, Using " (###) "; teamRec!(114, 1);: Print #1, teamRecDesc$(114, 1) + " ";
    Print #1, teamRecDesc$(114, 2);: Print #1, Using " (###)"; teamRec!(114, 2);: Print #1, ", " + teamRecDesc$(114, 3)
    Print #1,

    Print #1, "MOST STEALS, ONE HALF"
    Print #1, teamRec!(116, 0); "- "; teamRecDesc$(116, 0) + " " + teamRecDesc$(116, 1) + " " + teamRecDesc$(116, 2) + ", " + teamRecDesc$(116, 3); HALF$(teamRec!(116, 1))
    Print #1,

    Print #1, "MOST BLOCKS, GAME"
    Print #1, teamRec!(118, 0); "- "; teamRecDesc$(118, 0) + " " + teamRecDesc$(118, 1) + " " + teamRecDesc$(118, 2) + ", " + teamRecDesc$(118, 3)
    Print #1,

    Print #1, "MOST BLOCKS, BOTH TEAMS, GAME"
    Print #1, teamRec!(119, 0); "- "; teamRecDesc$(119, 0);
    Print #1, Using " (###) "; teamRec!(119, 1);: Print #1, teamRecDesc$(119, 1) + " ";
    Print #1, teamRecDesc$(119, 2);: Print #1, Using " (###)"; teamRec!(119, 2);: Print #1, ", " + teamRecDesc$(119, 3)
    Print #1,

    Print #1, "MOST BLOCKS, HALF"
    Print #1, teamRec!(121, 0); "- "; teamRecDesc$(121, 0) + " " + teamRecDesc$(121, 1) + " " + teamRecDesc$(121, 2) + ", " + teamRecDesc$(121, 3); HALF$(teamRec!(121, 1))
    Print #1,

    Print #1, "MOST TURNOVERS, GAME"
    Print #1, teamRec!(122, 0); "- "; teamRecDesc$(122, 0) + " " + teamRecDesc$(122, 1) + " " + teamRecDesc$(122, 2) + ", " + teamRecDesc$(122, 3)
    Print #1,

    Print #1, "FEWEST TURNOVERS, GAME"
    Print #1, teamRec!(123, 0); "- "; teamRecDesc$(123, 0) + " " + teamRecDesc$(123, 1) + " " + teamRecDesc$(123, 2) + ", " + teamRecDesc$(123, 3)
    Print #1,

    Print #1, "MOST TURNOVERS, BOTH TEAMS, GAME"
    Print #1, teamRec!(124, 0); "- "; teamRecDesc$(124, 0);
    Print #1, Using " (###) "; teamRec!(124, 1);: Print #1, teamRecDesc$(124, 1) + " ";
    Print #1, teamRecDesc$(124, 2);: Print #1, Using " (###)"; teamRec!(124, 2);: Print #1, ", " + teamRecDesc$(124, 3)
    Print #1,

    Print #1, "FEWEST TURNOVERS, BOTH TEAMS, GAME"
    Print #1, teamRec!(125, 0); "- "; teamRecDesc$(125, 0);
    Print #1, Using " (###) "; teamRec!(125, 1);: Print #1, teamRecDesc$(125, 1) + " ";
    Print #1, teamRecDesc$(125, 2);: Print #1, Using " (###)"; teamRec!(125, 2);: Print #1, ", " + teamRecDesc$(125, 3)

    Close #1

    Open "RECORDS" For Input As #2
    
    Do While Not EOF(2)
        Line Input #2, X$
        LPrint X$
    Loop

    LPrint Chr$(12)
    
    Close #2

END SUB
