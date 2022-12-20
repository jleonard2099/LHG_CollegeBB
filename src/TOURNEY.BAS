Sub TOURNEY ()

    _Title "Courtside College Basketball - Tournament"

    Shared homeTeam$(), visitingTeam$(), yearNumber$()
    Shared homeScore%(), NG%(), visitingScore%()
    Shared scheduleAP%()

    Dim NGG(MAX_TEAMS)

    Dim DY%(MAX_TEAMS), MON%(MAX_TEAMS)
    Dim yr%(MAX_TEAMS)

    Dim CM$(1), LC$(2), SC$(2)
    Dim TE$(1), TF$(1), TS$(1), U5$(3)

    CM$(0) = "COACH AND SELECT SHOTS           "
    CM$(1) = "COACH ONLY AND LET COMPUTER SHOOT"
    LC$(1) = "HOME COURT ADVANTAGE"
    LC$(2) = "NEUTRAL SITE        "
    SC$(0) = "NO SHOT CLOCK"
    SC$(1) = "45 SEC CLOCK "
    SC$(2) = "35 SEC CLOCK "
    TE$(0) = "SHOOT 1+1"
    TE$(1) = "SHOOT 2  "
    TF$(0) = "N": TF$(1) = "Y"
    TS$(0) = "N"
    TS$(1) = "Y"
    U5$(0) = "HUMAN OPPONENT            "
    U5$(1) = "COMPUTER IS VISITING TEAM "
    U5$(2) = "COMPUTER IS HOME TEAM     "
    U5$(3) = "COMPUTER VS COMPUTER      "
    yearNumber$(0, 3) = "10-01-91"
    yearNumber$(0, 2) = "07:30P"

    10 Spec$ = diskPaths$(0) + "*.TOU"
    null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
    Count% = DirCount%

    If Count% = 0 Then
        'This was added to handle if no TOU files are available
        'Original game would just try to push ahead anyway
        Locate 10
        Call CSMsgBox("NO TOUNAMENT FILES WERE FOUND PRESS ANY KEY TO RETURN TO MENU", 28, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)
    Else
        ReDim tournamentFiles$(0 To Count%)

        For X = 1 To Count%
            tournamentFiles$(X) = Space$(12)
        Next

        tournamentFiles$(0) = Spec$
        Call ReadFile(tournamentFiles$())

        'nf% = Count%

        'WE have to make all this a sub to keep the logic cleaner
        Color 7, 0
        Cls
        NG%(0, 8) = 0
        NG%(0, 3) = 1
        NG%(0, 7) = 0
        NG%(0, 2) = 1
        NG%(0, 5) = 5
        yearNumber$(0, 0) = ""
        yearNumber$(0, 1) = ""

        For I = 1 To 500
            visitingTeam$(I) = "": NG%(I, 13) = 0
            homeTeam$(I) = ""
        Next

        600 ' *** LOAD IN A SAVED SCHEDULE ***

        Do

            2100 Cls
            Color , 1
            Cls
            Locate , 28
            Color 14
            Print "SELECT TOURNAMENT TO PLAY": Color 15
            Selection% = 1
            Start% = 1
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
            Print "-SELECTS TOURNAMENT";

            _MouseShow "DEFAULT"
            MAMenu tournamentFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
            _MouseHide

            If ExitCode% <> 27 Then

                Color 15, 1
                targetFile$ = tournamentFiles$(Selection%)
                T1 = Selection%

                Locate 2, 33: Print targetFile$
				
				Do
					Locate , 33: Print "ANY CHANGE (YN)";
					I$ = GetKeyPress$
				Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            End If
			
			Color 15, 0

        Loop Until UCase$(I$) = "N"

        If ExitCode% <> 27 Then

            targetFile$ = RTrim$(targetFile$)
            WE$ = Left$(targetFile$, Len(targetFile$) - 4)
            targetFile$ = WE$

            Open diskPaths$(0) + targetFile$ + ".TOU" For Random As #1 Len = 90
            Y = LOF(1)
            Close 1

            scheduleAP%(0) = Y / 90
            BS% = Int((scheduleAP%(0) - 1) / 20)

            For I = 1 To scheduleAP%(0)
                Open diskPaths$(0) + targetFile$ + ".TOU" For Random As #1 Len = 90

                For X = 0 To 14
                    Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
                Next

                Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
                Field #1, 64 As X$, 3 As Q$(20), 3 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
                Field #1, 84 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)
                Get #1, I

                For X = 0 To 14: NG%(I, X) = CVI(Q$(X + 1)): Next
                For X = 0 To 3: yearNumber$(I, X) = Q$(20 + X): Next

                visitingScore%(I) = CVI(Q$(16)): homeScore%(I) = CVI(Q$(17))
                visitingTeam$(I) = Q$(18): homeTeam$(I) = Q$(19)

                For X = 1 To 3: NG%(I, X + 14) = CVI(Q$(X + 23)): Next
                For X = 2 To 3
                    yearNumber$(I, X) = RTrim$(yearNumber$(I, X))
                Next

                Close 1

            Next

            Do

                Cls
                Locate 23, 1: Color , 1: Print Tab(80);: Locate 23, 1
                Color 13: Print "SC";: Color 14: Print "-SHOT CLOCK  ";
                Color 13: Print "HC";: Color 14: Print "-HOME COURT ADVANTAGE  ";
                Color 13: Print "3";: Color 14: Print "-USE 3-POINT SHOT  ";
                Locate 24, 1: Color , 1: Print Tab(80);: Locate 24, 1
                Color 11: Print "PGUP  PGDN  ";
                Color 13: Print "REG";: Color 14: Print "-REGION  ";
                Color 13: Print "RND";: Color 14: Print "-ROUND  ";
                Color 13: Print "GM";: Color 14: Print "-GAME  ";
                Locate 25, 1: Color , 1: Print Tab(80);: Locate 25, 1
                Color 11: Print "P";: Color 14: Print "-PLAY TOURNAMENT   ";
                Color 11: Print "Q";: Color 14: Print "-QUIT  ";
                Color 11: Print "E";: Color 14: Print "-EDIT A GAME  ";
                Locate 1, 1: Print Tab(80);: Locate 1, 1: Print targetFile$
                Locate 1, 33: Color 14: Print "AUTO SCHEDULER"
                Color , 0

                Locate 2, 1
                Color 14
                Print "GM ID#  REG  RND  GM    VISITING TEAM        HOME TEAM            HC  3  SC": Color 15
                For X = 1 To 20
                    Locate X + 2, 1
                    If X + NS% * 20 > scheduleAP%(0) Then Exit For
                    Print Using "###      ##   ##   ##"; X + NS% * 20; NG%(X + NS% * 20, 15); NG%(X + NS% * 20, 16); NG%(X + NS% * 20, 17);
                    'LOCATE , 17: PRINT yearNumber$(X + NS% * 20, 2);
                    Locate , 25: Print visitingTeam$(X + NS% * 20); " ";
                    If NG%(X + NS% * 20, 13) = 1 Then Print Using "###"; visitingScore%(X + NS% * 20);
                    Locate , 46: Print homeTeam$(X + NS% * 20); " ";
                    If NG%(X + NS% * 20, 13) = 1 Then Print Using "###"; homeScore%(X + NS% * 20);
                    Locate , 67: If NG%(X + NS% * 20, 2) = 1 Then Print "Y";: Else Print "N";
                    Locate , 71: Print TS$(NG%(X + NS% * 20, 3));
                    Locate , 74: Print Left$(SC$(NG%(X + NS% * 20, 7)), 2)
                Next

                I$ = GetKeyPress$

                Select Case UCase$(I$)
                    Case Chr$(0) + "Q":
                        130 ' *** PAGE DOWN ***
                        If NS% >= BS% Then
                            NS% = BS%
                        Else
                            NS% = NS% + 1
                        End If

                    Case Chr$(0) + "I":
                        135 ' *** PAGE UP ***
                        If Not (NS% = 0) Then
                            NS% = NS% - 1
                        End If

                    Case "E":
                        Call EditGame_TOURNEY

                    Case "P":
                        350 ' *** SAVE SCHEDULE FILE ***
                        '    IF _FILEEXISTS(diskPaths$(0) + targetFile$ + ".TNY") THEN
                        '       KILL diskPaths$(0) + targetFile$ + ".TNY"
                        '    END IF

                        Open diskPaths$(0) + targetFile$ + ".TOU" For Random As #1 Len = 90

                        For X = 0 To 14
                            Field #1, X * 2 As X$, 2 As Q$(X + 1), 90 - 2 - 2 * X As X$
                        Next

                        Field #1, 30 As X$, 2 As Q$(16), 2 As Q$(17), 15 As Q$(18), 15 As Q$(19), 26 As X$
                        Field #1, 64 As X$, 3 As Q$(20), 3 As Q$(21), 6 As Q$(22), 8 As Q$(23), 6 As X$
                        Field #1, 84 As X$, 2 As Q$(24), 2 As Q$(25), 2 As Q$(26)

                        For I = 1 To scheduleAP%(0)
                            For X = 0 To 14: LSet Q$(X + 1) = MKI$(NG%(I, X)): Next
                            For X = 0 To 3: LSet Q$(20 + X) = yearNumber$(I, X): Next
                            LSet Q$(16) = MKI$(visitingScore%(I)): LSet Q$(17) = MKI$(homeScore%(I))
                            LSet Q$(18) = visitingTeam$(I): LSet Q$(19) = homeTeam$(I)
                            For X = 1 To 3: LSet Q$(X + 23) = MKI$(NG%(I, X + 14)): Next
                            Put #1, I
                        Next

                        Close 1

						' *** PLAY GAMES ***
						Color , 0: Cls
						JJ = 7
						Locate 6, 8: Color 15, 4
						Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
						Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
						Locate 7 + JJ, 8
						Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

                        Do						
							Locate 7, 32
                            Locate , 12: Print "TOTAL GAMES ="; scheduleAP%(0)
                            Locate , 12: Print "SELECT GAME # TO START AND END WITH": Print
                            Locate , 12: Input "ENTER START GAME #: "; SG%: Print
                            Locate , 12: Input "ENTER END GAME #:"; EG%
                            Locate , 12: Print "ANY CHANGE (YN)"
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "N"

                        If SG% <= 0 Then SG% = 1
                        If EG% > scheduleAP%(0) Then EG% = scheduleAP%(0)
                        Open "TOURNEY" For Output As #2
                        Print #2, targetFile$ ' + ".TNY"
                        Print #2, SG%
                        Print #2, EG%
                        Print #2, 0
                        Close 2

                        For I = 1 To scheduleAP%(0)
                            L = Len(yearNumber$(I, 2))
                            If L = 5 Then HR = Val(Left$(yearNumber$(I, 2), 1)): MIN = Val(Mid$(yearNumber$(I, 2), 3, 2))
                            If L = 6 Then HR = Val(Left$(yearNumber$(I, 2), 2)): MIN = Val(Mid$(yearNumber$(I, 2), 4, 2))
                            MON%(I) = Val(Left$(yearNumber$(I, 3), 2))
                            DY%(I) = Val(Mid$(yearNumber$(I, 3), 4, 2))
                            yr%(I) = Val(Mid$(yearNumber$(I, 3), 7, 2))
                            T$ = Right$(yearNumber$(I, 2), 1)
                            If UCase$(T$) = "P" And Left$(yearNumber$(I, 2), 2) <> "12" Then HR = HR + 12
                            TM = HR * 60 + MIN
                            NGG(I) = Int(TM / 30 - 18): NG%(I, 12) = NGG(I)
                        Next

                        Open "TICKER" For Output As #2
                        For I = 1 To 32: For I1 = 0 To 1: For I2 = 1 To 14
                        Print #2, -1: Next: Next: Next
                        For I = 1 To 32: For I1 = 0 To 1: Print #2, "XXX"
                        Next: Next
                        For I = SG% To SG% + 31
                            Print #2, NG%(I, 12)
                        Next
                        Close 2

                        Run "COLHOOP.EXE"
                    Case "Q":
                        'Exit Sub
                End Select

            Loop Until UCase$(I$) = "Q" Or Count% = 0

        End If 'Done checking for ESC

    End If 'Done checking for files

End Sub

'----------------------------------------
'       EditGame_TOURNEY Subroutine
'----------------------------------------
'...explanation...
Sub EditGame_TOURNEY ()

    Shared homeTeam$(), visitingTeam$(), yearNumber$()
    Shared NG%(), scheduleAP%()

    Dim CM$(1), LC$(2), SC$(2)
    Dim TE$(1), TF$(1), TS$(1), U5$(3)

    Dim U6!

    CM$(0) = "COACH AND SELECT SHOTS           "
    CM$(1) = "COACH ONLY AND LET COMPUTER SHOOT"
    LC$(1) = "HOME COURT ADVANTAGE"
    LC$(2) = "NEUTRAL SITE        "
    SC$(0) = "NO SHOT CLOCK"
    SC$(1) = "45 SEC CLOCK "
    SC$(2) = "35 SEC CLOCK "
    TE$(0) = "SHOOT 1+1"
    TE$(1) = "SHOOT 2  "
    TF$(0) = "N": TF$(1) = "Y"
    TS$(0) = "N"
    TS$(1) = "Y"
    U5$(0) = "HUMAN OPPONENT            "
    U5$(1) = "COMPUTER IS VISITING TEAM "
    U5$(2) = "COMPUTER IS HOME TEAM     "
    U5$(3) = "COMPUTER VS COMPUTER      "
    yearNumber$(0, 3) = "10-01-91"
    yearNumber$(0, 2) = "07:30P"

    Do
        140 ' *** EDIT A GAME ***
        Locate 23, 1: Print "ENTER GAME # TO EDIT        "
        Locate 23, 1: Input "ENTER GAME # TO EDIT "; NG%
        'If NG% < 1 Or NG% > scheduleAP%(0) Then 140
    Loop Until NG% >= 1 And NG% <= scheduleAP%(0)

    V$ = visitingTeam$(NG%)
    H$ = homeTeam$(NG%)

    Do

        149 Cls: Color 2: Print Tab(29); "SELECT GAME OPTIONS": Print
        151 Locate 2, 2: Color 14: Print "    ";: Color 15: Print "VISITING TEAM: "; V$
        152 Locate 3, 2: Color 14: Print "    ";: Color 15: Print "HOME TEAM: "; H$
        153 Locate 4, 2: Color 14: Print "(1) ";: Color 15: Print U5$(NG%(NG%, 8))
        154 Locate 5, 2: Color 14: Print "(2) ";: Color 15: Print "3-POINT SHOT: "; TS$(NG%(NG%, 3))
        155 Locate 6, 2: Color 14: Print "(3) ";: Color 15: Print "SHOT CLOCK "; SC$(NG%(NG%, 7))
        156 Locate 7, 2: Color 14: Print "(4) ";: Color 15: Print "HOME COURT ADVANTAGE: "; LC$(NG%(NG%, 2))
        157 Locate 8, 2: Color 14: Print "(5) ";: Color 15: Print Using "DISPLAY PAUSE IN SECONDS: ##.#  "; NG%(NG%, 4) / 10
        158 Locate 9, 2: Color 14: Print "(6) ";: Color 15: Print "FOULS TO DISQUALIFY: "; NG%(NG%, 5)
        159 Locate 10, 2: Color 14: Print "(7) ";: Color 15: Print "HUMAN COACHES WILL: "; CM$(NG%(NG%, 9))
        162 Locate 11, 2: Color 14: Print "(8) ";: Color 15: Print "FREE THROWS ON 10TH FOUL "; TE$(NG%(NG%, 6))
        164 Locate 12, 2: Color 14: Print "(9) ";: Color 15: Print "3-FT WHEN FOULED ON 3-PT ATTEMPT "; TF$(NG%(NG%, 10))
        165 Locate 13, 2: Color 14: Print "(A) ";: Color 15: Print "1 FT ON 1ST 6 FLOOR FOULS (PRIOR TO 1973): "; TF$(NG%(NG%, 14))
        163 Locate 14, 2: Color 14: Print "(0) ";: Color 15: Print "ACCEPT SETTINGS"

        Do
            175 I$ = GetKeyPress$
            If UCase$(I$) = "A" Then I$ = "10"
            I8% = Val(I$)
            'If I8% < 0 Or I8% > 10 Then 175
        Loop Until I8% >= 0 And I8% <= 10

        Select Case I8%
            Case 1:
                180 NG%(NG%, 8) = NG%(NG%, 8) + 1
                If NG%(NG%, 8) > 3 Then NG%(NG%, 8) = 0
                GoTo 151

            Case 2:
                184 NG%(NG%, 3) = 1 - NG%(NG%, 3)
                GoTo 154

            Case 3:
                185 NG%(NG%, 7) = 1 + NG%(NG%, 7)
                If NG%(NG%, 7) > 2 Then NG%(NG%, 7) = 0
                GoTo 155

            Case 4:
                186 N = 1 - (NG%(NG%, 2) - 1)
                NG%(NG%, 2) = N + 1
                GoTo 156

            Case 5:
                187 Locate 8, 32: Input U6!
                NG%(NG%, 4) = U6! * 10
                GoTo 157

            Case 6:
                188 NG%(NG%, 5) = 1 + NG%(NG%, 5)
                If NG%(NG%, 5) > 6 Then NG%(NG%, 5) = 5
                GoTo 158

            Case 7:
                189 NG%(NG%, 9) = 1 - NG%(NG%, 9)
                GoTo 159

            Case 8:
                192 NG%(NG%, 6) = 1 - NG%(NG%, 6)
                GoTo 162

            Case 9:
                193 NG%(NG%, 10) = 1 - NG%(NG%, 10)
                GoTo 149

            Case 10:
                194 NG%(NG%, 14) = 1 - NG%(NG%, 14)
                GoTo 149

        End Select

    Loop Until I8% = 0

End Sub
