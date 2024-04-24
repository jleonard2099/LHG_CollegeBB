'----------------------------------------
'          CAREER Subroutine
'----------------------------------------
'This routine gives the user a menu from 
'which they can choose several options
'for managing career files
Sub CAREER ()

    _Title "Courtside College Basketball - Career Stats"

    ReDim menuChoices$(7)

    Do

        Color 15, 0
        Cls

        Call TITLE_CAREER

        menuChoices$(1) = "CREATE AND MERGE INITIAL STAT FILE"
        menuChoices$(2) = "MERGE / UPDATE CAREER STATS       "
        'menuChoices$(3) = "VIEW / PRINT CAREER STATS        "
        menuChoices$(3) = "VIEW CAREER FILE                  "
        menuChoices$(4) = "PRINT CAREER FILE                 "
        menuChoices$(5) = "VIEW / PRINT CAREER RECORDS       "
        menuChoices$(6) = "EXIT TO MAIN MENU                 "
        Selection% = 1
        Count% = 6
        Start% = 1
        ExitCode% = 6
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row%-1, Column%-1, Row%+Count%, Column%+menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1:
                    Call CreateCareerFile

                Case 2:
                    Call MergeCareerFiles

                Case 3, 4:
                    Call CARVIEW

                Case 5:
                    Call ReviewCareerRecords

            End Select

        End If

    Loop Until Selection% = 6 Or ExitCode% = 27

End Sub


'----------------------------------------
'       CreateCareerFile Subroutine
'----------------------------------------
'This routine assists the user with the
'creation of a team's first career file.
'This naturally means it will guide them 
'through also adding the first year's
'stats to the file.
Sub CreateCareerFile ()

    Do

        Cls
        Color 14

        Locate 5, 27: Print " - - - C A U T I O N - - -"

        Color 15
        Locate 8, 18: Print "THIS OPTION SHOULD ONLY BE USED WHEN CREATING"
        Locate , 18: Print "AND MERGING THE INITIAL STAT FILE.  IF YOU ARE"
        Locate , 18: Print "ADDING AN ADDITIONAL YEAR TO AN EXISTING FILE,"
        Locate , 18: Print "            USE OPTION #2."
        Print
        Print
        Print

        Color 12
        Locate , 25: Print "DO YOU WISH TO CONTINUE? (Y/N)"

        I$ = GetKeyPress$

    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or I$ = Chr$(27)

    If UCase$(I$) = "Y" Then

        Call MergeFirstYear

    End If 'Done checking if user is ready

End Sub


'----------------------------------------
'        MergeFirstYear Subroutine
'----------------------------------------
'This routine actually prompts the user for 
'parameters to be used for merging the first
'year's worth of stats to the career file
Sub MergeFirstYear ()

    Dim NB%, TMR
    
    Dim careerA$(62)
    Dim A(62, 14)
    Dim careerW0!(62), careerW1!(62)
    Dim careerZ!(15), careerZ1!(15)

    Shared careerAR$()
    Shared ARS!()
    Shared W0S!(), W1S!()

    Do

        Cls

        For J = 15 To 18
            Locate J
            Print Space$(68)
        Next

        Color 11
        Locate 15, 5
        Input "ENTER NAME OF CAREER STAT FILE "; careerFile$

        teamNbr = 0
        Call ChooseTeam(YN$, NTMS, teamNbr, team$)

        If teamNbr <> 0 Then

            statFile$ = nameStatFile$(team$)

            Do

                Color 15, 0
                Cls

                Locate 8, 22: Input "ENTER YEAR TO ADD (EX. - 1961)"; statYear
                Locate 9, 22: Print "ANY CHANGE? (Y/N)"

                J$ = GetKeyPress$

            Loop Until UCase$(J$) = "Y" Or UCase$(J$) = "N" Or J$ = Chr$(27)

            If UCase$(J$) = "N" Then

                Do
                    Locate , 5
                    Print "WOULD YOU LIKE TO POST CAREER RECORDS? (Y/N)"
                    K$ = GetKeyPress$
                Loop Until UCase$(K$) = "Y" Or UCase$(K$) = "N" Or K$ = Chr$(27)

                If UCase$(K$) = "Y" Then doPostLeaders = 1 Else doPostLeaders = 0

                If Not _FileExists(diskPaths$(1) + statFile$) Then

                    MsgText$ = "NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU"
                    BoxWidth = 24
                    BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                    Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                    Call CSMsgBox(MsgText$, BoxWidth, Pass)
                    I$ = GetKeyPress$
                    Call CSMsgBox("", 0, Pass)

                Else

                    Locate 20, 5
                    Color 15
                    Print "MERGING AND UPDATING CAREER STATS . . . . PLEASE WAIT"

                    TMR = 945

                    Open diskPaths$(1) + statFile$ For Input As #1

                    Input #1, B$

                    For I = 0 To 13:

                        Input #1, careerA$(I)

                        For J = 0 To 13: Input #1, A(I, J): Next

                        Input #1, careerW0!(I)
                        Input #1, careerW1!(I)

                        Color 5
                        Locate 21, 38
                        TMR = TMR - 1
                        Print Using "####"; TMR

                    Next

                    For L = 0 To 15: Input #1, careerZ!(L): Next

                    For L = 0 To 15: Input #1, careerZ1!(L): Next

                    Close #1

                    For I = 0 To 61

                        N = 1

                        If careerA$(I) <> "END" And careerA$(I) <> "XXX" And careerA$(I) <> "" Then
                            NB% = NB% + 1
                        Else
                            careerA$(I) = ""
                        End If

                        For J = 0 To 13
                            ARS!(N, I, J) = A(I, J)
                        Next J

                        ARS!(N, I, 14) = 1
                        ARS!(N, I, 15) = statYear
                        W0S!(N, I) = careerW0!(I)
                        W1S!(N, I) = careerW1!(I)

                    Next I

                    For I = 0 To 61: careerAR$(I) = careerA$(I): Next

                    If doPostLeaders = 1 Then Call UpdateCareerLeaders(careerFile$)

                    Locate 20, 5
                    Color 15: Print "MERGING AND UPDATING CAREER STATS . . . . PLEASE WAIT"

                    Open diskPaths$(1) + careerFile$ + ".CAR" For Output As #2

                    Write #2, NB%, B$

                    For N = 1 To 15

                        For I = 0 To 61

                            Write #2, careerAR$(I)

                            For J = 0 To 15
                                Write #2, ARS!(N, I, J)
                            Next J

                            Write #2, W0S!(N, I)
                            Write #2, W1S!(N, I)

                            Color 5
                            Locate 21, 38
                            TMR = TMR - 1
                            Print Using "####"; TMR

                        Next I

                    Next N

                    Close #2

                End If 'Done checking for stat file

            End If 'Done checking for changes

        End If 'Done checking if team was chosen

    Loop Until UCase$(I$) = "N" Or UCase$(J$) = "N"

End Sub


'----------------------------------------
'    ReviewCareerRecords Subroutine
'----------------------------------------
'This routine loads in leader records from
'the .CLD file (which were compiled from  
'the career file). It then gives the user
'an option to View or Print results, AND
'formats the results for them as directed.
Sub ReviewCareerRecords ()

    Shared BL$()
    Shared BLYR(), BL()

    recordFile$ = ChooseFile$ (diskPaths$(2) + "*.CLD", "CAREER RECORD", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Color 15, 0
        Cls

        Open diskPaths$(2) + recordFile$ + ".CLD" For Input As #1

        For I = 0 To 20:
            Input #1, BLYR(I), BL$(I), BL(I)
        Next

        Close #1

        Color 15, 0
        Cls

        Call MenuBox(8, 25, 24, 5, 15, 1)

        Do

            Locate 10, 32
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

            I$ = GetKeyPress$

            Color 14, 0

        Loop Until UCase$(I$) = "F" Or UCase$(I$) = "P" Or UCase$(I$) = "S"

        Select Case UCase$(I$)

            Case "F"
                userFile$ = _SAVEFILEDIALOG$("Save Report File",_CWD$ + "\Career","*.txt","Text File")
                If userFile$ <> "" Then
                    Call CareerRecords_Report(0, userFile$)
                End If

            Case "P"
                Call CareerRecords_Report(1, "CAREER")

            Case "S":

                Do
                    Cls
                    Color 0, 15

                    Color 12: Print "CATEGORY                     RECORD           PLAYER              YEAR"
                    Print

                    Color 3: Print "FIELD GOALS MADE";: Color 7: Print Tab(33); Using "####"; BL(0);: Print Tab(47); BL$(0);: Print Tab(66); BLYR(0)
                    Color 3: Print "FIELD GOAL ATTEMPTS";: Color 7: Print Tab(33); Using "####"; BL(1);: Print Tab(47); BL$(1);: Print Tab(66); BLYR(1)
                    Color 3: Print "FIELD GOAL PCT.";: Color 7: Print Tab(32); Using "#.###"; BL(2);: Print Tab(47); BL$(2);: Print Tab(66); BLYR(2)
                    Color 3: Print "FREE THROWS MADE";: Color 7: Print Tab(33); Using "####"; BL(3);: Print Tab(47); BL$(3);: Print Tab(66); BLYR(3)
                    Color 3: Print "FREE THROW ATTEMPTS";: Color 7: Print Tab(33); Using "####"; BL(4);: Print Tab(47); BL$(4);: Print Tab(66); BLYR(4)
                    Color 3: Print "FREE THROW PCT.";: Color 7: Print Tab(32); Using "#.###"; BL(5);: Print Tab(47); BL$(5);: Print Tab(66); BLYR(5)
                    Color 3: Print "3 PT. FIELD GOALS MADE";: Color 7: Print Tab(33); Using "####"; BL(6);: Print Tab(47); BL$(6);: Print Tab(66); BLYR(6)
                    Color 3: Print "3 PT. FILED GOAL ATTEMPTS";: Color 7: Print Tab(33); Using "####"; BL(7);: Print Tab(47); BL$(7);: Print Tab(66); BLYR(7)
                    Color 3: Print "3 PT. FIELD GOAL PCT.";: Color 7: Print Tab(32); Using "#.###"; BL(8);: Print Tab(47); BL$(8);: Print Tab(66); BLYR(8)
                    Color 3: Print "POINTS";: Color 7: Print Tab(32); Using "#####"; BL(9);: Print Tab(47); BL$(9);: Print Tab(66); BLYR(9)
                    Color 3: Print "POINTS PER GAME";: Color 7: Print Tab(32); Using "###.#"; BL(10);: Print Tab(47); BL$(10);: Print Tab(66); BLYR(10)
                    Color 3: Print "OFFENSIVE REBOUNDS";: Color 7: Print Tab(32); Using "#####"; BL(11);: Print Tab(47); BL$(11);: Print Tab(66); BLYR(11)
                    Color 3: Print "DEFENSIVE REBOUNDS";: Color 7: Print Tab(32); Using "#####"; BL(12);: Print Tab(47); BL$(12);: Print Tab(66); BLYR(12)
                    Color 3: Print "TOTAL REBOUNDS";: Color 7: Print Tab(32); Using "#####"; BL(13);: Print Tab(47); BL$(13);: Print Tab(66); BLYR(13)
                    Color 3: Print "REBOUNDS PER GAME";: Color 7: Print Tab(32); Using "###.#"; BL(14);: Print Tab(47); BL$(14);: Print Tab(66); BLYR(14)

                    Color 15
                    Locate 23, 5: Print "HIT ANY KEY TO CONTINUE"
                    I$ = GetKeyPress$

                    Cls
                    Color 12: Print "CATEGORY                     RECORD           PLAYER              YEAR"
                    Print
                    Color 3: Print "PERSONAL FOULS";: Color 7: Print Tab(33); Using "####"; BL(15);: Print Tab(47); BL$(15);: Print Tab(66); BLYR(15)
                    Color 3: Print "ASSISTS";: Color 7: Print Tab(32); Using "#####"; BL(16);: Print Tab(47); BL$(16);: Print Tab(66); BLYR(16)
                    Color 3: Print "SHOTS BLOCKED";: Color 7: Print Tab(33); Using "####"; BL(17);: Print Tab(47); BL$(17);: Print Tab(66); BLYR(17)
                    Color 3: Print "STEALS";: Color 7: Print Tab(32); Using "#####"; BL(18);: Print Tab(47); BL$(18);: Print Tab(66); BLYR(18)
                    Color 3: Print "TURNOVERS";: Color 7: Print Tab(32); Using "#####"; BL(19);: Print Tab(47); BL$(19);: Print Tab(66); BLYR(19)
                    Color 3: Print "HI-GAME (POINTS)";: Color 7: Print Tab(33); Using "####"; BL(20);: Print Tab(47); BL$(20);: Print Tab(66); BLYR(20)

                    Color 15

                    Do
                        Locate 22: Print "VIEW AGAIN? (Y/N)"
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y"

                Loop Until UCase$(I$) = "N"

        End Select

    End If 'Done checking for ESC

End Sub


'----------------------------------------
'    CareerRecords_Report Subroutine
'----------------------------------------
'This routine formats career records for
'output to either a file or printer, as
'specified by the user. All operations
'involve creating a file first, that can
'then be looped through if sending to 
'printer
Sub CareerRecords_Report (printDest, destFile$)

    Shared BL$()
    Shared BLYR(), BL()

	Color 15, 0
	Cls

	Open destFile$ For Output As #1

    Print #1, "CATEGORY                     RECORD           PLAYER              YEAR"
    Print #1,
    Print #1, "FIELD GOALS MADE";: Print #1, Tab(33); Using "####"; BL(0);
    Print #1, Tab(47); BL$(0);
    Print #1, Tab(66); BLYR(0)
    Print #1, "FIELD GOAL ATTEMPTS";
    Print #1, Tab(33); Using "####"; BL(1);
    Print #1, Tab(47); BL$(1);
    Print #1, Tab(66); BLYR(1)
    Print #1, "FIELD GOAL PCT.";
    Print #1, Tab(32); Using "#.###"; BL(2);
    Print #1, Tab(47); BL$(2);
    Print #1, Tab(66); BLYR(2)
    Print #1, "FREE THROWS MADE";
    Print #1, Tab(33); Using "####"; BL(3);
    Print #1, Tab(47); BL$(3);
    Print #1, Tab(66); BLYR(3)
    Print #1, "FREE THROW ATTEMPTS";
    Print #1, Tab(33); Using "####"; BL(4);
    Print #1, Tab(47); BL$(4);
    Print #1, Tab(66); BLYR(4)
    Print #1, "FREE THROW PCT.";
    Print #1, Tab(32); Using "#.###"; BL(5);
    Print #1, Tab(47); BL$(5);
    Print #1, Tab(66); BLYR(5)
    Print #1, "3 PT. FIELD GOALS MADE";
    Print #1, Tab(33); Using "####"; BL(6);
    Print #1, Tab(47); BL$(6);
    Print #1, Tab(66); BLYR(6)
    Print #1, "3 PT. FILED GOAL ATTEMPTS";
    Print #1, Tab(33); Using "####"; BL(7);
    Print #1, Tab(47); BL$(7);
    Print #1, Tab(66); BLYR(7)
    Print #1, "3 PT. FIELD GOAL PCT.";
    Print #1, Tab(32); Using "#.###"; BL(8);
    Print #1, Tab(47); BL$(8);
    Print #1, Tab(66); BLYR(8)
    Print #1, "POINTS";
    Print #1, Tab(32); Using "#####"; BL(9);
    Print #1, Tab(47); BL$(9);
    Print #1, Tab(66); BLYR(9)
    Print #1, "POINTS PER GAME";
    Print #1, Tab(32); Using "###.#"; BL(10);
    Print #1, Tab(47); BL$(10);
    Print #1, Tab(66); BLYR(10)
    Print #1, "OFFENSIVE REBOUNDS";
    Print #1, Tab(32); Using "#####"; BL(11);
    Print #1, Tab(47); BL$(11);
    Print #1, Tab(66); BLYR(11)
    Print #1, "DEFENSIVE REBOUNDS";
    Print #1, Tab(32); Using "#####"; BL(12);
    Print #1, Tab(47); BL$(12);
    Print #1, Tab(66); BLYR(12)
    Print #1, "TOTAL REBOUNDS";
    Print #1, Tab(32); Using "#####"; BL(13);
    Print #1, Tab(47); BL$(13);
    Print #1, Tab(66); BLYR(13)
    Print #1, "REBOUNDS PER GAME";
    Print #1, Tab(32); Using "###.#"; BL(14);
    Print #1, Tab(47); BL$(14);
    Print #1, Tab(66); BLYR(14)
    Print #1, "PERSONAL FOULS";
    Print #1, Tab(33); Using "####"; BL(15);
    Print #1, Tab(47); BL$(15);
    Print #1, Tab(66); BLYR(15)
    Print #1, "ASSISTS";
    Print #1, Tab(32); Using "#####"; BL(16);
    Print #1, Tab(47); BL$(16);
    Print #1, Tab(66); BLYR(16)
    Print #1, "SHOTS BLOCKED";
    Print #1, Tab(33); Using "####"; BL(17);
    Print #1, Tab(47); BL$(17);
    Print #1, Tab(66); BLYR(17)
    Print #1, "STEALS";
    Print #1, Tab(32); Using "#####"; BL(18);
    Print #1, Tab(47); BL$(18);
    Print #1, Tab(66); BLYR(18)
    Print #1, "TURNOVERS";
    Print #1, Tab(32); Using "#####"; BL(19);
    Print #1, Tab(47); BL$(19);
    Print #1, Tab(66); BLYR(19)
    Print #1, "HI-GAME (POINTS)";
    Print #1, Tab(33); Using "####"; BL(20);
    Print #1, Tab(47); BL$(20);
    Print #1, Tab(66); BLYR(20)

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
'       MergeCareerFiles Subroutine
'----------------------------------------
'This routine assists thet user with the
'addition of career stats from any extra 
'season years that have been compiled.
Sub MergeCareerFiles ()

    _Title "Courtside College Basketball - Update Career Files"

    Do

        Color 14, 0
        Cls

        Locate 5, 27
        Print "- - - C A U T I O N - - -"

        Color 15
        Locate 8, 1

        Print "THIS OPTION SHOULD ONLY BE USED WHEN ADDING YEARS TO AN EXISTING STAT"
        Print "FILE. IF YOU ARE CREATING A NEW CAREER FILE, USE THE CREATE OPTION."
        Print
        Print "IF YOU ARE ADDING AN ADDITIONAL CAREER YEAR, MAKE SURE THAT ANY UNUSED PLAYER"
        Print "SLOTS (XXX, END, ETC.) ARE AT THE END OF THE SEASON STAT LIST BEFORE YOU MERGE."
        Print
        Print "ALSO MAKE SURE THAT THE PLAYERS NAMES IN THE FILE YOU ARE ADDING, MATCH"
        Print "THE EXACT SPELLING OF THE NAME OF THE SAME PLAYER IN THE CAREER FILE FOR THE"
        Print "PLAYERS THAT HAD PLAYED IN PREVIOUS YEARS.  EXAMPLE - IF BILLY WILLIAMS IS"
        Print "LISTED AS 'WILLIAMS' IN THE CAREER FILE AND THE SEASON STATS HAD HIM AS"
        Print "'B.WILLIAMS', HE SHOULD BE CHANGED TO 'WILLIAMS' IN THE SEASON FILE BEFORE"
        Print "MERGING, OR YOU COULD CHANGE HIM TO 'B.WILLIAMS' IN THE CAREER FILE"
        Print
        Print

        Color 12: Locate , 25
        Print "DO YOU WISH TO CONTINUE? (Y/N)"

        I$ = GetKeyPress$

    Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y" Or I$ = Chr$(27)

    If UCase$(I$) = "Y" Then

        careerFile$ = ChooseFile$ (diskPaths$(1) + "*.CAR", "CAREER", ExitCode%)

        If ExitCode% <> 27 And ExitCode% <> 0 Then

            teamNbr = 0
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

                Call MergeAddYear(careerFile$, statFile$)

            End If

        End If 'Done checking for ESC

    End If 'Done confirming user choice

End Sub


'----------------------------------------
'         MergeAddYear Subroutine
'----------------------------------------
'This routine actually prompts the user
'for parameters for the team / year to
'be updated. It then loads the existing 
'career stats, adds the team's stats to 
'them, then overwrites the career file 
'with the new values. 
Sub MergeAddYear (selectedFile$, statFile$)

    Dim NB%, TMR1%

    Dim careerA$(62)
    Dim A(62, 14)
    Dim careerW0!(62), careerW1!(62)
    Dim careerZ!(15), careerZ1!(15)

    Shared careerAR$()
    Shared ARS!(), W0S!(), W1S!()

    TMR1% = 1875
    NB% = -1

    Color 15, 0
    Cls

    Do
        Locate 8, 22: Input "ENTER YEAR TO ADD (EX. - 1961)"; statYear
        Locate 9, 22: Print "ANY CHANGE? (Y/N)"
        J$ = GetKeyPress$
    Loop Until UCase$(J$) = "Y" Or UCase$(J$) = "N"

    If UCase$(J$) = "N" Then

        Do
            Locate , 5
            Print "WOULD YOU LIKE TO POST CAREER RECORDS? (Y/N)"
            K$ = GetKeyPress$
            finalUpdate$ = UCase$(K$)
        Loop Until finalUpdate$ = "Y" Or finalUpdate$ = "N" Or K$ = Chr$(27)

        If finalUpdate$ = "Y" Then doPostLeaders = 1 Else doPostLeaders = 0

        Locate 20, 5
        Print "LOADING CAREER STATS . . . . PLEASE WAIT"

        Open diskPaths$(1) + selectedFile$ + ".CAR" For Input As #1

        Input #1, NB%, B$

        For N = 1 To 15

            For I = 0 To 61:

                Input #1, careerAR$(I)

                For J = 0 To 15:
                    Input #1, ARS!(N, I, J)
                Next J

                Input #1, W0S!(N, I)
                Input #1, W1S!(N, I)

                Color 5
                Locate 21, 38
                TMR1% = TMR1% - 1
                Print Using "####"; TMR1%

            Next I

        Next N

        Close #1

        Locate 20, 5
        Print "LOADING STATS FILE . . . . PLEASE WAIT"

        Open diskPaths$(1) + statFile$ For Input As #2

        Input #2, B$

        For I = 0 To 13:

            Input #2, careerA$(I)

            For J = 0 To 13: Input #2, A(I, J): Next

            Input #2, careerW0!(I)
            Input #2, careerW1!(I)

            Color 5
            Locate 21, 38
            TMR1% = TMR1% - 1
            Print Using "####"; TMR1%

        Next

        For L = 0 To 15: Input #2, careerZ!(L): Next

        For L = 0 To 15: Input #2, careerZ1!(L): Next

        Close #2

        For I = 0 To 13
            RR = 0
            CHK = 0

            For J = 0 To 61

                If careerAR$(J) = careerA$(I) Then

                    For W = 1 To 15
                        If ARS!(W, J, 15) = statYear Then
                            CHK = CHK + 1
                        End If
                    Next W

                    If CHK > 0 Then
                        N = ARS!(1, J, 14) + ARS!(2, J, 14) + ARS!(3, J, 14) + ARS!(4, J, 14)
                        N = N + ARS!(5, J, 14) + ARS!(6, J, 14) + ARS!(7, J, 14) + ARS!(8, J, 14)
                        N = N + ARS!(9, J, 14) + ARS!(10, J, 14) + ARS!(11, J, 14)
                        N = N + ARS!(12, J, 14) + ARS!(13, J, 14) + ARS!(14, J, 14) + ARS!(15, J, 14)
                    Else
                        N = ARS!(1, J, 14) + ARS!(2, J, 14) + ARS!(3, J, 14) + ARS!(4, J, 14)
                        N = N + ARS!(5, J, 14) + ARS!(6, J, 14) + ARS!(7, J, 14) + ARS!(8, J, 14)
                        N = N + ARS!(9, J, 14) + ARS!(10, J, 14) + ARS!(11, J, 14)
                        N = N + ARS!(12, J, 14) + ARS!(13, J, 14) + ARS!(14, J, 14) + ARS!(15, J, 14) + 1
                    End If

                    For K = 0 To 13
                        ARS!(N, J, K) = A(I, K)
                    Next K

                    ARS!(N, J, 14) = 1
                    ARS!(N, J, 15) = statYear
                    W0S!(N, J) = careerW0!(J)
                    W1S!(N, J) = careerW1!(J)

                Else
                    RR = RR + 1
                    If RR = 62 Then

                        If careerA$(I) <> "END" And careerA$(I) <> "XXX" And careerA$(I) <> "" Then

                            NB% = NB% + 1
                            careerAR$(NB%) = careerA$(I)
                            N = 1

                            For K = 0 To 13: ARS!(N, NB%, K) = A(I, K): Next

                            ARS!(N, NB%, 14) = 1
                            ARS!(N, NB%, 15) = statYear
                            W0S!(N, NB%) = careerW0!(NB%)
                            W1S!(N, NB%) = careerW1!(NB%)
                        End If

                    End If

                End If 'Done comparing careerAR$() to careerA$()

            Next J

        Next I

        If doPostLeaders = 1 Then Call UpdateCareerLeaders(selectedFile$)

        Locate 20, 5
        Print "MERGING CAREER STATS . . . . PLEASE WAIT"

        Open diskPaths$(1) + selectedFile$ + ".CAR" For Output As #3

        Write #3, NB%, B$

        For N = 1 To 15

            For I = 0 To 61

                Write #3, careerAR$(I)

                For J = 0 To 15:
                    Write #3, ARS!(N, I, J)
                Next J

                Write #3, W0S!(N, I)
                Write #3, W1S!(N, I)

                Color 5
                Locate 21, 38
                TMR1% = TMR1% - 1
                Print Using "####"; TMR1%:

            Next I

        Next N

        Close #3

    End If

End Sub


'----------------------------------------
'     UpdateCareerLeaders Subroutine
'----------------------------------------
'This routine reads in any existing career 
'leader files (.CLD), adds the new stats,
'and then saves the changes back to the
'career leader file (.CLD)
Sub UpdateCareerLeaders (targetFile$)

    Dim BL$(21)
    Dim BLYR(21), BL(21)

    Shared careerAR$()
    Shared ARS!(), W0S!(), W1S!()

    If _FileExists(diskPaths$(2) + targetFile$ + ".CLD") Then

        Open diskPaths$(2) + targetFile$ + ".CLD" For Input As #1

        For I = 0 To 20
            Input #1, BLYR(I), BL$(I), BL(I)
        Next

        Close #1

    End If

    For I = 0 To 61

        If careerAR$(I) <> "" And careerAR$(I) <> "XXX" And careerAR$(I) <> "END" Then

            For J = 1 To 15

                If ARS!(J, I, 15) <> 0 Then

                    If ARS!(J, I, 1) > BL(0) Then
                        BL(0) = ARS!(J, I, 1)
                        BL$(0) = careerAR$(I)
                        BLYR(0) = ARS!(J, I, 15)
                    End If

                    If ARS!(J, I, 2) > BL(1) Then
                        BL(1) = ARS!(J, I, 2)
                        BL$(1) = careerAR$(I)
                        BLYR(1) = ARS!(J, I, 15)
                    End If

                    If Not (ARS!(J, I, 1) = 0 Or ARS!(J, I, 1) / ARS!(J, I, 0) < 3.65) Then
                        If ARS!(J, I, 2) / ARS!(J, I, 1) > BL(2) Then
                            BL(2) = ARS!(J, I, 2) / ARS!(J, I, 1)
                            BL$(2) = careerAR$(I)
                            BLYR(2) = ARS!(J, I, 15)
                        End If
                    End If

                    If ARS!(J, I, 4) > BL(3) Then
                        BL(3) = ARS!(J, I, 4)
                        BL$(3) = careerAR$(I): BLYR(3) = ARS!(J, I, 15)
                    End If

                    If ARS!(J, I, 3) > BL(4) Then
                        BL(4) = ARS!(J, I, 3)
                        BL$(4) = careerAR$(I)
                        BLYR(4) = ARS!(J, I, 15)
                    End If

                    If Not (ARS!(J, I, 3) = 0 Or ARS!(J, I, 3) / ARS!(J, I, 0) < 1.5) Then
                        If ARS!(J, I, 4) / ARS!(J, I, 3) > BL(5) Then
                            BL(5) = ARS!(J, I, 4) / ARS!(J, I, 3)
                            BL$(5) = careerAR$(I): BLYR(5) = ARS!(J, I, 15)
                        End If
                    End If

                    If W0S!(J, I) > BL(6) Then
                        BL(6) = W0S!(J, I)
                        BL$(6) = careerAR$(I)
                        BLYR(6) = ARS!(J, I, 15)
                    End If

                    If W1S!(J, I) > BL(7) Then
                        BL(7) = W1S!(J, I)
                        BL$(7) = careerAR$(I)
                        BLYR(7) = ARS!(J, I, 15)
                    End If

                    If Not (W1S!(J, I) = 0 Or W1S!(J, I) / ARS!(J, I, 0) < .3) Then
                        If W0S!(J, I) / W1S!(J, I) > BL(8) Then
                            BL(8) = W0S!(J, I) / W1S!(J, I)
                            BL$(8) = careerAR$(I)
                            BLYR(8) = ARS!(J, I, 15)
                        End If
                    End If

                    If ARS!(J, I, 2) * 2 + ARS!(J, I, 4) + W0S!(J, I) > BL(9) Then
                        BL(9) = ARS!(J, I, 2) * 2 + ARS!(J, I, 4) + W0S!(J, I)
                        BL$(9) = careerAR$(I): BLYR(9) = ARS!(J, I, 15)
                    End If

                    If ARS!(J, I, 0) <> 0 Then
                        If (ARS!(J, I, 2) * 2 + ARS!(J, I, 4) + W0S!(J, I)) / ARS!(J, I, 0) > BL(10) Then
                            BL(10) = (ARS!(J, I, 2) * 2 + ARS!(J, I, 4) + W0S!(J, I)) / ARS!(J, I, 0)
                            BL$(10) = careerAR$(I)
                            BLYR(10) = ARS!(J, I, 15)
                        End If
                    End If

                    If ARS!(J, I, 12) > BL(11) Then
                        BL(11) = ARS!(J, I, 12)
                        BL$(11) = careerAR$(I): BLYR(11) = ARS!(J, I, 15)
                    End If

                    If ARS!(J, I, 5) - ARS!(J, I, 12) > BL(12) Then
                        BL(12) = ARS!(J, I, 5) - ARS!(J, I, 12)
                        BL$(12) = careerAR$(I): BLYR(12) = ARS!(J, I, 15)
                    End If

                    If ARS!(J, I, 5) > BL(13) Then
                        BL(13) = ARS!(J, I, 5)
                        BL$(13) = careerAR$(I)
                        BLYR(13) = ARS!(J, I, 15)
                    End If

                    If ARS!(J, I, 0) <> 0 Then
                        If ARS!(J, I, 5) / ARS!(J, I, 0) > BL(14) Then
                            BL(14) = ARS!(J, I, 5) / ARS!(J, I, 0)
                            BL$(14) = careerAR$(I)
                            BLYR(14) = ARS!(J, I, 15)
                        End If
                    End If

                    If ARS!(J, I, 6) > BL(15) Then
                        BL(15) = ARS!(J, I, 6)
                        BL$(15) = careerAR$(I)
                        BLYR(15) = ARS!(J, I, 15)
                    End If

                    If ARS!(J, I, 8) > BL(16) Then
                        BL(16) = ARS!(J, I, 8)
                        BL$(16) = careerAR$(I)
                        BLYR(16) = ARS!(J, I, 15)
                    End If

                    If ARS!(J, I, 9) > BL(17) Then
                        BL(17) = ARS!(J, I, 9)
                        BL$(17) = careerAR$(I)
                        BLYR(17) = ARS!(J, I, 15)
                    End If

                    If ARS!(J, I, 7) > BL(18) Then
                        BL(18) = ARS!(J, I, 7)
                        BL$(18) = careerAR$(I)
                        BLYR(18) = ARS!(J, I, 15)
                    End If

                    If ARS!(J, I, 11) > BL(19) Then
                        BL(19) = ARS!(J, I, 11)
                        BL$(19) = careerAR$(I)
                        BLYR(19) = ARS!(J, I, 15)
                    End If

                    If ARS!(J, I, 13) <= BL(20) Then
                        BL(20) = ARS!(J, I, 13)
                        BL$(20) = careerAR$(I)
                        BLYR(20) = ARS!(J, I, 15)
                    End If

                End If

            Next J

        End If

    Next I

    Open diskPaths$(2) + targetFile$ + ".CLD" For Output As #2

    For I = 0 To 20
        Write #2, BLYR(I), BL$(I), BL(I)
    Next

    Close #2

End Sub
