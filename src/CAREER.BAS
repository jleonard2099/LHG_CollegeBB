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

        menuChoices$(1) = "CREATE INITIAL CAREER STATS FILE "
        menuChoices$(2) = "ADD CAREER YEAR TO STATS         "
        menuChoices$(3) = "REMOVE PLAYERS FROM CAREER STATS "
        menuChoices$(4) = "VIEW / PRINT CAREER STATS        "
        menuChoices$(5) = "VIEW / PRINT CAREER RECORDS      "
        menuChoices$(6) = "UPDATE / VIEW TEAM CAREER LEADERS"
        menuChoices$(7) = "RETURN TO MAIN MENU              "
        SELECTION% = 1
        Count% = 7
        Start% = 1
        ExitCode% = 7
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row%-1, Column%-1, Row%+Count%, Column%+menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1:
                    Call CreateCareerFile

                Case 2:
                    Call MergeCareerFiles

                Case 3:
                    Call CAREDIT

                Case 4:
                    Call CARVIEW

                Case 5:
                    Call ReviewCareerRecords

                Case 6:
                    Call CARLDR

            End Select

        End If

    Loop Until SELECTION% = 7 Or ExitCode% = 27

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

    Shared NB, NP

    Dim carStatVal!(62, 14)
    Dim careerW0!(62), careerW1!(62)

    Shared careerPlayers$()
    Shared carPlyrStat!(), car3FGM!(), car3FGA!()
    Shared statsOffTot!(), statsDefTot!()

    NB = -1

    Do

        Color 11, 0
        Cls

        For J = 15 To 18
            Locate J
            Print Space$(68)
        Next

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

                If UCase$(K$) = "Y" Then 
                    doPostLeaders = 1 
                Else 
                    doPostLeaders = 0
                End If

                If Not _FileExists(diskPaths$(1) + statFile$) Then

                    MsgText$ = "NO COMPILED STATS COULD BE FOUND FOR THIS TEAM. PRESS ANY KEY TO RETURN TO THE MAIN MENU"
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

                    TMR = 14 * 17

                    Open diskPaths$(1) + statFile$ For Input As #1

                    Input #1, B$

                    For I = 0 To 13:

                        Input #1, careerPlayers$(I)

                        For J = 0 To 13: 
                            Input #1, carStatVal!(I, J)
                        Next

                        Input #1, careerW0!(I)
                        Input #1, careerW1!(I)

                        Color 5
                        Locate 21, 38
                        TMR = TMR - 1
                        Print Using "####"; TMR

                    Next

                    For L = 0 To 15: 
                        Input #1, statsOffTot!(L)
                    Next

                    For L = 0 To 15: 
                        Input #1, statsDefTot!(L)
                    Next

                    Close #1

                    For I = 0 To 61

                        N = 1

                        If careerPlayers$(I) = "END" Or careerPlayers$(I) = "XXX" Or careerPlayers$(I) = "" Then
                            careerPlayers$(I) = ""
                        Else
                            NB = NB + 1
                        End If

                        For J = 0 To 13
                            carPlyrStat!(N, I, J) = carStatVal!(I, J)
                        Next J

                        carPlyrStat!(N, I, 14) = 1
                        carPlyrStat!(N, I, 15) = statYear

                        car3FGM!(N, I) = careerW0!(I)
                        car3FGA!(N, I) = careerW1!(I)

                    Next I

                    If doPostLeaders = 1 Then Call UpdateCareerRecords(careerFile$)

                    Call SaveCareerFile (careerFile$)

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

    Shared carRecVal!(), carRecYear!()
    Shared BL$()

    recordFile$ = ChooseFile$ (diskPaths$(2) + "*.CLD", "CAREER RECORD", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Color 15, 0
        Cls

        Open diskPaths$(2) + recordFile$ + ".CLD" For Input As #1

        For I = 0 To 20:
            Input #1, carRecYear!(I), BL$(I), carRecVal!(I)
        Next

        Close #1

        userKey$ = GetOutputOptions$

        If userKey$ <> Chr$(27) Then

            Select Case userKey$

                Case "F"
                    userFile$ = _SaveFileDialog$("Save Report File", _CWD$ + "\Career", "*.txt", "Text File")
                    If userFile$ <> "" Then
                        Call CareerRecords_Report(0, userFile$)
                    End If

                Case "P"
                    Call CareerRecords_Report(1, "CAREER")

                Case "S":

                    Do
                        Cls
                        Color 15, 0

                        Color 12: Print "CATEGORY                     RECORD           PLAYER              YEAR"
                        Print

                        Color 3: Print "FIELD GOALS MADE";
                        Color 7: Print Tab(33); Using "####"; carRecVal!(0);: Print Tab(47); BL$(0);: Print Tab(66); carRecYear!(0)
                        Color 3: Print "FIELD GOAL ATTEMPTS";
                        Color 7: Print Tab(33); Using "####"; carRecVal!(1);: Print Tab(47); BL$(1);: Print Tab(66); carRecYear!(1)
                        Color 3: Print "FIELD GOAL PCT.";
                        Color 7: Print Tab(32); Using "#.###"; carRecVal!(2);: Print Tab(47); BL$(2);: Print Tab(66); carRecYear!(2)
                        Color 3: Print "FREE THROWS MADE";
                        Color 7: Print Tab(33); Using "####"; carRecVal!(3);: Print Tab(47); BL$(3);: Print Tab(66); carRecYear!(3)
                        Color 3: Print "FREE THROW ATTEMPTS";
                        Color 7: Print Tab(33); Using "####"; carRecVal!(4);: Print Tab(47); BL$(4);: Print Tab(66); carRecYear!(4)
                        Color 3: Print "FREE THROW PCT.";
                        Color 7: Print Tab(32); Using "#.###"; carRecVal!(5);: Print Tab(47); BL$(5);: Print Tab(66); carRecYear!(5)
                        Color 3: Print "3 PT. FIELD GOALS MADE";
                        Color 7: Print Tab(33); Using "####"; carRecVal!(6);: Print Tab(47); BL$(6);: Print Tab(66); carRecYear!(6)
                        Color 3: Print "3 PT. FILED GOAL ATTEMPTS";
                        Color 7: Print Tab(33); Using "####"; carRecVal!(7);: Print Tab(47); BL$(7);: Print Tab(66); carRecYear!(7)
                        Color 3: Print "3 PT. FIELD GOAL PCT.";
                        Color 7: Print Tab(32); Using "#.###"; carRecVal!(8);: Print Tab(47); BL$(8);: Print Tab(66); carRecYear!(8)
                        Color 3: Print "POINTS";
                        Color 7: Print Tab(32); Using "#####"; carRecVal!(9);: Print Tab(47); BL$(9);: Print Tab(66); carRecYear!(9)
                        Color 3: Print "POINTS PER GAME";
                        Color 7: Print Tab(32); Using "###.#"; carRecVal!(10);: Print Tab(47); BL$(10);: Print Tab(66); carRecYear!(10)
                        Color 3: Print "OFFENSIVE REBOUNDS";
                        Color 7: Print Tab(32); Using "#####"; carRecVal!(11);: Print Tab(47); BL$(11);: Print Tab(66); carRecYear!(11)
                        Color 3: Print "DEFENSIVE REBOUNDS";
                        Color 7: Print Tab(32); Using "#####"; carRecVal!(12);: Print Tab(47); BL$(12);: Print Tab(66); carRecYear!(12)
                        Color 3: Print "TOTAL REBOUNDS";
                        Color 7: Print Tab(32); Using "#####"; carRecVal!(13);: Print Tab(47); BL$(13);: Print Tab(66); carRecYear!(13)
                        Color 3: Print "REBOUNDS PER GAME";
                        Color 7: Print Tab(32); Using "###.#"; carRecVal!(14);: Print Tab(47); BL$(14);: Print Tab(66); carRecYear!(14)

                        Color 15
                        Locate 23, 5: Print "HIT ANY KEY TO CONTINUE"
                        I$ = GetKeyPress$

                        Cls
                        Color 12: Print "CATEGORY                     RECORD           PLAYER              YEAR"
                        Print
                        Color 3: Print "PERSONAL FOULS";
                        Color 7: Print Tab(33); Using "####"; carRecVal!(15);: Print Tab(47); BL$(15);: Print Tab(66); carRecYear!(15)
                        Color 3: Print "ASSISTS";
                        Color 7: Print Tab(32); Using "#####"; carRecVal!(16);: Print Tab(47); BL$(16);: Print Tab(66); carRecYear!(16)
                        Color 3: Print "SHOTS BLOCKED";
                        Color 7: Print Tab(33); Using "####"; carRecVal!(17);: Print Tab(47); BL$(17);: Print Tab(66); carRecYear!(17)
                        Color 3: Print "STEALS";
                        Color 7: Print Tab(32); Using "#####"; carRecVal!(18);: Print Tab(47); BL$(18);: Print Tab(66); carRecYear!(18)
                        Color 3: Print "TURNOVERS";
                        Color 7: Print Tab(32); Using "#####"; carRecVal!(19);: Print Tab(47); BL$(19);: Print Tab(66); carRecYear!(19)
                        Color 3: Print "HI-GAME (POINTS)";
                        Color 7: Print Tab(33); Using "####"; carRecVal!(20);: Print Tab(47); BL$(20);: Print Tab(66); carRecYear!(20)

                        Color 15

                        Do
                            Locate 22: Print "VIEW AGAIN? (Y/N)"
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "N" Or Ucase$(I$) = "Y" Or I$ = Chr$(27)

                    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

            End Select
            
        End If

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

    Shared carRecVal!(), carRecYear!()
    Shared BL$()

    Color 0, 15
	Cls

	Open destFile$ For Output As #1

    Print #1, "CATEGORY                     RECORD           PLAYER              YEAR"
    Print #1,
    Print #1, "FIELD GOALS MADE";
    Print #1, Tab(33); Using "####"; carRecVal!(0);
    Print #1, Tab(47); BL$(0);
    Print #1, Tab(66); carRecYear!(0)
    Print #1, "FIELD GOAL ATTEMPTS";
    Print #1, Tab(33); Using "####"; carRecVal!(1);
    Print #1, Tab(47); BL$(1);
    Print #1, Tab(66); carRecYear!(1)
    Print #1, "FIELD GOAL PCT.";
    Print #1, Tab(32); Using "#.###"; carRecVal!(2);
    Print #1, Tab(47); BL$(2);
    Print #1, Tab(66); carRecYear!(2)
    Print #1, "FREE THROWS MADE";
    Print #1, Tab(33); Using "####"; carRecVal!(3);
    Print #1, Tab(47); BL$(3);
    Print #1, Tab(66); carRecYear!(3)
    Print #1, "FREE THROW ATTEMPTS";
    Print #1, Tab(33); Using "####"; carRecVal!(4);
    Print #1, Tab(47); BL$(4);
    Print #1, Tab(66); carRecYear!(4)
    Print #1, "FREE THROW PCT.";
    Print #1, Tab(32); Using "#.###"; carRecVal!(5);
    Print #1, Tab(47); BL$(5);
    Print #1, Tab(66); carRecYear!(5)
    Print #1, "3 PT. FIELD GOALS MADE";
    Print #1, Tab(33); Using "####"; carRecVal!(6);
    Print #1, Tab(47); BL$(6);
    Print #1, Tab(66); carRecYear!(6)
    Print #1, "3 PT. FILED GOAL ATTEMPTS";
    Print #1, Tab(33); Using "####"; carRecVal!(7);
    Print #1, Tab(47); BL$(7);
    Print #1, Tab(66); carRecYear!(7)
    Print #1, "3 PT. FIELD GOAL PCT.";
    Print #1, Tab(32); Using "#.###"; carRecVal!(8);
    Print #1, Tab(47); BL$(8);
    Print #1, Tab(66); carRecYear!(8)
    Print #1, "POINTS";
    Print #1, Tab(32); Using "#####"; carRecVal!(9);
    Print #1, Tab(47); BL$(9);
    Print #1, Tab(66); carRecYear!(9)
    Print #1, "POINTS PER GAME";
    Print #1, Tab(32); Using "###.#"; carRecVal!(10);
    Print #1, Tab(47); BL$(10);
    Print #1, Tab(66); carRecYear!(10)
    Print #1, "OFFENSIVE REBOUNDS";
    Print #1, Tab(32); Using "#####"; carRecVal!(11);
    Print #1, Tab(47); BL$(11);
    Print #1, Tab(66); carRecYear!(11)
    Print #1, "DEFENSIVE REBOUNDS";
    Print #1, Tab(32); Using "#####"; carRecVal!(12);
    Print #1, Tab(47); BL$(12);
    Print #1, Tab(66); carRecYear!(12)
    Print #1, "TOTAL REBOUNDS";
    Print #1, Tab(32); Using "#####"; carRecVal!(13);
    Print #1, Tab(47); BL$(13);
    Print #1, Tab(66); carRecYear!(13)
    Print #1, "REBOUNDS PER GAME";
    Print #1, Tab(32); Using "###.#"; carRecVal!(14);
    Print #1, Tab(47); BL$(14);
    Print #1, Tab(66); carRecYear!(14)
    Print #1, "PERSONAL FOULS";
    Print #1, Tab(33); Using "####"; carRecVal!(15);
    Print #1, Tab(47); BL$(15);
    Print #1, Tab(66); carRecYear!(15)
    Print #1, "ASSISTS";
    Print #1, Tab(32); Using "#####"; carRecVal!(16);
    Print #1, Tab(47); BL$(16);
    Print #1, Tab(66); carRecYear!(16)
    Print #1, "SHOTS BLOCKED";
    Print #1, Tab(33); Using "####"; carRecVal!(17);
    Print #1, Tab(47); BL$(17);
    Print #1, Tab(66); carRecYear!(17)
    Print #1, "STEALS";
    Print #1, Tab(32); Using "#####"; carRecVal!(18);
    Print #1, Tab(47); BL$(18);
    Print #1, Tab(66); carRecYear!(18)
    Print #1, "TURNOVERS";
    Print #1, Tab(32); Using "#####"; carRecVal!(19);
    Print #1, Tab(47); BL$(19);
    Print #1, Tab(66); carRecYear!(19)
    Print #1, "HI-GAME (POINTS)";
    Print #1, Tab(33); Using "####"; carRecVal!(20);
    Print #1, Tab(47); BL$(20);
    Print #1, Tab(66); carRecYear!(20)

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

        Color 12
        Locate , 25
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

                MsgText$ = "NO COMPILED STATS COULD BE FOUND FOR THIS TEAM. PRESS ANY KEY TO RETURN TO THE MAIN MENU"
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

        If finalUpdate$ = "Y" Then 
            doPostLeaders = 1 
        Else 
            doPostLeaders = 0
        End If

        Call AddCareerYear(statYear, doPostLeaders, selectedFile$, statFile$)

    End If 'Done checking user confirmation
    
End Sub


'----------------------------------------
'       AddCareerYear Subroutine
'----------------------------------------
'This routine loads the existing career
'stats, adds the team's stats to them,
'then overwrites the career file with the
'new values. 
Sub AddCareerYear (targetYear, finalUpdate, careerFile$, statFile$)

    Shared NB, NP

    Dim carStatPlyr$(62)
    Dim carStatVal!(14, 14)
    
    Dim careerW0!(62), careerW1!(62)

    Shared careerPlayers$()

    Shared carPlyrStat!(), car3FGM!(), car3FGA!()
    Shared statsOffTot!(), statsDefTot!()

    NB = -1

    Call LoadCareerFile (careerFile$)

    Locate 20, 5
    Print "LOADING STATS FILE . . . . PLEASE WAIT"

    TMR1% = 14*17

    Open diskPaths$(1) + statFile$ For Input As #2

    Input #2, B$

    For I = 0 To 13:

        Input #2, carStatPlyr$(I)

        For J = 0 To 13: 
            Input #2, carStatVal!(I, J)
        Next

        Input #2, careerW0!(I)
        Input #2, careerW1!(I)

        Color 5
        Locate 21, 38
        TMR1% = TMR1% - 1
        Print Using "####"; TMR1%

    Next

    For L = 0 To 15: 
        Input #2, statsOffTot!(L)
    Next

    For L = 0 To 15: 
        Input #2, statsDefTot!(L)
    Next

    Close #2

    For I = 0 To 13

        RR = 0
        CHK = 0

        For J = 0 To 61

            If careerPlayers$(J) = carStatPlyr$(I) Then

                'Find next stat year to save
                For W = 1 To MAX_CAREER_YEARS
                    If carPlyrStat!(W, J, 15) = targetYear Then
                        CHK = CHK + 1
                    End If
                Next W

                'Look at number of years from previous
                For Z = 1 To MAX_CAREER_YEARS
                    N = N + carPlyrStat!(Z, J, 14)
                Next Z

                If CHK = 0 Then
                    '1st stat year
                    N = N + 1
                End If

                For K = 0 To 13
                    carPlyrStat!(N, J, K) = carStatVal!(I, K)
                Next K

                carPlyrStat!(N, J, 14) = 1
                carPlyrStat!(N, J, 15) = targetYear
                car3FGM!(N, J) = careerW0!(J)
                car3FGA!(N, J) = careerW1!(J)

            Else

                RR = RR + 1

                If RR = 62 Then

                    If carStatPlyr$(I) <> "END" And carStatPlyr$(I) <> "XXX" And carStatPlyr$(I) <> "" Then

                        NB = NB + 1
                        careerPlayers$(NB) = carStatPlyr$(I)
                        N = 1

                        For K = 0 To 13: 
                            carPlyrStat!(N, NB, K) = carStatVal!(I, K)
                        Next K

                        carPlyrStat!(N, NB, 14) = 1
                        carPlyrStat!(N, NB, 15) = targetYear
                        car3FGM!(N, NB) = careerW0!(NB)
                        car3FGA!(N, NB) = careerW1!(NB)
                    End If

                End If

            End If 'Done comparing careerPlayers$() to carStatPlyr$()

        Next J

    Next I

    If doPostLeaders = 1 Then Call UpdateCareerRecords(careerFile$)

    Call SaveCareerFile (careerFile$)

End Sub


'----------------------------------------
'      LoadCareerFile Subroutine
'----------------------------------------
'This routine loads the existing career
'file data into arrays.
Sub LoadCareerFile (loadFile$)

    Shared NB, NP

    Shared careerPlayers$()

    Shared carPlyrStat!(), car3FGM!(), car3FGA!()

    Color 15, 0
    Cls
    
    Locate 12, 10: Print "LOADING CAREER FILE . . . . PLEASE WAIT"

    TMR = 15 * 62
    
    Open diskPaths$(1) + loadFile$ + ".CAR" For Input As #1

    Input #1, NB, NP

    For N = 1 To MAX_CAREER_YEARS

        For I = 0 To 61

            Input #1, careerPlayers$(I)

            For J = 0 To 15
                Input #1, carPlyrStat!(N, I, J)
            Next J

            Input #1, car3FGM!(N, I)
            Input #1, car3FGA!(N, I)

            TMR = TMR - 1

            Color 5
            Locate 21, 38
            Print Using "####"; TMR

        Next I

    Next N

    Close #1

End Sub


'----------------------------------------
'      SaveCareerFile Subroutine
'----------------------------------------
'This routine updates the existing career
'file with the modified stats values.
Sub SaveCareerFile (saveFile$)

    Shared NB, NP

    Shared careerPlayers$()

    Shared carPlyrStat!(), car3FGM!(), car3FGA!()

    Cls
    Color 15
    Locate 12, 6: Print "SAVING UPDATED CAREER FILE . . . PLEASE WAIT"

	TMR = 15 * 62

    Open diskPaths$(1) + saveFile$ + ".CAR" For Output As #3

    Write #3, NB, NP

    For N = 1 To MAX_CAREER_YEARS

        For I = 0 To 61

            Write #3, careerPlayers$(I)

            For J = 0 To 15
                Write #3, carPlyrStat!(N, I, J)
            Next J

            Write #3, car3FGM!(N, I)
            Write #3, car3FGA!(N, I)

            Color 5
            Locate 21, 38

            TMR = TMR - 1
            Print Using "####"; TMR

        Next I

    Next N

    Close #3

End Sub


'----------------------------------------
'     UpdateCareerRecords Subroutine
'----------------------------------------
'This routine reads in any existing career 
'leader files (.CLD), adds the new stats,
'and then saves the changes back to the
'career leader file (.CLD)
Sub UpdateCareerRecords (careerFile$)

    Dim carRecVal!(21), carRecYear!(21)
    Dim BL$(21)

    Shared careerPlayers$()
    Shared carPlyrStat!(), car3FGM!(), car3FGA!()

    If _FileExists(diskPaths$(2) + careerFile$ + ".CLD") Then

        Open diskPaths$(2) + careerFile$ + ".CLD" For Input As #1

        For I = 0 To 20
            Input #1, carRecYear!(I), BL$(I), carRecVal!(I)
        Next

        Close #1

    End If

    For I = 0 To 61

        If careerPlayers$(I) <> "" And careerPlayers$(I) <> "XXX" And careerPlayers$(I) <> "END" Then

            For J = 1 To MAX_CAREER_YEARS

                If carPlyrStat!(J, I, 15) <> 0 Then

                    If carPlyrStat!(J, I, 1) > carRecVal!(0) Then
                        carRecVal!(0) = carPlyrStat!(J, I, 1)
                        BL$(0) = careerPlayers$(I)
                        carRecYear!(0) = carPlyrStat!(J, I, 15)
                    End If

                    If carPlyrStat!(J, I, 2) > carRecVal!(1) Then
                        carRecVal!(1) = carPlyrStat!(J, I, 2)
                        BL$(1) = careerPlayers$(I)
                        carRecYear!(1) = carPlyrStat!(J, I, 15)
                    End If

                    If carPlyrStat!(J, I, 1) <> 0 And carPlyrStat!(J, I, 1) / carPlyrStat!(J, I, 0) >= 3.65 Then
                        If carPlyrStat!(J, I, 2) / carPlyrStat!(J, I, 1) > carRecVal!(2) Then
                            carRecVal!(2) = carPlyrStat!(J, I, 2) / carPlyrStat!(J, I, 1)
                            BL$(2) = careerPlayers$(I)
                            carRecYear!(2) = carPlyrStat!(J, I, 15)
                        End If
                    End If

                    If carPlyrStat!(J, I, 4) > carRecVal!(3) Then
                        carRecVal!(3) = carPlyrStat!(J, I, 4)
                        BL$(3) = careerPlayers$(I)
                        carRecYear!(3) = carPlyrStat!(J, I, 15)
                    End If

                    If carPlyrStat!(J, I, 3) > carRecVal!(4) Then
                        carRecVal!(4) = carPlyrStat!(J, I, 3)
                        BL$(4) = careerPlayers$(I)
                        carRecYear!(4) = carPlyrStat!(J, I, 15)
                    End If

                    If carPlyrStat!(J, I, 3) <> 0 And carPlyrStat!(J, I, 3) / carPlyrStat!(J, I, 0) >= 1.5 Then
                        If carPlyrStat!(J, I, 4) / carPlyrStat!(J, I, 3) > carRecVal!(5) Then
                            carRecVal!(5) = carPlyrStat!(J, I, 4) / carPlyrStat!(J, I, 3)
                            BL$(5) = careerPlayers$(I)
                            carRecYear!(5) = carPlyrStat!(J, I, 15)
                        End If
                    End If

                    If car3FGM!(J, I) > carRecVal!(6) Then
                        carRecVal!(6) = car3FGM!(J, I)
                        BL$(6) = careerPlayers$(I)
                        carRecYear!(6) = carPlyrStat!(J, I, 15)
                    End If

                    If car3FGA!(J, I) > carRecVal!(7) Then
                        carRecVal!(7) = car3FGA!(J, I)
                        BL$(7) = careerPlayers$(I)
                        carRecYear!(7) = carPlyrStat!(J, I, 15)
                    End If

                    If car3FGA!(J, I) <> 0 And car3FGA!(J, I) / carPlyrStat!(J, I, 0) >= 0.3 Then
                        If car3FGM!(J, I) / car3FGA!(J, I) > carRecVal!(8) Then
                            carRecVal!(8) = car3FGM!(J, I) / car3FGA!(J, I)
                            BL$(8) = careerPlayers$(I)
                            carRecYear!(8) = carPlyrStat!(J, I, 15)
                        End If
                    End If

                    If carPlyrStat!(J, I, 2) * 2 + carPlyrStat!(J, I, 4) + car3FGM!(J, I) > carRecVal!(9) Then
                        carRecVal!(9) = carPlyrStat!(J, I, 2) * 2 + carPlyrStat!(J, I, 4) + car3FGM!(J, I)
                        BL$(9) = careerPlayers$(I)
                        carRecYear!(9) = carPlyrStat!(J, I, 15)
                    End If

                    If carPlyrStat!(J, I, 0) <> 0 Then
                        If (carPlyrStat!(J, I, 2) * 2 + carPlyrStat!(J, I, 4) + car3FGM!(J, I)) / carPlyrStat!(J, I, 0) > carRecVal!(10) Then
                            carRecVal!(10) = (carPlyrStat!(J, I, 2) * 2 + carPlyrStat!(J, I, 4) + car3FGM!(J, I)) / carPlyrStat!(J, I, 0)
                            BL$(10) = careerPlayers$(I)
                            carRecYear!(10) = carPlyrStat!(J, I, 15)
                        End If
                    End If

                    If carPlyrStat!(J, I, 12) > carRecVal!(11) Then
                        carRecVal!(11) = carPlyrStat!(J, I, 12)
                        BL$(11) = careerPlayers$(I)
                        carRecYear!(11) = carPlyrStat!(J, I, 15)
                    End If

                    If carPlyrStat!(J, I, 5) - carPlyrStat!(J, I, 12) > carRecVal!(12) Then
                        carRecVal!(12) = carPlyrStat!(J, I, 5) - carPlyrStat!(J, I, 12)
                        BL$(12) = careerPlayers$(I)
                        carRecYear!(12) = carPlyrStat!(J, I, 15)
                    End If

                    If carPlyrStat!(J, I, 5) > carRecVal!(13) Then
                        carRecVal!(13) = carPlyrStat!(J, I, 5)
                        BL$(13) = careerPlayers$(I)
                        carRecYear!(13) = carPlyrStat!(J, I, 15)
                    End If

                    If carPlyrStat!(J, I, 0) <> 0 Then
                        If carPlyrStat!(J, I, 5) / carPlyrStat!(J, I, 0) > carRecVal!(14) Then
                            carRecVal!(14) = carPlyrStat!(J, I, 5) / carPlyrStat!(J, I, 0)
                            BL$(14) = careerPlayers$(I)
                            carRecYear!(14) = carPlyrStat!(J, I, 15)
                        End If
                    End If

                    If carPlyrStat!(J, I, 6) > carRecVal!(15) Then
                        carRecVal!(15) = carPlyrStat!(J, I, 6)
                        BL$(15) = careerPlayers$(I)
                        carRecYear!(15) = carPlyrStat!(J, I, 15)
                    End If

                    If carPlyrStat!(J, I, 8) > carRecVal!(16) Then
                        carRecVal!(16) = carPlyrStat!(J, I, 8)
                        BL$(16) = careerPlayers$(I)
                        carRecYear!(16) = carPlyrStat!(J, I, 15)
                    End If

                    If carPlyrStat!(J, I, 9) > carRecVal!(17) Then
                        carRecVal!(17) = carPlyrStat!(J, I, 9)
                        BL$(17) = careerPlayers$(I)
                        carRecYear!(17) = carPlyrStat!(J, I, 15)
                    End If

                    If carPlyrStat!(J, I, 7) > carRecVal!(18) Then
                        carRecVal!(18) = carPlyrStat!(J, I, 7)
                        BL$(18) = careerPlayers$(I)
                        carRecYear!(18) = carPlyrStat!(J, I, 15)
                    End If

                    If carPlyrStat!(J, I, 11) > carRecVal!(19) Then
                        carRecVal!(19) = carPlyrStat!(J, I, 11)
                        BL$(19) = careerPlayers$(I)
                        carRecYear!(19) = carPlyrStat!(J, I, 15)
                    End If

                    If carPlyrStat!(J, I, 13) > carRecVal!(20) Then
                        carRecVal!(20) = carPlyrStat!(J, I, 13)
                        BL$(20) = careerPlayers$(I)
                        carRecYear!(20) = carPlyrStat!(J, I, 15)
                    End If

                End If

            Next J

        End If

    Next I

    Open diskPaths$(2) + careerFile$ + ".CLD" For Output As #2

    For I = 0 To 20
        Write #2, carRecYear!(I), BL$(I), carRecVal!(I)
    Next

    Close #2

End Sub
