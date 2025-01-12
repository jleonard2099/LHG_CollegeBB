'----------------------------------------
'        Records_Menu Subroutine
'----------------------------------------
'This subroutine gives the user a number of
'menu selections for viewing, updating and
'printing records
Sub Records_Menu ()

    _Title "Courtside College Basketball - Analyze Records"

    ReDim menuChoices$(1 To 7)

    Shared indRecordDesc$(), teamRecordDesc$()
    Shared plyrRecDesc$(), offRecDesc$(), defRecDesc$()
    Shared plyrRecords!(), offRecords!(), defRecords!()

    Do

        Cls

        Call TITLE

        menuChoices$(1) = "VIEW SEASON RECORDS      "
        menuChoices$(2) = "PRINT SEASON RECORDS     "
        menuChoices$(3) = "DELETE SEASON RECORDS    "
        menuChoices$(4) = "VIEW SINGLE GAME RECORDS "
        menuChoices$(5) = "PRINT SINGLE GAME RECORDS"
        menuChoices$(6) = "DELETE GAME RECORDS      "
        menuChoices$(7) = "RETURN TO GAME MENU      "
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

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            If Not _FileExists(diskPaths$(1) + "RECORDS.CBB") Then

                MsgText$ = "UNABLE TO LOCATE RECORDS PRESS ANY KEY TO RETURN TO MAIN MENU"
                BoxWidth = 24
                BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                Call CSMsgBox(MsgText$, BoxWidth, PASS)
                I$ = GetKeyPress$
                Call CSMsgBox("", 0, PASS)

            Else

                Call ReadSeasonRecords("RECORDS.CBB")

                Select Case SELECTION%

                        Case 1:
                            Call ViewSeasonRecords

                        Case 2:
                            userKey$ = GetPrintOptions$

                            If userKey$ <> Chr$(27) Then

                                Select Case userKey$

                                    Case "F"
                                        userFile$ = _SaveFileDialog$("Save Records Report", _CWD$ + "\Records", "*.txt", "Text File")
                                        If userFile$ <> "" Then
                                            Call SeasonRecords_Report(0, userFile$)
                                        End If

                                    Case "P"
                                        Call SeasonRecords_Report(1, "RECORDS")

                                End Select

                            End If

                        Case 3:
                            If _FileExists(diskPaths$(1) + "RECORDS.CBB") Then
                                Kill diskPaths$(1) + "RECORDS.CBB"
                            End If

                        Case 4:
                            Call GameRecords(1)

                        Case 5:
                            Call GameRecords(2)

                        Case 6:
                            If _FileExists(diskPaths$(1) + "RECORDS.GAM") Then
                                Kill diskPaths$(1) + "RECORDS.GAM"
                            End If

                End Select
                
            End If 'Done checking for record file

        End If 'Done checking for user ESC

    Loop Until SELECTION% = 7 Or ExitCode% = 27

    Cls

End Sub

'----------------------------------------
'       ViewSeasonRecords Subroutine
'----------------------------------------
'This subroutine assists the user with
'viewing full season records.
Sub ViewSeasonRecords ()

    Shared teamRecordDesc$(), indRecordDesc$()
    Shared plyrRecDesc$(), offRecDesc$(), defRecDesc$()
    Shared plyrRecords!(), offRecords!(), defRecords!()

        Call ReadSeasonRecords("RECORDS.CBB")

        Color 0, 0
        Cls

        Call ShadowBox(1, 1, 73, 20, 15, 4)

        Locate 2, 23
        Print "******* INDIVIDUAL PLAYER RECORDS *******"
        Print
        Locate , 4
        Print "CATEGORY                PLAYER           TEAM            RECORD"

        Color 7
        'Games Played
        Locate , 4: Print indRecordDesc$(1); Tab(28); Left$(plyrRecDesc$(1, 0), 14); Tab(42); plyrRecDesc$(1, 1); Tab(62); Using "####"; plyrRecords!(1)
        'Field Goal Attempts
        Locate , 4: Print indRecordDesc$(2); Tab(28); Left$(plyrRecDesc$(2, 0), 14); Tab(42); plyrRecDesc$(2, 1); Tab(62); Using "####"; plyrRecords!(2)
        'Field Goals Made
        Locate , 4: Print indRecordDesc$(3); Tab(28); Left$(plyrRecDesc$(3, 0), 14); Tab(42); plyrRecDesc$(3, 1); Tab(62); Using "####"; plyrRecords!(3)
        'Field Goal %
        Locate , 4: Print indRecordDesc$(4); Tab(28); Left$(plyrRecDesc$(4, 0), 14); Tab(42); plyrRecDesc$(4, 1); Tab(62); Using "##.#"; plyrRecords!(4) * 100
        '3 Field Goal Attempts
        Locate , 4: Print indRecordDesc$(5); Tab(28); Left$(plyrRecDesc$(5, 0), 14); Tab(42); plyrRecDesc$(5, 1); Tab(62); Using "####"; plyrRecords!(5)
        '3 Field Goal Made
        Locate , 4: Print indRecordDesc$(6); Tab(28); Left$(plyrRecDesc$(6, 0), 14); Tab(42); plyrRecDesc$(6, 1); Tab(62); Using "####"; plyrRecords!(6)
        '3 Field Goal %
        Locate , 4: Print indRecordDesc$(7); Tab(28); Left$(plyrRecDesc$(7, 0), 14); Tab(42); plyrRecDesc$(7, 1); Tab(62); Using "##.#"; plyrRecords!(7) * 100
        'Free Throw Attempts
        Locate , 4: Print indRecordDesc$(8); Tab(28); Left$(plyrRecDesc$(8, 0), 14); Tab(42); plyrRecDesc$(8, 1); Tab(62); Using "####"; plyrRecords!(8)
        'Free Throws Made
        Locate , 4: Print indRecordDesc$(9); Tab(28); Left$(plyrRecDesc$(9, 0), 14); Tab(42); plyrRecDesc$(9, 1); Tab(62); Using "####"; plyrRecords!(9)
        'Free Throw %
        Locate , 4: Print indRecordDesc$(10); Tab(28); Left$(plyrRecDesc$(10, 0), 14); Tab(42); plyrRecDesc$(10, 1); Tab(62); Using "##.#"; plyrRecords!(10) * 100
        'Minutes Played
        Locate , 4: Print indRecordDesc$(11); Tab(28); Left$(plyrRecDesc$(11, 0), 14); Tab(42); plyrRecDesc$(11, 1); Tab(62); Using "####"; plyrRecords!(11)
        'Points
        Locate , 4: Print indRecordDesc$(12); Tab(28); Left$(plyrRecDesc$(12, 0), 14); Tab(42); plyrRecDesc$(12, 1); Tab(62); Using "####"; plyrRecords!(12)
        'Scoring Average
        Locate , 4: Print indRecordDesc$(13); Tab(28); Left$(plyrRecDesc$(13, 0), 14); Tab(42); plyrRecDesc$(13, 1); Tab(62); Using "####"; plyrRecords!(13)

        Color 15
        Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$

        For I = 4 To 21: Locate I, 4
            Print Space$(72)
        Next

        Color 0, 0
        Cls

        Call ShadowBox(1, 1, 73, 20, 15, 4)

        Locate 2, 23
        Print "******* INDIVIDUAL PLAYER RECORDS *******"
        Print
        Locate , 4
        Print "CATEGORY                PLAYER           TEAM            RECORD"

        Color 7

        Locate , 4: Print indRecordDesc$(14); Tab(28); Left$(plyrRecDesc$(14, 0), 14); Tab(42); plyrRecDesc$(14, 1); Tab(62); Using "####"; plyrRecords!(14)
        Locate , 4: Print indRecordDesc$(15); Tab(28); Left$(plyrRecDesc$(15, 0), 14); Tab(42); plyrRecDesc$(15, 1); Tab(62); Using "####"; plyrRecords!(15)
        Locate , 4: Print indRecordDesc$(16); Tab(28); Left$(plyrRecDesc$(16, 0), 14); Tab(42); plyrRecDesc$(16, 1); Tab(62); Using "####"; plyrRecords!(16)
        Locate , 4: Print indRecordDesc$(17); Tab(28); Left$(plyrRecDesc$(17, 0), 14); Tab(42); plyrRecDesc$(17, 1); Tab(62); Using "####"; plyrRecords!(17)
        Locate , 4: Print indRecordDesc$(18); Tab(28); Left$(plyrRecDesc$(18, 0), 14); Tab(42); plyrRecDesc$(18, 1); Tab(62); Using "##.#"; plyrRecords!(18)
        Locate , 4: Print indRecordDesc$(19); Tab(28); Left$(plyrRecDesc$(19, 0), 14); Tab(42); plyrRecDesc$(19, 1); Tab(62); Using "####"; plyrRecords!(19)
        Locate , 4: Print indRecordDesc$(20); Tab(28); Left$(plyrRecDesc$(20, 0), 14); Tab(42); plyrRecDesc$(20, 1); Tab(62); Using "####"; plyrRecords!(20)
        Locate , 4: Print indRecordDesc$(21); Tab(28); Left$(plyrRecDesc$(21, 0), 14); Tab(42); plyrRecDesc$(21, 1); Tab(62); Using "####"; plyrRecords!(21)
        Locate , 4: Print indRecordDesc$(22); Tab(28); Left$(plyrRecDesc$(22, 0), 14); Tab(42); plyrRecDesc$(22, 1); Tab(62); Using "####"; plyrRecords!(22)
        Locate , 4: Print indRecordDesc$(23); Tab(28); Left$(plyrRecDesc$(23, 0), 14); Tab(42); plyrRecDesc$(23, 1); Tab(62); Using "####"; plyrRecords!(23)
        Locate , 4: Print indRecordDesc$(24); Tab(28); Left$(plyrRecDesc$(24, 0), 14); Tab(42); plyrRecDesc$(24, 1); Tab(62); Using "##.#"; plyrRecords!(24)
        Locate , 4: Print indRecordDesc$(25); Tab(28); Left$(plyrRecDesc$(25, 0), 14); Tab(42); plyrRecDesc$(25, 1); Tab(62); Using "####"; plyrRecords!(25)

        Color 15
        Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$

        For I = 4 To 21: Locate I, 4
            Print Space$(72)
        Next

        Color 0, 0
        Cls

        Call ShadowBox(1, 1, 73, 20, 15, 1)

        Locate 2, 23
        Print "*******  OFFENSIVE TEAM RECORDS *******"
        Print
        Locate , 4
        Print "CATEGORY                        TEAM             RECORD            "

        Color 14
        'Field Goal Attempts
        Locate , 4: Print teamRecordDesc$(1); Tab(33); offRecDesc$(1); Tab(53); Using "####"; offRecords!(1)
        'Field Goals Made
        Locate , 4: Print teamRecordDesc$(2); Tab(33); offRecDesc$(2); Tab(53); Using "####"; offRecords!(2)
        'Field Goal %
        Locate , 4: Print teamRecordDesc$(3); Tab(33); offRecDesc$(3); Tab(53); Using "##.#"; offRecords!(3) * 100
        '3 Field Goal Attempts
        Locate , 4: Print teamRecordDesc$(4); Tab(33); offRecDesc$(4); Tab(53); Using "####"; offRecords!(4)
        '3 Field Goals Made
        Locate , 4: Print teamRecordDesc$(5); Tab(33); offRecDesc$(5); Tab(53); Using "####"; offRecords!(5)
        '3 Field Goal %
        Locate , 4: Print teamRecordDesc$(6); Tab(33); offRecDesc$(6); Tab(53); Using "##.#"; offRecords!(6) * 100
        'Free Throw Attempts
        Locate , 4: Print teamRecordDesc$(7); Tab(33); offRecDesc$(7); Tab(53); Using "####"; offRecords!(7)
        'Free Throws Made
        Locate , 4: Print teamRecordDesc$(8); Tab(33); offRecDesc$(8); Tab(53); Using "####"; offRecords!(8)
        'Free Throw %
        Locate , 4: Print teamRecordDesc$(9); Tab(33); offRecDesc$(9); Tab(53); Using "##.#"; offRecords!(9) * 100

        Locate , 4: Print teamRecordDesc$(10); Tab(33); offRecDesc$(10); Tab(53); Using "####"; offRecords!(10)
        Locate , 4: Print teamRecordDesc$(11); Tab(33); offRecDesc$(11); Tab(53); Using "####"; offRecords!(11)
        Locate , 4: Print teamRecordDesc$(12); Tab(33); offRecDesc$(12); Tab(53); Using "####"; offRecords!(12)
        Locate , 4: Print teamRecordDesc$(13); Tab(33); offRecDesc$(13); Tab(53); Using "##.#"; offRecords!(13)
        Locate , 4: Print teamRecordDesc$(14); Tab(33); offRecDesc$(14); Tab(53); Using "####"; offRecords!(14)
        Locate , 4: Print teamRecordDesc$(15); Tab(33); offRecDesc$(15); Tab(53); Using "####"; offRecords!(15)

        Color 15
        Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$

        For I = 4 To 21:
            Locate I, 4
            Print Space$(72)
        Next

        Color 0, 0
        Cls

        Call ShadowBox(1, 1, 73, 20, 15, 1)

        Locate 2, 23
        Print "*******  OFFENSIVE TEAM RECORDS *******"
        Print
        Locate , 4
        Print "CATEGORY                        TEAM             RECORD            "

        Color 14
        Locate , 4: Print teamRecordDesc$(16); Tab(33); offRecDesc$(16); Tab(53); Using "####"; offRecords!(16)
        Locate , 4: Print teamRecordDesc$(17); Tab(33); offRecDesc$(17); Tab(53); Using "####"; offRecords!(17)
        Locate , 4: Print teamRecordDesc$(18); Tab(33); offRecDesc$(18); Tab(53); Using "####"; offRecords!(18)
        Locate , 4: Print teamRecordDesc$(19); Tab(33); offRecDesc$(19); Tab(52); Using "#####"; offRecords!(19)
        Locate , 4: Print teamRecordDesc$(20); Tab(33); offRecDesc$(20); Tab(52); Using "###.#"; offRecords!(20)
        Locate , 4: Print "WINS"; Tab(33); offRecDesc$(21); Tab(53); Using "####"; offRecords!(21)

        Color 15
        Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$

        For I = 4 To 21:
            Locate I, 4
            Print Space$(72)
        Next

        Color 0, 0
        Cls

        Call ShadowBox(1, 1, 73, 20, 15, 4)

        Color 14
        Locate 2, 23
        Print "*******  DEFENSIVE TEAM RECORDS *******"
        Print
        Locate , 4
        Print "CATEGORY                        TEAM             RECORD            "

        Color 15
        'Field Goal Attempts
        Locate , 4: Print teamRecordDesc$(1); Tab(33); defRecDesc$(1); Tab(53); Using "####"; defRecords!(1)
        'Field Goals Made
        Locate , 4: Print teamRecordDesc$(2); Tab(33); defRecDesc$(2); Tab(53); Using "####"; defRecords!(2)
        'Field Goal %
        Locate , 4: Print teamRecordDesc$(3); Tab(33); defRecDesc$(3); Tab(53); Using "##.#"; defRecords!(3) * 100
        '3 Field Goal Attempts
        Locate , 4: Print teamRecordDesc$(4); Tab(33); defRecDesc$(4); Tab(53); Using "####"; defRecords!(4)
        '3 Field Goals Made
        Locate , 4: Print teamRecordDesc$(5); Tab(33); defRecDesc$(5); Tab(53); Using "####"; defRecords!(5)
        '3 Field Goal %
        Locate , 4: Print teamRecordDesc$(6); Tab(33); defRecDesc$(6); Tab(53); Using "##.#"; defRecords!(6) * 100
        'Free Throw Attempts
        Locate , 4: Print teamRecordDesc$(7); Tab(33); defRecDesc$(7); Tab(53); Using "####"; defRecords!(7)
        'Free Throws Made
        Locate , 4: Print teamRecordDesc$(8); Tab(33); defRecDesc$(8); Tab(53); Using "####"; defRecords!(8)
        'Free Throw %
        Locate , 4: Print teamRecordDesc$(9); Tab(33); defRecDesc$(9); Tab(53); Using "##.#"; defRecords!(9) * 100

        Locate , 4: Print teamRecordDesc$(10); Tab(33); defRecDesc$(10); Tab(53); Using "####"; defRecords!(10)
        Locate , 4: Print teamRecordDesc$(11); Tab(33); defRecDesc$(11); Tab(53); Using "####"; defRecords!(11)
        Locate , 4: Print teamRecordDesc$(12); Tab(33); defRecDesc$(12); Tab(53); Using "####"; defRecords!(12)
        Locate , 4: Print teamRecordDesc$(13); Tab(33); defRecDesc$(13); Tab(53); Using "##.#"; defRecords!(13)
        Locate , 4: Print teamRecordDesc$(14); Tab(33); defRecDesc$(14); Tab(53); Using "####"; defRecords!(14)
        Locate , 4: Print teamRecordDesc$(15); Tab(33); defRecDesc$(15); Tab(53); Using "####"; defRecords!(15)

        Color 15
        Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$

        For I = 4 To 21: Locate I, 4
            Print Space$(72)
        Next

        Color 0, 0
        Cls

        Call ShadowBox(1, 1, 73, 20, 15, 4)

        Color 14
        Locate 2, 23: Print "*******  DEFENSIVE TEAM RECORDS *******"
        Print
        Locate , 4: Print "CATEGORY                        TEAM             RECORD            "

        Color 15

        Locate , 4: Print teamRecordDesc$(16); Tab(33); defRecDesc$(16); Tab(53); Using "####"; defRecords!(16)
        Locate , 4: Print teamRecordDesc$(17); Tab(33); defRecDesc$(17); Tab(53); Using "####"; defRecords!(17)
        Locate , 4: Print teamRecordDesc$(18); Tab(33); defRecDesc$(18); Tab(53); Using "####"; defRecords!(18)
        Locate , 4: Print teamRecordDesc$(19); Tab(33); defRecDesc$(19); Tab(52); Using "#####"; defRecords!(19)
        Locate , 4: Print teamRecordDesc$(20); Tab(33); defRecDesc$(20); Tab(52); Using "###.#"; defRecords!(20)
        Locate , 4: Print "LOSSES"; Tab(33); defRecDesc$(21); Tab(53); Using "####"; defRecords!(21)

        Color 15
        Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$

        For I = 4 To 21: Locate I, 4
            Print Space$(72)
        Next

End Sub

'----------------------------------------
'     SeasonRecords_Report Subroutine
'----------------------------------------
'This subroutine facilitates formatting
'of the record data to a file, that is
'subsequently either saved, or printed.
Sub SeasonRecords_Report (printDest, destFile$)

    Shared teamRecordDesc$(), indRecordDesc$()
    Shared plyrRecDesc$(), offRecDesc$(), defRecDesc$()
    Shared plyrRecords!(), offRecords!(), defRecords!()

        Color 15, 0
        Cls

        Open destFile$ For Output As #1

        Print #1, Tab(23); "******* INDIVIDUAL PLAYER RECORDS *******"
        Print #1,
        Print #1, "CATEGORY                PLAYER           TEAM            RECORD"

        For I = 1 To 3
            Print #1, indRecordDesc$(I); Tab(28); plyrRecDesc$(I, 0); Tab(42); plyrRecDesc$(I, 1); Tab(62); Using "####"; plyrRecords!(I)
        Next I

        Print #1, indRecordDesc$(4); Tab(28); plyrRecDesc$(4, 0); Tab(42); plyrRecDesc$(4, 1); Tab(62); Using "##.#"; plyrRecords!(4) * 100

        For I = 5 To 6
            Print #1, indRecordDesc$(I); Tab(28); plyrRecDesc$(I, 0); Tab(42); plyrRecDesc$(I, 1); Tab(62); Using "####"; plyrRecords!(I)
        Next I

        Print #1, indRecordDesc$(7); Tab(28); plyrRecDesc$(7, 0); Tab(42); plyrRecDesc$(7, 1); Tab(62); Using "##.#"; plyrRecords!(7) * 100

        For I = 8 To 9
            Print #1, indRecordDesc$(I); Tab(28); plyrRecDesc$(I, 0); Tab(42); plyrRecDesc$(I, 1); Tab(62); Using "####"; plyrRecords!(I)
        Next I

        Print #1, indRecordDesc$(10); Tab(28); plyrRecDesc$(10, 0); Tab(42); plyrRecDesc$(10, 1); Tab(62); Using "##.#"; plyrRecords!(10) * 100

        For I = 11 To 17
            Print #1, indRecordDesc$(I); Tab(28); plyrRecDesc$(I, 0); Tab(42); plyrRecDesc$(I, 1); Tab(62); Using "####"; plyrRecords!(I)
        Next I

        Print #1, indRecordDesc$(18); Tab(28); plyrRecDesc$(18, 0); Tab(42); plyrRecDesc$(18, 1); Tab(62); Using "##.#"; plyrRecords!(18)

        For I = 19 To 23
            Print #1, indRecordDesc$(I); Tab(28); plyrRecDesc$(I, 0); Tab(42); plyrRecDesc$(I, 1); Tab(62); Using "####"; plyrRecords!(I)
        Next I

        Print #1, indRecordDesc$(24); Tab(28); plyrRecDesc$(24, 0); Tab(42); plyrRecDesc$(24, 1); Tab(62); Using "##.#"; plyrRecords!(24)

        Print #1, indRecordDesc$(25); Tab(28); plyrRecDesc$(25, 0); Tab(42); plyrRecDesc$(25, 1); Tab(62); Using "####"; plyrRecords!(25)

        Print #1,
        Print #1, Tab(23); "*******  OFFENSIVE TEAM RECORDS *******"
        Print #1,
        Print #1, "CATEGORY                        TEAM             RECORD            "

        For I = 1 To 2
            Print #1, teamRecordDesc$(I); Tab(33); offRecDesc$(I); Tab(53); Using "####"; offRecords!(I)
        Next I

        Print #1, teamRecordDesc$(3); Tab(33); offRecDesc$(3); Tab(53); Using "##.#"; offRecords!(3) * 100

        For I = 4 To 5
            Print #1, teamRecordDesc$(I); Tab(33); offRecDesc$(I); Tab(53); Using "####"; offRecords!(I)
        Next I

        Print #1, teamRecordDesc$(6); Tab(33); offRecDesc$(6); Tab(53); Using "##.#"; offRecords!(6) * 100

        For I = 7 To 8
            Print #1, teamRecordDesc$(I); Tab(33); offRecDesc$(I); Tab(53); Using "####"; offRecords!(I)
        Next I

        Print #1, teamRecordDesc$(9); Tab(33); offRecDesc$(9); Tab(53); Using "##.#"; offRecords!(9) * 100

        For I = 10 To 12
            Print #1, teamRecordDesc$(I); Tab(33); offRecDesc$(I); Tab(53); Using "####"; offRecords!(I)
        Next I

        Print #1, teamRecordDesc$(13); Tab(33); offRecDesc$(13); Tab(53); Using "##.#"; offRecords!(13)

        For I = 14 To 18
            Print #1, teamRecordDesc$(I); Tab(33); offRecDesc$(I); Tab(53); Using "####"; offRecords!(I)
        Next I

        Print #1, teamRecordDesc$(19); Tab(33); offRecDesc$(19); Tab(52); Using "#####"; offRecords!(19)
        Print #1, teamRecordDesc$(20); Tab(33); offRecDesc$(20); Tab(52); Using "###.#"; offRecords!(20)

        Print #1, "WINS"; Tab(33); offRecDesc$(21); Tab(53); Using "####"; offRecords!(21)

        Print #1,
        Print #1, Tab(23); "*******  DEFENSIVE TEAM RECORDS *******"
        Print #1,
        Print #1, "CATEGORY                        TEAM             RECORD            "

        For I = 1 To 2
            Print #1, teamRecordDesc$(I); Tab(33); defRecDesc$(I); Tab(53); Using "####"; defRecords!(I)
        Next I

        Print #1, teamRecordDesc$(3); Tab(33); defRecDesc$(3); Tab(53); Using "##.#"; defRecords!(3) * 100

        For I = 4 To 5
            Print #1, teamRecordDesc$(I); Tab(33); defRecDesc$(I); Tab(53); Using "####"; defRecords!(I)
        Next I

        Print #1, teamRecordDesc$(6); Tab(33); defRecDesc$(6); Tab(53); Using "##.#"; defRecords!(6) * 100

        For I = 7 To 8
            Print #1, teamRecordDesc$(I); Tab(33); defRecDesc$(I); Tab(53); Using "####"; defRecords!(I)
        Next I

        Print #1, teamRecordDesc$(9); Tab(33); defRecDesc$(9); Tab(53); Using "##.#"; defRecords!(9) * 100

        For I = 10 To 12
            Print #1, teamRecordDesc$(I); Tab(33); defRecDesc$(I); Tab(53); Using "####"; defRecords!(I)
        Next I

        Print #1, teamRecordDesc$(13); Tab(33); defRecDesc$(13); Tab(53); Using "##.#"; defRecords!(13)

        For I = 14 To 18
            Print #1, teamRecordDesc$(I); Tab(33); defRecDesc$(I); Tab(53); Using "####"; defRecords!(I)
        Next I

        Print #1, teamRecordDesc$(19); Tab(33); defRecDesc$(19); Tab(52); Using "#####"; defRecords!(19)
        Print #1, teamRecordDesc$(20); Tab(33); defRecDesc$(20); Tab(52); Using "###.#"; defRecords!(20)

        Print #1, "LOSSES"; Tab(33); defRecDesc$(21); Tab(53); Using "####"; defRecords!(21)

        Close #1

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
