Sub BLANK ()

    _Title "Courtside College Basketball - Delete Conference Stats"

	'This should be redundant
    DefInt A-Z

    Dim H1$(MAX_CONFERENCES)

    Do
        'Display list of files...

        Cls

        Spec$ = diskPaths$(1) + "*.LIB"
        null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
        Count% = DirCount%

        If Count% = 0 Then
            Locate 10, 8
            Call CSMsgBox("NO CONFERENCE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)

        Else

            ReDim conferenceFiles$(0 To Count%)
            For X = 1 To Count%
                conferenceFiles$(X) = Space$(12)
            Next
            conferenceFiles$(0) = Spec$
            Call ReadFile(conferenceFiles$())

            'Select League

            Cls
            Color , 1
            Cls
            Locate , 28
            Color 14
            Print "SELECT CONFERENCE TO ERASE"
            Color 15
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
            Print "ENTER";
            Color 15
            Print "-SELECTS CONFERENCE";
            Color 14
            Print "HIT ESCAPE KEY TO EXIT";
            Color 15

            _MouseShow "DEFAULT"
            MAMenu conferenceFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
            _MouseHide

            If ExitCode% <> 27 Then

				Color 15, 1
                targetFile$ = conferenceFiles$(Selection%)
                Locate 2, 33: Print targetFile$

				Do
					Locate , 33: Print "ANY CHANGE (YN)";
					I$ = GetKeyPress$
				Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

				Color 15, 0

                If UCase$(I$) <> "Y" Then

                    targetFile$ = RTrim$(targetFile$)
                    WE$ = Left$(targetFile$, Len(targetFile$) - 4)
                    LH$ = Left$(WE$, 8)

					Color 7,0
                    Cls

                    Locate 10, 8
                    Call CSMsgBox("WARNING!!!::: THIS WILL ERASE ALL COMPILED STATS FOR THIS CONFERENCE. IF YOU STILL WISH TO ERASE ALL STATS, HIT 'Y' FOR YES", 26, Pass)
                    G$ = GetKeyPress$
                    Call CSMsgBox("", 0, Pass)

                    If UCase$(G$) = "Y" Then

                        Open diskPaths$(1) + LH$ + ".TMS" For Input As #2
                        Input #2, LG$, D1$, D2$ ', D3$, D4$
                        For XW = 1 To TEAMS_PER_CONFERENCE
                            'Need a check if we're at the end of the file
                            'This would be easiest if we just count the # of teams?
                            'But if the file is created with 40 names regardless...
                            'This should still work!!!
                            Input #2, H1$(XW)
                        Next
                        Close 2

                        For XW = 1 To TEAMS_PER_CONFERENCE
                            If H1$(XW) <> Chr$(42) Then
                                Print "DELETING "; H1$(XW); " STATS"
                                statFileName$ = diskPaths$(1) + H1$(XW)
                                If _FileExists(statFileName$) Then Kill statFileName$
                                If _FileExists(statFileName$ + "D") Then Kill statFileName$ + "D"
                                If _FileExists(statFileName$ + "R") Then Kill statFileName$ + "R"
                                If _FileExists(statFileName$ + "S") Then Kill statFileName$ + "S"
                            End If
                        Next

                    End If 'Done confirming to delete stats

                End If 'Done confirming file selection

            End If 'Done checking for ESC

        End If 'Done checking for file count

        '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0

End Sub
