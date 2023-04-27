Sub MERGE ()

    _Title "Courtside College Basketball - Merge Stats"

    ReDim menuChoices$(1 To 3)

    Do

        Color , 0

        Cls

        Call TITLE_MERGE

        menuChoices$(1) = "MERGE STATISTICAL FILES           "
        menuChoices$(2) = "ALIGN STATISTICAL FILES           "
        menuChoices$(3) = "EXIT TO MAIN MENU                 "
        Selection% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 11, 59, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1: Call MergeIt
                Case 2: Call ALIGN
                Case 3: Cls
            End Select
        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

End Sub

'----------------------------------------
'       MergeIt Subroutine
'----------------------------------------
'...explanation...
Sub MergeIt ()

    'Dim A$(14)
    Dim ARP$(14), mergeB$(26), mergeH$(100), HRP$(100)
    Dim mergeO$(100), ORP$(100), gameSite$(100), SITERP$(100)

    Dim mergeAP%(100, 1), APRD%(100, 1)
    Dim ORD%(100), TRD%(100)

    Dim mergeA(14, 14), ARD(14, 14), CRD(100), CRDRD(100)
    Dim W0RD(15), W1RD(15)

	Dim ZRD!(15), ZRD1!(15)

    Shared A$()
    Shared statsL%(), mergeO%(), mergeT%()
    Shared statsW0!(), statsW1!()
	Shared statsZ!(), statsZ1!()

	Do

		Color 12, 0
		Cls

		Locate 2, 5: Print "ENTER COMPLETE PATH FOR TEAM STATS TO MERGE"
		Color 15
		Locate 4, 5: Print "EXAMPLE - A:"
		Locate , 5: Print "          C:\HAFFNER"
		Locate , 5: Print "          C:\HAFFNER\1980"
		Locate , 5: Print "          D:\BASKET"

		For J = 10 To 13
			Locate J
			Print Space$(60)
		Next

		Do

			Color 10
			Locate 10, 5: Line Input "ROAD STATS:     "; PR$
			Locate , 5
			Locate , 5: Line Input "CURRENT STATS:  "; PC$

			Color 14
			
			Locate 13, 5: Print "ANY CHANGE (Y/N)"
			I$ = GetKeyPress$
			
        Loop Until UCase$(I$) = "N"

		'beginning of GetDiskID sub
		Do
			Color 15, 0
			Cls
			JJ = 6
			Locate 6, 8: Color 15, 4
			Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
			Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
			Locate 7 + JJ, 8
			Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";
			
			Locate 7, 32
			Locate , 12: Print "ENTER DISK ID: ": Print
			Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
			Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
			Print
			Locate , 12: Print "ESC TO RETURN TO MAIN MENU"

			I$ = GetKeyPress$

			If I$ = Chr$(27) Then Z$ = "END": Cls: Exit Sub

			If I$ = Chr$(13) Then
				Call GetTeamFile_MERGE(YN$, NTMS)
			Else
				Locate 7, 27: Print I$
				Locate 7, 28: Line Input NY$
				NY$ = I$ + NY$
				Color 15, 0

				If NY$ = "" And YN$ <> "" Then 
					Call GetTeamFile_MERGE(YN$, NTMS)
				Else
					YN$ = NY$
					Erase teamNames$
					Open diskPaths$(0) + "BASK." + YN$ For Random As #1 Len = 655
					Y = LOF(1)
					Field #1, 15 As Q$(0), 640 As X$
					For I1 = 1 To Y / 655
						Get #1, I1
						teamNames$(I1) = Q$(0)
						teamIndex%(I1) = I1
					Next
					Close 1
					NTMS = Y / 655

					Call GetTeamFile_MERGE(YN$, NTMS)
				End If

			End If

			A$ = RTrim$(Q$(0))

			If BO% = 1 Then
				BO% = 0
			Else
				Locate 2, 33
				Color mergeT%(0), mergeT%(1): Print A$

				Color 15, 4
				
				Do
					Locate 3, 33: Print "ANY CHANGE (YN)";
					I$ = GetKeyPress$
				Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

			End If
	
		Loop Until UCase$(I$) = "N"

		Color 15, 0

		'end of GetDiskID sub

		B$ = A$

		For XX = 4 To 11
			If Mid$(B$, XX, 1) = " " Or Mid$(B$, XX, 1) = "." Then Mid$(B$, XX, 1) = "_"
		Next XX

		For XX = 12 To 14
			If Mid$(B$, XX, 2) = "ST" Then Mid$(B$, 9, 1) = "_": Mid$(B$, 10, 2) = "ST"
		Next XX

		QQ = 8

		For XX = 11 To 4 Step -1
			If Mid$(B$, XX, 1) = "_" Then
				QQ = XX - 4
			Else
				Exit For
			End If
		Next XX

		Z$ = Mid$(B$, 4, QQ) + "." + Left$(B$, 2)

		Cls
		
		Locate 10, 15: Print "ROAD STATS ARE ON DRIVE "; PR$
		Locate , 15: Print "MASTER STATS ARE ON DRIVE "; PC$
		Locate , 15: Print "TEAM TO MERGE STATS FOR IS "; A$

		Do
			Locate 14, 15: Print "ANY CHANGE (YN)"
			I$ = GetKeyPress$
		Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

		If UCase$(I$) = "N" Then

			If Not _FileExists(PR$ + "\" + Z$) Then
				Cls
				Color 7
				Locate 2, 10: Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
				Print
				Locate , 10: Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$
			Else
			
				Open PR$ + "\" + Z$ For Input As #1
				Input #1, B$
				For I = 0 To 13: Input #1, ARP$(I)
				For J = 0 To 13: Input #1, ARD(I, J): Next: Input #1, W0RD(I): Input #1, W1RD(I): Next
				For L = 0 To 15: Input #1, ZRD!(L): Next
				For L = 0 To 15: Input #1, ZRD1!(L): Next
				Close #1


				Open PR$ + "\" + Z$ + "S" For Input As #1
				Input #1, YR%
				For I = 1 To 100: Input #1, ORD%(I): Next I
				For I = 1 To 100: Input #1, ORP$(I): Next I
				For I = 1 To 100: Input #1, TRD%(I): Next I
				For I = 1 To 100: Input #1, HRP$(I): Next I
				For I = 1 To 100
				For I1 = 0 To 1: Input #1, APRD%(I, I1): Next: Next
				For I = 1 To 100: Input #1, CRDRD(I): Next I
				For I = 1 To 100: Input #1, SITERP$(I): Next I
				Close #1

				Locate 21, 5: Color 3

				For J = 21 To 23
					Locate J
					Print Space$(64)
				Next

				If Not _FileExists(PC$ + "\" + Z$) Then
					Cls
					Locate 2, 10: Color 7: Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
					Print
					Locate , 10: Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$
				Else
					Open PC$ + "\" + Z$ For Input As #1
					Input #1, B$
					For I = 0 To 13: Input #1, A$(I)
					For J = 0 To 13: Input #1, mergeA(I, J): Next: Input #1, statsW0!(I): Input #1, statsW1!(I): Next
					For L = 0 To 15: Input #1, statsZ!(L): Next
					For L = 0 To 15: Input #1, statsZ1!(L): Next
					Close #1

					Open PR$ + "\" + Z$ + "S" For Input As #1
					Input #1, Y%
					For I = 1 To 100: Input #1, mergeO%(I): Next I
					For I = 1 To 100: Input #1, mergeO$(I): Next I
					For I = 1 To 100: Input #1, mergeT%(I): Next I
					For I = 1 To 100: Input #1, mergeH$(I): Next I
					For I = 1 To 100
					For I1 = 0 To 1: Input #1, mergeAP%(I, I1): Next: Next
					For I = 1 To 100: Input #1, CRD(I): Next I
					For I = 1 To 100: Input #1, gameSite$(I): Next I
					Close #1

					SEQ = 0

					For K = 0 To 13

						If ARP$(K) <> A$(K) Then
							SEQ = SEQ + 1
						End If

					Next

					If SEQ = 0 Then
						For K = 1 To YR%
							Y% = Y% + 1
							mergeO%(Y%) = ORD%(K)
							mergeO$(Y%) = ORP$(K)
							mergeT%(Y%) = TRD%(K)
							mergeH$(Y%) = HRP$(K)
							mergeAP%(Y%, 0) = APRD%(Y%, 0)
							mergeAP%(Y%, 1) = APRD%(Y%, 1)
							CRD(Y%) = CRDRD(Y%)
							gameSite$(Y%) = SITERP$(Y%)
						Next

						Locate 18, 20: Color 12: Print "MERGING STAT FILE AND SCHEDULE RESULTS . . . ."

						For I = 0 To 13
							For J = 0 To 12: mergeA(I, J) = mergeA(I, J) + ARD(I, J): Next J
							statsW0!(I) = statsW0!(I) + W0RD(I)
							statsW1!(I) = statsW1!(I) + W1RD(I)
							If mergeA(I, 13) < ARD(I, 13) Then mergeA(I, 13) = ARD(I, 13) Else mergeA(I, 13) = mergeA(I, 13)
						Next I

						For L = 0 To 15: statsZ!(L) = statsZ!(L) + ZRD!(L): statsZ1!(L) = statsZ1!(L) + ZRD1!(L): Next

						Open PC$ + "\" + Z$ For Output As #1
						Write #1, B$
						For I = 0 To 13: Write #1, A$(I)
						For J = 0 To 13: Write #1, mergeA(I, J): Next: Write #1, statsW0!(I): Write #1, statsW1!(I): Next
						For L = 0 To 15: Write #1, statsZ!(L): Next
						For L = 0 To 15: Write #1, statsZ1!(L): Next
						Close #1

						Open PC$ + "\" + Z$ + "S" For Output As #1
						Write #1, Y%
						For I = 1 To 100: Write #1, mergeO%(I): Next I
						For I = 1 To 100: Write #1, mergeO$(I): Next I
						For I = 1 To 100: Write #1, mergeT%(I): Next I
						For I = 1 To 100: Write #1, mergeH$(I): Next I
						For I = 1 To 100
							For I1 = 0 To 1: Write #1, mergeAP%(I, I1): Next
						Next
						For I = 1 To 100: Write #1, CRD(I): Next I
						For I = 1 To 100: Write #1, gameSite$(I): Next I

						Close #1

						Y = 0

						Locate 20, 20: Color 12: Print "RESULTS SUCCESSFULLY MERGED!!"

						For J = 13 To 23
							Locate J
							Print Space$(65)
						Next

					Else
						Cls
						Color 14
						Locate 5, 27: Print "PLAYERS NOT IN SEQUENCE !!" '; DUMMY$
						Color 15
						Locate 7, 20: Print "STATS WILL BE DISTORTED UNLESS THE ROAD"
						Locate , 19: Print "FILE LISTS THE PLAYERS IN THE SAME SEQUENCE"
						Locate , 24: Print "AS YOUR CURRENT PLAYER STAT FILE."
						Locate 11, 24: Color 12: Print "YOU MUST REALIGN ROAD STAT FILE OR"
						Locate , 31: Print "ENTER STATS MANUALLY."
						Locate 15, 29: Color 13: Line Input "HIT ENTER TO GO TO MENU"; DUMMY$

					End If 'Done checking SEQ value

				End If 'Done checking if home file exists

			End If 'Done checking if road file exists

		End If 'Done checking if ready to proceed

	Loop Until DUMMY$ <> "" Or UCase$(I$) = "N"

End Sub

'----------------------------------------
'       GetTeamFile_MERGE Subroutine
'----------------------------------------
'...explanation...
Sub GetTeamFile_MERGE (yearNumber$, numberTeams)

    Shared mergeT%(), statsL%()

    Selection% = 1
    Count% = numberTeams
    Start% = 1
    FileFGColr% = 15: FileBGColr% = 4
    HiLiteFGColr% = 0: HiLiteBGColr% = 7
    Rows% = 20
    Columns% = 4
    Spacing% = 6
    Row% = 4
    Column% = 1

    Color , 4: Cls: Locate , 33: Color 14
    Print "TEAM SELECTION": Color 15
    Locate 25, 1: Color 14: Print "ESC";: Color 15
    Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
    Print "ENTER";: Color 15: Print "-SELECTS TEAM";

	_MouseShow "DEFAULT"
	MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
	_MouseHide

	If ExitCode% = 27 Then BO% = 1: Exit Sub

    T1 = teamIndex%(Selection%)

    Open diskPaths$(0) + "BASK." + yearNumber$ For Random As #1 Len = 655
    Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 2 As Q$(16), 2 As Q$(17), 2 As Q$(18), 2 As Q$(19), 602 As X$
    For I = 0 To 13
        Field #1, 53 + I * 43 As X$, 15 As Q$(I * 15 + 20), 2 As Q$(I * 15 + 21), 2 As Q$(I * 15 + 22), 2 As Q$(I * 15 + 23), 2 As Q$(I * 15 + 24), 2 As Q$(I * 15 + 25), 2 As Q$(I * 15 + 26), 2 As Q$(I * 15 + 27), 2 As Q$(I * 15 + 28), 2 As Q$(I * 15 + 29), 2 As Q$(I * 15 + 30), 2 As Q$(I * 15 + 31), 2 As Q$(I * 15 + 32), 2 As Q$(I * 15 + 33), 2 As Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 As X$
    Next
    Get #1, T1

    staminaRating% = CVI(Q$(1)): defFGPctAdj% = CVI(Q$(2)): V1% = CVI(Q$(3)): V2% = CVI(Q$(4))
    For I = 0 To 4: statsL%(I) = CVI(Q$(I + 5)): Next
    For I = 0 To 9: mergeT%(I) = CVI(Q$(I + 10)): Next
    Close 1

End Sub
