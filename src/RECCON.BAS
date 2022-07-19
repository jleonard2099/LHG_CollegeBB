SUB RECCON ()

	_Title "Courtside College Basketball - Manage Records"

	Shared BRC(), TRC!(), TRC1!()

	Shared BRC$(), recconTRC$(), recconTRC1$()

	'-----
	ReDim menuChoices$(1 to 7)
	Shared TB$(), TT$()

	Do
		Cls
		
		2300 Call TITLE
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
		EXITCODE% = 7
		FileFGColr% = 15: FileBGColr% = 1
		HiLiteFGColr% = 0: HiLiteBGColr% = 7
		Spacing% = 1
		Row% = 8
		Column% = 25

		CALL box0(7, 24, 15, 50, 2,  15, 1)
			
		_MouseShow "DEFAULT"
		MMenuVert menuChoices$(), SELECTION%, Start%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
		_MouseHide

		Color , 0
		If Not (EXITCODE% = 27) Then	
			Select Case SELECTION%
				Case 1: Call ViewSeasonRecords
				Case 2: Call PrintSeasonRecords
				Case 3: If _FileExists(diskPaths$(1) + "RECORDS.CBB") Then Kill diskPaths$(1) + "RECORDS.CBB"
				Case 4: Call Records (1)
				Case 5: Call Records (2)
				Case 6: If _FileExists(diskPaths$(1) + "RECORDS.GAM") Then Kill diskPaths$(1) + "RECORDS.GAM"
				Case 7: Cls
			End Select
		End If
		
	Loop Until SELECTION% = 7 OR EXITCODE% = 27

Cls
    
'15 Exit Sub
End Sub


'----------------------------------------
'		ViewSeasonRecords Subroutine
'----------------------------------------
'...explanation...
Sub ViewSeasonRecords ()

	Shared BRC(), TRC!(), TRC1!()
	Shared TB$(), TT$(), BRC$()
	Shared recconTRC$(), recconTRC1$()

	If Not _FileExists(diskPaths$(1) + "RECORDS.CBB") Then 
		Call TITLE
		Locate 9, 30: Print "ERROR::: UNABLE TO LOCATE RECORDS"
		I$ = GetKeyPress$
	Else
		Open diskPaths$(1) + "RECORDS.CBB" For Input As #1
		For I = 1 To 25
			Input #1, BRC(I)
			Input #1, BRC$(I, 0)
			Input #1, BRC$(I, 1)
		Next
		For I = 1 To 21
			Input #1, TRC!(I)
			Input #1, recconTRC$(I)
			If TRC!(I) = 19999 Then TRC!(I) = 0: recconTRC$(I) = ""
		Next
		For I = 1 To 21
			Input #1, TRC1!(I)
			Input #1, recconTRC1$(I)
			If TRC1!(I) = 19999 Then TRC1!(I) = 0: recconTRC1$(I) = ""
		Next
		Close 1


		Color 0, 0
		Cls
		J = 20

		Locate 1, 1: Color 15, 4: Print " " + Chr$(214); String$(73, 196); Chr$(183) + " ";
		For II = 1 To J
			Color 15, 4
			Locate 1 + II, 1: Print " " + Chr$(186); String$(73, 32); Chr$(186) + " ";: Color 7, 4: Print String$(2, 177)
		Next

		Color 15, 4
		Locate 2 + J, 1: Print " " + Chr$(211); String$(73, 196); Chr$(189) + " ";: Color 7, 4: Print String$(2, 177)
		Locate , 3: Print String$(77, 177)
		Color 15: Locate 2, 23: Print "******* INDIVIDUAL PLAYER RECORDS *******"
		Print
		Locate , 4: Print "CATEGORY                PLAYER           TEAM            RECORD"
		Color 7
		Locate , 4: Print TB$(1); Tab(28); BRC$(1, 0); Tab(42); BRC$(1, 1); Tab(62); Using "####"; BRC(1)
		Locate , 4: Print TB$(2); Tab(28); BRC$(2, 0); Tab(42); BRC$(2, 1); Tab(62); Using "####"; BRC(2)
		Locate , 4: Print TB$(3); Tab(28); BRC$(3, 0); Tab(42); BRC$(3, 1); Tab(62); Using "####"; BRC(3)
		Locate , 4: Print TB$(4); Tab(28); BRC$(4, 0); Tab(42); BRC$(4, 1); Tab(62); Using "##.#"; BRC(4) * 100
		Locate , 4: Print TB$(5); Tab(28); BRC$(5, 0); Tab(42); BRC$(5, 1); Tab(62); Using "####"; BRC(5)
		Locate , 4: Print TB$(6); Tab(28); BRC$(6, 0); Tab(42); BRC$(6, 1); Tab(62); Using "####"; BRC(6)
		Locate , 4: Print TB$(7); Tab(28); BRC$(7, 0); Tab(42); BRC$(7, 1); Tab(62); Using "##.#"; BRC(7) * 100
		Locate , 4: Print TB$(8); Tab(28); BRC$(8, 0); Tab(42); BRC$(8, 1); Tab(62); Using "####"; BRC(8)
		Locate , 4: Print TB$(9); Tab(28); BRC$(9, 0); Tab(42); BRC$(9, 1); Tab(62); Using "####"; BRC(9)
		Locate , 4: Print TB$(10); Tab(28); BRC$(10, 0); Tab(42); BRC$(10, 1); Tab(62); Using "##.#"; BRC(10) * 100
		Locate , 4: Print TB$(11); Tab(28); BRC$(11, 0); Tab(42); BRC$(11, 1); Tab(62); Using "####"; BRC(11)
		Locate , 4: Print TB$(12); Tab(28); BRC$(12, 0); Tab(42); BRC$(12, 1); Tab(62); Using "####"; BRC(12)
		Locate , 4: Print TB$(13); Tab(28); BRC$(13, 0); Tab(42); BRC$(13, 1); Tab(62); Using "####"; BRC(13)
		Color 15
		Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
		I$ = GetKeyPress$

		For I = 4 To 21: Locate I, 4
			Print "                                                                        "
		Next


		Color 0, 0
		Cls
		J = 20

		Locate 1, 1: Color 15, 4: Print " " + Chr$(214); String$(73, 196); Chr$(183) + " ";
		For II = 1 To J
			Color 15, 4
			Locate 1 + II, 1: Print " " + Chr$(186); String$(73, 32); Chr$(186) + " ";: Color 7, 4: Print String$(2, 177)
		Next

		Color 15, 4
		Locate 2 + J, 1: Print " " + Chr$(211); String$(73, 196); Chr$(189) + " ";: Color 7, 4: Print String$(2, 177)
		Locate , 3: Print String$(77, 177)
		Color 15
		Locate 2, 23: Print "******* INDIVIDUAL PLAYER RECORDS *******"
		Print
		Locate , 4: Print "CATEGORY                PLAYER           TEAM            RECORD"
		Color 7
		Locate , 4: Print TB$(14); Tab(28); BRC$(14, 0); Tab(42); BRC$(14, 1); Tab(62); Using "####"; BRC(14)
		Locate , 4: Print TB$(15); Tab(28); BRC$(15, 0); Tab(42); BRC$(15, 1); Tab(62); Using "####"; BRC(15)
		Locate , 4: Print TB$(16); Tab(28); BRC$(16, 0); Tab(42); BRC$(16, 1); Tab(62); Using "####"; BRC(16)
		Locate , 4: Print TB$(17); Tab(28); BRC$(17, 0); Tab(42); BRC$(17, 1); Tab(62); Using "####"; BRC(17)
		Locate , 4: Print TB$(18); Tab(28); BRC$(18, 0); Tab(42); BRC$(18, 1); Tab(62); Using "##.#"; BRC(18)
		Locate , 4: Print TB$(19); Tab(28); BRC$(19, 0); Tab(42); BRC$(19, 1); Tab(62); Using "####"; BRC(19)
		Locate , 4: Print TB$(20); Tab(28); BRC$(20, 0); Tab(42); BRC$(20, 1); Tab(62); Using "####"; BRC(20)
		Locate , 4: Print TB$(21); Tab(28); BRC$(21, 0); Tab(42); BRC$(21, 1); Tab(62); Using "####"; BRC(21)
		Locate , 4: Print TB$(22); Tab(28); BRC$(22, 0); Tab(42); BRC$(22, 1); Tab(62); Using "####"; BRC(22)
		Locate , 4: Print TB$(23); Tab(28); BRC$(23, 0); Tab(42); BRC$(23, 1); Tab(62); Using "####"; BRC(23)
		Locate , 4: Print TB$(24); Tab(28); BRC$(24, 0); Tab(42); BRC$(24, 1); Tab(62); Using "##.#"; BRC(24)
		Locate , 4: Print TB$(25); Tab(28); BRC$(25, 0); Tab(42); BRC$(25, 1); Tab(62); Using "####"; BRC(25)
		Color 15
		Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
		I$ = GetKeyPress$

		For I = 4 To 21: Locate I, 4
			Print "                                                                        "
		Next


		Color 0, 0
		Cls
		J = 20
		Locate 1, 1
		Color 15, 1: Print " " + Chr$(214); String$(73, 196); Chr$(183) + " ";
		For II = 1 To J
			Color 15, 1
			Locate 1 + II, 1: Print " " + Chr$(186); String$(73, 32); Chr$(186) + " ";: Color 7, 1: Print String$(2, 177)
		Next

		Color 15, 1
		Locate 2 + J, 1: Print " " + Chr$(211); String$(73, 196); Chr$(189) + " ";: Color 7, 1: Print String$(2, 177)
		Locate , 3: Print String$(77, 177)

		Color 15
		Locate 2, 23: Print "*******  OFFENSIVE TEAM RECORDS *******"
		Print
		Locate , 4: Print "CATEGORY                        TEAM             RECORD            "
		Color 14
		Locate , 4: Print TT$(1); Tab(33); recconTRC$(1); Tab(53); Using "####"; TRC!(1)
		Locate , 4: Print TT$(2); Tab(33); recconTRC$(2); Tab(53); Using "####"; TRC!(2)
		Locate , 4: Print TT$(3); Tab(33); recconTRC$(3); Tab(53); Using "##.#"; TRC!(3) * 100
		Locate , 4: Print TT$(4); Tab(33); recconTRC$(4); Tab(53); Using "####"; TRC!(4)
		Locate , 4: Print TT$(5); Tab(33); recconTRC$(5); Tab(53); Using "####"; TRC!(5)
		Locate , 4: Print TT$(6); Tab(33); recconTRC$(6); Tab(53); Using "##.#"; TRC!(6) * 100
		Locate , 4: Print TT$(7); Tab(33); recconTRC$(7); Tab(53); Using "####"; TRC!(7)
		Locate , 4: Print TT$(8); Tab(33); recconTRC$(8); Tab(53); Using "####"; TRC!(8)
		Locate , 4: Print TT$(9); Tab(33); recconTRC$(9); Tab(53); Using "##.#"; TRC!(9) * 100
		Locate , 4: Print TT$(10); Tab(33); recconTRC$(10); Tab(53); Using "####"; TRC!(10)
		Locate , 4: Print TT$(11); Tab(33); recconTRC$(11); Tab(53); Using "####"; TRC!(11)
		Locate , 4: Print TT$(12); Tab(33); recconTRC$(12); Tab(53); Using "####"; TRC!(12)
		Locate , 4: Print TT$(13); Tab(33); recconTRC$(13); Tab(53); Using "##.#"; TRC!(13)
		Locate , 4: Print TT$(14); Tab(33); recconTRC$(14); Tab(53); Using "####"; TRC!(14)
		Locate , 4: Print TT$(15); Tab(33); recconTRC$(15); Tab(53); Using "####"; TRC!(15)
		Color 7
		I$ = GetKeyPress$

		For I = 4 To 21: Locate I, 4
			Print "                                                                        "
		Next

		Color 0, 0
		Cls
		J = 20
		Locate 1, 1
		Color 15, 1: Print " " + Chr$(214); String$(73, 196); Chr$(183) + " ";
		For II = 1 To J
			Color 15, 1
			Locate 1 + II, 1: Print " " + Chr$(186); String$(73, 32); Chr$(186) + " ";: Color 7, 1: Print String$(2, 177)
		Next

		Color 15, 1
		Locate 2 + J, 1: Print " " + Chr$(211); String$(73, 196); Chr$(189) + " ";: Color 7, 1: Print String$(2, 177)
		Locate , 3: Print String$(77, 177)
		Color 15
		Locate 2, 23: Print "*******  OFFENSIVE TEAM RECORDS *******"
		Print
		Locate , 4: Print "CATEGORY                        TEAM             RECORD            "
		Color 14
		Locate , 4: Print TT$(16); Tab(33); recconTRC$(16); Tab(53); Using "####"; TRC!(16)
		Locate , 4: Print TT$(17); Tab(33); recconTRC$(17); Tab(53); Using "####"; TRC!(17)
		Locate , 4: Print TT$(18); Tab(33); recconTRC$(18); Tab(53); Using "####"; TRC!(18)
		Locate , 4: Print TT$(19); Tab(33); recconTRC$(19); Tab(52); Using "#####"; TRC!(19)
		Locate , 4: Print TT$(20); Tab(33); recconTRC$(20); Tab(52); Using "###.#"; TRC!(20)
		Locate , 4: Print "WINS"; Tab(33); recconTRC$(21); Tab(53); Using "####"; TRC!(21)
		Color 7
		I$ = GetKeyPress$

		For I = 4 To 21: Locate I, 4
			Print "                                                                        "
		Next


		Color 0, 0
		Cls
		J = 20

		Locate 1, 1: Color 15, 4: Print " " + Chr$(214); String$(73, 196); Chr$(183) + " ";
		For II = 1 To J
			Color 15, 4
			Locate 1 + II, 1: Print " " + Chr$(186); String$(73, 32); Chr$(186) + " ";: Color 7, 4: Print String$(2, 177)
		Next

		Color 15, 4
		Locate 2 + J, 1: Print " " + Chr$(211); String$(73, 196); Chr$(189) + " ";: Color 7, 4: Print String$(2, 177)
		Locate , 3: Print String$(77, 177)
		Color 14: Locate 2, 23: Print "*******  DEFENSIVE TEAM RECORDS *******"
		Print
		Locate , 4: Print "CATEGORY                        TEAM             RECORD            "
		Color 15
		Locate , 4: Print TT$(1); Tab(33); recconTRC1$(1); Tab(53); Using "####"; TRC1!(1)
		Locate , 4: Print TT$(2); Tab(33); recconTRC1$(2); Tab(53); Using "####"; TRC1!(2)
		Locate , 4: Print TT$(3); Tab(33); recconTRC1$(3); Tab(53); Using "##.#"; TRC1!(3) * 100
		Locate , 4: Print TT$(4); Tab(33); recconTRC1$(4); Tab(53); Using "####"; TRC1!(4)
		Locate , 4: Print TT$(5); Tab(33); recconTRC1$(5); Tab(53); Using "####"; TRC1!(5)
		Locate , 4: Print TT$(6); Tab(33); recconTRC1$(6); Tab(53); Using "##.#"; TRC1!(6) * 100
		Locate , 4: Print TT$(7); Tab(33); recconTRC1$(7); Tab(53); Using "####"; TRC1!(7)
		Locate , 4: Print TT$(8); Tab(33); recconTRC1$(8); Tab(53); Using "####"; TRC1!(8)
		Locate , 4: Print TT$(9); Tab(33); recconTRC1$(9); Tab(53); Using "##.#"; TRC1!(9) * 100
		Locate , 4: Print TT$(10); Tab(33); recconTRC1$(10); Tab(53); Using "####"; TRC1!(10)
		Locate , 4: Print TT$(11); Tab(33); recconTRC1$(11); Tab(53); Using "####"; TRC1!(11)
		Locate , 4: Print TT$(12); Tab(33); recconTRC1$(12); Tab(53); Using "####"; TRC1!(12)
		Locate , 4: Print TT$(13); Tab(33); recconTRC1$(13); Tab(53); Using "##.#"; TRC1!(13)
		Locate , 4: Print TT$(14); Tab(33); recconTRC1$(14); Tab(53); Using "####"; TRC1!(14)
		Locate , 4: Print TT$(15); Tab(33); recconTRC1$(15); Tab(53); Using "####"; TRC1!(15)
		Color 7
		I$ = GetKeyPress$

		For I = 4 To 21: Locate I, 4
			Print "                                                                        "
		Next


		Color 0, 0
		Cls
		J = 20

		Locate 1, 1: Color 15, 4: Print " " + Chr$(214); String$(73, 196); Chr$(183) + " ";
		For II = 1 To J
			Color 15, 4
			Locate 1 + II, 1: Print " " + Chr$(186); String$(73, 32); Chr$(186) + " ";: Color 7, 4: Print String$(2, 177)
		Next

		Color 15, 4
		Locate 2 + J, 1: Print " " + Chr$(211); String$(73, 196); Chr$(189) + " ";: Color 7, 4: Print String$(2, 177)
		Locate , 3: Print String$(77, 177)
		Color 14: Locate 2, 23: Print "*******  DEFENSIVE TEAM RECORDS *******"
		Print
		Locate , 4: Print "CATEGORY                        TEAM             RECORD            "
		Color 15
		Locate , 4: Print TT$(16); Tab(33); recconTRC1$(16); Tab(53); Using "####"; TRC1!(16)
		Locate , 4: Print TT$(17); Tab(33); recconTRC1$(17); Tab(53); Using "####"; TRC1!(17)
		Locate , 4: Print TT$(18); Tab(33); recconTRC1$(18); Tab(53); Using "####"; TRC1!(18)
		Locate , 4: Print TT$(19); Tab(33); recconTRC1$(19); Tab(52); Using "#####"; TRC1!(19)
		Locate , 4: Print TT$(20); Tab(33); recconTRC1$(20); Tab(52); Using "###.#"; TRC1!(20)
		Locate , 4: Print "LOSSES"; Tab(33); recconTRC1$(21); Tab(53); Using "####"; TRC1!(21)
		Color 7
		I$ = GetKeyPress$

		For I = 4 To 21: Locate I, 4
			Print "                                                                        "
		Next

	End If
	
End Sub


'----------------------------------------
'		PrintSeasonRecords Subroutine
'----------------------------------------
'...explanation...
Sub PrintSeasonRecords ()

	Shared BRC(), TRC!(), TRC1!()
	Shared TB$(), TT$(), BRC$()
	Shared recconTRC$(), recconTRC1$()


	If Not _FileExists(diskPaths$(1) + "RECORDS.CBB") Then 
		'--This should be a CsMsgBox instead
		Call TITLE
		Locate 9, 30: Print "ERROR::: UNABLE TO LOCATE RECORDS"
		I$ = GetKeyPress$
	Else

		Open diskPaths$(1) + "RECORDS.CBB" For Input As #1
		For I = 1 To 25
			Input #1, BRC(I)
			Input #1, BRC$(I, 0)
			Input #1, BRC$(I, 1)
		Next
		For I = 1 To 21
			Input #1, TRC!(I)
			Input #1, recconTRC$(I)
			If TRC!(I) = 19999 Then TRC!(I) = 0: recconTRC$(I) = ""
		Next
		For I = 1 To 21
			Input #1, TRC1!(I)
			Input #1, recconTRC1$(I)
			If TRC1!(I) = 19999 Then TRC1!(I) = 0: recconTRC1$(I) = ""
		Next
		Close 1

		' PRINT RECORDS
		LPrint Tab(23); "******* INDIVIDUAL PLAYER RECORDS *******"
		LPrint
		LPrint "CATEGORY                PLAYER           TEAM            RECORD"
		LPrint TB$(1); Tab(28); BRC$(1, 0); Tab(42); BRC$(1, 1); Tab(62); Using "####"; BRC(1)
		LPrint TB$(2); Tab(28); BRC$(2, 0); Tab(42); BRC$(2, 1); Tab(62); Using "####"; BRC(2)
		LPrint TB$(3); Tab(28); BRC$(3, 0); Tab(42); BRC$(3, 1); Tab(62); Using "####"; BRC(3)
		LPrint TB$(4); Tab(28); BRC$(4, 0); Tab(42); BRC$(4, 1); Tab(62); Using "##.#"; BRC(4) * 100
		LPrint TB$(5); Tab(28); BRC$(5, 0); Tab(42); BRC$(5, 1); Tab(62); Using "####"; BRC(5)
		LPrint TB$(6); Tab(28); BRC$(6, 0); Tab(42); BRC$(6, 1); Tab(62); Using "####"; BRC(6)
		LPrint TB$(7); Tab(28); BRC$(7, 0); Tab(42); BRC$(7, 1); Tab(62); Using "##.#"; BRC(7) * 100
		LPrint TB$(8); Tab(28); BRC$(8, 0); Tab(42); BRC$(8, 1); Tab(62); Using "####"; BRC(8)
		LPrint TB$(9); Tab(28); BRC$(9, 0); Tab(42); BRC$(9, 1); Tab(62); Using "####"; BRC(9)
		LPrint TB$(10); Tab(28); BRC$(10, 0); Tab(42); BRC$(10, 1); Tab(62); Using "##.#"; BRC(10) * 100
		LPrint TB$(11); Tab(28); BRC$(11, 0); Tab(42); BRC$(11, 1); Tab(62); Using "####"; BRC(11)
		LPrint TB$(12); Tab(28); BRC$(12, 0); Tab(42); BRC$(12, 1); Tab(62); Using "####"; BRC(12)
		LPrint TB$(13); Tab(28); BRC$(13, 0); Tab(42); BRC$(13, 1); Tab(62); Using "####"; BRC(13)
		LPrint TB$(14); Tab(28); BRC$(14, 0); Tab(42); BRC$(14, 1); Tab(62); Using "####"; BRC(14)
		LPrint TB$(15); Tab(28); BRC$(15, 0); Tab(42); BRC$(15, 1); Tab(62); Using "####"; BRC(15)
		LPrint TB$(16); Tab(28); BRC$(16, 0); Tab(42); BRC$(16, 1); Tab(62); Using "####"; BRC(16)
		LPrint TB$(17); Tab(28); BRC$(17, 0); Tab(42); BRC$(17, 1); Tab(62); Using "####"; BRC(17)
		LPrint TB$(18); Tab(28); BRC$(18, 0); Tab(42); BRC$(18, 1); Tab(62); Using "##.#"; BRC(18)
		LPrint TB$(19); Tab(28); BRC$(19, 0); Tab(42); BRC$(19, 1); Tab(62); Using "####"; BRC(19)
		LPrint TB$(20); Tab(28); BRC$(20, 0); Tab(42); BRC$(20, 1); Tab(62); Using "####"; BRC(20)
		LPrint TB$(21); Tab(28); BRC$(21, 0); Tab(42); BRC$(21, 1); Tab(62); Using "####"; BRC(21)
		LPrint TB$(22); Tab(28); BRC$(22, 0); Tab(42); BRC$(22, 1); Tab(62); Using "####"; BRC(22)
		LPrint TB$(23); Tab(28); BRC$(23, 0); Tab(42); BRC$(23, 1); Tab(62); Using "####"; BRC(23)
		LPrint TB$(24); Tab(28); BRC$(24, 0); Tab(42); BRC$(24, 1); Tab(62); Using "##.#"; BRC(24)
		LPrint TB$(25); Tab(28); BRC$(25, 0); Tab(42); BRC$(25, 1); Tab(62); Using "####"; BRC(25)
		LPrint
		LPrint Tab(23); "*******  OFFENSIVE TEAM RECORDS *******"
		LPrint
		LPrint "CATEGORY                        TEAM             RECORD            "
		LPrint TT$(1); Tab(33); recconTRC$(1); Tab(53); Using "####"; TRC!(1)
		LPrint TT$(2); Tab(33); recconTRC$(2); Tab(53); Using "####"; TRC!(2)
		LPrint TT$(3); Tab(33); recconTRC$(3); Tab(53); Using "##.#"; TRC!(3) * 100
		LPrint TT$(4); Tab(33); recconTRC$(4); Tab(53); Using "####"; TRC!(4)
		LPrint TT$(5); Tab(33); recconTRC$(5); Tab(53); Using "####"; TRC!(5)
		LPrint TT$(6); Tab(33); recconTRC$(6); Tab(53); Using "##.#"; TRC!(6) * 100
		LPrint TT$(7); Tab(33); recconTRC$(7); Tab(53); Using "####"; TRC!(7)
		LPrint TT$(8); Tab(33); recconTRC$(8); Tab(53); Using "####"; TRC!(8)
		LPrint TT$(9); Tab(33); recconTRC$(9); Tab(53); Using "##.#"; TRC!(9) * 100
		LPrint TT$(10); Tab(33); recconTRC$(10); Tab(53); Using "####"; TRC!(10)
		LPrint TT$(11); Tab(33); recconTRC$(11); Tab(53); Using "####"; TRC!(11)
		LPrint TT$(12); Tab(33); recconTRC$(12); Tab(53); Using "####"; TRC!(12)
		LPrint TT$(13); Tab(33); recconTRC$(13); Tab(53); Using "##.#"; TRC!(13)
		LPrint TT$(14); Tab(33); recconTRC$(14); Tab(53); Using "####"; TRC!(14)
		LPrint TT$(15); Tab(33); recconTRC$(15); Tab(53); Using "####"; TRC!(15)
		LPrint TT$(16); Tab(33); recconTRC$(16); Tab(53); Using "####"; TRC!(16)
		LPrint TT$(17); Tab(33); recconTRC$(17); Tab(53); Using "####"; TRC!(17)
		LPrint TT$(18); Tab(33); recconTRC$(18); Tab(53); Using "####"; TRC!(18)
		LPrint TT$(19); Tab(33); recconTRC$(19); Tab(52); Using "#####"; TRC!(19)
		LPrint TT$(20); Tab(33); recconTRC$(20); Tab(52); Using "###.#"; TRC!(20)
		LPrint "WINS"; Tab(33); recconTRC$(21); Tab(53); Using "####"; TRC!(21)
		LPrint
		LPrint Tab(23); "*******  DEFENSIVE TEAM RECORDS *******"
		LPrint
		LPrint "CATEGORY                        TEAM             RECORD            "
		LPrint TT$(1); Tab(33); recconTRC1$(1); Tab(53); Using "####"; TRC1!(1)
		LPrint TT$(2); Tab(33); recconTRC1$(2); Tab(53); Using "####"; TRC1!(2)
		LPrint TT$(3); Tab(33); recconTRC1$(3); Tab(53); Using "##.#"; TRC1!(3) * 100
		LPrint TT$(4); Tab(33); recconTRC1$(4); Tab(53); Using "####"; TRC1!(4)
		LPrint TT$(5); Tab(33); recconTRC1$(5); Tab(53); Using "####"; TRC1!(5)
		LPrint TT$(6); Tab(33); recconTRC1$(6); Tab(53); Using "##.#"; TRC1!(6) * 100
		LPrint TT$(7); Tab(33); recconTRC1$(7); Tab(53); Using "####"; TRC1!(7)
		LPrint TT$(8); Tab(33); recconTRC1$(8); Tab(53); Using "####"; TRC1!(8)
		LPrint TT$(9); Tab(33); recconTRC1$(9); Tab(53); Using "##.#"; TRC1!(9) * 100
		LPrint TT$(10); Tab(33); recconTRC1$(10); Tab(53); Using "####"; TRC1!(10)
		LPrint TT$(11); Tab(33); recconTRC1$(11); Tab(53); Using "####"; TRC1!(11)
		LPrint TT$(12); Tab(33); recconTRC1$(12); Tab(53); Using "####"; TRC1!(12)
		LPrint TT$(13); Tab(33); recconTRC1$(13); Tab(53); Using "##.#"; TRC1!(13)
		LPrint TT$(14); Tab(33); recconTRC1$(14); Tab(53); Using "####"; TRC1!(14)
		LPrint TT$(15); Tab(33); recconTRC1$(15); Tab(53); Using "####"; TRC1!(15)
		LPrint TT$(16); Tab(33); recconTRC1$(16); Tab(53); Using "####"; TRC1!(16)
		LPrint TT$(17); Tab(33); recconTRC1$(17); Tab(53); Using "####"; TRC1!(17)
		LPrint TT$(18); Tab(33); recconTRC1$(18); Tab(53); Using "####"; TRC1!(18)
		LPrint TT$(19); Tab(33); recconTRC1$(19); Tab(52); Using "#####"; TRC1!(19)
		LPrint TT$(20); Tab(33); recconTRC1$(20); Tab(52); Using "###.#"; TRC1!(20)
		LPrint "LOSSES"; Tab(33); recconTRC1$(21); Tab(53); Using "####"; TRC1!(21)
		LPrint Chr$(12)

	End If

	Cls

END SUB
