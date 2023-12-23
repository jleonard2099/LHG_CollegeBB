'----------------------------------------
'        SeasonRecords Subroutine
'----------------------------------------
'This subroutine gives the user a number of 
'menu selections for viewing, updating and
'printing records
SUB SeasonRecords ()

	_Title "Courtside College Basketball - Analyze Records"

	Shared plyrRecords!(), offRecords!(), defRecords!()

	Shared plyrRecDesc$(), offRecDesc$(), defRecDesc$()
	Shared indRecords$(), teamRecords$()

	ReDim menuChoices$(1 to 7)

    If Not _FileExists(diskPaths$(1) + "RECORDS.CBB") Then

		MsgText$ = "UNABLE TO LOCATE RECORDS PRESS ANY KEY TO RETURN TO MAIN MENU"
		BoxWidth = 24
		BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
		Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
		Call CSMsgBox(MsgText$, BoxWidth, Pass)
		I$ = GetKeyPress$
		Call CSMsgBox("", 0, Pass)

    Else

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
			Selection% = 1
			Count% = 7
			Start% = 1
			ExitCode% = 7
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
						Call ViewSeasonRecords

					Case 2: 
						Call PrintSeasonRecords

					Case 3: 
						If _FileExists(diskPaths$(1) + "RECORDS.CBB") Then Kill diskPaths$(1) + "RECORDS.CBB"

					Case 4: 
						Call GameRecords (1)

					Case 5: 
						Call GameRecords (2)

					Case 6: 
						If _FileExists(diskPaths$(1) + "RECORDS.GAM") Then Kill diskPaths$(1) + "RECORDS.GAM"

					Case 7: Cls

				End Select

			End If

		Loop Until Selection% = 7 OR ExitCode% = 27

	End If

	Cls

End Sub

'----------------------------------------
'		ViewSeasonRecords Subroutine
'----------------------------------------
'This subroutine assists the usere with
'viewing the records on the default display
Sub ViewSeasonRecords ()

	Shared plyrRecords!(), offRecords!(), defRecords!()
	Shared indRecords$(), teamRecords$(), plyrRecDesc$()
	Shared offRecDesc$(), defRecDesc$()

	If Not _FileExists(diskPaths$(1) + "RECORDS.CBB") Then 

		MsgText$ = "UNABLE TO LOCATE RECORDS PRESS ANY KEY TO RETURN TO MAIN MENU"
		BoxWidth = 24
		BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
		Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
		Call CSMsgBox(MsgText$, BoxWidth, Pass)
		I$ = GetKeyPress$
		Call CSMsgBox("", 0, Pass)

	Else

		Open diskPaths$(1) + "RECORDS.CBB" For Input As #1

		For I = 1 To 25
			Input #1, plyrRecords!(I)
			Input #1, plyrRecDesc$(I, 0)
			Input #1, plyrRecDesc$(I, 1)
		Next

		For I = 1 To 21
			Input #1, offRecords!(I)
			Input #1, offRecDesc$(I)
			If offRecords!(I) = 19999 Then 
				offRecords!(I) = 0
				offRecDesc$(I) = ""
			End If
		Next

		For I = 1 To 21
			Input #1, defRecords!(I)
			Input #1, defRecDesc$(I)
			If defRecords!(I) = 19999 Then 
				defRecords!(I) = 0
				defRecDesc$(I) = ""
			End If
		Next

		Close #1

		Color 0, 0
		Cls

		J = 20

		Locate 1, 1

		Color 15, 4
		Print " " + Chr$(214); String$(73, 196); Chr$(183) + " ";
		
		For II = 1 To J
			Color 15, 4
			Locate 1 + II, 1: Print " " + Chr$(186); String$(73, 32); Chr$(186) + " ";
			Color 7, 4: Print String$(2, 177)
		Next

		Color 15, 4
		Locate 2 + J, 1: Print " " + Chr$(211); String$(73, 196); Chr$(189) + " ";
		Color 7, 4: Print String$(2, 177)
		Locate , 3: Print String$(77, 177)

		Color 15
		Locate 2, 23
		Print "******* INDIVIDUAL PLAYER RECORDS *******"
		Print
		Locate , 4
		Print "CATEGORY                PLAYER           TEAM            RECORD"

		Color 7
		Locate , 4: Print indRecords$(1); Tab(28); plyrRecDesc$(1, 0); Tab(42); plyrRecDesc$(1, 1); Tab(62); Using "####"; plyrRecords!(1)
		Locate , 4: Print indRecords$(2); Tab(28); plyrRecDesc$(2, 0); Tab(42); plyrRecDesc$(2, 1); Tab(62); Using "####"; plyrRecords!(2)
		Locate , 4: Print indRecords$(3); Tab(28); plyrRecDesc$(3, 0); Tab(42); plyrRecDesc$(3, 1); Tab(62); Using "####"; plyrRecords!(3)
		Locate , 4: Print indRecords$(4); Tab(28); plyrRecDesc$(4, 0); Tab(42); plyrRecDesc$(4, 1); Tab(62); Using "##.#"; plyrRecords!(4) * 100
		Locate , 4: Print indRecords$(5); Tab(28); plyrRecDesc$(5, 0); Tab(42); plyrRecDesc$(5, 1); Tab(62); Using "####"; plyrRecords!(5)
		Locate , 4: Print indRecords$(6); Tab(28); plyrRecDesc$(6, 0); Tab(42); plyrRecDesc$(6, 1); Tab(62); Using "####"; plyrRecords!(6)
		Locate , 4: Print indRecords$(7); Tab(28); plyrRecDesc$(7, 0); Tab(42); plyrRecDesc$(7, 1); Tab(62); Using "##.#"; plyrRecords!(7) * 100
		Locate , 4: Print indRecords$(8); Tab(28); plyrRecDesc$(8, 0); Tab(42); plyrRecDesc$(8, 1); Tab(62); Using "####"; plyrRecords!(8)
		Locate , 4: Print indRecords$(9); Tab(28); plyrRecDesc$(9, 0); Tab(42); plyrRecDesc$(9, 1); Tab(62); Using "####"; plyrRecords!(9)
		Locate , 4: Print indRecords$(10); Tab(28); plyrRecDesc$(10, 0); Tab(42); plyrRecDesc$(10, 1); Tab(62); Using "##.#"; plyrRecords!(10) * 100
		Locate , 4: Print indRecords$(11); Tab(28); plyrRecDesc$(11, 0); Tab(42); plyrRecDesc$(11, 1); Tab(62); Using "####"; plyrRecords!(11)
		Locate , 4: Print indRecords$(12); Tab(28); plyrRecDesc$(12, 0); Tab(42); plyrRecDesc$(12, 1); Tab(62); Using "####"; plyrRecords!(12)
		Locate , 4: Print indRecords$(13); Tab(28); plyrRecDesc$(13, 0); Tab(42); plyrRecDesc$(13, 1); Tab(62); Using "####"; plyrRecords!(13)

		Color 15
		Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
		I$ = GetKeyPress$

		For I = 4 To 21: Locate I, 4
			Print Space$(72)
		Next

		Color 0, 0
		Cls

		J = 20

		Locate 1, 1

		Color 15, 4
		Print " " + Chr$(214); String$(73, 196); Chr$(183) + " ";
		
		For II = 1 To J
			Color 15, 4
			Locate 1 + II, 1: Print " " + Chr$(186); String$(73, 32); Chr$(186) + " ";
			Color 7, 4: Print String$(2, 177)
		Next

		Color 15, 4
		Locate 2 + J, 1: Print " " + Chr$(211); String$(73, 196); Chr$(189) + " ";
		Color 7, 4: Print String$(2, 177)
		Locate , 3: Print String$(77, 177)
		
		Color 15
		Locate 2, 23
		Print "******* INDIVIDUAL PLAYER RECORDS *******"
		Print
		Locate , 4
		Print "CATEGORY                PLAYER           TEAM            RECORD"

		Color 7
		Locate , 4: Print indRecords$(14); Tab(28); plyrRecDesc$(14, 0); Tab(42); plyrRecDesc$(14, 1); Tab(62); Using "####"; plyrRecords!(14)
		Locate , 4: Print indRecords$(15); Tab(28); plyrRecDesc$(15, 0); Tab(42); plyrRecDesc$(15, 1); Tab(62); Using "####"; plyrRecords!(15)
		Locate , 4: Print indRecords$(16); Tab(28); plyrRecDesc$(16, 0); Tab(42); plyrRecDesc$(16, 1); Tab(62); Using "####"; plyrRecords!(16)
		Locate , 4: Print indRecords$(17); Tab(28); plyrRecDesc$(17, 0); Tab(42); plyrRecDesc$(17, 1); Tab(62); Using "####"; plyrRecords!(17)
		Locate , 4: Print indRecords$(18); Tab(28); plyrRecDesc$(18, 0); Tab(42); plyrRecDesc$(18, 1); Tab(62); Using "##.#"; plyrRecords!(18)
		Locate , 4: Print indRecords$(19); Tab(28); plyrRecDesc$(19, 0); Tab(42); plyrRecDesc$(19, 1); Tab(62); Using "####"; plyrRecords!(19)
		Locate , 4: Print indRecords$(20); Tab(28); plyrRecDesc$(20, 0); Tab(42); plyrRecDesc$(20, 1); Tab(62); Using "####"; plyrRecords!(20)
		Locate , 4: Print indRecords$(21); Tab(28); plyrRecDesc$(21, 0); Tab(42); plyrRecDesc$(21, 1); Tab(62); Using "####"; plyrRecords!(21)
		Locate , 4: Print indRecords$(22); Tab(28); plyrRecDesc$(22, 0); Tab(42); plyrRecDesc$(22, 1); Tab(62); Using "####"; plyrRecords!(22)
		Locate , 4: Print indRecords$(23); Tab(28); plyrRecDesc$(23, 0); Tab(42); plyrRecDesc$(23, 1); Tab(62); Using "####"; plyrRecords!(23)
		Locate , 4: Print indRecords$(24); Tab(28); plyrRecDesc$(24, 0); Tab(42); plyrRecDesc$(24, 1); Tab(62); Using "##.#"; plyrRecords!(24)
		Locate , 4: Print indRecords$(25); Tab(28); plyrRecDesc$(25, 0); Tab(42); plyrRecDesc$(25, 1); Tab(62); Using "####"; plyrRecords!(25)

		Color 15
		Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
		I$ = GetKeyPress$

		For I = 4 To 21: Locate I, 4
			Print Space$(72)
		Next

		Color 0, 0
		Cls

		J = 20

		Locate 1, 1

		Color 15, 1
		Print " " + Chr$(214); String$(73, 196); Chr$(183) + " ";
		
		For II = 1 To J
			Color 15, 1
			Locate 1 + II, 1: Print " " + Chr$(186); String$(73, 32); Chr$(186) + " ";
			Color 7, 1: Print String$(2, 177)
		Next

		Color 15, 1
		Locate 2 + J, 1: Print " " + Chr$(211); String$(73, 196); Chr$(189) + " ";
		Color 7, 1: Print String$(2, 177)
		Locate , 3: Print String$(77, 177)

		Color 15
		Locate 2, 23: Print "*******  OFFENSIVE TEAM RECORDS *******"
		Print
		Locate , 4
		Print "CATEGORY                        TEAM             RECORD            "

		Color 14
		Locate , 4: Print teamRecords$(1); Tab(33); offRecDesc$(1); Tab(53); Using "####"; offRecords!(1)
		Locate , 4: Print teamRecords$(2); Tab(33); offRecDesc$(2); Tab(53); Using "####"; offRecords!(2)
		Locate , 4: Print teamRecords$(3); Tab(33); offRecDesc$(3); Tab(53); Using "##.#"; offRecords!(3) * 100
		Locate , 4: Print teamRecords$(4); Tab(33); offRecDesc$(4); Tab(53); Using "####"; offRecords!(4)
		Locate , 4: Print teamRecords$(5); Tab(33); offRecDesc$(5); Tab(53); Using "####"; offRecords!(5)
		Locate , 4: Print teamRecords$(6); Tab(33); offRecDesc$(6); Tab(53); Using "##.#"; offRecords!(6) * 100
		Locate , 4: Print teamRecords$(7); Tab(33); offRecDesc$(7); Tab(53); Using "####"; offRecords!(7)
		Locate , 4: Print teamRecords$(8); Tab(33); offRecDesc$(8); Tab(53); Using "####"; offRecords!(8)
		Locate , 4: Print teamRecords$(9); Tab(33); offRecDesc$(9); Tab(53); Using "##.#"; offRecords!(9) * 100
		Locate , 4: Print teamRecords$(10); Tab(33); offRecDesc$(10); Tab(53); Using "####"; offRecords!(10)
		Locate , 4: Print teamRecords$(11); Tab(33); offRecDesc$(11); Tab(53); Using "####"; offRecords!(11)
		Locate , 4: Print teamRecords$(12); Tab(33); offRecDesc$(12); Tab(53); Using "####"; offRecords!(12)
		Locate , 4: Print teamRecords$(13); Tab(33); offRecDesc$(13); Tab(53); Using "##.#"; offRecords!(13)
		Locate , 4: Print teamRecords$(14); Tab(33); offRecDesc$(14); Tab(53); Using "####"; offRecords!(14)
		Locate , 4: Print teamRecords$(15); Tab(33); offRecDesc$(15); Tab(53); Using "####"; offRecords!(15)

		Color 15
		Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
		I$ = GetKeyPress$

		For I = 4 To 21: Locate I, 4
			Print Space$(72)
		Next

		Color 0, 0
		Cls

		J = 20

		Locate 1, 1

		Color 15, 1
		Print " " + Chr$(214); String$(73, 196); Chr$(183) + " ";
		
		For II = 1 To J
			Color 15, 1
			Locate 1 + II, 1: Print " " + Chr$(186); String$(73, 32); Chr$(186) + " ";
			Color 7, 1: Print String$(2, 177)
		Next

		Color 15, 1
		Locate 2 + J, 1: Print " " + Chr$(211); String$(73, 196); Chr$(189) + " ";
		Color 7, 1: Print String$(2, 177)
		Locate , 3: Print String$(77, 177)

		Color 15
		Locate 2, 23
		Print "*******  OFFENSIVE TEAM RECORDS *******"
		Print
		Locate , 4
		Print "CATEGORY                        TEAM             RECORD            "

		Color 14
		Locate , 4: Print teamRecords$(16); Tab(33); offRecDesc$(16); Tab(53); Using "####"; offRecords!(16)
		Locate , 4: Print teamRecords$(17); Tab(33); offRecDesc$(17); Tab(53); Using "####"; offRecords!(17)
		Locate , 4: Print teamRecords$(18); Tab(33); offRecDesc$(18); Tab(53); Using "####"; offRecords!(18)
		Locate , 4: Print teamRecords$(19); Tab(33); offRecDesc$(19); Tab(52); Using "#####"; offRecords!(19)
		Locate , 4: Print teamRecords$(20); Tab(33); offRecDesc$(20); Tab(52); Using "###.#"; offRecords!(20)
		Locate , 4: Print "WINS"; Tab(33); offRecDesc$(21); Tab(53); Using "####"; offRecords!(21)

		Color 15
		Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
		I$ = GetKeyPress$

		For I = 4 To 21: Locate I, 4
			Print Space$(72)
		Next

		Color 0, 0
		Cls

		J = 20

		Locate 1, 1

		Color 15, 4
		Print " " + Chr$(214); String$(73, 196); Chr$(183) + " ";
		
		For II = 1 To J
			Color 15, 4
			Locate 1 + II, 1: Print " " + Chr$(186); String$(73, 32); Chr$(186) + " ";
			Color 7, 4: Print String$(2, 177)
		Next

		Color 15, 4
		Locate 2 + J, 1: Print " " + Chr$(211); String$(73, 196); Chr$(189) + " ";
		Color 7, 4: Print String$(2, 177)
		Locate , 3: Print String$(77, 177)

		Color 14
		Locate 2, 23
		Print "*******  DEFENSIVE TEAM RECORDS *******"
		Print
		Locate , 4
		Print "CATEGORY                        TEAM             RECORD            "

		Color 15
		Locate , 4: Print teamRecords$(1); Tab(33); defRecDesc$(1); Tab(53); Using "####"; defRecords!(1)
		Locate , 4: Print teamRecords$(2); Tab(33); defRecDesc$(2); Tab(53); Using "####"; defRecords!(2)
		Locate , 4: Print teamRecords$(3); Tab(33); defRecDesc$(3); Tab(53); Using "##.#"; defRecords!(3) * 100
		Locate , 4: Print teamRecords$(4); Tab(33); defRecDesc$(4); Tab(53); Using "####"; defRecords!(4)
		Locate , 4: Print teamRecords$(5); Tab(33); defRecDesc$(5); Tab(53); Using "####"; defRecords!(5)
		Locate , 4: Print teamRecords$(6); Tab(33); defRecDesc$(6); Tab(53); Using "##.#"; defRecords!(6) * 100
		Locate , 4: Print teamRecords$(7); Tab(33); defRecDesc$(7); Tab(53); Using "####"; defRecords!(7)
		Locate , 4: Print teamRecords$(8); Tab(33); defRecDesc$(8); Tab(53); Using "####"; defRecords!(8)
		Locate , 4: Print teamRecords$(9); Tab(33); defRecDesc$(9); Tab(53); Using "##.#"; defRecords!(9) * 100
		Locate , 4: Print teamRecords$(10); Tab(33); defRecDesc$(10); Tab(53); Using "####"; defRecords!(10)
		Locate , 4: Print teamRecords$(11); Tab(33); defRecDesc$(11); Tab(53); Using "####"; defRecords!(11)
		Locate , 4: Print teamRecords$(12); Tab(33); defRecDesc$(12); Tab(53); Using "####"; defRecords!(12)
		Locate , 4: Print teamRecords$(13); Tab(33); defRecDesc$(13); Tab(53); Using "##.#"; defRecords!(13)
		Locate , 4: Print teamRecords$(14); Tab(33); defRecDesc$(14); Tab(53); Using "####"; defRecords!(14)
		Locate , 4: Print teamRecords$(15); Tab(33); defRecDesc$(15); Tab(53); Using "####"; defRecords!(15)

		Color 15
		Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
		I$ = GetKeyPress$

		For I = 4 To 21: Locate I, 4
			Print Space$(72)
		Next

		Color 0, 0
		Cls

		J = 20

		Locate 1, 1

		Color 15, 4
		Print " " + Chr$(214); String$(73, 196); Chr$(183) + " ";
		
		For II = 1 To J
			Color 15, 4
			Locate 1 + II, 1: Print " " + Chr$(186); String$(73, 32); Chr$(186) + " ";
			Color 7, 4: Print String$(2, 177)
		Next

		Color 15, 4
		Locate 2 + J, 1: Print " " + Chr$(211); String$(73, 196); Chr$(189) + " ";
		Color 7, 4: Print String$(2, 177)
		Locate , 3: Print String$(77, 177)

		Color 14
		Locate 2, 23: Print "*******  DEFENSIVE TEAM RECORDS *******"
		Print
		Locate , 4: Print "CATEGORY                        TEAM             RECORD            "

		Color 15

		Locate , 4: Print teamRecords$(16); Tab(33); defRecDesc$(16); Tab(53); Using "####"; defRecords!(16)
		Locate , 4: Print teamRecords$(17); Tab(33); defRecDesc$(17); Tab(53); Using "####"; defRecords!(17)
		Locate , 4: Print teamRecords$(18); Tab(33); defRecDesc$(18); Tab(53); Using "####"; defRecords!(18)
		Locate , 4: Print teamRecords$(19); Tab(33); defRecDesc$(19); Tab(52); Using "#####"; defRecords!(19)
		Locate , 4: Print teamRecords$(20); Tab(33); defRecDesc$(20); Tab(52); Using "###.#"; defRecords!(20)
		Locate , 4: Print "LOSSES"; Tab(33); defRecDesc$(21); Tab(53); Using "####"; defRecords!(21)

		Color 15
		Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
		I$ = GetKeyPress$

		For I = 4 To 21: Locate I, 4
			Print Space$(72)
		Next

	End If

End Sub

'----------------------------------------
'		PrintSeasonRecords Subroutine
'----------------------------------------
'This subroutine assists the usere with
'viewing the records on the default printer
Sub PrintSeasonRecords ()

	Shared plyrRecords!(), offRecords!(), defRecords!()
	Shared indRecords$(), teamRecords$(), plyrRecDesc$()
	Shared offRecDesc$(), defRecDesc$()

    Color 0, 15

	LPrint Tab(23); "******* INDIVIDUAL PLAYER RECORDS *******"
	LPrint
	LPrint "CATEGORY                PLAYER           TEAM            RECORD"
	LPrint indRecords$(1); Tab(28); plyrRecDesc$(1, 0); Tab(42); plyrRecDesc$(1, 1); Tab(62); Using "####"; plyrRecords!(1)
	LPrint indRecords$(2); Tab(28); plyrRecDesc$(2, 0); Tab(42); plyrRecDesc$(2, 1); Tab(62); Using "####"; plyrRecords!(2)
	LPrint indRecords$(3); Tab(28); plyrRecDesc$(3, 0); Tab(42); plyrRecDesc$(3, 1); Tab(62); Using "####"; plyrRecords!(3)
	LPrint indRecords$(4); Tab(28); plyrRecDesc$(4, 0); Tab(42); plyrRecDesc$(4, 1); Tab(62); Using "##.#"; plyrRecords!(4) * 100
	LPrint indRecords$(5); Tab(28); plyrRecDesc$(5, 0); Tab(42); plyrRecDesc$(5, 1); Tab(62); Using "####"; plyrRecords!(5)
	LPrint indRecords$(6); Tab(28); plyrRecDesc$(6, 0); Tab(42); plyrRecDesc$(6, 1); Tab(62); Using "####"; plyrRecords!(6)
	LPrint indRecords$(7); Tab(28); plyrRecDesc$(7, 0); Tab(42); plyrRecDesc$(7, 1); Tab(62); Using "##.#"; plyrRecords!(7) * 100
	LPrint indRecords$(8); Tab(28); plyrRecDesc$(8, 0); Tab(42); plyrRecDesc$(8, 1); Tab(62); Using "####"; plyrRecords!(8)
	LPrint indRecords$(9); Tab(28); plyrRecDesc$(9, 0); Tab(42); plyrRecDesc$(9, 1); Tab(62); Using "####"; plyrRecords!(9)
	LPrint indRecords$(10); Tab(28); plyrRecDesc$(10, 0); Tab(42); plyrRecDesc$(10, 1); Tab(62); Using "##.#"; plyrRecords!(10) * 100
	LPrint indRecords$(11); Tab(28); plyrRecDesc$(11, 0); Tab(42); plyrRecDesc$(11, 1); Tab(62); Using "####"; plyrRecords!(11)
	LPrint indRecords$(12); Tab(28); plyrRecDesc$(12, 0); Tab(42); plyrRecDesc$(12, 1); Tab(62); Using "####"; plyrRecords!(12)
	LPrint indRecords$(13); Tab(28); plyrRecDesc$(13, 0); Tab(42); plyrRecDesc$(13, 1); Tab(62); Using "####"; plyrRecords!(13)
	LPrint indRecords$(14); Tab(28); plyrRecDesc$(14, 0); Tab(42); plyrRecDesc$(14, 1); Tab(62); Using "####"; plyrRecords!(14)
	LPrint indRecords$(15); Tab(28); plyrRecDesc$(15, 0); Tab(42); plyrRecDesc$(15, 1); Tab(62); Using "####"; plyrRecords!(15)
	LPrint indRecords$(16); Tab(28); plyrRecDesc$(16, 0); Tab(42); plyrRecDesc$(16, 1); Tab(62); Using "####"; plyrRecords!(16)
	LPrint indRecords$(17); Tab(28); plyrRecDesc$(17, 0); Tab(42); plyrRecDesc$(17, 1); Tab(62); Using "####"; plyrRecords!(17)
	LPrint indRecords$(18); Tab(28); plyrRecDesc$(18, 0); Tab(42); plyrRecDesc$(18, 1); Tab(62); Using "##.#"; plyrRecords!(18)
	LPrint indRecords$(19); Tab(28); plyrRecDesc$(19, 0); Tab(42); plyrRecDesc$(19, 1); Tab(62); Using "####"; plyrRecords!(19)
	LPrint indRecords$(20); Tab(28); plyrRecDesc$(20, 0); Tab(42); plyrRecDesc$(20, 1); Tab(62); Using "####"; plyrRecords!(20)
	LPrint indRecords$(21); Tab(28); plyrRecDesc$(21, 0); Tab(42); plyrRecDesc$(21, 1); Tab(62); Using "####"; plyrRecords!(21)
	LPrint indRecords$(22); Tab(28); plyrRecDesc$(22, 0); Tab(42); plyrRecDesc$(22, 1); Tab(62); Using "####"; plyrRecords!(22)
	LPrint indRecords$(23); Tab(28); plyrRecDesc$(23, 0); Tab(42); plyrRecDesc$(23, 1); Tab(62); Using "####"; plyrRecords!(23)
	LPrint indRecords$(24); Tab(28); plyrRecDesc$(24, 0); Tab(42); plyrRecDesc$(24, 1); Tab(62); Using "##.#"; plyrRecords!(24)
	LPrint indRecords$(25); Tab(28); plyrRecDesc$(25, 0); Tab(42); plyrRecDesc$(25, 1); Tab(62); Using "####"; plyrRecords!(25)
	LPrint
	LPrint Tab(23); "*******  OFFENSIVE TEAM RECORDS *******"
	LPrint
	LPrint "CATEGORY                        TEAM             RECORD            "
	LPrint teamRecords$(1); Tab(33); offRecDesc$(1); Tab(53); Using "####"; offRecords!(1)
	LPrint teamRecords$(2); Tab(33); offRecDesc$(2); Tab(53); Using "####"; offRecords!(2)
	LPrint teamRecords$(3); Tab(33); offRecDesc$(3); Tab(53); Using "##.#"; offRecords!(3) * 100
	LPrint teamRecords$(4); Tab(33); offRecDesc$(4); Tab(53); Using "####"; offRecords!(4)
	LPrint teamRecords$(5); Tab(33); offRecDesc$(5); Tab(53); Using "####"; offRecords!(5)
	LPrint teamRecords$(6); Tab(33); offRecDesc$(6); Tab(53); Using "##.#"; offRecords!(6) * 100
	LPrint teamRecords$(7); Tab(33); offRecDesc$(7); Tab(53); Using "####"; offRecords!(7)
	LPrint teamRecords$(8); Tab(33); offRecDesc$(8); Tab(53); Using "####"; offRecords!(8)
	LPrint teamRecords$(9); Tab(33); offRecDesc$(9); Tab(53); Using "##.#"; offRecords!(9) * 100
	LPrint teamRecords$(10); Tab(33); offRecDesc$(10); Tab(53); Using "####"; offRecords!(10)
	LPrint teamRecords$(11); Tab(33); offRecDesc$(11); Tab(53); Using "####"; offRecords!(11)
	LPrint teamRecords$(12); Tab(33); offRecDesc$(12); Tab(53); Using "####"; offRecords!(12)
	LPrint teamRecords$(13); Tab(33); offRecDesc$(13); Tab(53); Using "##.#"; offRecords!(13)
	LPrint teamRecords$(14); Tab(33); offRecDesc$(14); Tab(53); Using "####"; offRecords!(14)
	LPrint teamRecords$(15); Tab(33); offRecDesc$(15); Tab(53); Using "####"; offRecords!(15)
	LPrint teamRecords$(16); Tab(33); offRecDesc$(16); Tab(53); Using "####"; offRecords!(16)
	LPrint teamRecords$(17); Tab(33); offRecDesc$(17); Tab(53); Using "####"; offRecords!(17)
	LPrint teamRecords$(18); Tab(33); offRecDesc$(18); Tab(53); Using "####"; offRecords!(18)
	LPrint teamRecords$(19); Tab(33); offRecDesc$(19); Tab(52); Using "#####"; offRecords!(19)
	LPrint teamRecords$(20); Tab(33); offRecDesc$(20); Tab(52); Using "###.#"; offRecords!(20)
	LPrint "WINS"; Tab(33); offRecDesc$(21); Tab(53); Using "####"; offRecords!(21)
	LPrint
	LPrint Tab(23); "*******  DEFENSIVE TEAM RECORDS *******"
	LPrint
	LPrint "CATEGORY                        TEAM             RECORD            "
	LPrint teamRecords$(1); Tab(33); defRecDesc$(1); Tab(53); Using "####"; defRecords!(1)
	LPrint teamRecords$(2); Tab(33); defRecDesc$(2); Tab(53); Using "####"; defRecords!(2)
	LPrint teamRecords$(3); Tab(33); defRecDesc$(3); Tab(53); Using "##.#"; defRecords!(3) * 100
	LPrint teamRecords$(4); Tab(33); defRecDesc$(4); Tab(53); Using "####"; defRecords!(4)
	LPrint teamRecords$(5); Tab(33); defRecDesc$(5); Tab(53); Using "####"; defRecords!(5)
	LPrint teamRecords$(6); Tab(33); defRecDesc$(6); Tab(53); Using "##.#"; defRecords!(6) * 100
	LPrint teamRecords$(7); Tab(33); defRecDesc$(7); Tab(53); Using "####"; defRecords!(7)
	LPrint teamRecords$(8); Tab(33); defRecDesc$(8); Tab(53); Using "####"; defRecords!(8)
	LPrint teamRecords$(9); Tab(33); defRecDesc$(9); Tab(53); Using "##.#"; defRecords!(9) * 100
	LPrint teamRecords$(10); Tab(33); defRecDesc$(10); Tab(53); Using "####"; defRecords!(10)
	LPrint teamRecords$(11); Tab(33); defRecDesc$(11); Tab(53); Using "####"; defRecords!(11)
	LPrint teamRecords$(12); Tab(33); defRecDesc$(12); Tab(53); Using "####"; defRecords!(12)
	LPrint teamRecords$(13); Tab(33); defRecDesc$(13); Tab(53); Using "##.#"; defRecords!(13)
	LPrint teamRecords$(14); Tab(33); defRecDesc$(14); Tab(53); Using "####"; defRecords!(14)
	LPrint teamRecords$(15); Tab(33); defRecDesc$(15); Tab(53); Using "####"; defRecords!(15)
	LPrint teamRecords$(16); Tab(33); defRecDesc$(16); Tab(53); Using "####"; defRecords!(16)
	LPrint teamRecords$(17); Tab(33); defRecDesc$(17); Tab(53); Using "####"; defRecords!(17)
	LPrint teamRecords$(18); Tab(33); defRecDesc$(18); Tab(53); Using "####"; defRecords!(18)
	LPrint teamRecords$(19); Tab(33); defRecDesc$(19); Tab(52); Using "#####"; defRecords!(19)
	LPrint teamRecords$(20); Tab(33); defRecDesc$(20); Tab(52); Using "###.#"; defRecords!(20)
	LPrint "LOSSES"; Tab(33); defRecDesc$(21); Tab(53); Using "####"; defRecords!(21)
	
	LPrint Chr$(12)

END SUB
