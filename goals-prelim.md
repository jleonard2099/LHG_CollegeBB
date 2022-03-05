
* PRELIMINARY WORK

** GOAL: ADDRESS MISSING / EXTRANEOUS SOURCE FILES

[X] College Basketball
	[X] BLANK.BAS
		used to 'DELETE CONFERENCE STATS' (from LEADERS/STANDINGS/RECORDS menu)
		was not in /src, and not referenced in menus
		added copy of file from Pro BB sources after evaluating code
		added to menus based on V4.2 compiled exe menu layout

	[X] BLANKORG.BAS
		used to 'DELETE ORGANIZATION STATS' (from LEADERS/STANDINGS/RECORDS menu)
		was not in /src, and not referenced in menus
		added copy of file from Football sources after evaluating code
		added to menus based on V4.2 compiled exe menu layout

	[X] SCHDCONV.EXE (no matching BAS source)
		file is part of game install, but is not referenced by source
		received subscript out of range trying to run from original game files
		all needed schedule features are in the main source / EXE


** GOAL: COMPILE FULL GAMES FROM SOURCES
*** From WWW - "COURTSIDE COLLEGE BASKETBALL V4.2"
*** From Code - "COURTSIDE COLLEGE BASKETBALL V4.1"

[ ] Re-create all library code natively in BASIC

[ ] Attempt first compilation of each game
	[ ] bring in files from "misc" as needed	
	[ ] delete "misc" folder when done 
