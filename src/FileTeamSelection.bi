Declare Sub ChooseTeam (yearNumber$, numberTeams, teamIdx)
Declare Sub SelectTeam (selectedIdx)
Declare Sub ReadTeam (yearNumber$, teamIdx)
Declare Sub ReadExtraData (teamYear$, targetTeam$, teamIdx)
Declare Function nameStatFile$ (teamName$)
Declare Function ChooseFile$(fileSpec$, fileDesc$)

Const TEAM_SIZE_BYTES = 655
Const DATA_SIZE_BYTES = 566
Const TEAM_FILE_NAME$ = "BASK"
Const DATA_FILE_NAME$ = "COLBBTMS"

Dim previousYear$

