Declare Sub ChooseTeam (yearNumber$, numberTeams, teamIdx)
Declare Sub SelectTeam (selectedIdx)
Declare Sub ReadTeam (teamYear$, targetTeam$, teamIdx)
Declare Sub ReadExtraData (teamYear$, teamIdx)
Declare Function nameStatFile$ (teamName$)
Declare Function ChooseFile$(fileSpec$, fileDesc$)

Const TEAM_SIZE_BYTES = 655
Const DATA_SIZE_BYTES = 566
Const TEAM_FILE_NAME$ = "BASK"
Const DATA_FILE_NAME$ = "COLBBTMS"

Dim previousYear$

'-- transition away from Dim Shared
'-- fileLength for sure
Dim Shared fileLength&

Dim CK, defFGPctAdj%
Dim def3FG_Adj, def3FGA_Adj
Dim staminaRating%, teamAttendance&

Dim coach$, mascot$, arenaName$

Dim d3FG_Adj(1), d3FGA_Adj(1)

Dim leagueRatings%(0 To 4)
Dim teamRatings%(0 To 9), teamStats(20)

Dim playerOff!(0 To 14, 0 To 19), playerRatings!(0 To 13, 0 To 6)
Dim playerDef!(13), playerStats!(13, 13)

Dim playerNames$(0 To 13), position$(0 To 13)
Dim statPlyrNames$(0 To 15)
