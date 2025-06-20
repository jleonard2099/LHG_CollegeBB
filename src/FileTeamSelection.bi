Declare Sub ChooseTeam (yearNumber$, numberTeams, chosenIdx, teamName$)
Declare Sub SelectTeam (numberTeams, yearNumber$, selectedIdx, teamName$, ExitCode%)
Declare Sub ReadTeam (teamYear$, targetTeam$, teamIdx)
Declare Sub ReadExtraData (teamYear$, teamIdx, dataOK)
Declare Sub UserFilePrompt (reportFile$)
Declare Sub ScheduleFilePrompt (scheduleFile$)

Declare Sub ReadTeamStats (teamName$, statFile$)
Declare Sub ReadScheduleStats (statFile$, numberGames)
Declare Sub SaveScheduleStats (saveFile$, numberGames)
Declare Sub ReadSeasonRecords (recordFile$)
Declare Sub ReadGameRecords (recordFile$)
Declare Sub ReadConferenceIndex (indexFile$)
Declare Sub ReadConferenceStats (statFile$)
Declare Sub ReadOrganizationFile (organizationFile$, orgName$)
Declare Sub ReadTournamentFile (tourneyFile$)

Declare Function nameStatFile$ (teamName$)
Declare Function CheckString% (targetString$)
Declare Function ChooseFile$(fileSpec$, fileDesc$)

Const TEAM_SIZE_BYTES = 655
Const DATA_SIZE_BYTES = 574
Const TEAM_FILE_NAME$ = "BASK"
Const DATA_FILE_NAME$ = "COLBBTMS"

Dim previousYear$

'-- transition away from this
Dim Shared fileLength&

Dim CK
Dim defFGPctAdj!, def3FGPctAdj, defthreeFGA_Adj
Dim staminaRating!

Dim teamAttendance&

Dim coach$, mascot$, arenaName$

Dim d3FG_Adj(1), d3FGA_Adj(1)

Dim leagueRatings(0 To 4)
Dim teamRatings(0 To 9), teamStats!(0 To 24)

'Why isn't this 0 to 13?
Dim playerOff!(0 To 14, 0 To 19), playerRatings!(0 To 13, 0 To 6)
Dim playerDef!(0 To 13), playerStats!(0 To 13, 0 To 13)

Dim playerNames$(0 To 13), position$(0 To 13)
'Dim playerNames$(0 To 13, 0 To 1)
Dim statPlyrNames$(0 To 15)
