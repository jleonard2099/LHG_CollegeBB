$Debug
Const GAME_VERSION$ = "5.00"
Const MAX_SCHEDULE_GAMES = 6000
Const MAX_TEAMS = 1000
Const MAX_FILES = 10240
'optimium = 1048576 (but slows code)
Const MAX_CONFERENCES = 40
Const TEAMS_PER_DIV = 15
Const DIVS_PER_CONFERENCE = 2
Const TEAMS_PER_CONFERENCE = 30

Const MAX_SCHED_STATS = 100
Const SCHEDULE_SIZE_BYTES = 52

DefInt A-Z

'$Include: 'BoxRoutines.bi'
'$Include: 'FileTeamSelection.bi'
'$Include: 'KeyInput.bi'
'$Include: 'QPProEqu.bi'
'$Include: 'PadRight.bi'
'$Include: 'Variables.bi'

Open "FOLDERS.CFG" For Input As #2

For I = 0 To 3
    Input #2, diskPaths$(I)
Next

Close #2

fileSpec$ = diskPaths$(0) + "COLBBTMS.*"

Count% = FileCount%(fileSpec$)
ReDim foundFiles$(0 To Count%)
foundFiles$(0) = fileSpec$
Call ReadFile(foundFiles$())

For X = 1 To Count%
    target$ = diskPaths$(0) + foundFiles$(X)
    Print "Converting "; target$
    Call ConvertTeam4to5(target$, 1)
    'Call ConvertTeam5to4 (target$, 1)
Next X

'$Include: 'Convert.BAS'
'$Include: 'BoxRoutines.bm'
'$Include: 'FileTeamSelection.bm'
'$Include: 'KeyInput.bm'
'$Include: 'QPProEqu.bm'
'$Include: 'PadRight.bm'
