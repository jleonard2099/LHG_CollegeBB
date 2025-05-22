'----------------------------------------
' Used in more than one source file
'----------------------------------------
Dim Shared menu&
Dim opSysType$
Dim Shared dirSeparator$

Dim Shared diskPaths$(0 To 3), Q$(0 To 500)
Dim Shared teamNames$(MAX_TEAMS)
Dim Shared teamIndex(MAX_TEAMS)

' *** Reading Stat Data ***
' -------------------------
Dim teamNameStats$

Dim gameAttendance&(MAX_SCHED_STATS)
Dim gameAtt_Road&(MAX_SCHED_STATS)

Dim oppScore(MAX_SCHED_STATS), teamScore(MAX_SCHED_STATS)
Dim oppScore_Road(MAX_SCHED_STATS), teamScore_Road(MAX_SCHED_STATS)

Dim powerRating(MAX_SCHED_STATS, 1)
Dim powerRating_Road(MAX_SCHED_STATS, 1)

Dim gameSite$(MAX_SCHED_STATS), locIndicator$(MAX_SCHED_STATS), oppName$(MAX_SCHED_STATS)
Dim gameSite_Road$(MAX_SCHED_STATS), locIndicator_Road$(MAX_SCHED_STATS), oppName_Road$(MAX_SCHED_STATS)


Dim statsPlyrRat!(15, 14)
Dim stats3FGM!(15), stats3FGA!(15)
Dim statsOffTot!(15), statsDefTot!(15)
Dim gameStatPlyrVal!(13, 13)

'Record / Standings
Dim confWins, confLosses
Dim fullWins, fullLosses
Dim replayLosses, replayWins

Dim homeLosses, homeWins, neutralLosses, neutralWins
Dim roadLosses, roadWins, totalLosses, totalWins

' Conferences / Orgs
Dim memberIdx(TEAMS_PER_CONFERENCE)
Dim memberConf$(MAX_CONFERENCES)
Dim memberName$(TEAMS_PER_CONFERENCE)
Dim memberYear$(TEAMS_PER_CONFERENCE)



' *** Schedule Data ***
' -------------------------
Dim BS%, NS%

ReDim scheduleAP%(MAX_SCHEDULE_GAMES), scheduleNG%(MAX_SCHEDULE_GAMES, 18)
ReDim homeScores(MAX_SCHEDULE_GAMES), visitorScores(MAX_SCHEDULE_GAMES)

ReDim scheduleH$(MAX_SCHEDULE_GAMES), scheduleV$(MAX_SCHEDULE_GAMES)
ReDim scheduleYN$(MAX_SCHEDULE_GAMES, 1)
'ReDim yearNumber$(MAX_SCHEDULE_GAMES, 1)

'-- These are used for tournaments
Dim homeTeam$(MAX_TOURNAMENT_GAMES), visitingTeam$(MAX_TOURNAMENT_GAMES)


' *** Game Options ***
' -------------------------
Dim Shared DT$, TM$

Dim eventSettings(13)

Dim coachMode$(0 To 1), foulShotOpt$(0 To 2)
Dim location$(0 To 2), modeAbbrev$(0 To 3)
Dim playMode$(0 To 3), sClockOpt$(0 To 3)
Dim teamIndicator$(1)
Dim yesNo$(0 To 1), yesNoText$(1)


'----------------------------------------
' Used in ALIGN / MERGE routines
'----------------------------------------
Dim plyrRat_MERGE(14, 14)

Dim plyrStat_Align!(15, 14), plyrStat_Merge!(14, 14), plyrStat_Road!(15, 14)

Dim threeFGM_Align!(15), threeFGA_Align!(15)
Dim threeFGM_Merge!(15), threeFGA_Merge!(15)
Dim threeFGM_Road!(15), threeFGA_Road!(15)

Dim statsOffTot_Road!(15), statsDefTot_Road!(15)

Dim plyrName_Road$(15), plyrName_Align$(15)


'----------------------------------------
' Used in CAREER / LEADER routines
'----------------------------------------
'-- 600 = 15 * 40
Dim NB, NP

Dim careerPlayers$(62), BL$(21)

Dim car3FGM!(MAX_CAREER_YEARS, 62), car3FGA!(MAX_CAREER_YEARS, 62)
Dim carPlyrStat!(MAX_CAREER_YEARS, 62, 15)
Dim leader3FGM!(600), leader3FGA!(600)

Dim carLdrPlyrName$(19, 20)
Dim carLdrVal!(19, 20)

Dim carRecVal!(21), carRecYear!(21)

Dim plyLeaderTeams$(40, 15), tmLeaderTeams$(40, 15)

Dim plyrLdrStats!(0 To 600, 0 To 14)
Dim plyrLeaderGames!(40), tmLeaderGames!(40)
Dim plyrLdrVal!(40, 15), teamLdrVal!(40, 15)

' For Expanded Leaders
Dim expIndCategory$(0 To 31), expTeamCategory$(0 To 39)
Dim expPlyrLdrName$(600), expPlyrLdrTeam$(600), expLdrTeam$(600), expLdrPlyrName$(600)

Dim totGames!(600), expLdrVal!(600)

'For Expanded Standings
Dim lastLoseStreak(TEAMS_PER_CONFERENCE), lastWinStreak(TEAMS_PER_CONFERENCE)
Dim lastTenLosses(TEAMS_PER_CONFERENCE), lastTenWins(TEAMS_PER_CONFERENCE)

Dim teamLoseStreak(TEAMS_PER_CONFERENCE), teamWinStreak(TEAMS_PER_CONFERENCE)

Dim totHomeLosses(TEAMS_PER_CONFERENCE), totHomeWins(TEAMS_PER_CONFERENCE)
Dim totAwayLosses(TEAMS_PER_CONFERENCE), totAwayWins(TEAMS_PER_CONFERENCE)
Dim totTeamLosses(TEAMS_PER_CONFERENCE), totTeamWins(TEAMS_PER_CONFERENCE)

Dim expStdLeagName$(TEAMS_PER_CONFERENCE)


'----------------------------------------
' Used in COMPILE / SEE routines
'----------------------------------------
Dim defTotals!(21), offTotals!(21)
Dim defLeaders!(60, 20), offLeaders!(60, 20)
Dim leaderVals!(1 To 250, 0 To 2), natLeaderVals!(1 To 250, 1 To 2)
Dim natDefLeaders!(MAX_CONFERENCES, 1 To 20), natOffLeaders!(MAX_CONFERENCES, 1 To 20)
Dim compStats!(0 To 14, 0 To 26)

Dim seasonWins(30), seasonLosses(30), seasonWinPct!(30), divWins(30), divLosses(30), divWinPct!(30)

Dim defLeaderTeams$(60), offLeaderTeams$(60)
Dim statCategoryAbbr$(25)

Dim leaderName$(1 To 250), leaderTeam$(1 To 250)
Dim leaderText$(250)

Dim pollTeam$(1200)
Dim pollRatings#(1200, 5)

Dim confLdrName$(30)
Dim confLdrVal!(260), confLdrAtt!(260)

Dim natDefLdrNames$(40), natOffLdrNames$(1 To 60)

Dim natLdrName$(1 To 250), natLdrTeam$(1 To 250)


'----------------------------------------
' Used in DRAFT routines
'----------------------------------------
Dim CK!(1)

Dim tmAtt_DRAFT&(1)

Dim dFGPctAdj_DRAFT!(1)

Dim leagRat_DRAFT(1, 4), avgRat_DRAFT(1, 13, 12)
Dim teamRat_DRAFT(1, 9)

Dim plyrDef_DRAFT!(1, 13), plyrOff_DRAFT!(1, 13, 6)
Dim plyrRat_DRAFT!(1, 13, 6), plyrStat_DRAFT!(1, 13, 13)
Dim stamina_DRAFT!(1), teamStats_DRAFT!(1, 24)


Dim coaches_DRAFT$(1), mascots_DRAFT$(1), stadiums_DRAFT$(1)
Dim plyrNames_DRAFT$(0 To 1, 0 To 13), posn_DRAFT$(1, 13)
Dim tmYears_DRAFT$(1), tmName_DRAFT$(1)


'----------------------------------------
'   Used in HD2HD routines
'----------------------------------------
Dim conferenceName$
Dim div1Name$, div2Name$

Dim awayLosses(50), awayWins(50)
Dim awayScoreTeam!(50), awayScoreOpp!(50)

Dim homeLosses(50), homeWins(50)
Dim homeScoreTeam!(50), homeScoreOpp!(50)

Dim totAwayScoreTeam!(40), totAwayScoreOpp!(40)
Dim totHomeScoreTeam!(40), totHomeScoreOpp!(40)


'----------------------------------------
' Used in RECORDS routines
'----------------------------------------
Dim plyrRecords!(32), offRecords!(1 To 21), defRecords!(1 To 21)
Dim plyrRecDesc$(32, 1), offRecDesc$(1 To 21), defRecDesc$(1 To 21)

Dim indRecords!(50, 2), teamRecords!(125, 2)
Dim indRecordDesc$(25), teamRecordDesc$(20)
Dim indRecDesc$(50, 4), teamRecDesc$(125, 3)


'----------------------------------------
' Used in STAT / INPUT routines
'----------------------------------------
Dim nbrGmsPlayed!, opp3FGA!, opp3FGAPct!, oppAsst!, oppBlocks!
Dim oppFGPct!, oppFouls!, oppFGA!, oppFTA!, oppFTPct!
Dim oppReb!, oppSteal!, oppTO!
Dim tm3FGA!, tm3FGAPct!, tmAsst!, tmBlocks!, tmFouls!
Dim tmFGA!, tmFGPct!, tmFTA!, tmFTPct!
Dim tmReb!, tmSteal!, tmTO!

Dim statPlyrVal!(0 To 14, 0 To 26)


'----------------------------------------
' Used in ADDTOURN / BRACKET routines
'----------------------------------------
Dim TC

Dim Shared playinOpt$

'           REGION, SEED NUMBER, TEAM#/MODE OF PLAY (0,1)
' Mode of Play is no longer used as it's saved
' with the file
Dim tourneyParams(0 To 17, 0 To 17, 0 To 3)

'Number of Teams Per Region (up to 16 Regions)
'Additional regions
'   Playin Teams
'   Championship Teams
Dim nbrRegionalTeams(0 To 17)

'Team Names for Each Seed Slot
Dim slotName$(0 To 17, 0 To 17, 0 To 3)

'Region Names
Dim regionNames$(0 To 17)

Dim yearNumber$(MAX_TEAMS, 3)

'TOURNAMENT DEFAULTS
Dim tourneyDefaults(14)

'0-CONSOLATION IN REGIONS, 1-CONSOLATION IN CHAMPIONSHIP
Dim consolationSetting(1)


'----------------------------------------
' Used in Game Routines
'----------------------------------------
Dim scheduleFile$, tourneyFile$
Dim schedHomeTm$, schedVisTm$

Dim actualAttendance&
Dim Shared whistle&, swish&, backboard&

Dim FY%(0 To 1)

Dim alpha$(4), tickerPeriod$(14), teamYrTourn$(0 To 3)

Dim schedOptions(18)
Dim tourneySettings(1 To 16, 1 To 16, 0 To 4)

Dim avgAttendance&(1)

' Shared / Global
Dim Shared earlyExit
Dim Shared C1, clockSecs
Dim Shared D, P, P9
Dim Shared autoPlay, ballCarrier, coachOpt, currHalf
Dim Shared compTeam, currGame, gameOver, endAllGames
Dim Shared fastBreak, forcedThree, ftNumber
Dim Shared G9, gameAvgTmStamina, gameLoc, guardIdx
Dim Shared halfTime
Dim Shared ft3PtFoul, ft6FloorFouls, ft10thFoul
Dim Shared foulOnPlay, foulsToDQ
Dim Shared guardNbr, lineupChanged, MJ
Dim Shared nbrLines, offStatus
Dim Shared passes, playerMode, playerOpt, pbpOpt, pbpType
Dim Shared rebFoulChance, S2, sClockVal, shotClock, shotType, sndOpt
Dim Shared threePtOpt, threePtShotChance
Dim Shared tickerGames, tickerIdx, tourneyPlay, willDunk, XS

Dim Shared GS%, J8%
Dim Shared X0%, X1%, XX%

Dim Shared gameClock!, pbpDelay!, shotChance!, timeElapsed!

Dim Shared A1$, B1$, C1$, D1$, E1$, F1$, G1$, H1$
Dim Shared prevBall$, pbpString$

Dim Shared crashBoards(1), G9%(1)
Dim Shared ST%(32), SX%(32, 1, 14)

Dim Shared defAdjVsPass(2, 4), defAdjVsShot(4, 3)
Dim Shared dFGPA(1), def3FG_Adj(1), defStrat(1), defCategory(1), defCoverage(1)
Dim Shared ft3PtFoul(1), homeCtFactor(1, 6)

Dim Shared P4(14), P5(14)

Dim Shared leagRat_GAME(1, 4), lineupIdx(0 To 1, 0 To 4), mandatoryTO(0 To 1), offStrat(1)
Dim Shared pbpFG(8), pbpBG(8), pctContrib(1)
Dim Shared plyrStaminaGame(13), plyrRebIdx(13), plyrRebVal(13)
Dim Shared playerTime(1, 13), pressureRating(1, 1)
Dim Shared plyrDef_GAME(0 To 1, 0 To 13), plyrOff_GAME!(0 To 1, 0 To 13, 0 To 24)
Dim Shared plyrRat_GAME(0 To 1, 0 To 13, 0 To 6), plyrStat_GAME(0 To 1, 0 To 13, 0 To 15)
Dim Shared QQ(1, 8, 13, 14), QR(1, 7, 14)
Dim Shared rebRatings(9), rosterIdx(13), rosterStatus(1, 13)
Dim Shared schedGame(2)
Dim Shared score(0 To 1, 0 To 10), scSettings(0 To 3), statTotals(14)
Dim Shared statsGame3FGM(1, 13), statsGame3FGA(1, 13)
Dim Shared teamAdj(0 To 1, 0 To 8), teamRat_GAME(1, 9), teamRecWins(1), teamRecLoss(1)
Dim Shared teamStats_GAME(0 To 1, 0 To 24), teamFouls(1), teamStamina(1), teamYears(1)
Dim Shared threeFG(1, 13, 1), timeouts(1), timePlayed(1, 13)
Dim Shared tmFatigue(1, 13), totRebounds(1), turnovers(1)
Dim tourneyOptions(18)

Dim Shared gameRatings!(0 To 1, 0 To 13, 0 To 25)
Dim game3FGM!(1, 13), game3FGA!(1, 13)

Dim Shared defStyles$(0 To 17), defStyles_brief$(0 To 17), diskIDs$(5)
Dim Shared gameArena$(0 To 1), gameCoaches$(0 To 1), gameMascots$(0 To 1), gameTeams$(0 To 1)
Dim Shared offStyles$(9), offStyles_brief$(15)
Dim Shared offStylesNew$(15)
Dim Shared pbpStyle$(1), players$(1, 13)
Dim Shared positions_GAME$(0 To 1, 0 To 13), posnName$(0 To 4)
Dim Shared seedSlotName$(1 To 16, 1 To 16, 0 To 4), SX$(32, 2)
Dim Shared teamGender$(0 To 1)

