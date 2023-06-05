'----------------------------------------
' Used across more than one source file
'----------------------------------------
Dim CK, defFGPctAdj%, finalUpdate%
Dim replayLosses%, staminaRating%, replayWins%
Dim teamAttendance%, V1%, V2%

Dim TC!

Dim coach$, mascot$, stadium$
Dim opSysType$

Dim A$(0 To 15)

Dim compT1%(20), statsL%(0 To 4)
Dim T%(0 To 9)

Dim statsA!(0 To 14, 0 To 13), DF!(13), RS!(13, 13), X!(13, 13)
Dim statsW0!(15), statsW1!(15)
Dim statsZ!(15), statsZ1!(15)

Dim homeTeam$(MAX_GAMES), visitingTeam$(MAX_GAMES)
Dim homeScores(MAX_GAMES), visitorScores(MAX_GAMES)

Dim Shared fileLength&

Dim Shared diskPaths$(3), Q$(0 To 500)
Dim Shared teamNames$(MAX_TEAMS)
Dim Shared teamIndex%(MAX_TEAMS)

' Game Options
Dim Shared DT$, TM$

Dim coachOpt$(0 To 1), foulShotOpt$(0 To 2)
Dim location$(0 To 2), MO$(0 To 3), modeAbbrev$(0 To 3)
Dim sclockOpt$(0 To 2), threeFtOpt$(0 To 1), threePtOpt$(0 To 1)
Dim yesNo$(0 To 1)

'----------------------------------------
' Used across ALIGN, MERGE routines
'----------------------------------------
Dim Ycurr%, Yroad%

Dim alignAR$(15)
Dim AN$(15)
Dim mergeH$(100), HRP$(100)
Dim mergeO$(100), ORP$(100), gameSite$(100), SITERP$(100)

Dim APRD%(100, 1)
Dim mergeAP%(100, 1)
Dim mergeA(14, 14), ARD(14, 14), CRD(100), CRDRD(100)
Dim ORD%(100), TRD%(100)

Dim AN!(15, 14), AR!(15, 14)
Dim W0N!(15), WN1!(15)
Dim W0R!(15), WR1!(15)
Dim W0RD!(15), W1RD!(15)
Dim ZR!(15), ZR1!(15)
Dim ZRD!(15), ZRD1!(15)


'----------------------------------------
' Used across ADDTOURN,
'   BRACKET routines
'----------------------------------------
Dim tourneyN%(17, 17, 3) 'REGION, SEED NUMBER, TEAM#/MODE OF PLAY (0,1)
Dim NN%(17) 'Number of Teams Per Region (up to 16 Regions)

Dim slotName$(17, 17, 0 To 3) 'Team Names for Each Seed Slot
Dim regionNames$(1 To 17) 'Region Names

Dim yearNumber$(MAX_TEAMS, 3)

Dim tourneyDefaults%(14) 'TOURNAMENT DEFAULTS
Dim consolationSetting%(1) '0-CONSOLATION IN REGIONS, 1-CONSOLATION IN CHAMPIONSHIP

'----------------------------------------
' Used across CARVIEW routines
'----------------------------------------
Dim AR$(62)
Dim ARS!(15, 62, 15)
Dim W0S!(15, 62), W1S!(15, 62)
Dim careerT%(0 To 34)

'----------------------------------------
' Used across CNGRAT routines
'----------------------------------------
'14 positions, 0 to 13 array index
Dim cngratA$(0 To 13), P$(0 To 13)

'----------------------------------------
' Used across COMPARE routines
'----------------------------------------
Dim compareA!(15, 14)
Dim A1!(13, 6), X1!(13, 6)
Dim HP%(100), compareL%(6), OP%(100), compareT%(34)
Dim compareB$(14), HP$(100), OP$(100)

'----------------------------------------
' Used across COMPNAT routines
'----------------------------------------
Dim compH$(100), compN$(0 To 14)
Dim LD!(1 To 250, 0 To 2)
Dim NDL!(1 To 40, 1 To 20), NLD!(1 To 250, 1 To 2), NOL!(1 To 40, 1 To 20)
Dim compS!(0 To 14, 0 To 26)

Dim DT!(21), OT!(21)

Dim NDL$(40), NZ0$(1 To 250), NZ1$(30), NZ3$(1 To 250)

Dim CP$(25), compO$(100)
Dim Z0$(1 To 250), Z3$(1 To 250)

'----------------------------------------
' Used across DRAFT routines
'----------------------------------------
Dim draftA$(0 To 1, 0 To 13), draftP$(1, 13), draftT$(1), draftYN$(1)
Dim teamMascots$(1), teamCoaches$(1), teamStadiums$(1)

Dim draftA!(1, 13, 6), attendance&(1), CK!(1)
Dim DA!(1), draftDF!(1, 13)
Dim draftRS!(1, 13, 13), SP!(1), draftX!(1, 13, 6)

Dim draftL%(1, 4), draftT%(1, 9), draftT1%(1, 20), draftTR%(1, 13, 12)
Dim draftV1%(1), draftV2%(1)

'----------------------------------------
' Used across NEWLDR routines
'----------------------------------------
Dim JB, tStats, ttStats
Dim TMR%

Dim AL$(600), TP$(31), TP1$(39)
Dim expanTT$(40, 15), TT1$(40, 15)
Dim TMA$(600)

Dim TMM$(600), TPP$(600)

'These should all be single
Dim statsAL!(1 To 600, 0 To 14), GM1!(1 To 40), GMA!(600)
Dim TT!(40, 15), TT1!(40, 15)
ReDim TYP!(600)
Dim W0L!(600), W1L!(600)

'----------------------------------------
' Used across HD2HD routines
'----------------------------------------
Dim AL!(50), AW!(50), HL!(50), HW!(50)
Dim R1!(50), R2!(50), hd2hdR3!(50), R4!(50)
Dim TAW!(40), THW!(40), THL!(40), TAL!(40)
Dim TR1!(40), TR2!(40), TR3!(40), TR4!(40)

'----------------------------------------
' Used across LOOKY/MERGE routines
'----------------------------------------
Dim HL%, HW%, NL%, NW%, VL%, VW%
Dim L%, W%

Dim mergeT%(100)
Dim mergeO%(100)

Dim lookyA!(0 To 14, 24)

'----------------------------------------
' Used across RECORDS routines
'----------------------------------------
'---> these should be Single
Dim REC!(50, 2), TREC!(125, 2)

Dim recordsA$(5), RC$(50, 4), TRC$(125, 3)

'----------------------------------------
' Used across RECCON routines
'----------------------------------------
Dim BRC!(32), TRC!(21), TRC1!(21)
Dim BRC$(32, 1), recconTRC$(21), recconTRC1$(21)

Dim TT$(20), recconTB$(25)

'----------------------------------------
' Used across SCHEDULE routines
'----------------------------------------
Dim BS%, NS%
Dim N$
Dim scheduleNG%(MAX_GAMES, 18) 'number of Games

Dim scheduleAP%(1), scheduleT%(9), scheduleZ1%(1 To 30)
Dim scheduleH$(1 To 20), scheduleV$(1 To 20)
Dim scheduleYN$(MAX_GAMES, 1)
Dim Z1$(1 To 30), Z2$(1 To 30)

'----------------------------------------
' Used across SEECON/SEENAT routines
'----------------------------------------
Dim DL$(60), OL$(60), NOL$(1 To 60), PR$(1200)
Dim DL!(60, 20), OL!(60, 20) ', seeconP(60)
Dim O1!(30), O2!(30), O3!(30), O4!(30), O5!(30), O6!(30)
Dim seenatZ!(260), seenatZ1!(260)
Dim seenatZ1$(260), seenatZ2$(260), seenatZ3$(260)
Dim PT#(1200, 5)
Dim T$(30)

'----------------------------------------
' Used across Game Routines
'----------------------------------------
Dim scheduleFile$

Dim Shared AP, B, C1, DY, CT, D, endGame, endAllGames, FO, F3, G9
Dim Shared H, halfTime, I, J, IN, M5, MJ, N, nbrLines, NTMS
Dim Shared P, P9, Q, S2, S9, sClockVal, shotClock
Dim Shared tickerIdx, TMT, TOU, U5, XM, XS

Dim Shared BO%, BU%, CM%, DK%, F3S%, FB%, FT%, J8%, LC%, N7%, NF%
Dim Shared PA%, PASS%, PB%, PT%, PZ%, ST%, TB%, TF%, X3%, X0%, X1%, XX%

Dim Shared F!, R0!

Dim Shared gameClock!, pbpDelay!, timeElapsed!

Dim Shared actualAttendance&

Dim Shared A1$, B1$, C1$, D1$, E1$, F1$, G1$, H1$, J$, PB$
Dim Shared tourneyFile$, U$, VT$, VT1$, W$, W1$, YN$

'-- FA%() I believe is related to tracking player fatigue
Dim Shared AP%(2), APT%(100, 1), B%(1, 13), CZ%(1), DT%(1), E%(13), FA%(1, 13), FY%(2)
Dim Shared G9%(1), HF%(1, 6), HT%(100), L%(1, 4), N%(16, 16, 0 To 4), NG%(18), NG1%(18)
Dim Shared O%(100), OF%(1), PC%(1), PR%(1, 1), ST%(32), SX%(32, 1, 14)
Dim Shared T2%(1, 20), TF%(1), TM%(1, 13), TP%(1), TR%(1, 9), V1%(1), V2%(1)
Dim Shared VG%(8), VH%(8), W%(1, 13, 1)

Dim Shared B1(0 To 1, 0 To 4)
Dim Shared C(1, 13, 6), D1(1), D2(1), D8(4, 3), DP(2, 4)
Dim Shared DF(1, 13)
Dim Shared F1(13), G4(13), G5(13), P2(1), P4(14), P5(14), P7(1)
Dim Shared QQ(1, 8, 13, 14), QR(1, 7, 14), RB(9), RS(1, 13, 13)
Dim Shared S1(1), S3(13), T0(1), T1(1), V9(14)
Dim Shared W2(1, 13), W3(1, 13), Z5(1), Z6(1), timePlayed(1, 13)

Dim Shared F5%(0 To 1, 0 To 8)

Dim Shared A!(0 To 1, 0 To 13, 0 To 24)
Dim Shared R3!(1), S!(1, 9), W0!(1, 13), W1!(1, 13)

'CRD stores attendance for stat files
'  I think this is because files are
'  operated on linearly, so 100 records
'  is the most supported?
Dim Shared avgAttendance&(1), CRD&(100)

Dim Shared AA$(1, 13), B$(1), D$(15), DB$(14)
Dim Shared gameMascot$(1), gameStadium$(1), gameCoach$(1)
Dim Shared H$(100), HO$(100), LC$(2), N$(16, 16, 0 To 4), O$(9), OB$(9)
Dim Shared PB$(1), PS$(4), R$(14), SITE$(100), SX$(32, 2)
Dim Shared PO$(1, 13)
Dim Shared X$(3), Y$(1), YN$(5), YN1$(3)

Dim Shared Z!(15), Z1!(15), Z2!(13, 13)

