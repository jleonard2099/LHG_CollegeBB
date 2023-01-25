'----------------------------------------
' Used across more than one source file
'----------------------------------------
Dim Shared DT$, TM$

Dim Shared diskPaths$(3), Q$(0 To 500)
Dim Shared teamNames$(MAX_TEAMS)
Dim Shared teamIndex%(MAX_TEAMS)

Dim Shared DirCount%

'File size - does this really need shared?
Dim Shared Y As Long


Dim A1$, C$, S$
Dim avgAttendance%, CK, defFGPctAdj%, finalUpdate%
Dim replayLosses%, staminaRating%, TC!, replayWins%
Dim BO%, V1%, V2%

Dim A$(0 To 15)
Dim L%(0 To 4), T1%(20)
Dim A!(0 To 14, 0 To 13), DF!(13), RS!(13, 13), X!(13, 13)
Dim W0!(15), W1!(15)
Dim Z!(15), Z1!(15)

Dim homeTeam$(MAX_GAMES), visitingTeam$(MAX_GAMES)

Dim homeScore%(MAX_GAMES), visitingScore%(MAX_GAMES)
Dim T%(0 To 9)

'----------------------------------------
' Used across ADDTOURN.BAS,
'   BRACKET.BAS routines
'----------------------------------------
Dim N%(17, 17, 3) 'REGION, SEED NUMBER, TEAM#/MODE OF PLAY (0,1)
Dim NN%(17) 'Number of Teams Per Region (up to 16 Regions)

Dim slotName$(17, 17, 0 To 3) 'Team Names for Each Seed Slot
Dim regionNames$(1 To 17) 'Region Names


Dim yearNumber$(MAX_TEAMS, 3)

Dim tourneyDefaults%(14) 'TOURNAMENT DEFAULTS
Dim consolationSetting%(1) '0-CONSOLATION IN REGIONS, 1-CONSOLATION IN CHAMPIONSHIP

'----------------------------------------
' Used across CARVIEW.BAS routines
'----------------------------------------
Dim AR$(62)
Dim ARS!(15, 62, 15)
Dim W0S!(15, 62), W1S!(15, 62)
Dim careerT%(0 To 34)

'----------------------------------------
' Used across CNGRAT.BAS routines
'----------------------------------------
'14 positions, 0 to 13 array index
Dim cngratA$(0 To 13), P$(0 To 13)

'----------------------------------------
' Used across COMPARE.BAS routines
'----------------------------------------
Dim compareA!(15, 14)
Dim A1!(13, 6), X1!(13, 6)
Dim HP%(100), compareL%(6), OP%(100), compareT%(34)
Dim B$(14), HP$(100), OP$(100)

'----------------------------------------
' Used across COMPNAT.BAS routines
'----------------------------------------
Dim compnatH$(100), compnatN$(0 To 14)
Dim LD!(1 To 250, 0 To 2)
Dim NDL!(1 To 40, 1 To 20), NLD!(1 To 250, 1 To 2), NOL!(1 To 40, 1 To 20)
Dim S!(0 To 14, 0 To 26)

Dim DT!(21), OT!(21)

Dim NDL$(40), NZ0$(1 To 250), NZ1$(30), NZ3$(1 To 250)

Dim CP$(25), O$(100)
Dim Z0$(1 To 250), Z3$(1 To 250)

'----------------------------------------
' Used across DRAFT.BAS routines
'----------------------------------------
Dim draftA$(0 To 1, 0 To 13), draftP$(1, 13), draftT$(1), draftYN$(1)
Dim A1$(1), C$(1), S$(1)

Dim draftA!(1, 13, 6), attendance&(1), CK!(1)
Dim DA!(1), draftDF!(1, 13)
Dim draftRS!(1, 13, 13), SP!(1), draftX!(1, 13, 6)

Dim draftL%(1, 4), draftT%(1, 9), draftT1%(1, 20), TR%(1, 13, 12)
Dim V1%(1), V2%(1)

'----------------------------------------
' Used across EXPANCON.BAS routines
'----------------------------------------
Dim JB, TS, TTS
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
' Used across HD2HD.BAS routines
'----------------------------------------
Dim AL!(50), AW!(50), HL!(50), HW!(50)
Dim R1!(50), R2!(50), R3!(50), R4!(50)
Dim TAW!(40), THW!(40), THL!(40), TAL!(40)
Dim TR1!(40), TR2!(40), TR3!(40), TR4!(40)

'----------------------------------------
' Used across LOOKY/MERGE.BAS routines
'----------------------------------------
Dim HL%, HW%, NL%, NW%, VL%, VW%
Dim L%, W%

Dim mergeT%(100)
Dim O%(100), P%(100)

Dim lookyA!(0 To 14, 24)

'----------------------------------------
' Used across RECORDS.BAS routines
'----------------------------------------
'---> these should be Single
Dim REC!(50, 2), TREC!(125, 2)

Dim recordsA$(5), RC$(50, 4), TRC$(125, 3)

'----------------------------------------
' Used across RECCON.BAS routines
'----------------------------------------
Dim BRC!(32), TRC!(21), TRC1!(21)
Dim BRC$(32, 1), recconTRC$(21), recconTRC1$(21)

Dim TT$(20), TB$(25)

'----------------------------------------
' Used across SCHEDULE.BAS routines
'----------------------------------------
Dim BS%, NS%
Dim N$
Dim NG%(MAX_GAMES, 18) 'number of Games

Dim scheduleAP%(1), scheduleT%(9), scheduleZ1%(1 To 30)
Dim scheduleH$(1 To 20), scheduleV$(1 To 20)
Dim scheduleYN$(MAX_GAMES, 1)
Dim Z1$(1 To 30), Z2$(1 To 30)

'----------------------------------------
' Used across SEECON/SEENAT.BAS routines
'----------------------------------------
Dim DL$(60), OL$(60), NOL$(60), PR$(1200)
Dim DL!(60, 20), OL!(60, 20) ', seeconP(60)
Dim O1!(30), O2!(30), O3!(30), O4!(30), O5!(30), O6!(30)
Dim seenatZ!(260), seenatZ1!(260)
Dim seenatZ1$(260), seenatZ2$(260), seenatZ3$(260)
Dim PT#(1200, 5)
Dim T$(30)

'----------------------------------------
' Used across Game Routines
'----------------------------------------

