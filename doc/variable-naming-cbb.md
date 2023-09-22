a## Variables ##

#### Naming ####

???

#### Useage ####

|Variable	|Description			|
|---------------|-------------------------------|
|FG		|2-point % made			|
|3FG		|3-point % made			|
|%3A		|percent 3-point attempts	|
|FT		|Free Throw %			|
|ORB		|Off Rebound Rating		|
|DRB		|Def Rebound Rating		|
|RB		|Total Rebound Rating		|
|PA		|Passing Rating			|
|S		|Steal Rating			|
|B		|Block Rating			|
|CX		|Contribution Rating 		|
|PT 		|points scored			|
|F 		|Fouls				|
|FC		|Foul Commit Rating		|

CT = computer team
(NOT Coin Toss)

TP$() = category names Player
TP1$() = category names Team

X8 = ShotClock
C7 = ShotClockUserOpt
C7$() = ShotClockMenuOpt

FY% = 2 * Team FGA/G - League FGA/G

P9 = current player???
Contribution = # of times player can shoot/rebound/foul before being hampered

|Position	| Ex. Value	|Field			|Var		|
|---------------|---------------|-----------------------|---------------|
|Q$(0)		|Wolves		|Team Name		|B$		|
|Q$(1)		|6C 00	~ 108 	|Stamina Rating		|S1		|
|Q$(2)		|04 00 	~ 4	|Def FG% Adj		|D1		|
|Q$(3)		|61 00 	~ 97	|Def 3FGA%/FGA Adj	|V1%		|
|Q$(4)		|62 00 	~ 98	|Def 3FGA/FGA Adj	|V2%		|
|Q$(5)		|63 00 	~ 99	|Off Steal Rating	|L%(x, 0)	|
|Q$(6)		|03 00 	~ 3	|Off TO Rating		|L%(x, 1)	|
|Q$(7)		|01 00 	~ 1	|Def TO Adj		|L%(x, 2)	|
|Q$(8)		|61 00 	~ 97	|Def Foul Adj		|L%(x, 3)	|
|Q$(9)		|63 00 	~ 99	|99 if values for 4-7	|L%(x, 4)	|
|Q$(10)		|0F 00 	~ 15	|Text Color		|TR%(x, 0)	|
|Q$(11)		|05 00 	~ 5	|BG Color		|TR%(x, 1)	|
|Q$(12)		|E7 03 	~ 59139	|			|TR%(x, 2)	|
|Q$(13)		|38 00 	~ 56	|Team FGA/G		|TR%(x, 3)	|
|Q$(14)		|3A 00 	~ 58	|League FGA/G		|TR%(x, 4)	|
|Q$(15)		|1D 00 	~ 29	|Team Games Player	|TR%(x, 5)	|
|Q$(16)		|06 00 	~ 6	|HomeCourt Factor	|TR%(x, 6)	|
|Q$(17)		|18 00 	~ 24	|Wins			|TR%(x, 7)	|
|Q$(18)		|05 00	~ 5	|Losses			|TR%(x, 8)	|
|Q$(19)		|4D 00	~ 77	|Power Rating		|TR%(x, 9)	|

X$ = 602
	
FA% = Fatigue
TM% = PlayerTime

U5 = Opponent Choices
0 = Human

N7 = 0 (in current files)

PB% = Play-By-Play type
	0 = Verbose
	1 = Haffner Classic

AP = 1
only if GAME82 or TOURNEY files exist

TM = TimeOfDay

AP=1 still has something to do with AutoPlay

H = Game Half

T1 = Timeouts

T0 = "T?" on scoreboard
R3 = "RB" on scoreboard (rebounds?)
P2 = "TF" on scoreboard (team fouls?)

TR% = Team Colors
B$ = Team Names
