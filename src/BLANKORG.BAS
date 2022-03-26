'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

DefInt A-Z

Dim O1(40), TM$(40), JS(46, 42) As Single, ZM%(46), NM$(46)
Dim H1$(40), PS$(40), D$(2), Z4$(40)
Dim BA$(40), DFT(40, 20) As Single, ER$(40), OFT(40, 20) As Single
Dim O2(40), Z2$(255), Z3$(255), Z1(255, 2) As Single, BAT(24), PIT(23)
Dim J As Single

4 Open "DEV.BAS" For Input As #2
For I = 0 To 1
    Input #2, D$(I)
Next
Close 2

5 Cls
APP = 0

4000 Spec$ = D$(1) + "*.CON"
null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then 4500
ReDim F$(0 To Count%)
For X = 1 To Count%
    F$(X) = Space$(12)
Next
F$(0) = Spec$
Call ReadFile(F$())
'CALL SortStr(VARPTR(F$(1)), Count%, 0)

1310 Cls
Color , 1
Cls
Locate , 28
Color 14
Print "SELECT ORGANIZATION TO ERASE": Color 15
SELECTION% = 1
Start% = 1
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Rows% = 20
Columns% = 4
Spacing% = 6
Row% = 4
Column% = 1
Locate 25, 1
Color 14
Print "ENTER";: Color 15
Print "-SELECTS ORGANIZATION   ";
Color 14
Print "HIT ESCAPE KEY TO EXIT";
Color 15

_MouseShow "DEFAULT"
MAMenu F$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If ExitCode% = 27 Then 99
F$ = F$(SELECTION%)
Locate 2, 33: Print F$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3910 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1310

3911 F$ = RTrim$(F$)
WE$ = Left$(F$, Len(F$) - 4)
LH$ = Left$(WE$, 8)

15 Cls
Locate 10, 8
    CALL CSMsgBox("WARNING!!!::: THIS WILL ERASE ALL COMPILED STATS AND LEAGUE FILES FOR THIS LEAGUE. IF YOU STILL WISH TO ERASE ALL LEAGUE FILES AND STATS, HIT 'Y' FOR YES", 26, Pass)
    I$ = GetKeyPress$
    CALL CSMsgBox("", 0, Pass)
    IF I$ <> "Y" THEN 1310

30 OPEN D$(1) + LH$ + ".CON" FOR INPUT AS #2
    INPUT #2, LG$
    FOR X = 1 TO 40
    INPUT #2, Z4$(X)
    NEXT
CLOSE 2
    FOR X = 1 TO 40
    IF Z4$(X) = "XXX" THEN 75
OPEN D$(1) + Z4$(X) + ".TMS" FOR INPUT AS #2
    INPUT #2, LG$, D1$, D2$
    FOR XW = 1 TO 30

35 INPUT #2, H1$(XW)
    NEXT
CLOSE 2

71 FOR XW = 1 TO 30
    IF H1$(XW) = CHR$(42) THEN 74

801 PRINT "DELETING "; H1$(XW); " STATS"
    KILL (D$(1) + H1$(XW))
    KILL (D$(1) + H1$(XW) + "S")

74 NEXT

75 NEXT
    CLS
    GOTO 4000

CLS

103 RUN "HELLO.EXE"

4500 LOCATE 10, 8
    CALL CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
    I$ = GetKeyPress$
    CALL CSMsgBox("", 0, Pass)
    GOTO 99


'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
