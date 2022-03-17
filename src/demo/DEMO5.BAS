'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

DEFINT A-Z

DECLARE SUB DRAWBOX ()
DECLARE SUB SHOTSTRING ()

DIM NG%(18), NG1%(18), AP%(2), HF%(1, 6), DP(2, 4), FA%(1, 13), FY%(2), OF%(1), DT%(1), TM%(1, 13)
DIM D8(4, 3), PR%(1, 1), PC%(1), CZ%(1), OB$(9), DB$(14)
DIM R$(14), p$(1), X$(3), C7$(2), TS$(1), CM$(1), U5$(3), LC$(2), TB$(1)
DIM TR%(1, 9), Y$(1), Z5(1), Z6(1), PS$(4), D2(1), S1(1), RB(9), P5(14), P4(14)
DIM vg%(8), vh%(8), V1%(1), V2%(1), L%(1, 4), P2(1), YN$(5), PB$(1), N%(16, 16, 3), N$(16, 16, 3)
DIM YN1$(3), TP%(1), TF%(1),Q$(250),O$(9),V%(500)
DIM O%(100), HT%(100), HO$(100), H$(100), SX%(32, 1, 14), ST%(32)
DIM APT%(100, 1), CRD(100) AS LONG, SITE$(100)
DIM G9%(1), F5%(1, 8), W%(1, 13, 1), IY(1), O0%(1), V$(500), Z2(13, 13) AS SINGLE
DIM F1(13), S3(13), G4(13), G5(13), D$(15), D1(1), W0(1, 13), W1(1, 13)
DIM A(1, 13, 24) AS SINGLE, AA$(1, 13), B$(1), P7(1), R3(1), T0(1), T1(1), SX$(32, 2), W2(1, 13), W3(1, 13)
DIM B1(1, 4), S(1, 9), V9(14), C(1, 13, 6), ZZ(1, 13), B%(1, 13, 18), Z(15) AS SINGLE, Z1(15) AS SINGLE
DIM C$(1),S$(1),A1$(1),ATT(1) AS LONG,DF(1,13),PO$(1,13),RS(1,13,13),T2%(1,20)
DIM E%(13), QQ(1, 8, 13, 14), QR(1, 7, 14)

COMMON SHARED APT%(), CRD() AS LONG, SITE$(), QQ(), QR()
COMMON SHARED I, TR%(), F5%(), P9, L%(), A() AS SINGLE, C(), V1%(), V2%()
COMMON SHARED p$(), B$(), S1(), D1(), AA$(), FY%(), B%(), U$, PASS%, U6 AS SINGLE, PT%, B1(), p, B, P7()
COMMON SHARED DK%, PZ%, FT%, X3%, XX%, FB%, F AS SINGLE, PB%, NL%
COMMON SHARED A1$, B1$, C1$, D1$, E1$, F1$, G1$, H1$, J$, vg%(), vh%()
COMMON SHARED AP,U5,HT%,G9,EG%,TMT,H,T AS SINGLE,TE AS SINGLE ,SX$(),SX%(),R$(),TS%,ST%,ST%()
COMMON SHARED AP%(),N,S(),ZZ(),W%(),V9(),T0(),R3()
COMMON SHARED DB$(),DT%(),D2(),OB$(),OF%(),S3(),F1(),G5(),G4(),NF%
COMMON SHARED FF$,NG1%(),YN1$(),YN$(),NG%(),VT$,W$,W1$,VT1$
COMMON SHARED Z2() AS SINGLE,W2(),W0(),Z1() AS SINGLE,Z() AS SINGLE,W3(),O%(),HO$(),HT%(),H$(),Q
COMMON SHARED W1(),Y$(),Q$(),FA%(),X,P2(),T1(),D,CTO%,XM,C1
COMMON SHARED Z5(),Z6(),RB(),X8,C7,DY,S9,D$(),JJ,G9%(),XS,CM%
COMMON SHARED TB%,TF%,N7%,TOU,TF$,N$(),N%(),PA%,HF%(),S2
COMMON SHARED DP(),D8(),X$(),C7$(),O$(),TS$(),CM$(),U5$(),LC$()
COMMON SHARED TB$(),PB$(),J,MJ,P5(),P4(),PS$(),TM%(),CZ%(),PR%(),IN,M5
COMMON SHARED F3S%,J8%,F3,BO%,V$(),V%(),NT,X0%,X1%,TP%(),LC%,FO,PB$
COMMON SHARED BU%,YN$,Y2 as long,R0 AS SINGLE,CT
COMMON SHARED C$(),S$(),A1$(),ATT() AS LONG,DF(),PO$(),RS(),T2%()
COMMON SHARED E%(), Y3 AS LONG

SUB RECORDSCHECK ()
DIM RC$(50, 4), REC(50, 2) AS SINGLE, TRC$(125, 3), TREC(125, 2) AS SINGLE
DIM V8(1, 14)
DIM VERSUS$(1)
	VERSUS$(0) = "AT": VERSUS$(1) = "VS."
	IF NOT _FILEEXISTS(P$(1) + "RECORDS.GAM") THEN
	 TREC(2, 0) = 999
	 TREC(4, 0) = 999
	 TREC(7, 0) = 999
	 TREC(9, 0) = 999
	 TREC(11, 0) = 999
	 TREC(13, 0) = 999
	 TREC(15, 0) = 999
	 TREC(17, 0) = 999
	 TREC(19, 0) = 999
	 TREC(21, 0) = 999
	 TREC(23, 0) = 999
	 TREC(25, 0) = 999
	 TREC(27, 0) = 999
	 TREC(29, 0) = 999
	 TREC(31, 0) = 999
	 TREC(33, 0) = 999
	 TREC(35, 0) = 999
	 TREC(37, 0) = 999
	 TREC(43, 0) = 999
	 TREC(45, 0) = 999
	 TREC(61, 0) = 999
	 TREC(63, 0) = 999
	 TREC(65, 0) = 999
	 TREC(67, 0) = 999
	 TREC(73, 0) = 999
	 TREC(75, 0) = 999
	 TREC(81, 0) = 999
	 TREC(83, 0) = 999
	 TREC(87, 0) = 999
	 TREC(89, 0) = 999
	 TREC(92, 0) = 999
	 TREC(94, 0) = 999
	 TREC(97, 0) = 999
	 TREC(99, 0) = 999
	 TREC(105, 0) = 999
	 TREC(107, 0) = 999
	 TREC(113, 0) = 999
	 TREC(115, 0) = 999
	 TREC(120, 0) = 999
	 TREC(123, 0) = 999
	 TREC(125, 0) = 999
	 GOTO 505
	END IF
OPEN P$(1) + "RECORDS.GAM" FOR INPUT AS #1
	FOR I = 0 TO 48
	FOR I1 = 0 TO 2
	INPUT #1, REC(I, I1)
	NEXT
	FOR I1 = 0 TO 4
	INPUT #1, RC$(I, I1)
	NEXT
	NEXT
FOR I = 0 TO 125
FOR I1 = 0 TO 2
INPUT #1, TREC(I, I1)
NEXT
FOR I1 = 0 TO 3
INPUT #1, TRC$(I, I1)
NEXT
NEXT
CLOSE 1

505 'CHECK THE INDIVIDUAL RECORDS
	 FOR I = 0 TO 1
	FOR I1 = 0 TO 13

' SCORING RECORDS FOR POINTS IN A GAME, HALF, QUARTER AND OVERTIME PERIOD
	IF A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0) > REC(0, 0) THEN
	  REC(0, 0) = A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0) ' POINTS
	  REC(0, 1) = 1                                             ' TIMES DONE
	  REC(0, 2) = 0                                             ' GAME
	  RC$(0, 0) = AA$(I, I1)                                 ' NAME
	  RC$(0, 1) = B$(I)                                      ' TEAM
	  RC$(0, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(0, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(0, 4) = DATE$                                         ' DATE
	END IF
	IF QQ(I, 1, I1, 13) > REC(1, 0) THEN
	  REC(1, 0) = QQ(I, 1, I1, 13)                              ' POINTS
	  REC(1, 1) = 1                                             ' TIMES DONE
	  REC(1, 2) = 1                                             ' HALF
	  RC$(1, 0) = AA$(I, I1)                                 ' NAME
	  RC$(1, 1) = B$(I)                                      ' TEAM
	  RC$(1, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(1, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(1, 4) = DATE$                                         ' DATE
	END IF
	 IF QQ(I, 2, I1, 13) > REC(1, 0) THEN
	  REC(1, 0) = QQ(I, 2, I1, 13)                           ' POINTS
	  REC(1, 1) = 1                                             ' TIMES DONE
	  REC(1, 2) = 2                                             ' HALF
	  RC$(1, 0) = AA$(I, I1)                                 ' NAME
	  RC$(1, 1) = B$(I)                                      ' TEAM
	  RC$(1, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(1, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(1, 4) = DATE$                                         ' DATE
	END IF
	 FOR I2 = 3 TO 8
	IF QQ(I, I2, I1, 13) > REC(3, 0) THEN
	  REC(3, 0) = QQ(I, I2, I1, 13)  ' POINTS
	  REC(3, 1) = 1                                             ' TIMES DONE
	  REC(3, 2) = I2                                             ' QTR
	  RC$(3, 0) = AA$(I, I1)                                 ' NAME
	  RC$(3, 1) = B$(I)                                      ' TEAM
	  RC$(3, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(3, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(3, 4) = DATE$                                         ' DATE
	END IF
	NEXT I2

' FIELD GOAL PERCENTAGE RECORDS FOR HIGH %, MOST W/NO MISSES, MOST W/NONE MADE
	IF A(I, I1, 8) < 15 OR A(I, I1, 7) = 0 THEN 900
	IF A(I, I1, 8) / A(I, I1, 7) > REC(4, 0) THEN
	  REC(4, 0) = A(I, I1, 8) / A(I, I1, 7)                     ' PCTG
	  REC(4, 1) = A(I, I1, 8)                                   ' FGM
	  REC(4, 2) = A(I, I1, 7)                                   ' FGA
	  RC$(4, 0) = AA$(I, I1)                                 ' NAME
	  RC$(4, 1) = B$(I)                                      ' TEAM
	  RC$(4, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(4, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(4, 4) = DATE$                                         ' DATE
	END IF

900
	IF A(I, I1, 8) = A(I, I1, 7) AND A(I, I1, 7) > REC(5, 0) THEN
	  REC(5, 0) = A(I, I1, 7)                                   ' MOST FGM
	  RC$(5, 0) = AA$(I, I1)                                 ' NAME
	  RC$(5, 1) = B$(I)                                      ' TEAM
	  RC$(5, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(5, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(5, 4) = DATE$                                         ' DATE
	END IF
	IF A(I, I1, 8) = 0 AND A(I, I1, 7) > REC(6, 0) THEN
	  REC(6, 0) = A(I, I1, 7)                                   ' 0 FGM /FGA
	  RC$(6, 0) = AA$(I, I1)                                 ' NAME
	  RC$(6, 1) = B$(I)                                      ' TEAM
	  RC$(6, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(6, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(6, 4) = DATE$                                         ' DATE
	END IF

' FIELD GOALS MADE IN A GAME, HALF, QUARTER AND OVERTIME PERIOD
	IF A(I, I1, 8) > REC(7, 0) THEN
	  REC(7, 0) = A(I, I1, 8)                                   ' FGM
	  REC(7, 1) = 1                                             ' TIMES DONE
	  REC(7, 2) = 0                                             ' GAME
	  RC$(7, 0) = AA$(I, I1)                                 ' NAME
	  RC$(7, 1) = B$(I)                                      ' TEAM
	  RC$(7, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(7, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(7, 4) = DATE$                                         ' DATE
	END IF
	IF QQ(I, 1, I1, 2)  > REC(8, 0) THEN
	  REC(8, 0) = QQ(I, 1, I1, 2)                            ' FGM
	  REC(8, 1) = 1                                             ' TIMES DONE
	  REC(8, 2) = 1                                             ' HALF
	  RC$(8, 0) = AA$(I, I1)                                 ' NAME
	  RC$(8, 1) = B$(I)                                      ' TEAM
	  RC$(8, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(8, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(8, 4) = DATE$                                         ' DATE
	END IF
	IF QQ(I, 2, I1, 2)  > REC(8, 0) THEN
	  REC(8, 0) = QQ(I, 2, I1, 2)                          ' FGM
	  REC(8, 1) = 1                                             ' TIMES DONE
	  REC(8, 2) = 2                                             ' HALF
	  RC$(8, 0) = AA$(I, I1)                                 ' NAME
	  RC$(8, 1) = B$(I)                                      ' TEAM
	  RC$(8, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(8, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(8, 4) = DATE$                                         ' DATE
	END IF
	 FOR I2 = 3 TO 8
	IF QQ(I, I2, I1, 2) > REC(10, 0) THEN
	  REC(10, 0) = QQ(I, I2, I1, 2)                              ' FGM
	  REC(10, 1) = 1                                             ' TIMES DONE
	  REC(10, 2) = I2                                             ' QTR
	  RC$(10, 0) = AA$(I, I1)                                 ' NAME
	  RC$(10, 1) = B$(I)                                      ' TEAM
	  RC$(10, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(10, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(10, 4) = DATE$                                         ' DATE
	END IF
	NEXT I2

' FIELD GOAL ATTEMPTS MADE IN A GAME, HALF, QUARTER AND OVERTIME PERIOD
	IF A(I, I1, 7) > REC(11, 0) THEN
	  REC(11, 0) = A(I, I1, 7)                                   ' FGA
	  REC(11, 1) = 1                                             ' TIMES DONE
	  REC(11, 2) = 0                                             ' GAME
	  RC$(11, 0) = AA$(I, I1)                                 ' NAME
	  RC$(11, 1) = B$(I)                                      ' TEAM
	  RC$(11, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(11, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(11, 4) = DATE$                                         ' DATE
	END IF
	 IF QQ(I, 1, I1, 1)  > REC(12, 0) THEN
	  REC(12, 0) = QQ(I, 1, I1, 1)                    ' FGA
	  REC(12, 1) = 1                                             ' TIMES DONE
	  REC(12, 2) = 1                                             ' HALF
	  RC$(12, 0) = AA$(I, I1)                                 ' NAME
	  RC$(12, 1) = B$(I)                                      ' TEAM
	  RC$(12, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(12, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(12, 4) = DATE$                                         ' DATE
	END IF
	IF QQ(I, 2, I1, 1) > REC(12, 0) THEN
	  REC(12, 0) = QQ(I, 2, I1, 1)                          ' FGA
	  REC(12, 1) = 1                                             ' TIMES DONE
	  REC(12, 2) = 2                                             ' HALF
	  RC$(12, 0) = AA$(I, I1)                                 ' NAME
	  RC$(12, 1) = B$(I)                                      ' TEAM
	  RC$(12, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(12, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(12, 4) = DATE$                                         ' DATE
	END IF
	 FOR I2 = 3 TO 8
	IF QQ(I, I2, I1, 1) > REC(14, 0) THEN
	  REC(14, 0) = QQ(I, I2, I1, 1)                              ' FGA
	  REC(14, 1) = 1                                             ' TIMES DONE
	  REC(14, 2) = I2                                             ' QTR
	  RC$(14, 0) = AA$(I, I1)                                 ' NAME
	  RC$(14, 1) = B$(I)                                      ' TEAM
	  RC$(14, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(14, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(14, 4) = DATE$                                         ' DATE
	END IF
	NEXT I2

' THREE-POINT FIELD GOALS MOST MADE NO MISSES, MOST ATT NONE MADE
	IF W%(I, I1, 0) = W%(I, I1, 1) AND W%(I, I1, 0) > REC(15, 0) THEN
	  REC(15, 0) = W%(I, I1, 0)                                  ' MOST 3-FGM
	  RC$(15, 0) = AA$(I, I1)                                 ' NAME
	  RC$(15, 1) = B$(I)                                      ' TEAM
	  RC$(15, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(15, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(15, 4) = DATE$                                         ' DATE
	END IF
	IF W%(I, I1, 0) = 0 AND W%(I, I1, 1) > REC(16, 0) THEN
	  REC(16, 0) = W%(I, I1, 1)                                   ' 0 3FGM /3FGA
	  RC$(16, 0) = AA$(I, I1)                                 ' NAME
	  RC$(16, 1) = B$(I)                                      ' TEAM
	  RC$(16, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(16, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(16, 4) = DATE$                                         ' DATE
	END IF

' 3-FIELD GOALS MADE IN A GAME, HALF, QUARTER AND OVERTIME PERIOD
	IF W%(I, I1, 0) > REC(17, 0) THEN
	  REC(17, 0) = W%(I, I1, 0)                                   ' 3FGM
	  REC(17, 1) = 1                                             ' TIMES DONE
	  REC(17, 2) = 0                                             ' GAME
	  RC$(17, 0) = AA$(I, I1)                                 ' NAME
	  RC$(17, 1) = B$(I)                                      ' TEAM
	  RC$(17, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(17, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(17, 4) = DATE$                                         ' DATE
	END IF
	IF QQ(I, 1, I1, 10) > REC(18, 0) THEN
	  REC(18, 0) = QQ(I, 1, I1, 10)              ' 3FGM
	  REC(18, 1) = 1                                             ' TIMES DONE
	  REC(18, 2) = 1                                             ' HALF
	  RC$(18, 0) = AA$(I, I1)                                 ' NAME
	  RC$(18, 1) = B$(I)                                      ' TEAM
	  RC$(18, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(18, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(18, 4) = DATE$                                         ' DATE
	END IF
	IF QQ(I, 2, I1, 10) > REC(18, 0) THEN
	  REC(18, 0) = QQ(I, 2, I1, 10)                ' 3FGM
	  REC(18, 1) = 1                                             ' TIMES DONE
	  REC(18, 2) = 2                                             ' HALF
	  RC$(18, 0) = AA$(I, I1)                                 ' NAME
	  RC$(18, 1) = B$(I)                                      ' TEAM
	  RC$(18, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(18, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(18, 4) = DATE$                                         ' DATE
	END IF
	 FOR I2 = 3 TO 8
	IF QQ(I, I2, I1, 10) > REC(20, 0) THEN
	  REC(20, 0) = QQ(I, I2, I1, 10)                              ' 3FGM
	  REC(20, 1) = 1                                             ' TIMES DONE
	  REC(20, 2) = I2                                             ' QTR
	  RC$(20, 0) = AA$(I, I1)                                 ' NAME
	  RC$(20, 1) = B$(I)                                      ' TEAM
	  RC$(20, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(20, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(20, 4) = DATE$                                         ' DATE
	END IF
	NEXT I2

' 3-FIELD GOALS ATT IN A GAME, HALF
	IF W%(I, I1, 1) > REC(21, 0) THEN
	  REC(21, 0) = W%(I, I1, 1)                                   ' 3FGA
	  REC(21, 1) = 1                                             ' TIMES DONE
	  REC(21, 2) = 0                                             ' GAME
	  RC$(21, 0) = AA$(I, I1)                                 ' NAME
	  RC$(21, 1) = B$(I)                                      ' TEAM
	  RC$(21, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(21, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(21, 4) = DATE$                                         ' DATE
	END IF
	IF QQ(I, 1, I1, 11) > REC(22, 0) THEN
	  REC(22, 0) = QQ(I, 1, I1, 11)                     ' 3FGA
	  REC(22, 1) = 1                                             ' TIMES DONE
	  REC(22, 2) = 1                                             ' HALF
	  RC$(22, 0) = AA$(I, I1)                                 ' NAME
	  RC$(22, 1) = B$(I)                                      ' TEAM
	  RC$(22, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(22, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(22, 4) = DATE$                                         ' DATE
	END IF
	IF QQ(I, 2, I1, 11) > REC(22, 0) THEN
	  REC(22, 0) = QQ(I, 2, I1, 10)                         ' 3FGA
	  REC(22, 1) = 1                                             ' TIMES DONE
	  REC(22, 2) = 2                                             ' HALF
	  RC$(22, 0) = AA$(I, I1)                                 ' NAME
	  RC$(22, 1) = B$(I)                                      ' TEAM
	  RC$(22, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(22, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(22, 4) = DATE$                                         ' DATE
	END IF
' FREE THROWS -  MOST MADE NO MISSES, MOST ATT NONE MADE
	IF A(I, I1, 9) = A(I, I1, 10) AND A(I, I1, 10) > REC(23, 0) THEN
	  REC(23, 0) = A(I, I1, 10)                                  ' MOST FTM
	  RC$(23, 0) = AA$(I, I1)                                 ' NAME
	  RC$(23, 1) = B$(I)                                      ' TEAM
	  RC$(23, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(23, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(23, 4) = DATE$                                         ' DATE
	END IF
	IF A(I, I1, 10) = 0 AND A(I, I1, 9) > REC(24, 0) THEN
	  REC(24, 0) = A(I, I1, 9)                                   ' 0 FTM /FTA
	  RC$(24, 0) = AA$(I, I1)                                 ' NAME
	  RC$(24, 1) = B$(I)                                      ' TEAM
	  RC$(24, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(24, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(24, 4) = DATE$                                         ' DATE
	END IF

' MADE FREE THROWS IN A GAME, HALF, QUARTER
	IF A(I, I1, 10) > REC(25, 0) THEN
	  REC(25, 0) = A(I, I1, 10)  ' FTM
	  REC(25, 1) = 1                                             ' TIMES DONE
	  REC(25, 2) = 0                                             ' GAME
	  RC$(25, 0) = AA$(I, I1)                                 ' NAME
	  RC$(25, 1) = B$(I)                                      ' TEAM
	  RC$(25, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(25, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(25, 4) = DATE$                                         ' DATE
	END IF
	IF QQ(I, 1, I1, 4)  > REC(26, 0) THEN
	  REC(26, 0) = QQ(I, 1, I1, 4)  ' FTM
	  REC(26, 1) = 1                                             ' TIMES DONE
	  REC(26, 2) = 1                                             ' HALF
	  RC$(26, 0) = AA$(I, I1)                                 ' NAME
	  RC$(26, 1) = B$(I)                                      ' TEAM
	  RC$(26, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(26, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(26, 4) = DATE$                                         ' DATE
	END IF
	IF QQ(I, 2, I1, 4)  > REC(26, 0) THEN
	  REC(26, 0) = QQ(I, 2, I1, 4)    ' FTM
	  REC(26, 1) = 1                                             ' TIMES DONE
	  REC(26, 2) = 2                                             ' HALF
	  RC$(26, 0) = AA$(I, I1)                                 ' NAME
	  RC$(26, 1) = B$(I)                                      ' TEAM
	  RC$(26, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(26, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(26, 4) = DATE$                                         ' DATE
	END IF

' FREE THROWS ATTEMPTS IN A GAME, HALF, QUARTER
	IF A(I, I1, 9) > REC(28, 0) THEN
	  REC(28, 0) = A(I, I1, 9)  ' FTA
	  REC(28, 1) = 1                                             ' TIMES DONE
	  REC(28, 2) = 0                                             ' GAME
	  RC$(28, 0) = AA$(I, I1)                                 ' NAME
	  RC$(28, 1) = B$(I)                                      ' TEAM
	  RC$(28, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(28, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(28, 4) = DATE$                                         ' DATE
	END IF
	IF QQ(I, 1, I1, 3) > REC(29, 0) THEN
	  REC(29, 0) = QQ(I, 1, I1, 3) ' FTA
	  REC(29, 1) = 1                                             ' TIMES DONE
	  REC(29, 2) = 1                                             ' HALF
	  RC$(29, 0) = AA$(I, I1)                                 ' NAME
	  RC$(29, 1) = B$(I)                                      ' TEAM
	  RC$(29, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(29, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(29, 4) = DATE$                                         ' DATE
	END IF
	IF QQ(I, 2, I1, 3) > REC(29, 0) THEN
	  REC(29, 0) = QQ(I, 2, I1, 3)  ' FTM
	  REC(29, 1) = 1                                             ' TIMES DONE
	  REC(29, 2) = 2                                             ' HALF
	  RC$(29, 0) = AA$(I, I1)                                 ' NAME
	  RC$(29, 1) = B$(I)                                      ' TEAM
	  RC$(29, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(29, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(29, 4) = DATE$                                         ' DATE
	END IF

' TOTAL REBOUNDS IN A GAME, HALF, QUARTER
	IF A(I, I1, 11) > REC(31, 0) THEN
	  REC(31, 0) = A(I, I1, 11)  ' FTA
	  REC(31, 1) = 1                                             ' TIMES DONE
	  REC(31, 2) = 0                                             ' GAME
	  RC$(31, 0) = AA$(I, I1)                                 ' NAME
	  RC$(31, 1) = B$(I)                                      ' TEAM
	  RC$(31, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(31, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(31, 4) = DATE$                                         ' DATE
	END IF
	IF QQ(I, 1, I1, 5) > REC(32, 0) THEN
	  REC(32, 0) = QQ(I, 1, I1, 5)  ' RB
	  REC(32, 1) = 1                                             ' TIMES DONE
	  REC(32, 2) = 1                                             ' HALF
	  RC$(32, 0) = AA$(I, I1)                                 ' NAME
	  RC$(32, 1) = B$(I)                                      ' TEAM
	  RC$(32, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(32, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(32, 4) = DATE$                                         ' DATE
	END IF
	IF QQ(I, 2, I1, 5) > REC(32, 0) THEN
	  REC(32, 0) = QQ(I, 2, I1, 5)  ' RB
	  REC(32, 1) = 1                                             ' TIMES DONE
	  REC(32, 2) = 2                                             ' HALF
	  RC$(32, 0) = AA$(I, I1)                                 ' NAME
	  RC$(32, 1) = B$(I)                                      ' TEAM
	  RC$(32, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(32, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(32, 4) = DATE$                                         ' DATE
	END IF

' TOTAL OFFENSIVE REBOUNDS IN A GAME, HALF, QUARTER
	IF A(I, I1, 24) > REC(34, 0) THEN
	  REC(34, 0) = A(I, I1, 24)  ' FTA
	  REC(34, 1) = 1                                             ' TIMES DONE
	  REC(34, 2) = 0                                             ' GAME
	  RC$(34, 0) = AA$(I, I1)                                 ' NAME
	  RC$(34, 1) = B$(I)                                      ' TEAM
	  RC$(34, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(34, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(34, 4) = DATE$                                         ' DATE
	END IF
	IF QQ(I, 1, I1, 12) > REC(35, 0) THEN
	  REC(35, 0) = QQ(I, 1, I1, 12)  ' RB
	  REC(35, 1) = 1                                             ' TIMES DONE
	  REC(35, 2) = 1                                             ' HALF
	  RC$(35, 0) = AA$(I, I1)                                 ' NAME
	  RC$(35, 1) = B$(I)                                      ' TEAM
	  RC$(35, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(35, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(35, 4) = DATE$                                         ' DATE
	END IF
	IF QQ(I, 2, I1, 12) > REC(35, 0) THEN
	  REC(35, 0) = QQ(I, 2, I1, 12)  ' RB
	  REC(35, 1) = 1                                             ' TIMES DONE
	  REC(35, 2) = 2                                             ' HALF
	  RC$(35, 0) = AA$(I, I1)                                 ' NAME
	  RC$(35, 1) = B$(I)                                      ' TEAM
	  RC$(35, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(35, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(35, 4) = DATE$                                         ' DATE
	END IF

' TOTAL DEFENSIVE REBOUNDS IN A GAME, HALF, QUARTER
	IF A(I, I1, 11) - A(I, I1, 24) > REC(37, 0) THEN
	  REC(37, 0) = A(I, I1, 11) - A(I, I1, 24)  ' FTA
	  REC(37, 1) = 1                                             ' TIMES DONE
	  REC(37, 2) = 0                                             ' GAME
	  RC$(37, 0) = AA$(I, I1)                                 ' NAME
	  RC$(37, 1) = B$(I)                                      ' TEAM
	  RC$(37, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(37, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(37, 4) = DATE$                                         ' DATE
	END IF
	IF QQ(I, 1, I1, 5) - QQ(I, 1, I1, 12) > REC(38, 0) THEN
	  REC(38, 0) = QQ(I, 1, I1, 5)  - QQ(I, 1, I1, 12)  ' RB
	  REC(38, 1) = 1                                             ' TIMES DONE
	  REC(38, 2) = 1                                             ' HALF
	  RC$(38, 0) = AA$(I, I1)                                 ' NAME
	  RC$(38, 1) = B$(I)                                      ' TEAM
	  RC$(38, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(38, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(38, 4) = DATE$                                         ' DATE
	END IF
	IF QQ(I, 2, I1, 5) - QQ(I, 2, I1, 12) > REC(38, 0) THEN
	  REC(38, 0) = QQ(I, 2, I1, 5) - QQ(I, 2, I1, 12) ' RB
	  REC(38, 1) = 1                                             ' TIMES DONE
	  REC(38, 2) = 2                                             ' HALF
	  RC$(38, 0) = AA$(I, I1)                                 ' NAME
	  RC$(38, 1) = B$(I)                                      ' TEAM
	  RC$(38, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(38, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(38, 4) = DATE$                                         ' DATE
	END IF

' ASSISTS IN A GAME, HALF, QUARTER
	IF A(I, I1, 14) > REC(40, 0) THEN
	  REC(40, 0) = A(I, I1, 14)  ' FTA
	  REC(40, 1) = 1                                             ' TIMES DONE
	  REC(40, 2) = 0                                             ' GAME
	  RC$(40, 0) = AA$(I, I1)                                 ' NAME
	  RC$(40, 1) = B$(I)                                      ' TEAM
	  RC$(40, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(40, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(40, 4) = DATE$                                         ' DATE
	END IF
	IF QQ(I, 1, I1, 8) > REC(41, 0) THEN
	  REC(41, 0) = QQ(I, 1, I1, 8)   ' FTA
	  REC(41, 1) = 1                                             ' TIMES DONE
	  REC(41, 2) = 1                                             ' HALF
	  RC$(41, 0) = AA$(I, I1)                                 ' NAME
	  RC$(41, 1) = B$(I)                                      ' TEAM
	  RC$(41, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(41, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(41, 4) = DATE$                                         ' DATE
	END IF
	IF QQ(I, 2, I1, 8) > REC(41, 0) THEN
	  REC(41, 0) = QQ(I, 2, I1, 8)                    ' FTM
	  REC(41, 1) = 1                                             ' TIMES DONE
	  REC(41, 2) = 2                                             ' HALF
	  RC$(41, 0) = AA$(I, I1)                                 ' NAME
	  RC$(41, 1) = B$(I)                                      ' TEAM
	  RC$(41, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(41, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(41, 4) = DATE$                                         ' DATE
	END IF

' STEALS IN A GAME, HALF, QUARTER
	IF A(I, I1, 13) > REC(43, 0) THEN
	  REC(43, 0) = A(I, I1, 13)  ' FTA
	  REC(43, 1) = 1                                             ' TIMES DONE
	  REC(43, 2) = 0                                             ' GAME
	  RC$(43, 0) = AA$(I, I1)                                 ' NAME
	  RC$(43, 1) = B$(I)                                      ' TEAM
	  RC$(43, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(43, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(43, 4) = DATE$                                         ' DATE
	END IF
	IF QQ(I, 1, I1, 7) > REC(44, 0) THEN
	  REC(44, 0) = QQ(I, 1, I1, 7)  ' FTA
	  REC(44, 1) = 1                                             ' TIMES DONE
	  REC(44, 2) = 1                                             ' HALF
	  RC$(44, 0) = AA$(I, I1)                                 ' NAME
	  RC$(44, 1) = B$(I)                                      ' TEAM
	  RC$(44, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(44, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(44, 4) = DATE$                                         ' DATE
	END IF
	IF QQ(I, 2, I1, 7) > REC(44, 0) THEN
	  REC(44, 0) = QQ(I, 2, I1, 7)    ' FTM
	  REC(44, 1) = 1                                             ' TIMES DONE
	  REC(44, 2) = 2                                             ' HALF
	  RC$(44, 0) = AA$(I, I1)                                 ' NAME
	  RC$(44, 1) = B$(I)                                      ' TEAM
	  RC$(44, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(44, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(44, 4) = DATE$                                         ' DATE
	END IF

' BLOCKS IN A GAME, HALF, QUARTER
	IF A(I, I1, 15) > REC(46, 0) THEN
	  REC(46, 0) = A(I, I1, 15)  ' FTA
	  REC(46, 1) = 1                                             ' TIMES DONE
	  REC(46, 2) = 0                                             ' GAME
	  RC$(46, 0) = AA$(I, I1)                                 ' NAME
	  RC$(46, 1) = B$(I)                                      ' TEAM
	  RC$(46, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(46, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(46, 4) = DATE$                                         ' DATE
	END IF
	IF QQ(I, 1, I1, 9) > REC(47, 0) THEN
	  REC(47, 0) = QQ(I, 1, I1, 9)  ' FTA
	  REC(47, 1) = 1                                             ' TIMES DONE
	  REC(47, 2) = 1                                             ' HALF
	  RC$(47, 0) = AA$(I, I1)                                 ' NAME
	  RC$(47, 1) = B$(I)                                      ' TEAM
	  RC$(47, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(47, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(47, 4) = DATE$                                         ' DATE
	END IF
	IF QQ(I, 2, I1, 9) > REC(47, 0) THEN
	  REC(47, 0) = QQ(I, 2, I1, 9)  ' FTM
	  REC(47, 1) = 1                                             ' TIMES DONE
	  REC(47, 2) = 2                                             ' HALF
	  RC$(47, 0) = AA$(I, I1)                                 ' NAME
	  RC$(47, 1) = B$(I)                                      ' TEAM
	  RC$(47, 2) = VERSUS$(I)                                      ' AT/VS
	  RC$(47, 3) = B$(1 - I)                                  ' OPPONENT
	  RC$(47, 4) = DATE$                                         ' DATE
	END IF
	 NEXT
	NEXT

' MOST OVERTIME PERIODS
IF H - 3 > TREC(0, 0) THEN
	TREC(0, 0) = H - 3
	TREC(0, 1) = S(0, 0)
	TREC(0, 2) = S(1, 0)
	TRC$(0, 0) = B$(0)
	TRC$(0, 1) = B$(1)
	TRC$(0, 2) = DATE$
END IF

FOR I = 0 TO 1

IF S(I, 0) > TREC(1, 0) THEN
	TREC(1, 0) = S(I, 0)
	TREC(1, 1) = 1
	TREC(1, 2) = 0
	TRC$(1, 0) = B$(I)
	TRC$(1, 1) = VERSUS$(I)
	TRC$(1, 2) = B$(1 - I)
	TRC$(1, 3) = DATE$
END IF

IF S(I, 0) < TREC(2, 0) THEN
	TREC(2, 0) = S(I, 0)
	TREC(2, 1) = 1
	TREC(2, 2) = 0
	TRC$(2, 0) = B$(I)
	TRC$(2, 1) = VERSUS$(I)
	TRC$(2, 2) = B$(1 - I)
	TRC$(2, 3) = DATE$
END IF

IF S(I, 0) + S(1 - I, 0) > TREC(3, 0) THEN
	TREC(3, 0) = S(I, 0) + S(1 - I, 0)
	TREC(3, 1) = S(I, 0)
	TREC(3, 2) = S(1 - I, 0)
	TRC$(3, 0) = B$(I)
	TRC$(3, 1) = VERSUS$(I)
	TRC$(3, 2) = B$(1 - I)
	TRC$(3, 3) = DATE$
END IF

IF S(I, 0) + S(1 - I, 0) < TREC(4, 0) THEN
	TREC(4, 0) = S(I, 0) + S(1 - I, 0)
	TREC(4, 1) = S(I, 0)
	TREC(4, 2) = S(1 - I, 0)
	TRC$(4, 0) = B$(I)
	TRC$(4, 1) = VERSUS$(I)
	TRC$(4, 2) = B$(1 - I)
	TRC$(4, 3) = DATE$
END IF

IF S(I, 1) > TREC(6, 0) THEN
	TREC(6, 0) = S(I, 1)
	TREC(6, 1) = 1
	TREC(6, 2) = 0
	TRC$(6, 0) = B$(I)
	TRC$(6, 1) = VERSUS$(I)
	TRC$(6, 2) = B$(1 - I)
	TRC$(6, 3) = DATE$
END IF

IF S(I, 1) < TREC(7, 0) THEN
	TREC(7, 0) = S(I, 1)
	TREC(7, 1) = 1
	TREC(7, 2) = 0
	TRC$(7, 0) = B$(I)
	TRC$(7, 1) = VERSUS$(I)
	TRC$(7, 2) = B$(1 - I)
	TRC$(7, 3) = DATE$
END IF

IF S(I, 1) + S(1 - I, 1) > TREC(8, 0) THEN
	TREC(8, 0) = S(I, 1) + S(1 - I, 1)
	TREC(8, 1) = S(I, 1)
	TREC(8, 2) = S(1 - I, 1)
	TRC$(8, 0) = B$(I)
	TRC$(8, 1) = VERSUS$(I)
	TRC$(8, 2) = B$(1 - I)
	TRC$(8, 3) = DATE$
END IF

IF S(I, 1) + S(1 - I, 1) < TREC(9, 0) THEN
	TREC(9, 0) = S(I, 1) + S(1 - I, 1)
	TREC(9, 1) = S(I, 1)
	TREC(9, 2) = S(1 - I, 1)
	TRC$(9, 0) = B$(I)
	TRC$(9, 1) = VERSUS$(I)
	TRC$(9, 2) = B$(1 - I)
	TRC$(9, 3) = DATE$
END IF

IF S(I, 2) > TREC(10, 0) THEN
	TREC(10, 0) = S(I, 2)
	TREC(10, 1) = 1
	TREC(10, 2) = 0
	TRC$(10, 0) = B$(I)
	TRC$(10, 1) = VERSUS$(I)
	TRC$(10, 2) = B$(1 - I)
	TRC$(10, 3) = DATE$
END IF

IF S(I, 2) < TREC(11, 0) THEN
	TREC(11, 0) = S(I, 2)
	TREC(11, 1) = 1
	TREC(11, 2) = 0
	TRC$(11, 0) = B$(I)
	TRC$(11, 1) = VERSUS$(I)
	TRC$(11, 2) = B$(1 - I)
	TRC$(11, 3) = DATE$
END IF

IF S(I, 2) + S(1 - I, 2) > TREC(12, 0) THEN
	TREC(12, 0) = S(I, 2) + S(1 - I, 2)
	TREC(12, 1) = S(I, 2)
	TREC(12, 2) = S(1 - I, 2)
	TRC$(12, 0) = B$(I)
	TRC$(12, 1) = VERSUS$(I)
	TRC$(12, 2) = B$(1 - I)
	TRC$(12, 3) = DATE$
END IF

IF S(I, 2) + S(1 - I, 2) < TREC(13, 0) THEN
	TREC(13, 0) = S(I, 2) + S(1 - I, 2)
	TREC(13, 1) = S(I, 2)
	TREC(13, 2) = S(1 - I, 2)
	TRC$(13, 0) = B$(I)
	TRC$(13, 1) = VERSUS$(I)
	TRC$(13, 2) = B$(1 - I)
	TRC$(13, 3) = DATE$
END IF

FOR I2 = 3 TO 8
IF S(I, I2) > TREC(14, 0) THEN
	TREC(14, 0) = S(I, I2)
	TREC(14, 1) = I2
	TREC(14, 2) = 0
	TRC$(14, 0) = B$(I)
	TRC$(14, 1) = VERSUS$(I)
	TRC$(14, 2) = B$(1 - I)
	TRC$(14, 3) = DATE$
END IF

IF S(I, I2) < TREC(15, 0) THEN
	TREC(15, 0) = S(I, I2)
	TREC(15, 1) = I2
	TREC(15, 2) = 0
	TRC$(15, 0) = B$(I)
	TRC$(15, 1) = VERSUS$(I)
	TRC$(15, 2) = B$(1 - I)
	TRC$(15, 3) = DATE$
END IF

IF S(I, I2) + S(1 - I, I2) > TREC(16, 0) THEN
	TREC(16, 0) = S(I, I2) + S(1 - I, I2)
	TREC(16, 1) = S(I, I2)
	TREC(16, 2) = S(1 - I, I2)
	TRC$(16, 0) = B$(I)
	TRC$(16, 1) = VERSUS$(I)
	TRC$(16, 2) = B$(1 - I)
	TRC$(16, 3) = DATE$
END IF

IF S(I, I2) + S(1 - I, I2) < TREC(17, 0) THEN
	TREC(17, 0) = S(I, I2) + S(1 - I, I2)
	TREC(17, 1) = S(I, I2)
	TREC(17, 2) = S(1 - I, I2)
	TRC$(17, 0) = B$(I)
	TRC$(17, 1) = VERSUS$(I)
	TRC$(17, 2) = B$(1 - I)
	TRC$(17, 3) = DATE$
END IF
NEXT I2
	' TEAM FIELD GOAL PERCENTAGES
FOR I1 = 0 TO 14: V8(I, I1) = 0: V8(1 - I, I1) = 0: NEXT
	FOR I2 = 0 TO 1
	FOR I1 = 0 TO 13
	V8(I2, 0) = V8(I2, 0) + A(I2, I1, 8)
	V8(I2, 1) = V8(I2, 1) + A(I2, I1, 7)
	V8(I2, 2) = V8(I2, 2) + A(I2, I1, 10)
	V8(I2, 3) = V8(I2, 3) + A(I2, I1, 9)
	V8(I2, 4) = V8(I2, 4) + A(I2, I1, 11)
	V8(I2, 5) = V8(I2, 5) + (A(I2, I1, 8) * 2) + A(I2, I1, 10) + W%(I2, I1, 0)
	V8(I2, 13) = V8(I2, 13) + W%(I2, I1, 0)
	V8(I2, 14) = V8(I2, 14) + W%(I2, I1, 1)
	V8(I2, 6) = V8(I2, 6) + A(I2, I1, 12)
	V8(I2, 7) = V8(I2, 7) + A(I2, I1, 14)
	V8(I2, 8) = V8(I2, 8) + A(I2, I1, 15)
	V8(I2, 9) = V8(I2, 9) + A(I2, I1, 13)
	V8(I2, 11) = V8(I2, 11) + A(I2, I1, 24)
	V8(I2, 12) = V8(I2, 12) + A(I2, I1, 23)
	V8(I2, 10) = V8(I2, 10) + ZZ(I2, I1)
	NEXT
	NEXT
	  IF V8(I, 1) = 0 THEN 101
IF V8(I, 0) / V8(I, 1) > TREC(30, 0) THEN
	TREC(30, 0) = V8(I, 0) / V8(I, 1)
	TREC(30, 1) = V8(I, 0)
	TREC(30, 2) = V8(I, 1)
	TRC$(30, 0) = B$(I)
	TRC$(30, 1) = VERSUS$(I)
	TRC$(30, 2) = B$(1 - I)
	TRC$(30, 3) = DATE$
END IF

IF V8(I, 0) / V8(I, 1) < TREC(31, 0) THEN
	TREC(31, 0) = V8(I, 0) / V8(I, 1)
	TREC(31, 1) = V8(I, 0)
	TREC(31, 2) = V8(I, 1)
	TRC$(31, 0) = B$(I)
	TRC$(31, 1) = VERSUS$(I)
	TRC$(31, 2) = B$(1 - I)
	TRC$(31, 3) = DATE$
END IF

101 IF V8(I, 1) + V8(1 - I, 1)  =0 THEN 102
	IF (V8(I, 0) + V8(1 - I, 0)) / (V8(I, 1) + V8(1 - I, 1)) > TREC(32, 0) THEN
	TREC(32, 0) = (V8(I, 0) + V8(1 - I, 0)) / (V8(I, 1) + V8(1 - I, 1))
	TREC(32, 1) = V8(I, 0) / V8(I, 1)
	TREC(32, 2) = V8(1 - I, 0) / (V8(1 - I, 1) + 0.01)
	TRC$(32, 0) = B$(I)
	TRC$(32, 1) = VERSUS$(I)
	TRC$(32, 2) = B$(1 - I)
	TRC$(32, 3) = DATE$
END IF

IF (V8(I, 0) + V8(1 - I, 0)) / (V8(I, 1) + V8(1 - I, 1)) < TREC(33, 0) THEN
	TREC(33, 0) = (V8(I, 0) + V8(1 - I, 0)) / (V8(I, 1) + V8(1 - I, 1))
	TREC(33, 1) = V8(I, 0) / V8(I, 1)
	TREC(33, 2) = V8(1 - I, 0) / (V8(1 - I, 1)+0.01)
	TRC$(33, 0) = B$(I)
	TRC$(33, 1) = VERSUS$(I)
	TRC$(33, 2) = B$(1 - I)
	TRC$(33, 3) = DATE$
END IF

102 ' TEAM FIELD GOALS MADE
IF V8(I, 0) > TREC(34, 0) THEN
	TREC(34, 0) = V8(I, 0)
	TREC(34, 1) = 0
	TREC(34, 2) = 0
	TRC$(34, 0) = B$(I)
	TRC$(34, 1) = VERSUS$(I)
	TRC$(34, 2) = B$(1 - I)
	TRC$(34, 3) = DATE$
END IF

IF V8(I, 0) < TREC(35, 0) THEN
	TREC(35, 0) = V8(I, 0)
	TREC(35, 1) = 0
	TREC(35, 2) = 0
	TRC$(35, 0) = B$(I)
	TRC$(35, 1) = VERSUS$(I)
	TRC$(35, 2) = B$(1 - I)
	TRC$(35, 3) = DATE$
END IF

IF V8(I, 0) + V8(1 - I, 0) > TREC(36, 0) THEN
	TREC(36, 0) = V8(I, 0) + V8(1 - I, 0)
	TREC(36, 1) = V8(I, 0)
	TREC(36, 2) = V8(1 - I, 0)
	TRC$(36, 0) = B$(I)
	TRC$(36, 1) = VERSUS$(I)
	TRC$(36, 2) = B$(1 - I)
	TRC$(36, 3) = DATE$
END IF

IF V8(I, 0) + V8(1 - I, 0) < TREC(37, 0) THEN
	TREC(37, 0) = V8(I, 0) + V8(1 - I, 0)
	TREC(37, 1) = V8(I, 0)
	TREC(37, 2) = V8(1 - I, 0)
	TRC$(37, 0) = B$(I)
	TRC$(37, 1) = VERSUS$(I)
	TRC$(37, 2) = B$(1 - I)
	TRC$(37, 3) = DATE$
END IF

IF QR(I, 1, 0) > TREC(38, 0) THEN
	TREC(38, 0) = QR(I, 1, 0)
	TREC(38, 1) = 1
	TREC(38, 2) = 0
	TRC$(38, 0) = B$(I)
	TRC$(38, 1) = VERSUS$(I)
	TRC$(38, 2) = B$(1 - I)
	TRC$(38, 3) = DATE$
END IF

IF QR(I, 2, 0) > TREC(38, 0) THEN
	TREC(38, 0) = QR(I, 2, 0)
	TREC(38, 1) = 2
	TREC(38, 2) = 0
	TRC$(38, 0) = B$(I)
	TRC$(38, 1) = VERSUS$(I)
	TRC$(38, 2) = B$(1 - I)
	TRC$(38, 3) = DATE$
END IF

IF QR(I, 1, 0) + QR(1 - I, 1, 0) > TREC(39, 0) THEN
	TREC(39, 0) = QR(I, 1, 0) + QR(1 - I, 1, 0)
	TREC(39, 1) = 1
	TREC(39, 2) = 0
	TRC$(39, 0) = B$(I)
	TRC$(39, 1) = VERSUS$(I)
	TRC$(39, 2) = B$(1 - I)
	TRC$(39, 3) = DATE$
END IF

IF QR(I, 2, 0) + QR(1 - I, 2, 0) > TREC(39, 0) THEN
	TREC(39, 0) = QR(I, 2, 0) + QR(1 - I, 2, 0)
	TREC(39, 1) = 2
	TREC(39, 2) = 0
	TRC$(39, 0) = B$(I)
	TRC$(39, 1) = VERSUS$(I)
	TRC$(39, 2) = B$(1 - I)
	TRC$(39, 3) = DATE$
END IF

'TEAM FIELD GOAL ATTEMPTS
IF V8(I, 1) > TREC(42, 0) THEN
	TREC(42, 0) = V8(I, 1)
	TREC(42, 1) = 0
	TREC(42, 2) = 0
	TRC$(42, 0) = B$(I)
	TRC$(42, 1) = VERSUS$(I)
	TRC$(42, 2) = B$(1 - I)
	TRC$(42, 3) = DATE$
END IF

IF V8(I, 1) < TREC(43, 0) THEN
	TREC(43, 0) = V8(I, 1)
	TREC(43, 1) = 0
	TREC(43, 2) = 0
	TRC$(43, 0) = B$(I)
	TRC$(43, 1) = VERSUS$(I)
	TRC$(43, 2) = B$(1 - I)
	TRC$(43, 3) = DATE$
END IF

IF V8(I, 1) + V8(1 - I, 1) > TREC(44, 0) THEN
	TREC(44, 0) = V8(I, 1) + V8(1 - I, 1)
	TREC(44, 1) = V8(I, 1)
	TREC(44, 2) = V8(1 - I, 1)
	TRC$(44, 0) = B$(I)
	TRC$(44, 1) = VERSUS$(I)
	TRC$(44, 2) = B$(1 - I)
	TRC$(44, 3) = DATE$
END IF

IF V8(I, 1) + V8(1 - I, 1) < TREC(45, 0) THEN
	TREC(45, 0) = V8(I, 1) + V8(1 - I, 1)
	TREC(45, 1) = V8(I, 1)
	TREC(45, 2) = V8(1 - I, 1)
	TRC$(45, 0) = B$(I)
	TRC$(45, 1) = VERSUS$(I)
	TRC$(45, 2) = B$(1 - I)
	TRC$(45, 3) = DATE$
END IF

IF QR(I, 1, 1) > TREC(46, 0) THEN
	TREC(46, 0) = QR(I, 1, 1)
	TREC(46, 1) = 1
	TREC(46, 2) = 0
	TRC$(46, 0) = B$(I)
	TRC$(46, 1) = VERSUS$(I)
	TRC$(46, 2) = B$(1 - I)
	TRC$(46, 3) = DATE$
END IF

IF QR(I, 2, 1) > TREC(46, 0) THEN
	TREC(46, 0) = QR(I, 2, 1)
	TREC(46, 1) = 2
	TREC(46, 2) = 0
	TRC$(46, 0) = B$(I)
	TRC$(46, 1) = VERSUS$(I)
	TRC$(46, 2) = B$(1 - I)
	TRC$(46, 3) = DATE$
END IF

IF QR(I, 1, 1) + QR(1 - I, 1, 1) > TREC(47, 0) THEN
	TREC(47, 0) = QR(I, 1, 1) + QR(1 - I, 1, 1)
	TREC(47, 1) = 1
	TREC(47, 2) = QR(I, 1, 1)
	TRC$(47, 0) = B$(I)
	TRC$(47, 1) = VERSUS$(I)
	TRC$(47, 2) = B$(1 - I)
	TRC$(47, 3) = DATE$
END IF

IF QR(I, 2, 1) + QR(1 - I, 2, 1) > TREC(47, 0) THEN
	TREC(47, 0) = QR(I, 2, 1) + QR(1 - I, 2, 1)
	TREC(47, 1) = 2
	TREC(47, 2) = QR(I, 2, 1)
	TRC$(47, 0) = B$(I)
	TRC$(47, 1) = VERSUS$(I)
	TRC$(47, 2) = B$(1 - I)
	TRC$(47, 3) = DATE$
END IF

'TEAM FIELD GOAL ATTEMPTS
IF V8(I, 13) = V8(I, 14) AND V8(I, 14) > TREC(50, 0) THEN
	TREC(50, 0) = V8(I, 13)
	TREC(50, 1) = 0
	TREC(50, 2) = 0
	TRC$(50, 0) = B$(I)
	TRC$(50, 1) = VERSUS$(I)
	TRC$(50, 2) = B$(1 - I)
	TRC$(50, 3) = DATE$
END IF

'TEAM FIELD GOAL ATTEMPTS MOST MADE
IF V8(I, 13) = V8(I, 14) AND V8(1 - I, 13) = V8(1 - I, 14) AND V8(I, 14) + V8(1 - I, 14) > TREC(51, 0) THEN
	TREC(51, 0) = V8(I, 13)
	TREC(51, 1) = V8(1 - I, 13)
	TREC(51, 2) = 0
	TRC$(51, 0) = B$(I)
	TRC$(51, 1) = VERSUS$(I)
	TRC$(51, 2) = B$(1 - I)
	TRC$(51, 3) = DATE$
END IF

'TEAM FIELD GOAL ATTEMPTS NONE MADE
IF V8(I, 13) = 0 AND V8(I, 14) > TREC(52, 0) THEN
	TREC(52, 0) = V8(I, 14)
	TREC(52, 1) = 0
	TREC(52, 2) = 0
	TRC$(52, 0) = B$(I)
	TRC$(52, 1) = VERSUS$(I)
	TRC$(52, 2) = B$(1 - I)
	TRC$(52, 3) = DATE$
END IF

'TEAM FIELD GOAL ATTEMPTS NONE MADE
IF V8(I, 13) > TREC(53, 0) THEN
	TREC(53, 0) = V8(I, 13)
	TREC(53, 1) = 0
	TREC(53, 2) = 0
	TRC$(53, 0) = B$(I)
	TRC$(53, 1) = VERSUS$(I)
	TRC$(53, 2) = B$(1 - I)
	TRC$(53, 3) = DATE$
END IF

'TEAM FIELD GOAL ATTEMPTS NONE MADE
IF V8(I, 13) + V8(1 - I, 13) > TREC(54, 0) THEN
	TREC(54, 0) = V8(I, 13) + V8(1 - I, 13)
	TREC(54, 1) = V8(I, 13)
	TREC(54, 2) = V8(1 - I, 13)
	TRC$(54, 0) = B$(I)
	TRC$(54, 1) = VERSUS$(I)
	TRC$(54, 2) = B$(1 - I)
	TRC$(54, 3) = DATE$
END IF

IF QR(I, 1, 13) > TREC(55, 0) THEN
	TREC(55, 0) = QR(I, 1, 13)
	TREC(55, 1) = 1
	TREC(55, 2) = 0
	TRC$(55, 0) = B$(I)
	TRC$(55, 1) = VERSUS$(I)
	TRC$(55, 2) = B$(1 - I)
	TRC$(55, 3) = DATE$
END IF

IF QR(I, 2, 13) > TREC(55, 0) THEN
	TREC(55, 0) = QR(I, 2, 13)
	TREC(55, 1) = 2
	TREC(55, 2) = 0
	TRC$(55, 0) = B$(I)
	TRC$(55, 1) = VERSUS$(I)
	TRC$(55, 2) = B$(1 - I)
	TRC$(55, 3) = DATE$
END IF

'TEAM FIELD GOAL ATTEMPTS NONE MADE
IF V8(I, 14) > TREC(57, 0) THEN
	TREC(57, 0) = V8(I, 14)
	TREC(57, 1) = 0
	TREC(57, 2) = 0
	TRC$(57, 0) = B$(I)
	TRC$(57, 1) = VERSUS$(I)
	TRC$(57, 2) = B$(1 - I)
	TRC$(57, 3) = DATE$
END IF

'TEAM FIELD GOAL ATTEMPTS NONE MADE
IF V8(I, 14) + V8(1 - I, 14) > TREC(58, 0) THEN
	TREC(58, 0) = V8(I, 14) + V8(1 - I, 14)
	TREC(58, 1) = V8(I, 14)
	TREC(58, 2) = V8(1 - I, 14)
	TRC$(58, 0) = B$(I)
	TRC$(58, 1) = VERSUS$(I)
	TRC$(58, 2) = B$(1 - I)
	TRC$(58, 3) = DATE$
END IF

IF QR(I, 1, 14) > TREC(59, 0) THEN
	TREC(59, 0) = QR(I, 1, 14)
	TREC(59, 1) = 1
	TREC(59, 2) = 0
	TRC$(59, 0) = B$(I)
	TRC$(59, 1) = VERSUS$(I)
	TRC$(59, 2) = B$(1 - I)
	TRC$(59, 3) = DATE$
END IF

IF QR(I, 2, 14) > TREC(59, 0) THEN
	TREC(59, 0) = QR(I, 2, 14)
	TREC(59, 1) = 2
	TREC(59, 2) = 0
	TRC$(59, 0) = B$(I)
	TRC$(59, 1) = VERSUS$(I)
	TRC$(59, 2) = B$(1 - I)
	TRC$(59, 3) = DATE$
END IF

' FREE THROW ATTEMPTS NO MISS
IF V8(I, 2) = V8(I, 3) AND V8(I, 2) > TREC(60, 0) THEN
	TREC(60, 0) = V8(I, 2)
	TREC(60, 1) = 0
	TREC(60, 2) = 0
	TRC$(60, 0) = B$(I)
	TRC$(60, 1) = VERSUS$(I)
	TRC$(60, 2) = B$(1 - I)
	TRC$(60, 3) = DATE$
END IF

' FREE THROW ATTEMPTS LOW %
	IF V8(I, 3) = 0 THEN 103
IF V8(I, 2) / V8(I, 3) < TREC(61, 0) THEN
	TREC(61, 0) = V8(I, 2) / V8(I, 3)
	TREC(61, 1) = V8(I, 2)
	TREC(61, 2) = V8(I, 3)
	TRC$(61, 0) = B$(I)
	TRC$(61, 1) = VERSUS$(I)
	TRC$(61, 2) = B$(1 - I)
	TRC$(61, 3) = DATE$
END IF

103 ' FREE THROW ATTEMPTS COMBINED HIGH %
IF V8(I, 3) + V8(1 - I, 3)=0 THEN 104
IF (V8(I, 2) + V8(1 - I, 2)) / (V8(I, 3) + V8(1 - I, 3)) > TREC(62, 0) THEN
	TREC(62, 0) = (V8(I, 2) + V8(1 - I, 2)) / (V8(I, 3) + V8(1 - I, 3))
	TREC(62, 1) = V8(I, 2) / V8(I, 3)
	TREC(62, 2) = V8(1 - I, 2) / (V8(1 - I, 3)+0.01)
	TRC$(62, 0) = B$(I)
	TRC$(62, 1) = VERSUS$(I)
	TRC$(62, 2) = B$(1 - I)
	TRC$(62, 3) = DATE$
END IF

' FREE THROW ATTEMPTS COMBINED LOW %
IF (V8(I, 2) + V8(1 - I, 2)) / (V8(I, 3) + V8(1 - I, 3)) < TREC(63, 0) THEN
	TREC(63, 0) = (V8(I, 2) + V8(1 - I, 2)) / (V8(I, 3) + V8(1 - I, 3))
	TREC(63, 1) = V8(I, 2) / V8(I, 3)
	TREC(63, 2) = V8(1 - I, 2) / (V8(1 - I, 3)+0.01)
	TRC$(63, 0) = B$(I)
	TRC$(63, 1) = VERSUS$(I)
	TRC$(63, 2) = B$(1 - I)
	TRC$(63, 3) = DATE$
END IF

' TEAM FREE THROWS MADE

104 IF V8(I, 2) > TREC(64, 0) THEN
	TREC(64, 0) = V8(I, 2)
	TREC(64, 1) = 0
	TREC(64, 2) = 0
	TRC$(64, 0) = B$(I)
	TRC$(64, 1) = VERSUS$(I)
	TRC$(64, 2) = B$(1 - I)
	TRC$(64, 3) = DATE$
END IF

IF V8(I, 2) < TREC(65, 0) THEN
	TREC(65, 0) = V8(I, 2)
	TREC(65, 1) = 0
	TREC(65, 2) = 0
	TRC$(65, 0) = B$(I)
	TRC$(65, 1) = VERSUS$(I)
	TRC$(65, 2) = B$(1 - I)
	TRC$(65, 3) = DATE$
END IF

IF V8(I, 2) + V8(1 - I, 2) > TREC(66, 0) THEN
	TREC(66, 0) = V8(I, 2) + V8(1 - I, 2)
	TREC(66, 1) = V8(I, 2)
	TREC(66, 2) = V8(1 - I, 2)
	TRC$(66, 0) = B$(I)
	TRC$(66, 1) = VERSUS$(I)
	TRC$(66, 2) = B$(1 - I)
	TRC$(66, 3) = DATE$
END IF

IF V8(I, 2) + V8(1 - I, 2) < TREC(67, 0) THEN
	TREC(67, 0) = V8(I, 2) + V8(1 - I, 2)
	TREC(67, 1) = V8(I, 2)
	TREC(67, 2) = V8(1 - I, 2)
	TRC$(67, 0) = B$(I)
	TRC$(67, 1) = VERSUS$(I)
	TRC$(67, 2) = B$(1 - I)
	TRC$(67, 3) = DATE$
END IF

IF QR(I, 1, 2) > TREC(68, 0) THEN
	TREC(68, 0) = QR(I, 1, 2)
	TREC(68, 1) = 1
	TREC(68, 2) = 0
	TRC$(68, 0) = B$(I)
	TRC$(68, 1) = VERSUS$(I)
	TRC$(68, 2) = B$(1 - I)
	TRC$(68, 3) = DATE$
END IF

IF QR(I, 2, 2) > TREC(68, 0) THEN
	TREC(68, 0) = QR(I, 2, 2)
	TREC(68, 1) = 2
	TREC(68, 2) = 0
	TRC$(68, 0) = B$(I)
	TRC$(68, 1) = VERSUS$(I)
	TRC$(68, 2) = B$(1 - I)
	TRC$(68, 3) = DATE$
END IF

' TEAM FREE THROWS ATTEMPTED
IF V8(I, 3) > TREC(72, 0) THEN
	TREC(72, 0) = V8(I, 3)
	TREC(72, 1) = 0
	TREC(72, 2) = 0
	TRC$(72, 0) = B$(I)
	TRC$(72, 1) = VERSUS$(I)
	TRC$(72, 2) = B$(1 - I)
	TRC$(72, 3) = DATE$
END IF

IF V8(I, 3) < TREC(73, 0) THEN
	TREC(73, 0) = V8(I, 3)
	TREC(73, 1) = 0
	TREC(73, 2) = 0
	TRC$(73, 0) = B$(I)
	TRC$(73, 1) = VERSUS$(I)
	TRC$(73, 2) = B$(1 - I)
	TRC$(73, 3) = DATE$
END IF

IF V8(I, 3) + V8(1 - I, 3) > TREC(74, 0) THEN
	TREC(74, 0) = V8(I, 3) + V8(1 - I, 3)
	TREC(74, 1) = V8(I, 3)
	TREC(74, 2) = V8(1 - I, 3)
	TRC$(74, 0) = B$(I)
	TRC$(74, 1) = VERSUS$(I)
	TRC$(74, 2) = B$(1 - I)
	TRC$(74, 3) = DATE$
END IF

IF V8(I, 3) + V8(1 - I, 3) < TREC(75, 0) THEN
	TREC(75, 0) = V8(I, 3) + V8(1 - I, 3)
	TREC(75, 1) = V8(I, 3)
	TREC(75, 2) = V8(1 - I, 3)
	TRC$(75, 0) = B$(I)
	TRC$(75, 1) = VERSUS$(I)
	TRC$(75, 2) = B$(1 - I)
	TRC$(75, 3) = DATE$
END IF

IF QR(I, 1, 3)  > TREC(76, 0) THEN
	TREC(76, 0) = QR(I, 1, 3)
	TREC(76, 1) = 1
	TREC(76, 2) = 0
	TRC$(76, 0) = B$(I)
	TRC$(76, 1) = VERSUS$(I)
	TRC$(76, 2) = B$(1 - I)
	TRC$(76, 3) = DATE$
END IF

IF QR(I, 2, 3) > TREC(76, 0) THEN
	TREC(76, 0) = QR(I, 2, 3)
	TREC(76, 1) = 2
	TREC(76, 2) = 0
	TRC$(76, 0) = B$(I)
	TRC$(76, 1) = VERSUS$(I)
	TRC$(76, 2) = B$(1 - I)
	TRC$(76, 3) = DATE$
END IF

IF QR(I, 1, 3) + QR(1 - I, 1, 3) > TREC(77, 0) THEN
	TREC(77, 0) = QR(I, 1, 3) + QR(1 - I, 1, 3)
	TREC(77, 1) = 1
	TREC(77, 2) = QR(I, 1, 3)
	TRC$(77, 0) = B$(I)
	TRC$(77, 1) = VERSUS$(I)
	TRC$(77, 2) = B$(1 - I)
	TRC$(77, 3) = DATE$
END IF

IF QR(I, 2, 3) + QR(1 - I, 2, 3) > TREC(77, 0) THEN
	TREC(77, 0) = QR(I, 2, 3) + QR(1 - I, 2, 3)
	TREC(77, 1) = 2
	TREC(77, 2) = QR(I, 2, 3)
	TRC$(77, 0) = B$(I)
	TRC$(77, 1) = VERSUS$(I)
	TRC$(77, 2) = B$(1 - I)
	TRC$(77, 3) = DATE$
END IF

IF R3(I) > TREC(80, 0) THEN
	TREC(80, 0) = R3(I)
	TREC(80, 1) = 0
	TREC(80, 2) = 0
	TRC$(80, 0) = B$(I)
	TRC$(80, 1) = VERSUS$(I)
	TRC$(80, 2) = B$(1 - I)
	TRC$(80, 3) = DATE$
END IF

IF R3(I) < TREC(81, 0) THEN
	TREC(81, 0) = R3(I)
	TREC(81, 1) = 0
	TREC(81, 2) = 0
	TRC$(81, 0) = B$(I)
	TRC$(81, 1) = VERSUS$(I)
	TRC$(81, 2) = B$(1 - I)
	TRC$(81, 3) = DATE$
END IF

IF R3(I) + R3(1 - I) > TREC(82, 0) THEN
	TREC(82, 0) = R3(I) + R3(1 - I)
	TREC(82, 1) = R3(I)
	TREC(82, 2) = R3(1 - I)
	TRC$(82, 0) = B$(I)
	TRC$(82, 1) = VERSUS$(I)
	TRC$(82, 2) = B$(1 - I)
	TRC$(82, 3) = DATE$
END IF

IF R3(I) + R3(1 - I) < TREC(83, 0) THEN
	TREC(83, 0) = R3(I) + R3(1 - I)
	TREC(83, 1) = R3(I)
	TREC(83, 2) = R3(1 - I)
	TRC$(83, 0) = B$(I)
	TRC$(83, 1) = VERSUS$(I)
	TRC$(83, 2) = B$(1 - I)
	TRC$(83, 3) = DATE$
END IF

IF QR(I, 1, 4) > TREC(84, 0) THEN
	TREC(84, 0) = QR(I, 1, 4)
	TREC(84, 1) = 1
	TREC(84, 2) = 0
	TRC$(84, 0) = B$(I)
	TRC$(84, 1) = VERSUS$(I)
	TRC$(84, 2) = B$(1 - I)
	TRC$(84, 3) = DATE$
END IF

IF QR(I, 2, 4) > TREC(84, 0) THEN
	TREC(84, 0) = QR(I, 2, 4)
	TREC(84, 1) = 2
	TREC(84, 2) = 0
	TRC$(84, 0) = B$(I)
	TRC$(84, 1) = VERSUS$(I)
	TRC$(84, 2) = B$(1 - I)
	TRC$(84, 3) = DATE$
END IF

IF V8(I, 11) > TREC(86, 0) THEN
	TREC(86, 0) = V8(I, 11)
	TREC(86, 1) = 0
	TREC(86, 2) = 0
	TRC$(86, 0) = B$(I)
	TRC$(86, 1) = VERSUS$(I)
	TRC$(86, 2) = B$(1 - I)
	TRC$(86, 3) = DATE$
END IF

IF V8(I, 11) < TREC(87, 0) THEN
	TREC(87, 0) = V8(I, 11)
	TREC(87, 1) = 0
	TREC(87, 2) = 0
	TRC$(87, 0) = B$(I)
	TRC$(87, 1) = VERSUS$(I)
	TRC$(87, 2) = B$(1 - I)
	TRC$(87, 3) = DATE$
END IF
	IF V8(I, 11) + V8(1 - I, 11) > TREC(88, 0) THEN
	TREC(88, 0) = V8(I, 11) + V8(1 - I, 11)
	TREC(88, 1) = V8(I, 11)
	TREC(88, 2) = V8(1 - I, 11)
	TRC$(88, 0) = B$(I)
	TRC$(88, 1) = VERSUS$(I)
	TRC$(88, 2) = B$(1 - I)
	TRC$(88, 3) = DATE$
END IF

IF V8(I, 11) + V8(1 - I, 11) < TREC(89, 0) THEN
	TREC(89, 0) = V8(I, 11) + V8(1 - I, 11)
	TREC(89, 1) = V8(I, 11)
	TREC(89, 2) = V8(1 - I, 11)
	TRC$(89, 0) = B$(I)
	TRC$(89, 1) = VERSUS$(I)
	TRC$(89, 2) = B$(1 - I)
	TRC$(89, 3) = DATE$
END IF

IF QR(I, 1, 11) > TREC(90, 0) THEN
	TREC(90, 0) = QR(I, 1, 11)
	TREC(90, 1) = 1
	TREC(90, 2) = 0
	TRC$(90, 0) = B$(I)
	TRC$(90, 1) = VERSUS$(I)
	TRC$(90, 2) = B$(1 - I)
	TRC$(90, 3) = DATE$
END IF

IF QR(I, 2, 11) > TREC(90, 0) THEN
	TREC(90, 0) = QR(I, 2, 11)
	TREC(90, 1) = 2
	TREC(90, 2) = 0
	TRC$(90, 0) = B$(I)
	TRC$(90, 1) = VERSUS$(I)
	TRC$(90, 2) = B$(1 - I)
	TRC$(90, 3) = DATE$
END IF

' ASSISTS
IF V8(I, 7) > TREC(96, 0) THEN
	TREC(96, 0) = V8(I, 7)
	TREC(96, 1) = 0
	TREC(96, 2) = 0
	TRC$(96, 0) = B$(I)
	TRC$(96, 1) = VERSUS$(I)
	TRC$(96, 2) = B$(1 - I)
	TRC$(96, 3) = DATE$
END IF

IF V8(I, 7) < TREC(97, 0) THEN
	TREC(97, 0) = V8(I, 7)
	TREC(97, 1) = 0
	TREC(97, 2) = 0
	TRC$(97, 0) = B$(I)
	TRC$(97, 1) = VERSUS$(I)
	TRC$(97, 2) = B$(1 - I)
	TRC$(97, 3) = DATE$

1 END IF

IF V8(I, 7) + V8(1 - I, 7) > TREC(98, 0) THEN
	TREC(98, 0) = V8(I, 7) + V8(1 - I, 7)
	TREC(98, 1) = V8(I, 7)
	TREC(98, 2) = V8(1 - I, 7)
	TRC$(98, 0) = B$(I)
	TRC$(98, 1) = VERSUS$(I)
	TRC$(98, 2) = B$(1 - I)
	TRC$(98, 3) = DATE$

2 END IF

IF V8(I, 7) + V8(1 - I, 7) < TREC(99, 0) THEN
	TREC(99, 0) = V8(I, 7) + V8(1 - I, 7)
	TREC(99, 1) = V8(I, 7)
	TREC(99, 2) = V8(1 - I, 7)
	TRC$(99, 0) = B$(I)
	TRC$(99, 1) = VERSUS$(I)
	TRC$(99, 2) = B$(1 - I)
	TRC$(99, 3) = DATE$

3 END IF

IF QR(I, 1, 7) > TREC(100, 0) THEN
	TREC(100, 0) = QR(I, 1, 7)
	TREC(100, 1) = 1
	TREC(100, 2) = 0
	TRC$(100, 0) = B$(I)
	TRC$(100, 1) = VERSUS$(I)
	TRC$(100, 2) = B$(1 - I)
	TRC$(100, 3) = DATE$

4 END IF

IF QR(I, 2, 7) > TREC(100, 0) THEN
	TREC(100, 0) = QR(I, 2, 7)
	TREC(100, 1) = 2
	TREC(100, 2) = 0
	TRC$(100, 0) = B$(I)
	TRC$(100, 1) = VERSUS$(I)
	TRC$(100, 2) = B$(1 - I)
	TRC$(100, 3) = DATE$
END IF

IF QR(I, 2, 7)  + QR(1 - I, 2, 7)> TREC(101, 0) THEN
	TREC(101, 0) = QR(I, 2, 7) + QR(1 - I, 2, 7)
	TREC(101, 1) = 2
	TREC(101, 2) = 0
	TRC$(101, 0) = B$(I)
	TRC$(101, 1) = VERSUS$(I)
	TRC$(101, 2) = B$(1 - I)
	TRC$(101, 3) = DATE$

5 END IF

' FOULS
IF V8(I, 6) > TREC(104, 0) THEN
	TREC(104, 0) = V8(I, 6)
	TREC(104, 1) = 0
	TREC(104, 2) = 0
	TRC$(104, 0) = B$(I)
	TRC$(104, 1) = VERSUS$(I)
	TRC$(104, 2) = B$(1 - I)
	TRC$(104, 3) = DATE$

10 END IF

IF V8(I, 6) < TREC(105, 0) THEN
	TREC(105, 0) = V8(I, 6)
	TREC(105, 1) = 0
	TREC(105, 2) = 0
	TRC$(105, 0) = B$(I)
	TRC$(105, 1) = VERSUS$(I)
	TRC$(105, 2) = B$(1 - I)
	TRC$(105, 3) = DATE$

11 END IF

IF V8(I, 6) + V8(1 - I, 6) > TREC(106, 0) THEN
	TREC(106, 0) = V8(I, 6) + V8(1 - I, 6)
	TREC(106, 1) = V8(I, 6)
	TREC(106, 2) = V8(1 - I, 6)
	TRC$(106, 0) = B$(I)
	TRC$(106, 1) = VERSUS$(I)
	TRC$(106, 2) = B$(1 - I)
	TRC$(106, 3) = DATE$

12 END IF

IF V8(I, 6) + V8(1 - I, 6) < TREC(107, 0) THEN
	TREC(107, 0) = V8(I, 6) + V8(1 - I, 6)
	TREC(107, 1) = V8(I, 6)
	TREC(107, 2) = V8(1 - I, 6)
	TRC$(107, 0) = B$(I)
	TRC$(107, 1) = VERSUS$(I)
	TRC$(107, 2) = B$(1 - I)
	TRC$(107, 3) = DATE$

13 END IF

IF QR(I, 1, 6) > TREC(108, 0) THEN
	TREC(108, 0) = QR(I, 1, 6)
	TREC(108, 1) = 1
	TREC(108, 2) = 0
	TRC$(108, 0) = B$(I)
	TRC$(108, 1) = VERSUS$(I)
	TRC$(108, 2) = B$(1 - I)
	TRC$(108, 3) = DATE$

14 END IF

IF QR(I, 2, 6) > TREC(108, 0) THEN
	TREC(108, 0) = QR(I, 2, 6)
	TREC(108, 1) = 2
	TREC(108, 2) = 0
	TRC$(108, 0) = B$(I)
	TRC$(108, 1) = VERSUS$(I)
	TRC$(108, 2) = B$(1 - I)
	TRC$(108, 3) = DATE$

15 END IF

IF QR(I, 1, 6) + QR(1 - I, 1, 6) > TREC(109, 0) THEN
	TREC(109, 0) = QR(I, 1, 6) + QR(1 - I, 1, 6)
	TREC(109, 1) = 1
	TREC(109, 2) = QR(I, 1, 6)
	TRC$(109, 0) = B$(I)
	TRC$(109, 1) = VERSUS$(I)
	TRC$(109, 2) = B$(1 - I)
	TRC$(109, 3) = DATE$

16 END IF

IF QR(I, 2, 6) + QR(1 - I, 2, 6) > TREC(109, 0) THEN
	TREC(109, 0) = QR(I, 2, 6) + QR(1 - I, 2, 6)
	TREC(109, 1) = 2
	TREC(109, 2) = QR(I, 2, 6)
	TRC$(109, 0) = B$(I)
	TRC$(109, 1) = VERSUS$(I)
	TRC$(109, 2) = B$(1 - I)
	TRC$(109, 3) = DATE$

17 END IF

' STEALS
IF V8(I, 9) > TREC(112, 0) THEN
	TREC(112, 0) = V8(I, 9)
	TREC(112, 1) = 0
	TREC(112, 2) = 0
	TRC$(112, 0) = B$(I)
	TRC$(112, 1) = VERSUS$(I)
	TRC$(112, 2) = B$(1 - I)
	TRC$(112, 3) = DATE$

20 END IF

IF V8(I, 9) + V8(1 - I, 9) > TREC(114, 0) THEN
	TREC(114, 0) = V8(I, 9) + V8(1 - I, 9)
	TREC(114, 1) = V8(I, 9)
	TREC(114, 2) = V8(1 - I, 9)
	TRC$(114, 0) = B$(I)
	TRC$(114, 1) = VERSUS$(I)
	TRC$(114, 2) = B$(1 - I)
	TRC$(114, 3) = DATE$

21 END IF

IF QR(I, 1, 9) > TREC(116, 0) THEN
	TREC(116, 0) = QR(I, 1, 9)
	TREC(116, 1) = 1
	TREC(116, 2) = 0
	TRC$(116, 0) = B$(I)
	TRC$(116, 1) = VERSUS$(I)
	TRC$(116, 2) = B$(1 - I)
	TRC$(116, 3) = DATE$
	END IF

IF QR(I, 2, 9) > TREC(116, 0) THEN
	TREC(116, 0) = QR(I, 2, 9)
	TREC(116, 1) = 2
	TREC(116, 2) = 0
	TRC$(116, 0) = B$(I)
	TRC$(116, 1) = VERSUS$(I)
	TRC$(116, 2) = B$(1 - I)
	TRC$(116, 3) = DATE$

23 END IF

' BLOCKS
IF V8(I, 8) > TREC(118, 0) THEN
	TREC(118, 0) = V8(I, 8)
	TREC(118, 1) = 0
	TREC(118, 2) = 0
	TRC$(118, 0) = B$(I)
	TRC$(118, 1) = VERSUS$(I)
	TRC$(118, 2) = B$(1 - I)
	TRC$(118, 3) = DATE$

25 END IF

IF V8(I, 8) + V8(1 - I, 8) > TREC(119, 0) THEN
	TREC(119, 0) = V8(I, 8) + V8(1 - I, 8)
	TREC(119, 1) = V8(I, 8)
	TREC(119, 2) = V8(1 - I, 8)
	TRC$(119, 0) = B$(I)
	TRC$(119, 1) = VERSUS$(I)
	TRC$(119, 2) = B$(1 - I)
	TRC$(119, 3) = DATE$

26 END IF

IF QR(I, 1, 8) > TREC(121, 0) THEN
	TREC(121, 0) = QR(I, 1, 8)
	TREC(121, 1) = 1
	TREC(121, 2) = 0
	TRC$(121, 0) = B$(I)
	TRC$(121, 1) = VERSUS$(I)
	TRC$(121, 2) = B$(1 - I)
	TRC$(121, 3) = DATE$

22 END IF

IF QR(I, 2, 8) > TREC(121, 0) THEN
	TREC(121, 0) = QR(I, 2, 8)
	TREC(121, 1) = 1
	TREC(121, 2) = 0
	TRC$(121, 0) = B$(I)
	TRC$(121, 1) = VERSUS$(I)
	TRC$(121, 2) = B$(1 - I)
	TRC$(121, 3) = DATE$
	END IF

' TURNOVERS
IF V8(I, 12) > TREC(122, 0) THEN
	TREC(122, 0) = V8(I, 12)
	TREC(122, 1) = 0
	TREC(122, 2) = 0
	TRC$(122, 0) = B$(I)
	TRC$(122, 1) = VERSUS$(I)
	TRC$(122, 2) = B$(1 - I)
	TRC$(122, 3) = DATE$

29 END IF

IF V8(I, 12) < TREC(123, 0) THEN
	TREC(123, 0) = V8(I, 12)
	TREC(123, 1) = 0
	TREC(123, 2) = 0
	TRC$(123, 0) = B$(I)
	TRC$(123, 1) = VERSUS$(I)
	TRC$(123, 2) = B$(1 - I)
	TRC$(123, 3) = DATE$

30 END IF
	IF V8(I, 12) + V8(1 - I, 12) > TREC(124, 0) THEN
	TREC(124, 0) = V8(I, 12) + V8(1 - I, 12)
	TREC(124, 1) = V8(I, 12)
	TREC(124, 2) = V8(1 - I, 12)
	TRC$(124, 0) = B$(I)
	TRC$(124, 1) = VERSUS$(I)
	TRC$(124, 2) = B$(1 - I)
	TRC$(124, 3) = DATE$

31 END IF

IF V8(I, 12) + V8(1 - I, 12) < TREC(125, 0) THEN
	TREC(125, 0) = V8(I, 12) + V8(1 - I, 12)
	TREC(125, 1) = V8(I, 12)
	TREC(125, 2) = V8(1 - I, 12)
	TRC$(125, 0) = B$(I)
	TRC$(125, 1) = VERSUS$(I)
	TRC$(125, 2) = B$(1 - I)
	TRC$(125, 3) = DATE$

32 END IF

NEXT

OPEN P$(1) + "RECORDS.GAM" FOR OUTPUT AS #1
	FOR I = 0 TO 48
		FOR I1 = 0 TO 2
			WRITE #1, REC(I, I1)
		NEXT
		FOR I1 = 0 TO 4
			WRITE #1, RC$(I, I1)
		NEXT
	NEXT
	
	FOR I = 0 TO 125
		FOR I1 = 0 TO 2
			WRITE #1, TREC(I, I1)
		NEXT
		FOR I1 = 0 TO 3
			WRITE #1, TRC$(I, I1)
		NEXT
	NEXT
CLOSE 1
END SUB

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
