'$INCLUDE: 'KeyInput.BI'

' THIS IS A UTILITY TO FILL IN THE BLANKS TO AVOID ENTERING ON MULTIPLE
' SEASONS SUCH AS COACH NAME, ARENA, ETC
' DEVELOPED IN CONJUNCTION WITH ADDSTATS.BAS
' QUICK AND DIRTY WAY TO ENTER W/L, POWER RATING

10 ' **************** REVISED NOVEMBER 1, 1988 ****************************

20DIM A$(13),A(13,6),Q$(250),X(13,6), DF(13),P$(13),RS(13,13),T1%(20)

30DIM W$(11),N$(13),V$(500)
DIM t$(340),a1$(340),s$(340),c$(340),att(340)

40 OPEN "DEV.BAS" FOR INPUT AS #2:INPUT #2,D$(0):CLOSE 2

open "specs.80" for input as #2

for t1 = 1 to 265
input #2, t$(t1),a1$(t1),c$(t1),s$(t1),att(t1)
next
close 2

for t1 = 1 to 265
yn$ = "80"

9000 OPEN D$(0)+"BASK."+YN$ AS #1 LEN=655
FIELD #1,15 AS Q$(0),2 AS Q$(1),2 AS Q$(2),2 AS Q$(3),2 AS Q$(4),2 AS Q$(5),2 AS Q$(6),2 AS Q$(7),2 AS Q$(8),2 AS Q$(9),2 AS Q$(10),2 AS Q$(11),2 AS Q$(12),2 AS Q$(13),2 AS Q$(14),2 AS Q$(15),2 AS Q$(16),2 AS Q$(17),2 AS Q$(18),2 AS Q$(19),602 AS _
X$
FOR I=0 TO 13
FIELD #1,53+I*43 AS X$,15 AS Q$(I*15+20),2 AS Q$(I*15+21),2 AS Q$(I*15+22),2 AS Q$(I*15+23),2 AS Q$(I*15+24),2 AS Q$(I*15+25),2 AS Q$(I*15+26),2 AS Q$(I*15+27),2 AS Q$(I*15+28),2 AS Q$(I*15+29),2 AS Q$(I*15+30),2 AS Q$(I*15+31),2 AS Q$(I*15+32),2 _
AS Q$(I*15+33),2 AS Q$(I*15+34),655-53-43-I*43 AS X$
NEXT
GET #1,T1
	A$=Q$(0)

9005 IF RIGHT$(A$,1)=" " THEN A$=LEFT$(A$,LEN(A$)-1):GOTO 9005
	SP=CVI(Q$(1)):DA=CVI(Q$(2)):V1%=CVI(Q$(3)):V2%=CVI(Q$(4))
	FOR I=0 TO 4:L%(I)=CVI(Q$(I+5)):NEXT
	FOR I=0 TO 9:T%(I)=CVI(Q$(I+10)):NEXT
	FOR I=0 TO 13
	A$(I)=Q$(I*15+20)

9007 IF RIGHT$(A$(I),1)=" " THEN A$(I)=LEFT$(A$(I),LEN(A$(I))-1):GOTO 9007
	FOR I1=0 TO 6
	A(I,I1)=CVI(Q$(I*15+I1+21))
	X(I,I1)=CVI(Q$(I*15+I1+28))
	NEXT:NEXT
CLOSE 1
	for t2 = 1 to 265
if a$ = t$(t2) then
	c$ = c$(t2)
	s$ = s$(t2)
	att = att(t2)
	a1$ = a1$(t2)
	goto 9010
end if
next

print a$
'input "nickname "a1$
input "coach "c$
input "arena "s$
input "attendance "att

9010 print USING "### ";T1;
	PRINT a$
	'input "wins "t%(7)
	'input "losses "t%(8)
	'input "power rating "t%(9)
OPEN D$(0)+"BASK."+YN$ AS #1 LEN=655
FIELD #1,15 AS Q$(0),2 AS Q$(1),2 AS Q$(2),2 AS Q$(3),2 AS Q$(4),2 AS Q$(5),2 AS Q$(6),2 AS Q$(7),2 AS Q$(8),2 AS Q$(9),2 AS Q$(10),2 AS Q$(11),2 AS Q$(12),2 AS Q$(13),2 AS Q$(14),2 AS Q$(15),2 AS Q$(16),2 AS Q$(17),2 AS Q$(18),2 AS Q$(19),602 AS _
X$
FOR I=0 TO 13
FIELD #1,53+I*43 AS X$,15 AS Q$(I*15+20),2 AS Q$(I*15+21),2 AS Q$(I*15+22),2 AS Q$(I*15+23),2 AS Q$(I*15+24),2 AS Q$(I*15+25),2 AS Q$(I*15+26),2 AS Q$(I*15+27),2 AS Q$(I*15+28),2 AS Q$(I*15+29),2 AS Q$(I*15+30),2 AS Q$(I*15+31),2 AS Q$(I*15+32),2 _
AS Q$(I*15+33),2 AS Q$(I*15+34),655-53-43-I*43 AS X$
NEXT
	LSET Q$(0)=A$:LSET Q$(1)=MKI$(SP):LSET Q$(2)=MKI$(DA):LSET Q$(3)=MKI$(V1%)
	LSET Q$(4)=MKI$(V2%):FOR I=0 TO 4:LSET Q$(I+5)=MKI$(L%(I)):NEXT
	T%(2)=999
	FOR I=0 TO 9:LSET Q$(I+10)=MKI$(T%(I)):NEXT
	FOR I=0 TO 13
	LSET Q$(I*15+20)=A$(I)
	FOR I1=0 TO 6
	LSET Q$(I*15+I1+21)=MKI$(A(I,I1))
	LSET Q$(I*15+I1+28)=MKI$(X(I,I1))
	NEXT:NEXT
	PUT #1,T1:CLOSE 1

OPEN D$(0)+"COLBBTMS."+YN$ AS #1 LEN=566
FIELD #1,15 AS Q$(0),40 AS Q$(1),15 AS Q$(2),4 AS Q$(3)
FOR I=0 TO 13
FIELD #1,74+I*32 AS X$,2 AS Q$(I*16+4),2 AS Q$(I*16+5),2 AS Q$(I*16+6),2 AS Q$(I*16+7),2 AS Q$(I*16+8),2 AS Q$(I*16+9),2 AS Q$(I*16+10),2 AS Q$(I*16+11),2 AS Q$(I*16+12),2 AS Q$(I*16+13),2 AS Q$(I*16+14),2 AS Q$(I*16+15),2 AS Q$(I*16+16),2 AS Q$(I_
*16+17),2 AS Q$(I*16+18),2 AS Q$(I*16+19)
FIELD #1, 522 AS X$,2 AS Q$(228),2 AS Q$(229),2 AS Q$(230),2 AS Q$(231),2 AS Q$(232),2 AS Q$(233),2 AS Q$(234),2 AS Q$(235),2 AS Q$(236),2 AS Q$(237),2 AS Q$(238),2 AS Q$(239),2 AS Q$(240),2 AS Q$(241),2 AS Q$(242),2 AS Q$(243),2 AS Q$(244),2 AS  _
Q$(245),2 AS Q$(246),2 AS Q$(247),2 AS Q$(248),2 AS Q$(249)
NEXT
	LSET Q$(0)=C$
	LSET Q$(1)=S$
	LSET Q$(2)=A1$
	LSET Q$(3)=MKL$(ATT)
	  FOR I=0 TO 13
	LSET Q$(I*16+4)=MKI$(DF(I))
	LSET Q$(I*16+5)=P$(I)
	FOR I1=0 TO 13
	LSET Q$(I*16+I1+6)=MKI$(RS(I,I1))
	NEXT:NEXT
	FOR I = 0 TO 20
	LSET Q$(228 + I) = MKI$(T1%(I))
	NEXT
	LSET Q$(249) = MKI$(999)
	PUT #1,T1:CLOSE 1

next t1

'$INCLUDE: 'KeyInput.BM'
