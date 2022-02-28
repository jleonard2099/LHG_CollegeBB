10 REM **************** REVISED NOVEMBER 1, 1988 ****************************
20 DIM A$(13), A(13, 6), Q$(250), X(13, 6), DF(13), P$(13), RS(13, 13), T1%(20)
30 DIM W$(11), N$(13), V$(500), L%(4), T%(9), D$(1), item$(7)
40 OPEN "DEV.BAS" FOR INPUT AS #2: INPUT #2, D$(0): CLOSE 2

FOR T1 = 1 TO 181

9000 OPEN "C:\COLBB\TEAMS\BASK.60" FOR RANDOM AS #1 LEN = 655
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 2 AS Q$(16), 2 AS Q$(17), 2 AS Q$(18), 2 _
 AS Q$(19), 602 AS X$
FOR I = 0 TO 13
FIELD #1, 53 + I * 43 AS X$, 15 AS Q$(I * 15 + 20), 2 AS Q$(I * 15 + 21), 2 AS Q$(I * 15 + 22), 2 AS Q$(I * 15 + 23), 2 AS Q$(I * 15 + 24), 2 AS Q$(I * 15 + 25), 2 AS Q$(I * 15 + 26), 2 AS Q$(I * 15 + 27), 2 AS Q$(I * 15 + 28), 2 AS Q$(I * 15 + 29) _
, 2 AS Q$(I * 15 + 30), 2 AS Q$(I * 15 + 31), 2 AS Q$(I * 15 + 32), 2 AS Q$(I * 15 + 33), 2 AS Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 AS X$
NEXT
GET #1, T1
   A$ = Q$(0)
9005 IF RIGHT$(A$, 1) = " " THEN A$ = LEFT$(A$, LEN(A$) - 1): GOTO 9005
     SP = CVI(Q$(1)): DA = CVI(Q$(2)): V1% = CVI(Q$(3)): V2% = CVI(Q$(4))
     FOR I = 0 TO 4: L%(I) = CVI(Q$(I + 5)): NEXT
     FOR I = 0 TO 9: T%(I) = CVI(Q$(I + 10)): NEXT
     FOR I = 0 TO 13
     A$(I) = Q$(I * 15 + 20)
9007 IF RIGHT$(A$(I), 1) = " " THEN A$(I) = LEFT$(A$(I), LEN(A$(I)) - 1): GOTO 9007
     FOR I1 = 0 TO 6
     A(I, I1) = CVI(Q$(I * 15 + I1 + 21))
     X(I, I1) = CVI(Q$(I * 15 + I1 + 28))
     NEXT: NEXT
     CLOSE 1

'     DA = DA + 1
'     SP = SP - 3
'     FOR I = 0 TO 13
'     A(I, 0) = A(I, 0) + 1
'     NEXT
     PRINT T1, A$


OPEN "WLT.60" FOR APPEND AS #1
WRITE #1, T%(7),T%(8),T%(9)
CLOSE 1
NEXT
END


     OPEN "C:\DLD\BASK.61S" FOR RANDOM AS #1 LEN = 655
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 2 AS Q$(16), 2 AS Q$(17), 2 AS Q$(18), 2 _
 AS Q$(19), 602 AS X$
FOR I = 0 TO 13
FIELD #1, 53 + I * 43 AS X$, 15 AS Q$(I * 15 + 20), 2 AS Q$(I * 15 + 21), 2 AS Q$(I * 15 + 22), 2 AS Q$(I * 15 + 23), 2 AS Q$(I * 15 + 24), 2 AS Q$(I * 15 + 25), 2 AS Q$(I * 15 + 26), 2 AS Q$(I * 15 + 27), 2 AS Q$(I * 15 + 28), 2 AS Q$(I * 15 + 29) _
, 2 AS Q$(I * 15 + 30), 2 AS Q$(I * 15 + 31), 2 AS Q$(I * 15 + 32), 2 AS Q$(I * 15 + 33), 2 AS Q$(I * 15 + 34), 655 - 53 - 43 - I * 43 AS X$
NEXT
     LSET Q$(0) = A$: LSET Q$(1) = MKI$(SP): LSET Q$(2) = MKI$(DA): LSET Q$(3) = MKI$(V1%)
     LSET Q$(4) = MKI$(V2%): FOR I = 0 TO 4: LSET Q$(I + 5) = MKI$(L%(I)): NEXT
     T%(2) = 999
     FOR I = 0 TO 9: LSET Q$(I + 10) = MKI$(T%(I)): NEXT
     FOR I = 0 TO 13
     LSET Q$(I * 15 + 20) = A$(I)
     FOR I1 = 0 TO 6
     LSET Q$(I * 15 + I1 + 21) = MKI$(A(I, I1))
     LSET Q$(I * 15 + I1 + 28) = MKI$(X(I, I1))
     NEXT: NEXT
     PUT #1, T1: CLOSE 1

'     NEXT T1
'     END


