* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*     FORTRAN 77 Programm "Viergewinnt" (Modernes Fortran 1977)
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      PROGRAM VIERGEWINNT
      IMPLICIT NONE
*     PRE Deklaration
      INTEGER LESE_SPALTE,GET_TOP_ELEMENT,LEN
      LOGICAL CHECK_WIN
*     Konstanten
      INTEGER LEER, GELB, ROT, BREITE, HOEHE
      PARAMETER(LEER=0, GELB=1, ROT=2, BREITE=7, HOEHE=6)
*     Variablen mit 1-Byte Präzision (-127, 128)
      INTEGER FELD(BREITE,HOEHE)
      INTEGER SPIELER,ZUG,X,Y,COUNT,I,TOP,SPALTE
      LOGICAL WON
      CHARACTER*10 NAME1,NAME2
      COMMON /NAMES/ NAME1, NAME2
*     Initialisiere Speicher des Felds mit 0
      DO 10 Y=1, HOEHE
         DO 20 X=1, BREITE
            FELD(X,Y) = 0
 20      CONTINUE
 10   CONTINUE
      SPIELER = 1
      ZUG = 0
      WRITE(*,*) 'Wilkommen zu Viergewinnt'
      WRITE(*,*) 'Spieler 1 gib deinen Namen ein:'
      READ (*,*) NAME1
      WRITE(*,*) 'Spieler 2 gib deinen Namen ein:'
      READ (*,*) NAME2
      WRITE(*,*) 'Hallo ',NAME1,' und ',NAME2,' danke fürs Spielen'
*     GAME LOGIC
 80   CONTINUE
      IF (ZUG .LE. 6*7) THEN
        CALL SCHREIBEFELD(FELD)
        SPALTE = LESE_SPALTE(SPIELER)
        TOP = GET_TOP_ELEMENT(FELD,SPALTE)
        FELD(SPALTE, TOP) = SPIELER
        CALL SCHREIBEFELD(FELD)
        WON = CHECK_WIN(FELD,SPIELER)
        IF (WON .EQV. .TRUE.) GOTO 90
        IF (SPIELER .EQ. 1) THEN
           SPIELER = 2
        ELSE IF (SPIELER .EQ. 2) THEN
           SPIELER = 1
        END IF
        WRITE(*,*) ' ----- Nächster Zug ----- '
        GOTO 80
      END IF
 90   CONTINUE
      IF(SPIELER .EQ. 1) WRITE (*,*) NAME1,' hat gewonnen'
      IF(SPIELER .EQ. 2) WRITE (*,*) NAME2,' hat gewonnen'
      END

      LOGICAL FUNCTION CHECK_SPALTE(FELD, SPIELER)
      INTEGER FELD(7,6), SPIELER
      INTEGER Y, X, COUNT
      CHECK_SPALTE = .FALSE.
c$$$      WRITE(*,*) 'CHECK_SPALTE'
      DO 10 X=1,7
         COUNT = 0
         DO 20 Y=1,6
            IF (FELD(X,Y) .EQ. SPIELER) THEN
               COUNT = COUNT + 1
            ELSE
               COUNT = 0
            END IF
            IF (COUNT .GE. 4) THEN
               CHECK_SPALTE = .TRUE.
               RETURN
            END IF
 20      CONTINUE
c$$$  WRITE (*,*) 'Count',COUNT,'Feld',(FELD(X,J), J=1,6)
 10   CONTINUE
      END

      LOGICAL FUNCTION CHECK_ZEILE(FELD, SPIELER)
      INTEGER FELD(7,6), SPIELER
      INTEGER Y, X, COUNT
      CHECK_ZEILE = .FALSE.
c$$$      WRITE(*,*) 'CHECK_ZEILE'
      DO 10 Y=1,6
         COUNT = 0
         DO 20 X=1,7
            IF (FELD(X,Y) .EQ. SPIELER) THEN
               COUNT = COUNT + 1
            ELSE
               COUNT = 0
            END IF
            IF (COUNT .GE. 4) THEN
               CHECK_ZEILE = .TRUE.
               RETURN
            END IF
 20      CONTINUE
c$$$  WRITE (*,*) 'Count',COUNT,'Feld',(FELD(J,Y), J=1,7)
 10   CONTINUE
      END

      LOGICAL FUNCTION CHECK_SUB_DIAGONAL(FELD,SPIELER,OFFX,OFFY)
      INTEGER FELD(7,6), SPIELER, OFFX, OFFY
      INTEGER I, C1, C2
*     Zählt /
      C1 = 0
*     Zählt \
      C2 = 0
      CHECK_SUB_DIAGONAL=.FALSE.
      DO 10 I = 1,4
         IF(FELD(I+OFFX,I+OFFY) .EQ. SPIELER) THEN
            C1 = C1 + 1
         ELSE
            C1 = 0
         END IF
         IF(FELD(5-(I+OFFX),I+OFFY) .EQ. SPIELER) THEN
            C2 = C2 + 1
         ELSE
            C2 = 0
         END IF
         IF(C1 .GE. 4) CHECK_SUB_DIAGONAL=.TRUE.
         IF(C2 .GE. 4) CHECK_SUB_DIAGONAL=.TRUE.
 10   CONTINUE
      END

      LOGICAL FUNCTION CHECK_DIAGONAL(FELD, SPIELER)
      LOGICAL CHECK_SUB_DIAGONAL
      INTEGER FELD(7,6), SPIELER
      INTEGER OFFX, OFFY
      LOGICAL RES
      CHECK_DIAGONAL = .FALSE.
      DO 20 OFFY = 0,6-4
         DO 10 OFFX = 0,7-4
            RES=CHECK_SUB_DIAGONAL(FELD,SPIELER,OFFX,OFFY)
            IF (RES) THEN
               CHECK_DIAGONAL=.TRUE.
               RETURN
            END IF
 10      CONTINUE
 20   CONTINUE
      END

      LOGICAL FUNCTION CHECK_WIN(FELD,SPIELER)
      INTEGER FELD(7,6), SPIELER
      LOGICAL TMP,CHECK_ZEILE,CHECK_SPALTE,CHECK_DIAGONAL
      TMP = CHECK_ZEILE(FELD,SPIELER)
      TMP = TMP .OR. CHECK_SPALTE(FELD,SPIELER)
      TMP = TMP .OR. CHECK_DIAGONAL(FELD,SPIELER)
      CHECK_WIN = TMP
      END

      SUBROUTINE SCHREIBEFELD(FELD)
      CHARACTER*10 NAME1,NAME2
      COMMON /NAMES/ NAME1, NAME2
      INTEGER FELD(7,6), I
      WRITE (*,*) '1=',NAME1,' 2=',NAME2
      WRITE (*,998) '-1.2.3.4.5.6.7-'
      I = 6
 20   CONTINUE
      IF ( I .GE. 1 ) THEN
         WRITE(*,999) FELD(:,I)
         I = I - 1
         GOTO 20
      END IF
*     WRITE (*,999) FELD
 998  FORMAT(15A)
 999  FORMAT('|',I1,'|',I1,'|',I1,'|',I1,'|',I1,'|',I1,'|',I1,'|')
      END

      INTEGER FUNCTION LESE_SPALTE(SPIELER)
      CHARACTER*10 NAME1,NAME2
      COMMON /NAMES/ NAME1, NAME2
      INTEGER SPALTE,SPIELER
      SPALTE = 0
 10   CONTINUE
      IF (SPIELER .EQ. 1) WRITE (*,*) NAME1 // ' gib eine Spalte ein:'
      IF (SPIELER .EQ. 2) WRITE (*,*) NAME2 // ' gib eine Spalte ein:'
      READ(*, 999) SPALTE
      IF (SPALTE .LT. 1 .OR. SPALTE .GT. 7) THEN
         WRITE(*,*) 'Etwas war falsch'
         GOTO 10
      END IF
      LESE_SPALTE=SPALTE
 999  FORMAT(I1)
      END

      INTEGER FUNCTION GET_TOP_ELEMENT(FELD, SPALTE)
      INTEGER FELD(7,6)
      INTEGER I, SPALTE, TOP
      I = 1
 10   CONTINUE
      IF (.NOT. (FELD(SPALTE, I) .EQ. 0)) THEN
      I = I + 1
      GOTO 10
      END IF
      GET_TOP_ELEMENT = I
      END
