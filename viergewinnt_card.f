C     FORTRAN 77 PROGRAMM 'VIERGEWINNT' (MODERNES FORTRAN 1977)               1A
      PROGRAM VIERGEWINNT                                                     2A
      IMPLICIT NONE                                                           3A
      INTEGER LESESPALTE,GETTOPELEMENT,BREITE,HOEHE                           4A
      LOGICAL CHECKWIN,WON                                                    5A
      PARAMETER(BREITE=7, HOEHE=6)                                            6A
      INTEGER FELD(BREITE,HOEHE),SPIELER,ZUG,X,Y,COUNT,I,TOP,SPALTE           7A
      CHARACTER*10 NAME1,NAME2                                                8A
      COMMON /NAMES/ NAME1, NAME2                                             9A
      DO 10 Y=1, HOEHE                                                       10A
         DO 20 X=1, BREITE                                                   11A
            FELD(X,Y) = 0                                                    12A
 20      CONTINUE                                                            13A
 10   CONTINUE                                                               14A
      SPIELER = 1                                                            15A
      ZUG = 0                                                                16A
      WRITE(*,*) 'WILKOMMEN ZU VIERGEWINNT'                                  17A
      WRITE(*,*) 'SPIELER 1 GIB DEINEN NAMEN EIN:'                           18A
      READ (*,*) NAME1                                                       19A
      WRITE(*,*) 'SPIELER 2 GIB DEINEN NAMEN EIN:'                           20A
      READ (*,*) NAME2                                                       21A
      WRITE(*,*) 'HALLO ',NAME1,' UND ',NAME2,' DANKE FUERS SPIELEN'         22A
 80   CONTINUE                                                               23A
      IF (ZUG .LE. 6*7) THEN                                                 24A
        CALL SCHREIBEFELD(FELD)                                              25A
        SPALTE = LESESPALTE(SPIELER)                                         26A
        TOP = GETTOPELEMENT(FELD,SPALTE)                                     27A
        FELD(SPALTE, TOP) = SPIELER                                          28A
        CALL SCHREIBEFELD(FELD)                                              29A
        WON = CHECKWIN(FELD,SPIELER)                                         30A
        IF (WON .EQV. .TRUE.) GOTO 90                                        31A
        IF (SPIELER .EQ. 1) THEN                                             32A
           SPIELER = 2                                                       33A
        ELSE IF (SPIELER .EQ. 2) THEN                                        34A
           SPIELER = 1                                                       35A
        END IF                                                               36A
        WRITE(*,*) ' ----- NAECHSTER ZUG ----- '                             37A
        GOTO 80                                                              38A
      END IF                                                                 39A
 90   CONTINUE                                                               40A
      IF(SPIELER .EQ. 1) WRITE (*,*) NAME1,' HAT GEWONNEN'                   41A
      IF(SPIELER .EQ. 2) WRITE (*,*) NAME2,' HAT GEWONNEN'                   42A
      END                                                                    43A
      LOGICAL FUNCTION CHECKSPALTE(FELD, SPIELER)                            44A
      INTEGER FELD(7,6), SPIELER, Y, X, COUNT                                45A
      CHECKSPALTE = .FALSE.                                                  46A
      DO 10 X=1,7                                                            47A
         COUNT = 0                                                           48A
         DO 20 Y=1,6                                                         49A
            IF (FELD(X,Y) .EQ. SPIELER) THEN                                 50A
               COUNT = COUNT + 1                                             51A
            ELSE                                                             52A
               COUNT = 0                                                     53A
            END IF                                                           54A
            IF (COUNT .GE. 4) THEN                                           55A
               CHECKSPALTE = .TRUE.                                          56A
               RETURN                                                        57A
            END IF                                                           58A
 20      CONTINUE                                                            59A
 10   CONTINUE                                                               60A
      END                                                                    61A
      LOGICAL FUNCTION CHECKZEILE(FELD, SPIELER)                             62A
      INTEGER FELD(7,6), SPIELER, Y, X, COUNT                                63A
      CHECKZEILE = .FALSE.                                                   64A
      DO 10 Y=1,6                                                            65A
         COUNT = 0                                                           66A
         DO 20 X=1,7                                                         67A
            IF (FELD(X,Y) .EQ. SPIELER) THEN                                 68A
               COUNT = COUNT + 1                                             69A
            ELSE                                                             70A
               COUNT = 0                                                     71A
            END IF                                                           72A
            IF (COUNT .GE. 4) THEN                                           73A
               CHECKZEILE = .TRUE.                                           74A
               RETURN                                                        75A
            END IF                                                           76A
 20      CONTINUE                                                            77A
 10   CONTINUE                                                               78A
      END                                                                    79A
      LOGICAL FUNCTION CHECKSUBDIAGONAL(FELD,SPIELER,OFFX,OFFY)              80A
      INTEGER FELD(7,6), SPIELER, OFFX, OFFY, I, C1, C2                      81A
      C1 = 0                                                                 82A
      C2 = 0                                                                 83A
      CHECKSUBDIAGONAL=.FALSE.                                               84A
      DO 10 I = 1,4                                                          85A
         IF(FELD(I+OFFX,I+OFFY) .EQ. SPIELER) THEN                           86A
            C1 = C1 + 1                                                      87A
         ELSE                                                                88A
            C1 = 0                                                           89A
         END IF                                                              90A
         IF(FELD(5-(I+OFFX),I+OFFY) .EQ. SPIELER) THEN                       91A
            C2 = C2 + 1                                                      92A
         ELSE                                                                93A
            C2 = 0                                                           94A
         END IF                                                              95A
         IF(C1 .GE. 4) CHECKSUBDIAGONAL=.TRUE.                               96A
         IF(C2 .GE. 4) CHECKSUBDIAGONAL=.TRUE.                               97A
 10   CONTINUE                                                               98A
      END                                                                    99A
      LOGICAL FUNCTION CHECKDIAGONAL(FELD, SPIELER)                         100A
      LOGICAL CHECKSUBDIAGONAL                                              101A
      INTEGER FELD(7,6), SPIELER, OFFX, OFFY                                102A
      LOGICAL RES                                                           103A
      CHECKDIAGONAL = .FALSE.                                               104A
      DO 20 OFFY = 0,6-4                                                    105A
         DO 10 OFFX = 0,7-4                                                 106A
            RES=CHECKSUBDIAGONAL(FELD,SPIELER,OFFX,OFFY)                    107A
            IF (RES) THEN                                                   108A
               CHECKDIAGONAL=.TRUE.                                         109A
               RETURN                                                       110A
            END IF                                                          111A
 10      CONTINUE                                                           112A
 20   CONTINUE                                                              113A
      END                                                                   114A
      LOGICAL FUNCTION CHECKWIN(FELD,SPIELER)                               115A
      INTEGER FELD(7,6), SPIELER                                            116A
      LOGICAL TMP,CHECKZEILE,CHECKSPALTE,CHECKDIAGONAL                      117A
      TMP = CHECKZEILE(FELD,SPIELER)                                        118A
      TMP = TMP .OR. CHECKSPALTE(FELD,SPIELER)                              119A
      TMP = TMP .OR. CHECKDIAGONAL(FELD,SPIELER)                            120A
      CHECKWIN = TMP                                                        121A
      END                                                                   122A
      SUBROUTINE SCHREIBEFELD(FELD)                                         123A
      CHARACTER*10 NAME1,NAME2                                              124A
      COMMON /NAMES/ NAME1, NAME2                                           125A
      INTEGER FELD(7,6), I                                                  126A
      WRITE (*,*) '1=',NAME1,' 2=',NAME2                                    127A
      WRITE (*,998) '-1.2.3.4.5.6.7-'                                       128A
      I = 6                                                                 129A
 20   CONTINUE                                                              130A
      IF ( I .GE. 1 ) THEN                                                  131A
         WRITE(*,999) FELD(:,I)                                             132A
         I = I - 1                                                          133A
         GOTO 20                                                            134A
      END IF                                                                135A
 998  FORMAT(15A)                                                           136A
 999  FORMAT('|',I1,'|',I1,'|',I1,'|',I1,'|',I1,'|',I1,'|',I1,'|')          137A
      END                                                                   138A
      INTEGER FUNCTION LESESPALTE(SPIELER)                                  139A
      CHARACTER*10 NAME1,NAME2                                              140A
      COMMON /NAMES/ NAME1, NAME2                                           141A
      INTEGER SPALTE,SPIELER                                                142A
      SPALTE = 0                                                            143A
 10   CONTINUE                                                              144A
      IF (SPIELER .EQ. 1) WRITE (*,*) NAME1 // ' GIB EINE SPALTE EIN:'      145A
      IF (SPIELER .EQ. 2) WRITE (*,*) NAME2 // ' GIB EINE SPALTE EIN:'      146A
      READ(*, 999) SPALTE                                                   147A
      IF (SPALTE .LT. 1 .OR. SPALTE .GT. 7) THEN                            148A
         WRITE(*,*) 'ETWAS WAR FALSCH'                                      149A
         GOTO 10                                                            150A
      END IF                                                                151A
      LESESPALTE=SPALTE                                                     152A
 999  FORMAT(I1)                                                            153A
      END                                                                   154A
      INTEGER FUNCTION GETTOPELEMENT(FELD, SPALTE)                          155A
      INTEGER FELD(7,6)                                                     156A
      INTEGER I, SPALTE, TOP                                                157A
      I = 1                                                                 158A
 10   CONTINUE                                                              159A
      IF (.NOT. (FELD(SPALTE, I) .EQ. 0)) THEN                              160A
      I = I + 1                                                             161A
      GOTO 10                                                               162A
      END IF                                                                163A
      GETTOPELEMENT = I                                                     164A
      END                                                                   165A
