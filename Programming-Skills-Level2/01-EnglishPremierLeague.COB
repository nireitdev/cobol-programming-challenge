      ******************************************************************
      * Author: gottdammer
      * Date:
      * Purpose:
      *>   * 5 - Airlines flights
      *>
      *>   El login te la debo..CTRL+C CTRL+V de los ejerc. anteriores.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 01-EnglishPremierLeague.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  MENU-OPTION PIC 9.

       01  TEAMS.
           05  TEAMS-R OCCURS 6 TIMES INDEXED BY IDXTEAM.
               10 TEAM PIC X(25).
               10 POINTS PIC 99 VALUE ZEROES.

       01  GAMES.
           05 ROUND-R OCCURS 3 TIMES.
               10 GAMES-R OCCURS 15 TIMES INDEXED BY IDXGAME.
                   15  IDXTEAM-1   PIC 9.
                   15  TEAM-1-PTS  PIC 9.
                   15  IDXTEAM-2   PIC 9.
                   15  TEAM-2-PTS  PIC 9.
       01  IDXROUND PIC 9.

       01  RESULT PIC 9 VALUE ZERO.


       01  AUX-TEAM-1      PIC 9.
       01  AUX-TEAM-2      PIC 9.


       01  TEAMS-S.
           05  TEAMS-R-S OCCURS 6 TIMES.
           10 TEAM-S PIC X(25).
           10 POINTS-S PIC 99 VALUE ZEROES.



       01  AUX-LINE-POS    PIC 99.

       01  AUX-X           PIC X.
       01  AUX-9           PIC 9.
       01  AUX-I           PIC 9.
       01  AUX-J           PIC 9.
       01  AUX-99          PIC 99.
       01  AUX-999         PIC 999.
       01  AUX-9999        PIC 9999.
       01  AUX-MSG         PIC X(130).



       PROCEDURE DIVISION.
       MAIN.
           PERFORM STARTUP.
           PERFORM MENU-START .
           STOP RUN.
       STARTUP.
           MOVE "Manchester United" TO TEAM(2).
           MOVE "Liverpool" TO TEAM(1).
           MOVE "Arsenal" TO TEAM(3).
           MOVE "Chelsea" TO TEAM(4).
           MOVE "Manchester City" TO TEAM(5).
           MOVE "Tottenham Hotspur" TO TEAM(6).
           EXIT.


       MENU-START.

       *>   Combinatoria de partidos a jugar:
           PERFORM VARYING IDXROUND FROM 1 BY 1 UNTIL IDXROUND>3
           PERFORM CLEAR-SCREEN
           DISPLAY  "MATCH Nr: "  LINE 5 COL 35 REVERSED
                    IDXROUND      LINE 5 COL 45 REVERSED

           MOVE 7 TO AUX-LINE-POS

           PERFORM VARYING AUX-I FROM 1 BY 1 UNTIL AUX-I>6
               COMPUTE AUX-9 = AUX-I + 1
               PERFORM VARYING AUX-J FROM AUX-9 BY 1 UNTIL AUX-J>6
                   PERFORM RESULT-GAME
                   EVALUATE RESULT
                       WHEN 0
                           MOVE 0 TO AUX-TEAM-1
                           MOVE 3 TO AUX-TEAM-2
                           ADD 3 TO POINTS(AUX-J)
                       WHEN 1
                           MOVE 1 TO AUX-TEAM-1
                           MOVE 1 TO AUX-TEAM-2
                           ADD 1 TO POINTS(AUX-I)
                           ADD 1 TO POINTS(AUX-J)
                       WHEN 2
                           MOVE 3 TO AUX-TEAM-1
                           MOVE 0 TO AUX-TEAM-2
                           ADD 3 TO POINTS(AUX-I)
                   END-EVALUATE

                   STRING
                       TEAM(AUX-I) DELIMITED BY SIZE
                       " [ "  DELIMITED by SIZE
                       AUX-TEAM-1
                       " ]"  DELIMITED by SIZE
                       "    vs   "  DELIMITED by SIZE
                       TEAM(AUX-J) DELIMITED BY SIZE
                       " [ "  DELIMITED by SIZE
                       AUX-TEAM-2
                       " ]"  DELIMITED by SIZE
                       INTO AUX-MSG
                   DISPLAY AUX-MSG LINE AUX-LINE-POS COL 5

                   ADD 1 to AUX-LINE-POS
               END-PERFORM

           END-PERFORM
               PERFORM TABLA-PUNTAJES
               PERFORM MENU-DISPLAYCONTINUE

           END-PERFORM.

           DISPLAY     "   WINNER:  "  LINE 25 COL 35
                       with UNDERLINE
                       TEAM-S(1) LINE 25 COL 47
                       .
           PERFORM MENU-DISPLAYCONTINUE.

       MENU-END.
           EXIT.


       TABLA-PUNTAJES.
      *>   Muestro puntaje de equipos ordeandos por puntaje
           PERFORM SORT-TEAMS.

           MOVE 7 TO AUX-LINE-POS
           PERFORM VARYING AUX-I FROM 1 BY 1 UNTIL AUX-I >6
                   DISPLAY  "TEAM STANDINGS"  LINE 5 COL 88 REVERSED

                   DISPLAY TEAM-S(AUX-I) LINE AUX-LINE-POS COL 85
                           POINTS-S(AUX-I) LINE AUX-LINE-POS COL 105
                   ADD 1 to AUX-LINE-POS
           END-PERFORM.





      *>   Calcula el resuyltado del partido referido al TEAM-1 :
      *>      0=PERDIO    1=EMPATO    2=GANO
      *>   Siempre va a beneficiar al gran Manchester!!!
       RESULT-GAME.
           COMPUTE AUX-9999 =  FUNCTION RANDOM() * 1000.
           IF AUX-9999 > 0 AND AUX-9999 < 300
                   MOVE 0 TO RESULT.
           IF AUX-9999 >= 300 AND AUX-9999 < 500
                   MOVE 1 TO RESULT.
           IF AUX-9999 >= 500
                   MOVE 2 TO RESULT.
           EXIT.


      *>   Hace un bluble sort en funcion de los puntajes obtenidos.
       SORT-TEAMS.
           PERFORM VARYING AUX-I FROM 1 BY 1 UNTIL AUX-I>6
               MOVE TEAM(AUX-I) TO TEAM-S(AUX-I)
               MOVE POINTS(AUX-I) TO POINTS-S(AUX-I)
           END-PERFORM.

           PERFORM VARYING AUX-I FROM 1 BY 1 UNTIL AUX-I>6
               MOVE AUX-I TO AUX-99
               ADD 1 TO AUX-99
               PERFORM VARYING AUX-J FROM AUX-99 BY 1 UNTIL AUX-J>6
                   IF POINTS-S(AUX-I) < POINTS-S(AUX-J)
                       MOVE TEAM-S(AUX-I) TO AUX-MSG
                       MOVE POINTS-S(AUX-I) TO AUX-9999

                       MOVE TEAM-S(AUX-J) TO TEAM-S(AUX-I)
                       MOVE POINTS-S(AUX-J) TO POINTS-S(AUX-I)

                       MOVE AUX-MSG TO TEAM-S(AUX-J)
                       MOVE AUX-9999 TO POINTS-S(AUX-J)

               END-PERFORM
           END-PERFORM.


      *>   *************************************************************
      *>   Utilidades
      *>   *************************************************************

      *>   Simple show info msg en el area de mensajeria (line 20 col 35
       SHOW-INFO-MSG.
           DISPLAY AUX-MSG LINE 22 COL 35.
           PERFORM MENU-DISPLAYCONTINUE.
           PERFORM CLEAR-INFO-SCREEN.


      *>   Simple <pause> para leer
       MENU-DISPLAYCONTINUE.
           DISPLAY "Apriete [ENTER] para continuar...."
                           LINE 29 COL 5.
           ACCEPT OMITTED.

       CLEAR-INFO-SCREEN.
           DISPLAY " " LINE 21 COL 1 ERASE EOS.

       CLEAR-SCREEN.
           DISPLAY " " LINE 1 COL 1 ERASE EOS.
           DISPLAY "=====  Big Six. v2.1 ======="
                   LINE 2 COL 40 REVERSED.

       END PROGRAM 01-EnglishPremierLeague.
