      ******************************************************************
      * Author: gottdammer
      * Date:
      * Purpose:
      *>   * 1 - Team Picker:
      *>      Randomize los equipos del Tottenham y del Manchester.
      *>      La formacion es 4-3-3 para los dos.
      *>      Segun la opcion se randomiza uno de los dos.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 01-TeamPicker.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  MENU-OPTION PIC X.

      *>   Guarda el plantel de cada equipo.
      *>   Solo guarda el ID del player.
       01  TEAMS.
           05 TEAM OCCURS 2 TIMES.
           10 TEAM-NAME        PIC X(18).
           10 TEAM-COMPOSITION OCCURS 11 TIMES.
               15 TEAM-IDXPLY  PIC 99.

      *>   Registro de Jugadores
      *>       Equipo: 1 Tottenham  2 Manchester
      *>       Posicion: 1 Goalkeepers 2 Defenders
      *>                 3 Midfielders 4 Strikers
      *>       Puntos
       01  PLAYERS.
           05  PLAYER OCCURS 56 TIMES INDEXED BY IDXPLAYER.
               10 PLY-NAME     PIC X(20).
               10 PLY-IDXTEAM  PIC 9.
               10 PLY-IDXPOS   PIC 9.
               10 PLY-POINTS   PIC 99.

      *>   Tipos Posicion
       01  TYPE-POSITION VALUE
                   "Goalkeepers Defenders   Midfielders Forwards".
           05 TYPE-POSITION-R OCCURS 4 TIMES INDEXED BY IDXTYPEPOS.
               10 POSITIONNAME     PIC X(12).

       01  CONST-MAX-PLAYERS   PIC 99 VALUE 56.

       01  RAND-LIST-IN.
           05 RAND-LIST-IN-R OCCURS 10 TIMES.
           10 RAND-ELEM-IN  PIC 99.

       01  RAND-LIST-OUT.
           05  RAND-LIST-OUT-R OCCURS 10 TIMES.
           10  RAND-ELEM-OUT  PIC 99.

      *>   Indica el equipo a randomizar
       01  RAND-TEAM   PIC 9.
      *>   Indica la posicion a filtar para el random
       01  RAND-POSITION   PIC 9.
      *>   Indica cuantos itesm elige random de la lista
       01  RAND-COUNT      PIC 9.

       01  AUX-LINE-POS    PIC 99.
       01  AUX-COL-POS     PIC 99 VALUE 5.
       01  AUX-BOOL        PIC X.
       01  AUX-9           PIC 9.
       01  AUX-99          PIC 99.
       01  AUX-999         PIC 999.
       01  AUX-COUNT       PIC 99.
       01  AUX-I           PIC 999.
       01  AUX-J           PIC 999.
       01  AUX-MSG         PIC X(100).
       01  RETURN-MSG-YES_NO PIC X.

       01  RANDOMNUMBER.
           10 FILLER PIC 9(6).
           10 RNDM  PIC 99.



       PROCEDURE DIVISION.
       MAIN.
           PERFORM CARGA-PARAMETROS.
           PERFORM MENU-START THRU MENU-START-END.
           STOP RUN.

       MENU-START.
           PERFORM CLEAR-SCREEN.

           PERFORM MAKE-TOTTENHAM.
           PERFORM MAKE-MANCHESTER.

           DISPLAY "1 - Randomize TOTTENHAM  Team"         LINE 5 COL 5
                   "2 - Randomize MANCHESTER Team"         LINE 6 COL 5
                   "0 - Quit"                              LINE 7 COL 5
                   "Your option:"                      LINE 9 COL 5
                   .

       REPEAT-MENU.
           ACCEPT MENU-OPTION  LINE 9 COL 18 PROMPT.
           EVALUATE MENU-OPTION
               WHEN 1
                   PERFORM MAKE-TOTTENHAM
               WHEN 2
                   PERFORM MAKE-MANCHESTER
               WHEN 0
                   STOP RUN
               WHEN OTHER
                   Display "Invalid Option. Retry" LINE 20 COL 5
                   PERFORM MENU-DISPLAYCONTINUE
                   GO TO REPEAT-MENU
           END-EVALUATE.

           PERFORM DISPLAY-TEAM.

           GO TO REPEAT-MENU.

       MENU-START-END.
           EXIT.

       MAKE-MANCHESTER.
           MOVE 2 TO RAND-TEAM.
           PERFORM MAKE-TEAMS.

       MAKE-TOTTENHAM.
           MOVE 1 TO RAND-TEAM.
           PERFORM MAKE-TEAMS.

       MAKE-TEAMS.
      *>   Pickeo random los 11 jugadores: 1 4 3 3
      *>   Randomize 1 arquero
           MOVE 1 TO RAND-COUNT.
           MOVE 1 TO RAND-POSITION.
           PERFORM RANDOM-LIST.
           MOVE RAND-ELEM-OUT(1) TO TEAM-IDXPLY(RAND-TEAM,1).

      *>   Randomize Defensor (ID=2) y cant 4
           MOVE 4 TO RAND-COUNT.
           MOVE 2 TO RAND-POSITION.
           PERFORM RANDOM-LIST.
           PERFORM VARYING AUX-I FROM 1 BY 1 UNTIL AUX-I > RAND-COUNT
               ADD 1 TO AUX-I GIVING AUX-J
               MOVE RAND-ELEM-OUT(AUX-I) TO TEAM-IDXPLY(RAND-TEAM,AUX-J)
           END-PERFORM.

      *>   Randomize Midlanes (ID=3) y cant 3
           MOVE 3 TO RAND-COUNT.
           MOVE 3 TO RAND-POSITION.
           PERFORM RANDOM-LIST.
           PERFORM VARYING AUX-I FROM 1 BY 1 UNTIL AUX-I > RAND-COUNT
               ADD 5 TO AUX-I GIVING AUX-J
               MOVE RAND-ELEM-OUT(AUX-I) TO TEAM-IDXPLY(RAND-TEAM,AUX-J)
           END-PERFORM.

      *>   Randomize Delanteros (ID=4) y cant 3
           MOVE 3 TO RAND-COUNT.
           MOVE 4 TO RAND-POSITION.
           PERFORM RANDOM-LIST.
           PERFORM VARYING AUX-I FROM 1 BY 1 UNTIL AUX-I > RAND-COUNT
               ADD 8 TO AUX-I GIVING AUX-J
               MOVE RAND-ELEM-OUT(AUX-I) TO TEAM-IDXPLY(RAND-TEAM,AUX-J)
           END-PERFORM.

       DISPLAY-TEAM.
           PERFORM VARYING AUX-COUNT FROM 1 BY 1 UNTIL AUX-COUNT>2
               MOVE 5 TO AUX-COL-POS
               COMPUTE AUX-99 =  10 * ( AUX-COUNT - 1 ) + 13

               IF AUX-COUNT = 1 THEN
                   MOVE 11 TO AUX-LINE-POS
               ELSE
                   MOVE 22 TO AUX-LINE-POS
               END-IF
               DISPLAY TEAM-NAME(AUX-COUNT)
                       LINE AUX-LINE-POS COL AUX-COL-POS REVERSED


               MOVE AUX-99 TO AUX-LINE-POS
               DISPLAY "Goalkeeper:"   LINE AUX-LINE-POS COL AUX-COL-POS
               ADD 1 TO AUX-LINE-POS
               DISPLAY PLY-NAME(TEAM-IDXPLY(AUX-COUNT,1))
                                       LINE AUX-LINE-POS COL AUX-COL-POS

               ADD 20 TO AUX-COL-POS
               MOVE AUX-99 TO AUX-LINE-POS
               DISPLAY "Defenders:"    LINE AUX-LINE-POS COL AUX-COL-POS

               PERFORM VARYING AUX-I FROM 2 BY 1 UNTIL AUX-I>5
               ADD 1 TO AUX-LINE-POS
               DISPLAY PLY-NAME(TEAM-IDXPLY(AUX-COUNT,AUX-I))
                                       LINE AUX-LINE-POS COL AUX-COL-POS
               END-PERFORM


               ADD 20 TO AUX-COL-POS
               MOVE AUX-99 TO AUX-LINE-POS
               DISPLAY "Midfielders:"  LINE AUX-LINE-POS COL AUX-COL-POS

               PERFORM VARYING AUX-I FROM 6 BY 1 UNTIL AUX-I>8
               ADD 1 TO AUX-LINE-POS
               DISPLAY PLY-NAME(TEAM-IDXPLY(AUX-COUNT,AUX-I))
                                       LINE AUX-LINE-POS COL AUX-COL-POS
               END-PERFORM

               ADD 20 TO AUX-COL-POS
               MOVE AUX-99 TO AUX-LINE-POS
               DISPLAY "Forwards:"     LINE AUX-LINE-POS COL AUX-COL-POS

               PERFORM VARYING AUX-I FROM 9 BY 1 UNTIL AUX-I>11
               ADD 1 TO AUX-LINE-POS
               DISPLAY PLY-NAME(TEAM-IDXPLY(AUX-COUNT,AUX-I))
                                       LINE AUX-LINE-POS COL AUX-COL-POS
               END-PERFORM

           END-PERFORM.

      *>   *************************************************************
      *>   De la lista "RAND-LIST-IN" con numeros retorna otra
      *>   lista "RAND-LIST-OUT" con numeros tomados de la primer lista.
      *>   La cantidad esta indicada por "RAND-COUNT"
      *>   *************************************************************
       RANDOM-LIST.
           INITIALIZE RAND-LIST-IN.
           INITIALIZE RAND-LIST-OUT.
           PERFORM ARRANGE-LIST-IN.

           PERFORM RND-LOOP THRU RANDOM-LOOP-END
               VARYING AUX-COUNT FROM 1 BY 1 UNTIL AUX-COUNT>RAND-COUNT.

      *>   Arma la lista "RAND-LIST-IN" de acuerdo con los paremtros
      *>   recibidos en RAND-TEAM, RAND-POSITION y RAND-COU
       ARRANGE-LIST-IN.
           MOVE 1 to AUX-J.
           PERFORM VARYING AUX-I FROM 1 BY 1
                                       UNTIL AUX-I > CONST-MAX-PLAYERS
               IF PLY-IDXTEAM(AUX-I) = RAND-TEAM
                   AND PLY-IDXPOS(AUX-I) = RAND-POSITION
                       MOVE AUX-I TO RAND-ELEM-IN(AUX-J)
                       ADD 1 TO AUX-J
               END-IF
           END-PERFORM.

       RND-LOOP.
      *>   BUG:Debido a que la funcion RANDOM() esta BUGGEADA
      *>   tengo que tomar el numero random de las decimas del tiempo
      *>   El Nro random va de 1 a 99
           ACCEPT RANDOMNUMBER FROM TIME.
           DIVIDE RNDM BY 9 GIVING AUX-99 REMAINDER AUX-99.
           ADD 1 to AUX-99.
           MOVE 1 TO AUX-I.

       RND-LOOP-IN.
           IF AUX-99 = 0 AND  RAND-ELEM-IN(AUX-I) > 0
      *>           Encontre el N-esimo numero random y lo muevo a OUT
                   MOVE RAND-ELEM-IN(AUX-I) TO RAND-ELEM-OUT(AUX-COUNT)
                   MOVE ZERO TO RAND-ELEM-IN(AUX-I)
                   GO TO RANDOM-LOOP-END
           END-IF
           IF RAND-ELEM-IN(AUX-I) > 0
                   SUBTRACT 1 FROM AUX-99
           END-IF.
           ADD 1 TO AUX-I.
           IF AUX-I>9
               MOVE 1 TO AUX-I
           END-IF.
           GO TO RND-LOOP-IN.

       RANDOM-LOOP-END.
           EXIT.


      *>   *************************************************************
      *>   Utilidades
      *>   *************************************************************

      *>   Simple show info msg en el area de mensajeria
       SHOW-INFO-MSG-YES_NO.
           DISPLAY AUX-MSG LINE 23 COL 35.
           DISPLAY "Y/N:" LINE 24 COL 50.
           ACCEPT RETURN-MSG-YES_NO LINE 24 COL 55 PROMPT.
           IF RETURN-MSG-YES_NO = "y"
                   MOVE "Y" TO RETURN-MSG-YES_NO.
           PERFORM MENU-DISPLAYCONTINUE.
           PERFORM CLEAR-INFO-SCREEN.

      *>   Simple show info msg en el area de mensajeria (line 20 col 35
       SHOW-INFO-MSG.
           DISPLAY AUX-MSG LINE 23 COL 35.
           PERFORM MENU-DISPLAYCONTINUE.
           PERFORM CLEAR-INFO-SCREEN.

      *>   Simple <pause> para leer
       MENU-DISPLAYCONTINUE.
           DISPLAY "Apriete [ENTER] para continuar...."
                           LINE 30 COL 1.
           ACCEPT OMITTED.

       CLEAR-INFO-SCREEN.
           DISPLAY " " LINE 12 COL 1 ERASE EOS.

       CLEAR-SCREEN.
           DISPLAY " " LINE 1 COL 1 ERASE EOS.
           DISPLAY "=====  Team Picker V 3.1 ======="
                   LINE 2 COL 40 REVERSED.

       CARGA-PARAMETROS.
           MOVE "Hugo Lloris         1185" TO PLAYER(1)
           MOVE "Guglielmo Vicario   1179" TO PLAYER(2)
           MOVE "Fraser Forster      1179" TO PLAYER(3)
           MOVE "Brandon Austin      1179" TO PLAYER(4)
           MOVE "Eric Dier           1280" TO PLAYER(5)
           MOVE "Cristian Romero     1280" TO PLAYER(6)
           MOVE "Davinson Ssanchez   1274" TO PLAYER(7)
           MOVE "Japhet Tanganga     1270" TO PLAYER(8)
           MOVE "Matt Doherty        1270" TO PLAYER(9)
           MOVE "Djed Spence         1270" TO PLAYER(10)
           MOVE "Sergio Reguilon     1274" TO PLAYER(11)
           MOVE "Ben Davies          1276" TO PLAYER(12)
           MOVE "Joe Rodon           1270" TO PLAYER(13)
           MOVE "Mislav Orsic        1271" TO PLAYER(14)
           MOVE "Oliver Skipp        1370" TO PLAYER(15)
           MOVE "Pierre-Emile Hojbjer1370" TO PLAYER(16)
           MOVE "Yves Bissouma       1372" TO PLAYER(17)
           MOVE "James Maddison      1374" TO PLAYER(18)
           MOVE "Giovani Lo Celso    1378" TO PLAYER(19)
           MOVE "Ryan Sessegnon      1380" TO PLAYER(20)
           MOVE "Dejan Kulusevski    1360" TO PLAYER(21)
           MOVE "Pape Sarr           1365" TO PLAYER(22)
           MOVE "Rodrigo Bentancur   1365" TO PLAYER(23)
           MOVE "Oliver Skipp        1365" TO PLAYER(24)
           MOVE "Son Heung-Min       1478" TO PLAYER(25)
           MOVE "Richarlison         1482" TO PLAYER(26)
           MOVE "Bryan Gil           1480" TO PLAYER(27)
           MOVE "Timo Werner         1482" TO PLAYER(28)
           MOVE "Brennan Johnson     1470" TO PLAYER(29)
           MOVE "Manor Solomon       1470" TO PLAYER(30)
           MOVE "Alejo Veliz         1475" TO PLAYER(31)
           MOVE "Dane Scarlett       1475" TO PLAYER(32)
           MOVE "Andre Onana         2180" TO PLAYER(33)
           MOVE "Tom Heaton          2175" TO PLAYER(34)
           MOVE "Altay Bayindir      2169" TO PLAYER(35)
           MOVE "Victor Lindelof     2280" TO PLAYER(36)
           MOVE "Harry Maguire       2282" TO PLAYER(37)
           MOVE "Lisandro Martinez   2282" TO PLAYER(38)
           MOVE "Tyrell Malacia      2267" TO PLAYER(39)
           MOVE "Raphael Varane      2280" TO PLAYER(40)
           MOVE "Diogo Dalot         2289" TO PLAYER(41)
           MOVE "Luke Shaw           2289" TO PLAYER(42)
           MOVE "Aaron Wan-Bissaka   2270" TO PLAYER(43)
           MOVE "Sofyan Amrabat      2376" TO PLAYER(44)
           MOVE "Scott McTominay     2380" TO PLAYER(45)
           MOVE "Bruno Fernandes     2388" TO PLAYER(46)
           MOVE "Christian Eriksen   2367" TO PLAYER(47)
           MOVE "Mason Mount         2377" TO PLAYER(48)
           MOVE "Kobbie Mainoo       2365" TO PLAYER(49)
           MOVE "Daniel Gore         2360" TO PLAYER(50)
           MOVE "Anthony Martial     2450" TO PLAYER(51)
           MOVE "Marcus Rashford     2476" TO PLAYER(52)
           MOVE "Antony              2475" TO PLAYER(53)
           MOVE "Rasmus Hojlund      2480" TO PLAYER(54)
           MOVE "Alejandro Garnacho  2485" TO PLAYER(55)
           MOVE "Facundo Pellistri   2475" TO PLAYER(56)


           MOVE "Tottenham Hotspur" TO TEAM-NAME(1).
           MOVE "Manchester United" TO TEAM-NAME(2).

           EXIT.

       END PROGRAM 01-TeamPicker.
