      ******************************************************************
      * Author: gottdammer
      * Date:
      * Purpose:
      *>   * 5 - Coach Helper V3
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 05-CoachHelperV3.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
      *>      Currency Sign is "L" with Picture Symbol "L".
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  MENU-OPTION PIC X.


      *>   Registro de Jugadores
      *>       Disponibilidad(Status): 1 Current  2 Market
      *>       Posicion: 1 Goalkeepers 2 Defenders  3 Right Side
      *>                4 Left Side  5 Midfielders 6 Strikers
      *>       Price, Salary,
      *>       Performance: 1 GOOD 2 Normal
      *>
       01  PLAYERS.
           05  PLAYERS-R  OCCURS 44 TIMES INDEXED BY IDXPLAYER.
               10 PLY-NAME     PIC X(19).
               10 FILLER       PIC X VALUE SPACE.
               10 PLY-IDXSTAT  PIC 9.
               10 FILLER       PIC X VALUE SPACE.
               10 PLY-IDXPOS   PIC 9.
               10 FILLER       PIC X VALUE SPACE.
               10 PLY-PRICE    PIC 9(8).
               10 FILLER       PIC X VALUE SPACE.
               10 PLY-SALARY   PIC 9(8).
               10 FILLER       PIC X VALUE SPACE.
               10 PLY-IDXPERF  PIC 9.


      *>   TIPOS de Status del jugador
       01  TYPE-STATUS VALUE "Current Market".
           05 TYPE-STATUS-R OCCURS 2 TIMES.
              10 STATUSNAME PIC X(8).

      *>   Tipos Posicion
       01  TYPE-POSITION.
           05 TYPE-POSITION-R OCCURS 7 TIMES INDEXED BY IDXTYPEPOS.
               10 POSITIONNAME     PIC X(12).

      *>   TIPOS de Perfomance
       01  TYPE-PERFOM VALUE "Good  Normal".
           05 TYPE-TYPE-PERFOM-R OCCURS 2 TIMES.
              10 PERFOMNAME PIC X(6).

      *>   Filtros seleccionados:
       01  FLT-STATUS      PIC 9.
       01  FLT-POSITION    PIC 9.
       01  FLT-FEATURE     PIC 9.


       01  SORT-PLAYERS.
           05  SORT-PLAYERS-R  OCCURS 44 TIMES INDEXED BY IDXPLAYER.
               10 SORT-PLY-NAME     PIC X(19).
               10 FILLER       PIC X VALUE SPACE.
               10 SORT-PLY-IDXSTAT  PIC 9.
               10 FILLER       PIC X VALUE SPACE.
               10 SORT-PLY-IDXPOS   PIC 9.
               10 FILLER       PIC X VALUE SPACE.
               10 SORT-PLY-PRICE    PIC 9(8).
               10 FILLER       PIC X VALUE SPACE.
               10 SORT-PLY-SALARY   PIC 9(8).
               10 FILLER       PIC X VALUE SPACE.
               10 SORT-PLY-IDXPERF  PIC 9.

       01  AUX-PLAYER.
               10 AUX-PLY-NAME     PIC X(19).
               10 FILLER       PIC X VALUE SPACE.
               10 AUX-PLY-IDXSTAT  PIC 9.
               10 FILLER       PIC X VALUE SPACE.
               10 AUX-PLY-IDXPOS   PIC 9.
               10 FILLER       PIC X VALUE SPACE.
               10 AUX-PLY-PRICE    PIC 9(8).
               10 FILLER       PIC X VALUE SPACE.
               10 AUX-PLY-SALARY   PIC 9(8).
               10 FILLER       PIC X VALUE SPACE.
               10 AUX-PLY-IDXPERF  PIC 9.

       01  CONST-MAX-PLAYERS   PIC 999 VALUE 44.

       77  TIPO-MONEDA     Pic $$$,$$$,$$9.

       01  AUX-LINE-POS    PIC 99.
       01  AUX-COL-POS     PIC 99 VALUE 5.
       01  AUX-9           PIC 9.
       01  AUX-99          PIC 99.
       01  AUX-999         PIC 999.
       01  AUX-I           PIC 999.
       01  AUX-J           PIC 999.
       01  AUX-MSG         PIC X(100).
       01  RETURN-MSG-YES_NO PIC X.




       PROCEDURE DIVISION.
       MAIN.
           PERFORM CARGA-PARAMETROS.
           PERFORM MENU-START THRU MENU-START-END.
           STOP RUN.





       MENU-START.
           PERFORM CLEAR-SCREEN.
      *>      DISPLAY "Filter availables: "               LINE 3 COL 5
           DISPLAY "Filter Available on:"      LINE 4 COL 5 REVERSED.
           DISPLAY "Filter by Position:"       LINE 4 COL 45 REVERSED.
           DISPLAY "Adittional Filters:"       LINE 4 COL 85 REVERSED.

           DISPLAY "A - Only Current Squad"            LINE 5 COL 5
                   "S - Only in Market"                LINE 6 COL 5
                   "D - ALL"                           LINE 7 COL 5

                   "Q - Goalkeepers"                   LINE 5 COL 35
                   "W - Defenders"                     LINE 6 COL 35
                   "E - Right Side"                    LINE 7 COL 35
                   "R - Left Side"                     LINE 8 COL 35
                   "T - Midfielders"                   LINE 5 COL 55
                   "Y - Strikers"                      LINE 6 COL 55
                   "U - ALL"                           LINE 7 COL 55

                   "1 - Only by Market Price"          LINE 5 COL 85
                   "2 - Only by Salary"                LINE 6 COL 85
                   "3 - Performance"                   LINE 7 COL 85
                   "4 - ALL"                           LINE 8 COL 85

                   "Your option:"                      LINE 10 COL 5
                   .

       REPEAT-MENU.
           ACCEPT MENU-OPTION  LINE 10 COL 18 PROMPT.

           MOVE FUNCTION UPPER-CASE(MENU-OPTION) TO MENU-OPTION.
           EVALUATE MENU-OPTION
               WHEN "A"
                   MOVE 1 TO FLT-STATUS
               WHEN "S"
                   MOVE 2 TO FLT-STATUS
               WHEN "D"
                   MOVE 0 TO FLT-STATUS

               WHEN "Q"
                   MOVE 1 TO FLT-POSITION
               WHEN "W"
                   MOVE 2 TO FLT-POSITION
               WHEN "E"
                   MOVE 3 TO FLT-POSITION
               WHEN "R"
                   MOVE 4 TO FLT-POSITION
               WHEN "T"
                   MOVE 5 TO FLT-POSITION
               WHEN "Y"
                   MOVE 6 TO FLT-POSITION
               WHEN "U"
                   MOVE 0 TO FLT-POSITION

               WHEN "1"
                   MOVE 1 TO FLT-FEATURE
               WHEN "2"
                   MOVE 2 TO FLT-FEATURE
               WHEN "3"
                   MOVE 3 TO FLT-FEATURE
               WHEN "4"
                   MOVE 0 TO FLT-FEATURE

           END-EVALUATE.

      *>   Display breadcrumbs de filtros seteados
           INITIALIZE AUX-MSG.
           EVALUATE FLT-STATUS
               WHEN 1
                   MOVE "Current_Squad, " TO AUX-MSG
               WHEN 2
                   MOVE "In_Market, " TO AUX-MSG
           END-EVALUATE.

           EVALUATE FLT-POSITION
               WHEN 1
                   STRING  AUX-MSG DELIMITED BY SPACE
                           "Goalkeepers, "
                   INTO AUX-MSG
               WHEN 2
                   STRING  AUX-MSG DELIMITED BY SPACE
                           "Defenders, "
                   INTO AUX-MSG
               WHEN 3
                   STRING  AUX-MSG DELIMITED BY SPACE
                           "Right_Side, "
                   INTO AUX-MSG
               WHEN 4
                   STRING  AUX-MSG DELIMITED BY SPACE
                           "Left_Side, "
                   INTO AUX-MSG
               WHEN 5
                   STRING  AUX-MSG DELIMITED BY SPACE
                           "Midfielders, "
                   INTO AUX-MSG
               WHEN 6
                   STRING  AUX-MSG DELIMITED BY SPACE
                           "Strikers, "
                   INTO AUX-MSG
           END-EVALUATE.

           EVALUATE FLT-FEATURE
               WHEN 1
                   STRING  AUX-MSG DELIMITED BY SPACE
                           "Price, "
                   INTO AUX-MSG
               WHEN 2
                   STRING  AUX-MSG DELIMITED BY SPACE
                           "Salary, "
                   INTO AUX-MSG
               WHEN 3
                   STRING  AUX-MSG DELIMITED BY SPACE
                           "Perfomance, "
                   INTO AUX-MSG
           END-EVALUATE.

           DISPLAY AUX-MSG LINE 10 COL 45.

      *>   SORT
           PERFORM FUNC-SORT-PLRYS.
           PERFORM FUNC-VIEW-PLY.
           GO TO REPEAT-MENU.



       MENU-START-END.
           EXIT.

      *>
      *>   *************************************************************
      *>   Ordeno los players segun:
      *>       1- Tipo de posicion     => SORT-BY-POSIT
      *>       2- La caracteristica    => SORT-BY-FIELD
      *>
      *>   Realiza un bluble-sort para el ordenamiento.
      *>   *************************************************************
       FUNC-SORT-PLRYS.
           INITIALIZE SORT-PLAYERS.

      *>   Muevo los players segun la position a sort-players:
           PERFORM VARYING AUX-I FROM 1 BY 1
                                           UNTIL AUX-I>CONST-MAX-PLAYERS
      *>     Filtro por Disponibilidad
             IF PLY-IDXSTAT(AUX-I) = FLT-STATUS OR FLT-STATUS=0
      *>       Filtro por posicion
               IF PLY-IDXPOS(AUX-I) = FLT-POSITION OR FLT-POSITION=0
                       MOVE PLAYERS-R(AUX-I)
                       TO SORT-PLAYERS-R(AUX-I)
               END-IF
             END-IF
           END-PERFORM.
      *>      DISPLAY SORT-PLAYERS line 1 col 5.
      *>      ACCEPT OMITTED.
      *>   Buble-sort
           PERFORM VARYING AUX-I FROM 1 BY 1
                                           UNTIL AUX-I>CONST-MAX-PLAYERS

               ADD 1 TO AUX-I GIVING AUX-999
               PERFORM VARYING AUX-J FROM AUX-999 BY 1
                                           UNTIL AUX-J>CONST-MAX-PLAYERS

                   MOVE 0 TO AUX-9
                   EVALUATE FLT-FEATURE
                   WHEN 1
                    IF SORT-PLY-PRICE(AUX-I) < SORT-PLY-PRICE(AUX-J)
                             MOVE 1 TO AUX-9
                    END-IF
                   WHEN 2
                      IF SORT-PLY-SALARY(AUX-I) < SORT-PLY-SALARY(AUX-J)
                               MOVE 1 TO AUX-9
                       END-IF
                   WHEN 3
                    IF SORT-PLY-IDXPERF(AUX-I) > SORT-PLY-IDXPERF(AUX-J)
                           MOVE 1 TO AUX-9
                    END-IF
                   END-EVALUATE

      *>           Si AUX-9 = 1 (TRUE) intercambio I por J
                   IF AUX-9 = 1
                     INITIALIZE AUX-PLAYER
                     MOVE SORT-PLAYERS-R(AUX-I) TO AUX-PLAYER
                     MOVE SORT-PLAYERS-R(AUX-J) TO SORT-PLAYERS-R(AUX-I)
                     MOVE AUX-PLAYER TO SORT-PLAYERS-R(AUX-J)
               END-PERFORM
           END-PERFORM.

       FUNC-VIEW-PLY.
           PERFORM CLEAR-INFO-SCREEN.
           MOVE 15 TO AUX-LINE-POS.
           MOVE 1 TO AUX-COL-POS.
           DISPLAY "Name:"     LINE 12 COL AUX-COL-POS
                   "Status:"   LINE 13 COL AUX-COL-POS
                   "Position:" LINE 14 COL AUX-COL-POS
                   "Price:"    LINE 15 COL AUX-COL-POS
                   "Salary:"   LINE 16 COL AUX-COL-POS
                   "Perfoman:" LINE 17 COL AUX-COL-POS
                   .



           MOVE 12 TO AUX-LINE-POS.
           MOVE 15 TO AUX-COL-POS.
           MOVE 0 TO AUX-J.

           PERFORM VARYING AUX-I FROM 1 BY 1
                                       UNTIL AUX-I>CONST-MAX-PLAYERS

               IF SORT-PLY-IDXSTAT(AUX-I)>0 THEN
      *>           Nombre
                   DISPLAY SORT-PLY-NAME(AUX-I)
                                       LINE AUX-LINE-POS COL AUX-COL-POS
                   ADD 1 to AUX-LINE-POS
      *>           Estado
                   DISPLAY STATUSNAME(SORT-PLY-IDXSTAT(AUX-I))
                                       LINE AUX-LINE-POS COL AUX-COL-POS

                   ADD 1 to AUX-LINE-POS

      *>           Posicion
                   DISPLAY POSITIONNAME(SORT-PLY-IDXPOS(AUX-I))
                                       LINE AUX-LINE-POS COL AUX-COL-POS
                   ADD 1 to AUX-LINE-POS

      *>           Precio
                   MOVE SORT-PLY-PRICE(AUX-I) TO TIPO-MONEDA
                   DISPLAY TIPO-MONEDA LINE AUX-LINE-POS COL AUX-COL-POS
                   ADD 1 to AUX-LINE-POS

      *>           Salario
                   MOVE SORT-PLY-SALARY(AUX-I) TO TIPO-MONEDA
                   DISPLAY TIPO-MONEDA LINE AUX-LINE-POS COL AUX-COL-POS
                   ADD 1 to AUX-LINE-POS

      *>           Perfomance
                   DISPLAY PERFOMNAME(SORT-PLY-IDXPERF(AUX-I))
                                       LINE AUX-LINE-POS COL AUX-COL-POS

                   MOVE 12 TO AUX-LINE-POS
                   ADD 20 TO AUX-COL-POS

      *>
               END-IF
      *>          Supero el largo y avanzo otra fila abajo
               IF AUX-COL-POS > 80
                   MOVE 1 TO AUX-J
                   MOVE 15 TO AUX-COL-POS
               END-IF

               IF AUX-J = 1
                       DISPLAY "Name:" LINE 20 COL 1
                       "Status:"       LINE 21 COL 1
                       "Position:"     LINE 22 COL 1
                       "Price:"        LINE 23 COL 1
                       "Salary:"       LINE 24 COL 1
                       "Perfoman:"     LINE 25 COL 1
                       END-DISPLAY

                       MOVE 20 TO AUX-LINE-POS
               END-IF

           END-PERFORM.
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
           DISPLAY "=====  Coach Helper v2.5.3 ======="
                   LINE 2 COL 40 REVERSED.

       CARGA-PARAMETROS.
           MOVE "Andre Onana         1 1 25000000 15000000 1"
           TO PLAYERS-R(1).
           MOVE "Altay Bayindir      1 1 05000000 05000000 2"
           TO PLAYERS-R(2).
           MOVE "Tom Heaton          1 1 05000000 02000000 2"
           TO PLAYERS-R(3).
           MOVE "Victor Lindelof     1 2 10000000 07000000 2"
           TO PLAYERS-R(4).
           MOVE "Harry Maguire       1 2 15000000 10000000 1"
           TO PLAYERS-R(5).
           MOVE "Raphael Varane      1 2 15000000 10000000 1"
           TO PLAYERS-R(6).
           MOVE "Lisandro Martinez   1 2 15000000 08000000 1"
           TO PLAYERS-R(7).
           MOVE "Jonny Evans         1 2 07000000 04000000 1"
           TO PLAYERS-R(8).
           MOVE "Willy Kambwala      1 2 04000000 01000000 2"
           TO PLAYERS-R(9).
           MOVE "Rhys Bennett        1 2 04000000 02000000 2"
           TO PLAYERS-R(10).
           MOVE "Diogo Dalot         1 3 40000000 10000000 1"
           TO PLAYERS-R(11).
           MOVE "Aaron Wan Bissaka   1 3 30000000 09000000 2"
           TO PLAYERS-R(12).
           MOVE "Luke Shaw           1 4 40000000 16000000 1"
           TO PLAYERS-R(13).
           MOVE "Sergio Reguilon     1 4 20000000 08000000 2"
           TO PLAYERS-R(14).
           MOVE "Tyrell Malacia      1 4 16000000 07000000 2"
           TO PLAYERS-R(15).
           MOVE "Sofyan Amrabat      1 5 25000000 09000000 2"
           TO PLAYERS-R(16).
           MOVE "Mason Mount         1 5 25000000 12000000 2"
           TO PLAYERS-R(17).
           MOVE "Carlos Casemiro     1 5 25000000 15000000 1"
           TO PLAYERS-R(18).
           MOVE "Bruno Fernandes     1 5 50000000 11000000 1"
           TO PLAYERS-R(19).
           MOVE "Christian Eriksen   1 5 25000000 08000000 2"
           TO PLAYERS-R(20).
           MOVE "Scott McTominay     1 5 40000000 08000000 1"
           TO PLAYERS-R(21).
           MOVE "Hannibal Mejbri     1 5 12000000 02000000 1"
           TO PLAYERS-R(22).
           MOVE "Kobbie Mainoo       1 5 12000000 02000000 1"
           TO PLAYERS-R(23).
           MOVE "Daniel Gore         1 5 09000000 02000000 2"
           TO PLAYERS-R(24).
           MOVE "Alejandro Garnacho  1 6 12000000 10000000 1"
           TO PLAYERS-R(25).
           MOVE "Rasmus Hojlund      1 6 20000000 07000000 1"
           TO PLAYERS-R(26).
           MOVE "Marcus Rashford     1 6 25000000 10000000 1"
           TO PLAYERS-R(27).
           MOVE "Antony              1 6 25000000 10000000 2"
           TO PLAYERS-R(28).
           MOVE "Anthony Martial     1 6 18000000 04000000 2"
           TO PLAYERS-R(29).
           MOVE "Facundo Pellistri   1 6 18000000 06000000 1"
           TO PLAYERS-R(30).
           MOVE "Andriy Lunin        2 1 30000000 10000000 1"
           TO PLAYERS-R(31).
           MOVE "Dominic Livakovic   2 1 15000000 09000000 1"
           TO PLAYERS-R(32).
           MOVE "Rui Patricio        2 1 10000000 07000000 2"
           TO PLAYERS-R(33).
           MOVE "Yassine Bounou      2 1 14000000 09000000 2"
           TO PLAYERS-R(34).
           MOVE "Enzo Fernandez      2 5 35000000 15000000 1"
           TO PLAYERS-R(35).
           MOVE "Jamal Musiala       2 5 30000000 10000000 1"
           TO PLAYERS-R(36).
           MOVE "Arda Guler          2 5 18000000 09000000 2"
           TO PLAYERS-R(37).
           MOVE "Achraf Hakimi       2 3 20000000 15000000 1"
           TO PLAYERS-R(38).
           MOVE "Jeremie Frimpong    2 3 12000000 08000000 1"
           TO PLAYERS-R(39).
           MOVE "Ronald Araujo       2 3 15000000 10000000 1"
           TO PLAYERS-R(40).
           MOVE "Victor Osimhen      2 6 30000000 12000000 1"
           TO PLAYERS-R(41).
           MOVE "Harry Kane          2 6 40000000 15000000 1"
           TO PLAYERS-R(42).
           MOVE "Karim Benzema       2 6 20000000 20000000 2"
           TO PLAYERS-R(43).

      *>   Cargo tipo de position
           MOVE "Goalkeepers"  TO TYPE-POSITION-R(1).
           MOVE "Defenders"    TO TYPE-POSITION-R(2).
           MOVE "Right Side"   TO TYPE-POSITION-R(3).
           MOVE "Left Side"    TO TYPE-POSITION-R(4).
           MOVE "Midfielders"  TO TYPE-POSITION-R(5).
           MOVE "Strikers"     TO TYPE-POSITION-R(6).
           EXIT.

       END PROGRAM 05-CoachHelperV3.
