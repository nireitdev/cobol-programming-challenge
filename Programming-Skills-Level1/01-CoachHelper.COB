      ******************************************************************
      * Author: gottdammer
      * Date:
      * Purpose:
      *>   * 1 - Coach helper
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 01-COACHHELPER.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  MENU-OPTION PIC 9.

       01  MAXCOUNT-PLAYERS PIC 9 VALUE 5.
       01  PLAYERS.
           05  PLAYER  OCCURS 5 TIMES INDEXED BY IDX.
               10 NAMEPL   PIC X(20).
               10 GOALS    PIC 99.
               10 SPEED    PIC 99.
               10 ASSIST   PIC 99.
               10 PASSING  PIC 99.
               10 DEFENSE  PIC 99.
               10 JERSEY   PIC 99.

       01  AUX-COLUMN-POS  PIC 99.
       01  AUX-NRO-PLAYER  PIC 9.
       01  AUX-JERSEYSRCH  PIC 99.
       01  AUX-JERSEYSRCH-Z PIC Z9.
       01  AUX-FIELDSEARCH PIC X(5).
       01  AUX-MAX-VALUE   PIC 99.
       01  AUX-NROPLAYER-MAX-VALUE   PIC 9.
       01  AUX-VALUE       PIC 99.
       01  AUX-VALUE99     PIC 99.
       01  AUX-MSG         PIC X(30).


       PROCEDURE DIVISION.
       MAIN.
           PERFORM CARGA-PARAMETROS.
           PERFORM MENU-START.
           STOP RUN.

      *>   *************************************************************
      *>   Carga las categorias por defecto
      *>   *************************************************************
       CARGA-PARAMETROS.
           MOVE "Bruno Fernandes     050609100308" TO PLAYER(1).
           MOVE "Rasmus Hojlund      120802060211" TO PLAYER(2).
           MOVE "Harry Maguire       010501070905" TO PLAYER(3).
           MOVE "Alejandro Garnacho  080708060017" TO PLAYER(4).
           MOVE "Mason Mount         020604080107" TO PLAYER(5).
           EXIT.


      *>   *************************************************************
      *>   Menu carga de entradas y salidas.
      *>   Cada entrada/salida tiene una descripcion y un monto
      *>   La categoria [1] es Ingreso de dinero y las demas Salidas.
      *>   *************************************************************
       MENU-START.
           PERFORM CLEAR-SCREEN.

           DISPLAY "[1] Player Review"                 LINE 5 COL 5
                   "[2] Compare Two Players"           LINE 6 COL 5
                   "[3] Fastest player"                LINE 7 COL 5
                   "[4] Top Goal Scorer"               LINE 8 COL 5
                   "[5] Player with most assists"      LINE 9 COL 5
                   "[6] Player highest passing acc."   LINE 10 COL 5
                   "[7] Player most defensive"         LINE 11 COL 5
                   "[0] EXIT"                          LINE 13 COL 5
                   .

           DISPLAY "Opcion [ ]"                        LINE 15 COL 5.
           ACCEPT MENU-OPTION                          LINE 15 COL 13.

           MOVE 5 TO AUX-COLUMN-POS.
           MOVE " " TO AUX-MSG.

           EVALUATE MENU-OPTION
               WHEN 0
                   DISPLAY "Bye!"
                   STOP RUN
               WHEN 1
                   PERFORM SHOW-COMPARE-PLAYERS THRU INPUT-PLY1
                   PERFORM MENU-DISPLAYCONTINUE

               WHEN 2
                   PERFORM SHOW-COMPARE-PLAYERS THRU
                           SHOW-COMPARE-PLAYERS-END

               WHEN 3
                   MOVE "Fastest player:" TO AUX-MSG
                   MOVE "SPEED" TO AUX-FIELDSEARCH
                   PERFORM SEARCH-FIELD
                   PERFORM SHOW-INFO-PLAYER

               WHEN 4
                   MOVE "Top Goal Scorer:" TO AUX-MSG
                   MOVE "GOALS" TO AUX-FIELDSEARCH
                   PERFORM SEARCH-FIELD
                   PERFORM SHOW-INFO-PLAYER
               WHEN 5
                   MOVE "Player with most assists:" TO AUX-MSG
                   MOVE "ASSIS" TO AUX-FIELDSEARCH
                   PERFORM SEARCH-FIELD
                   PERFORM SHOW-INFO-PLAYER
               WHEN 6
                   MOVE "Player highest passing acc:" TO AUX-MSG
                   MOVE "PASSI" TO AUX-FIELDSEARCH
                   PERFORM SEARCH-FIELD
                   PERFORM SHOW-INFO-PLAYER
               WHEN 7
                   MOVE "Player most defensive:" TO AUX-MSG
                   MOVE "DEFEN" TO AUX-FIELDSEARCH
                   PERFORM SEARCH-FIELD
                   PERFORM SHOW-INFO-PLAYER

               WHEN OTHER
                   DISPLAY "Opcion Incorrecta!!"   LINE 20 COL 5
                           "Reintente por favor!!" LINE 22 COL 5
                   PERFORM MENU-DISPLAYCONTINUE
           END-EVALUATE.
           GO TO MENU-START.


      *>   *************************************************************
      *>   Muestra info particular de un player
      *>   *************************************************************
       SHOW-INFO-PLAYER.
           IF NOT AUX-MSG = " " THEN
               PERFORM CLEAR-SCREEN
               DISPLAY AUX-MSG             LINE 5 COL AUX-COLUMN-POS
           END-IF

           DISPLAY "Name: "                LINE 8 COL AUX-COLUMN-POS
                   "Jersey Number: "       LINE 9 COL AUX-COLUMN-POS
                   "Goals: "               LINE 10 COL AUX-COLUMN-POS
                   "Speed: "               LINE 11 COL AUX-COLUMN-POS
                   "Assist: "              LINE 12 COL AUX-COLUMN-POS
                   "Passing: "             LINE 13 COL AUX-COLUMN-POS
                   "Defense: "             LINE 14 COL AUX-COLUMN-POS
                   .
           ADD 15 TO AUX-COLUMN-POS.
           DISPLAY NAMEPL(AUX-NRO-PLAYER)  LINE 8 COL AUX-COLUMN-POS
                   JERSEY(AUX-NRO-PLAYER)  LINE 9 COL AUX-COLUMN-POS
                   GOALS(AUX-NRO-PLAYER)   LINE 10 COL AUX-COLUMN-POS
                   SPEED(AUX-NRO-PLAYER)   LINE 11 COL AUX-COLUMN-POS
                   ASSIST(AUX-NRO-PLAYER)  LINE 12 COL AUX-COLUMN-POS
                   PASSING(AUX-NRO-PLAYER) LINE 13 COL AUX-COLUMN-POS
                   DEFENSE(AUX-NRO-PLAYER) LINE 14 COL AUX-COLUMN-POS
                   .

           IF NOT AUX-MSG = " " THEN
               PERFORM MENU-DISPLAYCONTINUE
           END-IF.
           EXIT.

      *>   *************************************************************
      *>   Busca el nro interno de PLAYER segun el nro de jersey
      *>   *************************************************************
       SEARCH-JERSEY-NUMBER.
           MOVE 0 TO AUX-NRO-PLAYER.
           MOVE 0 TO AUX-VALUE.
           PERFORM SEARCH-IDX-NUMBER VARYING IDX FROM 1 BY 1
                   UNTIL IDX>MAXCOUNT-PLAYERS.

       SEARCH-IDX-NUMBER.
           ADD 1 TO AUX-VALUE.
           IF JERSEY(IDX) = AUX-JERSEYSRCH
               MOVE AUX-VALUE TO AUX-NRO-PLAYER.
           EXIT.


      *>   *************************************************************
      *>   Segun el nro de campo indicado en AUX-FIELDSEARCH
      *>   se determina el maximo valor y se devuelve en AUX-NRO-PLAYER
      *>   el jugador.
      *>   *************************************************************
       SEARCH-FIELD.
           MOVE 0 TO AUX-NRO-PLAYER.
           MOVE 0 TO AUX-MAX-VALUE.
           MOVE 0 TO AUX-NROPLAYER-MAX-VALUE.

           PERFORM SEARCH-HIGHER VARYING IDX FROM 1 BY 1
                   UNTIL IDX>MAXCOUNT-PLAYERS.

           MOVE AUX-NROPLAYER-MAX-VALUE TO AUX-NRO-PLAYER.
           EXIT.

       SEARCH-HIGHER.
           ADD 1 TO AUX-NRO-PLAYER.
           EVALUATE AUX-FIELDSEARCH
               WHEN "GOALS"
                   MOVE GOALS(IDX) TO AUX-VALUE99
               WHEN "SPEED"
                   MOVE SPEED(IDX) TO AUX-VALUE99
               WHEN "ASSIS"
                   MOVE ASSIST(IDX) TO AUX-VALUE99
               WHEN "PASSI"
                   MOVE PASSING(IDX) TO AUX-VALUE99
               WHEN "DEFEN"
                   MOVE DEFENSE(IDX) TO AUX-VALUE99
           END-EVALUATE.
           IF  AUX-VALUE99 > AUX-MAX-VALUE
               MOVE AUX-VALUE99 TO AUX-MAX-VALUE
               MOVE AUX-NRO-PLAYER TO AUX-NROPLAYER-MAX-VALUE
           END-IF.
           EXIT.

       SHOW-COMPARE-PLAYERS.
           PERFORM CLEAR-SCREEN.
           DISPLAY "INFO PLAYERS " LINE 4 COL 5.

       INPUT-PLY1.
           DISPLAY "Jersey number player 1: "  LINE 6 COL 5.
           ACCEPT AUX-JERSEYSRCH-Z             LINE 6 COL 30 PROMPT.
           MOVE AUX-JERSEYSRCH-Z TO AUX-JERSEYSRCH.
           PERFORM SEARCH-JERSEY-NUMBER
           IF AUX-NRO-PLAYER = 0
               DISPLAY "INVALID JERSEY NUMBER. "  LINE 20 COL 5
               PERFORM MENU-DISPLAYCONTINUE
               PERFORM CLEAR-INFO-SCREEN
               GO TO INPUT-PLY1
           END-IF.
           MOVE 5 TO AUX-COLUMN-POS.
           PERFORM SHOW-INFO-PLAYER.

       INPUT-PLY2.
           DISPLAY "Jersey number player 2: " LINE 6 COL 45
           ACCEPT AUX-JERSEYSRCH-Z                LINE 6 COL 75 PROMPT
           MOVE AUX-JERSEYSRCH-Z TO AUX-JERSEYSRCH
           PERFORM SEARCH-JERSEY-NUMBER
           IF AUX-NRO-PLAYER = 0
               DISPLAY "INVALID JERSEY NUMBER. "  LINE 20 COL 5
               PERFORM MENU-DISPLAYCONTINUE
               PERFORM CLEAR-INFO-SCREEN
               GO TO INPUT-PLY2
           END-IF.
           MOVE 45 TO AUX-COLUMN-POS
           PERFORM SHOW-INFO-PLAYER

           PERFORM MENU-DISPLAYCONTINUE.

       SHOW-COMPARE-PLAYERS-END.
           EXIT.

       MENU-END.
           EXIT.


      *>   *************************************************************
      *>   Utilidades
      *>   *************************************************************

      *>   Simple <pause> para leer
       MENU-DISPLAYCONTINUE.
           DISPLAY "Apriete [ENTER] para continuar...."
                           LINE 28 COL 5.
           ACCEPT MENU-OPTION LINE 28 COL 40.

       CLEAR-INFO-SCREEN.
           DISPLAY " " LINE 20 COL 1 ERASE EOS.

       CLEAR-SCREEN.
           DISPLAY " " LINE 1 COL 1 ERASE EOS.
           DISPLAY "=====  COACH HELPER v1.1 ======="
                   LINE 2 COL 5 REVERSED.

       END PROGRAM 01-COACHHELPER.
