      ******************************************************************
      * Author: gottdammer
      * Date:
      * Purpose:
      *>   * 3 - Coach helper Improved
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 03-COACHHELPER-V2.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  MENU-OPTION PIC 9.

      *>   Cant de registros PLAYERS usables para guardar datos.
      *>      No se cuenta el ultimo de la tabla porque es temporal.
       01  MAXCOUNT-PLAYERS PIC 99 VALUE 15.

      *>      Cuantos players existen cargados. No se cuenta el temporal
       01  COUNTER-PLAYERS PIC 99 VALUE 0.

      *>   Registro de jugadores
      *>      El ultimo se usa como registro temporal auxiliar.
       01  PLAYERS.
           05  PLAYER  OCCURS 16 TIMES INDEXED BY IDX.
               10 NAMEPL   PIC X(20).
               10 GOALS    PIC 99.
               10 SPEED    PIC 99.
               10 ASSIST   PIC 99.
               10 PASSING  PIC 99.
               10 DEFENSE  PIC 99.
               10 JERSEY   PIC 99.
               10 IDXPOSIT PIC 99  VALUE 1.
               10 AGE      PIC 99.
               10 HEIGHT   PIC 999.

       01  POSITIONS VALUE "DELANTERO ARQUERO   DEFENSOR  CENTRO".
           05 POSITION-R OCCURS 5 TIMES INDEXED BY IDXPOS.
              10 NAMEPOSITION PIC X(10).

      *>   Indica el tipo de operacion
      *>      "CREATE" "VIEW" "UPDATE" "DELETE"
      *>   Podria haber usado numeros 0=Create 1=View 2=Update 3=Delete
       01  AUX-CRUD-OPER PIC X(6).

       01  AUX-COLUMN-POS  PIC 99.
       01  AUX-NRO-PLAYER  PIC 99.
       01  AUX-JERSEYSRCH  PIC 99.
       01  AUX-JERSEYSRCH-Z PIC Z9.
       01  AUX-FIELDSEARCH PIC X(5).
       01  AUX-MAX-VALUE   PIC 99.
       01  AUX-NROPLAYER-MAX-VALUE   PIC 99.
       01  AUX-VALUE       PIC 99.
       01  AUX-VALUE99     PIC 99.
       01  AUX-VALUE999    PIC 999.
       01  AUX-MSG         PIC X(100).

      *>   Devuelve "N" y  "Y" si el usuario confirmo el cambio.
       01  RETURN-MSG-YES_NO PIC X(1).


       PROCEDURE DIVISION.
       MAIN.
           PERFORM CARGA-PARAMETROS.
           PERFORM MENU-START.
           STOP RUN.


       CARGA-PARAMETROS.
           MOVE "Bruno Fernandes     0506091003080400000" TO PLAYER(1).
           MOVE "Rasmus Hojlund      1208020602110100000" TO PLAYER(2).
           MOVE "Harry Maguire       0105010709050300000" TO PLAYER(3).
           MOVE "Alejandro Garnacho  0807080600170100000" TO PLAYER(4).
           MOVE "Mason Mount         0206040801070400000" TO PLAYER(5).
           MOVE 5 TO COUNTER-PLAYERS.
           EXIT.



       MENU-START.
           PERFORM CLEAR-SCREEN.

           DISPLAY "Analisis:"                         LINE 5 COL 5
                   "[1] Player Review"                 LINE 6 COL 5
                   "[2] Compare Two Players"           LINE 7 COL 5
                   "Managment:"                        LINE 9 COL 5
                   "[3] Create New Player"             LINE 10 COL 5
                   "[4] Update Player"                 LINE 11 COL 5
                   "[5] Delete Player"                 LINE 12 COL 5
                   "Stadistics:"                       LINE 14 COL 5
                   "[6] Fastest player"                LINE 15 COL 5
                   "[7] Top Goal Scorer"               LINE 16 COL 5
                   "[8] Player with most assists"      LINE 17 COL 5
                   "[9] Player highest passing acc."   LINE 18 COL 5
                   "Overview:"                         LINE 20 COL 5
                   "[0] View table of players"         LINE 21 COL 5

                   .

           MOVE 0 to MENU-OPTION.
           DISPLAY "Opcion [ ]"                LINE 23 COL 5.
           ACCEPT MENU-OPTION                  LINE 23 COL 13 UPDATE.

           MOVE 5 TO AUX-COLUMN-POS.
           MOVE " " TO AUX-MSG.

      *>   Por defecto es VIEW
           MOVE "VIEW" TO AUX-CRUD-OPER.

           EVALUATE MENU-OPTION

               WHEN 1
                   MOVE "VIEW" TO AUX-CRUD-OPER
                   PERFORM SHOW-COMPARE-PLAYERS THRU INPUT-PLY1
                   PERFORM MENU-DISPLAYCONTINUE

               WHEN 2
                   PERFORM SHOW-COMPARE-PLAYERS THRU
                           SHOW-COMPARE-PLAYERS-END

               WHEN 3
                   MOVE "New player information:" TO AUX-MSG
                   MOVE "CREATE" TO AUX-CRUD-OPER
                   PERFORM SHOW-INFO-PLAYER

               WHEN 4
                   MOVE "Update player information:" TO AUX-MSG
                   MOVE "UPDATE" TO AUX-CRUD-OPER
                   PERFORM SHOW-COMPARE-PLAYERS THRU INPUT-PLY1

               WHEN 5
                   MOVE "Delete player:" TO AUX-MSG
                   PERFORM DELETE-PLAYER


               WHEN 6
                   MOVE "Fastest player:" TO AUX-MSG
                   MOVE "SPEED" TO AUX-FIELDSEARCH
                   PERFORM SEARCH-FIELD
                   PERFORM SHOW-INFO-PLAYER

               WHEN 7
                   MOVE "Top Goal Scorer:" TO AUX-MSG
                   MOVE "GOALS" TO AUX-FIELDSEARCH
                   PERFORM SEARCH-FIELD
                   PERFORM SHOW-INFO-PLAYER
               WHEN 8
                   MOVE "Player with most assists:" TO AUX-MSG
                   MOVE "ASSIS" TO AUX-FIELDSEARCH
                   PERFORM SEARCH-FIELD
                   PERFORM SHOW-INFO-PLAYER
               WHEN 9
                   MOVE "Player highest passing acc:" TO AUX-MSG
                   MOVE "PASSI" TO AUX-FIELDSEARCH
                   PERFORM SEARCH-FIELD
                   PERFORM SHOW-INFO-PLAYER
               WHEN 0
                   PERFORM SHOW-ALL-PLAYERS

      *>          WHEN 0
      *>              MOVE "Player most defensive:" TO AUX-MSG
      *>              MOVE "DEFEN" TO AUX-FIELDSEARCH
      *>              PERFORM SEARCH-FIELD
      *>              PERFORM SHOW-INFO-PLAYER

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


           DISPLAY "Name:"             LINE  8 COL 5
                   "Jersey Number"     LINE  9 COL 5
                   "Speed"             LINE 10 COL 5
                   "Assist"            LINE 11 COL 5
                   "Passing"           LINE 12 COL 5
                   "Defense"           LINE 13 COL 5
                   "Goals"             LINE 14 COL 5
                   "IDXPOSIT"          LINE 15 COL 5
                   "Agre"              LINE 16 COL 5
                   "Height cms"            LINE 17 COL 5
           END-DISPLAY.

           ADD 15 TO AUX-COLUMN-POS.
           IF AUX-CRUD-OPER = "VIEW" OR AUX-CRUD-OPER="UPDATE" THEN
               DISPLAY NAMEPL(AUX-NRO-PLAYER)LINE 8 COL AUX-COLUMN-POS
                   JERSEY(AUX-NRO-PLAYER)  LINE 9 COL AUX-COLUMN-POS
                   SPEED(AUX-NRO-PLAYER)   LINE 10 COL AUX-COLUMN-POS
                   ASSIST(AUX-NRO-PLAYER)  LINE 11 COL AUX-COLUMN-POS
                   PASSING(AUX-NRO-PLAYER) LINE 12 COL AUX-COLUMN-POS
                   DEFENSE(AUX-NRO-PLAYER) LINE 13 COL AUX-COLUMN-POS
                   GOALS(AUX-NRO-PLAYER)   LINE 14 COL AUX-COLUMN-POS
                   IDXPOSIT(AUX-NRO-PLAYER)   LINE 15 COL AUX-COLUMN-POS
                   AGE(AUX-NRO-PLAYER)   LINE 16 COL AUX-COLUMN-POS
                   HEIGHT(AUX-NRO-PLAYER)   LINE 17 COL AUX-COLUMN-POS
           END-IF.


           IF AUX-CRUD-OPER = "CREATE"
      *>       Nuevo jugador ultimo + 1
               ADD 1 TO COUNTER-PLAYERS GIVING AUX-NRO-PLAYER
           END-IF
           IF AUX-CRUD-OPER = "UPDATE"
      *>       Guardo el numero de player
               MOVE AUX-NRO-PLAYER TO AUX-VALUE999
      *>       Guardo en un temporal los datos a modificar:
               ADD 1 TO MAXCOUNT-PLAYERS GIVING AUX-NRO-PLAYER
               MOVE PLAYER(AUX-VALUE999) TO PLAYER(AUX-NRO-PLAYER)
           END-IF

           IF AUX-CRUD-OPER = "CREATE" OR AUX-CRUD-OPER="UPDATE" THEN

               ACCEPT  NAMEPL(AUX-NRO-PLAYER)  LINE 8
                                               COL AUX-COLUMN-POS UPDATE
               ACCEPT  JERSEY(AUX-NRO-PLAYER)   LINE 9
                                               COL AUX-COLUMN-POS UPDATE
               ACCEPT  SPEED(AUX-NRO-PLAYER)   LINE 10
                                               COL AUX-COLUMN-POS UPDATE
               ACCEPT  ASSIST(AUX-NRO-PLAYER)  LINE 11
                                               COL AUX-COLUMN-POS UPDATE
               ACCEPT  PASSING(AUX-NRO-PLAYER) LINE 12
                                               COL AUX-COLUMN-POS UPDATE
               ACCEPT  DEFENSE(AUX-NRO-PLAYER) LINE 13
                                               COL AUX-COLUMN-POS UPDATE
               ACCEPT  GOALS(AUX-NRO-PLAYER)  LINE 14
                                               COL AUX-COLUMN-POS UPDATE
               ACCEPT  IDXPOSIT(AUX-NRO-PLAYER)LINE 15
                                               COL AUX-COLUMN-POS UPDATE
               ACCEPT  AGE(AUX-NRO-PLAYER)     LINE 16
                                               COL AUX-COLUMN-POS UPDATE
               ACCEPT  HEIGHT(AUX-NRO-PLAYER)  LINE 17
                                               COL AUX-COLUMN-POS UPDATE

               IF AUX-CRUD-OPER="CREATE"
                   MOVE " A NEW PLAYER WILL BE CREATED." TO AUX-MSG
                   PERFORM SHOW-INFO-MSG-YES_NO
                   IF RETURN-MSG-YES_NO = "Y"
                       ADD 1 TO COUNTER-PLAYERS
                   ELSE
                       INITIALIZE PLAYER(AUX-NRO-PLAYER)
                   END-IF
               END-IF
               IF AUX-CRUD-OPER="UPDATE"
                   MOVE " UPDATE PLAYER DATA." TO AUX-MSG
                   PERFORM SHOW-INFO-MSG-YES_NO
                   IF RETURN-MSG-YES_NO = "Y"
      *>               Muevo datos temporal al lugar del player
                       MOVE PLAYER(AUX-NRO-PLAYER)
                           TO PLAYER(AUX-VALUE999)
                   ELSE
      *>               Borro temporal
                       INITIALIZE PLAYER(AUX-NRO-PLAYER)
                   END-IF
               END-IF

           END-IF.

           IF NOT AUX-MSG = " " THEN
               PERFORM MENU-DISPLAYCONTINUE
           END-IF.
           EXIT.

      *>   *************************************************************
      *>   Busca el nro interno de PLAYER segun el nro de jersey
      *>   Y lo guarda en AUX-NRO-PLAYER
      *>   *************************************************************
       SEARCH-JERSEY-NUMBER.
           MOVE 0 TO AUX-NRO-PLAYER.
           MOVE 0 TO AUX-VALUE.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX>MAXCOUNT-PLAYERS
               ADD 1 TO AUX-VALUE
               IF JERSEY(IDX) = AUX-JERSEYSRCH
                   MOVE AUX-VALUE TO AUX-NRO-PLAYER
           END-PERFORM.
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


       DELETE-PLAYER.
           PERFORM CLEAR-SCREEN.
           DISPLAY "Jersey number player to Delete: "  LINE 6 COL 5.
           ACCEPT AUX-JERSEYSRCH-Z             LINE 6 COL 40 PROMPT.
           MOVE AUX-JERSEYSRCH-Z TO AUX-JERSEYSRCH.
           PERFORM SEARCH-JERSEY-NUMBER
           IF AUX-NRO-PLAYER = 0
               DISPLAY "INVALID JERSEY NUMBER. "  LINE 20 COL 5
               PERFORM MENU-DISPLAYCONTINUE
               PERFORM CLEAR-INFO-SCREEN
               GO TO DELETE-PLAYER
           END-IF.
           MOVE "DELETE PLAYER DATA." TO AUX-MSG.
           PERFORM SHOW-INFO-MSG-YES_NO.
           IF RETURN-MSG-YES_NO = "Y" THEN
      *>       Limpio los datos del jugador:
               INITIALIZE PLAYER(AUX-NRO-PLAYER)
      *>       Y el 'hueco' lo lleno con el ultimo player. Defragmentar!
               MOVE PLAYER(COUNTER-PLAYERS) TO PLAYER(AUX-NRO-PLAYER)
               SUBTRACT 1 FROM COUNTER-PLAYERS
           END-IF.

       SHOW-ALL-PLAYERS.
           PERFORM CLEAR-SCREEN.
           DISPLAY "PLAYERS OVERVIEW "  LINE 6 COL 5.
           DISPLAY "NAME"              LINE 7  COL 5
                   "NUMB "             LINE 7  COL 25
                   "GOAL"              LINE 7  COL 30
                   "SPED"              LINE 7  COL 35
                   "ASIS"              LINE 7  COL 40
                   "PASS"              LINE 7  COL 45
                   "DEFE"              LINE 7  COL 50
                   "AGE "              LINE 7  COL 55
                   "HEIG"              LINE 7  COL 60
                   "POSITION"          LINE 7  COL 65
           END-DISPLAY.
           DISPLAY AUX-MSG LINE 6 COL 5.
           MOVE 8 TO AUX-VALUE99.
           PERFORM VARYING AUX-VALUE FROM 1 BY 1
                                       UNTIL AUX-VALUE > COUNTER-PLAYERS
               STRING  NAMEPL(AUX-VALUE)
                       " "
                       JERSEY(AUX-VALUE)
                       "   "
                       GOALS(AUX-VALUE)
                       "   "
                       SPEED(AUX-VALUE)
                       "   "
                       ASSIST(AUX-VALUE)
                       "   "
                       PASSING(AUX-VALUE)
                       "   "
                       DEFENSE(AUX-VALUE)
                       "   "
                       AGE(AUX-VALUE)
                       "   "
                       HEIGHT(AUX-VALUE)
                       " "
                       NAMEPOSITION(IDXPOSIT(AUX-VALUE))

               INTO AUX-MSG
               DISPLAY AUX-MSG LINE AUX-VALUE99 COL 5
               ADD 1 TO AUX-VALUE99
           END-PERFORM.
           PERFORM MENU-DISPLAYCONTINUE.
           EXIT.



      *>   *************************************************************
      *>   Utilidades
      *>   *************************************************************

      *>   Simple show info msg en el area de mensajeria
       SHOW-INFO-MSG-YES_NO.
           DISPLAY AUX-MSG LINE 23 COL 35.
           DISPLAY "ARE YO SURE? Y/N:" LINE 24 COL 40.
           ACCEPT RETURN-MSG-YES_NO LINE 24 COL 59 PROMPT.
           PERFORM MENU-DISPLAYCONTINUE.
           PERFORM CLEAR-INFO-SCREEN.

      *>   Simple show info msg en el area de mensajeria (line 20 col 35
       SHOW-INFO-MSG.
           DISPLAY AUX-MSG LINE 25 COL 35.
           PERFORM MENU-DISPLAYCONTINUE.
           PERFORM CLEAR-INFO-SCREEN.

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

       END PROGRAM 03-COACHHELPER-V2.
