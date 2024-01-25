      ******************************************************************
      * Author: gottdammer
      * Date:
      * Purpose:
      *>   * 1 - Travel Agency
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 02-TRAVELAGENCY.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  MENU-OPTION PIC 9.

      *>  01  TRAVELOPTIONS PIC 9 VALUE 5.
       01  TRAVEL.
           05  SEASONS  OCCURS 4 TIMES INDEXED BY IDXSEAS.
               10 SEASON   PIC X(6).
               10 FILLER   PIC X VALUE SPACE.
               10 COST     PIC 9(4).
               10 FILLER   PIC X VALUE SPACE.
      *>           Indica cant de paises por season. Max 2 paises.
               10 COUNTER  PIC 9.
               10 FILLER   PIC X VALUE SPACE.
      *>           La cantidad que se repite se puede aumentar.
      *>           El problema da 2 opciones por temporada.
               10 PLACES OCCURS 2 TIMES INDEXED BY IDXPLC.
                   15 COUNTRY PIC X(10).
                   15 FILLER   PIC X VALUE SPACE.
                   15 TOUR  PIC 9 VALUE 0.
                   15 SKII  PIC 9 VALUE 0.
                   15 HIKG  PIC 9 VALUE 0.
                   15 BEAC  PIC 9 VALUE 0.
                   15 XTRM  PIC 9 VALUE 0.
                   15 FILLER   PIC X VALUE SPACE.

       01  CHOICE-AMOUNT PIC 9(5) VALUE ZEROES.
       01  CHOICE-AMOUNT-Z PIC Z(4)9 VALUE ZEROES.
       01  CHOICE-SEASON PIC 9 VALUE ZEROES.
       01  CHOICE-ACTIV PIC 9 VALUE ZEROES.

       01  AUX-LINE-POS    PIC 99.
       01  AUX-FOUND       PIC 9.
       01  AUX-RESULTS     PIC 9.
       01  AUX-9           PIC 9.
       01  AUX-99          PIC 99.
       01  AUX-MSG         PIC X(130).


       PROCEDURE DIVISION.
       MAIN.
           PERFORM CARGA-PARAMETROS.
           PERFORM MENU-START THRU MENU-END.
           STOP RUN.

      *>   *************************************************************
      *>   Carga las categorias por defecto
      *>   *************************************************************
       CARGA-PARAMETROS.
           MOVE "WINTER 0100 2 Andorra    01000 Switzerlnd 10000"
               TO SEASONS(1).
           MOVE "SUMMER 0400 2 Spain      00101 Portugal   00010"
               TO SEASONS(2).
           MOVE "SPRING 0300 2 France     00001 Italy      10000"
               TO SEASONS(3).
           MOVE "AUTUMN 0200 2 Belgium    00101 Austria    10000"
               TO SEASONS(4).
           EXIT.


      *>   *************************************************************
      *>   Menu carga de entradas y salidas.
      *>   Cada entrada/salida tiene una descripcion y un monto
      *>   La categoria [1] es Ingreso de dinero y las demas Salidas.
      *>   *************************************************************
       MENU-START.
           PERFORM CLEAR-SCREEN.
           MOVE ZEROS TO CHOICE-AMOUNT.
           MOVE ZEROS TO CHOICE-SEASON.
           MOVE ZEROS TO CHOICE-ACTIV.

           DISPLAY "Checklist for decision making:"     LINE 4 COL 5.

           MOVE 0 TO CHOICE-AMOUNT-Z.

           DISPLAY "Max Budget available (0=dont care)" LINE 6 COL 5.
           ACCEPT CHOICE-AMOUNT-Z  LINE 6 COL 40 PROMPT.
           MOVE CHOICE-AMOUNT-Z TO CHOICE-AMOUNT.
           PERFORM MSG-CHOICES.

           DISPLAY "Preferred Season (0=dont care)" LINE 8 COL 5
                   "[1] Winter" LINE 9 COL 5
                   "[2] Summer" LINE 10 COL 5
                   "[3] Spring" LINE 11 COL 5
                   "[4] Autumn" LINE 12 COL 5
                   "[0] Dont Care" LINE 13 COL 5
                   "Your Option: " LINE 15 COL 5
                   .
           ACCEPT CHOICE-SEASON  LINE 15 COL 18 PROMPT.
           PERFORM MSG-CHOICES.
           IF CHOICE-SEASON > 0
               IF CHOICE-AMOUNT > 0 AND
                   COST(CHOICE-SEASON)>CHOICE-AMOUNT THEN
                   DISPLAY  "The expenses for traveling during "
                                   LINE 20 COL 35
                           "that season are more than what you "
                                   LINE 21 COL 35
                           "can afford." LINE 22 COL 35
                   PERFORM MENU-DISPLAYCONTINUE
                   GO TO MENU-START
           END-IF.


           DISPLAY "Choice one activity:" LINE 8 COL 60
                   "[1] Tour actv" LINE 9 COL 60
                   "[2] Skiing" LINE 10 COL 60
                   "[3] Hiking" LINE 11 COL 60
                   "[4] Beach actv" LINE 12 COL 60
                   "[5] Extreme actv" LINE 13 COL 60
                   "Your Option: " LINE 15 COL 60
                   .
       CH-ACTIV.
           ACCEPT CHOICE-ACTIV LINE 15 COL 73 PROMPT.

           IF CHOICE-ACTIV >=1 AND CHOICE-ACTIV <= 5 THEN
               PERFORM MSG-CHOICES
               PERFORM SEARCH-ACTIVITY
           ELSE
               MOVE "NO VALID ACTIVITY. Choose one." TO AUX-MSG
               PERFORM SHOW-INFO-MSG
               GO TO CH-ACTIV
           END-IF

           IF CHOICE-AMOUNT = 0
               AND CHOICE-SEASON = 0
               AND CHOICE-ACTIV = 0
               MOVE "NO CHOICES HAVE MADE!!! WTF!!" TO AUX-MSG
               PERFORM SHOW-INFO-MSG
               .

       MENU-END.
           GO TO MENU-START.
           EXIT.


       SEARCH-ACTIVITY.
           MOVE 0 TO AUX-RESULTS.
           MOVE 20 TO AUX-LINE-POS.
           PERFORM SEARCH-COUNTRY VARYING IDXSEAS FROM 1 BY 1
                   UNTIL IDXSEAS>4.
           IF AUX-RESULTS = 0
               DISPLAY "NO RESULTS FOUND." LINE 20 COL 35.
           PERFORM MENU-DISPLAYCONTINUE.


       SEARCH-COUNTRY.
           PERFORM SEARCH-ACTIV VARYING IDXPLC FROM 1 BY 1
                   UNTIL IDXPLC>2.

       SEARCH-ACTIV.
           MOVE 0 TO AUX-FOUND.
           MOVE " " TO AUX-MSG.
           EVALUATE CHOICE-ACTIV
               WHEN 1
                   MOVE TOUR(IDXSEAS,IDXPLC) TO AUX-9
               WHEN 2
                   MOVE SKII(IDXSEAS,IDXPLC) TO AUX-9
               WHEN 3
                   MOVE HIKG(IDXSEAS,IDXPLC) TO AUX-9
               WHEN 4
                   MOVE BEAC(IDXSEAS,IDXPLC) TO AUX-9
               WHEN 5
                   MOVE XTRM(IDXSEAS,IDXPLC) TO AUX-9
           END-EVALUATE.

      *>   Encontre la actividad
           IF AUX-9 = 1 THEN
      *>       Eligio una temporada?
               IF CHOICE-SEASON > 0  AND IDXSEAS = CHOICE-SEASON THEN
      *    >           Eligio un monto maximo?
                       IF CHOICE-AMOUNT > 0 AND
                           COST(IDXSEAS)<=CHOICE-AMOUNT THEN
                           MOVE 1 TO AUX-FOUND
                       END-IF
                       IF CHOICE-AMOUNT=0 THEN
                           MOVE 1 TO AUX-FOUND
                       END-IF
               END-IF
               IF CHOICE-SEASON = 0
       *>          Eligio un monto maximo?
                   IF CHOICE-AMOUNT > 0
                       IF COST(IDXSEAS)<=CHOICE-AMOUNT THEN
                           MOVE 1 TO AUX-FOUND
                       END-IF
                   ELSE
                       MOVE 1 TO AUX-FOUND
                   END-IF

               END-IF

           END-IF.

           IF AUX-FOUND = 1 THEN
               STRING SEASON(IDXSEAS) " - " COUNTRY(IDXSEAS,IDXPLC)
                   INTO AUX-MSG
               DISPLAY AUX-MSG LINE AUX-LINE-POS COL 35
               ADD 1 TO AUX-LINE-POS
               MOVE 1 TO AUX-RESULTS
           END-IF.

       SEARCH-ACTIVITY-END.
           EXIT.


      *>   Arma el string de mensaje con las opciones elegidas
       MSG-CHOICES.

           MOVE "Choices=> " TO AUX-MSG.
           IF CHOICE-AMOUNT > 0
               STRING AUX-MSG DELIMITED BY SPACE
                   "Budget=$"  CHOICE-AMOUNT
                   ", "
                   INTO AUX-MSG.

           EVALUATE CHOICE-SEASON
               WHEN 1
                 STRING AUX-MSG DELIMITED BY SPACE
                   "Season=Winter, " INTO AUX-MSG
               WHEN 2
                 STRING AUX-MSG DELIMITED BY SPACE
                   "Season=Summer, " INTO AUX-MSG
               WHEN 3
                 STRING AUX-MSG DELIMITED BY SPACE
                   "Season=Spring, " INTO AUX-MSG
               WHEN 4
                 STRING AUX-MSG DELIMITED BY SPACE
                   "Season=Autumn, " INTO AUX-MSG
           END-EVALUATE
       EVALUATE CHOICE-ACTIV
               WHEN 1
                 STRING AUX-MSG DELIMITED BY SPACE
                   "Activ=Tour" INTO AUX-MSG
               WHEN 2
                 STRING AUX-MSG DELIMITED BY SPACE
                   "Activ=Ski" INTO AUX-MSG
               WHEN 3
                 STRING AUX-MSG DELIMITED BY SPACE
                   "Activ=Hiking" INTO AUX-MSG
               WHEN 4
                 STRING AUX-MSG DELIMITED BY SPACE
                   "Activ=Beach" INTO AUX-MSG
               WHEN 5
                 STRING AUX-MSG DELIMITED BY SPACE
                   "Activ=Extreme" INTO AUX-MSG
           END-EVALUATE
           DISPLAY AUX-MSG LINE 17 COL 5.
           MOVE SPACES to AUX-MSG.




      *>   *************************************************************
      *>   Utilidades
      *>   *************************************************************

      *>   Simple show info msg en el area de mensajeria (line 20 col 35
       SHOW-INFO-MSG.
           DISPLAY AUX-MSG LINE 20 COL 35.
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
           DISPLAY "=====  TRAVEL AGENCY v1.2 ======="
                   LINE 2 COL 5 REVERSED.

       END PROGRAM 02-TRAVELAGENCY.
