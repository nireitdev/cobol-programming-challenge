      ******************************************************************
      * Author: gottdammer
      * Date:
      * Purpose:
      *>   * 5 - Airlines flights
      *>
      *>   El login te la debo..CTRL+C CTRL+V de los ejerc. anteriores.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 05-AirlinesFlights.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  MENU-OPTION PIC 9.

       01  PAISES
           VALUE "Turkey    Greece    Lebanon   Spain     Portugal".
           05  PAIS-R OCCURS 5 TIMES INDEXED BY IDXPAIS.
               10 PAIS PIC X(10).

       01  TYPECONDITION VALUE "Economy   FirstClass".
           05  CONDIT-R OCCURS 2 TIMES.
               10 CONDIT PIC X(10).

       01  MEALS   VALUE "Regular    Vegetarian Kosher".
           05 MEALS-R OCCURS 3 TIMES.
               10 MEAL PIC X(11).

      *>   Estructura que guarda los dos viajes de IDA y el de VUELTA
      *>       FLIGHT-ROUTE: (1) IDA   (2)RETORNO
      *>       Condicion: "1" Economy  "2" Fist Class
      *>       Extra Luggage: "1" True  KG??
       01  FLIGHTS.
           05 FLIGHTS-R OCCURS 2 TIMES INDEXED BY FLIGHT-ROUTE.
               10 F-IDXPAIS PIC 9.
               10 F-DATE VALUE "20240101".
                   15 FDATE-YYYY   PIC 9999.
                   15 FDATE-MM     PIC 99.
                   15 FDATE-DD     PIC 99.
               10 FCONDITION       PIC 9 VALUE 1.
               10 FMEAL            PIC 9 VALUE 1.
               10 FEXTRALUGG       PIC 9 VALUE 0.


       01  AUX-LINE-POS    PIC 99.

       01  AUX-X           PIC X.
       01  AUX-9           PIC 9.
       01  AUX-99          PIC 99.
       01  AUX-999         PIC 999.
       01  AUX-9999        PIC 9999.
       01  AUX-MSG         PIC X(130).


       PROCEDURE DIVISION.
       MAIN.
           PERFORM MENU-START .
           STOP RUN.

       MENU-START.
      *>   *** VIAJE DE IDA ***
           PERFORM CLEAR-SCREEN.
           DISPLAY "Please choose options for DESTINATION flight"
                                           LINE 14 COL 35 WITH REVERSED.
           PERFORM MENU-DISPLAYCONTINUE.
           PERFORM CLEAR-SCREEN.
           DISPLAY "OUTBOUND FLIGHT: DESTINATION FLIGHT"
                                           LINE 4 COL 5 WITH REVERSED.
           MOVE 1 TO FLIGHT-ROUTE.
           PERFORM FLIGHT-OPTIONS THRU FLIGHT-OPTIONS-END.


      *>   *** VIAJE DE VUELTA ***
           PERFORM CLEAR-SCREEN.
           DISPLAY "Please choose options for RETURN flight"
                                           LINE 14 COL 35 WITH REVERSED.
           PERFORM MENU-DISPLAYCONTINUE.
           PERFORM CLEAR-SCREEN.
           DISPLAY "RETURN FLIGHT  : ORIGIN COUNTRY"
                                           LINE 4 COL 5 WITH REVERSED.
           MOVE 2 TO FLIGHT-ROUTE.
           PERFORM FLIGHT-OPTIONS THRU FLIGHT-OPTIONS-END.

      *>   *** PRINT TICKET ***
           PERFORM CLEAR-SCREEN.
           DISPLAY "Now we will print your flight ticket"
                                           LINE 14 COL 35 WITH REVERSED.
           PERFORM MENU-DISPLAYCONTINUE.
           PERFORM CLEAR-SCREEN.
           PERFORM PRINT-TICKET.

       MENU-END.
           GO TO MENU-START.
           EXIT.

       FLIGHT-OPTIONS.
           MOVE 6 TO AUX-LINE-POS.
           PERFORM VARYING AUX-99 FROM 1 BY 1 UNTIL AUX-99>5
               DISPLAY AUX-99                  LINE AUX-LINE-POS COL 5
               DISPLAY "["                     LINE AUX-LINE-POS COL 5
               DISPLAY "]"                     LINE AUX-LINE-POS COL 7
               DISPLAY PAIS(AUX-99)            LINE AUX-LINE-POS COL 10
               ADD 1 TO AUX-LINE-POS
           END-PERFORM.
           DISPLAY "Your choice: "     LINE 11  COL 5 .
       REPEAT-CNTRY.
           ACCEPT AUX-9  LINE 11  COL 18 PROMPT .
           EVALUATE AUX-9
               WHEN < 5
                   MOVE AUX-9 TO F-IDXPAIS(FLIGHT-ROUTE)
               WHEN > 5
                   MOVE "Incorrect Option. Please choose again."
                   TO AUX-MSG
                   PERFORM SHOW-INFO-MSG
           END-EVALUATE.

       *>  Eleccion Dia:
           DISPLAY "Choose Day of reservation:"        LINE 13 COL 5
                                                       WITH REVERSED.
           MOVE 6 TO AUX-LINE-POS.
           DISPLAY "Day / Month / Year :  /  /    "    LINE 14 COL 5.

       REPEAT-DAY.
           ACCEPT AUX-99 LINE 14  COL 25 PROMPT.
           IF AUX-99 > 30 OR AUX-99 = 0
                   MOVE "Incorrect Option. Please choose again."
                   TO AUX-MSG
                   PERFORM SHOW-INFO-MSG
                   GO TO REPEAT-DAY
                   .
           MOVE AUX-99 TO FDATE-DD(FLIGHT-ROUTE).

       REPEAT-MONTH.
           ACCEPT AUX-99 LINE 14  COL 28 PROMPT.
           IF AUX-99 > 12 OR AUX-99 = 0
                   MOVE "Incorrect Option. Please choose again."
                   TO AUX-MSG
                   PERFORM SHOW-INFO-MSG
                   GO TO REPEAT-MONTH
                   .
           MOVE AUX-99 TO FDATE-MM(FLIGHT-ROUTE).

       REPEAT-YEAR.
           ACCEPT AUX-9999 LINE 14 COL 31 PROMPT.
           IF AUX-9999 < 2024 OR AUX-9999 = 0
                   MOVE "Incorrect Option. Please choose again."
                   TO AUX-MSG
                   PERFORM SHOW-INFO-MSG
                   GO TO REPEAT-YEAR
                   .
           MOVE AUX-9999 TO FDATE-YYYY(FLIGHT-ROUTE).


           DISPLAY "Condition : " LINE 16 COL 5 WITH REVERSED.
           DISPLAY "[1] Economy " LINE 17 COL 8.
           DISPLAY "[2] First Class" LINE 18 COL 8.
           DISPLAY "Option:" LINE 19 COL 5.
       REPEAT-CONDIT.
           ACCEPT AUX-9 LINE 19  COL 15 PROMPT.
           IF AUX-9 = 0 OR AUX-9 > 2
                   MOVE "Incorrect Option. Please choose again."
                   TO AUX-MSG
                   PERFORM SHOW-INFO-MSG
                   GO TO REPEAT-CONDIT
                   .
           MOVE AUX-9 TO FCONDITION(FLIGHT-ROUTE).

           DISPLAY "Prefered Meal : " LINE 6 COL 45 WITH REVERSED.
           DISPLAY "[1] Regular " LINE 7 COL 48.
           DISPLAY "[2] Vegetarian" LINE 8 COL 48.
           DISPLAY "[3] Vegetarian" LINE 9 COL 48.
           DISPLAY "Option:" LINE 11 COL 45.
       REPEAT-MEAL.
           ACCEPT AUX-9 LINE 11  COL 55 PROMPT.
           IF AUX-9 = 0 OR AUX-9 > 3
                   MOVE "Incorrect Option. Please choose again."
                   TO AUX-MSG
                   PERFORM SHOW-INFO-MSG
                   GO TO REPEAT-MEAL
                   .
           MOVE AUX-9 TO FMEAL(FLIGHT-ROUTE).

           DISPLAY "Carry extra luggage? Y/N: " LINE 16 COL 45
                                                   WITH REVERSED.

       REPEAT-LUGGAGE.
           ACCEPT AUX-X LINE 16  COL 70 PROMPT.
           IF AUX-X = "Y" OR AUX-X = "y"
                   MOVE 1 TO FEXTRALUGG(FLIGHT-ROUTE)
                   .

       FLIGHT-OPTIONS-END.
           EXIT.


       PRINT-TICKET.
           DISPLAY "Your complete Name: " LINE 6 COL 5.
           ACCEPT AUX-MSG LINE 6 COL 26.
           DISPLAY "          Passport: " LINE 8 COL 5.
           ACCEPT AUX-MSG LINE 8 COL 26.

           STRING  "OUTBOUND FLIGHT:  " DELIMITED BY SIZE
                   PAIS(1)
                   " Date: " DELIMITED BY SIZE
                   FDATE-DD(1) "/" FDATE-MM(1) "/" FDATE-YYYY(1)
                   " Type of Seat:" DELIMITED BY SIZE
                   CONDIT(1)
                   " MEAL: " DELIMITED BY SIZE
                   MEAL(1)
           INTO AUX-MSG.
           DISPLAY AUX-MSG LINE 11 COL 5.
           STRING  "  RETURN FLIGHT:  " DELIMITED BY SIZE
                   PAIS(2)
                   " Date: " DELIMITED BY SIZE
                   FDATE-DD(2) "/" FDATE-MM(2) "/" FDATE-YYYY(2)
                   " Type of Seat:" DELIMITED BY SIZE
                   CONDIT(2)
                   " MEAL: " DELIMITED BY SIZE
                   MEAL(2)
           INTO AUX-MSG.
           DISPLAY AUX-MSG LINE 12 COL 5.

           PERFORM MENU-DISPLAYCONTINUE.

       PRINT-TICKET-END.
           EXIT.





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
           DISPLAY "=====  Turkish Airlines v1.5 ======="
                   LINE 2 COL 5 REVERSED.

       END PROGRAM 05-AirlinesFlights.
