      ******************************************************************
      * Author: gottdammer
      * Date:
      * Purpose:
      *>   * 4 - Hotel Reservation
      *>   IMPORTANTE: No hago validacion de reservas ya hechas.
      *>               El ejercicio tampoco lo pide. :p
      *>   El login te la debo..CTRL+C CTRL+V de los ejerc. anteriores.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 04-HotelReservation.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  MENU-OPTION PIC 9.

      *>   Paises y Ciudades
       01  PAISES.
           05  PAIS-R OCCURS 9 TIMES INDEXED BY IDXPAIS.
               10 PAIS PIC X(10).
               10 CIUDAD-R OCCURS 3 TIMES INDEXED BY IDXCIUD.
                   15  CIUDAD PIC X(10).


       01  COSTO-TOTAL     PIC 9(6).
       01  COSTO-TOTAL-Z   PIC Z(5)9.

       01  CHOICE-CTRY PIC 9 VALUE ZEROES.
       01  CHOICE-CITY PIC 9 VALUE ZEROES.

       01  CHOICE-DATE VALUE "20240101".
           05 CHOICE-DATE-YYYY     PIC 9999.
           05 CHOICE-DATE-MM       PIC 99.
           05 CHOICE-DATE-DD       PIC 99.

       01  CHOICE-NIGTHS           PIC 99.

       01  CHOICE-ROOMS-SINGLE     PIC 9.
       01  CHOICE-ROOMS-DOUBLE     PIC 9.
       01  CHOICE-ROOMS-VIP        PIC 9.
       01  CHOICE-ROOMS-LUXURY     PIC 9.


       01  CTE-PRICE-SINGLE    PIC 999 VALUE 100.
       01  CTE-PRICE-DOUBLE    PIC 999 VALUE 200.
       01  CTE-PRICE-VIP       PIC 999 VALUE 450.
       01  CTE-PRICE-LUJURY    PIC 999 VALUE 550.

       01  AUX-LINE-POS    PIC 99.

       01  AUX-X           PIC X.
       01  AUX-9           PIC 9.
       01  AUX-99          PIC 99.
       01  AUX-999         PIC 999.
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
           MOVE "Spain     Madrid    Barcelona Valencia"   TO PAIS-R(1).
           MOVE "France    Paris     Marseille"            TO PAIS-R(2).
           MOVE "Portugal  Madeira   Lisbon    Porto"      TO PAIS-R(3).
           MOVE "Italy     Rome      Milan"                TO PAIS-R(4).
           MOVE "Germany   Munich    Berlin"               TO PAIS-R(5).

           EXIT.


      *>   *************************************************************
      *>   Menu carga de entradas y salidas.
      *>   Cada entrada/salida tiene una descripcion y un monto
      *>   La categoria [1] es Ingreso de dinero y las demas Salidas.
      *>   *************************************************************
       MENU-START.
           PERFORM CLEAR-SCREEN.

      *>   Eleccion Ciudad:
           DISPLAY "Choose Country:"     LINE 4 COL 5 WITH REVERSED.
           MOVE 6 TO AUX-LINE-POS.
           PERFORM VARYING AUX-99 FROM 1 BY 1 UNTIL AUX-99>5
               DISPLAY AUX-99                  LINE AUX-LINE-POS COL 5
               DISPLAY "["                     LINE AUX-LINE-POS COL 5
               DISPLAY "]"                     LINE AUX-LINE-POS COL 7
               DISPLAY PAIS(AUX-99)            LINE AUX-LINE-POS COL 10
               ADD 1 TO AUX-LINE-POS
           END-PERFORM.
           DISPLAY "[0] Quit system."          LINE 12  COL 5.
           DISPLAY "Your choice: "     LINE 14  COL 5 .
       REPEAT-CNTRY.
           ACCEPT CHOICE-CTRY  LINE 14  COL 18 PROMPT WITH UNDERLINE.
           EVALUATE CHOICE-CTRY
               WHEN 0
                   STOP RUN
               WHEN > 5
                   MOVE "Incorrect Option. Please choose again."
                   TO AUX-MSG
                   PERFORM SHOW-INFO-MSG
           END-EVALUATE.


      *>   Eleccion ciudad:
           DISPLAY "Choose City:"          LINE 4 COL 45 WITH REVERSED.
           MOVE 6 TO AUX-LINE-POS.
           PERFORM VARYING AUX-99 FROM 1 BY 1 UNTIL AUX-99>3
               IF NOT CIUDAD(CHOICE-CTRY,AUX-99) = SPACES
                   DISPLAY AUX-99              LINE AUX-LINE-POS COL 45
                   DISPLAY "["                 LINE AUX-LINE-POS COL 45
                   DISPLAY "]"                 LINE AUX-LINE-POS COL 47
                   DISPLAY CIUDAD(CHOICE-CTRY,AUX-99)
                                               LINE AUX-LINE-POS COL 50
                   ADD 1 TO AUX-LINE-POS
           END-PERFORM.

           DISPLAY "Your choice: "             LINE 10  COL 45 .
       REPEAT-CITY.
           ACCEPT CHOICE-CITY                  LINE 10  COL 58 PROMPT
                                               WITH UNDERLINE.
           IF CHOICE-CITY > 3 OR CHOICE-CITY = 0
                   MOVE "Incorrect Option. Please choose again."
                   TO AUX-MSG
                   PERFORM SHOW-INFO-MSG
                   GO TO REPEAT-CITY
                   .


       *>  Eleccion Dia:
           DISPLAY "Choose Day of reservation:"        LINE 4 COL 80
                                                       WITH REVERSED.
           MOVE 6 TO AUX-LINE-POS.

           DISPLAY "Day / Month / Year :  /  /    "    LINE 5 COL 80.

       REPEAT-DAY.
           ACCEPT CHOICE-DATE-DD LINE 5  COL 100 PROMPT WITH UNDERLINE.
           IF CHOICE-DATE-DD > 30 OR CHOICE-CITY = 0
                   MOVE "Incorrect Option. Please choose again."
                   TO AUX-MSG
                   PERFORM SHOW-INFO-MSG
                   GO TO REPEAT-DAY
                   .

       REPEAT-MONTH.
           ACCEPT CHOICE-DATE-MM LINE 5  COL 103 PROMPT WITH UNDERLINE.
           IF CHOICE-DATE-MM > 12 OR CHOICE-DATE-MM = 0
                   MOVE "Incorrect Option. Please choose again."
                   TO AUX-MSG
                   PERFORM SHOW-INFO-MSG
                   GO TO REPEAT-MONTH
                   .

       REPEAT-YEAR.
           ACCEPT CHOICE-DATE-YYYY LINE 5 COL 106 PROMPT WITH UNDERLINE.
           IF CHOICE-DATE-YYYY < 2024 OR CHOICE-DATE-YYYY = 0
                   MOVE "Incorrect Option. Please choose again."
                   TO AUX-MSG
                   PERFORM SHOW-INFO-MSG
                   GO TO REPEAT-YEAR
                   .


           DISPLAY "How many nights: "     LINE 8 COL 80 WITH REVERSED.
       REPEAT-NIGHTS.
           ACCEPT CHOICE-NIGTHS LINE 8  COL 100 PROMPT WITH UNDERLINE.
           IF CHOICE-NIGTHS > 30 OR CHOICE-NIGTHS = 0
                   MOVE "Incorrect Option. Please choose again."
                   TO AUX-MSG
                   PERFORM SHOW-INFO-MSG
                   GO TO REPEAT-NIGHTS
                   .

           DISPLAY "How many rooms: " LINE 10 COL 80 WITH REVERSED.

           DISPLAY "SINGLE rooms: "    LINE 12 COL 86.
           DISPLAY "DOUBLE rooms: "    LINE 13 COL 86.
           DISPLAY "VIP  rooms: "      LINE 14 COL 86.
           DISPLAY "LUXURY rooms: "    LINE 15 COL 86.
           DISPLAY "You must choose at least one room."LINE 17 COL 80.
       REPEAT-ROOMS.
           ACCEPT CHOICE-ROOMS-SINGLE  LINE 12  COL 100 PROMPT
                                                        WITH UNDERLINE.
           ACCEPT CHOICE-ROOMS-DOUBLE  LINE 13  COL 100 PROMPT
                                                        WITH UNDERLINE.
           ACCEPT CHOICE-ROOMS-VIP     LINE 14  COL 100 PROMPT
                                                        WITH UNDERLINE.
           ACCEPT CHOICE-ROOMS-LUXURY  LINE 15  COL 100 PROMPT
                                                        WITH UNDERLINE.
           IF CHOICE-ROOMS-SINGLE = 0  AND CHOICE-ROOMS-DOUBLE = 0
              AND CHOICE-ROOMS-VIP = 0 AND CHOICE-ROOMS-LUXURY = 0
                   MOVE "You must choose some rooms to reservation."
                   TO AUX-MSG
                   PERFORM SHOW-INFO-MSG
                   GO TO REPEAT-ROOMS
                   .


      *>   CALCULO TOTAL DEL COSTO
           COMPUTE COSTO-TOTAL = CTE-PRICE-SINGLE * CHOICE-ROOMS-SINGLE+
                               CTE-PRICE-DOUBLE * CHOICE-ROOMS-DOUBLE +
                               CTE-PRICE-VIP * CHOICE-ROOMS-VIP +
                               CTE-PRICE-LUJURY*CHOICE-ROOMS-LUXURY.
           COMPUTE COSTO-TOTAL = COSTO-TOTAL * CHOICE-NIGTHS.
           MOVE COSTO-TOTAL TO COSTO-TOTAL-Z.

           STRING "Total Cost: $ " DELIMITED BY SIZE
                   COSTO-TOTAL-Z
           INTO AUX-MSG.
           DISPLAY AUX-MSG LINE 22 COL 35.
           DISPLAY "Press [Y] to aggree reservation: " LINE 24 COL 35.
           ACCEPT AUX-X LINE 24 COL 70 .
           IF AUX-X = "Y" or AUX-X = "y"
               DISPLAY "RESERVATION DONE. Thanks." LINE 26 COL 35
           ELSE
               DISPLAY "RESERVATION Cancelled. Returning to Main Menu."
                               LINE 26 COL 35
           END-IF.

           PERFORM MENU-DISPLAYCONTINUE.




       MENU-END.
           GO TO MENU-START.
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
           DISPLAY "=====  Hotel Reservation v1.4 ======="
                   LINE 2 COL 5 REVERSED.

       END PROGRAM 04-HotelReservation.
