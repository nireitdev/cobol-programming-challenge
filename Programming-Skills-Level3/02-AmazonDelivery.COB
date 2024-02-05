      ******************************************************************
      * Author: gottdammer
      * Date:
      * Purpose:
      *>   * 2 - Amazon Delivery:
      *>      Randomize el viaje desde un estado a otro y
      *>      tambien randomize la cantidad de paquetes.
      *>      Luego estimo el costo total del viaje.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 02-AmazonDelivery.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  MENU-OPTION PIC X.

      *>   Guarda distancias entre estados.
       01  DISTANC.
           05 DIST-PAIR OCCURS 19 TIMES.
           10 STATE1        PIC X(12).
           10 STATE2        PIC X(12).
           10 TOTALDIST     PIC 9(4).

       01  CTE-TOTAL-STATEPAIRS        PIC 99 VALUE 19.
       01  CTE-PRICE-BY-KM             PIC 99 VALUE 50.

      *>   A partir de cuando se aplica el nuevo precio:
       01  CTE-COUNT-APPLY-PRICEKM     PIC 999 VALUE 200.
       01  CTE-PRICE-BY-KM-GRT         PIC 99 VALUE 60.

      *>   Limites en cantidad
       01  CTE-COUNT-MIN-PCKG          PIC 999 VALUE 100.
       01  CTE-COUNT-MAX-PCKG          PIC 999 VALUE 500.

       01  IDX-STATES-PAIR             PIC 999.
       01  TOTAL-PCKGS                 PIC 999.
       01  TOTAL-COST-DELIVERY         PIC 99999.
       01  TOTAL-COST-DELIVERY-Z       PIC $Z(3)9.

       01  RANDOMNUMBER.
           10 FILLER PIC 9(6).
           10 RNDM  PIC 99.


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


       PROCEDURE DIVISION.
       MAIN.
           PERFORM CARGA-PARAMETROS.
           PERFORM MENU-START THRU MENU-END.
           STOP RUN.

       MENU-START.
           PERFORM CLEAR-SCREEN.

           DISPLAY "1 - New Delivery"                      LINE 5 COL 5
                   "0 - Quit"                              LINE 7 COL 5
                   "Your option:"                          LINE 9 COL 5
                   .

       REPEAT-MENU.
           ACCEPT MENU-OPTION  LINE 9 COL 18 PROMPT.

           IF MENU-OPTION = 0
               STOP RUN.

           INITIALIZE AUX-MSG.
      *>   Randomizo el IDX de la tabla de distancias:
           ACCEPT RANDOMNUMBER FROM TIME.
           DIVIDE RNDM BY 19 GIVING AUX-99 REMAINDER IDX-STATES-PAIR.
           ADD 1 to IDX-STATES-PAIR.

      *>   Randomizo el total de paquetes:
           ACCEPT RANDOMNUMBER FROM TIME.
           COMPUTE TOTAL-PCKGS = RNDM / 100 * 600.
      *>      DIVIDE RNDM BY 500 GIVING AUX-99 REMAINDER TOTAL-PCKGS.
           ADD 1 to TOTAL-PCKGS.


           STRING "Choosed Delivery FROM "
                   STATE1(IDX-STATES-PAIR)
                   " TO " DELIMITED BY SIZE
                   STATE2(IDX-STATES-PAIR)
           INTO AUX-MSG.
           DISPLAY AUX-MSG LINE 11 COL 5.

           INITIALIZE AUX-MSG.
           STRING "Total distance: "
                   TOTALDIST(IDX-STATES-PAIR)
                   " km."
           INTO AUX-MSG.
           DISPLAY AUX-MSG LINE 12 COL 5.

           INITIALIZE AUX-MSG.
           STRING "Total packages: "
                   TOTAL-PCKGS
           INTO AUX-MSG.
           DISPLAY AUX-MSG LINE 13 COL 5.

           IF TOTAL-PCKGS < CTE-COUNT-MIN-PCKG
               INITIALIZE AUX-MSG
               STRING "Amount of total packages is LOWER than: "
                       CTE-COUNT-MIN-PCKG
                       ". Delivery cancelled."
               INTO AUX-MSG
               PERFORM SHOW-INFO-MSG-YES_NO
               GO TO MENU-END
           END-IF.

           IF TOTAL-PCKGS > CTE-COUNT-MAX-PCKG
               INITIALIZE AUX-MSG
               STRING "Amount of total packages is HIGHER than: "
                       CTE-COUNT-MAX-PCKG
                       ". Delivery cancelled."
               INTO AUX-MSG
               PERFORM SHOW-INFO-MSG-YES_NO
               GO TO MENU-END
           END-IF.


           IF TOTAL-PCKGS > CTE-COUNT-APPLY-PRICEKM
              COMPUTE TOTAL-COST-DELIVERY =
                   CTE-PRICE-BY-KM-GRT * TOTALDIST(IDX-STATES-PAIR)
           ELSE
               COMPUTE TOTAL-COST-DELIVERY =
                   CTE-PRICE-BY-KM * TOTALDIST(IDX-STATES-PAIR)

           END-IF.

           INITIALIZE AUX-MSG.
           MOVE TOTAL-COST-DELIVERY TO TOTAL-COST-DELIVERY-Z.
           STRING "Total Cost: "
                   TOTAL-COST-DELIVERY-Z

           INTO AUX-MSG.
           DISPLAY AUX-MSG LINE 14 COL 5.



       MENU-END.

           GO TO REPEAT-MENU.


      *>   *************************************************************
      *>   Utilidades
      *>   *************************************************************

      *>   Simple show info msg en el area de mensajeria
       SHOW-INFO-MSG-YES_NO.
           DISPLAY AUX-MSG LINE 23 COL 35.
      *>      DISPLAY "Y/N:" LINE 24 COL 50.
      *>      ACCEPT RETURN-MSG-YES_NO LINE 24 COL 55 PROMPT.
           ACCEPT OMITTED.

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
           DISPLAY "=====  Amazon Delivery V 3.2 ======="
                   LINE 2 COL 40 REVERSED.

       CARGA-PARAMETROS.
           MOVE "California  Texas       2379" TO DIST-PAIR(1).
           MOVE "New_York    Pennsylvania0396" TO DIST-PAIR(2).
           MOVE "Ohio        Indiana     0358" TO DIST-PAIR(3).
           MOVE "Illinois    Missouri    0418" TO DIST-PAIR(4).
           MOVE "Michigan    Wisconsin   0355" TO DIST-PAIR(5).
           MOVE "Florida     Georgia     0294" TO DIST-PAIR(6).
           MOVE "Texas       New_Mexico  0567" TO DIST-PAIR(7).
           MOVE "PennsylvaniaNew_Jersey  0180" TO DIST-PAIR(8).
           MOVE "California  Oregon      0634" TO DIST-PAIR(9).
           MOVE "New_York    Connecticut 0133" TO DIST-PAIR(10).
           MOVE "Ohio        Kentucky    0297" TO DIST-PAIR(11).
           MOVE "Wisconsin   Minnesota   0514" TO DIST-PAIR(12).
           MOVE "PennsylvaniaDelaware    0147" TO DIST-PAIR(13).
           MOVE "Texas       Oklahoma    0468" TO DIST-PAIR(14).
           MOVE "California  Nevada      0385" TO DIST-PAIR(15).
           MOVE "Georgia     SouthCarolin0219" TO DIST-PAIR(16).
           MOVE "New_Jersey  Delaware    0162" TO DIST-PAIR(17).
           MOVE "New_York    Massachusett0222" TO DIST-PAIR(18).
           MOVE "Minnesota   Iowa        0484" TO DIST-PAIR(19).

           EXIT.

       END PROGRAM 02-AmazonDelivery.
