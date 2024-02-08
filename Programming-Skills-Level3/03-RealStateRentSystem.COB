      ******************************************************************
      * Author: gottdammer
      * Date:
      * Purpose:
      *>   * 03- Real State Rent System :
      *>      Simple calculador de costos
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 03-RealStateRentSystem.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  MENU-OPTION PIC X.


       01  HOMES.
           05 HOME-R OCCURS 5 TIMES.
           10 H-SIZE        PIC 999.
           10 H-BEDROOMS    PIC 9.
           10 H-BATHROOMS   PIC 9.
           10 H-LOCATION    PIC X(12).


       01  COSTS-BY-BATHROOM       PIC 99 VALUE 30.
       01  COSTS-BY-BEDROOM        PIC 99 VALUE 40.
       01  COSTS-BY-SQRMTS         PIC 99 VALUE 90.

       01  TOTAL-COST-RENTAL       PIC $$$$$9.



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

           DISPLAY "Calculate Rental Price"                LINE 5 COL 5.


           MOVE 7 TO AUX-LINE-POS.

           PERFORM VARYING AUX-I FROM 1 BY 1 UNTIL AUX-I >5
               COMPUTE TOTAL-COST-RENTAL =
                               H-BEDROOMS(AUX-I) * COSTS-BY-BEDROOM +
                               H-BATHROOMS(AUX-I) * COSTS-BY-BATHROOM +
                               H-SIZE(AUX-I) * COSTS-BY-SQRMTS
               END-COMPUTE
               STRING  "Rent cost house n: "
                       AUX-I
                       " "
                       TOTAL-COST-RENTAL
               INTO AUX-MSG
               DISPLAY AUX-MSG LINE AUX-LINE-POS COL 10
               ADD 1 TO AUX-LINE-POS

           END-PERFORM.
           PERFORM MENU-DISPLAYCONTINUE.


       MENU-END.

           EXIT.


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
           DISPLAY "=====  Rental Houses V 3.3 ======="
                   LINE 2 COL 40 REVERSED.

       CARGA-PARAMETROS.
           MOVE "20032Lalala"  TO HOME-R(1).
           MOVE "15022Esaaa"   TO HOME-R(2).
           MOVE "10022Mmmmm"   TO HOME-R(3).
           MOVE "20012Aaaaa"   TO HOME-R(4).
           MOVE "08011Paaaa"   TO HOME-R(5).

           EXIT.

       END PROGRAM 03-RealStateRentSystem.
