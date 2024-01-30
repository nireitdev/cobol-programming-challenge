      ******************************************************************
      * Author: gottdammer
      * Date:
      * Purpose:
      *>   * 4 - Ticket system
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 04-TicketSystem.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  MENU-OPTION PIC 9.


      *>   Registro tickets vendidos.
      *>       Tipo de seat.[1-3716]VIPBox [3716-11146] Vip Seat
      *>       Guarda el numero del comprador. Deberia ser un ID  :p
      *>       Fecha de compra YYYYMMDDHHMM
       01  PURCH-SEATS.
           05  PURCH-SEAT-R  OCCURS 74310 TIMES INDEXED BY IDXSEAT.
               10 ID-TYPESEAT  PIC 9.
               10 MEMBERNAME   PIC 9(3).
               10 PURCHDATE    PIC X(12).

      *>   Registro de TIPOS de lugares/tickets
       01  TYPE-SEATS.
           05 TYPE-SEATS-R OCCURS 3 TIMES INDEXED BY IDXTYPESEAT.
              10 TYPESEATNAME PIC X(10).
              10 COSTSEAT     PIC 9(4).
              10 MINCOUNTSEAT PIC 9(5).
              10 MAXCOUNTSEAT PIC 9(5).

      *>   Registro de tipos de miembros y caracteristicas.
      *>       Nombre del tipo
      *>       Cant maxima de ticket de asientos que puede comprar
       01  TYPE-MEMBERS VALUE "MEMBER    10NON-MEMBER03".
           05 TYPE-MEMBERS-R OCCURS 2 TIMES INDEXED BY IDXTYPEMEMB.
               10 TYPEMEMBNAME     PIC X(10).
               10 MAXSEATPURCHASE  PIC 99.

      *>   Registro de usuarios. No lo utilizo. Tal vez para futuros
      *>   ejercicios.
       01  MEMBERS.
           05 MEMBERS-R OCCURS 10000 TIMES INDEXED BY IDXUSER.
               10 USERNAME     PIC X(10).
               10 PASSWORD     PIC X(10).
               10 IDXUSERTYPE  PIC 9.
               10 FIRSTNAME    PIC X(20).
               10 LASTNAME     PIC X(20).


       01  CTE-MAXTOTALSEATS PIC 99999 VALUE 74310.

       01  AUX-IDMEMBER    PIC 999.
       01  AUX-TYPEMEMBER  PIC 9.
       01  AUX-SEAT        PIC 9(5).
       01  AUX-SEAT-Z      PIC Z(4)9.
       01  AUX-TOTALSEATS  PIC 99.
       01  AUX-TOTALCOSTS  PIC 99999.

       01  AUX-LINE-POS    PIC 99.
       01  AUX-COL-POS    PIC 99 VALUE 5.
       01  AUX-VALUE9      PIC 9.
       01  AUX-VALUE99     PIC 99.
       01  AUX-VALUE999    PIC 999.
       01  AUX-MSG         PIC X(100).
       01  RETURN-MSG-YES_NO PIC X.




       PROCEDURE DIVISION.
       MAIN.
           PERFORM CARGA-PARAMETROS.
           PERFORM LOGIN.
           PERFORM MENU-START THRU MENU-START-END.
           STOP RUN.


       CARGA-PARAMETROS.
           MOVE "VIP Box   10000000103716" TO TYPE-SEATS-R(1).
           MOVE "VIP Seat  05000371711146" TO TYPE-SEATS-R(2).
           MOVE "General   00901114759448" TO TYPE-SEATS-R(3).


           PERFORM VARYING AUX-VALUE9 FROM 1 BY 1 UNTIL AUX-VALUE9 >3
               PERFORM VARYING IDXSEAT FROM MINCOUNTSEAT(AUX-VALUE9)BY 1
                           UNTIL IDXSEAT>MAXCOUNTSEAT(AUX-VALUE9)
                   MOVE AUX-VALUE9 TO ID-TYPESEAT(IDXSEAT)
               END-PERFORM
           END-PERFORM.
           EXIT.


       MENU-START.
           PERFORM CLEAR-SCREEN.
           DISPLAY "WELCOME user number: "             LINE 4 COL 5
                   AUX-IDMEMBER                        LINE 4 COL 32

                   "Member type: "                     LINE 5 COL 5
                   TYPEMEMBNAME(AUX-TYPEMEMBER)        LINE 5 COL 32

                   "You can buy A maximum of :"        LINE 6 COL 5
                   MAXSEATPURCHASE(AUX-TYPEMEMBER)     LINE 6 COL 32
                   "seats"                             LINE 6 COL 35


                   "=== Purchase Ticket ==="           LINE 9 COL 5

                   "   Distribution:"                  LINE 10 COL 5
                   .

           MOVE 12 TO AUX-LINE-POS.

           PERFORM VARYING IDXTYPESEAT FROM 1 BY 1 UNTIL IDXTYPESEAT>3
               STRING TYPESEATNAME(IDXTYPESEAT)
                        "from "
                        MINCOUNTSEAT(IDXTYPESEAT)
                        " up to "
                        MAXCOUNTSEAT(IDXTYPESEAT)
                        " price L$ "
                        COSTSEAT(IDXTYPESEAT)
               INTO AUX-MSG
               DISPLAY AUX-MSG     LINE AUX-LINE-POS COL 10
               ADD 1 TO AUX-LINE-POS

           END-PERFORM.

       REPEAT-SEATS.
           DISPLAY "Select number of seat that you wish to buy:"
                                                       LINE 16 COL 5.
           MOVE 0 to AUX-SEAT-Z.
           ACCEPT AUX-SEAT-Z  LINE 16 COL 49 PROMPT.
           MOVE AUX-SEAT-Z TO AUX-SEAT.
           IF AUX-SEAT=0 OR MEMBERNAME(AUX-SEAT)>0 THEN
               MOVE "INVALID NUMBER OF SEAT. ALREADY SOLD." TO AUX-MSG
               PERFORM SHOW-INFO-MSG
               PERFORM CLEAR-INFO-SCREEN
               GO TO REPEAT-SEATS
           END-IF.

      *>   Se compra el asiento, guardo el id del usuario
           MOVE AUX-IDMEMBER TO MEMBERNAME(AUX-SEAT).
           MOVE "202401291313" TO PURCHDATE(AUX-SEAT).
           ADD 1 TO AUX-TOTALSEATS.


      *>   Informacion de compras
           ADD 6 TO AUX-COL-POS.
           DISPLAY "Seats: " LINE 18 COL 5
                   AUX-SEAT-Z  LINE 18 COL AUX-COL-POS.

           ADD COSTSEAT(ID-TYPESEAT(AUX-SEAT)) TO AUX-TOTALCOSTS.
           INITIALISE AUX-MSG.
           STRING  "Total seats: "
                   AUX-TOTALSEATS
                   "    Total costs: L$ "
                   AUX-TOTALCOSTS
           INTO AUX-MSG.
           DISPLAY AUX-MSG LINE 19 COL 5.

           MOVE "       PURCHASE DONE. " TO AUX-MSG.
           PERFORM SHOW-INFO-MSG.

           IF AUX-TOTALSEATS < MAXSEATPURCHASE(AUX-TYPEMEMBER) THEN
               MOVE "DO YOU WANT PURCHASE ANOTHER TICKET?" TO AUX-MSG
               PERFORM SHOW-INFO-MSG-YES_NO
               IF RETURN-MSG-YES_NO = "Y"
                   GO TO REPEAT-SEATS
               END-IF
           END-IF.

           MOVE "       PROCEED TO CHECKOUT?" TO AUX-MSG.
           PERFORM SHOW-INFO-MSG-YES_NO.
           IF NOT RETURN-MSG-YES_NO = "Y"
      *>           FIN programa
                   GO TO MENU-START-END.

       CHECKOUT.
           IF AUX-TYPEMEMBER = 1 THEN
               COMPUTE AUX-VALUE999 = 0.15 * AUX-TOTALCOSTS
               STRING  "Discounts for Members:        - L$ "AUX-VALUE999
               INTO AUX-MSG
               DISPLAY AUX-MSG      LINE 20 col 5
               SUBTRACT AUX-VALUE999 FROM AUX-TOTALCOSTS
           END-IF.

           INITIALIZE AUX-MSG.
           STRING  "TOTAL TO PAY: L$ " AUX-TOTALCOSTS
           INTO AUX-MSG.

           DISPLAY AUX-MSG      LINE 21 col 23 UNDERLINE.

           PERFORM MENU-DISPLAYCONTINUE.


       MENU-START-END.
           EXIT.



      *>   *************************************************************
      *>   Login.
      *>       Simplifico bastante el Login de usuarios.
      *>      Deberia crear un sistema de creacion y
      *>      login de nuevos usuarios. Tal vez para futuras resoluciones
      *>      lo haga.
      *>       Entrar 100 para miembros. Cualquier otra para no miembros
      *>   *************************************************************
       LOGIN.
           PERFORM CLEAR-SCREEN.
           DISPLAY "Membership Login."     LINE 4  COL 5
                   "Number of member:"     LINE 7  COL 5
                   " example: 100 member ,  101 non-member" LINE 8 COL 5
           END-DISPLAY.
      *>      MOVE 100 TO AUX-VALUE999
           ACCEPT AUX-IDMEMBER             LINE 7 COL 25 UPDATE.

      *>   Login simplificado para el ejemplo
           EVALUATE AUX-IDMEMBER
               WHEN 100
                   MOVE 1 to AUX-TYPEMEMBER
               WHEN OTHER
                   MOVE 2 to AUX-TYPEMEMBER
           END-EVALUATE.







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
                           LINE 28 COL 5.
           ACCEPT OMITTED.

       CLEAR-INFO-SCREEN.
           DISPLAY " " LINE 21 COL 1 ERASE EOS.

       CLEAR-SCREEN.
           DISPLAY " " LINE 1 COL 1 ERASE EOS.
           DISPLAY "=====  Ticket System v2.4 ======="
                   LINE 2 COL 5 REVERSED.

       END PROGRAM 04-TicketSystem.
