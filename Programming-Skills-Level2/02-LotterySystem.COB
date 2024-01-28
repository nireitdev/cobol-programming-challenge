      ******************************************************************
      * Author: gottdammer
      * Date:
      * Purpose:
      *>   * 02-LotterySystem
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 02-LotterySystem.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  MENU-OPTION PIC 9.

       01  TICKETS.
           05  TICKET-R OCCURS 20 TIMES INDEXED BY IDXTICKET.
               10 TICKET PIC X(5).
       01  TICKET-BUY-1 PIC 99.
       01  TICKET-BUY-2 PIC 99.

       01  AUX-LINE-POS    PIC 99.

       01  AUX-X           PIC X.
       01  AUX-9           PIC 9.
       01  AUX-I           PIC 9.
       01  AUX-J           PIC 9.
       01  AUX-99          PIC 99.
       01  AUX-999         PIC 999.
       01  AUX-9999        PIC 9999.
       01  AUX-MSG         PIC X(60).



       PROCEDURE DIVISION.
       MAIN.
           PERFORM STARTUP.
           PERFORM MENU-START .
           STOP RUN.

       STARTUP.
           STRING  "5678B9876C2345D6789E3456F8765G4321H7890J5432K2109L"
                   "8765M1357N2468P6543Q7891R3579S9821T4682U5763V1234A"
           INTO TICKETS.

       MENU-START.
           PERFORM CLEAR-SCREEN.
           PERFORM DISPLAY-TICKETS THRU DISPLAY-TICKETS-END.
           PERFORM PAYTICKET THRU PAYTICKET-END.
           PERFORM PLAYLOTTERY.
           PERFORM MENU-DISPLAYCONTINUE.

       MENU-END.
           EXIT.


       DISPLAY-TICKETS.
           DISPLAY "Available Tickets:" LINE 4 COL 5.
           MOVE 6 to AUX-LINE-POS.
           PERFORM VARYING AUX-99 FROM 1 BY 1 UNTIL AUX-99>20
               DISPLAY "[  ]" LINE AUX-LINE-POS COL 5
                   AUX-99 LINE AUX-LINE-POS COL 6
                   TICKET(AUX-99) LINE AUX-LINE-POS COL 10
               ADD 1 TO AUX-LINE-POS
           END-PERFORM.

       REPEAT-BUY-1.
           DISPLAY "Select ticket to buy:"             LINE 6 COL 45.
           ACCEPT TICKET-BUY-1                         LINE 6 COL 75.
           IF TICKET-BUY-1 = 0 OR TICKET-BUY-1 >20
                   MOVE "INVALID TICKET NUMBER. CHOOSE AGAIN."
                   TO AUX-MSG
                   PERFORM SHOW-INFO-MSG
                   GO TO REPEAT-BUY-1.

       REPEAT-BUY-2.
           DISPLAY "Select ANOTHER ticket to buy: "    LINE 7 COL 45
                   "(00 to cancel)"                   LINE 8 COL 45.
           ACCEPT TICKET-BUY-2                         LINE 7 COL 75.
           IF TICKET-BUY-2 >20
               MOVE "INVALID TICKET NUMBER. CHOOSE AGAIN."
               TO AUX-MSG
               PERFORM SHOW-INFO-MSG
               GO TO REPEAT-BUY-2.

           IF TICKET-BUY-2 = TICKET-BUY-1
           MOVE "IT IS NOT POSSIBLE TO SELECT THE SAME TICKET AGAIN"
           TO AUX-MSG
           PERFORM SHOW-INFO-MSG
           GO TO REPEAT-BUY-2.

       DISPLAY-TICKETS-END.
           EXIT.

       PAYTICKET.
           MOVE 1 TO AUX-9.
           IF NOT TICKET-BUY-2 = 0
               MOVE 2 TO AUX-9.
           STRING "Total cost of " DELIMITED BY SIZE
                   AUX-9
                   " ticket/s: $" DELIMITED BY SIZE
                   AUX-9  INTO AUX-MSG.
           DISPLAY AUX-MSG LINE 10 COL 45.

       REPEAT-PAY.
           DISPLAY "Chose method of payment: "  LINE 12 COL 45
                   " [1] Cash bills of U$D 1" LINE 13 COL 45
                   " [2] Cash bill of U$D 5" LINE 14 COL 45
                   " [3] Credit Card"           LINE 15 COL 45
                   "You option: "               LINE 17 COL 45.
           ACCEPT MENU-OPTION                   LINE 17 COL 57.
           IF MENU-OPTION = 0 OR MENU-OPTION > 3
               MOVE "INVALID OPTION. Retry."
               TO AUX-MSG
               PERFORM SHOW-INFO-MSG
               GO TO REPEAT-PAY.

           IF MENU-OPTION = 2
               MOVE 4 TO AUX-9
               IF TICKET-BUY-2 > 0
                   MOVE 3 TO AUX-9
               END-IF
               DISPLAY "Return of U$S " LINE 18 COL 45
                       AUX-9   LINE 18 COL 61
           END-IF

           DISPLAY "THANK YOU FOR YOUR PURCHASE."      LINE 20 COL 45
                                                               REVERSED.
           DISPLAY "      GOOD LUCK!!           "      LINE 21 COL 45
                                                               REVERSED.

       PAYTICKET-END.
           EXIT.


       PLAYLOTTERY.
           PERFORM CLEAR-SCREEN.
           DISPLAY "LET's PLAY THE LOTTERY!" LINE 10 COL 45 REVERSED.
           DISPLAY "Press [ENTER] key to draw a ticket number. "
                                               LINE 12 COL 35.
           ACCEPT OMITTED.

           COMPUTE AUX-9999 = FUNCTION RANDOM() * 20 + 1.


           STRING  "              " DELIMITED by SIZE
                   "The winner ticket is: " DELIMITED BY SIZE
                   TICKET(AUX-9999)
           INTO AUX-MSG.
           DISPLAY AUX-MSG   LINE 15 COL 25 REVERSED.

           MOVE SPACES TO AUX-MSG.
           IF AUX-99 = TICKET-BUY-1 OR AUX-99 = TICKET-BUY-2
               STRING  "              "        DELIMITED by SIZE
                   "YOU WIN THE LOTTERY!!!: "  DELIMITED BY SIZE
               INTO AUX-MSG
               DISPLAY AUX-MSG             LINE 18 COL 25 REVERSED
           END-IF

           PERFORM MENU-DISPLAYCONTINUE.

      *>   *************************************************************
      *>   Utilidades
      *>   *************************************************************

      *>   Simple show info msg en el area de mensajeria (line 20 col 35
       SHOW-INFO-MSG.
           DISPLAY AUX-MSG LINE 27 COL 35.
           PERFORM MENU-DISPLAYCONTINUE.
           PERFORM CLEAR-INFO-SCREEN.


      *>   Simple <pause> para leer
       MENU-DISPLAYCONTINUE.
           DISPLAY "Press [ENTER] to continue.."
                           LINE 29 COL 5.
           ACCEPT OMITTED.

       CLEAR-INFO-SCREEN.
           DISPLAY " " LINE 27 COL 1 ERASE EOS.

       CLEAR-SCREEN.
           DISPLAY " " LINE 1 COL 1 ERASE EOS.
           DISPLAY "=====  Lottery System v2.2 ======="
                   LINE 2 COL 40 REVERSED.

       END PROGRAM 02-LotterySystem.
