      ******************************************************************
      * Author: gottdammer
      * Date:
      * Purpose:
      *>   * 04-ManchesterShopping
      *>       Shopping Cart
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 04-ManchesterShopping.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  MENU-OPTION PIC X.

      *>   El codigo de barras identifica el item guardado en Stock
      *>   Cada item tiene un barcode, despcriocion y precio
       01  STOCKS.
           05 STOCK OCCURS 30  TIMES.
           10 BARCODE.
               15 ITEM-NUMBER  PIC X(3).
               15 ITEM-GENDER  PIC X(1).
                   88 ForMen       VALUE "M".
                   88 ForWoMen     VALUE "W".
                   88 ForChildren  VALUE "C".
               15 ITEM-SIZE    PIC X(3).
                   88 SizeXS       VALUE "XS".
                   88 SizeXM       VALUE "XM".
                   88 SizeXL       VALUE "XL".
                   88 Size2XS      VALUE "2XL".
                   88 Size3XS      VALUE "3XL".
               15 ITEM-SLEEVE  PIC X(1).
                   88 SleeveShort  VALUE "S".
                   88 SleeveLong   VALUE "L".
           10 DESCRIP          PIC X(25).
           10 PRICE            PIC 9(4).
           10 AMOUNTSTOCK      PIC 9(3).

       01  FILTER-GENDER   PIC X(1).
       01  FILTER-SIZE     PIC X(3).
       01  FILTER-SLEEVE   PIC X(1).

       01  CTE-MAX-STOCK       PIC 99 VALUE 27.

       01  LIST-SEARCHS.
           05  LIST-SEARCH OCCURS 30 TIMES.
               10 SRCH-IDXSTOCK    PIC 99.

      *>   Guarda los items (IDX stock) y la cantidad comprada
       01  LIST-BUYS.
           05  LIST-BUY    OCCURS 30 TIMES.
               10 BUY-IDXSTOCK     PIC 99.
               10 BUY-AMOUNT       PIC 999.

       01  BUY-COUNTER             PIC 99.
       01  TOTAL-CHECKOUT          PIC 9(5).
       01  TOTAL-ITEM              PIC 9(5).
       01  AUX-PAY-Z               PIC $Z(4)9.
       01  AUX-PAY-ZZ               PIC $Z(4)9.


       01  AUX-LINE-POS    PIC 99.
       01  AUX-COL-POS     PIC 99 VALUE 5.
       01  AUX-BOOL        PIC X.
       01  AUX-9           PIC 9.
       01  AUX-99          PIC 99.
       01  AUX-999         PIC 999.
       01  AUX-99999       PIC 99999.
       01  AUX-COUNT       PIC 99.
       01  AUX-I           PIC 999.
       01  AUX-J           PIC 999.
       01  AUX-MSG         PIC X(50).
       01  AUX-MSGXL       PIC X(150).
       01  RETURN-MSG-YES_NO PIC X.


       PROCEDURE DIVISION.
       MAIN.
           PERFORM CARGA-PARAMETROS.
           PERFORM MENU-START THRU MENU-END.
           STOP RUN.

       MENU-START.
           PERFORM CLEAR-SCREEN.

           DISPLAY "Search by Type:"                   LINE 5 COL 5
                   "[M] for Men"                       LINE 6 COL 5
                   "[W] for Woman"                     LINE 7 COL 5
                   "[C] for Children"                  LINE 8 COL 5

                   "Search by Size:"                   LINE 5 COL 25
                   "[1] XS"                            LINE 6 COL 25
                   "[2] M"                             LINE 7 COL 25
                   "[3] L"                             LINE 8 COL 25
                   "[4] XL"                            LINE 6 COL 32
                   "[5] 2XL"                           LINE 7 COL 32
                   "[6] 3XL"                           LINE 8 COL 32

                   "Search by Other:"                  LINE 5 COL 45
                   "[S] Short Sleeve"                  LINE 6 COL 45
                   "[L] Long Sleeve"                   LINE 7 COL 45

                   "[R] Reset Filters"                 LINE 5 COL 70
                   "[B] Buy Menu"                      LINE 7 COL 70
                   "[P] Checkout/Pay Menu"             LINE 8 COL 70

                   "Your Option:"                      LINE 9 COL 40
                   .

       REPEAT-MENU.
           DISPLAY "Gender:"       LINE 10 COL 5
                   FILTER-GENDER   LINE 10 COL 13
                   "Size:"         LINE 10 COL 25
                   FILTER-SIZE     LINE 10 COL 31
                   "Sleeves:"      LINE 10 COL 45
                   FILTER-SLEEVE   LINE 10 COL 54
                   .

           ACCEPT MENU-OPTION  LINE 9 COL 52 PROMPT.
           MOVE FUNCTION UPPER-CASE(MENU-OPTION) TO MENU-OPTION.

           EVALUATE MENU-OPTION
               WHEN "0"
                   STOP RUN
               WHEN "M"    MOVE "M" TO FILTER-GENDER
               WHEN "W"    MOVE "W" TO FILTER-GENDER
               WHEN "C"    MOVE "C" TO FILTER-GENDER

               WHEN "1"    MOVE "XS"  TO FILTER-SIZE
               WHEN "2"    MOVE "M"   TO FILTER-SIZE
               WHEN "3"    MOVE "L"   TO FILTER-SIZE
               WHEN "4"    MOVE "XL"  TO FILTER-SIZE
               WHEN "5"    MOVE "2XL" TO FILTER-SIZE
               WHEN "6"    MOVE "3XL" TO FILTER-SIZE

               WHEN "S"    MOVE "S" TO FILTER-SLEEVE
               WHEN "L"    MOVE "L" TO FILTER-SLEEVE


               WHEN "R"
                   MOVE SPACES TO FILTER-GENDER
                   MOVE SPACES TO FILTER-SIZE
                   MOVE SPACES TO FILTER-SLEEVE

               WHEN "P"
                   PERFORM DISPLAY-CHECKOUT

           END-EVALUATE.

      *>   Menu Compras:
           IF MENU-OPTION = "B"
               DISPLAY "Nro ITEM to buy:"  LINE 29 COL 5
                       "Qty:"  LINE 29 COL 35

               ACCEPT AUX-99   LINE 29 COL 24
               ACCEPT AUX-999  LINE 29 COL 40
               IF AUX-999 > AMOUNTSTOCK(AUX-99)
                   DISPLAY "INVALID AMOUNT! Exceeds the stock quantity"
                                                       LINE 29 COL 45
                   GO TO REPEAT-MENU
               END-IF
               ADD 1 TO BUY-COUNTER
               MOVE AUX-99 TO BUY-IDXSTOCK(BUY-COUNTER)
               MOVE AUX-999 TO BUY-AMOUNT(BUY-COUNTER)
               SUBTRACT AUX-999 FROM AMOUNTSTOCK(AUX-99)
           END-IF.


           PERFORM SEARCH-STOCK.
           PERFORM DISPLAY-SEARCH.

       MENU-END.

           GO TO REPEAT-MENU.


      *>   Filtra las busquedas acorde a los filtros seteados
       SEARCH-STOCK.
           INITIALISE LIST-SEARCHS.

           MOVE 1 TO AUX-99.
           PERFORM VARYING AUX-I FROM 1 BY 1 UNTIL AUX-I > CTE-MAX-STOCK
               MOVE "F" TO AUX-BOOL

      *>       Todos los filtros en TRUE para mostrar resultado:
               IF  (FILTER-GENDER = ITEM-GENDER(AUX-I)
                       OR  FILTER-GENDER = SPACES )
                   AND
                   (FILTER-SIZE = ITEM-SIZE(AUX-I)
                       OR  FILTER-SIZE = SPACE )
                   AND
                   (FILTER-SLEEVE = ITEM-SLEEVE(AUX-I)
                       OR  FILTER-SLEEVE = SPACE )

                       MOVE AUX-I TO SRCH-IDXSTOCK(AUX-99)
                       ADD 1 TO AUX-99
               END-IF
           END-PERFORM.

       SEARCH-STOCK-END.
           EXIT.

      *>   Muestra las busquedas
       DISPLAY-SEARCH.
           PERFORM CLEAR-INFO-SCREEN.
           MOVE 14 TO AUX-LINE-POS.
           MOVE 1 TO AUX-COL-POS.

      *>   Header:
           STRING " ITEM  G SZ  SL         DESCR              Stock"
           INTO AUX-MSGXL.
           DISPLAY AUX-MSGXL LINE 12 COL 1.
           DISPLAY AUX-MSGXL LINE 12 COL 55.
           STRING "+-----+-+--+---+--------------------------+-----"
           INTO AUX-MSGXL.
           DISPLAY AUX-MSGXL LINE 13 COL 1.
           DISPLAY AUX-MSGXL LINE 13 COL 55.


           PERFORM VARYING AUX-I FROM 1 BY 1 UNTIL AUX-I > CTE-MAX-STOCK
               INITIALISE AUX-MSG
               MOVE SRCH-IDXSTOCK(AUX-I) TO AUX-99
               IF AUX-99 > 0
                   STRING "[ " ,AUX-99, " ] "
                           ITEM-GENDER(AUX-99), " "
                           ITEM-SIZE(AUX-99), " "
                           ITEM-SLEEVE(AUX-99)  , "  "
                           DESCRIP(AUX-99)     , "  "
                           AMOUNTSTOCK(AUX-99) , " "
                   INTO AUX-MSG
                   DISPLAY AUX-MSG LINE AUX-LINE-POS COL AUX-COL-POS
                   ADD 1 TO AUX-LINE-POS
                   IF AUX-LINE-POS > 26
                       MOVE 14 TO AUX-LINE-POS
                       MOVE 55 TO AUX-COL-POS
                   END-IF

               END-IF
           END-PERFORM.
       DISPLAY-SEARCH-END.
           EXIT.

      *>   Pantalla de checkout
       DISPLAY-CHECKOUT.
           PERFORM CLEAR-INFO-SCREEN.

      *>   Header:
           STRING " ITEM  G SZ  SL         DESCR               "
                  "Qty    Unit$    SubTotal"
           INTO AUX-MSGXL.
           DISPLAY AUX-MSGXL LINE 12 COL 1.
           STRING "+-----+-+--+---+--------------------------+-----"
                  "+---------+----------+--------------"
           INTO AUX-MSGXL.
           DISPLAY AUX-MSGXL LINE 13 COL 1.

           MOVE 14 TO AUX-LINE-POS.
           MOVE 1  TO AUX-COL-POS.


      *>   Itero por cada compra:
           PERFORM VARYING AUX-I FROM 1 BY 1 UNTIL AUX-I > BUY-COUNTER
               INITIALISE AUX-MSGXL
               MOVE BUY-IDXSTOCK(AUX-I) TO AUX-99

               COMPUTE TOTAL-ITEM = BUY-AMOUNT(AUX-I) * PRICE(AUX-99)
               MOVE PRICE(AUX-99) TO AUX-PAY-Z
               MOVE TOTAL-ITEM TO AUX-PAY-ZZ
               ADD TOTAL-ITEM TO TOTAL-CHECKOUT

               IF AUX-99 > 0
                   STRING "[ " ,AUX-99, " ] "
                           ITEM-GENDER(AUX-99), " "
                           ITEM-SIZE(AUX-99), " "
                           ITEM-SLEEVE(AUX-99)  , " "
                           DESCRIP(AUX-99)     , "     "
                           BUY-AMOUNT(AUX-I) , "   "
                           AUX-PAY-Z , "    "
                           AUX-PAY-ZZ
                   INTO AUX-MSGXL
                   DISPLAY AUX-MSGXL LINE AUX-LINE-POS COL AUX-COL-POS
                   ADD 1 TO AUX-LINE-POS
                   IF AUX-LINE-POS > 26
                       MOVE 12 TO AUX-LINE-POS
                       MOVE 55 TO AUX-COL-POS
                   END-IF

               END-IF
           END-PERFORM.

      *>   Aplico descuentos y recargos
           COMPUTE AUX-99999 = TOTAL-CHECKOUT * 20 / 100
           MOVE AUX-99999 TO AUX-PAY-Z.
           DISPLAY "Club Member  -20%:  "   LINE 21 COL 41
                   AUX-PAY-Z   LINE 21 COL 61
                   .

           COMPUTE TOTAL-CHECKOUT = TOTAL-CHECKOUT
                                    - TOTAL-CHECKOUT * 20 / 100
           MOVE TOTAL-CHECKOUT TO AUX-PAY-Z.
           DISPLAY "TOTAL CHECKOUT:  "   LINE 23 COL 41
                   AUX-PAY-Z   LINE 23 COL 61
                   .
           ACCEPT OMITTED.

       DISPLAY-CHECKOUT-END.
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
           DISPLAY " " LINE 10 COL 1 ERASE EOS.

       CLEAR-SCREEN.
           DISPLAY " " LINE 1 COL 1 ERASE EOS.
           DISPLAY "=====  Manchester Shopping V 3.4 ======="
                   LINE 2 COL 40 REVERSED.

       CARGA-PARAMETROS.
           MOVE "105MXL SJersey-Short sleeved-1Kit0100120" TO STOCK(1).
           MOVE "106MXL LJersey-Long  sleeved-1Kit0120020" TO STOCK(2).
           MOVE "108M2XLLJersey-Long  sleeved-1Kit0120020" TO STOCK(3).
           MOVE "109M3XLSJersey-Short sleeved-1Kit0100120" TO STOCK(4).
           MOVE "110M3XLLJersey-Long  sleeved-1Kit0120020" TO STOCK(5).
           MOVE "111WXS SJersey-Short sleeved-1Kit0100130" TO STOCK(6).
           MOVE "112WXS LJersey-Long  sleeved-1Kit0120030" TO STOCK(7).
           MOVE "113WXM SJersey-Short sleeved-1Kit0100130" TO STOCK(8).
           MOVE "125CXL SJersey-Short sleeved-1Kit0070100" TO STOCK(9).
           MOVE "126CXL LJersey-Long  sleeved-1Kit0090000" TO STOCK(10).
           MOVE "127C2XLSJersey-Short sleeved-1Kit0070100" TO STOCK(11).
           MOVE "141MXS SJersey-Short sleeved-2Kit0100100" TO STOCK(12).
           MOVE "142MXS LJersey-Long  sleeved-2Kit0120090" TO STOCK(13).
           MOVE "164CM  LJersey-Long  sleeved-2Kit0090090" TO STOCK(14).
           MOVE "165CL  SJersey-Short sleeved-2Kit0070100" TO STOCK(15).
           MOVE "166CXL LJersey-Long  sleeved-2Kit0090090" TO STOCK(16).
           MOVE "167C2XLSJersey-Short sleeved-2Kit0070100" TO STOCK(17).
           MOVE "168C2XLLJersey-Long  sleeved-2Kit0090090" TO STOCK(18).
           MOVE "169C3XLSJersey-Short sleeved-2Kit0070100" TO STOCK(19).
           MOVE "170C3XLLJersey-Long  sleeved-2Kit0090090" TO STOCK(20).
           MOVE "171MXS SJersey-Short sleeved-3Kit0100100" TO STOCK(21).
           MOVE "172MXS LJersey-Long  sleeved-3Kit0120090" TO STOCK(22).
           MOVE "186WXL LJersey-Long  sleeved-3Kit0120090" TO STOCK(23).
           MOVE "187W2XLSJersey-Short sleeved-3Kit0100100" TO STOCK(24).
           MOVE "188W2XLLJersey-Long  sleeved-3Kit0120090" TO STOCK(25).
           MOVE "199C3XLSJersey-Short sleeved-3Kit0070100" TO STOCK(26).
           MOVE "200C3XLLJersey-Long  sleeved-3Kit0090090" TO STOCK(27).

           MOVE 0 TO BUY-COUNTER.
           EXIT.

       END PROGRAM 04-ManchesterShopping.
