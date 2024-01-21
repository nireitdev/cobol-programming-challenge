      ******************************************************************
      * Author: gottdammer
      * Date:
      * Purpose:
      *>   * 2. Create a currency converter between CLP, ARS, USD, EUR, TRY, GBP
      *>
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 02-CURRENCYCONVERTER.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-MENUOPCION PIC X(1).
       01  WS-MENSAJE  PIC X(65).
       01  WS-MONEDA-MENU   PIC X(3).
       01  WS-MONEDA-ORIGEN PIC X(3).
       01  WS-MONEDA-DESTINO PIC X(3).

       01  WS-CONVER-ORIGEN PIC 9(6)V9(2).
       01  WS-CONVER-DESTINO PIC 9(6)V9(2).
       01  WS-MONEDA-MINIMO    PIC 9(9)V9(2).
       01  WS-MONEDA-MAXIMO    PIC 9(9)V9(2).

       01  WS-RETIRO-FONDOS    PIC 9(9)V9(2).

      *>   CONSTANTES DEL SISTEMA
      *>   @TODO: Deberian ser leidos de un archivo texto o de un DB2

      *>   Conversiones referidas todos al valor de u$d 1 (1 dolar)
       01  CONVERS-CLP-USD PIC 9(6)V9(2) VALUE 921.63.
       01  CONVERS-ARS-USD PIC 9(6)V9(2) VALUE 818.60.
       01  CONVERS-EUR-USD PIC 9(6)V9(2) VALUE 0.92.
       01  CONVERS-TRY-USD PIC 9(6)V9(2) VALUE 30.16.
       01  CONVERS-GBP-USD PIC 9(6)V9(2) VALUE 0.79.

      *>  01  WS-TOTALBALANCE PIC S9(9)V9(2) VALUE +2000.00.
      *>  01  WS-TRANSACTION  PIC S9(9)V9(2) VALUE +0.00.

      *>  01  WS-TOTALBALANCE-Z PIC -(8)9.9(2).



       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM ELEGIR-INICIO THRU ELEGIR-FIN.
           STOP RUN.


       ELEGIR-INICIO.

       ELEGIR-MONEDA-ORIGEN.
           STRING "Por favor eligir moneda INICIAL:" INTO WS-MENSAJE.
           PERFORM MENU-MONEDAS.

           EVALUATE WS-MENUOPCION
               WHEN '1'
                   MOVE "CLP" TO WS-MONEDA-MENU
                   MOVE 100 TO WS-MONEDA-MINIMO
                   MOVE 10000 TO WS-MONEDA-MAXIMO
                   MOVE CONVERS-CLP-USD TO WS-CONVER-ORIGEN
               WHEN '2'
                   MOVE "ARS" TO WS-MONEDA-MENU
                   MOVE 1000 TO WS-MONEDA-MINIMO
                   MOVE 100000 TO WS-MONEDA-MAXIMO
                   MOVE CONVERS-ARS-USD TO WS-CONVER-ORIGEN
               WHEN '3'
                   MOVE "USD" TO WS-MONEDA-MENU
                   MOVE 1 TO WS-MONEDA-MINIMO
                   MOVE 10000 TO WS-MONEDA-MAXIMO
                   MOVE 1 TO WS-CONVER-ORIGEN

               WHEN '4'
                   MOVE "EUR" TO WS-MONEDA-MENU
                   MOVE 1 TO WS-MONEDA-MINIMO
                   MOVE 10000 TO WS-MONEDA-MAXIMO
                   MOVE CONVERS-EUR-USD TO WS-CONVER-ORIGEN
               WHEN '5'
                   MOVE "TRY" TO WS-MONEDA-MENU
                   MOVE 10 TO WS-MONEDA-MINIMO
                   MOVE 1000 TO WS-MONEDA-MAXIMO
                   MOVE CONVERS-TRY-USD TO WS-CONVER-ORIGEN
               WHEN '6'
                   MOVE "GBP" TO WS-MONEDA-MENU
                   MOVE 10 TO WS-MONEDA-MINIMO
                   MOVE 1000 TO WS-MONEDA-MAXIMO
                   MOVE CONVERS-GBP-USD TO WS-CONVER-ORIGEN
              WHEN OTHER
                     DISPLAY "Opcion incorrecta. Reitentar."
                           LINE 20 COL 5
                       "apriete [ENTER] para continuar"
                           LINE 21 COL 5
                   ACCEPT WS-MENUOPCION

                   GO TO ELEGIR-MONEDA-ORIGEN
           END-EVALUATE.
           MOVE WS-MONEDA-MENU TO WS-MONEDA-ORIGEN.


       ELEGIR-MONEDA-DESTINO.
           STRING "Moneda origen: " WS-MONEDA-ORIGEN ". "
                   " Por favor eligir moneda DESTINO:" INTO WS-MENSAJE.
           PERFORM MENU-MONEDAS.


           EVALUATE WS-MENUOPCION
               WHEN '1'
                   MOVE "CLP" TO WS-MONEDA-MENU
                   MOVE CONVERS-CLP-USD TO WS-CONVER-DESTINO

               WHEN '2'
                   MOVE "ARS" TO WS-MONEDA-MENU
                   MOVE CONVERS-ARS-USD TO WS-CONVER-DESTINO
               WHEN '3'
                   MOVE "USD" TO WS-MONEDA-MENU
                   MOVE 1 TO WS-CONVER-DESTINO
               WHEN '4'
                   MOVE "EUR" TO WS-MONEDA-MENU
                   MOVE CONVERS-EUR-USD TO WS-CONVER-DESTINO
               WHEN '5'
                   MOVE "TRY" TO WS-MONEDA-MENU
                   MOVE CONVERS-TRY-USD TO WS-CONVER-DESTINO
               WHEN '6'
                   MOVE "GBP" TO WS-MONEDA-MENU
                   MOVE CONVERS-GBP-USD TO WS-CONVER-DESTINO
               WHEN '0'
                   DISPLAY "BYE!"
                   STOP RUN
              WHEN OTHER
                   DISPLAY "Opcion incorrecta. Reitentar."
                           LINE 20 COL 5
                       "apriete [ENTER] para continuar"
                           LINE 21 COL 5
                   ACCEPT WS-MENUOPCION
                   GO TO ELEGIR-MONEDA-DESTINO
           END-EVALUATE.

           IF WS-MONEDA-MENU = WS-MONEDA-ORIGEN
               DISPLAY "MONEDA ORIGEN Y DESTINO NO PUEDEN SER IGUALES"
                           LINE 20 COL 5
                       "apriete [ENTER] para continuar"
                           LINE 21 COL 5
               ACCEPT WS-MENUOPCION
               GO TO ELEGIR-MONEDA-DESTINO
           END-IF.

           MOVE WS-MONEDA-MENU TO WS-MONEDA-DESTINO.


       ELEGIR-RETIRO.
           PERFORM CLEAR-SCREEN.

           MOVE " " TO WS-MENSAJE.
           DISPLAY "IMPORTANTE: monto MINIMO de extraccion:"
                           LINE 12 COL 5
                   WS-MONEDA-MINIMO LINE 12 COL 47
                   "monto MAXIMO de extraccion:" LINE 13 COL 17
                   WS-MONEDA-MAXIMO LINE 13 COL 47
                   .


           DISPLAY "Indique monto en " LINE 10 COL 5
                       WS-MONEDA-ORIGEN LINE 10 COL 23
                       ":" LINE 10 COL 27.

           ACCEPT WS-RETIRO-FONDOS LINE 10 COL 30.

           STRING "Resultado total convertido a " WS-MONEDA-DESTINO
                   INTO WS-MENSAJE.
           DISPLAY WS-MENSAJE LINE 20 COL 5.

           MULTIPLY WS-CONVER-DESTINO BY WS-RETIRO-FONDOS.
           DIVIDE WS-RETIRO-FONDOS BY  WS-CONVER-ORIGEN
                       GIVING WS-RETIRO-FONDOS.

           DISPLAY WS-RETIRO-FONDOS LINE 50 COL 5.

           ACCEPT WS-MENUOPCION.

       ELEGIR-FIN.
           EXIT.



       MENU-MONEDAS.
           PERFORM CLEAR-SCREEN.
           DISPLAY WS-MENSAJE      LINE 5 COL 5.

           DISPLAY "[1] CLP"       LINE 7 COL 5
                   "[2] ARS"       LINE 8 COL 5
                   "[3] USD"       LINE 9 COL 5
                   "[4] EUR"       LINE 10 COL 5
                   "[5] TRY"       LINE 11 COL 5
                   "[6] GBP"       LINE 12 COL 5
                   "[0] Salir" LINE 14 COL 5
                   .

           DISPLAY "Su opcion : [   ]" LINE 16 COL 5.
           ACCEPT WS-MENUOPCION LINE 16 COL 19.





       CLEAR-SCREEN.
           DISPLAY " " LINE 1 COL 1 ERASE EOS.
           DISPLAY "=====  BANCO ONLINE CONVERSION DE MONEDAS ======="
                           LINE 2 COL 5 REVERSED.

       END PROGRAM 02-CURRENCYCONVERTER.
