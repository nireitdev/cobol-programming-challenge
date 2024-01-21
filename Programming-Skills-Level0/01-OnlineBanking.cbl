      ******************************************************************
      * Author: gottdammer
      * Date:
      * Purpose:
      *>   * 1. Create an online banking system with the following features:
      *>      * Users must BACKGROUND-COLORe able to log in with a username and password.
      *>      * If the user enters the wrong credentials three times,
      *>           the system must lock them out.
      *>      * The initial balance in the bank account is $2000.
      *>      * The system must allow users to deposit, withdraw, view,
      *>           and transfer money.
      *>      * The system must display a menu for users to perform transactions.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 01-ONLINE-BANKING.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-USERNAME PIC X(15).
       01  WS-PASSWORD PIC X(15).
       01  WS-LOGINCOUNT PIC 9(2).
       01  WS-MAXLOGINCOUNT PIC 9(2) VALUE 3.
       01  WS-TOTALBALANCE PIC S9(9)V9(2) VALUE +2000.00.
       01  WS-TRANSACTION  PIC S9(9)V9(2) VALUE +0.00.
       01  WS-TRANSFERDEST  PIC X(15).
       01  WS-MENUOPCION PIC X(1).

       01  WS-STRING-SALDO-ACTUAL PIC X(60).
       01  WS-TOTALBALANCE-Z PIC -(8)9.9(2).



       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM LOGIN-PROCEDURE.
            STOP RUN.

       LOGIN-PROCEDURE.
           PERFORM CLEAR-SCREEN.
           DISPLAY "     ACCESO USUARIOS       " LINE 3 COL 5
                   "Ingrese su usuario:"         LINE 7 COL 5.
           ACCEPT WS-USERNAME                    LINE 7 COL 25 .
           DISPLAY "Ingrese su password:"        LINE 8 COL 5.
           ACCEPT WS-PASSWORD                    LINE 8 COL 25 .

           IF WS-USERNAME = "usuario"
               IF WS-PASSWORD = "password"
                   PERFORM MENU-PROCEDURE
               ELSE
                   PERFORM DISPLAYINVALIDUSER-PROCEDURE
               END-IF
           ELSE
               PERFORM DISPLAYINVALIDUSER-PROCEDURE
           END-IF.
           GO TO LOGIN-PROCEDURE.

       DISPLAYINVALIDUSER-PROCEDURE.

           DISPLAY "Usuario Invalido."             LINE 20 COL 6
                   "Por favor reintente"           LINE 20 COL 6.

           ADD 1 TO WS-LOGINCOUNT.
           IF WS-LOGINCOUNT > WS-MAXLOGINCOUNT
               PERFORM CLEAR-SCREEN
               DISPLAY
               "*** Su usuario ha sido bloqueado por      *** "
                                               LINE 10 COL 5 REVERSED
               "*** reiterados accesos invalidos          *** "
                                               LINE 11 COL 5 REVERSED
               "*** Su tarjeta sera retenida y destruida! *** "
                                               LINE 12 COL 5 REVERSED
               "***            uwu!                       *** "
                                               LINE 13 COL 5 REVERSED
               PERFORM DISPLAYCONTINUE-PROCEDURE
               STOP RUN
           END-IF.


       MENU-PROCEDURE.
           PERFORM CLEAR-SCREEN.
           DISPLAY "Menu de Usuario: "             LINE 6 COL 5
                   "[D] Depositar"                 LINE 8 COL 5
                   "[R] Retirar"                   LINE 9 COL 5
                   "[V] Ver Saldos"                LINE 10 COL 5
                   "[T] Transferencias"            LINE 11 COL 5
                   
                   "[X] Finalizar"                 LINE 13 COL 5.

           DISPLAY "Opcion [ ]"                    LINE 15 COL 5.
           ACCEPT WS-MENUOPCION                    LINE 15 COL 13.
           EVALUATE WS-MENUOPCION
               WHEN 'D'
                   PERFORM DEPOSIT-PROCEDURE
               WHEN 'R'
                   PERFORM WITHDRAW-PROCEDURE
               WHEN 'V'
                   PERFORM VIEWACCOUNT-PROCEDURE
               WHEN 'T'
                   PERFORM TRANSFER-PROCEDURE
               WHEN 'X'
                   DISPLAY "Bye!"
                   STOP RUN
               WHEN OTHER
                   DISPLAY "Opcion Incorrecta!!"   LINE 20 COL 5
                           "Solo letras mayusculas."LINE 21 COL 5
                           "Reintente por favor!!" LINE 23 COL 5
                   PERFORM DISPLAYCONTINUE-PROCEDURE

           END-EVALUATE.
           GO TO MENU-PROCEDURE.

       DEPOSIT-PROCEDURE.
           PERFORM CLEAR-SCREEN.
           DISPLAY "Menu Despositar Dinero"        LINE 6 COL 5.
           PERFORM DISPLAYBALANCE-PROCEDURE.
           DISPLAY WS-STRING-SALDO-ACTUAL          LINE 7 COL 5.
           DISPLAY "Indique monto a depositar: "   LINE 10 COL 5.
           ACCEPT WS-TOTALBALANCE-Z              LINE 10 COL 32 PROMPT.

      *>      Sumo deposito al saldo total
           MOVE WS-TOTALBALANCE-Z TO WS-TRANSACTION.
           ADD WS-TRANSACTION TO WS-TOTALBALANCE.

           PERFORM DISPLAYBALANCE-PROCEDURE.
           DISPLAY WS-STRING-SALDO-ACTUAL          LINE 12 COL 5.
           PERFORM DISPLAYCONTINUE-PROCEDURE.


      *>   Solo muestra el saldo actual
       DISPLAYBALANCE-PROCEDURE.
           MOVE WS-TOTALBALANCE TO WS-TOTALBALANCE-Z.
           STRING "Saldo actual: ARS " WS-TOTALBALANCE-Z
                   INTO WS-STRING-SALDO-ACTUAL.


      *>      Simple <pause> para leer
       DISPLAYCONTINUE-PROCEDURE.
           DISPLAY "Apriete [ENTER] para continuar...."
                           LINE 28 COL 5.
           ACCEPT WS-MENUOPCION LINE 28 COL 40.


       WITHDRAW-PROCEDURE.
           PERFORM CLEAR-SCREEN.
           DISPLAY "Menu Retirar Dinero"         LINE 6 COL 5.
           PERFORM DISPLAYBALANCE-PROCEDURE.
           DISPLAY WS-STRING-SALDO-ACTUAL        LINE 7 COL 5.
           DISPLAY "Indique monto a retirar: "   LINE 10 COL 5.
           ACCEPT WS-TOTALBALANCE-Z              LINE 10 COL 32 PROMPT.
           MOVE WS-TOTALBALANCE-Z TO WS-TRANSACTION.

           IF WS-TRANSACTION>WS-TOTALBALANCE THEN
               DISPLAY "No es posible retirar "            LINE 14 COL 5
                       "un monto mayor al saldo actual "   LINE 15 COL 5
                       "de su cuenta."                     LINE 16 COL 5
           ELSE
               SUBTRACT WS-TRANSACTION FROM WS-TOTALBALANCE
               PERFORM DISPLAYBALANCE-PROCEDURE
               DISPLAY WS-STRING-SALDO-ACTUAL        LINE 12 COL 5
           END-IF.
           PERFORM DISPLAYCONTINUE-PROCEDURE.

       VIEWACCOUNT-PROCEDURE.
           PERFORM CLEAR-SCREEN.
           DISPLAY "Menu Saldos"         LINE 6 COL 5.
           PERFORM DISPLAYBALANCE-PROCEDURE.
           DISPLAY WS-STRING-SALDO-ACTUAL        LINE 8 COL 5 REVERSED.
           PERFORM DISPLAYCONTINUE-PROCEDURE.

       TRANSFER-PROCEDURE.
           PERFORM CLEAR-SCREEN.
           DISPLAY "Menu Transferencias"         LINE 6 COL 5.
           PERFORM DISPLAYBALANCE-PROCEDURE.
           DISPLAY WS-STRING-SALDO-ACTUAL        LINE 8 COL 5.

           DISPLAY "Indique codigo Destinatario: " LINE 10 COL 5.
           ACCEPT WS-TRANSFERDEST LINE 10 COL 35.

           DISPLAY "Indique monto a Transferir: " LINE 11 COL 5.
           ACCEPT WS-TOTALBALANCE-Z              LINE 11 COL 40 PROMPT.
           MOVE WS-TOTALBALANCE-Z TO WS-TRANSACTION.


           DISPLAY "Esta seguro? [S][N]" LINE 12 COL 5.
           ACCEPT WS-MENUOPCION LINE 12 COL 30.
           EVALUATE WS-MENUOPCION
               WHEN 'S'
                   IF WS-TRANSACTION>WS-TOTALBALANCE THEN
                       DISPLAY "ERROR: No dispone de fondos."
                                               LINE 15 COL 5
                               "Para esta transferencia." LINE 16 COL 5
                   ELSE
                       SUBTRACT WS-TRANSACTION FROM WS-TOTALBALANCE
                       PERFORM DISPLAYBALANCE-PROCEDURE
                       DISPLAY WS-STRING-SALDO-ACTUAL
                                       LINE 15 COL 5 REVERSED

                   END-IF
               WHEN OTHER
                   DISPLAY "Se cancela la operacion." LINE 15 COL 5
           END-EVALUATE.
           PERFORM DISPLAYCONTINUE-PROCEDURE.


       CURRENCYCONVERTER-PROCEDURE.
           DISPLAY "==== Menu Monedas y Conversiones".
           DISPLAY "Monedas disponibles: ".
           DISPLAY "[1] CLP".
           DISPLAY "[2] USD".
           DISPLAY "[3] EUR".
           DISPLAY "[4] TRY".
           DISPLAY "[5] GBP".
           DISPLAY " ".
           DISPLAY "opcion:"
           ACCEPT WS-MENUOPCION FROM CONSOLE.
           EVALUATE WS-MENUOPCION
               WHEN '1'
                   PERFORM DEPOSIT-PROCEDURE
               WHEN '2'
                   PERFORM WITHDRAW-PROCEDURE
               WHEN '3'
                   PERFORM VIEWACCOUNT-PROCEDURE
               WHEN '4'
                   PERFORM TRANSFER-PROCEDURE
               WHEN '5'
                   DISPLAY "Bye!"
                   STOP RUN
               WHEN OTHER
                   DISPLAY "Opcion Incorrecta!!"
                   DISPLAY " "
           END-EVALUATE.

       CLEAR-SCREEN.
           DISPLAY " " LINE 1 COL 1 ERASE EOS.
           DISPLAY "=====  BANCO ONLINE =======" LINE 2 COL 5 REVERSED.

       END PROGRAM 01-ONLINE-BANKING.
