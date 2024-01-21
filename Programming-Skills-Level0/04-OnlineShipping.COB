      ******************************************************************
      * Author: gottdammer
      * Date:
      * Purpose:
      *>   * 4. Online shipping program
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 04-ONLINESHIPPING.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  WS-MENU-OPTION PIC X.

       01  WS-USERNAME PIC X(15).
       01  WS-PASSWORD PIC X(15).
       01  WS-LOGINCOUNT PIC 9(2).

      *>   Datos del envio
       01  WS-SENDER-NAME  PIC X(25).
       01  WS-SENDER-DIRECTION  PIC X(25).
       01  WS-RECIPT-NAME  PIC X(25).
       01  WS-RECIPT-DIRECTION  PIC X(25).

       01  WS-PACKG-WEIGHT-KG PIC 9(5).
       01  WS-PACKG-WEIGHT-KG-Z PIC Z(4)9.
       01  WS-PACKG-TOTAL-COST PIC 9(4).
       01  WS-PACKG-TOTAL-COST-Z PIC Z(3)9.

       01  WS-PACKG-ID PIC 9(4) VALUES ZEROES.

      *>   CONSTANTES
       01  CONST-SHPNG-COST-BY-KG PIC 9 VALUE 2.
       01  CONST-SHPNG-MAXWEIGHT  PIC 99 VALUE 25.

       01  CONST-MAX-LOGIN-COUNT-TOBLOCK PIC 9 VALUE 2.


       PROCEDURE DIVISION.
       MAIN.
           PERFORM MENU-LOGIN.
           PERFORM MENU-MAIN.
           STOP RUN.

      *>   *************************************************************
      *>       Menu Login de  usuarios.
      *>   Default user/password:   usuario/password
      *>   *************************************************************
       MENU-LOGIN.
           PERFORM CLEAR-SCREEN.
           DISPLAY "     ACCESO USUARIOS       " LINE 3 COL 5
                   "Ingrese su usuario:"         LINE 7 COL 5
                   .
           ACCEPT WS-USERNAME                    LINE 7 COL 25 .
           DISPLAY "Ingrese su password:"        LINE 8 COL 5.
           ACCEPT WS-PASSWORD                    LINE 8 COL 25 .

      *>   @TODO: faltaria trabajar con mas usuarios => DB2 o archivos
           IF WS-USERNAME = "usuario"
               IF WS-PASSWORD = "password"
                   GO TO MENU-LOGIN-END
               ELSE
                   PERFORM MENU-LOGIN-USUARIO-INVALIDO
               END-IF
           ELSE
               PERFORM MENU-LOGIN-USUARIO-INVALIDO
           END-IF.
           GO TO MENU-LOGIN.

       MENU-LOGIN-USUARIO-INVALIDO.
           DISPLAY "Usuario Invalido."             LINE 20 COL 6
                   "Por favor reintente"           LINE 20 COL 6
                   .

           ADD 1 TO WS-LOGINCOUNT.
           IF WS-LOGINCOUNT > CONST-MAX-LOGIN-COUNT-TOBLOCK
               PERFORM CLEAR-SCREEN
               DISPLAY
               "*** Su usuario ha sido bloqueado por      *** "
                                               LINE 10 COL 5 REVERSED
               "*** reiterados accesos invalidos          *** "
                                               LINE 11 COL 5 REVERSED
               "***            uwu!                       *** "
                                               LINE 12 COL 5 REVERSED
               PERFORM MENU-DISPLAYCONTINUE
               STOP RUN
           END-IF.

       MENU-LOGIN-END.
           EXIT.


      *>   *************************************************************
      *>       Menu seleccion
      *>   *************************************************************
       MENU-MAIN.
           PERFORM CLEAR-SCREEN.
           DISPLAY "Opciones disponibles: "        LINE 6 COL 5
                   "[1] Envio de paquete"          LINE 8 COL 5
                   "[0] Salir"                     LINE 10 COL 5
                   .

           DISPLAY "Opcion [ ]"                    LINE 12 COL 5.
           ACCEPT WS-MENU-OPTION                   LINE 12 COL 13.
           EVALUATE WS-MENU-OPTION
               WHEN '1'
                   PERFORM SEND-PACKAGE THRU SEND-PACKAGE-END
               WHEN '0'
                   DISPLAY "Bye!"
                   STOP RUN
               WHEN OTHER
                   DISPLAY "Opcion Incorrecta!!"   LINE 20 COL 5
                           "Reintente por favor!!" LINE 22 COL 5
                   PERFORM MENU-DISPLAYCONTINUE

           END-EVALUATE.
           GO TO MENU-MAIN.

       MENU-MAIN-END.
           EXIT.

      *>   *************************************************************
      *>   Opciones del envio
      *>   *************************************************************
       SEND-PACKAGE.
           PERFORM CLEAR-SCREEN.
           COMPUTE WS-PACKG-ID = FUNCTION RANDOM * 10000.
           DISPLAY "Envio numero ID:"          LINE 5 COL 5
                   WS-PACKG-ID                 LINE 5 COL 25
                   .
      *>      DISPLAY "Complete los campos: "     LINE 6 COL 5
           DISPLAY "Envio: "                   LINE 7 COL 5
                   "   Nombre:"                LINE 8 COL 5
                   "   Direccion:"             LINE 9 COL 5
                   "Destinatario: "            LINE 10 COL 5
                   "   Nombre:"                LINE 11 COL 5
                   "   Direccion:"             LINE 12 COL 5
                   "Peso del paquete [kg]:"    LINE 14 COL 5
                   .

           ACCEPT WS-SENDER-NAME               LINE 8 COL 20 PROMPT.
           ACCEPT WS-SENDER-DIRECTION          LINE 9 COL 20 PROMPT.
           ACCEPT WS-RECIPT-NAME               LINE 11 COL 20 PROMPT.
           ACCEPT WS-RECIPT-DIRECTION          LINE 12 COL 20 PROMPT.

       PACKAGE-VALIDATE-WEIGHT.
           ACCEPT WS-PACKG-WEIGHT-KG-Z         LINE 14 COL 30 PROMPT.
           MOVE WS-PACKG-WEIGHT-KG-Z TO WS-PACKG-WEIGHT-KG.

           IF  WS-PACKG-WEIGHT-KG = 0  OR
               WS-PACKG-WEIGHT-KG>CONST-SHPNG-MAXWEIGHT THEN
               DISPLAY "El peso del paquete debe ser " LINE 20 COL 5
                       "mayor a 0 KG y menor a "       LINE 21 COL 5
                       CONST-SHPNG-MAXWEIGHT           LINE 21 COL 26
                       " KGs."                         LINE 21 COL 28
               PERFORM MENU-DISPLAYCONTINUE
               PERFORM CLEAR-INFO-SCREEN
               GO TO PACKAGE-VALIDATE-WEIGHT
           END-IF.

       CALCULATE-SHIPPING-COST.
           COMPUTE  WS-PACKG-TOTAL-COST =
                     WS-PACKG-WEIGHT-KG * CONST-SHPNG-COST-BY-KG.
           MOVE WS-PACKG-TOTAL-COST TO WS-PACKG-TOTAL-COST-Z.
           DISPLAY "COSTO TOTAL DEL ENVIO: $"   LINE 16 COL 5
                   WS-PACKG-TOTAL-COST-Z        LINE 16 COL 30 REVERSE.

           DISPLAY "Desea realizar el envio? [S/N]" LINE 18 COL 5.
           ACCEPT WS-MENU-OPTION LINE 18 COL 37.
           IF WS-MENU-OPTION = 'S' OR WS-MENU-OPTION = 's' THEN
               DISPLAY "Envio aceptado. Gracias por confiar en Nos."
                                                   LINE 20 COL 5
           ELSE
               DISPLAY "Envio cancelado."
                                                   LINE 20 COL 5
           END-IF.
           PERFORM MENU-DISPLAYCONTINUE.

       SEND-PACKAGE-END.
           EXIT.


      *>   Simple <pause> para leer
       MENU-DISPLAYCONTINUE.
           DISPLAY "Apriete [ENTER] para continuar...."
                           LINE 28 COL 5.
           ACCEPT WS-MENU-OPTION LINE 28 COL 40.

       CLEAR-INFO-SCREEN.
           DISPLAY " " LINE 20 COL 1 ERASE EOS.

       CLEAR-SCREEN.
           DISPLAY " " LINE 1 COL 1 ERASE EOS.
           DISPLAY "=====  SISTEMA de ENVIOS ONLINE ======="
                   LINE 2 COL 5 REVERSED.

       END PROGRAM 04-ONLINESHIPPING.
