      ******************************************************************
      * Author: gottdammer
      * Date:
      * Purpose:
      *>   * 3. Create an university enrollment system
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 03-UNIVERSITY.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-USERNAME PIC X(15).
       01  WS-PASSWORD PIC X(15).
       01  WS-LOGINCOUNT PIC 9(2).

      *>   Contadores de plazas ocupadas
       01  WS-COUNTER-CSI PIC 9 VALUE 0.
       01  WS-COUNTER-MED PIC 9 VALUE 0.
       01  WS-COUNTER-MKT PIC 9 VALUE 0.
       01  WS-COUNTER-ART PIC 9 VALUE 0.

      *>   Londres
       01  WS-COUNTER-LON-CSI PIC 9 VALUE 0.
       01  WS-COUNTER-LON-MED PIC 9 VALUE 0.
       01  WS-COUNTER-LON-MKT PIC 9 VALUE 0.
       01  WS-COUNTER-LON-ART PIC 9 VALUE 0.

      *>   Manchester
       01  WS-COUNTER-MAN-CSI PIC 9 VALUE 0.
       01  WS-COUNTER-MAN-MED PIC 9 VALUE 0.
       01  WS-COUNTER-MAN-MKT PIC 9 VALUE 0.
       01  WS-COUNTER-MAN-ART PIC 9 VALUE 0.

      *>   Liverpool
       01  WS-COUNTER-LIV-CSI PIC 9 VALUE 0.
       01  WS-COUNTER-LIV-MED PIC 9 VALUE 0.
       01  WS-COUNTER-LIV-MKT PIC 9 VALUE 0.
       01  WS-COUNTER-LIV-ART PIC 9 VALUE 0.

       01  WS-TOTALBALANCE PIC S9(9)V9(2) VALUE +2000.00.
       01  WS-TRANSACTION  PIC S9(9)V9(2) VALUE +0.00.
       01  WS-TRANSFERDEST  PIC X(15).
       01  WS-MENUOPCION PIC X(1).

       01  WS-AUX-MESSAGE PIC X(50).
       01  WS-AUX-COUNTER PIC 9 VALUE 0.
       01  WS-AUX-PROGRAM  PIC X(3).
       01  WS-AUX-LOCATION PIC X(3).

      *>   CONSTANTES
       01  CONST-MAX-SLOTS-FOR-PROGRAMS PIC 9 VALUE 5.
       01  CONST-MAX-SLOTS-LONDON PIC 9 VALUE 1.
       01  CONST-MAX-SLOTS-MANCHESTER PIC 9 VALUE 3.
       01  CONST-MAX-SLOTS-LIVERPOOL PIC 9 VALUE 1.

       01  CONST-MAX-LOGIN-COUNT-TOBLOCK PIC 9 VALUE 3.


       PROCEDURE DIVISION.
       MAIN.
           PERFORM MENU-LOGIN.
           PERFORM MENU-PROGRAMS.


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
      *>       Menu seleccion de programas disponibles
      *>   *************************************************************
       MENU-PROGRAMS.
           PERFORM CLEAR-SCREEN.
           DISPLAY "Programas disponibles: "       LINE 6 COL 5
                   "[1] Computer Science"          LINE 8 COL 5
                   "[2] Medicine"                  LINE 9 COL 5
                   "[3] Marketing"                 LINE 10 COL 5
                   "[4] Arts"                      LINE 11 COL 5
                   "[9] Reporte Inscp"             LINE 12 COL 5
                   "[0] Salir"                     LINE 13 COL 5
                   .

           STRING "( " WS-COUNTER-CSI " )     " INTO WS-AUX-MESSAGE.
           DISPLAY WS-AUX-MESSAGE LINE 8 COL 30.
           STRING "( " WS-COUNTER-MED " )     " INTO WS-AUX-MESSAGE.
           DISPLAY WS-AUX-MESSAGE LINE 9 COL 30.
           STRING "( " WS-COUNTER-MKT " )     " INTO WS-AUX-MESSAGE.
           DISPLAY WS-AUX-MESSAGE LINE 10 COL 30.
           STRING "( " WS-COUNTER-ART " )     " INTO WS-AUX-MESSAGE.
           DISPLAY WS-AUX-MESSAGE LINE 11 COL 30.

           DISPLAY "Entre (X) se indica cantidad ACTUAL"   LINE 15 COL 5
                   "de plazas disponibles por cada programa. "
                                                           LINE 16 COL 5
                   .

           DISPLAY "Opcion [ ]"                    LINE 18 COL 5.
           ACCEPT WS-MENUOPCION                    LINE 18 COL 13.
           EVALUATE WS-MENUOPCION
               WHEN '1'
                   MOVE WS-COUNTER-CSI TO WS-AUX-COUNTER
                   PERFORM VALIDATE-MAX-COUNT-PROGRMS
                   ADD 1 TO WS-COUNTER-CSI
                   MOVE "CSI" TO WS-AUX-PROGRAM
                   PERFORM MENU-CAMPUS-LOCATIONS
               WHEN '2'
                   MOVE WS-COUNTER-MED TO WS-AUX-COUNTER
                   PERFORM VALIDATE-MAX-COUNT-PROGRMS
                   ADD 1 TO WS-COUNTER-MED
                   MOVE "MED" TO WS-AUX-PROGRAM
                   PERFORM MENU-CAMPUS-LOCATIONS
               WHEN '3'
                   MOVE WS-COUNTER-MKT TO WS-AUX-COUNTER
                   PERFORM VALIDATE-MAX-COUNT-PROGRMS
                   ADD 1 TO WS-COUNTER-MKT
                   MOVE "MKT" TO WS-AUX-PROGRAM
                   PERFORM MENU-CAMPUS-LOCATIONS
               WHEN '4'
                   MOVE WS-COUNTER-ART TO WS-AUX-COUNTER
                   PERFORM VALIDATE-MAX-COUNT-PROGRMS
                   ADD 1 TO WS-COUNTER-ART
                   MOVE "ART" TO WS-AUX-PROGRAM
                   PERFORM MENU-CAMPUS-LOCATIONS

               WHEN '9'
                   PERFORM REPORTE-INSCRIPCIONES

               WHEN '0'
                   DISPLAY "Bye!"
                   STOP RUN
               WHEN OTHER
                   DISPLAY "Opcion Incorrecta!!"   LINE 20 COL 5
                           "Reintente por favor!!" LINE 22 COL 5
                   PERFORM MENU-DISPLAYCONTINUE

           END-EVALUATE.
           GO TO MENU-PROGRAMS.

      *>   Valida que no se supere la cant maxima cupos de programas
       VALIDATE-MAX-COUNT-PROGRMS.
           IF WS-AUX-COUNTER = CONST-MAX-SLOTS-FOR-PROGRAMS
               DISPLAY
                   "No existe mas cupos para el programa seleccionado."
                   LINE 20 COL 5
                   "Seleccione otro programa disponible."
                   LINE 21 COL 5
                PERFORM MENU-DISPLAYCONTINUE
                GO TO MENU-PROGRAMS
           END-IF.


       MENU-PROGRAMS-END.
           EXIT.


       MENU-CAMPUS-LOCATIONS.
           PERFORM CLEAR-SCREEN.
           DISPLAY "Ubicacion de los Campus: "     LINE 6 COL 5
                   "[1] London"                    LINE 8 COL 5
                   "[2] Manchester"                LINE 9 COL 5
                   "[3] Liverpool"                 LINE 10 COL 5
                   .

           DISPLAY "Opcion [ ]"                    LINE 12 COL 5.
           ACCEPT WS-MENUOPCION                    LINE 12 COL 13.
           EVALUATE WS-MENUOPCION
               WHEN '1'
                   MOVE "LON" TO WS-AUX-LOCATION
                   GO TO VALIDATE-LOCATIONS
               WHEN '2'
                   MOVE "MAN" TO WS-AUX-LOCATION
                   GO TO VALIDATE-LOCATIONS
               WHEN '3'
                   MOVE "LIV" TO WS-AUX-LOCATION
                   GO TO VALIDATE-LOCATIONS

               WHEN '0'
                   DISPLAY "Bye!"
                   STOP RUN
               WHEN OTHER
                   DISPLAY "Opcion Incorrecta!!"   LINE 20 COL 5
                           "Reintente por favor!!" LINE 22 COL 5
                   PERFORM MENU-DISPLAYCONTINUE
           END-EVALUATE.
           GO TO MENU-CAMPUS-LOCATIONS.

       VALIDATE-LOCATIONS.
           IF WS-AUX-LOCATION = "LON"
              EVALUATE WS-AUX-PROGRAM
                   WHEN "CSI"
                       IF WS-COUNTER-LON-CSI = CONST-MAX-SLOTS-LONDON
                           GO TO VALIDATE-LOCATIONS-NOMOREPLACES
                       END-IF
                       ADD 1 TO WS-COUNTER-LON-CSI
                   WHEN "MED"
                       IF WS-COUNTER-LON-MED = CONST-MAX-SLOTS-LONDON
                           GO TO VALIDATE-LOCATIONS-NOMOREPLACES
                       END-IF
                       ADD 1 TO WS-COUNTER-LON-MED
                   WHEN "MKT"
                       IF WS-COUNTER-LON-MKT = CONST-MAX-SLOTS-LONDON
                           GO TO VALIDATE-LOCATIONS-NOMOREPLACES
                       END-IF
                       ADD 1 TO WS-COUNTER-LON-MKT
                   WHEN "ART"
                       IF WS-COUNTER-LON-ART = CONST-MAX-SLOTS-LONDON
                           GO TO VALIDATE-LOCATIONS-NOMOREPLACES
                       END-IF
                       ADD 1 TO WS-COUNTER-LON-ART
              END-EVALUATE
           END-IF.
           IF WS-AUX-LOCATION = "MAN"
                            EVALUATE WS-AUX-PROGRAM
                   WHEN "CSI"
                      IF WS-COUNTER-MAN-CSI = CONST-MAX-SLOTS-MANCHESTER
                           GO TO VALIDATE-LOCATIONS-NOMOREPLACES
                      END-IF
                      ADD 1 TO WS-COUNTER-MAN-CSI
                   WHEN "MED"
                      IF WS-COUNTER-MAN-MED = CONST-MAX-SLOTS-MANCHESTER
                           GO TO VALIDATE-LOCATIONS-NOMOREPLACES
                      END-IF
                      ADD 1 TO WS-COUNTER-MAN-MED
                   WHEN "MKT"
                      IF WS-COUNTER-MAN-MKT = CONST-MAX-SLOTS-MANCHESTER
                           GO TO VALIDATE-LOCATIONS-NOMOREPLACES
                      END-IF
                      ADD 1 TO WS-COUNTER-MAN-MKT
                   WHEN "ART"
                      IF WS-COUNTER-MAN-ART = CONST-MAX-SLOTS-MANCHESTER
                           GO TO VALIDATE-LOCATIONS-NOMOREPLACES
                      END-IF
                      ADD 1 TO WS-COUNTER-MAN-ART
              END-EVALUATE
           END-IF.
           IF WS-AUX-LOCATION = "LIV"
              EVALUATE WS-AUX-PROGRAM
                   WHEN "CSI"
                       IF WS-COUNTER-LIV-CSI = CONST-MAX-SLOTS-LIVERPOOL
                           GO TO VALIDATE-LOCATIONS-NOMOREPLACES
                       END-IF
                       ADD 1 TO WS-COUNTER-LIV-CSI
                   WHEN "MED"
                       IF WS-COUNTER-LIV-MED = CONST-MAX-SLOTS-LIVERPOOL
                           GO TO VALIDATE-LOCATIONS-NOMOREPLACES
                       END-IF
                       ADD 1 TO WS-COUNTER-LIV-MED
                   WHEN "MKT"
                       IF WS-COUNTER-LIV-MKT = CONST-MAX-SLOTS-LIVERPOOL
                           GO TO VALIDATE-LOCATIONS-NOMOREPLACES
                       END-IF
                       ADD 1 TO WS-COUNTER-LIV-MKT
                   WHEN "ART"
                       IF WS-COUNTER-LIV-ART = CONST-MAX-SLOTS-LIVERPOOL
                           GO TO VALIDATE-LOCATIONS-NOMOREPLACES
                       END-IF
                       ADD 1 TO WS-COUNTER-LIV-ART
              END-EVALUATE
           END-IF.
           GO TO MENU-CAMPUS-LOCATIONS-END.

       VALIDATE-LOCATIONS-NOMOREPLACES.
           DISPLAY "No hay mas cupo para el campus  " LINE 20 COL 5.
           EVALUATE WS-AUX-LOCATION
               WHEN "LON"
                   MOVE "Londres" TO WS-AUX-MESSAGE
               WHEN "MAN"
                   MOVE "Manchester" TO WS-AUX-MESSAGE
               WHEN "LIV"
                   MOVE "Liverpool" TO WS-AUX-MESSAGE
           END-EVALUATE.
           DISPLAY WS-AUX-MESSAGE LINE 20 COL 37.
           PERFORM MENU-DISPLAYCONTINUE.
           GO TO MENU-CAMPUS-LOCATIONS.


       MENU-CAMPUS-LOCATIONS-END.
           GO TO MENU-PROGRAMS.


       REPORTE-INSCRIPCIONES.
           PERFORM CLEAR-SCREEN.
           DISPLAY "Informe Inscripciones Total x Campus:" LINE 6 COL 5.
           DISPLAY "Lond. Manch. Liver."
                   LINE 7 COL 23.
           STRING  " C. Informaticas     " WS-COUNTER-LON-CSI
                   "    " WS-COUNTER-MAN-CSI
                   "    " WS-COUNTER-LIV-CSI
                   INTO WS-AUX-MESSAGE.
           DISPLAY WS-AUX-MESSAGE       LINE 8 COL 5.
           STRING  " Medicina            " WS-COUNTER-LON-MED
                   "    " WS-COUNTER-MAN-MED
                   "    " WS-COUNTER-LIV-MED
                   INTO WS-AUX-MESSAGE.
           DISPLAY WS-AUX-MESSAGE       LINE 9 COL 5.
           STRING  " Marketing           " WS-COUNTER-LON-MKT
                   "    " WS-COUNTER-MAN-MKT
                   "    " WS-COUNTER-LIV-MKT
                   INTO WS-AUX-MESSAGE.
           DISPLAY WS-AUX-MESSAGE       LINE 10 COL 5.
           STRING  " Arte                " WS-COUNTER-LON-ART
                   "    " WS-COUNTER-MAN-ART
                   "    " WS-COUNTER-LIV-ART
                   INTO WS-AUX-MESSAGE.
           DISPLAY WS-AUX-MESSAGE       LINE 11 COL 5.

           PERFORM MENU-DISPLAYCONTINUE.
           MOVE "" TO WS-AUX-MESSAGE.


      *>   Simple <pause> para leer
       MENU-DISPLAYCONTINUE.
           DISPLAY "Apriete [ENTER] para continuar...."
                           LINE 28 COL 5.
           ACCEPT WS-MENUOPCION LINE 28 COL 40.


       CLEAR-SCREEN.
           DISPLAY " " LINE 1 COL 1 ERASE EOS.
           DISPLAY "=====  SISTEMA UNIVERSITARIO ONLINE ======="
                   LINE 2 COL 5 REVERSED.





       END PROGRAM 03-UNIVERSITY.
