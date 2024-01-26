      ******************************************************************
      * Author: gottdammer
      * Date:
      * Purpose:
      *>   * 3 - Medical Appointments
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 03-Medical-Appointment.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  MENU-OPTION PIC 9.

      *>   Tabla con especialidades y 3 doctores por cada una.
       01  SPECIALTIES.
           05  SPECIALTY  OCCURS 9 TIMES INDEXED BY IDXSPEC.
               10 NAMESPEC   PIC X(18).
               10 FILLER PIC X VALUE SPACE.
               10 DOCTORS OCCURS 3 TIMES INDEXED BY IDXDOCT.
                   15  DOCNAME PIC X(20).
                   15  FILLER  PIC X VALUE SPACE.
       01  COUNT-APPOINTMENTS PIC 9.
       01  APPOINTMENTS.
           05 APPOINTMENT OCCURS 3 TIMES INDEXED BY IDXAPP.
               10 APP-IDXSPC   PIC 99.
               10 APP-IDXDOC   PIC 99.
               10 FILLER       PIC X VALUE SPACE.
               10 APP-IDXTIME  PIC 9.


       01  TIMESSLOTS.
           05 TIMESLOT OCCURS 8 TIMES INDEXED BY IDXTIME.
               10 TIME-SLOT    PIC X(5).

       01  RANDOMNAMES.
           05 RND-NAME OCCURS 15 TIMES INDEXED BY IDXRND.
               10 RNDNAME PIC X(10).

       01  CHOICE-SPEC PIC 9 VALUE ZEROES.
       01  CHOICE-DOCT PIC 9 VALUE ZEROES.
       01  CHOICE-HHMM PIC 9 VALUE ZEROES.


       01  AUX-LINE-POS    PIC 99.
       01  AUX-FOUND       PIC 9.
       01  AUX-RESULTS     PIC 9.
       01  AUX-X           PIC X.
       01  AUX-9           PIC 9.
       01  AUX-99          PIC 99.
       01  AUX-999         PIC 999.
       01  AUX-MSG         PIC X(130).

       01  AUX-CHR.
           05  CHAR PIC X OCCURS 40 TIMES INDEXED BY IDXORD.




       PROCEDURE DIVISION.
       MAIN.
      *>   Falta implementar un aburrido Login ;) CTRL+C CTRL+V
           PERFORM CARGA-PARAMETROS.
           PERFORM MENU-START THRU MENU-END.
           STOP RUN.

      *>   *************************************************************
      *>   Carga las categorias por defecto
      *>   *************************************************************
       CARGA-PARAMETROS.
           MOVE "General Medicine" TO NAMESPEC(1).
           MOVE "Emergency Care" TO NAMESPEC(2).
           MOVE "Clinical Analysis" TO NAMESPEC(3).
           MOVE "Cardiology" TO NAMESPEC(4).
           MOVE "Neurology" TO NAMESPEC(5).
           MOVE "Nutrition" TO NAMESPEC(6).
           MOVE "Physiotherapy" TO NAMESPEC(7).
           MOVE "Traumatology" TO NAMESPEC(8).
           MOVE "Internal Medicine" TO NAMESPEC(9).

      *>   Genero doctores random
           MOVE "Isabella  " TO RNDNAME(1)
           MOVE "Carlos    " TO RNDNAME(2)
           MOVE "Sofia     " TO RNDNAME(3)
           MOVE "Miguel    " TO RNDNAME(4)
           MOVE "Gabriela  " TO RNDNAME(5)
           MOVE "Alejandro " TO RNDNAME(6)
           MOVE "Elena     " TO RNDNAME(7)
           MOVE "Diego     " TO RNDNAME(8)
           MOVE "Alicia    " TO RNDNAME(9)
           MOVE "Rafael    " TO RNDNAME(10)

           PERFORM VARYING IDXSPEC FROM 1 BY 1 UNTIL IDXSPEC>9
               PERFORM VARYING IDXDOCT FROM 1 BY 1 UNTIL IDXDOCT>3
                   MOVE SPACES TO AUX-MSG
                       COMPUTE IDXRND = FUNCTION RANDOM() * 10 + 1
                       COMPUTE AUX-99 = FUNCTION RANDOM() * 9 + 1
                       STRING  RNDNAME(IDXRND) DELIMITED BY SPACE
                               " " DELIMITED BY SIZE
                               RNDNAME(AUX-99) DELIMITED BY SPACE
                               INTO AUX-MSG
                   MOVE AUX-MSG TO DOCNAME(IDXSPEC,IDXDOCT)
               END-PERFORM
           END-PERFORM.

           MOVE "10:00" TO TIME-SLOT(1).
           MOVE "10:30" TO TIME-SLOT(2).
           MOVE "11:00" TO TIME-SLOT(3).
           MOVE "11:30" TO TIME-SLOT(4).
           MOVE "16:00" TO TIME-SLOT(5).
           MOVE "16:30" TO TIME-SLOT(6).
           MOVE "17:00" TO TIME-SLOT(7).
           MOVE "17:30" TO TIME-SLOT(8).
           EXIT.


      *>   *************************************************************
      *>   Menu carga de entradas y salidas.
      *>   Cada entrada/salida tiene una descripcion y un monto
      *>   La categoria [1] es Ingreso de dinero y las demas Salidas.
      *>   *************************************************************
       MENU-START.
           PERFORM CLEAR-SCREEN.

           DISPLAY "APPOINTMENTS"     LINE 4 COL 5.

      *>   Muestro las citas guardadas:
           PERFORM SHOW-APPOINTMENTS.

      *>   Eleccion Especialidad
           DISPLAY "Choose specialty: " LINE 6 COL 5.
           MOVE 8 TO AUX-LINE-POS.
           PERFORM VARYING AUX-99 FROM 1 BY 1 UNTIL AUX-99>9

               DISPLAY AUX-99                  LINE AUX-LINE-POS COL 5
               DISPLAY "["                     LINE AUX-LINE-POS COL 5
               DISPLAY "]"                     LINE AUX-LINE-POS COL 7
               DISPLAY NAMESPEC(AUX-99)        LINE AUX-LINE-POS COL 10
               ADD 1 TO AUX-LINE-POS
               END-PERFORM.

           DISPLAY "[0] Quit system."          LINE 18  COL 5.
           DISPLAY "Your choice: "             LINE 20  COL 5.
       CHOOSE-SPEC.
           ACCEPT CHOICE-SPEC                  LINE 20  COL 18 PROMPT.

           IF CHOICE-SPEC = 0
                   STOP RUN.

       *>   Verifico que no haya otra cita de la misma especialidad

           PERFORM VARYING IDXAPP FROM 1 BY 1 UNTIL IDXAPP>3
               MOVE APP-IDXSPC(IDXAPP) TO AUX-9
               IF APP-IDXSPC(IDXAPP) = CHOICE-SPEC THEN
               MOVE
                 "You have already an appointment of the same specialty"
               TO  AUX-MSG
               PERFORM SHOW-INFO-MSG
               GO TO  CHOOSE-SPEC
           END-PERFORM


      *>   Eleccion Doctor
           DISPLAY "Choose Doctor: " LINE 6 COL 45.
           MOVE 8 TO AUX-LINE-POS.
           PERFORM VARYING AUX-9 FROM 1 BY 1 UNTIL AUX-9>3
               DISPLAY AUX-9                  LINE AUX-LINE-POS COL 46
               DISPLAY "["                     LINE AUX-LINE-POS COL 45
               DISPLAY "]"                     LINE AUX-LINE-POS COL 47
               DISPLAY DOCNAME(CHOICE-SPEC,AUX-9)
                                               LINE AUX-LINE-POS COL 50
               ADD 1 TO AUX-LINE-POS
               END-PERFORM.

           DISPLAY "Your choice: " LINE 13 COL 45.
       CHOOSE-DOCT.
           ACCEPT CHOICE-DOCT  LINE 13 COL 58 PROMPT.
           IF CHOICE-DOCT = 0 THEN
               MOVE "Invalid options. Choose Doctor again." TO  AUX-MSG
               PERFORM SHOW-INFO-MSG
               GO TO CHOOSE-DOCT
               .


       *>  Horarios:
           DISPLAY "Choose Time Slot: " LINE 6 COL 85.
           MOVE 8 TO AUX-LINE-POS.
           PERFORM VARYING AUX-9 FROM 1 BY 1 UNTIL AUX-9>8
               DISPLAY AUX-9                   LINE AUX-LINE-POS COL 86
               DISPLAY "["                     LINE AUX-LINE-POS COL 85
               DISPLAY "]"                     LINE AUX-LINE-POS COL 87
               DISPLAY TIME-SLOT(AUX-9)        LINE AUX-LINE-POS COL 90
               ADD 1 TO AUX-LINE-POS
           END-PERFORM.
           ADD 2 TO AUX-LINE-POS.
           DISPLAY "Your choice: " LINE AUX-LINE-POS COL 85.
       CHOOSE-TIMESLOT.
           ACCEPT CHOICE-HHMM  LINE AUX-LINE-POS COL 100 PROMPT.
           IF CHOICE-HHMM = 0 THEN
               MOVE "Invalid options. Choose Time again." TO  AUX-MSG
               PERFORM SHOW-INFO-MSG
               GO TO CHOOSE-TIMESLOT
               .



      *>   Verifico la cita
           IF COUNT-APPOINTMENTS = 3
               MOVE
                 "You have already 3 appointments."
               TO  AUX-MSG
               PERFORM SHOW-INFO-MSG
               GO TO MENU-START
               .

      *>   Finalmente Guardo cita
           ADD 1 TO COUNT-APPOINTMENTS.
           MOVE CHOICE-SPEC TO APP-IDXSPC(COUNT-APPOINTMENTS).
           MOVE CHOICE-DOCT TO APP-IDXDOC(COUNT-APPOINTMENTS).
           MOVE CHOICE-HHMM TO APP-IDXTIME(COUNT-APPOINTMENTS).


           MOVE "Appointment Save!" TO  AUX-MSG.
           PERFORM SHOW-INFO-MSG.
           GO TO MENU-START.

       MENU-END.

           EXIT.

       SHOW-APPOINTMENTS.
           MOVE 24 TO AUX-LINE-POS.
           DISPLAY "App Specialty            Doctor               HH:MM"
               LINE AUX-LINE-POS COL 25.
           PERFORM VARYING AUX-9 FROM 1 BY 1
                       UNTIL AUX-9>COUNT-APPOINTMENTS
               MOVE APP-IDXSPC(AUX-9) TO CHOICE-SPEC
               MOVE APP-IDXSPC(AUX-9) TO CHOICE-DOCT
               MOVE APP-IDXTIME(AUX-9) TO CHOICE-HHMM
               STRING  AUX-9
                       "   "   DELIMITED BY SIZE
                       NAMESPEC(CHOICE-SPEC)
                       "   "  DELIMITED BY SIZE
                       DOCNAME(CHOICE-SPEC,CHOICE-DOCT)
                       " " DELIMITED BY SIZE
                       TIME-SLOT(CHOICE-HHMM)
               INTO AUX-MSG
               ADD 1 to AUX-LINE-POS
               DISPLAY AUX-MSG LINE AUX-LINE-POS COL 25
           END-PERFORM.




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
           ACCEPT MENU-OPTION LINE 30 COL 1.

       CLEAR-INFO-SCREEN.
           DISPLAY " " LINE 21 COL 1 ERASE EOS.

       CLEAR-SCREEN.
           DISPLAY " " LINE 1 COL 1 ERASE EOS.
           DISPLAY "=====  The Valencia Hospital v1.3 ======="
                   LINE 2 COL 5 REVERSED.

       END PROGRAM 03-Medical-Appointment.
