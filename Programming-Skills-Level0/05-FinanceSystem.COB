      ******************************************************************
      * Author: gottdammer
      * Date:
      * Purpose:
      *>   * 5 - Personal financial management
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 05-FINANCESYSTEM.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  WS-MENU-OPTION PIC 9.

       01  TOTAL-ENTRADAS      PIC 9(9)V99 VALUE 0.
       01  TOTAL-ENTRADAS-Z    PIC -(8)9.99 VALUE "0.00".
       01  TOTAL-SALIDAS       PIC 9(9)V99 VALUE 0.
       01  TOTAL-SALIDAS-Z     PIC -(8)9.99 VALUE "0.00".
       01  TOTAL-BALANCE       PIC 9(9)V99 VALUE 0.
       01  TOTAL-BALANCE-Z       PIC -(8)9.99 VALUE "0.00".

       01  MAXCOUNT-CATEGORIAS PIC 9(2) VALUE 6.
       01  CATEGORIAS.
           05  CATEGORIA  OCCURS 6 TIMES INDEXED BY IDXCAT.
      *>           NOMCAT: nombre de la categoria
               10 NOMCAT PIC X(25).
      *>           CANTDETALLES: cant detalles cargados en la categoria
               10 CANTDETALLES PIC 99 VALUE 0.
      *>           MOVIMIENTO: descripc. del detalle y el monto
               10 MOVIMIENTO OCCURS 10 TIMES INDEXED BY IDXDET.
                   15 DETALLE PIC X(35).
                   15 MONTO PIC 9(6)V99.

       01  AUX-MOV-DETALLE PIC X(35).
       01  AUX-MOV-MONTO-Z PIC Z(5)9.99.
       01  AUX-MOV-MONTO PIC 9(6)V99.

       01  AUX-COUNTER PIC 9.
       01  AUX-POS PIC 99.
       01  AUX-MSG PIC X(25).



       PROCEDURE DIVISION.
       MAIN.
           PERFORM CARGA-PARAMETROS.
           PERFORM MENU-OPT.
           STOP RUN.

      *>   *************************************************************
      *>   Carga las categorias por defecto
      *>   *************************************************************
       CARGA-PARAMETROS.
           MOVE "INGRESOS" TO NOMCAT(1).
           MOVE "GASTOS MEDICOS" TO NOMCAT(2).
           MOVE "GASTOS DE HOGAR" TO NOMCAT(3).
           MOVE "GASTOS DE RECREACION" TO NOMCAT(4).
           MOVE "GASTOS EN AHORROS" TO NOMCAT(5).
           MOVE "GASTOS EN EDUCACION" TO NOMCAT(6).





      *>   *************************************************************
      *>   Menu carga de entradas y salidas.
      *>   Cada entrada/salida tiene una descripcion y un monto
      *>   La categoria [1] es Ingreso de dinero y las demas Salidas.
      *>   *************************************************************
       MENU-OPT.
           PERFORM CLEAR-SCREEN.

           MOVE 0 TO AUX-COUNTER.
           MOVE 3 TO AUX-POS.
           PERFORM SHOW-CAT VARYING IDXCAT FROM 1 BY 1
                   UNTIL IDXCAT>MAXCOUNT-CATEGORIAS.


           MOVE TOTAL-ENTRADAS TO TOTAL-ENTRADAS-Z.
           MOVE TOTAL-SALIDAS TO TOTAL-SALIDAS-Z.
           DISPLAY "Total Ingresos: "              LINE 15 COL 5
                   TOTAL-ENTRADAS-Z                LINE 15 COL 20
                   "Total Egresos: "               LINE 16 COL 5
                   TOTAL-SALIDAS-Z                 LINE 16 COL 20
                   .
           DISPLAY "Balance: "                 LINE 17 COL 5 REVERSED
                   TOTAL-BALANCE-Z             LINE 17 COL 20
                   .

           IF TOTAL-BALANCE < 0
               STRING "Deberias gastar menos!!" INTO AUX-MSG.
           IF TOTAL-BALANCE = 0
               STRING "MSG =0" INTO AUX-MSG.
           IF TOTAL-BALANCE > 0
               STRING "Te sobra plata!" INTO AUX-MSG.

           DISPLAY AUX-MSG LINE 20 COL 5 REVERSED.

           DISPLAY "[ 9 ] Ver todos los movimientos" LINE 11 COL 5.
           DISPLAY "Opcion [ ]"                    LINE 13 COL 5.
           ACCEPT WS-MENU-OPTION                   LINE 13 COL 13.

           EVALUATE WS-MENU-OPTION
               WHEN 0
                   DISPLAY "Bye!"
                   STOP RUN
               WHEN 9
                   PERFORM SHOW-DETALLES
               WHEN OTHER
                   PERFORM MENU-INFO-CATEGORIA

           END-EVALUATE.
           GO TO MENU-OPT.



       SHOW-CAT.
           ADD 1 TO AUX-COUNTER.
           ADD 1 TO AUX-POS.
           DISPLAY "[ " LINE AUX-POS COL 5
                   AUX-COUNTER LINE AUX-POS COL 7
                   " ]" LINE AUX-POS COL 8
                   NOMCAT(IDXCAT) LINE AUX-POS COL 11.
           EXIT.

      *>   *************************************************************
      *>   Carga un detalle y un monto en la categoria selccionada.
      *>   *************************************************************
       MENU-INFO-CATEGORIA.
           PERFORM CLEAR-SCREEN.
           MOVE 0.00 TO AUX-MOV-MONTO-Z.
           DISPLAY
                   "Nuevo movimiento en la categoria " LINE 3 COL 5
                   NOMCAT(WS-MENU-OPTION) LINE 3 COL 40
                   "Detalles del movimiento: " LINE 5 COL 5
                   "Monto del movimiento: " LINE 6 COL 5
                   AUX-MOV-MONTO-Z LINE 6 COL 30.

           ACCEPT AUX-MOV-DETALLE LINE 5 COL 30 PROMPT.
           ACCEPT AUX-MOV-MONTO-Z LINE 6 COL 30 UPDATE.

           MOVE AUX-MOV-MONTO-Z TO AUX-MOV-MONTO.

      *>   Incremento la cant de detalles en la categoria
           ADD 1 TO CANTDETALLES(WS-MENU-OPTION).

      *>   Guardo los detalles dentro de su categoria
           MOVE AUX-MOV-DETALLE TO
               DETALLE(WS-MENU-OPTION,CANTDETALLES(WS-MENU-OPTION)).
           MOVE AUX-MOV-MONTO TO
               MONTO(WS-MENU-OPTION,CANTDETALLES(WS-MENU-OPTION)).

           IF WS-MENU-OPTION = 1 THEN
               ADD AUX-MOV-MONTO TO TOTAL-ENTRADAS
           ELSE
               ADD AUX-MOV-MONTO TO TOTAL-SALIDAS
           END-IF.

           COMPUTE TOTAL-BALANCE = TOTAL-ENTRADAS - TOTAL-SALIDAS.
           MOVE TOTAL-BALANCE TO TOTAL-BALANCE-Z.

           PERFORM MENU-DISPLAYCONTINUE.

       SHOW-DETALLES.
           PERFORM CLEAR-SCREEN.
           MOVE 2 TO AUX-POS.
           PERFORM SHOW-CATEGORIAS VARYING IDXCAT FROM 1 BY 1
                   UNTIL IDXCAT>MAXCOUNT-CATEGORIAS.
           PERFORM MENU-DISPLAYCONTINUE.
           EXIT.

       SHOW-CATEGORIAS.
           ADD 1 TO AUX-POS.
           DISPLAY NOMCAT(IDXCAT) LINE AUX-POS COL 5.
           PERFORM SHOW-MOVIMIENTOS VARYING IDXDET FROM 1 BY 1
                   UNTIL IDXDET>CANTDETALLES(IDXCAT).
           EXIT.

       SHOW-MOVIMIENTOS.
           ADD 1 TO AUX-POS.
           MOVE MONTO(IDXCAT,IDXDET) TO AUX-MOV-MONTO-Z.
           DISPLAY DETALLE(IDXCAT,IDXDET) LINE AUX-POS COL 10
                   AUX-MOV-MONTO-Z LINE AUX-POS COL 45
                   .
           EXIT.

       MENU-END.
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
           DISPLAY "=====  PersonalFinance 1.0 ======="
                   LINE 2 COL 5 REVERSED.

       END PROGRAM 05-FINANCESYSTEM.
