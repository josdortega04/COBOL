      *========================================================================*
                                                                         
      *  PROGRAMA: CARGTRX.COB                                                 *
                                                                        
      *  DESCRIPCION: Carga y valida transacciones bancarias                   *

      *  AUTOR: JOSE DAVID ORTEGA                                              *
                                             
      *  FECHA: 13/05/2026                                                     *
      *========================================================================*
                   
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CARGTRX.
       
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       
           SELECT ARCHIVO-ENTRADA ASSIGN TO 'transacciones.dat'
               ORGANIZATION IS LINE SEQUENTIAL
                FILE STATUS IS WS-STATUS-ENT.

           SELECT ARCHIVO-VALIDAS ASSIGN TO 'trx-validas.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS-VAL.

           SELECT ARCHIVO-ERRORES  ASSIGN TO 'trx-errores.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS-ERR. 
       
       
       DATA DIVISION.
       FILE SECTION.


       FD ARCHIVO-ENTRADA.
       01 REG-ENTRADA.
          05 ENT-CUENTA          PIC X(10).
          05 ENT-TIPO            PIC X(01).
          05 ENT-MONTO           PIC 9(10)V99.
          05 ENT-FECHA           PIC 9(08).
          05 ENT-DESCRIPCION     PIC X(50).

       FD ARCHIVO-VALIDAS.
       01 REG-VALIDA         PIC X(61).


       FD ARCHIVO-ERRORES.
       01 REG-ERRORES         PIC X(80).



       WORKING-STORAGE SECTION.

        01 WS-FILE-STATUS.
           05 WS-STATUS-ENT     PIC XX VALUE '00'.
           05 WS-STATUS-VAL     PIC XX VALUE '00'.
           05 WS-STATUS-ERR     PIC XX VALUE '00'.

       01 WS-EOF                 PIC X VALUE 'N'.
       01 WS-CONT-LEIDOS         PIC 9(6) VALUE 0.
       01 WS-CONT-VALIDOS        PIC 9(6) VALUE 0.
       01 WS-CONT-ERRORES        PIC 9(6) VALUE 0.
       01 WS-ERROR-DESC          PIC X(30).
       01 WS-LINEA-ERROR.
           05 WS-ERR-DATOS        PIC X(61).
           05 FILLER              PIC X  VALUE ' '.
           05 WS-ERR-MSG          PIC X(18).

       PROCEDURE DIVISION.

       0000-PRINCIPAL.
           PERFORM 1000-INICIAR
           PERFORM 2000-PROCESAR UNTIL WS-EOF ='S'
           PERFORM 3000-FINALIZAR
           STOP RUN.

       1000-INICIAR.
           OPEN INPUT ARCHIVO-ENTRADA
            IF WS-STATUS-ENT NOT = '00'
               DISPLAY '==============================================='
               DISPLAY '[ERROR] No se pudo abrir transacciones.dat'
               DISPLAY '[ERROR] Codigo: ' WS-STATUS-ENT
               DISPLAY '==============================================='
               STOP RUN
           END-IF

           OPEN OUTPUT ARCHIVO-VALIDAS
           IF WS-STATUS-VAL NOT = '00'
                   DISPLAY '[ERROR] No se pudo crear trx-validas.dat'
                   DISPLAY '[ERROR] Codigo: ' WS-STATUS-VAL
                   STOP RUN
           END-IF
           
           OPEN OUTPUT ARCHIVO-ERRORES
            IF WS-STATUS-ERR NOT = '00'
                   DISPLAY '[ERROR] No se pudo crear trx-errores.dat'
                   DISPLAY '[ERROR] Codigo: ' WS-STATUS-ERR
                   STOP RUN
           END-IF
           READ ARCHIVO-ENTRADA AT END MOVE 'S' TO WS-EOF

           END-READ
           IF WS-EOF = 'S'
                   DISPLAY '[ERROR] Archivo de transacciones vacio'
                   STOP RUN
           END-IF.

       2000-PROCESAR.
           ADD 1 TO WS-CONT-LEIDOS
           PERFORM 2100-VALIDAR
           READ ARCHIVO-ENTRADA AT END MOVE 'S' TO WS-EOF.

       2100-VALIDAR.
           MOVE SPACE TO WS-ERROR-DESC

           EVALUATE TRUE
               WHEN ENT-CUENTA = SPACES
                   MOVE 'CUENTA INVALIDA' TO WS-ERROR-DESC
               WHEN ENT-TIPO NOT = 'D' AND ENT-TIPO NOT = 'C'
                   MOVE 'TIPO INCORRECTO' TO WS-ERROR-DESC
               WHEN ENT-MONTO = ZEROS
                   MOVE 'MONTO CERO' TO WS-ERROR-DESC
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF WS-ERROR-DESC = SPACES
               WRITE REG-VALIDA FROM REG-ENTRADA
               ADD 1 TO WS-CONT-VALIDOS
           ELSE
               MOVE REG-ENTRADA TO WS-ERR-DATOS
               MOVE WS-ERROR-DESC TO WS-ERR-MSG
               WRITE REG-ERRORES FROM WS-LINEA-ERROR
               ADD 1 TO WS-CONT-ERRORES
           END-IF.

       3000-FINALIZAR.
           DISPLAY "================================================="
           DISPLAY "   RESUMEN DE CARGA                             "
           DISPLAY "================================================="
           DISPLAY "  Registros leidos  : " WS-CONT-LEIDOS "        "
           DISPLAY "  Registros validos : " WS-CONT-VALIDOS "       "
           DISPLAY "  Registros error   : " WS-CONT-ERRORES "       "
           DISPLAY "================================================"
           CLOSE ARCHIVO-ENTRADA
           CLOSE ARCHIVO-VALIDAS
           CLOSE ARCHIVO-ERRORES.



      