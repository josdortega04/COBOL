      *================================================================*
      * PROGRAMA: DETECTAN.COB                                         *
      * DESCRIPCION: Deteccion de transacciones anomalas               *
      *              - Montos superiores al limite configurado          *
      *              - Multiples debitos consecutivos                  *
      *              - Cuentas con saldo negativo critico              *
      *              - Transacciones duplicadas                        *
      * AUTOR: JOSE DAVID ORTEGA                                       *
      * FECHA: 2026                                                    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DETECTAN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-TRANSACCIONES ASSIGN TO 'trx-validas.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARCHIVO-ANOMALAS ASSIGN TO 'anomalas.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARCHIVO-REPORTE   ASSIGN TO 'reporte-anomalas.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD ARCHIVO-TRANSACCIONES.
       01 REG-TRANSACCION.
           05 TRX-CUENTA        PIC X(10).
           05 TRX-TIPO          PIC X(01).
           05 TRX-MONTO         PIC 9(10)V99.
           05 TRX-FECHA         PIC 9(08).
           05 TRX-DESCRIPCION   PIC X(30).

       FD ARCHIVO-ANOMALAS.
       01 REG-ANOMALA.
           05 ANO-CUENTA        PIC X(10).
           05 ANO-TIPO          PIC X(01).
           05 ANO-MONTO         PIC 9(10)V99.
           05 ANO-FECHA         PIC 9(08).
           05 ANO-MOTIVO        PIC X(30).

       FD ARCHIVO-REPORTE.
       01 REG-LINEA             PIC X(80).

       WORKING-STORAGE SECTION.
       01 WS-EOF                PIC X VALUE 'N'.

      *-- Reglas de negocio configurables --*
       01 WS-REGLAS.
           05 WS-LIMITE-MONTO    PIC 9(10)V99 VALUE 5000000.00.
           05 WS-MAX-DEB-CONSEC  PIC 9(2)     VALUE 3.
           05 WS-LIMITE-NEGATIVO PIC 9(10)V99 VALUE 1000000.00.

      *-- Control de anomalias --*
       01 WS-CONTROL.
           05 WS-CUENTA-ANT       PIC X(10)    VALUE SPACES.
           05 WS-TIPO-ANT         PIC X(01)    VALUE SPACES.
           05 WS-MONTO-ANT        PIC 9(10)V99 VALUE ZEROS.
           05 WS-FECHA-ANT        PIC 9(08)    VALUE ZEROS.
           05 WS-DEB-CONSECUTIVOS PIC 9(2)     VALUE ZEROS.

      *-- Contadores --*
       01 WS-CONTADORES.
           05 WS-CONT-LEIDAS     PIC 9(6) VALUE ZEROS.
           05 WS-CONT-ANOMALAS   PIC 9(6) VALUE ZEROS.
           05 WS-CONT-MONTO      PIC 9(6) VALUE ZEROS.
           05 WS-CONT-CONSEC     PIC 9(6) VALUE ZEROS.
           05 WS-CONT-DUPLICADAS PIC 9(6) VALUE ZEROS.

      *-- Campos de edicion --*
       01 WS-ED-MONTO            PIC Z,ZZZ,ZZZ,ZZ9.99.
       01 WS-ED-LIMITE           PIC Z,ZZZ,ZZZ,ZZ9.99.
       01 WS-MOTIVO              PIC X(30).
       01 WS-LINEA               PIC X(80).
       01 WS-SEPARADOR           PIC X(80)
           VALUE '================================================'.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INICIAR
           PERFORM 2000-ENCABEZADO
           PERFORM 3000-PROCESAR UNTIL WS-EOF = 'S'
           PERFORM 4000-RESUMEN
           PERFORM 5000-FINALIZAR
           STOP RUN.

      *----------------------------------------------------------------*
       1000-INICIAR.
           OPEN INPUT  ARCHIVO-TRANSACCIONES
           OPEN OUTPUT ARCHIVO-ANOMALAS
           OPEN OUTPUT ARCHIVO-REPORTE
           READ ARCHIVO-TRANSACCIONES
               AT END MOVE 'S' TO WS-EOF
           END-READ
           IF WS-EOF = 'S'
               DISPLAY 'ERROR: Archivo de transacciones vacio'
               STOP RUN
           END-IF.

      *----------------------------------------------------------------*
       2000-ENCABEZADO.
           WRITE REG-LINEA FROM WS-SEPARADOR
           MOVE '     BANCO SISTEMA - REPORTE DE ANOMALIAS'
               TO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA
           WRITE REG-LINEA FROM WS-SEPARADOR
           MOVE '  CUENTA     TIPO  MONTO              MOTIVO'
               TO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA
           MOVE '------------------------------------------------'
               TO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA.

      *----------------------------------------------------------------*
       3000-PROCESAR.
           ADD 1 TO WS-CONT-LEIDAS
           MOVE SPACES TO WS-MOTIVO

           PERFORM 3100-VERIFICAR-MONTO
           PERFORM 3200-VERIFICAR-CONSECUTIVOS
           PERFORM 3300-VERIFICAR-DUPLICADA

           IF WS-MOTIVO NOT = SPACES
               PERFORM 3400-GRABAR-ANOMALA
           END-IF

           MOVE TRX-CUENTA TO WS-CUENTA-ANT
           MOVE TRX-TIPO   TO WS-TIPO-ANT
           MOVE TRX-MONTO  TO WS-MONTO-ANT
           MOVE TRX-FECHA  TO WS-FECHA-ANT

           READ ARCHIVO-TRANSACCIONES
               AT END MOVE 'S' TO WS-EOF
           END-READ.

      *----------------------------------------------------------------*
       3100-VERIFICAR-MONTO.
           IF TRX-MONTO > WS-LIMITE-MONTO
               MOVE 'MONTO SUPERA LIMITE       ' TO WS-MOTIVO
               ADD 1 TO WS-CONT-MONTO
           END-IF.

      *----------------------------------------------------------------*
       3200-VERIFICAR-CONSECUTIVOS.
           IF TRX-CUENTA = WS-CUENTA-ANT AND TRX-TIPO = 'D'
                                      AND WS-TIPO-ANT = 'D'
               ADD 1 TO WS-DEB-CONSECUTIVOS
               IF WS-DEB-CONSECUTIVOS >= WS-MAX-DEB-CONSEC
                   MOVE 'DEBITOS CONSECUTIVOS      ' TO WS-MOTIVO
                   ADD 1 TO WS-CONT-CONSEC
               END-IF
           ELSE
               MOVE ZEROS TO WS-DEB-CONSECUTIVOS
           END-IF.

      *----------------------------------------------------------------*
       3300-VERIFICAR-DUPLICADA.
           IF TRX-CUENTA = WS-CUENTA-ANT
              AND TRX-MONTO = WS-MONTO-ANT
              AND TRX-FECHA = WS-FECHA-ANT
              AND TRX-TIPO  = WS-TIPO-ANT
               MOVE 'POSIBLE DUPLICADA         ' TO WS-MOTIVO
               ADD 1 TO WS-CONT-DUPLICADAS
           END-IF.

      *----------------------------------------------------------------*
       3400-GRABAR-ANOMALA.
           ADD 1 TO WS-CONT-ANOMALAS
           MOVE TRX-CUENTA TO ANO-CUENTA
           MOVE TRX-TIPO   TO ANO-TIPO
           MOVE TRX-MONTO  TO ANO-MONTO
           MOVE TRX-FECHA  TO ANO-FECHA
           MOVE WS-MOTIVO  TO ANO-MOTIVO
           WRITE REG-ANOMALA

           MOVE TRX-MONTO  TO WS-ED-MONTO
           STRING '  ' DELIMITED SIZE
                  TRX-CUENTA DELIMITED SIZE
                  '  ' DELIMITED SIZE
                  TRX-TIPO DELIMITED SIZE
                  '  ' DELIMITED SIZE
                  WS-ED-MONTO DELIMITED SIZE
                  '  ' DELIMITED SIZE
                  WS-MOTIVO DELIMITED SIZE
               INTO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA.

      *----------------------------------------------------------------*
       4000-RESUMEN.
           MOVE WS-LIMITE-MONTO TO WS-ED-LIMITE
           WRITE REG-LINEA FROM WS-SEPARADOR
           MOVE '        RESUMEN DE ANOMALIAS DETECTADAS' TO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA
           WRITE REG-LINEA FROM WS-SEPARADOR

           STRING '  Transacciones analizadas : '
                  DELIMITED SIZE
                  WS-CONT-LEIDAS DELIMITED SIZE
               INTO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA

           STRING '  Total anomalias          : '
                  DELIMITED SIZE
                  WS-CONT-ANOMALAS DELIMITED SIZE
               INTO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA

           STRING '  Montos sobre limite      : '
                  DELIMITED SIZE
                  WS-CONT-MONTO DELIMITED SIZE
               INTO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA

           STRING '  Debitos consecutivos     : '
                  DELIMITED SIZE
                  WS-CONT-CONSEC DELIMITED SIZE
               INTO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA

           STRING '  Posibles duplicadas      : '
                  DELIMITED SIZE
                  WS-CONT-DUPLICADAS DELIMITED SIZE
               INTO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA

           WRITE REG-LINEA FROM WS-SEPARADOR

           DISPLAY "================================================"
           DISPLAY "     RESUMEN DE ANOMALIAS DETECTADAS            "
           DISPLAY "================================================"
           DISPLAY "  Transacciones analizadas : " WS-CONT-LEIDAS
           DISPLAY "  Total anomalias          : " WS-CONT-ANOMALAS
           DISPLAY "  Montos sobre limite      : " WS-CONT-MONTO
           DISPLAY "  Debitos consecutivos     : " WS-CONT-CONSEC
           DISPLAY "  Posibles duplicadas      : " WS-CONT-DUPLICADAS
           DISPLAY "================================================"
           DISPLAY "  Reporte generado en: reporte-anomalas.txt     "
           DISPLAY "================================================".

      *----------------------------------------------------------------*
       5000-FINALIZAR.
           CLOSE ARCHIVO-TRANSACCIONES
           CLOSE ARCHIVO-ANOMALAS
           CLOSE ARCHIVO-REPORTE.
