      *================================================================*
      * PROGRAMA: GENREPORT.COB                                        *
      * DESCRIPCION: Modulo de reporteria financiera                   *
      *              - Totales por tipo de transaccion                 *
      *              - Resumen por cuenta con saldo final              *
      *              - Resumen ejecutivo general                       *
      * AUTOR: JOSE DAVID ORTEGA                                       *
      * FECHA: 2026                                                    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GENREPORT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-SALDOS ASSIGN TO 'saldos.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARCHIVO-REPORTE ASSIGN TO 'reporte.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD ARCHIVO-SALDOS.
       01 REG-SALDO.
           05 SAL-CUENTA        PIC X(10).
           05 SAL-TOTAL-DEB     PIC 9(10)V99.
           05 SAL-TOTAL-CRE     PIC 9(10)V99.
           05 SAL-SALDO-FINAL   PIC S9(10)V99.
           05 SAL-NUM-TRX       PIC 9(06).

       FD ARCHIVO-REPORTE.
       01 REG-LINEA             PIC X(80).

       WORKING-STORAGE SECTION.
       01 WS-EOF                PIC X VALUE 'N'.

      *-- Acumuladores generales --*
       01 WS-TOTALES.
           05 WS-TOT-CUENTAS    PIC 9(6)      VALUE ZEROS.
           05 WS-TOT-TRX        PIC 9(8)      VALUE ZEROS.
           05 WS-TOT-DEBITOS    PIC 9(12)V99  VALUE ZEROS.
           05 WS-TOT-CREDITOS   PIC 9(12)V99  VALUE ZEROS.
           05 WS-TOT-SALDO      PIC S9(12)V99 VALUE ZEROS.
           05 WS-CONT-POSITIVOS PIC 9(6)      VALUE ZEROS.
           05 WS-CONT-NEGATIVOS PIC 9(6)      VALUE ZEROS.
           05 WS-CONT-NEUTROS   PIC 9(6)      VALUE ZEROS.

      *-- Lineas de reporte --*
       01 WS-LINEA              PIC X(80) VALUE SPACES.
       01 WS-SEPARADOR          PIC X(80)
           VALUE '================================================'.

      *-- Campos de edicion para display --*
       01 WS-ED-DEBITOS         PIC Z,ZZZ,ZZZ,ZZ9.99.
       01 WS-ED-CREDITOS        PIC Z,ZZZ,ZZZ,ZZ9.99.
       01 WS-ED-SALDO           PIC -Z,ZZZ,ZZZ,ZZ9.99.
       01 WS-ED-TOT-DEB         PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99.
       01 WS-ED-TOT-CRE         PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99.
       01 WS-ED-TOT-SALDO       PIC -Z,ZZZ,ZZZ,ZZZ,ZZ9.99.
       01 WS-ED-NUM-TRX         PIC ZZZ,ZZ9.
       01 WS-ED-CUENTAS         PIC ZZZ,ZZ9.
       01 WS-ED-TOT-TRX         PIC ZZZ,ZZZ,ZZ9.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INICIAR
           PERFORM 2000-ENCABEZADO-REPORTE
           PERFORM 3000-DETALLE-CUENTAS UNTIL WS-EOF = 'S'
           PERFORM 4000-RESUMEN-EJECUTIVO
           PERFORM 5000-FINALIZAR
           STOP RUN.

      *----------------------------------------------------------------*
       1000-INICIAR.
           OPEN INPUT  ARCHIVO-SALDOS
           OPEN OUTPUT ARCHIVO-REPORTE
           READ ARCHIVO-SALDOS
               AT END MOVE 'S' TO WS-EOF
           END-READ
           IF WS-EOF = 'S'
               DISPLAY 'ERROR: Archivo de saldos vacio'
               STOP RUN
           END-IF.

      *----------------------------------------------------------------*
       2000-ENCABEZADO-REPORTE.
           MOVE '        FECHA: 13/05/2026' TO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA
           
           WRITE REG-LINEA FROM WS-SEPARADOR
           MOVE '        BANCO SISTEMA - REPORTE FINANCIERO'
               TO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA
           MOVE '        DETALLE DE SALDOS Y MOVIMIENTOS POR CUENTA'
               TO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA
           WRITE REG-LINEA FROM WS-SEPARADOR
           MOVE SPACES TO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA

           MOVE '  CUENTA      DEBITOS           CREDITOS' TO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA
           MOVE '              SALDO FINAL        NUM.TRX ' TO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA
           MOVE '------------------------------------------------'
               TO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA.

      *----------------------------------------------------------------*
       3000-DETALLE-CUENTAS.
           ADD 1                TO WS-TOT-CUENTAS
           ADD SAL-NUM-TRX      TO WS-TOT-TRX
           ADD SAL-TOTAL-DEB    TO WS-TOT-DEBITOS
           ADD SAL-TOTAL-CRE    TO WS-TOT-CREDITOS
           ADD SAL-SALDO-FINAL  TO WS-TOT-SALDO

           EVALUATE TRUE
               WHEN SAL-SALDO-FINAL > ZERO
                   ADD 1 TO WS-CONT-POSITIVOS
               WHEN SAL-SALDO-FINAL < ZERO
                   ADD 1 TO WS-CONT-NEGATIVOS
               WHEN OTHER
                   ADD 1 TO WS-CONT-NEUTROS
           END-EVALUATE

           MOVE SAL-TOTAL-DEB   TO WS-ED-DEBITOS
           MOVE SAL-TOTAL-CRE   TO WS-ED-CREDITOS
           MOVE SAL-SALDO-FINAL TO WS-ED-SALDO
           MOVE SAL-NUM-TRX     TO WS-ED-NUM-TRX

           STRING '  ' DELIMITED SIZE
                  SAL-CUENTA DELIMITED SIZE
                  '  ' DELIMITED SIZE
                  WS-ED-DEBITOS DELIMITED SIZE
                  '  ' DELIMITED SIZE
                  WS-ED-CREDITOS DELIMITED SIZE
               INTO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA

           STRING '              ' DELIMITED SIZE
                  WS-ED-SALDO DELIMITED SIZE
                  '      ' DELIMITED SIZE
                  WS-ED-NUM-TRX DELIMITED SIZE
               INTO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA

           MOVE SPACES TO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA

           READ ARCHIVO-SALDOS
               AT END MOVE 'S' TO WS-EOF
           END-READ.

      *----------------------------------------------------------------*
       4000-RESUMEN-EJECUTIVO.
           MOVE WS-TOT-DEBITOS  TO WS-ED-TOT-DEB
           MOVE WS-TOT-CREDITOS TO WS-ED-TOT-CRE
           MOVE WS-TOT-SALDO    TO WS-ED-TOT-SALDO
           MOVE WS-TOT-CUENTAS  TO WS-ED-CUENTAS
           MOVE WS-TOT-TRX      TO WS-ED-TOT-TRX

           WRITE REG-LINEA FROM WS-SEPARADOR
           MOVE '        RESUMEN EJECUTIVO GENERAL' TO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA
           WRITE REG-LINEA FROM WS-SEPARADOR

           STRING '  Total cuentas procesadas : '
                  DELIMITED SIZE
                  WS-ED-CUENTAS DELIMITED SIZE
               INTO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA

           STRING '  Total transacciones      : '
                  DELIMITED SIZE
                  WS-ED-TOT-TRX DELIMITED SIZE
               INTO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA

           MOVE SPACES TO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA

           MOVE '  --- TOTALES POR TIPO DE TRANSACCION ---'
               TO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA

           STRING '  Total debitos            : '
                  DELIMITED SIZE
                  WS-ED-TOT-DEB DELIMITED SIZE
               INTO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA

           STRING '  Total creditos           : '
                  DELIMITED SIZE
                  WS-ED-TOT-CRE DELIMITED SIZE
               INTO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA

           STRING '  Saldo neto consolidado   : '
                  DELIMITED SIZE
                  WS-ED-TOT-SALDO DELIMITED SIZE
               INTO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA

           MOVE SPACES TO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA

           MOVE '  --- CLASIFICACION DE CUENTAS ---' TO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA

           STRING '  Cuentas con saldo positivo: '
                  DELIMITED SIZE
                  WS-CONT-POSITIVOS DELIMITED SIZE
               INTO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA

           STRING '  Cuentas con saldo negativo: '
                  DELIMITED SIZE
                  WS-CONT-NEGATIVOS DELIMITED SIZE
               INTO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA

           STRING '  Cuentas en cero           : '
                  DELIMITED SIZE
                  WS-CONT-NEUTROS DELIMITED SIZE
               INTO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA

           WRITE REG-LINEA FROM WS-SEPARADOR

      *-- Tambien mostrar en pantalla --*
           DISPLAY "================================================"
           DISPLAY "        RESUMEN EJECUTIVO GENERAL               "
           DISPLAY "================================================"
           DISPLAY "  Cuentas procesadas  : " WS-ED-CUENTAS
           DISPLAY "  Total transacciones : " WS-ED-TOT-TRX
           DISPLAY "  Total debitos       : " WS-ED-TOT-DEB
           DISPLAY "  Total creditos      : " WS-ED-TOT-CRE
           DISPLAY "  Saldo neto          : " WS-ED-TOT-SALDO
           DISPLAY "  Cuentas positivas   : " WS-CONT-POSITIVOS
           DISPLAY "  Cuentas negativas   : " WS-CONT-NEGATIVOS
           DISPLAY "  Cuentas en cero     : " WS-CONT-NEUTROS
           DISPLAY "================================================"
           DISPLAY "  Reporte generado en : reporte.txt             "
           DISPLAY "================================================".

      *----------------------------------------------------------------*
       5000-FINALIZAR.
           CLOSE ARCHIVO-SALDOS
           CLOSE ARCHIVO-REPORTE.
