      *================================================================*
      * PROGRAMA: ACTUALSLD.COB                                         *
      * DESCRIPCION: Actualiza saldos en el archivo maestro VSAM        *
      *              Lee nuevas transacciones y actualiza cada cuenta   *
      *              usando REWRITE - operacion critica en banca        *
      * AUTOR: JOSE DAVID ORTEGA                                        *
      * FECHA: 2026                                                     *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACTUALSLD.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *-- Archivo de nuevas transacciones --*
           SELECT ARCHIVO-TRX
               ASSIGN TO 'trx-validas.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS-TRX.

      *-- Archivo maestro VSAM --*
           SELECT MAESTRO-CUENTAS
               ASSIGN TO 'cuentas-master.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS MCU-CUENTA
               FILE STATUS IS WS-STATUS-MASTER.

      *-- Archivo de log de actualizaciones --*
           SELECT ARCHIVO-LOG
               ASSIGN TO 'log-actualizaciones.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS-LOG.

       DATA DIVISION.
       FILE SECTION.

       FD ARCHIVO-TRX.
       01 REG-TRX.
           05 TRX-CUENTA        PIC X(10).
           05 TRX-TIPO          PIC X(01).
           05 TRX-MONTO         PIC 9(10)V99.
           05 TRX-FECHA         PIC 9(08).
           05 TRX-DESCRIPCION   PIC X(30).

       FD MAESTRO-CUENTAS.
       01 REG-MAESTRO.
           05 MCU-CUENTA        PIC X(10).
           05 MCU-TITULAR       PIC X(30).
           05 MCU-SALDO         PIC S9(10)V99.
           05 MCU-TOTAL-DEB     PIC 9(10)V99.
           05 MCU-TOTAL-CRE     PIC 9(10)V99.
           05 MCU-NUM-TRX       PIC 9(06).
           05 MCU-ESTADO        PIC X(01).
           05 MCU-FECHA-ACT     PIC 9(08).

       FD ARCHIVO-LOG.
       01 REG-LOG               PIC X(100).

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS.
           05 WS-STATUS-TRX     PIC XX VALUE '00'.
           05 WS-STATUS-MASTER PIC XX VALUE '00'.
           05 WS-STATUS-LOG    PIC XX VALUE '00'.

       01 WS-EOF                PIC X VALUE 'N'.

       01 WS-CONTADORES.
           05 WS-CONT-TRX           PIC 9(6) VALUE ZEROS.
           05 WS-CONT-ACTUALIZADOS  PIC 9(6) VALUE ZEROS.
           05 WS-CONT-NO-MAESTRO    PIC 9(6) VALUE ZEROS.
           05 WS-CONT-ERRORES       PIC 9(6) VALUE ZEROS.

       01 WS-SALDO-ANTERIOR     PIC S9(10)V99 VALUE ZEROS.
       01 WS-SALDO-NUEVO        PIC S9(10)V99 VALUE ZEROS.
       01 WS-FECHA-HOY          PIC 9(8)      VALUE 20260513.

       01 WS-ED-SALDO-ANT        PIC -Z,ZZZ,ZZZ,ZZ9.99.
       01 WS-ED-SALDO-NVO        PIC -Z,ZZZ,ZZZ,ZZ9.99.
       01 WS-ED-MONTO            PIC Z,ZZZ,ZZZ,ZZ9.99.

       01 WS-LINEA-LOG          PIC X(100).

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INICIAR
           PERFORM 2000-PROCESAR UNTIL WS-EOF = 'S'
           PERFORM 3000-FINALIZAR
           STOP RUN.

      *----------------------------------------------------------------*
       1000-INICIAR.
           OPEN INPUT ARCHIVO-TRX
           IF WS-STATUS-TRX NOT = '00'
               DISPLAY '[ERROR] No se pudo abrir trx-validas.dat'
               DISPLAY '[ERROR] Codigo: ' WS-STATUS-TRX
               STOP RUN
           END-IF

           OPEN I-O MAESTRO-CUENTAS
           IF WS-STATUS-MASTER NOT = '00'
               DISPLAY '[ERROR] No se pudo abrir cuentas-master.dat'
               DISPLAY '[ERROR] Codigo: ' WS-STATUS-MASTER
               STOP RUN
           END-IF

           OPEN OUTPUT ARCHIVO-LOG
           IF WS-STATUS-LOG NOT = '00'
           DISPLAY '[ERROR] No se pudo crear log-actualizaciones.dat'
               STOP RUN
           END-IF

           READ ARCHIVO-TRX
               AT END MOVE 'S' TO WS-EOF
           END-READ
           IF WS-EOF = 'S'
               DISPLAY '[ERROR] Archivo trx-validas.dat vacio'
               STOP RUN
           END-IF

           DISPLAY '================================================'
           DISPLAY '   ACTUALIZACION DE SALDOS - VSAM REWRITE       '
           DISPLAY '================================================'.

      *----------------------------------------------------------------*
       2000-PROCESAR.
           ADD 1 TO WS-CONT-TRX
           PERFORM 2100-LEER-MAESTRO

           READ ARCHIVO-TRX
               AT END MOVE 'S' TO WS-EOF
           END-READ.

      *----------------------------------------------------------------*
       2100-LEER-MAESTRO.
           MOVE TRX-CUENTA TO MCU-CUENTA
           READ MAESTRO-CUENTAS
               INVALID KEY
                   CONTINUE
           END-READ
           IF WS-STATUS-MASTER = '23'
               DISPLAY '[AVISO] Cuenta no existe: ' TRX-CUENTA
               ADD 1 TO WS-CONT-NO-MAESTRO
           ELSE
               PERFORM 2200-ACTUALIZAR-SALDO
           END-IF.

      *----------------------------------------------------------------*
       2200-ACTUALIZAR-SALDO.
           MOVE MCU-SALDO TO WS-SALDO-ANTERIOR

      *-- Aplicar la transaccion al saldo --*
           IF TRX-TIPO = 'D'
               SUBTRACT TRX-MONTO FROM MCU-SALDO
               ADD TRX-MONTO TO MCU-TOTAL-DEB
           ELSE
               ADD TRX-MONTO TO MCU-SALDO
               ADD TRX-MONTO TO MCU-TOTAL-CRE
           END-IF

           ADD 1 TO MCU-NUM-TRX
           MOVE WS-FECHA-HOY TO MCU-FECHA-ACT

      *-- Actualizar estado segun nuevo saldo --*
           EVALUATE TRUE
               WHEN MCU-SALDO > ZEROS
                   MOVE 'A' TO MCU-ESTADO
               WHEN MCU-SALDO < ZEROS
                   MOVE 'N' TO MCU-ESTADO
               WHEN OTHER
                   MOVE 'C' TO MCU-ESTADO
           END-EVALUATE

           MOVE MCU-SALDO TO WS-SALDO-NUEVO

      *-- Grabar el registro actualizado --*
           REWRITE REG-MAESTRO
           IF WS-STATUS-MASTER = '00'
               ADD 1 TO WS-CONT-ACTUALIZADOS
               PERFORM 2300-GRABAR-LOG
           ELSE
               DISPLAY '[ERROR] REWRITE fallido: ' TRX-CUENTA
               DISPLAY '[ERROR] Codigo: ' WS-STATUS-MASTER
               ADD 1 TO WS-CONT-ERRORES
           END-IF.

      *----------------------------------------------------------------*
       2300-GRABAR-LOG.
           MOVE WS-SALDO-ANTERIOR TO WS-ED-SALDO-ANT
           MOVE WS-SALDO-NUEVO    TO WS-ED-SALDO-NVO
           MOVE TRX-MONTO         TO WS-ED-MONTO

           STRING
               TRX-CUENTA        DELIMITED BY SIZE
               ' | '             DELIMITED BY SIZE
               TRX-TIPO          DELIMITED BY SIZE
               ' | '             DELIMITED BY SIZE
               WS-ED-MONTO       DELIMITED BY SIZE
               ' | ANT: '        DELIMITED BY SIZE
               WS-ED-SALDO-ANT   DELIMITED BY SIZE
               ' | NVO: '        DELIMITED BY SIZE
               WS-ED-SALDO-NVO   DELIMITED BY SIZE
               INTO WS-LINEA-LOG
           END-STRING

           WRITE REG-LOG FROM WS-LINEA-LOG.

      *----------------------------------------------------------------*
       3000-FINALIZAR.
           DISPLAY '================================================'
           DISPLAY '   RESUMEN DE ACTUALIZACION                     '
           DISPLAY '================================================'
           DISPLAY '  Transacciones procesadas : ' WS-CONT-TRX
           DISPLAY '  Cuentas actualizadas     : ' WS-CONT-ACTUALIZADOS
           DISPLAY '  Sin maestro              : ' WS-CONT-NO-MAESTRO
           DISPLAY '  Errores de actualizacion : ' WS-CONT-ERRORES
           DISPLAY '================================================'
           DISPLAY '  Log generado: log-actualizaciones.dat         '
           DISPLAY '================================================'
           CLOSE ARCHIVO-TRX
           CLOSE MAESTRO-CUENTAS
           CLOSE ARCHIVO-LOG.
