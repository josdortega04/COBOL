      *================================================================*
      * PROGRAMA: CALCSALD.COB                                         *
      * DESCRIPCION: Calcula saldos por cuenta bancaria                *
      * AUTOR: JOSE DAVID ORTEGA                                       *
      * FECHA: 2026                                                    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCSALD.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-TRANSACCIONES ASSIGN TO 'trx-validas.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARCHIVO-SALDOS ASSIGN TO 'saldos.dat'
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

       FD ARCHIVO-SALDOS.
       01 REG-SALDO.
           05 SAL-CUENTA        PIC X(10).
           05 SAL-TOTAL-DEB     PIC 9(10)V99.
           05 SAL-TOTAL-CRE     PIC 9(10)V99.
           05 SAL-SALDO-FINAL   PIC S9(10)V99.
           05 SAL-NUM-TRX       PIC 9(06).

       WORKING-STORAGE SECTION.
       01 WS-EOF                PIC X VALUE 'N'.
       01 WS-CUENTA-ANT         PIC X(10) VALUE SPACES.
       01 WS-CONT-CUENTAS       PIC 9(6)  VALUE 0.
       01 WS-CONT-TRX           PIC 9(6)  VALUE 0.

       01 WS-ACUM.
           05 WS-TOT-DEBITOS    PIC 9(10)V99 VALUE ZEROS.
           05 WS-TOT-CREDITOS   PIC 9(10)V99 VALUE ZEROS.
           05 WS-SALDO          PIC S9(10)V99 VALUE ZEROS.
           05 WS-NUM-TRX        PIC 9(6)      VALUE ZEROS.

       01 WS-SALDO-DISPLAY.
           05 FILLER            PIC X(15) VALUE 'Saldo final   :'.
           05 WS-SALDO-OUT      PIC -ZZZ,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INICIAR
           PERFORM 2000-PROCESAR UNTIL WS-EOF = 'S'
           PERFORM 3000-CERRAR-ULTIMA-CUENTA
           PERFORM 4000-FINALIZAR
           STOP RUN.

       1000-INICIAR.
           OPEN INPUT  ARCHIVO-TRANSACCIONES
           OPEN OUTPUT ARCHIVO-SALDOS
           READ ARCHIVO-TRANSACCIONES
               AT END MOVE 'S' TO WS-EOF
           END-READ.

       2000-PROCESAR.
           ADD 1 TO WS-CONT-TRX

           IF WS-CUENTA-ANT = SPACES
               MOVE TRX-CUENTA TO WS-CUENTA-ANT
           END-IF

           IF TRX-CUENTA NOT = WS-CUENTA-ANT
               PERFORM 2100-GRABAR-SALDO
               PERFORM 2200-RESETEAR-ACUMULADORES
               MOVE TRX-CUENTA TO WS-CUENTA-ANT
           END-IF

           PERFORM 2300-ACUMULAR-TRANSACCION

           READ ARCHIVO-TRANSACCIONES
               AT END MOVE 'S' TO WS-EOF
           END-READ.

       2100-GRABAR-SALDO.
           MOVE WS-CUENTA-ANT    TO SAL-CUENTA
           MOVE WS-TOT-DEBITOS   TO SAL-TOTAL-DEB
           MOVE WS-TOT-CREDITOS  TO SAL-TOTAL-CRE
           COMPUTE WS-SALDO = WS-TOT-CREDITOS - WS-TOT-DEBITOS
           MOVE WS-SALDO         TO SAL-SALDO-FINAL
           MOVE WS-NUM-TRX       TO SAL-NUM-TRX
           WRITE REG-SALDO
           ADD 1 TO WS-CONT-CUENTAS.

       2200-RESETEAR-ACUMULADORES.
           MOVE ZEROS TO WS-TOT-DEBITOS
           MOVE ZEROS TO WS-TOT-CREDITOS
           MOVE ZEROS TO WS-SALDO
           MOVE ZEROS TO WS-NUM-TRX.

       2300-ACUMULAR-TRANSACCION.
           ADD 1 TO WS-NUM-TRX
           IF TRX-TIPO = 'D'
               ADD TRX-MONTO TO WS-TOT-DEBITOS
           ELSE
               ADD TRX-MONTO TO WS-TOT-CREDITOS
           END-IF.

       3000-CERRAR-ULTIMA-CUENTA.
           IF WS-CUENTA-ANT NOT = SPACES
               PERFORM 2100-GRABAR-SALDO
           END-IF.

       4000-FINALIZAR.
           MOVE WS-SALDO TO WS-SALDO-OUT
           DISPLAY "=================================="
           DISPLAY "   RESUMEN DE SALDOS              "
           DISPLAY "=================================="
           DISPLAY "  Cuentas procesadas: " WS-CONT-CUENTAS
           DISPLAY "  Transacciones    : " WS-CONT-TRX
           DISPLAY "=================================="
           CLOSE ARCHIVO-TRANSACCIONES
           CLOSE ARCHIVO-SALDOS.
