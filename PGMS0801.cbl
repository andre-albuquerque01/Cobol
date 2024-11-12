      ******************************************************************
      * Author: André Albuquerque Gonçalves
      * Date: 29/10/2024
      * Purpose:
      * Tectonics: cobc
      * Obeservation: ER significa ERESUMO.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ERESUMO.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION SECTION.
      *
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ERESUMO ASSIGN TO "ERESUMO.DAT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WK-FS-ER.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE SECTION.

       FD  ERESUMO.

       01  REG-ERESUMO                 PIC X(80).

      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.

       77  WK-FS-ER                    PIC 9(05) VALUE ZEROS.
       77  WK-MSG                      PIC X(40) VALUE SPACES.
       77  WK-FIM                      PIC X(01) VALUE SPACES.

       01  WK-REG-ERESUMO.
           05 WK-ER-NUMCTA-A                PIC 9(06)    VALUES ZEROS.
           05 WK-ER-NUMCTA   REDEFINES WK-ER-NUMCTA-A    PIC 9(06).
           05 WK-ER-NOME                  PIC X(23)    VALUES SPACES.
           05 WK-ER-PERIODO OCCURS 5 TIMES.
               10 WK-ER-VALORS-A          PIC X(07)    VALUE SPACES.
               10 WK-ER-VALORS REDEFINES  WK-ER-VALORS-A  PIC 9(05)V99.
               10 WK-ER-DIAS-A            PIC X(03)    VALUE SPACES.
               10 WK-ER-DIAS REDEFINES    WK-ER-DIAS-A    PIC 9(03).
       01  WK-I                           PIC 9    VALUES 1.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------

       0000-MAIN.
           PERFORM 0100-INITIAL THRU 0100-END
           PERFORM 0300-PROCESS THRU 0300-END
                        UNTIL WK-FIM = "S" OR WK-FIM = "s"
           PERFORM 0500-FINISH  THRU 0500-END

           STOP RUN.

      *    PROCEDIMENTO DE INICIALIZACAO
       0100-INITIAL SECTION.
           OPEN OUTPUT ERESUMO.

           IF WK-FS-ER NOT EQUAL ZEROS
               MOVE "ERROR TO OPEN FILE" TO WK-MSG
               PERFORM 0700-ERROR   THRU 0700-END
           END-IF
           .
       0100-END.

       0300-PROCESS SECTION.
           DISPLAY "NUMERO DA CONTA: [6]"
           ACCEPT WK-ER-NUMCTA-A END-ACCEPT

           DISPLAY "QUAL O NOME: [23]"
           ACCEPT WK-ER-NOME END-ACCEPT

           PERFORM VARYING WK-I FROM 1 BY 1 UNTIL WK-I > 5
               DISPLAY "QUAL OS VALORES DO PERIODO " WK-I ": "
               " EXEMPLO: APENAS R$ 1,01 , FICA: 0000101" END-DISPLAY
               ACCEPT WK-ER-VALORS-A (WK-I) END-ACCEPT

               DISPLAY "QUANTOS DIAS DO PERIODO " WK-I ": "
                   " EXEMPLO: APENAS 1 DIA, FICA: 001" END-DISPLAY
               ACCEPT WK-ER-DIAS-A (WK-I) END-ACCEPT
           END-PERFORM

      *    GRAVAR REGISTRO
           WRITE REG-ERESUMO FROM WK-REG-ERESUMO

           IF WK-FS-ER NOT EQUAL ZEROS
               MOVE "ERRO NO WRITE DO ARQUIVO" TO WK-MSG
               PERFORM 0700-ERROR
           END-IF
           .

           DISPLAY "DESEJA ENCERRAR O PROCESSAMENTO (S/N:)" END-DISPLAY.
           ACCEPT WK-FIM END-ACCEPT
           .
       0300-END. EXIT.

       0500-FINISH SECTION.

           CLOSE ERESUMO

           IF WK-FS-ER NOT EQUAL ZEROS
               MOVE "ERROR, CLOSE THE FILE" TO WK-MSG
               PERFORM 0700-ERROR
           END-IF
           .

       0500-END. EXIT.


      *    TRATAR ERROR
       0700-ERROR SECTION.
           DISPLAY "==================================================="
           DISPLAY "PROGRAMA COM ERRO"
           DISPLAY "==================================================="
           DISPLAY WK-MSG
           DISPLAY "==================================================="
           DISPLAY "FILE STATUS: " WK-FS-ER
           DISPLAY "==================================================="
           .
       0700-END. EXIT.

       END PROGRAM ERESUMO.
