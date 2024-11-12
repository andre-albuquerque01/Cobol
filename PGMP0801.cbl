      ******************************************************************
      * Author: Andre Albuquerque Goncalves
      * Date: 31/10/2024
      * Purpose:
      * Tectonics: cobc
      * Obeservation: ER significa ERESUMO.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMP0801.
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

           SELECT RELATO ASSIGN TO "RELATO.DAT".
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE SECTION.

       FD  ERESUMO
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.

       01  REG-ER                      PIC X(80).

       FD  RELATO.

       01  REG-REL                     PIC X(72).

      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.

       77  WK-FS-ER                    PIC 9(02) VALUE ZEROS.
       77  WK-MSG                      PIC X(35) VALUE SPACES.
       77  WK-FIM                      PIC X(01) VALUE SPACES.
       77  WK-ERRO                     PIC X(01) VALUE SPACES.

       01  WK-TOT-LIDOS                PIC 9(06) VALUE ZEROS.
       01  WK-TOT-PROCESSADOS          PIC 9(06) VALUE ZEROS.
       01  WK-TOT-SELECIONADOS         PIC 9(06) VALUE ZEROS.
       01  WK-TOT-REJEITADOS           PIC 9(06) VALUE ZEROS.
       01  WK-AC-PAGINAS               PIC 9(03) VALUE ZEROS.
       01  WK-AC-LINHAS                PIC 9(05) VALUE ZEROS.


       01  WK-CURRENT-DATE.
           05 WK-CD-AAAA               PIC 9(04) VALUE ZEROS.
           05 WK-CD-MM                 PIC 9(02) VALUE ZEROS.
           05 WK-CD-DD                 PIC 9(02) VALUE ZEROS.

       01  WK-DATA.
           05 WK-DATA1-DD              PIC 9(02) VALUE ZEROS.
           05 FILLER                   PIC X     VALUE '/'.
           05 WK-DATA1-MM              PIC 9(02) VALUE ZEROS.
           05 FILLER                   PIC X     VALUE '/'.
           05 WK-DATA1-AAAA            PIC 9(04) VALUE ZEROS.

       01  WK-REG-ERESUMO.
           05 WK-ER-NUMCTA                PIC X(06)    VALUE SPACES.
           05 WK-ER-NOME                  PIC X(23)    VALUE SPACES.
           05 WK-ER-PERIODO OCCURS 5 TIMES.
               10 WK-ER-VALORS-A          PIC X(07)    VALUE SPACES.
               10 WK-ER-VALORS REDEFINES  WK-ER-VALORS-A  PIC 9(05)V99.
               10 WK-ER-DIAS-A            PIC X(03)    VALUE SPACES.
               10 WK-ER-DIAS   REDEFINES  WK-ER-DIAS-A    PIC 9(03).
       01  WK-I                           PIC 9    VALUE 1.

       01  WK-TOT-VALOR-DIA            PIC S9(06)V99 VALUE ZEROS.
       01  WK-TOT-DIAS                 PIC 9(04)     VALUE ZEROS.
       01  WK-SALDO-MEDIO              PIC S9(06)V99 VALUE ZEROS.
       01  WK-SALDO-MEDIO-FMT          PIC +9(05),99.

      *    LINHAS DO RELATORIO DE SAIDA
      **** 1 MONTEDATA PROCESSADORA DE DADOS LTDA
       01  WK-RELATO1.
           05 FILLER                   PIC X(36) VALUE
           'MONTEDATA PROCESSADORA DE DADOS LTDA'.
           05 FILLER                   PIC X(13) VALUE SPACES.
           05 FILLER                   PIC X(05) VALUE 'PAG. '.
           05 REL1-PAG                 PIC X(03) VALUE SPACES.
           05 FILLER                   PIC X(07) VALUE SPACES.
           05 FILLER                   PIC X(08) VALUE 'PGMP0802'.

      *****2 DEPARTAMENTO DE ANALISE DE SUPORTE
       01  WK-RELATO2.
           05 FILLER                   PIC X(34) VALUE
           'DEPARTAMENTO DE ANALISE DE SUPORTE'.
           05 FILLER                   PIC X(15) VALUE SPACES.
           05 REL2-DATA1               PIC X(10) VALUE SPACES.
           05 FILLER                   PIC X(05) VALUE SPACES.
           05 FILLER                   PIC X(08) VALUE 'ANDRE.A'.

      *****4 RELATORIO DE SALDO MEDIO
       01  WK-RELATO3.
           05 FILLER                   PIC X(15).
           05 FILLER                   PIC X(35) VALUE
           'RELATORIO DE SALDO MEDIO'.

      *****4 NUMERO NOME SALDO MEDIO
       01  WK-RELATO4.
           05 FILLER                   PIC X(02) VALUE SPACES.
           05 FILLER                   PIC X(06) VALUE
           'NUMERO'.
           05 FILLER                   PIC X(16) VALUE SPACES.
           05 FILLER                   PIC X(04) VALUE
           'NOME'.
           05 FILLER                   PIC X(16) VALUE SPACES.
           05 FILLER                   PIC X(11) VALUE
           'SALDO MEDIO'.

      *****8 NUMERO NOME SALDO MEDIO
       01  WK-RELATO8.
           05 FILLER                   PIC X(01) VALUE SPACES.
           05 DET-CONTA-CLI            PIC ZZZ.ZZZ.
           05 FILLER                   PIC X(07) VALUE SPACES.
           05 DET-NOME-CLI             PIC X(23) VALUE SPACES.
           05 FILLER                   PIC X(7)  VALUE SPACES.
           05 DET-SALDO-CLI            PIC X(09) VALUE SPACES.

      *****3 TOTAIS DE CONTROLE - ETFXXX01 -
       01  WK-RELATO9.
           05 FILLER                   PIC X(10) VALUE SPACES.
           05 FILLER                   PIC X(32) VALUE
           "TOTAIS DE CONTROLE - PGMP0802 - ".
           05 REL2-DATA2               PIC X(10) VALUE SPACES.

      *****4 CARTOES LIDOS
       01  WK-RELATO10.
           05 FILLER                   PIC X(16) VALUE SPACES.
           05 FILLER                   PIC X(13) VALUE
           "CARTOES LIDOS".
           05 FILLER                   PIC X(10) VALUE SPACES.
           05 REL2-LIDOS                PIC 9(5) VALUE ZEROS.

      *****4 CARTOES PROCESSADOS
       01  WK-RELATO11.
           05 FILLER                   PIC X(16) VALUE SPACES.
           05 FILLER                   PIC X(19) VALUE
           "CARTOES PROCESSADOS".
           05 FILLER                   PIC X(4) VALUE SPACES.
           05 REL2-PROCESSADOS         PIC 9(5) VALUE ZEROS.

      *****4 CARTOES PROCESSADOS
       01  WK-RELATO12.
           05 FILLER                   PIC X(16) VALUE SPACES.
           05 FILLER                   PIC X(18) VALUE
           "CARTOES REJEITADOS".
           05 FILLER                   PIC X(5) VALUE SPACES.
           05 REL2-REJEITADOS          PIC 9(5) VALUE ZEROS.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       0000-INITIAL.
           PERFORM 1000-INICIALIZAR THRU END-1000
           PERFORM UNTIL WK-FIM = "S"
               PERFORM 2000-PROCESSAMENTO THRU END-2000
           END-PERFORM
           PERFORM 3000-FINALIZAR THRU END-3000
           PERFORM 0520-FECHAR-ARQ THRU END-0520

           STOP RUN.

           1000-INICIALIZAR SECTION.
      *    ABRIR O ARQUIVO COM REGISTROS
               PERFORM 1010-OPEN THRU END-1010
      *    INICIALIZAR VARIAVEIS
               PERFORM 1020-VARIAVEIS THRU END-1020
      *    LER OS REGISTROS DO ARQUIVO
               PERFORM 1030-READ THRU END-1030
           .
           END-1000. EXIT.

           1010-OPEN SECTION.
               OPEN INPUT ERESUMO

               IF WK-FS-ER NOT EQUAL ZEROS
                   MOVE "Erro na Abertura do arquivo ERESUMO" TO WK-MSG
                   PERFORM 9000-ERRO-PROCESSAMENTO THRU 9000-FIM
               END-IF

               OPEN OUTPUT RELATO
           .
           END-1010. EXIT.

           1020-VARIAVEIS SECTION.
               MOVE ZEROS TO WK-TOT-PROCESSADOS
                             WK-TOT-SELECIONADOS
                             WK-TOT-REJEITADOS
                             WK-AC-PAGINAS

               MOVE 999 TO WK-AC-LINHAS

               MOVE FUNCTION CURRENT-DATE(1:8)
                             TO WK-CURRENT-DATE

               MOVE WK-CD-AAAA TO WK-DATA1-AAAA
               MOVE WK-CD-MM TO WK-DATA1-MM
               MOVE WK-CD-DD TO WK-DATA1-DD
           .
           END-1020. EXIT.

           1030-READ SECTION.
               READ ERESUMO INTO WK-REG-ERESUMO
                    AT END
                       MOVE "S" TO WK-FIM
                    NOT AT END
                       ADD 1 TO WK-TOT-LIDOS
               END-READ.
               IF WK-FS-ER NOT EQUAL ZEROS AND
                   WK-FS-ER NOT EQUAL 10
                   MOVE "Erro na Leitura do ERESUMO" TO WK-MSG
                   PERFORM 9000-ERRO-PROCESSAMENTO THRU 9000-FIM
               END-IF
           .
           END-1030. EXIT.

           2000-PROCESSAMENTO SECTION.
               IF WK-FIM NOT = "S"
                   PERFORM 2010-CONSISTENCIA THRU END-2010
                   IF WK-ERRO EQUAL "S"
                       ADD 1 TO WK-TOT-REJEITADOS
                   ELSE
                       PERFORM 2030-CALCULAR-SALDO THRU END-2030
                       PERFORM 2040-IMPRIMIR-SALDO THRU END-2040
                       ADD 1 TO WK-TOT-PROCESSADOS
                  END-IF
               PERFORM 1030-READ THRU END-1030
               END-IF
               .
           END-2000. EXIT.

           2010-CONSISTENCIA SECTION.
               MOVE SPACES TO WK-ERRO
               IF WK-ER-NUMCTA >= 0
                   IF WK-ER-NOME NOT EQUAL SPACES
                   ELSE
                       MOVE "S" TO WK-ERRO
                   END-IF
              ELSE
                   MOVE "S" TO WK-ERRO
              END-IF
              IF WK-ERRO EQUAL "S"
              ELSE
                  PERFORM 2210-TRATAR-VALOR THRU END-2210
              END-IF
           .
           END-2010. EXIT.

           2210-TRATAR-VALOR SECTION.
               PERFORM VARYING WK-I FROM 1 BY 1 UNTIL WK-I EQUAL 6
                   IF WK-ER-VALORS (WK-I) IS NUMERIC
                   ELSE
                       IF WK-ER-VALORS-A (WK-I) EQUAL SPACE
                           MOVE 0000000 TO WK-ER-VALORS (WK-I)
                       ELSE
                           MOVE "S" TO WK-ERRO
                       END-IF
                   END-IF
               IF WK-ERRO NOT EQUAL "S"
                   PERFORM 2220-TRATAR-DIA THRU END-2220
               END-IF
               END-PERFORM

           .
           END-2210. EXIT.

           2220-TRATAR-DIA SECTION.
                   IF WK-ER-DIAS (WK-I) IS NUMERIC
                       IF WK-ER-DIAS (WK-I) EQUAL ZERO
                           IF WK-ER-VALORS (WK-I) <> ZERO
                               MOVE "S" TO WK-ERRO
                           END-IF
                       END-IF
                   ELSE
                       IF WK-ER-DIAS-A (WK-I) EQUAL SPACES
                           MOVE 000 TO WK-ER-DIAS (WK-I)
                       ELSE
                           MOVE "S" TO WK-ERRO
                       END-IF
                   END-IF
           .
           END-2220. EXIT.


           2030-CALCULAR-SALDO SECTION.
               MOVE 0 TO WK-SALDO-MEDIO
               COMPUTE WK-TOT-VALOR-DIA =
                           ((WK-ER-VALORS (1) * WK-ER-DIAS (1)) +
                           (WK-ER-VALORS (2) * WK-ER-DIAS (2)) +
                           (WK-ER-VALORS (3) * WK-ER-DIAS (3)) +
                           (WK-ER-VALORS (4) * WK-ER-DIAS (4)) +
                           (WK-ER-VALORS (5) * WK-ER-DIAS (5)))
               END-COMPUTE
               COMPUTE WK-TOT-DIAS =
                       (WK-ER-DIAS (1) + WK-ER-DIAS (2) + WK-ER-DIAS (3)
                       + WK-ER-DIAS(4) + WK-ER-DIAS (5))
               END-COMPUTE
               COMPUTE WK-SALDO-MEDIO = WK-TOT-VALOR-DIA / WK-TOT-DIAS
               END-COMPUTE
               MOVE WK-SALDO-MEDIO TO WK-SALDO-MEDIO-FMT
           .
           END-2030. EXIT.

           2040-IMPRIMIR-SALDO SECTION.
               IF WK-AC-LINHAS > 57
                   PERFORM 2041-IMPRIMIR-CABECALHO THRU END-2041
               END-IF
               PERFORM 2043-IMPRIMIR-DETALHO THRU END-2043
           .
           END-2040. EXIT.

           2041-IMPRIMIR-CABECALHO SECTION.
               ADD 1 TO WK-AC-PAGINAS
               MOVE WK-AC-PAGINAS TO REL1-PAG
               MOVE WK-DATA TO REL2-DATA1

               WRITE REG-REL FROM WK-RELATO1 BEFORE 1
               WRITE REG-REL FROM WK-RELATO2 AFTER 1
               WRITE REG-REL FROM WK-RELATO3 AFTER 1
               WRITE REG-REL FROM WK-RELATO4 AFTER 1

               MOVE 7 TO WK-AC-LINHAS
           .
           END-2041. EXIT.

           2043-IMPRIMIR-DETALHO SECTION.
               ADD 1 TO WK-AC-LINHAS
               MOVE WK-ER-NUMCTA TO DET-CONTA-CLI
               MOVE WK-ER-NOME   TO DET-NOME-CLI
               MOVE WK-SALDO-MEDIO-FMT TO DET-SALDO-CLI.

               WRITE REG-REL FROM WK-RELATO8 AFTER 1
           .
           END-2043. EXIT.

           3000-FINALIZAR SECTION.
               MOVE WK-DATA TO REL2-DATA2
               MOVE WK-TOT-LIDOS TO REL2-LIDOS
               MOVE WK-TOT-PROCESSADOS TO REL2-PROCESSADOS
               MOVE WK-TOT-REJEITADOS TO REL2-REJEITADOS

               WRITE REG-REL FROM WK-RELATO9  AFTER 1
               WRITE REG-REL FROM WK-RELATO10 AFTER 1
               WRITE REG-REL FROM WK-RELATO11 AFTER 1
               WRITE REG-REL FROM WK-RELATO12 AFTER 1
           .
           END-3000. EXIT.

           0520-FECHAR-ARQ SECTION.

           CLOSE ERESUMO

           IF WK-FS-ER NOT EQUAL ZEROS AND
               WK-FS-ER NOT EQUAL 1000
               MOVE "ERRO NO CLOSE DO ARQUIVO ERESUMO" TO WK-MSG
               PERFORM 9000-ERRO-PROCESSAMENTO
           END-IF

           CLOSE RELATO
           .
           END-0520. EXIT.

           9000-ERRO-PROCESSAMENTO SECTION.
           DISPLAY "==================================================="
           DISPLAY "PROGRAMA COM ERRO"
           DISPLAY "==================================================="
           DISPLAY WK-MSG
           DISPLAY "==================================================="
           DISPLAY "FILE STATUS: " WK-FS-ER
           DISPLAY "==================================================="
           .
           9000-FIM. EXIT.


       END PROGRAM PGMP0801
           .
