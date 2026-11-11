       IDENTIFICATION DIVISION.
       PROGRAM-ID. FATRESUM.
      *================================================================*
      * PROGRAMA : FATRESUM                                            *
      * OBJETIVO : RESUMIR AS DIVERGENCIAS (FATURA.DIVERG) POR TIPO    *
      *            PS / FS / PM, COM QTD, VLR FATURA E VLR PAGO.       *
      * ENTRADA  : DD DIVERG  (LAYOUT COPYBOOK CPDIVER)                *
      * SAIDA    : SYSOUT (DISPLAY)                                    *
      * RETURN   : 0000 OK / 0004 ARQ. VAZIO / 0008 TOTAIS NAO BATEM / *
      *            0012 ERRO DE I/O                                    *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-DIVERG ASSIGN TO DIVERG
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS WS-FS-DIVERG.

       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-DIVERG
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY CPDIVER.

       WORKING-STORAGE SECTION.
       77  WS-FS-DIVERG             PIC X(02)  VALUE SPACES.
       77  WS-MSG-ERRO              PIC X(30)  VALUE SPACES.
       77  WS-LAST-CC               PIC 9(04)  VALUE ZEROS.
       77  WS-SOMA-TIPOS            PIC 9(07)  VALUE ZEROS.
       77  WS-STATUS-CONF           PIC X(04)  VALUE SPACES.
       77  WS-FLAG-FIM              PIC X(01)  VALUE 'N'.
           88  FIM-ARQUIVO                     VALUE 'S'.
           88  NAO-FIM-ARQUIVO                 VALUE 'N'.

       01  WS-CONTADORES.
           05  WS-TOTAL-LIDOS       PIC 9(07)  VALUE ZEROS.
           05  WS-QTD-PS            PIC 9(07)  VALUE ZEROS.
           05  WS-QTD-FS            PIC 9(07)  VALUE ZEROS.
           05  WS-QTD-PM            PIC 9(07)  VALUE ZEROS.
           05  WS-QTD-OUTROS        PIC 9(07)  VALUE ZEROS.

       01  WS-ACUMULADORES.
           05  WS-FAT-PS            PIC 9(11)V99 COMP-3 VALUE ZEROS.
           05  WS-PAGO-PS           PIC 9(11)V99 COMP-3 VALUE ZEROS.
           05  WS-FAT-FS            PIC 9(11)V99 COMP-3 VALUE ZEROS.
           05  WS-PAGO-FS           PIC 9(11)V99 COMP-3 VALUE ZEROS.
           05  WS-FAT-PM            PIC 9(11)V99 COMP-3 VALUE ZEROS.
           05  WS-PAGO-PM           PIC 9(11)V99 COMP-3 VALUE ZEROS.
           05  WS-FAT-TOTAL         PIC 9(11)V99 COMP-3 VALUE ZEROS.
           05  WS-PAGO-TOTAL        PIC 9(11)V99 COMP-3 VALUE ZEROS.
           05  WS-SALDO-RECEBER     PIC S9(11)V99 COMP-3 VALUE ZEROS.

       01  WS-EDICAO.
           05  ED-QTD               PIC ZZZ.ZZ9.
           05  ED-VLR               PIC ZZ.ZZZ.ZZZ.ZZ9,99.
           05  ED-SALDO             PIC ZZ.ZZZ.ZZZ.ZZ9,99-.

       01  WS-LINHA-IGUAL           PIC X(54)  VALUE ALL '='.
       01  WS-LINHA-TRACO           PIC X(54)  VALUE ALL '-'.

       01  WS-CAB-COLUNAS.
           05  FILLER               PIC X(07)  VALUE ' TIPO  '.
           05  FILLER               PIC X(04)  VALUE SPACES.
           05  FILLER               PIC X(03)  VALUE 'QTD'.
           05  FILLER               PIC X(08)  VALUE SPACES.
           05  FILLER               PIC X(12)  VALUE 'VALOR FATURA'.
           05  FILLER               PIC X(10)  VALUE SPACES.
           05  FILLER               PIC X(10)  VALUE 'VALOR PAGO'.

       01  WS-LINHA-DET.
           05  FILLER               PIC X(01)  VALUE SPACE.
           05  DET-TIPO             PIC X(05)  VALUE SPACES.
           05  FILLER               PIC X(01)  VALUE SPACE.
           05  DET-QTD              PIC ZZZ.ZZ9.
           05  FILLER               PIC X(03)  VALUE SPACES.
           05  DET-VLR-FATURA       PIC ZZ.ZZZ.ZZZ.ZZ9,99.
           05  FILLER               PIC X(03)  VALUE SPACES.
           05  DET-VLR-PAGO         PIC ZZ.ZZZ.ZZZ.ZZ9,99.

      *================================================================*
       PROCEDURE DIVISION.
      *================================================================*
       0000-PRINCIPAL.
           PERFORM 1000-INICIALIZAR THRU 1000-FIM.
           PERFORM 2000-PROCESSAR   THRU 2000-FIM
               UNTIL FIM-ARQUIVO.
           PERFORM 3000-FINALIZAR   THRU 3000-FIM.
           MOVE WS-LAST-CC TO RETURN-CODE.
           STOP RUN.

      *----------------------------------------------------------------*
      * BLOCO 1 - INICIALIZACAO                                        *
      *----------------------------------------------------------------*
       1000-INICIALIZAR.
           DISPLAY 'FATRESUM - INICIO DO PROCESSAMENTO'.
           OPEN INPUT ARQ-DIVERG.
           IF WS-FS-DIVERG NOT = '00'
              MOVE 'ERRO NO OPEN DO DIVERG' TO WS-MSG-ERRO
              PERFORM 9000-ABORTAR THRU 9000-FIM
           END-IF.
           PERFORM 2100-LER-DIVERG THRU 2100-FIM.
       1000-FIM.
           EXIT.

      *----------------------------------------------------------------*
      * BLOCO 2 - PROCESSAMENTO (AGRUPAMENTO POR TIPO)                 *
      *----------------------------------------------------------------*
       2000-PROCESSAR.
           ADD 1 TO WS-TOTAL-LIDOS.
           EVALUATE TRUE
               WHEN DV-PAGTO-SEM-FATURA
                    ADD 1              TO WS-QTD-PS
                    ADD DV-VALOR-FATURA TO WS-FAT-PS
                    ADD DV-VALOR-PAGO  TO WS-PAGO-PS
               WHEN DV-FATURA-SEM-PAGTO
                    ADD 1              TO WS-QTD-FS
                    ADD DV-VALOR-FATURA TO WS-FAT-FS
                    ADD DV-VALOR-PAGO  TO WS-PAGO-FS
               WHEN DV-PAGTO-A-MENOR
                    ADD 1              TO WS-QTD-PM
                    ADD DV-VALOR-FATURA TO WS-FAT-PM
                    ADD DV-VALOR-PAGO  TO WS-PAGO-PM
               WHEN OTHER
                    ADD 1              TO WS-QTD-OUTROS
                    MOVE WS-TOTAL-LIDOS TO ED-QTD
                    DISPLAY 'AVISO: TIPO INVALIDO NO REGISTRO ' ED-QTD
           END-EVALUATE.
           PERFORM 2100-LER-DIVERG THRU 2100-FIM.
       2000-FIM.
           EXIT.

       2100-LER-DIVERG.
           READ ARQ-DIVERG
               AT END
                  SET FIM-ARQUIVO TO TRUE
           END-READ.
           EVALUATE WS-FS-DIVERG
               WHEN '00'
                    CONTINUE
               WHEN '10'
                    SET FIM-ARQUIVO TO TRUE
               WHEN OTHER
                    MOVE 'ERRO NA LEITURA DO DIVERG' TO WS-MSG-ERRO
                    PERFORM 9000-ABORTAR THRU 9000-FIM
           END-EVALUATE.
       2100-FIM.
           EXIT.

      *----------------------------------------------------------------*
      * BLOCO 3 - FINALIZACAO                                          *
      *----------------------------------------------------------------*
       3000-FINALIZAR.
           CLOSE ARQ-DIVERG.
           IF WS-FS-DIVERG NOT = '00'
              MOVE 'ERRO NO CLOSE DO DIVERG' TO WS-MSG-ERRO
              PERFORM 9000-ABORTAR THRU 9000-FIM
           END-IF.
           PERFORM 3100-CALCULAR-TOTAIS THRU 3100-FIM.
           PERFORM 3200-EXIBIR-RESUMO   THRU 3200-FIM.
       3000-FIM.
           EXIT.

       3100-CALCULAR-TOTAIS.
           COMPUTE WS-FAT-TOTAL  = WS-FAT-PS  + WS-FAT-FS
                                 + WS-FAT-PM.
           COMPUTE WS-PAGO-TOTAL = WS-PAGO-PS + WS-PAGO-FS
                                 + WS-PAGO-PM.
           COMPUTE WS-SALDO-RECEBER = WS-FAT-TOTAL - WS-PAGO-TOTAL.
           COMPUTE WS-SOMA-TIPOS = WS-QTD-PS + WS-QTD-FS + WS-QTD-PM.
           IF WS-SOMA-TIPOS = WS-TOTAL-LIDOS
              MOVE 'OK  ' TO WS-STATUS-CONF
           ELSE
              MOVE 'ERRO' TO WS-STATUS-CONF
              MOVE 8      TO WS-LAST-CC
           END-IF.
           IF WS-TOTAL-LIDOS = ZEROS AND WS-LAST-CC = ZEROS
              MOVE 4 TO WS-LAST-CC
           END-IF.
       3100-FIM.
           EXIT.

       3200-EXIBIR-RESUMO.
           DISPLAY WS-LINHA-IGUAL.
           DISPLAY ' FATRESUM - RESUMO DAS DIVERGENCIAS DE FATURAS'.
           DISPLAY WS-LINHA-IGUAL.
           DISPLAY WS-CAB-COLUNAS.
           DISPLAY WS-LINHA-TRACO.

           MOVE 'PS'         TO DET-TIPO.
           MOVE WS-QTD-PS    TO DET-QTD.
           MOVE WS-FAT-PS    TO DET-VLR-FATURA.
           MOVE WS-PAGO-PS   TO DET-VLR-PAGO.
           DISPLAY WS-LINHA-DET.

           MOVE 'FS'         TO DET-TIPO.
           MOVE WS-QTD-FS    TO DET-QTD.
           MOVE WS-FAT-FS    TO DET-VLR-FATURA.
           MOVE WS-PAGO-FS   TO DET-VLR-PAGO.
           DISPLAY WS-LINHA-DET.

           MOVE 'PM'         TO DET-TIPO.
           MOVE WS-QTD-PM    TO DET-QTD.
           MOVE WS-FAT-PM    TO DET-VLR-FATURA.
           MOVE WS-PAGO-PM   TO DET-VLR-PAGO.
           DISPLAY WS-LINHA-DET.

           DISPLAY WS-LINHA-TRACO.
           MOVE 'TOTAL'          TO DET-TIPO.
           MOVE WS-SOMA-TIPOS    TO DET-QTD.
           MOVE WS-FAT-TOTAL     TO DET-VLR-FATURA.
           MOVE WS-PAGO-TOTAL    TO DET-VLR-PAGO.
           DISPLAY WS-LINHA-DET.
           DISPLAY WS-LINHA-IGUAL.

           MOVE WS-TOTAL-LIDOS   TO ED-QTD.
           DISPLAY ' REGISTROS LIDOS ...............: ' ED-QTD.
           MOVE WS-QTD-OUTROS    TO ED-QTD.
           DISPLAY ' REGISTROS DE TIPO INVALIDO ....: ' ED-QTD.
           DISPLAY ' CONFERENCIA PS+FS+PM = LIDOS ..: ' WS-STATUS-CONF.
           MOVE WS-SALDO-RECEBER TO ED-SALDO.
           DISPLAY ' SALDO A RECEBER (FATURA-PAGO) .: ' ED-SALDO.
           DISPLAY WS-LINHA-IGUAL.
           DISPLAY ' LAST-CC = ' WS-LAST-CC.
           DISPLAY 'FATRESUM - FIM DO PROCESSAMENTO'.
       3200-FIM.
           EXIT.

      *----------------------------------------------------------------*
      * ABORT PADRAO PARA ERRO DE I/O                                  *
      *----------------------------------------------------------------*
       9000-ABORTAR.
           DISPLAY '*** ' WS-MSG-ERRO ' - FILE STATUS: ' WS-FS-DIVERG.
           MOVE 12 TO WS-LAST-CC.
           DISPLAY ' LAST-CC = ' WS-LAST-CC.
           MOVE WS-LAST-CC TO RETURN-CODE.
           STOP RUN.
       9000-FIM.
           EXIT.