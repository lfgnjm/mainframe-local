      *================================================================*
      * PROGRAMA : FATRESUM                                            *
      * AULA     : 10 - DESAFIO: RESUMO DAS DIVERGENCIAS               *
      * OBJETIVO : LER O ARQUIVO DE DIVERGENCIAS E EXIBIR NO SYSOUT O  *
      *            RESUMO POR TIPO (PS, FS, PM): QUANTIDADE, VALOR DE  *
      *            FATURA E VALOR PAGO, MAIS O TOTAL GERAL E O SALDO   *
      *            A RECEBER (FATURA - PAGO).                          *
      * ENTRADA  : DD DIVERG  - CPDIVER (FATURA.DIVERG)                *
      * SAIDA    : SYSOUT     - RESUMO VIA DISPLAY                     *
      * RETORNO  : 0000 OK / 0004 ARQUIVO VAZIO / 0008 TOTAIS NAO      *
      *            BATEM / 0016 ERRO DE I/O                            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FATRESUM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-DIVERG ASSIGN TO DIVERG
                  FILE STATUS IS WS-FS-DIVERG.

       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-DIVERG
           RECORDING MODE IS F.
           COPY CPDIVER.

       WORKING-STORAGE SECTION.
       01  WS-FS-DIVERG               PIC X(02).
           88  WS-FS-DIV-OK                     VALUE '00'.
           88  WS-FS-DIV-FIM                    VALUE '10'.

      *--- CONTADORES POR TIPO ----------------------------------------*
       01  WS-TOTAL-LIDOS             PIC 9(07) VALUE ZERO.
       01  WS-QTD-PS                  PIC 9(07) VALUE ZERO.
       01  WS-QTD-FS                  PIC 9(07) VALUE ZERO.
       01  WS-QTD-PM                  PIC 9(07) VALUE ZERO.
       01  WS-QTD-OUTROS              PIC 9(07) VALUE ZERO.
       01  WS-SOMA-TIPOS              PIC 9(07) VALUE ZERO.

      *--- ACUMULADORES DE VALOR --------------------------------------*
       01  WS-FATURA-PS               PIC S9(11)V99 COMP-3 VALUE ZERO.
       01  WS-PAGO-PS                 PIC S9(11)V99 COMP-3 VALUE ZERO.
       01  WS-FATURA-FS               PIC S9(11)V99 COMP-3 VALUE ZERO.
       01  WS-PAGO-FS                 PIC S9(11)V99 COMP-3 VALUE ZERO.
       01  WS-FATURA-PM               PIC S9(11)V99 COMP-3 VALUE ZERO.
       01  WS-PAGO-PM                 PIC S9(11)V99 COMP-3 VALUE ZERO.
       01  WS-FATURA-TOTAL            PIC S9(13)V99 COMP-3 VALUE ZERO.
       01  WS-PAGO-TOTAL              PIC S9(13)V99 COMP-3 VALUE ZERO.
       01  WS-SALDO-RECEBER           PIC S9(13)V99 COMP-3 VALUE ZERO.

      *--- CONTROLE ---------------------------------------------------*
       01  WS-LAST-CC                 PIC 9(04) VALUE ZERO.
       01  WS-STATUS-CONFERENCIA      PIC X(04) VALUE SPACES.

      *--- EDICAO -----------------------------------------------------*
       01  WS-ED-QTD                  PIC ZZZ,ZZ9.
       01  WS-ED-SALDO                PIC ZZ,ZZZ,ZZZ,ZZ9.99-.

      *--- LINHAS DO RESUMO -------------------------------------------*
       01  WS-LINHA-IGUAL             PIC X(54) VALUE ALL '='.
       01  WS-LINHA-TRACO             PIC X(54) VALUE ALL '-'.
       01  WS-CAB-1.
           05  FILLER                 PIC X(46) VALUE
               ' FATRESUM - RESUMO DAS DIVERGENCIAS DE FATURAS'.
       01  WS-CAB-2.
           05  FILLER                 PIC X(07) VALUE ' TIPO  '.
           05  FILLER                 PIC X(04) VALUE SPACES.
           05  FILLER                 PIC X(03) VALUE 'QTD'.
           05  FILLER                 PIC X(08) VALUE SPACES.
           05  FILLER                 PIC X(12) VALUE 'VALOR FATURA'.
           05  FILLER                 PIC X(10) VALUE SPACES.
           05  FILLER                 PIC X(10) VALUE 'VALOR PAGO'.
       01  WS-DETALHE.
           05  FILLER                 PIC X(01) VALUE SPACE.
           05  DT-TIPO                PIC X(05).
           05  FILLER                 PIC X(01) VALUE SPACE.
           05  DT-QTD                 PIC ZZZ,ZZ9.
           05  FILLER                 PIC X(03) VALUE SPACES.
           05  DT-FATURA              PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                 PIC X(03) VALUE SPACES.
           05  DT-PAGO                PIC ZZ,ZZZ,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INICIALIZAR THRU 1000-FIM
           PERFORM 2000-PROCESSAR   THRU 2000-FIM
              UNTIL WS-FS-DIV-FIM
           PERFORM 3000-FINALIZAR   THRU 3000-FIM
           MOVE WS-LAST-CC TO RETURN-CODE
           GOBACK
           .

      *----------------------------------------------------------------*
      * BLOCO 1 - INICIALIZACAO                                        *
      *----------------------------------------------------------------*
       1000-INICIALIZAR.
           DISPLAY 'FATRESUM - INICIO DO PROCESSAMENTO'
           OPEN INPUT ARQ-DIVERG
           IF NOT WS-FS-DIV-OK
              DISPLAY 'FATRESUM - ERRO NO OPEN DE DIVERG. FS='
                      WS-FS-DIVERG
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           PERFORM 8000-LER-DIVERG
           .
       1000-FIM.
           EXIT.

      *----------------------------------------------------------------*
      * BLOCO 2 - PROCESSAMENTO: UM CICLO = UM REGISTRO                *
      *----------------------------------------------------------------*
       2000-PROCESSAR.
           ADD 1 TO WS-TOTAL-LIDOS
           EVALUATE TRUE
              WHEN DIV-PS
                 ADD 1              TO WS-QTD-PS
                 ADD DIV-VLR-FATURA TO WS-FATURA-PS
                 ADD DIV-VLR-PAGO   TO WS-PAGO-PS
              WHEN DIV-FS
                 ADD 1              TO WS-QTD-FS
                 ADD DIV-VLR-FATURA TO WS-FATURA-FS
                 ADD DIV-VLR-PAGO   TO WS-PAGO-FS
              WHEN DIV-PM
                 ADD 1              TO WS-QTD-PM
                 ADD DIV-VLR-FATURA TO WS-FATURA-PM
                 ADD DIV-VLR-PAGO   TO WS-PAGO-PM
              WHEN OTHER
                 ADD 1              TO WS-QTD-OUTROS
                 MOVE WS-TOTAL-LIDOS TO WS-ED-QTD
                 DISPLAY 'FATRESUM - TIPO INVALIDO NO REGISTRO '
                         WS-ED-QTD
           END-EVALUATE
           PERFORM 8000-LER-DIVERG
           .
       2000-FIM.
           EXIT.

      *----------------------------------------------------------------*
      * BLOCO 3 - FINALIZACAO                                          *
      *----------------------------------------------------------------*
       3000-FINALIZAR.
           CLOSE ARQ-DIVERG
           IF NOT WS-FS-DIV-OK
              DISPLAY 'FATRESUM - ERRO NO CLOSE DE DIVERG. FS='
                      WS-FS-DIVERG
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           PERFORM 3100-CALCULAR-TOTAIS THRU 3100-FIM
           PERFORM 3200-EXIBIR-RESUMO   THRU 3200-FIM
           DISPLAY 'FATRESUM - FIM DO PROCESSAMENTO'
           .
       3000-FIM.
           EXIT.

       3100-CALCULAR-TOTAIS.
           COMPUTE WS-FATURA-TOTAL = WS-FATURA-PS + WS-FATURA-FS
                                   + WS-FATURA-PM
           COMPUTE WS-PAGO-TOTAL   = WS-PAGO-PS + WS-PAGO-FS
                                   + WS-PAGO-PM
           COMPUTE WS-SALDO-RECEBER = WS-FATURA-TOTAL - WS-PAGO-TOTAL
           COMPUTE WS-SOMA-TIPOS = WS-QTD-PS + WS-QTD-FS + WS-QTD-PM
           IF WS-SOMA-TIPOS = WS-TOTAL-LIDOS
              MOVE 'OK  ' TO WS-STATUS-CONFERENCIA
           ELSE
              MOVE 'ERRO' TO WS-STATUS-CONFERENCIA
              MOVE 8      TO WS-LAST-CC
           END-IF
           IF WS-TOTAL-LIDOS = ZERO AND WS-LAST-CC = ZERO
              MOVE 4 TO WS-LAST-CC
           END-IF
           .
       3100-FIM.
           EXIT.

       3200-EXIBIR-RESUMO.
           DISPLAY WS-LINHA-IGUAL
           DISPLAY WS-CAB-1
           DISPLAY WS-LINHA-IGUAL
           DISPLAY WS-CAB-2
           DISPLAY WS-LINHA-TRACO

           MOVE 'PS'             TO DT-TIPO
           MOVE WS-QTD-PS        TO DT-QTD
           MOVE WS-FATURA-PS     TO DT-FATURA
           MOVE WS-PAGO-PS       TO DT-PAGO
           DISPLAY WS-DETALHE

           MOVE 'FS'             TO DT-TIPO
           MOVE WS-QTD-FS        TO DT-QTD
           MOVE WS-FATURA-FS     TO DT-FATURA
           MOVE WS-PAGO-FS       TO DT-PAGO
           DISPLAY WS-DETALHE

           MOVE 'PM'             TO DT-TIPO
           MOVE WS-QTD-PM        TO DT-QTD
           MOVE WS-FATURA-PM     TO DT-FATURA
           MOVE WS-PAGO-PM       TO DT-PAGO
           DISPLAY WS-DETALHE

           DISPLAY WS-LINHA-TRACO
           MOVE 'TOTAL'          TO DT-TIPO
           MOVE WS-SOMA-TIPOS    TO DT-QTD
           MOVE WS-FATURA-TOTAL  TO DT-FATURA
           MOVE WS-PAGO-TOTAL    TO DT-PAGO
           DISPLAY WS-DETALHE
           DISPLAY WS-LINHA-IGUAL

           MOVE WS-TOTAL-LIDOS   TO WS-ED-QTD
           DISPLAY ' REGISTROS LIDOS ...............: ' WS-ED-QTD
           MOVE WS-QTD-OUTROS    TO WS-ED-QTD
           DISPLAY ' REGISTROS DE TIPO INVALIDO ....: ' WS-ED-QTD
           DISPLAY ' CONFERENCIA PS+FS+PM = LIDOS ..: '
                   WS-STATUS-CONFERENCIA
           MOVE WS-SALDO-RECEBER TO WS-ED-SALDO
           DISPLAY ' SALDO A RECEBER (FATURA-PAGO) .: ' WS-ED-SALDO
           DISPLAY WS-LINHA-IGUAL
           DISPLAY ' LAST-CC = ' WS-LAST-CC
           .
       3200-FIM.
           EXIT.

       8000-LER-DIVERG.
           READ ARQ-DIVERG
              AT END CONTINUE
           END-READ
           IF NOT WS-FS-DIV-OK AND NOT WS-FS-DIV-FIM
              DISPLAY 'FATRESUM - ERRO NO READ DE DIVERG. FS='
                      WS-FS-DIVERG
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           .

       9900-ENCERRAR-COM-ERRO.
           MOVE 16 TO WS-LAST-CC
           DISPLAY ' LAST-CC = ' WS-LAST-CC
           MOVE WS-LAST-CC TO RETURN-CODE
           STOP RUN
           .