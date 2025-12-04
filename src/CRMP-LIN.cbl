      *=================================================================
       IDENTIFICATION                                         DIVISION.
       PROGRAM-ID. LISTA-I.
      *=================================================================
       ENVIRONMENT                                            DIVISION.
      *-----------------------------------------------------------------
       INPUT-OUTPUT                                           SECTION.
       FILE-CONTROL.
           SELECT ARQCRM  ASSIGN TO './files/ARQCRM.DAT'
              ORGANIZATION IS INDEXED
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS IS ARQCRM-FS
              RECORD KEY IS ARQCRM-KEY.
      *=================================================================
       DATA                                                   DIVISION.
      *-----------------------------------------------------------------
       FILE                                                   SECTION.
       FD  ARQCRM.
       COPY 'CRMK-000'.
      *-----------------------------------------------------------------
       WORKING-STORAGE                                        SECTION.
       01  ARQCRM-FS            PIC 9(02)   VALUE ZERO.
       01  WRK-LINHA            PIC 9(02)   COMP VALUE 12.
       01  WRK-OPCAO            PIC X(01).
       COPY 'CRMK-TAB'.
      *-----------------------------------------------------------------
       SCREEN                                                 SECTION.
       COPY 'CRMK-SCR'.
      *=================================================================
       PROCEDURE                                              DIVISION.
      *-----------------------------------------------------------------
       PRINCIPAL                                              SECTION.
      *----------------------------------
           PERFORM INICIAR
           PERFORM PROCESSAR
           PERFORM VOLTAR-MENU.
      *-----------------------------------------------------------------
       INICIAR                                                SECTION.
      *----------------------------------
           PERFORM LIMPA-RODAPE
           PERFORM LIMPA-CORPO

           PERFORM ABRE-ARQUIVO

           IF ARQCRM-FS NOT EQUAL ZERO
              PERFORM ERRO-ABRE-ARQUIVO
           END-IF.

           DISPLAY SCR-CTR

           MOVE 'ARQCRM ABERTO' TO SCR-TRL-L1
           DISPLAY SCR-TRL.
      *-----------------------------------------------------------------
       PROCESSAR                                              SECTION.
           PERFORM LIMPA-RODAPE

           MOVE TAB-ARQCRM-HDR TO SCR-CTR-L2

           DISPLAY SCR-CTR

           MOVE 10 TO WRK-LINHA
           MOVE SPACE TO WRK-OPCAO

           PERFORM LE-ENTRADA

           PERFORM UNTIL ARQCRM-FS NOT EQUAL TO ZERO
              IF ARQCRM-INATIVO
                 PERFORM MOSTRA-LINHA
                 ADD 1 TO WRK-LINHA
              END-IF

              IF WRK-LINHA IS GREATER THAN 20
                 PERFORM LIMPA-RODAPE

                 MOVE 'Continua ... [pressione ENTER]'   TO SCR-TRL-L1
                 DISPLAY SCR-TRL

                 ACCEPT WRK-OPCAO                    LINE 23 COLUMN 32

                 MOVE 10 TO WRK-LINHA
                 DISPLAY SCR-CTR
              END-IF

              PERFORM LE-ENTRADA
           END-PERFORM

           PERFORM LIMPA-RODAPE

           EVALUATE ARQCRM-FS
              WHEN 10
                 MOVE 'Relatorio concluido'       TO SCR-TRL-L1
              WHEN OTHER
                 PERFORM ERRO-OUTRO-LE-ARQUIVO
           END-EVALUATE

           DISPLAY SCR-TRL

           ACCEPT WRK-OPCAO                       LINE 25 COLUMN 1.
      *-----------------------------------------------------------------
       MOSTRA-LINHA                                           SECTION.
           MOVE ARQCRM-ID         TO TAB-ARQCRM-ID
           MOVE ARQCRM-NOME       TO TAB-ARQCRM-NOME
           MOVE ARQCRM-EMAIL      TO TAB-ARQCRM-EMAIL
           MOVE ARQCRM-TELEFONE   TO TAB-ARQCRM-TELEFONE

           DISPLAY TAB-ARQCRM-REGISTRO         LINE WRK-LINHA COLUMN 1.
      *-----------------------------------------------------------------
       ERROS                                                  SECTION.
      *----------------------------------
       ERRO-ABRE-ARQUIVO.
           PERFORM LIMPA-RODAPE
           MOVE 'ERRO ABERTURA ARQCRM' TO SCR-TRL-L1
           PERFORM ABENDA.
      *
       ERRO-OUTRO-LE-ARQUIVO.
           PERFORM LIMPA-RODAPE

           STRING
              'Erro na leitura do registro - ARQCRM-FS: '
              ARQCRM-FS
           DELIMITED BY SIZE
           INTO SCR-TRL-L1

           DISPLAY SCR-TRL

           ACCEPT WRK-OPCAO                          LINE 23 COLUMN 46.
      *-----------------------------------------------------------------
       FINALIZAR                                              SECTION.
      *----------------------------------
       ABENDA.
           MOVE 'FIM ANORMAL DO PROGRAMA' TO SCR-TRL-L3
           DISPLAY SCR-TRL
           ACCEPT WRK-OPCAO                          LINE 25 COLUMN 25

           PERFORM VOLTAR-MENU.
      *
       VOLTAR-MENU.
           PERFORM FECHA-ARQUIVO
           GOBACK.
      *-----------------------------------------------------------------
       ARQUIVOS                                               SECTION.
      *----------------------------------
       ABRE-ARQUIVO.
           OPEN I-O ARQCRM.
      *
       FECHA-ARQUIVO.
           CLOSE ARQCRM.
      *
       LE-ENTRADA.
           READ ARQCRM.
      *-----------------------------------------------------------------
       UTILITARIOS                                            SECTION.
      *----------------------------------
       LIMPA-RODAPE.
           MOVE TPL-TRL TO SCR-TRL.
      *
       LIMPA-CORPO.
           MOVE TPL-CTR TO SCR-CTR

           STRING
              '------------------------------'
              ' REGISTROS INATIVOS '
              '------------------------------'
           DELIMITED BY SIZE
           INTO SCR-CTR-L0

           MOVE 'ID      :' TO SCR-CTR-L2.
      *------------------------ FIM DO ARQUIVO -------------------------
