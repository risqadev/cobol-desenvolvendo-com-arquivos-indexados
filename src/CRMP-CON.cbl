      *=================================================================
       IDENTIFICATION                                         DIVISION.
       PROGRAM-ID. CONSULTA.
      *=================================================================
       ENVIRONMENT                                            DIVISION.
      *-----------------------------------------------------------------
       INPUT-OUTPUT                                           SECTION.
      *----------------------------------
       FILE-CONTROL.
           SELECT ARQCRM  ASSIGN TO './files/ARQCRM.DAT'
              ORGANIZATION IS INDEXED
              ACCESS MODE IS RANDOM
              FILE STATUS IS ARQCRM-FS
              RECORD KEY IS ARQCRM-KEY.
      *=================================================================
       DATA                                                   DIVISION.
      *-----------------------------------------------------------------
       FILE                                                   SECTION.
      *----------------------------------
       FD  ARQCRM.
       COPY 'CRMK-000'.
      *-----------------------------------------------------------------
       WORKING-STORAGE                                        SECTION.
      *----------------------------------
       01  ARQCRM-FS            PIC 9(02)   VALUE ZERO.
       01  WRK-OPCAO            PIC X(01).
       01  WRK-TXT-STATUS       PIC X(10)   VALUE SPACES.
      *-----------------------------------------------------------------
       SCREEN                                                 SECTION.
      *----------------------------------
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

           PERFORM ABRE-ARQUIVO

           IF ARQCRM-FS NOT EQUAL ZERO
              PERFORM ERRO-ABRE-ARQUIVO
           END-IF.

           DISPLAY SCR-HDR
           DISPLAY SCR-CTR

           MOVE 'ARQCRM ABERTO' TO SCR-TRL-L1.
           DISPLAY SCR-TRL.
      *-----------------------------------------------------------------
       PROCESSAR                                              SECTION.
      *----------------------------------
           PERFORM LIMPA-CORPO
           DISPLAY SCR-CTR

           ACCEPT ARQCRM-ID                          LINE 9  COLUMN 11

           PERFORM LE-ENTRADA

           EVALUATE ARQCRM-FS
              WHEN 0
                 PERFORM EXIBE-INFORMACOES
              WHEN 23
                 PERFORM ERRO-REGISTRO-NAO-ENCONTRADO
              WHEN OTHER
                 PERFORM ERRO-OUTRO-LE-ARQUIVO
           END-EVALUATE.
      *-----------------------------------------------------------------
       EXIBE-INFORMACOES                                  SECTION.
      *----------------------------------
           PERFORM LIMPA-RODAPE
           MOVE 'Registro encontrado.' TO SCR-TRL-L1
           DISPLAY SCR-TRL

           PERFORM PREENCHE-CAMPOS
           DISPLAY SCR-CTR.

           ACCEPT WRK-OPCAO                          LINE 23 COLUMN 22.
      *-----------------------------------------------------------------
       PREENCHE-CAMPOS                                        SECTION.
      *----------------------------------
           STRING 'ID      : ' ARQCRM-ID
              DELIMITED BY SIZE
              INTO SCR-CTR-L2

           STRING 'Nome    : ' ARQCRM-NOME
              DELIMITED BY SIZE
              INTO SCR-CTR-L3

           STRING 'E-mail  : ' ARQCRM-EMAIL
              DELIMITED BY SIZE
              INTO SCR-CTR-L4

           STRING 'Telefone: ' ARQCRM-TELEFONE
              DELIMITED BY SIZE
              INTO SCR-CTR-L5

           EVALUATE ARQCRM-STATUS
              WHEN 0
                 MOVE 'ATIVO'   TO WRK-TXT-STATUS
              WHEN 1
                 MOVE 'INATIVO' TO WRK-TXT-STATUS
           END-EVALUATE

           STRING 'Status  : ' WRK-TXT-STATUS
              DELIMITED BY SIZE
              INTO SCR-CTR-L6.
      *-----------------------------------------------------------------
       ERROS                                                  SECTION.
      *----------------------------------
       ERRO-ABRE-ARQUIVO.
           PERFORM LIMPA-RODAPE
           MOVE 'ERRO ABERTURA ARQCRM' TO SCR-TRL-L1
           PERFORM ABENDA.
      *
       ERRO-REGISTRO-NAO-ENCONTRADO.
           PERFORM LIMPA-RODAPE
           MOVE 'ID nao encontrado' TO SCR-TRL-L1
           DISPLAY SCR-TRL
           ACCEPT WRK-OPCAO                          LINE 23 COLUMN 19.
      *
       ERRO-OUTRO-LE-ARQUIVO.
           PERFORM LIMPA-RODAPE
           MOVE 'Erro na busca do registro' TO SCR-TRL-L1
           DISPLAY SCR-TRL
           ACCEPT WRK-OPCAO                          LINE 23 COLUMN 27.
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
           OPEN INPUT ARQCRM.
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
              '-----------------------------------'
              ' ALTERACAO '
              '-----------------------------------'
           DELIMITED BY SIZE
           INTO SCR-CTR-L0

           MOVE 'ID      :' TO SCR-CTR-L2.
      *------------------------ FIM DO ARQUIVO -------------------------
