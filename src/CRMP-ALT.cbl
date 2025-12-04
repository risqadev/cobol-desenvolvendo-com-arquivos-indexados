      *=================================================================
       IDENTIFICATION                                         DIVISION.
       PROGRAM-ID. ALTERACAO.
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
       COPY 'CRMK-WRK'.
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
           END-IF

           DISPLAY SCR-CTR

           MOVE 'ARQCRM ABERTO' TO SCR-TRL-L1
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
                 IF ARQCRM-ATIVO
                    PERFORM ALTERAR
                 ELSE
                    PERFORM ERRO-REGISTRO-INATIVO
                 END-IF
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
       ALTERAR                                                SECTION.
      *----------------------------------
           MOVE 'Deseja alterar? (S/N)' TO SCR-TRL-L2
           DISPLAY SCR-TRL

           ACCEPT WRK-OPCAO                        LINE 24 COLUMN 25
           MOVE FUNCTION UPPER-CASE(WRK-OPCAO) TO WRK-OPCAO

           IF WRK-OPCAO NOT EQUAL 'S'
              PERFORM VOLTAR-MENU
           END-IF

           MOVE ARQCRM-REGISTRO TO WRK-ARQCRM-REGISTRO

           PERFORM FORMULARIO

           PERFORM LIMPA-RODAPE
           MOVE 'Confirma a alteracao? (S/N)' TO SCR-TRL-L1
           DISPLAY SCR-TRL

           ACCEPT WRK-OPCAO                        LINE 23 COLUMN 30
           MOVE FUNCTION UPPER-CASE(WRK-OPCAO) TO WRK-OPCAO

           IF WRK-OPCAO NOT EQUAL 'S'
              MOVE 'Registro NAO ALTERADO.' TO SCR-TRL-L2
              DISPLAY SCR-TRL
              PERFORM VOLTAR-MENU
           END-IF

           PERFORM GRAVA-REGISTRO

           EVALUATE ARQCRM-FS
              WHEN 0
                 MOVE 'Registro ALTERADO' TO SCR-TRL-L2
                 DISPLAY SCR-TRL
                 ACCEPT WRK-OPCAO                  LINE 24 COLUMN 20
              WHEN OTHER
                 PERFORM ERRO-GRAVACAO-REGISTRO
           END-EVALUATE.
      *-----------------------------------------------------------------
       FORMULARIO                                             SECTION.
      *----------------------------------
           PERFORM LIMPA-CORPO

           STRING 'ID      : ' WRK-ARQCRM-ID
              DELIMITED BY SIZE
              INTO SCR-CTR-L2
           MOVE 'Nome    : ' TO SCR-CTR-L3
           MOVE 'E-mail  : ' TO SCR-CTR-L4
           MOVE 'Telefone: ' TO SCR-CTR-L5
           DISPLAY SCR-CTR

           ACCEPT WRK-ARQCRM-NOME                    LINE 10 COLUMN 11
           ACCEPT WRK-ARQCRM-EMAIL                   LINE 11 COLUMN 11
           ACCEPT WRK-ARQCRM-TELEFONE                LINE 12 COLUMN 11.
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
       ERRO-REGISTRO-INATIVO.
           PERFORM LIMPA-RODAPE
           MOVE 'Cadastro INATIVO. Nao e possivel altera-lo.'
                                                  TO SCR-TRL-L1
           DISPLAY SCR-TRL
           ACCEPT WRK-OPCAO                          LINE 23 COLUMN 45.
      *
       ERRO-OUTRO-LE-ARQUIVO.
           PERFORM LIMPA-RODAPE
           MOVE 'Erro na busca do registro' TO SCR-TRL-L1
           DISPLAY SCR-TRL
           ACCEPT WRK-OPCAO                          LINE 23 COLUMN 27.
      *
       ERRO-GRAVACAO-REGISTRO.
           PERFORM LIMPA-RODAPE

           STRING
              'Erro na gravacao do registro - ARQCRM-FS: '
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
      *
       GRAVA-REGISTRO.
           MOVE WRK-ARQCRM-REGISTRO TO ARQCRM-REGISTRO
           REWRITE ARQCRM-REGISTRO.
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
