      *=================================================================
       IDENTIFICATION                                         DIVISION.
       PROGRAM-ID. CADASTRO.
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

           IF ARQCRM-FS NOT EQUAL 0
              PERFORM ERRO-ABRE-ARQUIVO
           END-IF

           DISPLAY SCR-HDR
           DISPLAY SCR-CTR

           MOVE 'ARQCRM ABERTO' TO SCR-TRL-L1
           DISPLAY SCR-TRL.
      *-----------------------------------------------------------------
       PROCESSAR                                             SECTION.
      *----------------------------------
           PERFORM LIMPA-CORPO
           DISPLAY SCR-CTR

           PERFORM FORMULARIO

           PERFORM LIMPA-RODAPE
           MOVE 'Deseja gravar o registro? (S/N)'    TO SCR-TRL-L1
           DISPLAY SCR-TRL

           ACCEPT WRK-OPCAO                          LINE 23 COLUMN 33
           MOVE FUNCTION UPPER-CASE(WRK-OPCAO) TO WRK-OPCAO

           PERFORM LIMPA-RODAPE

           IF WRK-OPCAO NOT EQUAL 'S'
              MOVE 'Registro NAO gravado.'  TO SCR-TRL-L1
              DISPLAY SCR-TRL
              ACCEPT WRK-OPCAO                       LINE 23 COLUMN 23
              PERFORM VOLTAR-MENU
           END-IF

           MOVE WRK-ARQCRM-REGISTRO TO ARQCRM-REGISTRO
           WRITE ARQCRM-REGISTRO

           EVALUATE ARQCRM-FS
              WHEN 0
                 MOVE 'Registro gravado.'   TO SCR-TRL-L1
                 DISPLAY SCR-TRL
              WHEN 22
                 PERFORM ERRO-ID-DUPLICADO
              WHEN OTHER
                 PERFORM ERRO-GRAVACAO-REGISTRO
           END-EVALUATE.
      *-----------------------------------------------------------------
       FORMULARIO                                             SECTION.
      *----------------------------------
           PERFORM LIMPA-CORPO

           MOVE 'Nome    : ' TO SCR-CTR-L3
           MOVE 'E-mail  : ' TO SCR-CTR-L4
           MOVE 'Telefone: ' TO SCR-CTR-L5
           DISPLAY SCR-CTR

           ACCEPT WRK-ARQCRM-ID                      LINE 9  COLUMN 11
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
       ERRO-ID-DUPLICADO.
           PERFORM LIMPA-RODAPE
           MOVE 'ID já registrado' TO SCR-TRL-L1
           DISPLAY SCR-TRL
           ACCEPT WRK-OPCAO                          LINE 23 COLUMN 18.
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
           PERFORM LIMPA-RODAPE
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
       EXCLUI-REGISTRO.
           DELETE ARQCRM.
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
              ' CADASTRO '
              '-----------------------------------'
           DELIMITED BY SIZE
           INTO SCR-CTR-L0

           MOVE 'ID      : ' TO SCR-CTR-L2.
      *------------------------ FIM DO ARQUIVO -------------------------
