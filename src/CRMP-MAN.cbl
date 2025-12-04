      *=================================================================
       IDENTIFICATION                                         DIVISION.
       PROGRAM-ID. MANUT.
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
           PERFORM VOLTAR-MENU.
      *-----------------------------------------------------------------
       INICIAR                                                SECTION.
      *----------------------------------
           PERFORM LIMPA-RODAPE

           OPEN INPUT ARQCRM

           PERFORM UNTIL ARQCRM-FS EQUAL 0
              EVALUATE ARQCRM-FS
                  WHEN 35
                    MOVE 'ARQUIVO INEXISTENTE... CRIANDO...'
                                                          TO SCR-TRL-L1
                    DISPLAY SCR-TRL
                    OPEN OUTPUT ARQCRM
                  WHEN OTHER
                    PERFORM ERRO-CRIACAO-ARQUIVO
              END-EVALUATE
           END-PERFORM

           MOVE 'ARQUIVO EXISTENTE.'  TO SCR-TRL-L2
           DISPLAY SCR-TRL.
      *-----------------------------------------------------------------
       ERROS                                                  SECTION.
      *----------------------------------
       ERRO-CRIACAO-ARQUIVO.
           STRING
              'Erro na gravacao do registro - ARQCRM-FS: '
              ARQCRM-FS
           DELIMITED BY SIZE
           INTO SCR-TRL-L2

           DISPLAY SCR-TRL

           PERFORM ABENDA.
      *-----------------------------------------------------------------
       FINALIZAR                                              SECTION.
      *----------------------------------
       ABENDA.
           MOVE 'FIM ANORMAL DO PROGRAMA' TO SCR-TRL-L3
           DISPLAY SCR-TRL

           PERFORM VOLTAR-MENU.
      *
       VOLTAR-MENU.
           CLOSE ARQCRM
           GOBACK.
      *-----------------------------------------------------------------
       UTILITARIOS                                            SECTION.
      *----------------------------------
       LIMPA-RODAPE.
           MOVE TPL-TRL TO SCR-TRL.
      *------------------------ FIM DO ARQUIVO ------------------------*
