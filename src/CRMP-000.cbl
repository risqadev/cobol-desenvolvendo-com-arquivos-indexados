      *=================================================================
       IDENTIFICATION                                         DIVISION.
       PROGRAM-ID. CRM.
      *=================================================================
       ENVIRONMENT                                            DIVISION.
      *=================================================================
       DATA                                                   DIVISION.
      *-----------------------------------------------------------------
       WORKING-STORAGE                                        SECTION.
      *----------------------------------
       01  WRK-OPCAO   PIC X          VALUE SPACE.
      *-----------------------------------------------------------------
       SCREEN                                                 SECTION.
      *----------------------------------
       COPY 'CRMK-SCR'.
      *=================================================================
       PROCEDURE                                              DIVISION.
      *-----------------------------------------------------------------
       PRINCIPAL                                              SECTION.
      *----------------------------------
           DISPLAY SCR-HDR
           DISPLAY SCR-CTR

           PERFORM EXIBE-MENU UNTIL WRK-OPCAO EQUAL 9

           STOP RUN.
      *-----------------------------------------------------------------
       EXIBE-MENU                                             SECTION.
      *----------------------------------
           PERFORM LIMPA-CORPO

           MOVE '1. Cadastro'         TO SCR-CTR-L2
           MOVE '2. Consulta'         TO SCR-CTR-L3
           MOVE '3. Alteracao'        TO SCR-CTR-L4
           MOVE '4. Exclusao'         TO SCR-CTR-L5
           MOVE '5. Ativar/Inativar'  TO SCR-CTR-L6
           MOVE '6. Rel. Ativos'      TO SCR-CTR-L7
           MOVE '7. Rel. Inativos'    TO SCR-CTR-L8
           MOVE '8. Setup Sistema'    TO SCR-CTR-L9
           MOVE '9. Encerrar'         TO SCR-CTR-L10
           MOVE 'OPCAO:'              TO SCR-CTR-L13

           DISPLAY SCR-CTR
           DISPLAY SCR-TRL-L0

           ACCEPT WRK-OPCAO                           LINE 20 COLUMN 8
           MOVE FUNCTION UPPER-CASE(WRK-OPCAO) TO WRK-OPCAO

           EVALUATE WRK-OPCAO
              WHEN 1
                 CALL 'CADASTRO'
              WHEN 2
                 CALL 'CONSULTA'
              WHEN 3
                 CALL 'ALTERACAO'
              WHEN 4
                 CALL 'EXCLUSAO'
              WHEN 5
                 CALL 'SITUACAO'
              WHEN 6
                 CALL 'LISTA-A'
              WHEN 7
                 CALL 'LISTA-I'
              WHEN 8
                 CALL 'MANUT'
              WHEN 9
                 CONTINUE
              WHEN OTHER
                 PERFORM ERRO-OPCAO-INVALIDA
           END-EVALUATE.
      *-----------------------------------------------------------------
       UTILITARIOS                                            SECTION.
      *----------------------------------
       LIMPA-RODAPE.
           MOVE TPL-TRL TO SCR-TRL.
      *
       LIMPA-CORPO.
           MOVE TPL-CTR TO SCR-CTR

           STRING
              '-------------------------------------'
              ' MENU '
              '-------------------------------------'
           DELIMITED BY SIZE
           INTO SCR-CTR-L0.
      *-----------------------------------------------------------------
       ERROS                                                  SECTION.
      *----------------------------------
       ERRO-OPCAO-INVALIDA.
           PERFORM LIMPA-RODAPE
           MOVE 'OPCAO INVALIDA' TO SCR-TRL-L1
           DISPLAY SCR-TRL.
      *------------------------ FIM DO ARQUIVO ------------------------*
