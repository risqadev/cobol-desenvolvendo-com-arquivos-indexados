       01  ARQCRM-REGISTRO.
           05 ARQCRM-KEY.
              10 ARQCRM-ID         PIC X(04) VALUE SPACES.
           05 ARQCRM-NOME          PIC X(25) VALUE SPACES.
           05 ARQCRM-EMAIL         PIC X(30) VALUE SPACES.
           05 ARQCRM-TELEFONE      PIC X(14) VALUE SPACES.
           05 ARQCRM-STATUS        PIC 9(01) VALUE ZERO.
              88 ARQCRM-ATIVO                VALUE 0.
              88 ARQCRM-INATIVO              VALUE 1.
