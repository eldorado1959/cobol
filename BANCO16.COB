
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    BANCO16.
       AUTHOR. ROGERIO-MACHADO.

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BANCO     ASSIGN TO DISK
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS NUM-CHE
                  FILE STATUS  IS FS.

       DATA DIVISION.
       FILE SECTION.
       FD BANCO             LABEL RECORD IS STANDARD
                               VALUE OF FILE-ID IS "BCO.DAT".
       01 REG-PROD.
           03 NUM-CHE          PIC X(06).
           03 CONTA-CHE        PIC X(10).
           03 DESCRI-CHE       PIC X(05).
           03 DESCRI2-CHE      PIC X(15).
           03 DIA-CHE          PIC 99.
           03 MES-CHE          PIC 99.
           03 ANO-CHE          PIC 99.
           03 VALOR-CHE        PIC 9(06)V99.
           03 OBS-CHE          PIC X(15).
           03 INSCR-CHE        PIC 9(06).
           03 DATA-CONF        PIC 9(06).

      ***********************************
       WORKING-STORAGE SECTION.
       77 FS                   PIC XX.
       77 WS-LIMPA             PIC X(50) VALUE SPACES.
       77 WS-COD               PIC X(08) VALUE SPACES.
       77 WS-CODIGO            PIC 9(08) VALUE ZEROS.
       77 WS-OPCAO             PIC X(01) VALUE SPACES.
       77 WS-TOTAL-IMPRESSOS   PIC 99 VALUE ZEROS.
       77 ws-p                 PIC X.
       77 WS-DESCRI            PIC X(05) VALUE SPACES.
       77 WS-QUANT             PIC 9(03) VALUE ZEROS.
       77 WS-DATA-C            PIC 9(06) VALUE ZEROS.
       77 WS-MOSTRA-CODIGO     PIC ZZZZZZZ9 VALUE SPACES.
       77 WS-MOSTRA-VALOR      PIC ZZZ.ZZ9,99 VALUE ZEROS.
       77 WS-TOTAL-VALOR       PIC 9(07)V99 VALUE ZEROS.

       01 WS-DATA.
           03 ANO              PIC 99.
           03 MES              PIC 99.
           03 DIA              PIC 99.

       01 WS-DATA-AUX.
           03 DIA-A            PIC 99.
           03 MES-A            PIC 99.
           03 ANO-A            PIC 99.

       SCREEN SECTION.
       01 TELA.
           02 BLANK SCREEN.
           02 LINE 02 COLUMN 67 VALUE "       /  /  .".
           02 LINE 02 COLUMN 25 VALUE " E l d o r a d o " BLINK.
           02 LINE 01 COLUMN 70 VALUE "Banco16" BLINK.
       01 TELA-2.
          
           02 LINE 02 COLUMN 30 VALUE "Controle de Titulos".
           02 LINE 03 COLUMN 30 VALUE "  Data de Pgto".
           02 LINE 04 COLUMN 01 VALUE "|Depto    | No.Titulo |".
           02 LINE 04 COLUMN 15 VALUE "|   Favorecido ".
           02 LINE 04 COLUMN 33 VALUE "     Valor        Vcto.|".
      ********************************************

       PROCEDURE DIVISION.

       ABRIR.
           MOVE ZEROS TO WS-TOTAL-VALOR.
           MOVE ZEROS TO WS-TOTAL-IMPRESSOS.
           OPEN INPUT BANCO.

       P-DESCRI-CHE.
           DISPLAY TELA.
           PERFORM P-DATA.
           MOVE 5 TO LIN.
           DISPLAY (08 14) "!  Consulta Titulos p/data de pgto.  !". 
           DISPLAY (10 16) "Informe data de pgto do Titulo ".
           DISPLAY (11 16) "[      ]".
           ACCEPT (11 17) WS-DATA-C WITH PROMPT AUTO-SKIP.
           IF WS-DATA-C = ZEROS
               CLOSE BANCO
               CHAIN "BANCO.COM". 
           DISPLAY (01 01) ERASE. 
           DISPLAY TELA-2.          
       LER.
           READ BANCO NEXT RECORD
                AT END
                    PERFORM P-FINAL-IMP
                    GO TO P-DESCRI-CHE.
           IF CONTA-CHE NOT = "TITULOS"
               GO TO LER.
      *    MOVE DIA-CHE TO DIA-A.
      *    MOVE MES-CHE TO MES-A.
      *    MOVE ANO-CHE TO ANO-A.
           IF DATA-CONF NOT = WS-DATA-C
               GO TO LER.
           PERFORM P-MOSTRA.
           GO TO LER.   

      *************************************************

       P-DATA.
           ACCEPT WS-DATA FROM DATE.
           DISPLAY (02 72) DIA.
           DISPLAY (02 75) MES.
           DISPLAY (02 78) ANO.

       P-FINAL-IMP.
           CLOSE BANCO.
           DISPLAY (21 63) WS-TOTAL-IMPRESSOS " Titulos".
           MOVE WS-TOTAL-VALOR TO WS-MOSTRA-VALOR.
           DISPLAY (22 63) "R$ " WS-MOSTRA-VALOR.
           stop " ".
           CHAIN "BANCO16.COM".

       P-ERRO-LEITURA.
           DISPLAY (12 20) "!!!!!  CHAVE INVALIDA  !!!!!".
           STOP RUN.

       P-MOSTRA.
           ADD 1 TO LIN.

           IF OBS-CHE = 01 DISPLAY (lin , 01) "LOJA".
           IF OBS-CHE = 02 DISPLAY (lin , 01) "ELETRO".
           IF OBS-CHE = 03 DISPLAY (lin , 01) "CALCADOS".
           IF OBS-CHE = 04 DISPLAY (lin , 01) "CONFEC.".
           IF OBS-CHE = 05 DISPLAY (lin , 01) "MERCADO".
           IF OBS-CHE = 06 DISPLAY (lin , 01) "FERRAGEM".
           IF OBS-CHE = 07 DISPLAY (lin , 01) "OUTROS".
           IF OBS-CHE = 08 DISPLAY (lin , 01) "DESPESAS".


           MOVE NUM-CHE TO WS-MOSTRA-CODIGO.
           DISPLAY (LIN , 08) WS-MOSTRA-CODIGO.
           DISPLAY (LIN , 20) DESCRI-CHE DESCRI2-CHE.
           MOVE VALOR-CHE TO WS-MOSTRA-VALOR. 
           ADD VALOR-CHE TO WS-TOTAL-VALOR.
           DISPLAY (LIN , 35) "              ".
           DISPLAY (LIN , 35) WS-MOSTRA-VALOR.
           DISPLAY (LIN , 50) DIA-CHE "/" MES-CHE "/" ANO-CHE.
           IF DATA-CONF = 0 
               DISPLAY (LIN , 62) "NAO Pg" 
           ELSE
               DISPLAY (LIN , 60) "Pg." DATA-CONF.
           DISPLAY (LIN , 71) "Rec." INSCR-CHE.
           ADD 1 TO WS-TOTAL-IMPRESSOS.
           IF LIN > 18 PERFORM P-PARA.

       P-PARA.
           DISPLAY (22 25) "< ENTER > Continua".
           DISPLAY (23 25) "  < N > Encerra".
           ACCEPT (22 29) WS-P WITH AUTO-SKIP.
           DISPLAY (22 25) WS-LIMPA.
           DISPLAY (23 25) WS-LIMPA.
           if ws-p = "N" or "n" perform p-final-imp.                     
      *     DISPLAY (06 00) ERASE. 
           MOVE 6 TO LIN.


