
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    BANCO07.
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
       77 WS-TOTAL-IMPRESSOS   PIC 9(03) VALUE ZEROS.
       77 ws-p                 PIC X.
       77 WS-DESCRI            PIC X(05) VALUE SPACES.
       77 WS-QUANT             PIC 9(03) VALUE ZEROS.
       77 WS-MOSTRA-CODIGO     PIC ZZZZZZZ9 VALUE SPACES.
       77 WS-MOSTRA-VALOR      PIC ZZZ.ZZ9,99 VALUE ZEROS.

       01 WS-DATA.
           03 ANO              PIC 99.
           03 MES              PIC 99.
           03 DIA              PIC 99.

       SCREEN SECTION.
       01 TELA.
           02 BLANK SCREEN.
           02 LINE 02 COLUMN 67 VALUE "DATA:  /  /  .".
           02 LINE 02 COLUMN 25 VALUE " E L D O R A D O " BLINK.
           02 LINE 01 COLUMN 70 VALUE "BANCO07" BLINK.
       01 TELA-2.
          
           02 LINE 02 COLUMN 40 VALUE "CONTROLE DE CHEQUES".
           02 LINE 03 COLUMN 04 VALUE "|NRO CHEQUE|".
           02 LINE 03 COLUMN 15 VALUE "|   FAVORECIDO ".
           02 LINE 03 COLUMN 33 VALUE "  VALOR  == CONTA     SITUACAO|".
           02 LINE 04 COLUMN 01 VALUE "                         " BLINK.
           02 LINE 04 COLUMN 20 VALUE "                         " BLINK.
           02 LINE 04 COLUMN 40 VALUE "                         " BLINK.
      ********************************************

       PROCEDURE DIVISION.

       ABRIR.
           OPEN INPUT BANCO. 

       P-DESCRI-CHE.
           DISPLAY TELA.
           PERFORM P-DATA.
           MOVE 5 TO LIN.
           DISPLAY (08 14) "!  C O N S U L T A  D E  C H E Q U E S  !". 
           DISPLAY (10 16) "Informe o FAVORECIDO DO CHEQUE ".
           DISPLAY (11 16) "[     ]".
           ACCEPT (11 17) WS-DESCRI WITH PROMPT.
           IF WS-DESCRI = SPACES
               CLOSE BANCO
               CHAIN "C:BANCO.COM". 
           DISPLAY (01 01) ERASE. 
           DISPLAY TELA-2.          
       LER.
           READ BANCO NEXT RECORD
                AT END
                    PERFORM P-FINAL-IMP
                    GO TO P-DESCRI-CHE. 
           IF WS-DESCRI NOT = DESCRI-CHE
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
           DISPLAY (18 50) WS-TOTAL-IMPRESSOS " CHEQUES LISTADOS".
           DISPLAY (19 50) "  Tecle < ENTER >  ".
           CHAIN "C:BANCONF.COM".

       P-ERRO-LEITURA.
           DISPLAY (12 20) "!!!!!  CHAVE INVALIDA  !!!!!".
           STOP RUN.

       P-MOSTRA.
           ADD 1 TO LIN.
           MOVE NUM-CHE TO WS-MOSTRA-CODIGO.
           DISPLAY (LIN , 03) WS-MOSTRA-CODIGO.
           DISPLAY (LIN , 21) DESCRI-CHE DESCRI2-CHE.
           DISPLAY (LIN , 44) CONTA-CHE.
           MOVE VALOR-CHE TO WS-MOSTRA-VALOR. 
           DISPLAY (LIN , 29) WS-MOSTRA-VALOR.
           IF DATA-CONF = 0 
               DISPLAY (LIN , 56) "NAO conferido" 
           ELSE
               DISPLAY (LIN , 56) "Cheque conferido " DATA-CONF.
           ADD 1 TO WS-TOTAL-IMPRESSOS.
           IF LIN > 18 PERFORM P-PARA.

       P-PARA.
           DISPLAY (18 25) "< ENTER > Continua".
           DISPLAY (19 25) "  < N > Encerra".
           ACCEPT (19 29) WS-P WITH AUTO-SKIP.
           DISPLAY (18 25) "                  ".
           DISPLAY (19 25) "               ".
           if ws-p = "N" or "n" perform p-final-imp.                     
           DISPLAY (06 00) ERASE. 
           MOVE 6 TO LIN.


