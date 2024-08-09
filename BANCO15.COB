
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    BANCO15.
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

           SELECT RELATO    ASSIGN TO PRINTER.

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

       FD RELATO            LABEL RECORD IS OMITTED.
       01 REG-RELATO           PIC X(80).

      ***********************************
       WORKING-STORAGE SECTION.
       77 FS                   PIC XX.
       77 WS-LIMPA             PIC X(50) VALUE SPACES.
       77 WS-COD               PIC X(08) VALUE SPACES.
       77 WS-CODIGO            PIC 9(08) VALUE ZEROS.
       77 WS-OPCAO             PIC X(01) VALUE SPACES.
       77 WS-TOTAL-IMPRESSOS   PIC 9(03) VALUE ZEROS.
       77 ws-p                 PIC X.
       77 ws-conf              PIC X.
       77 WS-DESCRI            PIC X(05) VALUE SPACES.
       77 WS-QUANT             PIC 9(03) VALUE ZEROS.
       77 WS-MOSTRA-CODIGO     PIC X(08) VALUE SPACES.
      * 77 WS-MOSTRA-CODIGO     PIC ZZZZZZZ9 VALUE SPACES.
       77 WS-MOSTRA-VALOR      PIC ZZZ.ZZ9,99 VALUE ZEROS.
       77 WS-TOTAL-VALOR       PIC 9(07)V99 VALUE ZEROS.
       77 WS-CONTA             PIC X(10) VALUE SPACES.
       77 WS-NUM-CONTA         PIC 9 VALUE ZEROS.

       01 WS-DATA.
           03 ANO              PIC 99.
           03 MES              PIC 99.
           03 DIA              PIC 99.

       01 WS-DATA-C.
           03 ANO-C            PIC 99.
           03 MES-C            PIC 99.
           03 DIA-C            PIC 99.

       01 WS-DATA-R.
           03 ANO-R            PIC 99.
           03 MES-R            PIC 99.
           03 DIA-R            PIC 99.

       01 LINHA-CABEC1.
           02 f pic x(02) value spaces.
           02 F PIC X(34) VALUE "Controle Bancario  E l d o r a d o".  
           02 f pic x(02) value spaces.
           02 DIA-DET PIC 99.
           02 f pic x value "/".
           02 MES-DET PIC 99.
           02 f pic x value "/".
           02 ANO-DET PIC 99.
           02 f pic x value ".".

       01 LINHA-DATA.
           02 f pic x(02) value spaces.
           02 F PIC X(23) VALUE "TOTAL CHEQUE/TIT. ATE :".  
           02 DIA-DATA PIC 99.
           02 F PIC X(01) VALUE "/".
           02 MES-DATA PIC 99.
           02 F PIC X(01) VALUE "/".
           02 ANO-DATA PIC 99.
           02 f pic x(02) value spaces.
           02 NOME-CONTA pic x(10). 
           
       01 LINHA-DET1.
           02 f pic x(02) value spaces.
           02 CONTCHEQ-DET PIC ZZ9.
           02 F PIC X(22) VALUE "CHEQ/TITULOS A ENTRAR ".  
           02 F PIC X(15) VALUE "NO VALOR DE R$ ".  
           02 VALTOTCHEQ-DET PIC ZZZ.ZZ9,99.

       01 LINHA-TRACO.
           02 f pic x(60) value ALL "-".

       01 LINHA-LIMPA.
           02 f pic x(80) value ALL " ".

       SCREEN SECTION.
       01 TELA.
           02 BLANK SCREEN.
           02 LINE 02 COLUMN 67 VALUE "DATA:  /  /  .".
           02 LINE 02 COLUMN 25 VALUE " E L D O R A D O " BLINK.
           02 LINE 01 COLUMN 70 VALUE "BANCO10" BLINK.
       01 TELA-2.
           02 LINE 02 COLUMN 40 VALUE "CONTROLE CHEQUE/TIT.".
           02 LINE 03 COLUMN 04 VALUE "|NRO CHEQ/TIT|".
           02 LINE 03 COLUMN 15 VALUE "|   FAVORECIDO   VALOR        ".
           02 LINE 03 COLUMN 43 VALUE "   DATA  == CONTA|".
           02 LINE 04 COLUMN 01 VALUE "                         " BLINK.
           02 LINE 04 COLUMN 20 VALUE "                         " BLINK.
           02 LINE 04 COLUMN 40 VALUE "                         " BLINK.

       01 TELA-CONTA.
           02 LINE 12 COLUMN 50 VALUE "[1] LIANE CIA".
           02 LINE 13 COLUMN 50 VALUE "[2] FERNANDO ".
           02 LINE 14 COLUMN 50 VALUE "[3] ROGERIO  ".
           02 LINE 15 COLUMN 50 VALUE "[4] TITULOS  ".

       01 TELA-LIMPA-CONTA.
           02 LINE 12 COLUMN 50 VALUE "             ".
           02 LINE 13 COLUMN 50 VALUE "             ".
           02 LINE 14 COLUMN 50 VALUE "             ".
           02 LINE 15 COLUMN 50 VALUE "             ".
      ********************************************

       PROCEDURE DIVISION.

       ABRIR.
           OPEN INPUT BANCO. 
           OPEN OUTPUT RELATO. 

       P-DESCRI-CHE.
           MOVE ZEROS TO WS-TOTAL-IMPRESSOS.
           MOVE ZEROS TO WS-TOTAL-VALOR.
           DISPLAY TELA.
           PERFORM P-DATA.
           MOVE 5 TO LIN.
           DISPLAY (08 14) "! CONFERENCIA CHEQ.TIT. ATE DET. DATA :". 
           DISPLAY (10 16) "Informe a DATA DESEJADA ".
           DISPLAY (11 15) "  /  /  .".
           ACCEPT (11 15) DIA-C WITH PROMPT AUTO-SKIP.
           ACCEPT (11 18) MES-C WITH PROMPT AUTO-SKIP.
           ACCEPT (11 21) ANO-C WITH PROMPT AUTO-SKIP.
           IF WS-DATA-C = ZEROS 
               CLOSE BANCO
               CHAIN "BANCO.COM". 
           DISPLAY (01 01) ERASE. 
           DISPLAY TELA-2.          
           DISPLAY TELA-CONTA.          
           DISPLAY (12 16) "Informe a CONTA CORRENTE -> [ ]".
           ACCEPT (12 45) WS-NUM-CONTA WITH PROMPT AUTO-SKIP.
           DISPLAY TELA-LIMPA-CONTA.          
           IF WS-NUM-CONTA = 1 MOVE "LIANE CIA" TO WS-CONTA.
           IF WS-NUM-CONTA = 2 MOVE "FERNANDO " TO WS-CONTA.
           IF WS-NUM-CONTA = 3 MOVE "ROGERIO  " TO WS-CONTA.
           IF WS-NUM-CONTA = 4 MOVE "TITULOS  " TO WS-CONTA.
           IF WS-NUM-CONTA = 1 MOVE "LIANE CIA" TO NOME-CONTA.
           IF WS-NUM-CONTA = 2 MOVE "FERNANDO " TO NOME-CONTA.
           IF WS-NUM-CONTA = 3 MOVE "ROGERIO  " TO NOME-CONTA.
           IF WS-NUM-CONTA = 4 MOVE "TITULOS  " TO NOME-CONTA.
           DISPLAY (12 16) WS-LIMPA.
  
       LER.
           READ BANCO NEXT RECORD
                AT END
                    PERFORM P-FINAL-IMP
                    GO TO P-DESCRI-CHE. 
           MOVE DIA-CHE TO DIA-R.
           MOVE MES-CHE TO MES-R.
           MOVE ANO-CHE TO ANO-R.
      *     IF WS-DATA-R = WS-DATA-C
      *         GO TO LER.
           IF DATA-CONF > 0
               GO TO LER.
           IF WS-CONTA NOT = CONTA-CHE
               GO TO LER.
           IF DESCRI-CHE = "NULO "  
               GO TO LER.
           IF WS-DATA-R = WS-DATA-C
               PERFORM P-MOSTRA.
           GO TO LER.   

      *************************************************

       P-DATA.
           ACCEPT WS-DATA FROM DATE.
           DISPLAY (02 72) DIA.
           DISPLAY (02 75) MES.
           DISPLAY (02 78) ANO.
           MOVE DIA TO DIA-DET. 
           MOVE MES TO MES-DET. 
           MOVE ANO TO ANO-DET.
       P-FINAL-IMP.
           DISPLAY (18 50) WS-TOTAL-IMPRESSOS " CHEQUES LISTADOS".
           MOVE WS-TOTAL-VALOR TO WS-MOSTRA-VALOR. 
           DISPLAY (20 50) "NO TOTAL DE R$: " WS-MOSTRA-VALOR .
      *     ACCEPT (19 49) WS-P.
           MOVE WS-TOTAL-IMPRESSOS TO CONTCHEQ-DET.
           MOVE WS-TOTAL-VALOR TO VALTOTCHEQ-DET.
           MOVE DIA-C TO DIA-DATA.
           MOVE MES-C TO MES-DATA.
           MOVE ANO-C TO ANO-DATA.
           DISPLAY (21 05) "Imprimir ?  < S/N >" .
           ACCEPT (21 25) WS-CONF WITH PROMPT AUTO-SKIP.
           IF WS-CONF = "S" or "s" OR "0"
              PERFORM P-IMPRIME.
           CLOSE BANCO RELATO.
           CHAIN "BANCO.COM".

       P-IMPRIME.
           WRITE REG-RELATO FROM LINHA-TRACO.
           WRITE REG-RELATO FROM LINHA-CABEC1.
           WRITE REG-RELATO FROM LINHA-TRACO.
           WRITE REG-RELATO FROM LINHA-LIMPA.
           WRITE REG-RELATO FROM LINHA-DATA.
           WRITE REG-RELATO FROM LINHA-LIMPA.
           WRITE REG-RELATO FROM LINHA-DET1.
           WRITE REG-RELATO FROM LINHA-LIMPA.
           WRITE REG-RELATO FROM LINHA-LIMPA.
           WRITE REG-RELATO FROM LINHA-TRACO BEFORE ADVANCING 1 LINE.

       P-ERRO-LEITURA.
           DISPLAY (12 20) "!!!!!  CHAVE INVALIDA  !!!!!".
           STOP RUN.

       P-MOSTRA.
           ADD VALOR-CHE TO WS-TOTAL-VALOR
           ADD 1 TO LIN.
           MOVE NUM-CHE TO WS-MOSTRA-CODIGO.
           DISPLAY (LIN , 03) WS-MOSTRA-CODIGO.
           DISPLAY (LIN , 15) DESCRI-CHE DESCRI2-CHE.
           MOVE VALOR-CHE TO WS-MOSTRA-VALOR. 
           DISPLAY (LIN , 32) WS-MOSTRA-VALOR.
           DISPLAY (LIN , 44) DIA-CHE "/" MES-CHE "/" ANO-CHE ".".
           DISPLAY (LIN , 54) CONTA-CHE.
           ADD 1 TO WS-TOTAL-IMPRESSOS.
           IF LIN > 18 PERFORM P-PARA.

       P-PARA.
           DISPLAY (20 25) "< ENTER > Continua".
           DISPLAY (21 25) "  < N > Encerra".
           ACCEPT (21 29) WS-P WITH AUTO-SKIP.
           DISPLAY (20 25) "                  ".
           DISPLAY (21 25) "               ".
           if ws-p = "N" or "n" perform p-final-imp.                     
           DISPLAY (06 00) ERASE. 
           MOVE 6 TO LIN.



