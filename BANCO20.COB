

       IDENTIFICATION DIVISION.
       PROGRAM-ID.    BANCO20.
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

           SELECT SORTEIO      ASSIGN TO DISK
                  SORT STATUS  IS ST.

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

       SD SORTEIO.
       01 REG-SORT.
           03 NUM-CHE-S        PIC X(06).
           03 CONTA-CHE-S      PIC X(10).
           03 DESCRI-CHE-S     PIC X(05).
           03 DESCRI2-CHE-S    PIC X(15).
           03 DIA-CHE-S        PIC 99.
           03 MES-CHE-S        PIC 99.
           03 ANO-CHE-S        PIC 99.
           03 VALOR-CHE-S      PIC 9(06)V99.
           03 OBS-CHE-S        PIC X(15).
           03 INSCR-CHE-S      PIC 9(06).
           03 DATA-CONF-S      PIC 9(06).

      ***********************************
       WORKING-STORAGE SECTION.
       77 ST                   PIC XX.
       77 FS                   PIC XX.
       77 WS-LIMPA             PIC X(50) VALUE SPACES.
       77 WS-COD               PIC X(08) VALUE SPACES.
       77 WS-CODIGO            PIC 9(08) VALUE ZEROS.
       77 WS-OPCAO             PIC X(01) VALUE SPACES.
       77 WS-TOTAL-IMPRESSOS   PIC 9(04) VALUE ZEROS.
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
       77 WS-TOTAL-LOJA        PIC 9(07)V99 VALUE ZEROS.
       77 WS-TOTAL-ELETRO      PIC 9(07)V99 VALUE ZEROS.
       77 WS-TOTAL-CALCADOS    PIC 9(07)V99 VALUE ZEROS.
       77 WS-TOTAL-CONFEC      PIC 9(07)V99 VALUE ZEROS.
       77 WS-TOTAL-MERCADO     PIC 9(07)V99 VALUE ZEROS.
       77 WS-TOTAL-FERRAGEM    PIC 9(07)V99 VALUE ZEROS.
       77 WS-TOTAL-OUTROS      PIC 9(07)V99 VALUE ZEROS.
       77 WS-TOTAL-DESPESAS    PIC 9(07)V99 VALUE ZEROS.
       77 WS-TOTAL-ACOUGUE     PIC 9(07)V99 VALUE ZEROS.

       77 WS-CONTCHEQ-LOJA     PIC 9(03) VALUE ZEROS.
       77 WS-CONTCHEQ-ELETRO   PIC 9(03) VALUE ZEROS.
       77 WS-CONTCHEQ-CALCADOS PIC 9(03) VALUE ZEROS.
       77 WS-CONTCHEQ-CONFEC   PIC 9(03) VALUE ZEROS.
       77 WS-CONTCHEQ-MERCADO  PIC 9(03) VALUE ZEROS.
       77 WS-CONTCHEQ-FERRAGEM PIC 9(03) VALUE ZEROS.
       77 WS-CONTCHEQ-OUTROS   PIC 9(03) VALUE ZEROS.
       77 WS-CONTCHEQ-DESPESAS PIC 9(03) VALUE ZEROS.
       77 WS-CONTCHEQ-ACOUGUE  PIC 9(03) VALUE ZEROS.

       77 WS-ACUM              PIC 9(04)V99 VALUE ZEROS.
       77 WS-PERC              PIC 9(04)V99 VALUE ZEROS.
       77 WS-PERC-LOJA         PIC 9(02)V99 VALUE ZEROS.
       77 WS-PERC-ELETRO       PIC 9(02)V99 VALUE ZEROS.
       77 WS-PERC-CALCADOS     PIC 9(02)V99 VALUE ZEROS.
       77 WS-PERC-CONFEC       PIC 9(02)V99 VALUE ZEROS.
       77 WS-PERC-MERCADO      PIC 9(02)V99 VALUE ZEROS.
       77 WS-PERC-FERRAGEM     PIC 9(02)V99 VALUE ZEROS.
       77 WS-PERC-OUTROS       PIC 9(02)V99 VALUE ZEROS.
       77 WS-PERC-DESPESAS     PIC 9(02)V99 VALUE ZEROS.
       77 WS-PERC-ACOUGUE      PIC 9(02)V99 VALUE ZEROS.
          

       77 WS-DEPTO             PIC 99.
       88 DEPTO-88             VALUE  0 1 2 3 4 5 6 7 8 9.

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
           02 f pic x(01) value spaces.
           02 F PIC X(36) VALUE "Contr.Financ.E l d o r a d o".  
           02 f pic x(01) value spaces.
      *     02 DEPTO-DET1 PIC X(08).
           02 f pic x(02) value spaces.
           02 DIA-DET PIC 99.
           02 f pic x value "/".
           02 MES-DET PIC 99.
           02 f pic x value "/".
           02 ANO-DET PIC 99.
           02 f pic x value ".".

       01 LINHA-DATA.
           02 f pic x(02) value spaces.
           02 F PIC X(20) VALUE "Total Cheq/Tit.ate: ".  
           02 DIA-DATA PIC 99.
           02 F PIC X(01) VALUE "/".
           02 MES-DATA PIC 99.
           02 F PIC X(01) VALUE "/".
           02 ANO-DATA PIC 99.
           02 f pic x(02) value spaces.
           02 NOME-CONTA pic x(10). 
           

       01 LINHA-DET.
           02 f pic x(01) value spaces.
           02 DEPTO-DET PIC X(08).
           02 f pic x(01) value spaces.
           02 CONTA-DET PIC X(10).
           02 f pic x(01) value spaces.
           02 NUM-CHE-DET PIC 9(06).
           02 f pic x(02) value spaces.
           02 DESCRI-CHE-DET PIC X(05).
           02 DESCRI2-CHE-DET PIC X(10).
           02 f pic x(01) value spaces.
           02 DIA-CHE-DET PIC 99.
           02 f pic x(01) value "/".
           02 MES-CHE-DET PIC 99.
           02 f pic x(01) value "/".
           02 ANO-CHE-DET PIC 99.
           02 VALOR-CHE-DET PIC ZZ.ZZ9,99.


       01 LINHA-DET1.
           02 f pic x(02) value spaces.
           02 CONTCHEQ-DET PIC ZZ9.
           02 F PIC X(11) VALUE "Cheq/tit/a ".  
           02 F PIC X(15) VALUE "no valor de R$ ".  
           02 VALTOTCHEQ-DET PIC ZZZ.ZZ9,99.

       01 LINHA-TOTAL-LOJA.
           02 f pic x(02) value spaces.
           02 CONTCHEQ-LOJA-DET PIC ZZ9.
           02 F PIC X(11) VALUE " Cheq/tit/ ".  
           02 F PIC X(12) VALUE "    LOJA R$ ".  
           02 VALTOT-LOJA-DET PIC ZZZ.ZZ9,99.
       01 LINHA-TOTAL-ELETRO.
           02 f pic x(02) value spaces.
           02 CONTCHEQ-ELETRO-DET PIC ZZ9.
           02 F PIC X(11) VALUE " Cheq/tit/ ".  
           02 F PIC X(12) VALUE "  ELETRO R$ ".  
           02 VALTOT-ELETRO-DET PIC ZZZ.ZZ9,99.
       01 LINHA-TOTAL-CALCADOS.
           02 f pic x(02) value spaces.
           02 CONTCHEQ-CALCADOS-DET PIC ZZ9.
           02 F PIC X(11) VALUE " Cheq/tit/ ".  
           02 F PIC X(12) VALUE "CALCADOS R$ ".  
           02 VALTOT-CALCADOS-DET PIC ZZZ.ZZ9,99.
       01 LINHA-TOTAL-CONFEC.
           02 f pic x(02) value spaces.
           02 CONTCHEQ-CONFEC-DET PIC ZZ9.
           02 F PIC X(11) VALUE " Cheq/tit/ ".  
           02 F PIC X(12) VALUE "  CONFEC R$ ".  
           02 VALTOT-CONFEC-DET PIC ZZZ.ZZ9,99.
       01 LINHA-TOTAL-MERCADO.
           02 f pic x(02) value spaces.
           02 CONTCHEQ-MERCADO-DET PIC ZZ9.
           02 F PIC X(11) VALUE " Cheq/tit/ ".  
           02 F PIC X(12) VALUE " MERCADO R$ ".  
           02 VALTOT-MERCADO-DET PIC ZZZ.ZZ9,99.
       01 LINHA-TOTAL-FERRAGEM.
           02 f pic x(02) value spaces.
           02 CONTCHEQ-FERRAGEM-DET PIC ZZ9.
           02 F PIC X(11) VALUE " Cheq/tit/ ".  
           02 F PIC X(12) VALUE "FERRAGEM R$ ".  
           02 VALTOT-FERRAGEM-DET PIC ZZZ.ZZ9,99.
       01 LINHA-TOTAL-OUTROS.
           02 f pic x(02) value spaces.
           02 CONTCHEQ-OUTROS-DET PIC ZZ9.
           02 F PIC X(11) VALUE " Cheq/tit/ ".  
           02 F PIC X(12) VALUE "  OUTROS R$ ".  
           02 VALTOT-OUTROS-DET PIC ZZZ.ZZ9,99.
       01 LINHA-TOTAL-DESPESAS.
           02 f pic x(02) value spaces.
           02 CONTCHEQ-DESPESAS-DET PIC ZZ9.
           02 F PIC X(11) VALUE " Cheq/tit/ ".  
           02 F PIC X(12) VALUE "DESPESAS R$ ".  
           02 VALTOT-DESPESAS-DET PIC ZZZ.ZZ9,99.
       01 LINHA-TOTAL-ACOUGUE.
           02 f pic x(02) value spaces.
           02 CONTCHEQ-ACOUGUE-DET PIC ZZ9.
           02 F PIC X(11) VALUE " Cheq/tit/ ".  
           02 F PIC X(12) VALUE "ACOUGUE  R$ ".  
           02 VALTOT-ACOUGUE-DET PIC ZZZ.ZZ9,99.

       01 LINHA-TRACO.
           02 f pic x(60) value ALL "-".

       01 LINHA-LIMPA.
           02 f pic x(80) value ALL " ".

       SCREEN SECTION.
       01 TELA.
           02 BLANK SCREEN.
           02 LINE 02 COLUMN 67 VALUE "       /  /  .".
           02 LINE 02 COLUMN 25 VALUE " E L D O R A D O " BLINK.
           02 LINE 01 COLUMN 70 VALUE "Relban09" BLINK.
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
           02 LINE 16 COLUMN 50 VALUE "[5] DESPESAS ".

       01 TELA-DEPTO.
           02 LINE 06 COLUMN 50 VALUE "<01> Loja".
           02 LINE 07 COLUMN 50 VALUE "<02> Eletro".
           02 LINE 08 COLUMN 50 VALUE "<03> Calcados".
           02 LINE 09 COLUMN 50 VALUE "<04> Confec".
           02 LINE 10 COLUMN 50 VALUE "<05> Mercado".
           02 LINE 11 COLUMN 50 VALUE "<06> Ferragem".
           02 LINE 12 COLUMN 50 VALUE "<07> Outros  ".
           02 LINE 13 COLUMN 50 VALUE "<08> Despesas".
           02 LINE 14 COLUMN 50 VALUE "<09> Acougue ".

       01 TELA-LIMPA-CONTA.
           02 LINE 12 COLUMN 50 VALUE "             ".
           02 LINE 13 COLUMN 50 VALUE "             ".
           02 LINE 14 COLUMN 50 VALUE "             ".
           02 LINE 15 COLUMN 50 VALUE "             ".
           02 LINE 16 COLUMN 50 VALUE "             ".
      ********************************************

       PROCEDURE DIVISION.

       P01-CLASSIFICA.
           SORT SORTEIO ON ASCENDING KEY 
               OBS-CHE-S CONTA-CHE-S DESCRI-CHE-S
               USING BANCO OUTPUT PROCEDURE IS SAIDA.

       SAIDA SECTION.

       ABRIR.
           OPEN INPUT BANCO. 

       P-DESCRI-CHE.
           MOVE ZEROS TO WS-TOTAL-IMPRESSOS.
           MOVE ZEROS TO WS-TOTAL-VALOR.
           DISPLAY TELA.
           PERFORM P-DATA.
           MOVE 5 TO LIN.
           DISPLAY (12 14) "! imprime CHEQ.TIT. ATE DET. DATA :". 
           DISPLAY (13 16) "Informe a DATA DESEJADA ".
           DISPLAY (14 15) "  /  /  .".
           ACCEPT (14 15) DIA-C WITH PROMPT AUTO-SKIP.
           ACCEPT (14 18) MES-C WITH PROMPT AUTO-SKIP.
           ACCEPT (14 21) ANO-C WITH PROMPT AUTO-SKIP.
           IF WS-DATA-C = ZEROS 
               CLOSE BANCO
               CHAIN "mrelban.COM". 
           DISPLAY (01 01) ERASE. 
           DISPLAY TELA-2.          

      * P-REC-DEPTO.
      *     DISPLAY (12 16) "Informe a Depto p/imprimir> [ ]".
      *     DISPLAY TELA-LIMPA-CONTA.
      *     DISPLAY TELA-DEPTO.
      *     ACCEPT (12 45) WS-DEPTO WITH PROMPT AUTO-SKIP.
      *     IF NOT DEPTO-88 GO TO P-REC-DEPTO.
      *     IF WS-DEPTO = 1 DISPLAY (10 25) "LOJA".
      *     IF WS-DEPTO = 2 DISPLAY (10 25) "ELETRO".
      *     IF WS-DEPTO = 3 DISPLAY (10 25) "CALCADOS".
      *     IF WS-DEPTO = 4 DISPLAY (10 25) "CONFEC".
      *     IF WS-DEPTO = 5 DISPLAY (10 25) "MERCADO".
      *     IF WS-DEPTO = 6 DISPLAY (10 25) "FERRAGEM".
      *     IF WS-DEPTO = 7 DISPLAY (10 25) "OUTROS".
      *     IF WS-DEPTO = 8 DISPLAY (10 25) "DESPESAS".
      *     DISPLAY (12 16) WS-LIMPA.
      *     IF WS-DEPTO = 01 MOVE "LOJA" TO DEPTO-DET1.
      *     IF WS-DEPTO = 02 MOVE "ELETRO" TO DEPTO-DET1.
      *     IF WS-DEPTO = 03 MOVE "CALCADOS" TO DEPTO-DET1.
      *     IF WS-DEPTO = 04 MOVE "CONFEC" TO DEPTO-DET1.
      *     IF WS-DEPTO = 05 MOVE "MERCADO" TO DEPTO-DET1.
      *     IF WS-DEPTO = 06 MOVE "FERRAGEM" TO DEPTO-DET1.
      *     IF WS-DEPTO = 07 MOVE "OUTROS" TO DEPTO-DET1.
      *     IF WS-DEPTO = 08 MOVE "DESPESAS" TO DEPTO-DET1.
      *     IF WS-DEPTO = SPACES MOVE "-+-+-+-+" TO DEPTO-DET1.




       P-IMPRIME-CABEC.
           DISPLAY (24 01) "Mostrando....".
  
       LER.
      ****-----
           return SORTEIO 
                AT END
                    PERFORM P-FINAL-IMP
                    GO TO P-DESCRI-CHE. 
           MOVE DIA-CHE-s TO DIA-R.
           MOVE MES-CHE-s TO MES-R.
           MOVE ANO-CHE-s TO ANO-R.
           IF WS-DATA-R > WS-DATA-C
               GO TO LER.
           IF MES-CHE-S NOT = MES-C
               GO TO LER.
           IF ANO-CHE-S NOT = ANO-C
               GO TO LER.
           IF obs-CHE-S = SPACES
               GO TO LER.
           IF DESCRI-CHE-S = "NULO "  
               GO TO LER.

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
           display (19 04) "|--------------------|".
           DISPLAY (20 04) "|" WS-TOTAL-IMPRESSOS  "  Doc.Mostrados |".
           MOVE WS-TOTAL-VALOR TO WS-MOSTRA-VALOR. 
           DISPLAY (21 04) "|    no Valor de     |".
           DISPLAY (22 04) "|   R$ " WS-MOSTRA-VALOR "    |".
           display (23 04) "|--------------------|".
           MOVE WS-TOTAL-IMPRESSOS TO CONTCHEQ-DET.
           MOVE WS-TOTAL-VALOR TO VALTOTCHEQ-DET.

           MOVE WS-TOTAL-LOJA TO VALTOT-LOJA-DET.
           MOVE WS-CONTCHEQ-LOJA TO CONTCHEQ-LOJA-DET.

           MOVE WS-TOTAL-ELETRO TO VALTOT-ELETRO-DET.
           MOVE WS-CONTCHEQ-ELETRO TO CONTCHEQ-ELETRO-DET.

           MOVE WS-TOTAL-CALCADOS TO VALTOT-CALCADOS-DET.
           MOVE WS-CONTCHEQ-CALCADOS TO CONTCHEQ-CALCADOS-DET.

           MOVE WS-TOTAL-CONFEC TO VALTOT-CONFEC-DET.
           MOVE WS-CONTCHEQ-CONFEC TO CONTCHEQ-CONFEC-DET.

           MOVE WS-TOTAL-MERCADO TO VALTOT-MERCADO-DET.
           MOVE WS-CONTCHEQ-MERCADO TO CONTCHEQ-MERCADO-DET.

           MOVE WS-TOTAL-FERRAGEM TO VALTOT-FERRAGEM-DET.
           MOVE WS-CONTCHEQ-FERRAGEM TO CONTCHEQ-FERRAGEM-DET.

           MOVE WS-TOTAL-OUTROS TO VALTOT-OUTROS-DET.
           MOVE WS-CONTCHEQ-OUTROS TO CONTCHEQ-OUTROS-DET.

           MOVE WS-TOTAL-DESPESAS TO VALTOT-DESPESAS-DET.
           MOVE WS-CONTCHEQ-DESPESAS TO CONTCHEQ-DESPESAS-DET.

           MOVE WS-TOTAL-ACOUGUE TO VALTOT-ACOUGUE-DET.
           MOVE WS-CONTCHEQ-ACOUGUE TO CONTCHEQ-ACOUGUE-DET.



           MOVE DIA-C TO DIA-DATA.
           MOVE MES-C TO MES-DATA.
           MOVE ANO-C TO ANO-DATA.
      *     DISPLAY (21 05) "Imprimir ?  < S/N >" .
      *     ACCEPT (21 25) WS-CONF WITH PROMPT AUTO-SKIP.
      *     IF WS-CONF = "S" or "s" OR "0"
           PERFORM P-IMPRIME.

           CLOSE BANCO.
           CHAIN "BANCO.COM".

       P-IMPRIME.
           display (06 30) "--------------------------------".
           MOVE WS-TOTAL-LOJA TO WS-MOSTRA-VALOR. 
           display (07 30) "Loja...." WS-MOSTRA-VALOR.
           compute ws-PERC = (ws-total-LOJA * 100) / ws-total-valor.
           MOVE WS-PERC TO WS-MOSTRA-VALOR.
           display (07 50) "....%...." WS-MOSTRA-VALOR.
           display (08 30) "--------------------------------".
           MOVE WS-TOTAL-ELETRO TO WS-MOSTRA-VALOR. 
           display (09 30) "Eletro.." WS-MOSTRA-VALOR.
           compute ws-PERC = (ws-total-ELETRO * 100) / ws-total-valor.
           MOVE WS-PERC TO WS-MOSTRA-VALOR.
           display (09 50) "....%...." WS-MOSTRA-VALOR.
           display (10 30) "--------------------------------".
           MOVE WS-TOTAL-CALCADOS TO WS-MOSTRA-VALOR. 
           display (11 30) "Calcados" WS-MOSTRA-VALOR.
           compute ws-PERC = (ws-total-CALCADOS * 100) / ws-total-valor.
           MOVE WS-PERC TO WS-MOSTRA-VALOR.
           display (11 50) "....%...." WS-MOSTRA-VALOR.
           display (12 30) "--------------------------------".
           MOVE WS-TOTAL-CONFEC TO WS-MOSTRA-VALOR. 
           display (13 30) "Confec.." WS-MOSTRA-VALOR.
           compute ws-PERC = (ws-total-CONFEC * 100) / ws-total-valor.
           MOVE WS-PERC TO WS-MOSTRA-VALOR.
           display (13 50) "....%...." WS-MOSTRA-VALOR.
           display (14 30) "--------------------------------".

           MOVE WS-TOTAL-MERCADO TO WS-MOSTRA-VALOR.
           display (15 30) "Mercado." WS-MOSTRA-VALOR.
           compute ws-PERC = (ws-total-MERCADO * 100) / ws-total-valor.
           MOVE WS-PERC TO WS-MOSTRA-VALOR.
           display (15 50) "....%...." WS-MOSTRA-VALOR.
           display (16 30) "--------------------------------".

           MOVE WS-TOTAL-FERRAGEM TO WS-MOSTRA-VALOR.
           display (17 30) "Ferragem" WS-MOSTRA-VALOR.
           compute ws-PERC = (ws-total-FERRAGEM * 100) / ws-total-valor.
      *     compute ws-perc =
      *     ( ws-total-valor / (ws-total-valor - ws-total-ferragem) - 1 ).
      *     compute ws-perc = ws-perc * 100
           MOVE WS-PERC TO WS-MOSTRA-VALOR.
           display (17 50) "....%...." WS-MOSTRA-VALOR.
           display (18 30) "--------------------------------".
           MOVE WS-TOTAL-OUTROS TO WS-MOSTRA-VALOR. 
           display (19 30) "Outros.." WS-MOSTRA-VALOR.
           compute ws-PERC = (ws-total-OUTROS * 100) / ws-total-valor.
           MOVE WS-PERC TO WS-MOSTRA-VALOR.
           display (19 50) "....%...." WS-MOSTRA-VALOR.
           display (20 30) "--------------------------------".
           MOVE WS-TOTAL-DESPESAS TO WS-MOSTRA-VALOR. 
           display (21 30) "Despesas" WS-MOSTRA-VALOR.
           compute ws-PERC = (ws-total-DESPESAS * 100) / ws-total-valor.
           MOVE WS-PERC TO WS-MOSTRA-VALOR.
           display (21 50) "....%...." WS-MOSTRA-VALOR.
           display (22 30) "--------------------------------".
           MOVE WS-TOTAL-ACOUGUE TO WS-MOSTRA-VALOR. 
           display (23 30) "Acougue" WS-MOSTRA-VALOR.
           compute ws-PERC = (ws-total-ACOUGUE * 100) / ws-total-valor.
           MOVE WS-PERC TO WS-MOSTRA-VALOR.
           display (23 50) "....%...." WS-MOSTRA-VALOR.
           display (24 30) "--------------------------------".
           ACCEPT (24 79) WS-P. 
       P-ERRO-LEITURA.
           DISPLAY (12 20) "!!!!!  CHAVE INVALIDA  !!!!!".
           STOP RUN.

       P-MOSTRA.
           ADD 1 TO LIN.
           MOVE NUM-CHE TO WS-MOSTRA-CODIGO.
      *     DISPLAY (LIN , 03) WS-MOSTRA-CODIGO.
      *     DISPLAY (LIN , 15) DESCRI-CHE DESCRI2-CHE.
      *     MOVE VALOR-CHE TO WS-MOSTRA-VALOR. 
      *     DISPLAY (LIN , 32) WS-MOSTRA-VALOR.
      *     DISPLAY (LIN , 44) DIA-CHE "/" MES-CHE "/" ANO-CHE ".".
      *     DISPLAY (LIN , 54) CONTA-CHE.
           ADD 1 TO WS-TOTAL-IMPRESSOS.
      *     IF LIN > 18 PERFORM P-PARA.

           MOVE NUM-CHE-S TO NUM-CHE-DET.
           MOVE CONTA-CHE-S TO CONTA-DET.

           IF OBS-CHE-S = 01 MOVE "LOJA" TO DEPTO-DET.
           IF OBS-CHE-S = 02 MOVE "ELETRO" TO DEPTO-DET.
           IF OBS-CHE-S = 03 MOVE "CALCADOS" TO DEPTO-DET.
           IF OBS-CHE-S = 04 MOVE "CONFEC" TO DEPTO-DET.
           IF OBS-CHE-S = 05 MOVE "MERCADO" TO DEPTO-DET.
           IF OBS-CHE-S = 06 MOVE "FERRAGEM" TO DEPTO-DET.
           IF OBS-CHE-S = 07 MOVE "OUTROS" TO DEPTO-DET.
           IF OBS-CHE-S = 08 MOVE "DESPESAS" TO DEPTO-DET.
           IF OBS-CHE-S = 09 MOVE "ACOUGUE" TO DEPTO-DET.
           IF OBS-CHE-S = SPACES MOVE "-+-+-+-+" TO DEPTO-DET.

           MOVE NUM-CHE-S TO NUM-CHE-DET.
           MOVE DESCRI-CHE-S TO DESCRI-CHE-DET.
           MOVE DESCRI2-CHE-S TO DESCRI2-CHE-DET.
           MOVE DIA-CHE-S TO DIA-CHE-DET.
           MOVE MES-CHE-S TO MES-CHE-DET.
           MOVE ANO-CHE-S TO ANO-CHE-DET.
           MOVE VALOR-CHE-S TO VALOR-CHE-DET.
           ADD VALOR-CHE-S TO WS-TOTAL-VALOR.
            
           IF OBS-CHE-S = 01 
           ADD VALOR-CHE-S TO WS-TOTAL-LOJA
           ADD 1 TO WS-CONTCHEQ-LOJA.

           IF OBS-CHE-S = 02 
           ADD VALOR-CHE-S TO WS-TOTAL-ELETRO
           ADD 1 TO WS-CONTCHEQ-ELETRO.
           IF OBS-CHE-S = 03 
           ADD VALOR-CHE-S TO WS-TOTAL-CALCADOS
           ADD 1 TO WS-CONTCHEQ-CALCADOS.
           IF OBS-CHE-S = 04 
           ADD VALOR-CHE-S TO WS-TOTAL-CONFEC
           ADD 1 TO WS-CONTCHEQ-CONFEC.
           IF OBS-CHE-S = 05 
           ADD VALOR-CHE-S TO WS-TOTAL-MERCADO
           ADD 1 TO WS-CONTCHEQ-MERCADO.
           IF OBS-CHE-S = 06 
           ADD VALOR-CHE-S TO WS-TOTAL-FERRAGEM
           ADD 1 TO WS-CONTCHEQ-FERRAGEM.
           IF OBS-CHE-S = 07 
           ADD VALOR-CHE-S TO WS-TOTAL-OUTROS
           ADD 1 TO WS-CONTCHEQ-OUTROS.
           IF OBS-CHE-S = 08 
           ADD VALOR-CHE-S TO WS-TOTAL-DESPESAS
           ADD 1 TO WS-CONTCHEQ-DESPESAS.
           IF OBS-CHE-S = 09 
           ADD VALOR-CHE-S TO WS-TOTAL-ACOUGUE
           ADD 1 TO WS-CONTCHEQ-ACOUGUE.


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
