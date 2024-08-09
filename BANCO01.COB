
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    BANCO01.
       AUTHOR. ROGERIO-MACHADO.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BANCO ASSIGN TO DISK
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS NUM-CHE
                  FILE STATUS IS FS.
       DATA DIVISION.
       FILE SECTION.
       FD BANCO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "BCO.DAT".
       01 REG-BCO.
           03 NUM-CHE          PIC X(06).
           03 CONTA-CHE        PIC X(10).
           03 DESCRI-CHE       PIC X(20).
           03 DIA-CHE          PIC 99.   
           03 MES-CHE          PIC 99.   
           03 ANO-CHE          PIC 99.   
           03 VALOR-CHE        PIC 9(06)V99.
           03 OBS-CHE          PIC X(15). 
           03 INSCR-CHE        PIC 9(06).
           03 DATA-CONF        PIC 9(06). 
      ***********************************

       WORKING-STORAGE SECTION.
         
       01 WS-DATA.
           03 ANO              PIC 99.
           03 MES              PIC 99.
           03 DIA              PIC 99.
       01 WS-DATA-N.
           03 ANO-N            PIC 99.
           03 MES-N            PIC 99.
           03 DIA-N            PIC 99.
       01 WS-DATA-C.
           03 DIA-C            PIC 99.
           03 MES-C            PIC 99.
           03 ANO-C            PIC 99.
       01 WS-DATA-INSC.
           03 DIA-INSC         PIC 99.
           03 MES-INSC         PIC 99.
           03 ANO-INSC         PIC 99.
  
       77 FS                   PIC XX.
       77 WS-LIMPA             PIC X(40) VALUE SPACES.
       77 WS-CODIGO            PIC X(05) VALUE SPACES.
       77 WS-P                 PIC X VALUE SPACES.
       77 WS-SITUACAO          PIC X VALUE SPACES. 
       88 SIT-88               VALUE "D" "C" "A" "X" "B".

       77 WS-CONTA             PIC 9.
       88 CONTA-88             VALUE  0 1 2 3 4 5 6.

       77 WS-DEPTO             PIC 99.
       88 DEPTO-88             VALUE  0 1 2 3 4 5 6 7 8 9.

       77 WS-CIDADE            PIC X(15) VALUE "SAPUCAIA DO SUL".
       77 WS-CONTA-CODIGO      PIC X(05) VALUE ZEROS.
       77 WS-NUM               PIC 9(06) VALUE ZEROS.
       77 WS-DESCRI            PIC 99 VALUE ZEROS.
       77 WS-CONTA-CHE         PIC X(10) VALUE SPACES.

 
      ***********************************

       SCREEN SECTION.
       01 TELA-1.
           02 BLANK SCREEN.
           02 LINE 02 COLUMN 67 VALUE "       /  /  ".
           02 LINE 03 COLUMN 25 VALUE " Controle Financeiro " BLINK. 
           02 LINE 04 COLUMN 57 VALUE "< Inclusao >".
           02 LINE 08 COLUMN 04 VALUE "No.do Documento....=".
           02 LINE 09 COLUMN 04 VALUE "Conta..............=".
           02 LINE 10 COLUMN 04 VALUE "Departamento.......=".
           02 LINE 11 COLUMN 04 VALUE "Favorecido.........=".
           02 LINE 12 COLUMN 04 VALUE "Vecto Documento....=".
           02 LINE 13 COLUMN 04 VALUE "Valor Documento....=".
           02 LINE 22 COLUMN 05 VALUE "Mensagem".

       01 TELA-CONTA.
           02 LINE 12 COLUMN 30 VALUE "<1> LIANE CIA".
           02 LINE 13 COLUMN 30 VALUE "<2> FERNANDO".
           02 LINE 14 COLUMN 30 VALUE "<3> PIX".
           02 LINE 15 COLUMN 30 VALUE "<4> TITULOS".
           02 LINE 16 COLUMN 30 VALUE "<5> ITAU FERR.".
           02 LINE 17 COLUMN 30 VALUE "<6> FERRAGEM  ".

       01 TELA-DEPTO.
           02 LINE 04 COLUMN 35 VALUE "<01> LOJA".
           02 LINE 05 COLUMN 35 VALUE "<02> ELETRO".
           02 LINE 06 COLUMN 35 VALUE "<03> CALCADOS".
           02 LINE 07 COLUMN 35 VALUE "<04> CONFEC".
           02 LINE 08 COLUMN 35 VALUE "<05> MERCADO".
           02 LINE 09 COLUMN 35 VALUE "<06> FERRAGEM".
           02 LINE 10 COLUMN 35 VALUE "<07> OUTROS".
           02 LINE 11 COLUMN 35 VALUE "<08> DESPESAS".
           02 LINE 12 COLUMN 35 VALUE "<09> ACOUGUE".
       01 TELA-FORN.
           02 LINE 05 COLUMN 35 VALUE "<01> BCO.".
           02 LINE 06 COLUMN 35 VALUE "<02> FABIANO ".
           02 LINE 07 COLUMN 35 VALUE "<03> LORENCINI".
           02 LINE 08 COLUMN 35 VALUE "<04> CODALL".
           02 LINE 09 COLUMN 35 VALUE "<05> STA.CLARA".
           02 LINE 10 COLUMN 35 VALUE "<06> BIONIC".
           02 LINE 11 COLUMN 35 VALUE "<07> RENASCER".
           02 LINE 12 COLUMN 35 VALUE "<08> ".
           02 LINE 13 COLUMN 35 VALUE "<09> RUBASKI".
           02 LINE 14 COLUMN 35 VALUE "<10> ROHERS".
           02 LINE 15 COLUMN 35 VALUE "<11> FIHL   ".
           02 LINE 16 COLUMN 35 VALUE "<12> SERVBEM".
           02 LINE 17 COLUMN 35 VALUE "<13> MENON".
           02 LINE 18 COLUMN 35 VALUE "<14> BORRUSSIA".
           02 LINE 19 COLUMN 35 VALUE "<15> PEPSI".
           02 LINE 20 COLUMN 35 VALUE "<16> ALTECON".
           02 LINE 21 COLUMN 35 VALUE "<17> CARNE".
           02 LINE 22 COLUMN 35 VALUE "<18> MULLER BEB".
           02 LINE 23 COLUMN 35 VALUE "<19> S.PASSOS".

                     
       01 TELA-LIMPA.
           02 LINE 04 COLUMN 35 VALUE "                ".
           02 LINE 05 COLUMN 35 VALUE "                ".
           02 LINE 06 COLUMN 35 VALUE "                ".
           02 LINE 07 COLUMN 35 VALUE "                ".
           02 LINE 08 COLUMN 35 VALUE "                ".
           02 LINE 09 COLUMN 35 VALUE "                 ".
           02 LINE 10 COLUMN 35 VALUE "                 ".
           02 LINE 11 COLUMN 35 VALUE "                ".
           02 LINE 12 COLUMN 35 VALUE "                ".
           02 LINE 13 COLUMN 35 VALUE "                ".
           02 LINE 14 COLUMN 35 VALUE "                ".
           02 LINE 15 COLUMN 35 VALUE "                ".
           02 LINE 16 COLUMN 35 VALUE "                ".
           02 LINE 17 COLUMN 35 VALUE "                ".
           02 LINE 18 COLUMN 35 VALUE "                ".
           02 LINE 19 COLUMN 35 VALUE "                ".
           02 LINE 20 COLUMN 35 VALUE "                ".
           02 LINE 21 COLUMN 35 VALUE "                ".
           02 LINE 22 COLUMN 35 VALUE "                ".
           02 LINE 23 COLUMN 35 VALUE "                ".

       01 TELA-LIMPA-CONTA.
           02 LINE 11 COLUMN 30 VALUE "                ".
           02 LINE 12 COLUMN 30 VALUE "                ".
           02 LINE 13 COLUMN 30 VALUE "                ".
           02 LINE 14 COLUMN 30 VALUE "                ".
           02 LINE 15 COLUMN 30 VALUE "                ".
           02 LINE 16 COLUMN 30 VALUE "                ".
           02 LINE 17 COLUMN 30 VALUE "                ".
           02 LINE 18 COLUMN 30 VALUE "                ".


      *
       PROCEDURE DIVISION.
       
       P03-ABERTURA.
           OPEN I-O BANCO.
           IF FS = "30"
               CLOSE BANCO
               OPEN OUTPUT BANCO
               CLOSE BANCO 
               GO TO P03-ABERTURA.
           PERFORM P-DATA.

      * P-MOSTRA-CODIGO.
      *     DISPLAY TELA-1.
      *     MOVE SPACES TO WS-CONTA-CODIGO.
      *     DISPLAY (08 35) "Informe como Codigo Inicial, codigo ".      
      *     DISPLAY (09 37) ", acrescido de 1 (Some 1 ao codigo inicial)".      
      *     PERFORM P-CONTA-CODIGO UNTIL FS = "10".
      *     DISPLAY (08 72) NUM-CHE.      
      *     MOVE NUM-CHE TO WS-CONTADOR.
      *     COMPUTE WS-CONTADOR = WS-CONTADOR + 1.
      *     DISPLAY (08 72) WS-CONTADOR.      
      *     DISPLAY (10 37) "Tecle  < ENTER > ".
      *     ACCEPT (09 72) WS-P.
      *     MOVE WS-CONTADOR TO WS-NUM-CODIGO.
      *     DISPLAY (08 25) WS-NUM-CODIGO.
      *     MOVE WS-NUM-CODIGO TO NUM-CHE.
      *     GO TO P-LER.

       P-RECEBE-PRIMEIRO.
           DISPLAY TELA-1.
           ACCEPT (08 25) WS-NUM WITH PROMPT.
           IF WS-NUM = ZEROS perform p-FIM.
           MOVE WS-NUM TO NUM-CHE.

           DISPLAY TELA-CONTA.
           ACCEPT (09 25) WS-CONTA WITH PROMPT AUTO-SKIP.
           IF NOT CONTA-88 GO TO P-REC2.
           IF WS-CONTA = 1 MOVE "LIANE CIA" TO WS-CONTA-CHE.
           IF WS-CONTA = 2 MOVE "FERNANDO " TO WS-CONTA-CHE.
           IF WS-CONTA = 3 MOVE "PIX" TO WS-CONTA-CHE.
           IF WS-CONTA = 4 MOVE "TITULOS  " TO WS-CONTA-CHE.
           IF WS-CONTA = 5 MOVE "UNIBANCO " TO WS-CONTA-CHE.
           IF WS-CONTA = 6 MOVE "FERRAGEM " TO WS-CONTA-CHE.
           IF WS-CONTA = 0 PERFORM P-FIM.
           DISPLAY (09 25) CONTA-CHE.
           DISPLAY TELA-LIMPA.
           GO TO P-LER.

       P01-TELA-1.
           DISPLAY TELA-1.
       P02-DATA.
           PERFORM P-DATA.
       P04-CODIGO.
           DISPLAY (22 04) "<CODIGO =    ,Sai da Inclusao".
           compute ws-num = ws-num + 1.
           DISPLAY (08 25) WS-NUM.
           MOVE WS-NUM TO NUM-CHE.
       P-LER. 
           READ BANCO
               INVALID KEY
                   GO TO P-GRAVA.
           DISPLAY (22 04) "!! < Codigo ja Cadastrado > !!".
           DISPLAY (23 04) " Tecle  -  <  ENTER  > ".
           ACCEPT WS-P.
           DISPLAY (22 04) WS-LIMPA.
           DISPLAY (23 04) WS-LIMPA.
           GO TO P04-CODIGO.           
      *******************************************

       P-GRAVA.
           PERFORM P-REC2 THRU P-REC5.
           WRITE REG-BCO INVALID KEY
               DISPLAY (21 04) "ERRO DE GRAVACAO - FS = " FS
               CLOSE BANCO
               STOP RUN.
           GO TO P01-TELA-1.

       P-DATA.
           ACCEPT WS-DATA FROM DATE.
           DISPLAY (02 72) DIA.
           DISPLAY (02 75) MES.
           DISPLAY (02 78) ANO.
      *-----------------------------------------* 
      * P-REC1. 
      *     ACCEPT (08 25) NUM-CHE WITH PROMPT.
      *     IF NUM-CHE = SPACES perform p-FIM.
       P-REC2. 
           MOVE WS-CONTA-CHE TO CONTA-CHE. 
           DISPLAY (09 25) CONTA-CHE.
       P-REC2-1.
           DISPLAY TELA-LIMPA-CONTA.
           DISPLAY TELA-DEPTO.
           ACCEPT (10 25) WS-DEPTO WITH PROMPT AUTO-SKIP.
           IF NOT DEPTO-88 GO TO P-REC2-1.
           IF WS-DEPTO = 1 DISPLAY (10 25) "LOJA".
           IF WS-DEPTO = 2 DISPLAY (10 25) "ELETRO".
           IF WS-DEPTO = 3 DISPLAY (10 25) "CALCADOS".
           IF WS-DEPTO = 4 DISPLAY (10 25) "CONFEC".
           IF WS-DEPTO = 5 DISPLAY (10 25) "MERCADO".
           IF WS-DEPTO = 6 DISPLAY (10 25) "FERRAGEM".
           IF WS-DEPTO = 7 DISPLAY (10 25) "OUTROS".
           IF WS-DEPTO = 8 DISPLAY (10 25) "DESPESAS".
           IF WS-DEPTO = 9 DISPLAY (10 25) "ACOUGUE".
           MOVE WS-DEPTO TO OBS-CHE.

       P-REC3.
           DISPLAY TELA-LIMPA-CONTA.
           DISPLAY TELA-FORN.
           ACCEPT (11 25) WS-DESCRI WITH PROMPT AUTO-SKIP.
           IF WS-DESCRI = 1 MOVE "BCO." TO DESCRI-CHE.
           IF WS-DESCRI = 2 MOVE "FABIANO" TO DESCRI-CHE.
           IF WS-DESCRI = 3 MOVE "LORENCINI" TO DESCRI-CHE.
           IF WS-DESCRI = 4 MOVE "CODALL" TO DESCRI-CHE.
           IF WS-DESCRI = 5 MOVE "STA.CLARA" TO DESCRI-CHE.
           IF WS-DESCRI = 6 MOVE "BIONIC   " TO DESCRI-CHE.
           IF WS-DESCRI = 7 MOVE "RENASCER" TO DESCRI-CHE.
           IF WS-DESCRI = 8 MOVE " " TO DESCRI-CHE.
           IF WS-DESCRI = 9 MOVE "RUBASKI" TO DESCRI-CHE.
           IF WS-DESCRI = 10 MOVE "ROHERS" TO DESCRI-CHE.
           IF WS-DESCRI = 11 MOVE "FIHL  " TO DESCRI-CHE.
           IF WS-DESCRI = 12 MOVE "SERVBEM" TO DESCRI-CHE.
           IF WS-DESCRI = 13 MOVE "MENON" TO DESCRI-CHE.
           IF WS-DESCRI = 14 MOVE "BORRUSSIA " TO DESCRI-CHE.
           IF WS-DESCRI = 15 MOVE "PEPSI " TO DESCRI-CHE.
           IF WS-DESCRI = 16 MOVE "ALTECON " TO DESCRI-CHE.
           IF WS-DESCRI = 17 MOVE "CARNE " TO DESCRI-CHE.
           IF WS-DESCRI = 18 MOVE "MULLER BEB" TO DESCRI-CHE.
           IF WS-DESCRI = 19 MOVE "S.PASSOS" TO DESCRI-CHE.
           IF WS-DESCRI = ZEROS
               ACCEPT (11 27) DESCRI-CHE WITH PROMPT.
           DISPLAY (11 25) DESCRI-CHE.
           DISPLAY TELA-LIMPA.
           IF DESCRI-CHE = SPACES perform p-FIM.


       P-REC4.
           ACCEPT (12 25) DIA-CHE WITH PROMPT AUTO-SKIP.
           IF DIA-CHE > 31 GO TO P-REC4.
       P-REC41.
           ACCEPT (12 28) MES-CHE WITH PROMPT AUTO-SKIP. 
           IF MES-CHE > 12 GO TO P-REC41.
       P-REC42.
           MOVE ANO TO ANO-CHE.
      *     ACCEPT (12 31) ANO-CHE WITH PROMPT AUTO-SKIP.

      *------- cheque recebe ano + 1 ao cadastrar proximo do final de ano
      *     IF MES-CHE < 10 add 1 to ano .

           MOVE ANO-CHE TO ANO-C.
           MOVE MES-CHE TO MES-C.
           MOVE DIA-CHE TO DIA-C.
      *     IF WS-DATA-C > WS-DATA GO TO P-REC4. 
       P-REC5.
           ACCEPT (13 25) VALOR-CHE WITH PROMPT AUTO-SKIP.
            
           MOVE ANO TO ANO-INSC.
           MOVE MES TO MES-INSC.
           MOVE DIA TO DIA-INSC.
           MOVE WS-DATA-INSC TO INSCR-CHE.
           DISPLAY (21 65) INSCR-CHE.
           MOVE ZEROS TO DATA-CONF.

      *-----------------------------------------*
       P-CONTA-CODIGO.
           READ BANCO NEXT RECORD.
               IF NUM-CHE > WS-CONTA-CODIGO
      *             DISPLAY (07 50) NUM-CHE
      *             STOP " "
                   MOVE NUM-CHE TO WS-CONTA-CODIGO.    
             

       P-FIM.
           CLOSE BANCO.
           DISPLAY (01 01) ERASE.
           CHAIN "BANCO.COM".   





