
                              
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    BANCO36.
       AUTHOR.        ROGERIO FERNANDO MACHADO.
      *
      *    M E N U - PRODUTOS MERCADO
      * 
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	   SELECT RELATO ASSIGN TO PRINTER.
       DATA DIVISION.
       FILE SECTION.
       FD RELATO   
	   LABEL RECORD IS OMITTED.
       01 REG-RELATO           PIC X(80).

       WORKING-STORAGE SECTION.
       77 WS-OPCAO             PIC 99.
       88 OPCAO-88
           VALUE 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19
           20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 99. 
       77 WS-LIMPA             PIC X(50) VALUE SPACES.
       77 WS-P                 PIC X VALUE SPACES.
       77 WS-FORN              PIC X(10) VALUE SPACES.
       77 WS-DINHEIRO          PIC 9(05)V99 VALUE ZEROS.
       77 WS-TITULOS           PIC 9(05)V99 VALUE ZEROS.
       77 WS-JUROS             PIC 9(04)V99 VALUE ZEROS.
       77 WS-TOTAL             PIC 9(05)V99 VALUE ZEROS.

       01 WS-LIGA.
           03 F PIC 9 COMP VALUE 27.
           03 F PIC 9 COMP VALUE 15.
       01 WS-DESLIGA.
           03 F PIC 9 COMP VALUE 27.
           03 F PIC 9 COMP VALUE 18.
       01 WS-DATA.
	   03 ANO              PIC 9(02).
	   03 MES              PIC 9(02).
	   03 DIA              PIC 9(02).

       01 CHAMADOR.
	   03 FILLER           PIC X(04) VALUE "MERC".
	   03 NRO-PROG         PIC 99.
	   03 FILLER           PIC X(04) VALUE ".COM".

       01 LINHA.
           03 F PIC X(80) VALUE SPACES.   
       01 LINHA-1.
           03 F PIC X(06) VALUE SPACES.   
           03 F PIC X(12) VALUE "Dinheiro -->".   
           03 DINHEIRO-DET PIC zzzz9.99.
	   03 F PIC X(10) VALUE SPACES.   
       01 LINHA-2.
           03 F PIC X(06) VALUE SPACES.   
           03 F PIC X(12) VALUE "Titulos --->".   
           03 TITULOS-DET PIC zzzz9.99.
           03 FORNECEDOR-DET PIC X(10).
           03 FORNECEDOR2-DET PIC X(10).
       01 LINHA-2-1.
           03 F PIC X(06) VALUE SPACES.   
           03 F PIC X(12) VALUE "Juros   --->".   
           03 JUROS-DET PIC zzz9.99.
       01 LINHA-3.
           03 F PIC X(06) VALUE SPACES.   
           03 F PIC X(12) VALUE "TOTAL ----->".   
           03 TOTAL-DET PIC zzzz9.99.
       01 LINHA-4.
           03 F PIC X(06) VALUE SPACES.   
           03 F PIC X(22) VALUE "Depositar 190171850-6".   
       01 LINHA-5.
           03 F PIC X(06) VALUE SPACES.   
           03 F PIC X(24) VALUE "CNPJ 94.675.469/0001-74".   
       01 LINHA-6.
           03 F PIC X(06) VALUE SPACES.   
           03 F PIC X(24) VALUE "CPF 646943800-87".   


       01 LINHA-MENSAGEM.
           03 F PIC X(25) VALUE "       E l d o r a d o ".
           03 DIA-DET PIC 99.
           03 F PIC X(01) VALUE "/".   
	   03 MES-DET PIC 99.
           03 F PIC X(01) VALUE "/".   
	   03 ANO-DET PIC 99.

       01 LINHA-TRACO.
           03 F PIC X(40) VALUE ALL "-".   

       SCREEN SECTION.
       01 TELA.
       02 BLANK SCREEN.
           02 LINE 01 COLUMN 01 PIC X(80) FROM ALL "-".
           02 LINE 02 COLUMN 25 VALUE " SUPERMERCADO ELDORADO" BLINK.
           02 LINE 03 COLUMN 01 PIC X(80) FROM ALL "-".                       
           02 LINE 04 COLUMN 03 VALUE "CONTROLE DE ESTOQUE DE PRODUTOS".
           02 LINE 04 COLUMN 60 VALUE "  /  /  .".
           02 LINE 05 COLUMN 04 VALUE " Estoque " BLINK.
           02 LINE 05 COLUMN 14 VALUE "- Estoque   R  E  D  E ".

           02 LINE 24 COLUMN 17 VALUE "OPCAO : [  ]".
      *     02 LINE 24 COLUMN 01 PIC X(80) FROM ALL "-" BLINK.
      *     02 LINE 24 COLUMN 05 VALUE "Mensagem : " BLINK.       

       01 TELA-TRAB.
	   02 BLANK SCREEN.
	   02 LINE 03 COLUMN 01 PIC X(80) FROM ALL "-".                       
	   02 LINE 02 COLUMN 28 VALUE "Supermercado  ELDORADO" BLINK.
	   02 LINE 04 COLUMN 28 VALUE "CONTROLE DE ESTOQUE DE PRODUTOS".
	   02 LINE 08 COLUMN 18 VALUE " -----------------------------".
	   02 LINE 09 COLUMN 18 VALUE " -   TENHA UM BOM TRABALHO   -".
	   02 LINE 10 COLUMN 18 VALUE " -----------------------------".
	   02 LINE 07 COLUMN 18 VALUE "                    " BLINK.
	   02 LINE 07 COLUMN 30 VALUE "                    " BLINK.
	   02 LINE 11 COLUMN 18 VALUE "                    " BLINK.
	   02 LINE 11 COLUMN 29 VALUE "                    " BLINK.
	   02 LINE 15 COLUMN 29 VALUE "Tecle  < ENTER > Para INICIAR ".


      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                                                         *  
       PROCEDURE DIVISION.
      *                                                         *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
       P-ABRE.
           OPEN OUTPUT RELATO.
       P10-TELA-1.    
           PERFORM P-TELA.
       P20-OPCAO.
	   
           DISPLAY (09 02) "Dinh.".
           ACCEPT (09 10) WS-DINHEIRO WITH PROMPT AUTO-SKIP.
               IF WS-DINHEIRO = 9
               DISPLAY (01 01) ERASE
               CLOSE RELATO
               CHAIN "BANCO.COM".

           DISPLAY (10 02) "Tit. ".
           ACCEPT (10 10) WS-TITULOS WITH PROMPT AUTO-SKIP.
               IF WS-TITULOS = 9
               DISPLAY (01 01) ERASE
               CLOSE RELATO
               CHAIN "BANCO.COM".

           DISPLAY (11 02) "Juros ".
           ACCEPT (11 10) WS-JUROS WITH PROMPT AUTO-SKIP.


           COMPUTE WS-TITULOS = ( WS-JUROS + WS-TITULOS ).
           COMPUTE WS-TOTAL = ( WS-DINHEIRO - WS-TITULOS ).
           DISPLAY (12 10) WS-TOTAL.
           ACCEPT WS-P.

           MOVE WS-DINHEIRO TO DINHEIRO-DET.
           MOVE WS-TITULOS TO TITULOS-DET.
           MOVE WS-JUROS TO JUROS-DET.
           MOVE WS-TOTAL TO TOTAL-DET.

           WRITE REG-RELATO FROM WS-DESLIGA. 
           WRITE REG-RELATO FROM LINHA-mensagem.
           WRITE REG-RELATO FROM LINHA-TRACO.
           WRITE REG-RELATO FROM LINHA.
           WRITE REG-RELATO FROM LINHA-1.
           WRITE REG-RELATO FROM LINHA-2.
           WRITE REG-RELATO FROM LINHA.
           WRITE REG-RELATO FROM LINHA-TRACO.
           WRITE REG-RELATO FROM LINHA-3.
           WRITE REG-RELATO FROM LINHA.
           WRITE REG-RELATO FROM LINHA-4.
           WRITE REG-RELATO FROM LINHA-5.
           WRITE REG-RELATO FROM LINHA-6.
           WRITE REG-RELATO FROM LINHA.
           WRITE REG-RELATO FROM LINHA-2-1.
           WRITE REG-RELATO FROM WS-DESLIGA. 

           ACCEPT WS-P.

           DISPLAY (01 01) ERASE.
           CLOSE RELATO.
           CHAIN "BANCO.COM".


       P30-MOVE-OPCAO.
           MOVE WS-OPCAO TO NRO-PROG.
           CHAIN CHAMADOR.

      * * * * * * * * * * * * * * * * * * * * * *
      *           P E R F O R M S               *
      * * * * * * * * * * * * * * * * * * * * * *
       
       P-TELA.
           DISPLAY TELA.
           ACCEPT WS-DATA FROM DATE.
           DISPLAY (04 60) DIA.
           DISPLAY (04 63) MES.
           DISPLAY (04 66) ANO.
           MOVE DIA TO DIA-DET.
           MOVE MES TO MES-DET.
           MOVE ANO TO ANO-DET.
       P-OPCAO-INCORRETA.
           DISPLAY (23 04) "OPCAO INCORRETA - TECLE - < ENTER >".
           ACCEPT WS-P. 
           DISPLAY (23 04) WS-LIMPA.

       P-IMP-MENSAGEM.
           DISPLAY (25 01) "ATENCAO ARRUMAR PRECOS".
           ACCEPT (25 70) WS-P WITH AUTO-SKIP.
           IF WS-P = "S" OR "s" 
               WRITE REG-RELATO FROM LINHA-TRACO 
               WRITE REG-RELATO FROM LINHA-MENSAGEM 
               WRITE REG-RELATO FROM LINHA-TRACO. 
           DISPLAY (25 01) WS-LIMPA.


