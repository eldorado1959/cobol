
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    BANCO.
       AUTHOR.        ROGERIO FERNANDO MACHADO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WS-OPCAO	       PIC 99.
       88 OPCAO-88
          VALUE 1 2 3 4 5 6 7 8 9 10 11 12 
                14 15 16 17 18 19 20 21 22
                23 24 25 26 27 30 31 32 33 34 35 36 39 41 99.
       77 WS-LIMPA             PIC X(50) VALUE SPACES.
       77 WS-P                 PIC X VALUE SPACES.

       01 WS-DATA.
           03 ANO              PIC 9(02).
           03 MES              PIC 9(02).
           03 DIA              PIC 9(02).

       01 CHAMADOR.
           03 F                PIC X(05) VALUE "BANCO".
           03 NRO              PIC 9(02).
           03 F                PIC X(04) VALUE ".COM".

       SCREEN SECTION.
       01 TELA.
           02 BLANK SCREEN.
           02 LINE 03 COLUMN 25 VALUE " E l d o r a d o " BLINK.
           02 LINE 04 COLUMN 03 VALUE "Controle Financeiro".
           02 LINE 04 COLUMN 67 VALUE "       /  /  .".
           02 LINE 07 COLUMN 08 VALUE "<  Menu Geral  >". 
           02 LINE 09 COLUMN 02 VALUE "01-Inclusao Cheques".
           02 LINE 10 COLUMN 02 VALUE "02-Consulta Lancamentos".
           02 LINE 11 COLUMN 02 VALUE "03-Altera Lancamentos".
           02 LINE 12 COLUMN 02 VALUE "04-Exclui Lancamentos".
           02 LINE 13 COLUMN 02 VALUE "05-Baixa Cheque p/No.".
           02 LINE 14 COLUMN 02 VALUE "06-Inclusao Titulos".
           02 LINE 15 COLUMN 02 VALUE "07-CONFERENCIA CHEQ.P/NOME".
           02 LINE 16 COLUMN 02 VALUE "08-Pesq.Lanc.p/dta.Vcto.".
           02 LINE 17 COLUMN 02 VALUE "09-  M e n u  Relatorios".
           02 LINE 18 COLUMN 02 VALUE "10-Altera ANO".
           02 LINE 19 COLUMN 02 VALUE "11-PESQUISA DE CHEQ.P/VALOR".
           02 LINE 20 COLUMN 02 VALUE "12-TOTAL P/MES Conf/N.Conf.".
           02 LINE 09 COLUMN 30 VALUE "                     ".
           02 LINE 10 COLUMN 30 VALUE "14-Baixa titulo p/Vcto".
           02 LINE 11 COLUMN 30 VALUE "15-Imprime p/Vcto".
           02 LINE 12 COLUMN 30 VALUE "16-Tot.p/dia de pgto.".
           02 LINE 13 COLUMN 30 VALUE "17-Tot./depto/mes./tela".
           02 LINE 14 COLUMN 30 VALUE "18-Totaliza p/conta ".
           02 LINE 15 COLUMN 30 VALUE "19-Pesq.p/3 Letras".
           02 LINE 16 COLUMN 30 VALUE "20-Mostra p/depto c/ %".
           02 LINE 17 COLUMN 30 VALUE "21-Cad.Tit.p/conf.Post.".
           02 LINE 18 COLUMN 30 VALUE "22-Confere Tit.p/Nome ANO".
           02 LINE 19 COLUMN 30 VALUE "23-Confere Tit.p/Nome".
           02 LINE 20 COLUMN 30 VALUE "24-Confere Tit.p/Valor".
           02 LINE 21 COLUMN 30 VALUE "25-Excl.c/consulta p/R$".
           02 LINE 22 COLUMN 30 VALUE "26-Excl.c/cons.p/Nome".
           02 LINE 23 COLUMN 30 VALUE "27-Excl.c/cons.p/ANO".
                             
           02 LINE 08 COLUMN 55 VALUE "30-Mostra/depto/% S/OUTROS".
           02 LINE 10 COLUMN 55 VALUE "31-total diario tela".
           02 LINE 11 COLUMN 55 VALUE "32-total diario /conta".
           02 LINE 12 COLUMN 55 VALUE "33-total diario /dta.cad".
           02 LINE 13 COLUMN 55 VALUE "34-ALTERA P/NAO PAGO".
           02 LINE 14 COLUMN 55 VALUE "35-Malote".
           02 LINE 15 COLUMN 55 VALUE "36-Malote c/juros".
           02 LINE 17 COLUMN 55 VALUE "39-tot.diario NAO PG.tela".
           02 LINE 18 COLUMN 55 VALUE "40-CONF.TIT.CAD.ANT.desat.".
           02 LINE 19 COLUMN 55 VALUE "41-Confere p/Nome MES/ANO".


           02 LINE 23 COLUMN 04 VALUE "99 - !!!  F I M  !!!".
           02 LINE 24 COLUMN 06 VALUE "OPCAO : [  ]".
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *  	                                                *  
       PROCEDURE DIVISION.
      *                                                         *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
       P10-TELA-1.
           PERFORM P-TELA.
       P20-OPCAO.
           ACCEPT (24 15) WS-OPCAO WITH PROMPT AUTO-SKIP.
           IF NOT OPCAO-88
               PERFORM P-OPCAO-INCORRETA
               GO TO P20-OPCAO.
           IF WS-OPCAO = 99
               CHAIN "FINAL.COM".
           IF WS-OPCAO = 09
               CHAIN "MRELBAN.COM".
       P30-MOVE-OPCAO.
           MOVE WS-OPCAO TO NRO.
           CHAIN CHAMADOR.

      * * * * * * * * * * * * * * * * * * * * * *
      *           P E R F O R M S   	        *
      * * * * * * * * * * * * * * * * * * * * * *
       
       P-TELA.
           DISPLAY TELA.
           ACCEPT WS-DATA FROM DATE.
           DISPLAY (04 72) DIA.
           DISPLAY (04 75) MES.
           DISPLAY (04 78) ANO.
       P-OPCAO-INCORRETA.
           DISPLAY (22 04) "OPCAO INCORRETA < ENTER >".
           ACCEPT (23 01) WS-P. 
           DISPLAY (22 04) WS-LIMPA.
       P-FIM-PROGRAMA.
           DISPLAY (01 01) ERASE.
           DISPLAY (11 20) "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!".
           DISPLAY (12 20) "!! F I M   D E   P R O G R A M A !!" .
           DISPLAY (13 20) "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!".
           STOP RUN.









