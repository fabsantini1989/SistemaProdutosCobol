       IDENTIFICATION DIVISION.
       PROGRAM-ID. STP003.
      *AUTHOR. FABIANO SANTINI MARQUES.
      **************************************
      * CADASTRO DE PRODUTOS            *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADPROD ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE  IS DYNAMIC
                      RECORD KEY   IS CODIGO
                      ALTERNATE RECORD KEY IS CODIGO
                      FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADPROD
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADPROD.DAT". 
       01 REGPROD.
	      03 CODIGO          PIC 9(06).
          03 DESCRICAO       PIC X(30).
          03 UNIDADE         PIC X(02).
          03 APLICACAO       PIC 9(01).
          03 QUANTIDADE      PIC 9(06).
          03 PRECO           PIC 9(08).  
          03 CNPJ            PIC 9(14).
          03 RSOC            PIC X(40).
          03 NOMEF           PIC X(12).
          03 CEP             PIC 9(08).
          03 NOME     	     PIC X(30).
          03 ENDERECO        PIC X(40).
          03 NUMERO          PIC 9(04).
          03 COMPLEMENTO     PIC X(12).
          03 BAIRRO          PIC X(20).
          03 CIDADE          PIC X(20).
          03 UF              PIC X(02).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-CONT        PIC 9(06) VALUE ZEROS.
       01 W-OPCAO       PIC X(01) VALUE SPACES.
       01 W-ACT         PIC 9(02) VALUE ZEROS.
       01 MENS          PIC X(50) VALUE SPACES.
       01 LIMPA         PIC X(50) VALUE SPACES. 
       01 ST-ERRO       PIC X(02) VALUE "00".
       01 W-SEL         PIC 9(01) VALUE ZEROS.
       01 IND           PIC 9(02) VALUE ZEROS.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELAFORN.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  "________________________________________".
           05  LINE 01  COLUMN 41 
               VALUE  "________________________________________".
           05  LINE 02  COLUMN 01 
               VALUE  "____________________________[CADASTRO DE".
           05  LINE 02  COLUMN 41 
               VALUE  " PRODUTOS]______________________________".			   
           05  LINE 04  COLUMN 1 VALUE "CODIGO.....: ".
           05  LINE 05  COLUMN 1 VALUE "DESCRICAO..: ".
           05  LINE 06  COLUMN 1 VALUE "UNIDADE....: ".           	   
		   05  LINE 07  COLUMN 1 VALUE "APLICACAO..: ".
		   05  LINE 08  COLUMN 1 VALUE "QUANTIDADE.: ".
		   05  LINE 09  COLUMN 1 VALUE "PRECO......: ".
           05  LINE 11  COLUMN 01 
               VALUE  "________________________________[FORNECED".
           05  LINE 11  COLUMN 41 
               VALUE  "DORES]___________________________________".		   
		   05  LINE 12  COLUMN 1 VALUE "CNPJ.......: ".
           05  LINE 13  COLUMN 1 VALUE "NOME F.....: ".
           05  LINE 14  COLUMN 1 VALUE "R. SOCIAL..: ".           	   
		   05  LINE 15  COLUMN 1 VALUE "CEP........: ".
		   05  LINE 16  COLUMN 1 VALUE "ENDERECO...: ".
		   05  LINE 17  COLUMN 1 VALUE "NUMERO.....: ".
		   05  LINE 18  COLUMN 1 VALUE "COMPL......: ".
		   05  LINE 19  COLUMN 1 VALUE "BAIRRO.....: ".
		   05  LINE 20  COLUMN 1 VALUE "CIDADE.....: ".
		   05  LINE 21  COLUMN 1 VALUE "ESTADO.....: ".
           05  LINE 22  COLUMN 01 
               VALUE  "__________________________________[PAINE".
           05  LINE 22  COLUMN 41 
               VALUE  "L]______________________________________".
           05  TCODIGO
               LINE 04  COLUMN 15  PIC 999.999
               USING  CODIGO.
           05  TDESCRICAO
               LINE 05  COLUMN 15   PIC X(30)
               USING  DESCRICAO.
           05  TUNIDADE
               LINE 06  COLUMN 15   PIC X(02)
               USING  UNIDADE.
           05  TAPLICACAO
               LINE 07  COLUMN 15   PIC 9(01)
               USING  APLICACAO.
           05  TQUANTIDADE
               LINE 08  COLUMN 15   PIC 99.999.9
               USING  QUANTIDADE.
           05  TPRECO
               LINE 09  COLUMN 15   PIC 99.9999.99
               USING  PRECO.
           05  TCNPJ
               LINE 12  COLUMN 15   PIC 99.999.999/9999.99
               USING  CNPJ.
           05  TNOMEF
               LINE 13  COLUMN 15   PIC X(12)
               USING  NOMEF.
           05  TRSOC
               LINE 14  COLUMN 15   PIC X(40)
               USING  RSOC.
           05  TCEP
               LINE 15  COLUMN 15   PIC 99999.999
               USING  CEP.
           05  TENDERECO
               LINE 16  COLUMN 15   PIC X(40)
               USING  ENDERECO.
           05  TNUMERO
               LINE 17  COLUMN 15   PIC 9999
               USING  NUMERO.
           05  TCOMPLEMENTO
               LINE 18  COLUMN 15  PIC X(12)
               USING  COMPLEMENTO.
           05  TBAIRRO
               LINE 19  COLUMN 15  PIC X(20)
               USING  BAIRRO.
           05  TCIDADE
               LINE 20  COLUMN 15  PIC X(20)
               USING  CIDADE.
           05  TUF
               LINE 21  COLUMN 15  PIC X(02)
               USING  UF.
      *
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
       
       R0.
           OPEN I-O CADPROD
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADPROD
                 CLOSE CADPROD
                 MOVE "*** ARQUIVO CADPROD FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                IF ST-ERRO = "95"
                    MOVE "*** ISAM NAO EXCUTADO **" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
                ELSE
                    MOVE "ERRO NA ABERTURA DO ARQUIVO CADPROD" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      * 
      *------------[ INICIALIZACAO DAS VARIAVEIS ]----------------------
       R1.
           MOVE ZEROS TO CODIGO APLICACAO
		   MOVE SPACES TO DESCRICAO UNIDADE
		   MOVE ZEROS TO QUANTIDADE PRECO.
		   MOVE SPACES TO RSOC NOMEF NOME ENDERECO
		   MOVE SPACES TO COMPLEMENTO BAIRRO CIDADE UF
           MOVE ZEROS TO CNPJ CEP NUMERO.
      *-------------[VISUALIZACAO DA TELA]------------------------------
           DISPLAY TELAFORN.
      *-------------[  ENTRADA DOS DADOS ]------------------------------
       R2.
           DISPLAY (23, 12) " INSIRA O CODIGO DO PRODUTO "
           ACCEPT TCODIGO
           DISPLAY (23, 12) "                      "
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   CLOSE CADPROD
                   GO TO ROT-FIM.
           IF W-ACT = 02
                     MOVE "*** TECLEI O F1 **" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R2.
           IF W-ACT = 10
                     MOVE "*** TECLEI O F9 **" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R2.
           IF CODIGO = ZEROS
                     MOVE "*** DADO NAO PODE FICAR VAZIO **" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R2.   
       LER-CADMFORN.
           MOVE 0 TO W-SEL
           READ CADPROD
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00" OR "02"
                DISPLAY TELAFORN
                MOVE "*** PRODUTO JA CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADPROD" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** PRODUTO NAO CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.
       R3.       
           ACCEPT TDESCRICAO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R1.
       R4.
           ACCEPT TUNIDADE.  
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R5.
       R5.
           ACCEPT TAPLICACAO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R6.
       R6.
           ACCEPT TQUANTIDADE.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R7.
       R7.
           ACCEPT TPRECO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R8.
       R8.
           ACCEPT TCNPJ.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R9.
       R9.
           ACCEPT TNOMEF.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R10.
       R10.
           ACCEPT TRSOC.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R11.
       R11.
           ACCEPT TCEP.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R12.
       R12.
           ACCEPT TENDERECO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R13.
       R13.
           ACCEPT TNUMERO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R14.
       R14.
           ACCEPT TCOMPLEMENTO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R15.
       R15.
           ACCEPT TBAIRRO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R16.
       R16.
           ACCEPT TCIDADE.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R17.
       R17.
           ACCEPT TUF.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ALT-OPC.				   
      * ------------- VERICAR SE E ALTERACAO -----------------
           IF W-SEL = 1 
                GO TO ALT-OPC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "GRAVAR (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R1.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGPROD
                IF ST-ERRO = "00"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU
					  ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "* FORNECEDOR JA EXISTE,DADOS NAO GRAVADOS *"
				  TO MENS
                  PERFORM ROT-MENS
				  THRU ROT-MENS-FIM
                  GO TO ACE-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE PRODUTO" 
					  TO MENS
                      PERFORM ROT-MENS
					  THRU ROT-MENS-FIM
                      GO TO ROT-FIM.

      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 12)
                     "N=NOVO REGISTRO   A=ALTERAR   E=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A" 
                    AND W-OPCAO NOT = "E" GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-OPCAO = "N"
                   GO TO R1  
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      GO TO R3.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADPROD RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO PRODUTO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGPROD
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO PRODUTO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *-----------------------------------------------------------------
       ROT-FIM.
           CLOSE CADPROD.
           STOP RUN.

      *---------[ ROTINA DE MENSAGEM ]---------------------
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (23, 12) MENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.


      

