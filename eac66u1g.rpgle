       //************************************************************************
       //SALES MANAGEMENT SYSTEM : Artikel fiche Limka
       //************************************************************************
      /copy qrpglesrc,#hsrv

       //************************************************************************
       // Files
       //************************************************************************
       dcl-f eaccomp  disk keyed usage(*input);
       dcl-f eacitmp  disk keyed usage(*input);
       dcl-f eansodl3 disk keyed usage(*input);
       dcl-f iieloqla disk keyed usage(*input);
       dcl-f iiestmlf disk keyed usage(*input);
       dcl-f eac66u1d workstn infds(infds_eac66u1d);

       //************************************************************************
       // Prototypes
       //************************************************************************
      /copy qrpglesrc,lscllpro
      /copy qrpglesrc,lsgenpro

       //************************************************************************
       // Data Strutures
       //************************************************************************
      /copy qrpglesrc,lsgensds

       dcl-ds infds_eac66u1d qualified;
         $01DEV  char(8) pos(001);
         $01CSR  int(5) pos(370);
       end-ds;

       dcl-s $00csr packed(4:0);

       //***********************************************************************
       // Main procedure
       //***********************************************************************
       initialize();

       write msgsfctl;
       exfmt eac6611;
       clearMessages();
       dow *inkc='0' and *inkl='0';

         $00csr=infds_eac66u1d.$01csr;

         if *inkd='1';
           prompt();
         else;
           editScreen1();
           if *in70='0';
             procesItem();
           endif;
         endif;

         if *inkc='0';
           positionCursor();
           write msgsfctl;
           exfmt EAC6611;
         endif;

       enddo;

       if com001<>parXAU05E1G.company;
         clear parXAU05E1G;
         parXAU05E1G.option=' 2';
         parXAU05E1G.fields='11000000';
         parXAU05E1G.application='SMS';
         parXAU05E1G.company=com001;
         callXAU05E1G(parXAU05E1G);
       endif;
       callEAA20A1P();
       *inlr='1';
       return;
       //***********************************************************************
       // position cursot
       //***********************************************************************
       dcl-proc positionCursor;

         $00crw=%div($00csr:256);
         $00ccl=%rem($00csr:256);

         *in59='1';
         if *in70='1' or *in19='1';
           *in59='0';
         endif;

       end-proc;
       //***********************************************************************
       // Proces item
       //***********************************************************************
       dcl-proc procesItem;

         callEAC66U2G(act001:com001:itm001:cpy001);

       end-proc;
       //***********************************************************************
       // Edit screen 1
       //***********************************************************************
       dcl-proc editScreen1;

         if com001=*blanks;
           *in70='1';
           *in71='1';
           callEAA65A4P('SMS0050':'EAAMSGF':'':'0':'*SAME':programSDS.program);
         else;
           chain (com001) eaccomp;
           if not %found(eaccomp);
             *in70='1';
             *in71='1';
             callEAA65A4P('SMS0106':'EAAMSGF':'':'0':'*SAME'
                         :programSDS.program);
           endif;
         endif;

         if %subst(act001:2:1)=*blanks;
           act001=' '+%subst(act001:1:1);
         endif;

         if *in70='0';
           chain (com001:itm001) eacitmp;
           if act001=' 1' and %found(eacitmp);
             *in70='1';
             *in72='1';
             callEAA65A4P('SMS0974':'EAAMSGF':itm001:'0':'*SAME'
                         :programSDS.program);
           endif;
           if act001<>' 1' and not %found(eacitmp);
             *in70='1';
             *in72='1';
             callEAA65A4P('SMS0109':'EAAMSGF':itm001:'0':'*SAME'
                         :programSDS.program);
           endif;
         endif;

         if *in70='0';
           if act001<>' 1' and act001<>' 2' and act001<>' 4';
             *in70='1';
             *in74='1';
             callEAA65A4P('SMS0039':'EAAMSGF':act001:'0':'*SAME'
                         :programSDS.program);
           endif;
         endif;

         if act001=' 1' and cpy001<>*blanks;
           chain (com001:cpy001) eacitmp;
           if not %found(eacitmp);
             *in70='1';
             *in76='1';
             callEAA65A4P('SMS0109':'EAAMSGF':itm001:'0':'*SAME'
                         :programSDS.program);
           endif;
         endif;

         if act001=' 4' and *in70='0';
           chain (com001:itm001) eansodl3;
           if %found(eansodl3);
             *in70='1';
             *in76='1';
             callEAA65A4P('SMS8160':'EAAMSGF':itm001:'0':'*SAME'
                         :programSDS.program);
           else;
             chain (com001:itm001) iiestmlf;
             chain (com001:itm001) iieloqla;
             if %found(iiestmlf) or %found(iieloqla);
               *in70='1';
               *in76='1';
               callEAA65A4P('SMS8161':'EAAMSGF':itm001:'0':'*SAME'
                           :programSDS.program);
             endif;
           endif;
         endif;


         clear parXAU10E1G;
         XAU10Company=com001;
         XAU10Caller=programSDS.program;
         XAU10user=programSDS.user;
         XAU10error='1';
         callXAU10E1G(parXAU10E1G);
         if XAU10error='1';
           *in70='1';
         endif;

       end-proc;
       //***********************************************************************
       // Prompt
       //***********************************************************************
       dcl-proc prompt;

         dcl-s pmt000 char(1) inz('0');

         if %scan('?':com001)>0 or $00fld='COM001';
           clear parEAC00I1G;
           eac00F04='1';
           callEAC00I1G(parEAC00I1G);
           com001=eac00Company;
           pmt000='1';
         endif;

         if %scan('?':itm001)>0 or $00fld='ITM001';
           if not itemPrompt(itm001);
             *in70='1';
             *in72='1';
           endif;
           pmt000='1';
         endif;

         if %scan('?':cpy001)>0 or $00fld='CPY001';
           if not itemPrompt(cpy001);
             *in70='1';
             *in76='1';
           endif;
           pmt000='1';
         endif;

         if pmt000<>'1';
           callEAA65A4P('SMS9041':'EAAMSGF':'':'0':'*SAME':programSDS.program);
         endif;

       end-proc;
       //***********************************************************************
       // Item prompt
       //***********************************************************************
       dcl-proc itemPrompt;
         dcl-pi *n ind;
           dcl-parm item char(15);
         end-pi;

           chain (com001) eaccomp;

         clear parEAC65I1P;
         parEAC65I1P.error='0';
         parEAC65I1P.F03='0';
         parEAC65I1P.F04='1';
         parEAC65I1P.company=com001;
         parEAC65I1P.sequence=itpcom;
         select;
           when itpcom=*blanks or itpcom='1';
             parEAC65I1P.itemFrom=%scanrpl('?':'':item);
             parEAC65I1P.itemTo=%trim(parEAC65I1P.itemFrom)
                               +'99999999999999999';
           when itpcom='2';
             parEAC65I1P.description=%trim(item)+'*';
           other;
             parEAC65I1P.matchCodeFrom=%scanrpl('?':'':item);
             parEAC65I1P.matchCodeTo=%trim(parEAC65I1P.itemFrom)
                               +'99999999999999999';
         endsl;

         callEAC65I1P(parEAC65I1P);

         if  parEAC65I1P.error='0';
           item=parEAC65I1p.itemFrom;
           return *on;
         else;
           return *off;
         endif;

       end-proc;
       //***********************************************************************
       // Initialize program
       //***********************************************************************
       dcl-proc clearMessages;
         dcl-s i int(10);

         callEAA65A4P(' ':'EAAMSGF':'':'1':'*SAME':programSDS.program);

         for i=70 to 97;
           *in(i)='0';
         endfor;

         *in19='0';

       end-proc;
       //***********************************************************************
       // Initialize program
       //***********************************************************************
       dcl-proc initialize;

         clear parXAU05E1G;
         parXAU05E1G.option=' 1';
         parXAU05E1G.fields='11000000';
         parXAU05E1G.application='SMS';
         callXAU05E1G(parXAU05E1G);
         if parXAU05E1G.error='0';
           com001=parXAU05E1G.company;
         endif;

         pgm000=programSDS.program;
         act001=' 2';
         *in19='1';

       end-proc;
