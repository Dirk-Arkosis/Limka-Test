     H*****************************************************************/
     H* SALES MANAGEMENT SYSTEM :  Calendar - Date calculation        */
     H*****************************************************************/
     H*****************************************************************/
     H*IDP*  16/09/2024                                               */
     H*  09/01/95 : A900 : Call off and inspection procedures         */
     H*  13/04/95 : C702 : Corrections III                            */
     H*  29/06/95 : A719 : Add country to Calendar                    */
     H*  04/09/95 : A725 : Route maintenance from calendar            */
     H*  22/09/95 : A755 : Add-on additions                           */
     H*  16/01/96 : A767 : Performance optimization                   */
     H*  10/02/97 : A804 : Corrections V                              */
     H*  09/07/97 : A819 : Exception error handling                   */
     H*  04/07/97 : A818 : Interfacing Sales->P.O->Sales              */
     H*  12/08/97 : M783 : Year 2000 Conversion                       */
     H*  10/02/97 : M770 : Bonus & commission calculation             */
     H*  27/02/98 : M795 : Year 2000 conversion. Part 3               */
     H*  04/06/98 : M201 : Performance                                */
     H*  04/01/99 : C210 : Small corrections                          */
     H*  03/07/00 : A216 : Lot characteristics                        */
     H*  10/09/24 : xxxx : Git it                                     */
     H*IDP*                                                           */
     H*****************************************************************/
     H*****************************************************************/
     H* Following possibilities exist when calling this program       */
     H*                                                               */
     H*  OPT   COM/BRN DAT  WRK  DAY  OUT                             */
     H* 'FS'   Req.    Req.  /    /   Filled : DAT is converted from  */
     H*                                        YMD format to the      */
     H*                                        ouput format defined   */
     H*                                        on company level.      */
     H* 'SF'   Req.    Req.  /    /   Filled : DAT is converted from  */
     H*                                        Ouput format to YMD.   */
     H* ' 2'    /      Req.  /    0    =DAT  : DAT is checked if it   */
     H*                                        is a valid date.       */
     H* ' 2'    /      Req. '1'   0    =DAT  : DAT is checked if it   */
     H*                                        is a valid working day.*/
     H* ' 2'    /      Req.  /   <>0  Filled : Days is added to DAT   */
     H*                                        Where days are consi-  */
     H*                                        dered normal days.     */
     H* ' 2'    /      Req. '1'  <>0  Filled : Days is added to DAT   */
     H*                                        Where days are consi-  */
     H*                                        dered working days.    */
A767AH* '68'    /      Req.  /    /   Filled : Convert 6 long day to  */
A767AH*                                        8 long. No checks are  */
A767AH*                                        made.                  */
     H* '99'    /       /    /    /   Filled : Current date is ret-   */
     H*                                        urned.                 */
     H*                                                               */
     H* WARNING: Date is 8 bytes. Use MOVE for filling the date.      */
     H*          OU6=Date in 6 positions. OU8=Date in 8 positions     */
     H*****************************************************************/
      /copy qrpglesrc,#hsrv
     FEACCOMP   IF   E           K DISK    USROPN
     FEACCALLA  IF   E           K DISK    USROPN
     FEACCALLC  IF   E           K DISK    USROPN
     F                                     RENAME(EACCAL:CALLC)
     FEACCALLD  IF   E           K DISK    USROPN
     F                                     RENAME(EACCAL:CALLD)
     F*
M201AD MTH             S             17    DIM(50)
M201AD LDM             S              3  0 DIM(50)
M201AD LD2             S              3  0 DIM(50)
     D*****************************************************************
     D*          DATE STRUCTURES
     D*****************************************************************
     D PAR999          DS           256
     D  OPT999                 1      2
     D  PGM999                 3     12
     D  ERR999                13     13
A818AD  F03999                53     53
     D  COM999               101    103
     D  BRN999               104    106
     D*Incoming date
     D  DAT999               107    114  0
     D*Incoming date 2 (Differnce is calculate between days)
     D  DA2999               115    122  0
     D*Working days
     D  WRK999               123    123
     D*Add days
     D  DAY999               124    128  0
     D*Outgoing date 6 positions
     D  OU6999               150    155
     D*Outgoing date 8 positions
     D  OU8999               156    163
A725AD*Incoming Country
     D  CCY999               164    168
A725AD*Outgoing Week
A725AD  WEK999               169    170  0
A725AD*Outgoing Day of week
A725AD  DWK999               171    171  0
A755AD*Outgoing Last day of the month (Opt 99 only !)
A755AD  LDM999               172    173  0
A755AD  LD2999               174    175  0
M785AD*Outgoing Relative record number (Calendar days)
M785AD  RNB999               176    180  0
     D                 DS
     D  YMD000                 1      8
     D  YMD00C                 1      2
     D  YMD00Y                 3      4
     D  YMD0CY                 1      4  0
     D  YMD00M                 5      6
     D  YMDN0M                 5      6  0
     D  YMD00D                 7      8
     D  YMDN0D                 7      8  0
     D                 DS
     D  FLD000                 1      8  0
     D  F01000                 1      2
     D  F02000                 3      4
     D  F03000                 5      6
     D  F04000                 7      8
M201AD                 DS
M201AD  KY5000                 1     17
M201AD  COM000                 1      3
M201AD  BRN000                 4      6
M201AD  CCY000                 7     11
M201AD  YEA000                12     15  0
M201AD  MTH000                16     17  0
A819AD SDS000         SDS
A819AD  PRM000           *PARMS
     C*****************************************************************
     C*          PARAMETERS AND KEYFIELDS
     C*****************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    PAR999
     C     PA65A4        PLIST
     C                   PARM                    PARMID            7
     C                   PARM      'EAAMSGF'     PARMFL           10
     C                   PARM                    PARMDT          100
     C                   PARM                    PARMCL            1
     C                   PARM      '*SAME'       PARMPQ            5
     C                   PARM      PGM999        PARMPG           10
DH99AC     PC05A2        PLIST
DH99AC                   PARM                    T125A2           12 0
DH99AC                   PARM                    DAT5A2            8
DH99AC                   PARM                    TIM5A2            6
     C     CALKEY        KLIST
     C                   KFLD                    COM999
     C                   KFLD                    BRN999
A719AC                   KFLD                    CCYCAL
     C                   KFLD                    YMD0CY
     C                   KFLD                    YMDN0M
     C                   KFLD                    YMDN0D
     C     CALKY2        KLIST
     C                   KFLD                    COM999
     C                   KFLD                    BRN999
A719AC                   KFLD                    CCYCAL
     C                   KFLD                    LNGCAL
     C     CALKY3        KLIST
     C                   KFLD                    COM999
     C                   KFLD                    BRN999
A719AC                   KFLD                    CCYCAL
     C                   KFLD                    RNBCAL
     C     CALKY4        KLIST
     C                   KFLD                    COM999
     C                   KFLD                    BRN999
A719AC                   KFLD                    CCYCAL
A755AC     CALKY5        KLIST
A755AC                   KFLD                    COM999
A755AC                   KFLD                    BRN999
A755AC                   KFLD                    CCYCAL
A755AC                   KFLD                    YMD0CY
A755AC                   KFLD                    YMDN0M
     C*****************************************************************
     C* S00-    MAIN PROCEDURE
     C*****************************************************************
M201AC     INI000        IFNE      '1'
M201AC                   MOVE      '1'           INI000            1
M201AC                   Z-ADD     0             M                 5 0
M201AC                   MOVE      *BLANKS       MTH
M201AC                   MOVE      *ZEROS        LDM
M201AC                   MOVE      *ZEROS        LD2
M201AC                   END
     C                   MOVE      '0'           ERR999
     C                   MOVE      *ZEROS        OU6999
     C                   MOVE      *ZEROS        OU8999
A725AC                   MOVE      *ZEROS        WEK999
A725AC                   MOVE      *ZEROS        DWK999
A755AC                   MOVE      *ZEROS        LDM999
M770AC                   MOVE      *ZEROS        LD2999
M795AC                   MOVE      *ZEROS        RNB999
C702AC     BRN999        IFEQ      *BLANKS
C702AC                   MOVEL     COM999        BRN999
C702AC                   END
     C*
     C     OPT999        IFEQ      'FS'
     C                   EXSR      FILSCR
     C                   ELSE
     C     OPT999        IFEQ      'SF'
     C                   EXSR      SCRFIL
     C                   ELSE
A767AC     OPT999        IFEQ      '68'
A767AC                   EXSR      CVT68
A767AC                   ELSE
     C     OPT999        IFEQ      '99'
     C                   EXSR      GETDAT
     C                   ELSE
     C                   EXSR      EDTDAT
     C                   END
A767aC                   END
     C                   END
     C                   END
     C*
     C*
     C                   Z-ADD     0             DAY999
     C*
A818AC     F03999        IFEQ      '1'
A818AC                   SETON                                        LR
A818AC                   ELSE
     C                   SETON                                        RT
A818AC                   END
     C                   RETURN
     C****************************************************************
     C*          Convert date from file to screen format
     C****************************************************************
     C     FILSCR        BEGSR
     C*
     C*
     C     COM999        IFNE      COMZ15
     C                   OPEN      EACCOMP
     C     COM999        CHAIN     EACCOMP                            50
     C                   CLOSE     EACCOMP
     C                   END
     C*
     C                   MOVE      DAT999        YMD000
     C     DAT999        IFNE      0
     C     YMD000        IFLT      '1000000'
     C     YMD00Y        IFLT      '80'
     C                   MOVE      '20'          YMD00C
     C                   ELSE
     C                   MOVE      '19'          YMD00C
     C                   END
     C                   END
     C                   END
     C*
     C     DTFZZZ        IFEQ      '1'
     C     YMD00D        CAT(P)    YMD00M:0      OU6999
     C                   CAT       YMD00Y:0      OU6999
     C     YMD00D        CAT(P)    YMD00M:0      OU8999
     C                   CAT       YMD00C:0      OU8999
     C                   CAT       YMD00Y:0      OU8999
     C                   END
     C*
     C     DTFZZZ        IFEQ      '2'
     C     YMD00M        CAT(P)    YMD00D:0      OU6999
     C                   CAT       YMD00Y:0      OU6999
     C     YMD00M        CAT(P)    YMD00D:0      OU8999
     C                   CAT       YMD00C:0      OU8999
     C                   CAT       YMD00Y:0      OU8999
     C                   END
     C*
     C     DTFZZZ        IFEQ      '3'
     C     YMD00Y        CAT(P)    YMD00M:0      OU6999
     C                   CAT       YMD00D:0      OU6999
     C                   MOVEL(P)  YMD000        OU8999
     C                   END
     C*
     C*
     C                   ENDSR
     C****************************************************************
     C*          Convert date from screen to file format
     C****************************************************************
     C     SCRFIL        BEGSR
     C*
     C* test
     C     COM999        IFNE      COMZ15
     C                   OPEN      EACCOMP
     C     COM999        CHAIN     EACCOMP                            50
     C                   CLOSE     EACCOMP
     C                   END
     C*
     C                   Z-ADD     DAT999        FLD000
     C*
     C     DTFZZZ        IFEQ      '1'
     C     FLD000        IFLT      1000000
     C     '00'          CAT(P)    F04000:0      YMD000
     C                   CAT       F03000:0      YMD000
     C                   CAT       F02000:0      YMD000
     C                   ELSE
     C     F03000        CAT(P)    F04000:0      YMD000
     C                   CAT       F02000:0      YMD000
     C                   CAT       F01000:0      YMD000
     C                   END
     C                   END
     C*
     C     DTFZZZ        IFEQ      '2'
     C     FLD000        IFLT      1000000
     C     '00'          CAT(P)    F04000:0      YMD000
     C                   CAT       F02000:0      YMD000
     C                   CAT       F01000:0      YMD000
     C                   ELSE
     C     F03000        CAT(P)    F04000:0      YMD000
     C                   CAT       F01000:0      YMD000
     C                   CAT       F02000:0      YMD000
     C                   END
     C                   END
     C*
     C     DTFZZZ        IFEQ      '3'
     C                   END
     C*
     C     DAT999        IFNE      0
M783AC     YMD00C        IFLE      '00'
     C     YMD00Y        IFLT      '80'
     C                   MOVE      '20'          YMD00C
     C                   ELSE
     C                   MOVE      '19'          YMD00C
     C                   END
M783AC                   END
     C                   END
     C                   MOVE      YMD000        OU8999
     C                   MOVE      YMD000        OU6999
     C*
     C*
     C                   ENDSR
     C****************************************************************
     C*          Get current date
     C****************************************************************
     C     GETDAT        BEGSR
     C*
     C*
DH99DC****                 MOVE UYEAR     YMD00Y
DH99DC****                 MOVE UMONTH    YMD00M
DH99DC****                 MOVE UDAY      YMD00D
DH99DC****       YMD00Y    IFLT '80'
DH99DC****                 MOVE '20'      YMD00C
DH99DC****                 ELSE
DH99DC****                 MOVE '19'      YMD00C
DH99DC****                 END
DH99DC***                  MOVE YMD000    OU6999
DH99DC****                 MOVE YMD000    OU8999
DH99AC                   TIME                    T125A2           12 0
DH99DC****                 CALL 'IAC05A2P'PC05A2
DH99AC                   CALL      'EAC10A2P'    PC05A2
DH99AC                   MOVE      DAT5A2        OU6999
DH99AC                   MOVE      DAT5A2        OU8999
DH99AC                   MOVE      DAT5A2        YMD000
     C*
A725AC     OPN000        IFNE      '1'
A725AC                   OPEN      EACCALLA
A725AC                   MOVE      '1'           OPN000            1
A725AC                   END
A725AC                   MOVEL     CCY999        CCYCAL
A725AC     CALKEY        CHAIN     EACCALLA                           50
A725AC     *IN50         IFEQ      '1'
A725AC                   MOVEL     *BLANKS       CCYCAL
A725AC     CALKEY        CHAIN     EACCALLA                           50
A725AC                   END
A725AC                   Z-ADD     WHNCAL        WEK999
A725AC                   Z-ADD     DAWCAL        DWK999
M795AC                   Z-ADD     RNBCAL        RNB999
     C*
M201DC****       CALKY5    SETGTEACCALLA
M201DC****       CALKY5    REDPEEACCALLA                 50
M201DC****       *IN50     DOWEQ'0'
M201DC****       WDICAL    ANDNE' '
M201DC****       LD2999    IFEQ 0
M201DC****       WDICAL    ANDNE'X'
M201DC****                 Z-ADDCDMCAL    LD2999
M201DC****                 END
M201DC****       CALKY5    REDPEEACCALLA                 50
M201DC****                 END
M201DC****       *IN50     IFEQ '0'
M201DC****                 Z-ADDCDMCAL    LDM999
M201DC****       LD2999    IFEQ 0
M201DC****                 Z-ADDCDMCAL    LD2999
M201DC****                 END
M201DC****                 END
M201AC                   EXSR      GETLDM
     C*
     C*
     C                   ENDSR
     C****************************************************************
     C*          Edit date
     C****************************************************************
     C     EDTDAT        BEGSR
     C*
     C*
     C     OPN000        IFNE      '1'
     C                   OPEN      EACCALLA
     C                   MOVE      '1'           OPN000            1
     C                   END
     C     OPN200        IFNE      '1'
     C     DAY999        ANDNE     0
     C                   OPEN      EACCALLC
     C****                 OPEN EACCALLD
     C                   MOVE      '1'           OPN200            1
     C                   END
     C*
     C                   MOVE      DAT999        YMD000
     C     YMD00C        IFEQ      '00'
     C     DAT999        ANDNE     *ZEROS
     C     YMD00Y        IFLT      '80'
     C                   MOVE      '20'          YMD00C
     C                   ELSE
     C                   MOVE      '19'          YMD00C
     C                   END
     C                   END
A719AC                   MOVEL     CCY999        CCYCAL
     C     CALKEY        CHAIN     EACCALLA                           50
     C     *IN50         IFEQ      '1'
A719AC                   MOVEL     *BLANKS       CCYCAL
A719AC     CALKEY        CHAIN     EACCALLA                           50
A719AC     *IN50         IFEQ      '1'
     C                   MOVE      'SMS8274'     PARMID
     C                   EXSR      ERRMSG
A719AC                   END
     C                   END
     C*
     C     DAY999        IFNE      0
     C*
     C     WRK999        IFEQ      '1'
     C*If Current is not a working day LNGCAL=0
     C     *IN50         DOWEQ     '0'
     C     LNGCAL        ANDEQ     0
     C     CALKY4        READE     EACCALLA                               50
     C                   END
     C  N50              ADD       DAY999        LNGCAL
     C  N50CALKY2        CHAIN     EACCALLC                           50
     C                   ELSE
     C     CALD00        IFNE      '1'
     C                   OPEN      EACCALLD
     C                   MOVE      '1'           CALD00            1
     C                   END
     C                   ADD       DAY999        RNBCAL
     C     CALKY3        CHAIN     EACCALLD                           50
     C                   END
     C     *IN50         IFEQ      '1'
     C     ERR999        ANDNE     '1'
     C                   MOVE      'SMS8275'     PARMID
     C                   EXSR      ERRMSG
     C                   END
     C*
     C                   ELSE
     C*
     C     WRK999        IFEQ      '1'
     C     WDICAL        ANDEQ     '0'
     C     ERR999        ANDEQ     '0'
     C                   MOVE      'SMS8276'     PARMID
     C                   EXSR      ERRMSG
     C                   END
     C*
     C                   END
     C*
     C     ERR999        IFNE      '1'
     C                   MOVEL     CYYCAL        YMD0CY
     C                   MOVEL     CMMCAL        YMD00M
     C                   MOVEL     CDMCAL        YMD00D
     C                   END
     C                   Z-ADD     WHNCAL        WEK999
     C                   Z-ADD     DAWCAL        DWK999
M795AC                   Z-ADD     RNBCAL        RNB999
     C                   MOVE      YMD000        OU8999
     C                   MOVE      YMD000        OU6999
     C*
A755AC     ERR999        IFNE      '1'
M201AC                   EXSR      GETLDM
A755AC                   END
     C*
     C*
     C                   ENDSR
M201AC****************************************************************
M201AC*          Get last working day of month
M201AC****************************************************************
M201AC     GETLDM        BEGSR
     C*
     C*
M201AC                   MOVEL     COM999        COM000
M201AC                   MOVEL     BRN999        BRN000
M201AC                   MOVEL     CCYCAL        CCY000
M201AC                   Z-ADD     YMD0CY        YEA000
M201AC                   Z-ADD     YMDN0M        MTH000
M201AC                   Z-ADD     1             I                 5 0
M201AC     KY5000        LOOKUP    MTH(I)                                 95
     C*
M201AC     *IN95         IFEQ      '1'
     C*
M201AC                   Z-ADD     LDM(I)        LDM999
M201AC                   Z-ADD     LD2(I)        LD2999
     C*
M201AC                   ELSE
     C*
M201MC     CALKY5        SETGT     EACCALLA
M201MC     CALKY5        READPE    EACCALLA                               50
M201MC     *IN50         DOWEQ     '0'
M201MC     WDICAL        ANDNE     ' '
M201MC     LD2999        IFEQ      0
M201MC     WDICAL        ANDNE     'X'
M201MC                   Z-ADD     CDMCAL        LD2999
M201MC                   END
M201MC     CALKY5        READPE    EACCALLA                               50
M201MC                   END
M201MC     *IN50         IFEQ      '0'
M201MC                   Z-ADD     CDMCAL        LDM999
M201MC     LD2999        IFEQ      0
M201MC                   Z-ADD     CDMCAL        LD2999
M201MC                   END
M201AC     M             IFLT      50
M201AC                   ADD       1             M
M201AC                   MOVE      KY5000        MTH(M)
M201AC                   Z-ADD     LDM999        LDM(M)
M201AC                   Z-ADD     LD2999        LD2(M)
M201AC                   END
M201MC                   END
     C*
M201AC                   END
     C*
     C*
M201AC                   ENDSR
     C****************************************************************
     C*          Convert day to 8 long without checking
     C****************************************************************
     C     CVT68         BEGSR
     C*
     C*
     C                   MOVE      DAT999        YMD000
     C     YMD00C        IFEQ      '00'
     C     DAT999        ANDNE     *ZEROS
     C     YMD00Y        IFLT      '80'
     C                   MOVE      '20'          YMD00C
     C                   ELSE
     C                   MOVE      '19'          YMD00C
     C                   END
     C                   END
     C                   MOVE      YMD000        OU8999
     C                   MOVE      YMD000        OU6999
     C*
     C*
     C                   ENDSR
     C****************************************************************
     C*          Send error message
     C****************************************************************
     C     ERRMSG        BEGSR
     C*
     C*
     C                   MOVE      '1'           ERR999
     C*
     C     PGM999        IFNE      *BLANKS
     C                   EXSR      FILSCR
     C                   MOVEL     OU6999        PARMDT
     C                   CALL      'EAA65A4P'    PA65A4
     C                   END
     C*
     C*
     C                   ENDSR
A819AC*****************************************************************
A819AC*          Exception error handling
A819AC*****************************************************************
A819AC     *PSSR         BEGSR
A819AC*
A819AC*
A819AC     PRM000        IFGE      1
A819AC                   CALL      'EAB01U1G'                           50
A819AC                   PARM                    SDS000
A819AC                   PARM                    PAR999
A819AC                   ELSE
A819AC                   CALL      'EAB01U1G'                           50
A819AC                   PARM                    SDS000
A819AC                   END
A819AC                   DUMP
A819AC*
A819AC*
A819AC                   ENDSR     '*CANCL'
