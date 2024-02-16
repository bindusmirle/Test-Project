       IDENTIFICATION DIVISION.                                         LSNNB402
       PROGRAM-ID.           LSNNB402.                                   0000002
      * CODE EXPANSION - VER 1.0 - CONVERTED - DELL SERVICES                    
*******                                                                 DELLRT13
       AUTHOR.        KAY MCGINNIS.                                      0000004
       DATE-WRITTEN.  APRIL 2011.                                        0000005
      *MODIFICATIONS.                                                   I       
      *REMARKS.       PROCESS NBS APPLICATIONS FROM WEB APP.             0000006
      *--------------------------------------------------------------*   0000007
      * THIS PROGRAM WILL ADD NEW BUSINESS APPLICATIONS SENT VIA     *   0000008
      * LAN2TP FROM WEB APPLICATION - IN A BATCH MODE.               *   0000009
      *--------------------------------------------------------------*   0000010
KM041 * 06/25/11 - TG1INTHV  R104   KAY MCGINNIS (KKM01)             *   0000011
KM041 *  FIX COPYBOOK NAME BEING PASSED TO LSNNB080 ON UPDATE OF AP  *   0000012
KM041 *  FILE - PLOG ENTRY LSSH267A                                  *   0000013
KM041 *--------------------------------------------------------------*   0000014
BT042 * 06/28/11 - TG1INTHV  R104   BRIAN THAL   (BJT01)             *   0000015
BT042 *  REPLACE DELETED LINE OF CODE FROM PREVIOUS ELEVATION.       *   0000016
BT042 *--------------------------------------------------------------*   0000017
BT043 * 07/06/11 - TG1INTHV  R104   BRIAN THAL   (BJT01)             *   0000018
BT043 *  CORRECT INV AND PCE ERROR PROCESSING.                       *   0000019
BT043 *--------------------------------------------------------------*   0000020
NK081 * 01/2012  - EFLBILP2  R108   NITIN KUSHWAHA                   *   0000021
NK081 *   UPDATED TO SUPPORT THE NEW INSTANT ISSUE TERM PRODUCT      *   0000022
NK081 *--------------------------------------------------------------*   0000023
BT091 * 04/2012  - TMIOCD12  P109   BRIAN THAL                       *   0000024
BT091 *   TASK RECORD MISSING IMAGE REF ID. WO #248984               *   0000025
BT091 *   TASK-MSTR-PDATE NOT POPULATED.                             *   0000026
BT091 *--------------------------------------------------------------*   0000027
KM101 * 06/2012 - METBIL63  R110  - KAY MCGINNIS                     *   0000028
KM101 * UPDATED TO PROCESS PRE-ASSIGNED POLICY NUMBERS AND 3 COMMENT *   0000029
KM101 * FIELDS ON TASK RECORD (ALL THE INPUT DATA ON WASY RECORD)    *   0000030
KM101 *--------------------------------------------------------------*   0000031
BT111 * 07/2012 - METBIL63  R111  - BRIAN THAL                       *   0000032
BT111 * FIX PLOG 120713AA LSNB3290 ABEND. SET WA-STATUS FOR ALL WA   *   0000033
BT111 * TRANSACTION RECORDS.                                         *   0000034
BT111 *--------------------------------------------------------------*   0000035
CL131 * 12/2012 - LNLBILDY  R113  - CHUCK LANNING                    *   0000036
CL131 * ENROLLMENT PROCESSING FOR GROUP                              *   0000037
CL131 *--------------------------------------------------------------*   0000038
BT132 * 12/2012 - LNLBILDY  R113  - BRIAN THAL                       *   0000039
BT132 * ENROLLMENT PROCESSING FOR GROUP                              *   0000040
BT132 *--------------------------------------------------------------*   0000041
AK131 * 11/2012 - EFLBILQL  R113  - ALKA KOHLI                       *   0000042
AK131 * CHANGES MADE TO UPDATE STP FOR OPT-IN INDICATOR AND EMAIL    *   0000043
AK131 * ADDRESS STORED ON PERSON MASTER FILE.                        *   0000044
AK131 *--------------------------------------------------------------*   0000045
MK131 * 11/2012 - EFLBILQF  R113  - MUKESH KUMAR                     *   0000046
MK131 * CHANGES MADE TO PROCESS E-PRE-APP FOR NEW INITIAL AND FINAL  *   0000047
MK131 * E-PRE-APPS.                                                  *   0000048
MK131 *--------------------------------------------------------------*   0000049
PS133 * 12/2012 - LNLBILDY  R113  - PATRICK SHANAHAN                 *   0000050
PS133 * ADDING APAP1-DELL-GUID-PROCESSED-FLAG TO THE PROCESSING OF   *   0000051
PS133 * THE W9 ALTERNATE INDEX TO PREVENT EXCESS I/O.                *   0000052
PS133 *--------------------------------------------------------------*   0000053
PS134 * 12/2012 - LNLBILDY  R113  - PATRICK SHANAHAN                 *   0000054
PS134 *  - FIXING ABEND-CODE FIELD DEFINITION                        *   0000055
PS134 *  - ADDING PROCESS TO CLEAR THE APAP1-DELL-GUID-PROCESSED-FLAG*   0000056
PS134 *  - ADDING READX PARM TO CONTROL PROCESSING AFTER RESTARTS    *   0000057
PS134 *           ---> PARM 01 = APPLICATION LIMIT                   *   0000058
PS134 *                        = PROCESSING WILL STOP WHEN THE TOTAL *   0000059
PS134 *                          NUMBER OF APPLICATIONS PROCESSED    *   0000060
PS134 *                          MATCHES THE APPLICATION LIMIT IN    *   0000061
PS134 *                          PARM 01.  ONLY 1 PARM 01 CARD IS    *   0000062
PS134 *                          ALLOWED.                            *   0000063
PS134 *                          APP-COUNTER IS ADVANCED +1 FOR EACH *   0000064
PS134 *                          WA-REC-TYPE = 'APAP  01' RECORD     *   0000065
PS134 *                          READ.                               *   0000066
PS134 *           ---> PARM 02 = DELL GUID BYPASS                    *   0000067
PS134 *                        = WHEN A DELL GUID (WA-DELL-GUID)     *   0000068
PS134 *                          IS LISTED AS A PARM 02, IT WILL     *   0000069
PS134 *                          NOT BE PROCESSED.  A MAXIMUM OF 10  *   0000070
PS134 *                          PARM 02 CARDS IS ALLOWED.           *   0000071
PS134 *--------------------------------------------------------------*   0000072
BT140 * 01/2013 - EFLBILQL  R114  - BRIAN THAL                       *   0000073
BT140 * FIX SOC04 PM SEGMENTS LOADING INTO WA-RECORD-TABLE           *   0000074
BT140 *--------------------------------------------------------------*   0000075
PS141 * 01/2013 - METBIL88  R114 - PATRICK SHANAHAN                  *   0000076
PS141 *  - ADDING CALL OF LSNNB452                                   *   0000077
PS141 *--------------------------------------------------------------*   0000078
BT142 * 01/2013 - METBIL88  R114 - BRIAN THAL                        *   0000079
BT142 *  - MET ENROLLMENT STP PROCESSING                             *   0000080
BT142 *--------------------------------------------------------------*   0000081
BT151 * 03/2013 - METBIL88  R115 - BRIAN THAL  CR-05                 *   0000082
BT151 *  - MET ENROLLMENT STP PROCESSING  ADD DELETE FUNCTIONALITY   *   0000083
BT151 *  - MET CHANGE INDICATOR WASY-LS-PARAM-37 VALUE 'D'           *   0000084
BT151 *  -  ADD DELETE FUNCTIONALITY TASK RECORDS                    *   0000085
BT151 *--------------------------------------------------------------*   0000086
BT152 * 04/2013 - METBIL88  R115 - BRIAN THAL                        *   0000087
BT152 *  -  FIX TO RETAIN APAP2-OLD-POLICY-NA VALUE                  *   0000088
BT152 *  -  FOR DELETE FUNCTION,  EENRL/DEL WORKFLOW TASK RECORDS    *   0000089
BT152 *--------------------------------------------------------------*   0000090
BT153 * 04/2013 - EFLBILQF  R115  - BRIAN THAL                       *   0000091
BT153 * FIX DUPLICATES, POPULATE IMAGE REFID ON FINAL APP.WO #281950 *   0000092
BT153 *--------------------------------------------------------------*   0000093
BT154 * 04/2013 - METBIL2K  R115  - BRIAN THAL                       *   0000094
BT154 * MET DIRECT STP PROCESSING                                    *   0000095
BT154 *--------------------------------------------------------------*   0000096
BT161 * 05/2013 - METBIL2K  R116  - BRIAN THAL.                      *   0000097
BT161 * FIX WO# 285274 LAN2TP AB SEG SEQUENCE CAUSED TP ABEND THAT   *   0000098
BT161 * RESULTED IN BATCH CYCLE ABEND TO ADD SY RECORD THAT EXISTS   *   0000099
BT161 * PLOG FIX 130528AJ AND 130525AC AND 130524AA                  *   0000100
BT161 *--------------------------------------------------------------*   0000101
BT162 * 04/2013 - METBIL2K  R115  - BRIAN THAL  CR02                 *   0000102
BT162 * MET DIRECT STP PROCESSING  ADD AC SEGMENT FUNCTIONALITY      *   0000103
BT162 *--------------------------------------------------------------*   0000104
BT170 *  07/2013 - METBIL88 - R117 - BRIAN THAL                          0000105
BT170 *    CHANGE REQUEST - 14, FIX ISSUE WITH MANNUAL POLICY ENTERED    0000106
BT170 *    AND CHANGE IND TYPE 'A' OR 'U' ON RECYCLE FILE                0000107
BT170 *    PREVENT DUPLICATE POLICY BEING GENERATED.                     0000108
BT170 *--------------------------------------------------------------*   0000109
BT171 * 08/2013 - EFLBILT6  R117  - BRIAN THAL                       *   0000110
BT171 * UPDATE PROCESS STEP EAPPLC/DAT FOR TASK REPORTING.           *   0000111
BT171 *--------------------------------------------------------------*   0000112
BT181 *  09/2013 LF1INT58 - R118 BRIAN THAL                              0000113
BT181 *    ADD FIELD FOR EMPLOYER ID POPULATE ON TASK RECORDS.           0000114
BT181 ****************************************************************** 0000115
CL181 * 09/2013 - LF1INT58  R118  - CHUCK LANNING                    *   0000116
CL181 * ASSIGN THE COVERAGE APPID FROM TABLE, INSTEAD OF ADDING 1    *   0000117
CL181 *--------------------------------------------------------------*   0000118
BT182 *  09/2013 MAPBIL25 - R118 BRIAN THAL                          *   0000119
BT182 * UPDATE PROCESS STEP EAPPL/DAA EAPPL/DAD FOR TASK REPORTING.  *   0000120
BT182 ****************************************************************   0000121
BT183 *  09/2013 METINT3N - R118 BRIAN THAL                          *   0000122
BT183 * SET BRIDGE LIMIT TO 49999 FROM 39999                         *   0000123
BT183 ****************************************************************   0000124
BT184 *  09/2013 METBIL4R - R118 BRIAN THAL                          *   0000125
BT184 * FIX HPQC DEFECT 5593 ADD TO COMPARE LOGIC FIELDS             *   0000126
BT184 * FRATERNAL-INFO2-NA, BASE-PLAN-PLANOPT-NA, N1-REC-N1          *   0000127
BT184 * OCC-CLASS-N1                                                 *   0000128
BT184 ****************************************************************   0000129
BT185 *  10/2013 METBIL4R - R118 BRIAN THAL                          *   0000130
BT185 * INITIALIZE SAVE-EMPLOYER-ID TO SPACES.                       *   0000131
BT185 ****************************************************************   0000132
BT186 *  10/2013 MAPBIL25 - R118 BRIAN THAL                          *   0000133
BT186 * CALL ROUTINE FOR ADDITIONAL TASKS PROCESSING.                *   0000134
BT186 ****************************************************************   0000135
BT191 *  11/2013 METBIL7H - R119 BRIAN THAL                          *   0000136
BT191 * OVERRIDE '03' POPULATE SERVICING AGENT FROM ALT ID LOOK UP   *   0000137
BT191 ****************************************************************   0000138
NK192 *  11/2013 METCON4Z - R119 NITIN KUSHWAHA                      *   0000139
NK192 * ADD TASK CLOSED TIME AND TASK OPEN TIME                      *   0000140
NK192 ****************************************************************   0000141
BT193 *  11/2013 FSTBILQ9 - R119 BRIAN THAL                          *   0000142
BT193 * CALL NEW PROGRAM FOR OVERRIDE '04' POPULATE CONTRACT STATE   *   0000143
BT193 ****************************************************************   0000144
BT194 *  12/2013 METBIL7E - R119 BRIAN THAL                          *   0000145
BT194 * PEND-CT    SET TASK CLOSED STATUS 009 OR LESS.               *   0000146
BT194 * PERFORM NEXT STEPS.                                          *   0000147
BT194 ****************************************************************   0000148
BT195 *  12/2013 METCON18 - R119 BRIAN THAL                          *   0000149
BT195 * FIX COMPARE TO FIND NEW BUSINESS POLICY ON FIELD OCC CLASS   *   0000150
BT195 ****************************************************************   0000151
SM211 *  03/2014 METINT3K - R121 STEVEN MATHIAS               [SRM01]*  DELLRET2
SM211 * CHANGES WRITE2 TO WRITE4 TO REUSE WRITE2 FOR THE             *   0000153
SM211 *  NEW NFU FILE.                                               *   0000154
SM211 ****************************************************************   0000155
BT212 *  03/2014 METINT3K - R121 BRIAN THAL                   BJT01  *   0000156
BT212 * ADD CODE TO GENERATE ENNRL NFU TASK                          *   0000157
BT212 * UPDATE MESSAGE ON TASK FOR POLICY NOT FOUND                  *   0000158
BT212 ****************************************************************   0000159
BT213 *  04/2014 METINT3K - R121 BRIAN THAL                   BJT01  *   0000160
BT213 * ADD CDC-DATE FOR AB SEGMENT COMPARE                          *   0000161
BT213 ****************************************************************   0000162
BT214 *  04/2014 METINT3K - R121 BRIAN THAL   CR001           BJT01  *   0000163
BT214 * ADD EENRL CHG TASK IN CLOSE STATUS FOR SUCCESSFUL NFU        *   0000164
BT214 * UPDATE. AUTOMATIC UPDATES FOR COMPARE FIELDS                 *   0000165
BT214 ****************************************************************   0000166
BT231 *  06/2014 FG2BIL11 - R123 BRIAN THAL                   BJT01  *   0000167
BT231 * ADD CODE TO PROCESS OW SEGMENTS                              *   0000168
BT231 ****************************************************************   0000169
SM241 *  09/2014 METBILBE - R124 STEVEN MATHIAS              [SRM01] *  DELLRET2
SM241 * ADD CODE FOR THE NEW PHONE TP SEGMENT OF THE POLICY. [SM241] *  DELLRET2
SM241 ****************************************************************  R       
PS251 *  01/2015 METCONEU - R125 PATRICK SHANAHAN                    *  DELLRETC
PS251 * CHANGING CALL OF LSNNB545 TO CALL LSNNB547 INSTEAD.          *  DELLRETC
PS251 * THIS ALLOWS FOR THE NEW ALTERNATE INDEX TO BE USED IN BATCH  *  DELLRETC
PS251 ****************************************************************  DELLRETC
BT251 *  12/2014 - METINTA2 - R125 - BRIAN THAL                      *  DELLRETC
BT251 *    WO# 27585 RESET THE STATUS FIELD ON AB RECORDS THAT       *  DELLRETC
BT251 *    WERE SET TO BE DELETED IN PROGRAM  LSNNB545               *  DELLRETC
BT251 *    THEY NEED TO BE RETAINED ON RECYCLE FILE                  *  DELLRETC
BT251 *--------------------------------------------------------------*  DELLRETC
BT261 *  02/2015 - METINTEV - R126 - BRIAN THAL                      *  DELLRETC
BT261 *    WO#G31578 AB SEG NOT UPDATED ON SPOUSE RIDER. INITIALIZE  *  DELLRETC
BT261 *    ALL RECORDS ON THE NB FILE FOR WORKSITE UPDATE TRANSACTION*  DELLRETC
BT261 *    PROCESSING THE POLICY IS STILL IN NEW BUSINESS.           *  DELLRETC
BT261 *--------------------------------------------------------------*  DELLRETC
BT262 *  02/2015 - METINTEV - R126 - BRIAN THAL                      *  DELLRETC
BT262 *    WO#G32315 UPDATE TRANSACTION SENT WITH PMPN RECORDS, WHEN *  DELLRETC
BT262 *    POLICY WAS STILL IN NEW BUSINESS THE PERSON MASTER UPDATE *  DELLRETC
BT262 *    FAILED AND SET THE APPLICATION TO 008.                    *  DELLRETC
BT262 *--------------------------------------------------------------*  DELLRETC
BT271 *  04/2014 - METINTEV - R127 - BRIAN THAL                         DELLRETC
BT271 *    WO# 32886 FIX RSE (RECORD SET INCCOMPLETE) AB SEG RECORD  *  DELLRETC
BT271 *    ADD BACK THE ORIGINAL FOR APP ID 01 AND DELETE THE AB     *  DELLRETC
BT271 *    RECORD ADDED IN THE POST IMPORT PROCESS FOR APP ID 02.    *  DELLRETC
BT271 *    AND DELETE AB SEG WHERE WA-TOTAL-RECS = 999.              *  DELLRETC
BT271 *--------------------------------------------------------------*  DELLRETC
DD281 *  06/2015 - METBILE2 - R128 - DIPANNITA DUTTA                    DELLRET2
DD281 *    ADDING PROCESSING OF NEW RECORD TYPE APOR. THIS IS A NEW  *  DELLRET2
DD281 *    RECORD TYPE ADDED IN APPLICATION FILE. FOR THIS TYPE OF   *  DELLRET2
DD281 *    RECORD A NEW REQUEST 10/0039 WOULD BE ADDED ON 04 SCREEN. *  DELLRET2
DD281 *--------------------------------------------------------------*  DELLRET2
BT282 *  07/2015 - METCONEU - R128 - BRIAN THAL   ACMS 42685         *  DELLRET2
BT282 *    WO# 44593 FIX WHEN DELETE FAILS IN 4600-CHECK-NEW-BUSINESS*  DELLRET2
BT282 *    DO NOT CONTINUE TO ADD POLICY IN NEW BUSINESS. FIX TO     *  DELLRET2
BT282 *    RETAIN POLICY NUMBER ON GUID FILE, UNKNOWN WILL BE USED   *  DELLRET2
BT282 *    WHEN GU-POLICY IS SPACES.                                 *  DELLRET2
BT282 *--------------------------------------------------------------*  DELLRET2
VC291 *  07/2015 - STABIL30 - R129 - VIBHA CHAUDHARY                 *  DELLRET2
VC291 *    ADDED LOGIC AND A CALL TO LSNNB548 FOR STANDARD.          *  DELLRET2
VC291 *--------------------------------------------------------------*  DELLRET2
BT292 *  08/2015 METINTEV - R129 BRIAN THAL                  [BJT01] *  DELLRET2
BT292 * FIX ACTION DATE FOR ADDITIONAL TASKS WITH STATUS OF H BJ292  *  DELLRET2
BT292 * WO # 46900     DEFAULT WILL BE 5 DAYS                [BJ292] *  DELLRET2
BT292 ****************************************************************  DELLRET2
BT301 *  09/2015 METINTF5 - R130 BRIAN THAL                  [BJT01] *  DELLRET2
BT301 * REQ 002, 003 AND 007.                                 BJ301  *  DELLRET2
BT301 ****************************************************************  DELLRET2
BT302 *  09/2015 METINTF5 - R130 BRIAN THAL                  [BJT01] *  DELLRET2
BT302 * REQ 001, AND METBILHE.                                BJ301  *  DELLRET2
BT302 ****************************************************************  DELLRET2
BT311 *  01/2016 - METINTEV - R131 - BRIAN THAL   ACMS 444724        *  DELLRET2
BT311 *    WO# 59984 FIX WHEN 008 FOR WASY-FINAL-STATUS RESET POLICY *  DELLRET2
BT311 *    TO STATUS 008 AND GENERATE TASK WITH PENDING STATUS       *  DELLRET2
BT311 ****************************************************************  DELLRET2
DL321 *  01/2016 - METINTEV - R132 - DOUG LANNIN  ACMS #44986        *  DELLRET3
DL321 *    WO# 59985 FIX WHEN 010 FOR WASY-FINAL-STATUS RESET STATUS *  DELLRET3
DL321 *    CODE AND DO NOT CREATE FIELD TO SPACES SO RECORDS WILL    *  DELLRET3
DL321 *    NOT DROP OFF RECYCLE FILE                                 *  DELLRET3
DL321 ****************************************************************  DELLRET3
DL322 *  02/2016 - METINTKH - R132 - DOUG LANNIN  ACMS #45143        *  DELLRET3
DL322 *    WO# 59985 FIX WHEN 010 FOR WASY-FINAL-STATUS DELETE NBAB  *  DELLRET3
DL322 *    RECORDS WHERE REC COUNT = 999                             *  DELLRET3
DL322 ****************************************************************  DELLRET3
BT331 *  04/2016 METINTF5 - R133 BRIAN THAL                  [BJT01] *  DELLRET5
BT331 *  FIX HPQC 8777 AND 8778.                              BT331 *   DELLRET5
BT331 *-------------------------------------------------------------*   DELLRET5
DL343 *  05/2016 - MAPBIL79 - R134 - DOUG LANNIN                    *   DELLRET6
DL343 *    ADDED UNDERWRITING STATUS UPDATE FUNCTIONALITY           *   DELLRET6
DL343 *-------------------------------------------------------------*   DELLRET6
DL344 *  05/2016 - METINT8U - R134 - DOUG LANNIN                    *   DELLRET6
DL344 *    UPDATE STP TO SUPPORT THE ELECTRONIC DATA ENTRY SLA      *   DELLRET6
DL344 *    MEASUREMENT.                                             *   DELLRET6
DL344 *-------------------------------------------------------------*   DELLRET6
BT345 *  06/2016 - METBILIC - R134 - BRIAN THAL                     *   DELLRET6
BT345 *    UPDATE TO SUPPORT COC, NEW TASKS                         *   DELLRET6
BT345 *-------------------------------------------------------------*   DELLRET6
BT346 *  06/2016 - MAPINT2A - R134 - BRIAN THAL                     *   DELLRET6
BT346 *    FIX TICKET 77345, PREVENT SBC AS BASE COVERAGE           *   DELLRET6
BT346 *-------------------------------------------------------------*   DELLRET6
DL347 *  07/2016 - MAPBIL79 - R134 - DOUG LANNIN                    *   DELLRET6
DL347 *    ADD NEW VALIDATION CONDITION SINCE XML 160005 WILL NOT   *   DELLRET6
DL347 *    CONTAIN PLAN INFORMATION                                 *   DELLRET6
DL347 *-------------------------------------------------------------*   DELLRET6
DL348 *  07/2016 - FSTBIL9Q - R134 - DOUG LANNIN                    *   DELLRET6
DL348 *    ADDED SPECIAL TASK PROCESSING                            *   DELLRET6
DL348 *-------------------------------------------------------------*   DELLRET6
BT349 *  07/2016 - TG2INT1F - R134 - BRIAN THAL                     *   DELLRET6
BT349 *    HARD CODE 7Z TO BYPASS BRIDGE LIMIT                      *   DELLRET6
BT349 *-------------------------------------------------------------*   DELLRET6
DL351 *  07/2016 - MAPBIL79 - R135 - DOUG LANNIN                    *   DELLRET7
DL351 *    MOVED INITIALIZATION CODE THAT WAS AT THE BEGINNING OF   *   DELLRET7
DL351 *    PARA'S 2100 AND 2100E TO PARA 1110-VALIDATION-PROCESS.   *   DELLRET7
DL351 *    I NEEDED TO HAVE THE SAVE-WASY-OVERRIDE-IND SET FOR THE  *   DELLRET7
DL351 *    NEW 160005 TRANSACTION                                   *   DELLRET7
DL351 *-------------------------------------------------------------*   DELLRET7
DL352 *  07/2016 - FSTBIL9Q - R135 - DOUG LANNIN                    *   DELLRET7
DL352 *    CORRECTED SPECIAL TASK PROCESSING                        *   DELLRET7
DL352 *-------------------------------------------------------------*   DELLRET7
DL353 *  08/2016 - FSTBIL9Q - R135 - DOUG LANNIN                    *   DELLRET7
DL353 *    CR006 UPDATES TO SUPPORT IMAGING                         *   DELLRET7
DL353 *-------------------------------------------------------------*   DELLRET7
DL354 *  09/2016 - MAPBIL79 - R135 - DOUG LANNIN                    *   DELLRET8
DL354 *    CORRECTED GO TO AFTER THE CALL TO LSNNB458               *   DELLRET8
DL354 *-------------------------------------------------------------*   DELLRET8
BT361 *  09/2016 - TG2INT1F - R136 - BRIAN THAL                     *   DELLRET8
BT361 *    SET BRIDGE LIMIT TO 100,000 FOR OVERRIDE 02              *   DELLRET8
BT361 *-------------------------------------------------------------*   DELLRET8
BT362 *  09/2016 - METBILLF - R136 - BRIAN THAL                     *   DELLRET8
BT362 *    UPDATE TO SUPPORT VOLUNTARY CANCEL                       *   DELLRET8
BT362 *-------------------------------------------------------------*   DELLRET8
BT363 *  11/2016 - METBILLF - R136 - BRIAN THAL                     *   DELLRET8
BT363 *    BACK OUT CODE                                            *   DELLRET8
BT363 *-------------------------------------------------------------*   DELLRET8
BT371 *  09/2016 - METBILLF - R137 - BRIAN THAL                     *   DELLRET9
BT371 *    UPDATE TO SUPPORT VOLUNTARY CANCEL                       *   DELLRET9
BT371 *    REALIGN AND INSTALL METBILLF                             *   DELLRET9
BT371 *-------------------------------------------------------------*   DELLRET9
DL372 *  11/2016 - SRECNV2U - R137 - DOUG LANNIN                    *   DELLRET9
DL372 *    UPDATES TO SUPPORT ADDITIONAL TASK CREATION              *   DELLRET9
DL372 *-------------------------------------------------------------*   DELLRET9
BT373 *  11/2016 - METBILLF - R137 - BRIAN THAL                     *   DELLRET9
BT373 *    UPDATE ORDER REQ STATUS FROM 'N' TO 'O'                  *   DELLRET9
BT373 *-------------------------------------------------------------*   DELLRET9
DL391 *  03/2017 - SREBIL3V - R139 - DOUG LANNIN                    *   DELLRT11
DL391 *    UPDATES TO ADDITIONAL TASK CREATION                      *   DELLRT11
DL391 *-------------------------------------------------------------*   DELLRT11
BT392 *  03/2017 - METBILLF - R139 - BRIAN THAL                     *   DELLRT10
BT392 *    FIX FOR R2:000055282418  CORRECT LENGHT SAVE-LS-PARAM-8-1*   DELLRT10
BT392 *-------------------------------------------------------------*   DELLRT10
DL401 *  05/2017 - EREINT88/FG2BIL5W - R140 - DOUG LANNIN           *   DELLRT12
DL401 *    SET STATUS TO 'IND' WHEN PRE-ASSIGNED POLICY AND         *   DELLRT12
DL401 *    POLICY ASSIGNMENT INDICATOR ARE NOT IN SYNC              *   DELLRT12
DL401 *-------------------------------------------------------------*   DELLRT12
PS401 *  06/2017 - ERIBIL82  - R140 - PATRICK SHANAHAN  (ERIE@50)   *   DELLRT12
PS401 *    ADDING PROCESSING FOR FINAL STATUS OF 1125 RECORDS       *   DELLRT12
PS401 *-------------------------------------------------------------*   DELLRT12
PS402 *  06/2017 - ERIBIL82  - R140 - PATRICK SHANAHAN  (ERIE@50)   *   DELLRT12
PS402 *    PERMANENT FIX FOR FOR FINAL STATUS OF 1125 RECORDS       *   DELLRT12
PS402 *-------------------------------------------------------------*   DELLRT12
BT411 *  08/2017 - EREINT88 - R141 - BRIAN THAL R2:000056721025     *   DELLRT13
BT411 *    FIX CODE FOR ERIRE NOT TO SET ERROR FOR UPDATES          *   DELLRT13
BT411 *    WHEN WA-POLCRECTYPE = 'U' DO NOT SET ERROR FOR           *   DELLRT13
BT411 *    SET STATUS TO 'IND' WHEN PRE-ASSIGNED POLICY AND         *   DELLRT13
BT411 *    POLICY ASSIGNMENT INDICATOR ARE NOT IN SYNC              *   DELLRT13
BT411 *-------------------------------------------------------------*   DELLRT13
DL431 *  01/2018 - SREBIL4G          - R143 - DOUG LANNIN           *   DELLRT15
DL431 *    SECOND BN/BD COMBINATIONS WERE NOT BEING PROCESSED       *   DELLRT15
DL431 *    CORRECTLY RESULTING IN WA-STATUS ALWAYS BEING SET TO 008 *   DELLRT15
DL431 *-------------------------------------------------------------*   DELLRT15
PS443 *  02/2018 - ERIBIL82          - R144 - PATRICK SHANAHAN      *   DELLRT16
PS443 *    PREVENTING IND STATUS FROM BEING APPLIED TO POLRECTYPE S *   DELLRT16
PS443 *-------------------------------------------------------------*   DELLRT16
BT444 *  02/2018 - FG2BIL7W          - R144 - BRIAN THAL             *  DELLRT16
BT444 *    ADD NEW PROGRAM LSNNB426 TO PROCESS NEW STP RECORD        *  DELLRT16
BT444 *    EI INFORMATION FOR STRATEGIES.                            *  DELLRT16
BT444 *-------------------------------------------------------------*   DELLRT16
DL441 *  01/2018 - METBILPA          - R144 - DOUG LANNIN           *   DELLRT16
DL441 *    BENEFIT AMOUNT OVERRIDE                                  *   DELLRT16
DL441 *-------------------------------------------------------------*   DELLRT16
DL442 *  01/2018 - METCON3V          - R144 - DOUG LANNIN           *   DELLRT16
DL442 *    SEPARATING PHS FROM NBS FOR METLIFE                      *   DELLRT16
DL442 *-------------------------------------------------------------*   DELLRT16
DL451 *  03/2018 - METCON3V          - R145 - DOUG LANNIN           *   DELLRT17
DL451 *    SEPARATING PHS FROM NBS FOR METLIFE - CORRECTION         *   DELLRT17
DL451 *-------------------------------------------------------------*   DELLRT17
DL451 *  03/2018 - METBILPA          - R145 - DOUG LANNIN           *   DELLRT17
DL451 *    BENEFIT AMOUNT OVERRIDE - ADDITIONAL UPDATES             *   DELLRT17
DL451 *-------------------------------------------------------------*   DELLRT17
PS452 *  04/2018 - ERIBIL82          - R145 - PATRICK SHANAHAN      *
PS452 *    CORRECTING TASK WRITING FOR BATCH.                       *
PS452 *-------------------------------------------------------------*
PS453 *  04/2018 - ERIBIL82          - R145 - PATRICK SHANAHAN      *
PS453 *    PREVENTING DUPLICATE TASKS FROM WRITING.                 *
PS453 *-------------------------------------------------------------*
DL454 *  04/2018 - METBILPA          - R145 - DOUG LANNIN           *   DELLRT17
DL454 *    BENEFIT AMOUNT OVERRIDE - ADDITIONAL UPDATES             *   DELLRT17
DL454 *-------------------------------------------------------------*   DELLRT17
DL455 *  04/2018 - METBILPA          - R145 - DOUG LANNIN           *   DELLRT17
DL455 *    BENEFIT AMOUNT OVERRIDE - ADDITIONAL UPDATES             *   DELLRT17
DL455 *-------------------------------------------------------------*   DELLRT17
DL456 *  04/2018 - METBILPA          - R145 - DOUG LANNIN           *   DELLRT17
DL456 *    BENEFIT AMOUNT OVERRIDE - ADDITIONAL UPDATES             *   DELLRT17
DL456 *-------------------------------------------------------------*   DELLRT17
DL457 *  05/2018 - METCON3V          - R145 - DOUG LANNIN           *   DELLRT17
DL457 *    ADDING RECORD COUNTS                                     *   DELLRT17
DL457 *-------------------------------------------------------------*   DELLRT17
DL458 *  05/2018 - METCON3V          - R145 - DOUG LANNIN           *   DELLRT17
DL458 *    UPDATING RECORD COUNTS                                   *   DELLRT17
DL458 *-------------------------------------------------------------*   DELLRT17
DL461 *  05/2018 - SREBIL9F          - R146 - DOUG LANNIN           *
DL461 *    INCOMPLETE/WITHDRAWAL PROCESSING                         *
DL461 *-------------------------------------------------------------*
AR150 *  01/2019 - FG2BIL9I          - R150 - ARVINDER               *
AR150 *    ADD NEW PROGRAMS LSNNB427 LSNNB428 AND LSSNB429  TO       *
AR150 *    PROCESS NEW STP RECORDS AL GW AND IB INFORMATION FOR      *
AR150 *    STRATEGIES.                                               *
AR150 *--------------------------------------------------------------*
SS521 *  03/2019 - SREBIL2X          - R152 - SACHIN SHETTY         *
SS521 *    ADD NEW PROGRAM LSNNB430 TO PROCESS NEW STP RECORDS WITH *
SS521 *    DI SEGMENT.                                              *
SS521 ***************************************************************
AS541 *  05/2019 INC#000061996582 TO PASS COMPANY NUMBER TO
AS541 *     TASK-COMPANY OF TASK MASTER WHICH WILL BE USED
AS541 *     IN LSN160.
AS541 *-------------------------------------------------------------*
AS551 *  05/2019 - METBILU3         - R155 - ARVINDER SODHI
AS551 *     UPDATED CODE FOR METBILU3 REQ-003 
AS551 *-------------------------------------------------------------*
AS951 *  OCT 2019 SREINTCA - R159 ARVINDER SODHI                     *  
AS951 *        UPDATES TO ADDITIONAL TASK CREATION                   *  
AS951 *--------------------------------------------------------------*
DL631 *  03/2020 - METBILU3 - DOUG LANNIN                            *
DL631 *    UPDATED EMPLOYER ID ON CERTAIN TASK RECORDS THAT WERE     *
DL631 *    SPACES                                                    *
DL631 ****************************************************************
BT641 *  04/2020 - R2:000063349724 R2:000063305840 - Brian Thal      *
BT641 *  Fix error code not in table, PARAM 38 populated and policy  *
BT641 *  not in PHS, but on added to report NB32008. Add E74 error   *
BT641 ****************************************************************
PB631 *  04/2020 - SREBIL2X-C6       - R163 - PRASHANTH K B          *
PB631 *    REQ002-UPDATE NBS PROCESSING TO AUTOMATICALLY POPULATE THE*
PB631 *    REASON CODE WHEN THE POLICY STATUS IS UPDATED.            *
PB631 ****************************************************************
DL651 *  04/2020 - SREBILER - R163 DOUG LANNIN                       *
DL651 *    TASK STATUS WILL BE SET TO 'C' FOR ALL APPL/009 TASKS     *
DL651 *    ADDITIONAL CHANGES TO IFMAIL/INC TASKS                    *
DL651 ****************************************************************
DL681 *  07/2020 - EREBIL3O - R168 DOUG LANNIN                       *
DL681 *    UPDATES TO LETTER WRITER                                  *
DL681 ****************************************************************
RS691 *  09/2020 - STAINT9S - R169 RAMNIWAS SINGH                    *
RS691 *  ALL EENRL / DAT TASKS WILL BE AUTOMATICALLY CLOSED BY STP   *
RS691 *  REGARDLESS OF STATUS.                                       *
RS691 ****************************************************************
BT701 *  09/2020 - METBILWD - R170 BINDU THOMAS                      *
BT701 ****************************************************************
JJ711 *  10/2020 - BRIDGE LIMIT ENHANCEMENT - R170 JYOTI             *
JJ711 *  BRIDGE LIMIT ENHANCEMENT AND RESET COUNTER                  *
JJ711 *  OF POLICY COUNT IN RE  FILE                                 *
JJ711 ****************************************************************
NK771 *  12/2020 - FG2BILGH - R177 NARENDER KUMAR                    *
NK771 *  ADDITION OF PROCESSING FOR NEWLY ADDED FM SEGMENT.          *
NK771 ****************************************************************
DL791 *  07/2021 - METBILX8 - R179 - DOUG LANNIN                     *
DL791 *  ADDED NB/DI AS VALID WA RECORD                              *
DL791 ****************************************************************
DL821 *  08/2021 - METBILX8 - R182 - DOUG LANNIN                     *
DL821 *  ADDED EAPPID LOGIC WHEN DOING POLICY LOOKUP FOR WHOLE LIFE  *
DL821 ****************************************************************
DL831 *  10/2021 - METBILX8 - R183 - DOUG LANNIN                     *
DL831 *  ON PHS MATCH OR NBS ADD MOVE POLICY NUMBER TO WASY RECORD   *
DL831 ****************************************************************
PB881 *  02/2022 - METBILX3 - R188A - PRASHANTHA K B                 *
PB881 *    UPDATE TO AUTOMATE NOTE AND TASK CREATIONS FOR CONTRACT   *
PB881 *    STATE UPDATE.                                             *
PB881 ****************************************************************
PB882 *  02/2022 - METINTX3 - R188A - PRASHANTHA K B                 *
PB882 *    NOT GENERATE THE EENRL CHG FOR AUTOMATIC CHANGES          *
PB882 *    EENRL CHG TASK FOR CONTRACT STATE CHANGE GENERATE         *
PB882`*    ONLY ONE EENRL CHG TASK AND POPULATE THE TASK STATUS DATE *
PB882 ****************************************************************
PB883 *  03/2022 - METINTX3 - R188A - PRASHANTHA K B                 *
PB883 *  POPULATE GUID TO TASK-COMMENT1-A FOR EENRL CHG TASK         *
PB833 ****************************************************************
DG891 *  05/2022 - STAINTA8 - R189  - DEEPANSHU GARG                 *
DG891 *    ADDED ALTERNATE INDEX TO VSAMEM TO BE USED IN LSNNB548    *
DG891 **************************************************************** 
DL90A *  06/2022 - STAINT8A - R190A - DOUG LANNIN                    *
DL90A *    UPDATED CANCEL LOGIC WHEN FINAL STATUS EQUALS '019' FOR   *
DL90A *    STANDARD OVERRIDE 'SS'                                    *
DL90A *--------------------------------------------------------------*
NK200A* TG2PRD1U - R200-A - NITIN KUSHWAHA                           *
NK200A*  REDUCE THE SIZE OF TASK MASTER FILE.                        *
NK200A****************************************************************
BP931 *  09/2022 - ME2INT80 - R193 - BABA PANDA                      *
BP931 *    UPDATED TO POPULATE A NEW FIELD APCV1-POLICY WITH THE     *
BP931 *    POLICY NUMBER ON THE AP CV1 RECORD.                       *
BP931 *--------------------------------------------------------------*
AS991 *  03/2023 - ME2BIL53 - R199 - AMIT KUMAR SINGH                *
AS991 *    TO DETERMINE NEW ENROLLMENT IF CONVERSION                 *
AS991 *--------------------------------------------------------------*
AS201 *  05/2023 - ME2BIL1B - AISWARYA LAKSHMI S                     *
AS201 *    TO CREATE A NEW REPL LTC TASK ON SCREEN 07/PF24 - TS001   *
AS201 *    THIS APPLIES TO LO1 PRODUCT. REMARK1 & REMARK2 IS ALSO    *
AS201 *    POPULATED ON TS002 SCREEN                                 *
AS201 *--------------------------------------------------------------*
AS203 *  06/2023 - ME2BIL1B - AISWARYA LAKSHMI S                     *
AS203 *    TO POPULATE REMARK1 & REMARK2 ON TASK SCREEN WITH         *
AS203 *    REPL POLICY AND REPL INSURER/COMPANY NAME (PER UIS FILE)  *
AS203 *    RESPECTIVELY, FOR LTC TASKS OF LO1 PRODUCT                *
AS203 *--------------------------------------------------------------*
AS204 *  06/2023 - ME2INT1A - PHASE1 - AISWARYA LAKSHMI S            *
AS204 *   UPDATES TO POPULATE REMARK2 AND REMARK3 ON TS002 SCREEN    *
AS204 *   FOR AUTOMATIC CLOSE TASKS - AS PER ADMIN                   *
AS204 ****************************************************************
DL04A *  07/2023 - FG2INTMH - FG2BILKS - DOUG LANNIN                 *
DL04A *    ADDED CONDITIONAL TO CREATE TASK IN 'P' STATUS            *
DL04A ****************************************************************


       ENVIRONMENT DIVISION.                                             0000172
       INPUT-OUTPUT  SECTION.                                            0000173
       FILE-CONTROL.                                                     0000174
           SELECT VSAM-AC ASSIGN VSAMAC                                 DELMODID
               ORGANIZATION IS INDEXED                                  DELMODID
               ACCESS MODE IS DYNAMIC                                   DELMODID
               RECORD KEY IS VSAM-AC-PRIM                               DELMODID
               FILE STATUS IS VSAM-AC-FS.                               DELMODID
           SELECT VSAM-AG ASSIGN VSAMAG                                 DELMODID
               ORGANIZATION IS INDEXED                                  DELMODID
               ACCESS MODE IS DYNAMIC                                   DELMODID
               RECORD KEY IS VSAM-AG-PRIM                               DELMODID
               FILE STATUS IS VSAM-AG-FS.                               DELMODID
           SELECT VSAM-A3 ASSIGN VSAMA3                                 DELLIXCH
               ORGANIZATION IS INDEXED                                  DELLIXCH
               ACCESS MODE IS DYNAMIC                                   DELLIXCH
               RECORD KEY IS VSAM-A3-PRIM                               DELLIXCH
               ALTERNATE RECORD KEY IS VSAM-A4-ALT-KEY1 WITH DUPLICATES
               FILE STATUS IS VSAM-A3-FS.                               DELLIXCH
           SELECT VSAM-CS ASSIGN VSAMCS                                 DELMODID
               ORGANIZATION IS INDEXED                                  DELMODID
               ACCESS MODE IS DYNAMIC                                   DELMODID
               RECORD KEY IS VSAM-CS-PRIM                               DELMODID
               FILE STATUS IS VSAM-CS-FS.                               DELMODID
           SELECT VSAM-EM ASSIGN VSAMEM                                 DELMODID
               ORGANIZATION IS INDEXED                                  DELMODID
               ACCESS MODE IS DYNAMIC                                   DELMODID
               RECORD KEY IS VSAM-EM-PRIM                               DELMODID
DG891          ALTERNATE RECORD KEY IS VSAM-EM-ALT-E1 WITH DUPLICATES
               FILE STATUS IS VSAM-EM-FS.                               DELMODID
           SELECT VSAM-GS ASSIGN VSAMGS                                 DELMODID
               ORGANIZATION IS INDEXED                                  DELMODID
               ACCESS MODE IS DYNAMIC                                   DELMODID
               RECORD KEY IS VSAM-GS-PRIM                               DELMODID
               ALTERNATE RECORD KEY IS VSAM-GS-ALT-KEY1 WITH DUPLICATES DELLMNCH
               ALTERNATE RECORD KEY IS VSAM-G5-ALT-KEY1 WITH DUPLICATES DELLMNCH
               FILE STATUS IS VSAM-GS-FS.                               DELMODID
           SELECT VSAM-HD ASSIGN VSAMHD                                 DELMODID
               ORGANIZATION IS INDEXED                                  DELMODID
               ACCESS MODE IS DYNAMIC                                   DELMODID
               RECORD KEY IS VSAM-HD-PRIM                               DELMODID
               FILE STATUS IS VSAM-HD-FS.                               DELMODID
           SELECT VSAM-NT ASSIGN VSAMNT                                 DELMODID
               ORGANIZATION IS INDEXED                                  DELMODID
               ACCESS MODE IS DYNAMIC                                   DELMODID
               RECORD KEY IS VSAM-NT-PRIM                               DELMODID
               FILE STATUS IS VSAM-NT-FS.                               DELMODID
           SELECT VSAM-NU ASSIGN VSAMNU                                 DELMODID
               ORGANIZATION IS INDEXED                                  DELMODID
               ACCESS MODE IS DYNAMIC                                   DELMODID
               RECORD KEY IS VSAM-NU-PRIM                               DELMODID
               FILE STATUS IS VSAM-NU-FS.                               DELMODID
           SELECT VSAM-PL ASSIGN VSAMPL                                 DELMODID
               ORGANIZATION IS INDEXED                                  DELMODID
               ACCESS MODE IS DYNAMIC                                   DELMODID
               RECORD KEY IS VSAM-PL-PRIM                               DELMODID
               FILE STATUS IS VSAM-PL-FS.                               DELMODID
           SELECT VSAM-PM ASSIGN VSAMPM                                 DELMODID
               ORGANIZATION IS INDEXED                                  DELMODID
               ACCESS MODE IS DYNAMIC                                   DELMODID
               RECORD KEY IS VSAM-PM-PRIM                               DELMODID
               FILE STATUS IS VSAM-PM-FS.                               DELMODID
           SELECT VSAM-RT ASSIGN VSAMRT                                 DELMODID
               ORGANIZATION IS INDEXED                                  DELMODID
               ACCESS MODE IS DYNAMIC                                   DELMODID
               RECORD KEY IS VSAM-RT-PRIM                               DELMODID
               FILE STATUS IS VSAM-RT-FS.                               DELMODID
           SELECT VSAM-RV ASSIGN VSAMRV                                 DELMODID
               ORGANIZATION IS INDEXED                                  DELMODID
               ACCESS MODE IS DYNAMIC                                   DELMODID
               RECORD KEY IS VSAM-RV-PRIM                               DELMODID
               FILE STATUS IS VSAM-RV-FS.                               DELMODID
           SELECT VSAM-SR ASSIGN VSAMSR                                 DELMODID
               ORGANIZATION IS INDEXED                                  DELMODID
               ACCESS MODE IS DYNAMIC                                   DELMODID
               RECORD KEY IS VSAM-SR-PRIM                               DELMODID
               FILE STATUS IS VSAM-SR-FS.                               DELMODID
           SELECT VSAM-SS ASSIGN VSAMSS                                 DELMODID
               ORGANIZATION IS INDEXED                                  DELMODID
               ACCESS MODE IS DYNAMIC                                   DELMODID
               RECORD KEY IS VSAM-SS-PRIM                               DELMODID
               FILE STATUS IS VSAM-SS-FS.                               DELMODID
           SELECT VSAM-TC ASSIGN VSAMTC                                 DELMODID
               ORGANIZATION IS INDEXED                                  DELMODID
               ACCESS MODE IS DYNAMIC                                   DELMODID
               RECORD KEY IS VSAM-TC-PRIM                               DELMODID
AS203          ALTERNATE KEY IS VSAM-TC-ALT-KEY WITH DUPLICATES
               FILE STATUS IS VSAM-TC-FS.                               DELMODID
           SELECT VSAM-TH ASSIGN VSAMTH                                 DELMODID
               ORGANIZATION IS INDEXED                                  DELMODID
               ACCESS MODE IS DYNAMIC                                   DELMODID
               RECORD KEY IS VSAM-TH-PRIM                               DELMODID
               FILE STATUS IS VSAM-TH-FS.                               DELMODID
           SELECT VSAM-TK ASSIGN VSAMTK                                 DELMODID
               ORGANIZATION IS INDEXED                                  DELMODID
               ACCESS MODE IS DYNAMIC                                   DELMODID
               RECORD KEY IS VSAM-TK-PRIM                               DELMODID
               ALTERNATE RECORD KEY IS VSAM-ALT-KEY5 WITH DUPLICATES    DELLAIXC
               FILE STATUS IS VSAM-TK-FS.                               DELMODID
           SELECT VSAM-AP ASSIGN VSAMAP                                 DELLIDCH
               ORGANIZATION IS INDEXED                                  DELLIDCH
               ACCESS MODE IS DYNAMIC                                   DELLIDCH
               RECORD KEY IS VSAM-AP-PRIM                               DELLIDCH
               ALTERNATE RECORD KEY IS VSAM-ALT-N3KEY WITH DUPLICATES   DELLAIXC
               ALTERNATE RECORD KEY IS VSAM-AP-N5 WITH DUPLICATES       DELLMNCH
               FILE STATUS IS VSAM-AP-FS.                               DELLIDCH
           SELECT VSAM-GU ASSIGN VSAMGU                                 DELLIDCH
               ORGANIZATION IS INDEXED                                  DELLIDCH
               ACCESS MODE IS DYNAMIC                                   DELLIDCH
               RECORD KEY IS VSAM-GU-PRIM                               DELLIDCH
               ALTERNATE RECORD KEY IS VSAM-ALT-GU-KEY1 WITH DUPLICATES DELLMNCH
               FILE STATUS IS VSAM-GU-FS.                               DELLIDCH
           SELECT VSAM-MS ASSIGN VSAMMS                                 DELLIDCH
              ORGANIZATION IS INDEXED                                   DELLIDCH
              ACCESS MODE IS DYNAMIC                                    DELLIDCH
              RECORD KEY IS VSAM-MS-PRIM                                DELLIDCH
               ALTERNATE RECORD KEY IS VSAM-MS-ALT-KEY WITH DUPLICATES  DELLMNCH
               ALTERNATE RECORD KEY IS VSAM-ALT-KEY1 WITH DUPLICATES    DELLMNCH
              FILE STATUS IS VSAM-MS-FS.                                DELLIDCH
           SELECT VSAM-NB ASSIGN VSAMNB                                 DELLIDCH
               ORGANIZATION IS INDEXED                                  DELLIDCH
               ACCESS MODE IS DYNAMIC                                   DELLIDCH
               RECORD KEY IS VSAM-NB-PRIM                               DELLIDCH
               FILE STATUS IS VSAM-NB-FS.                               DELLIDCH
           SELECT VSAM-PO ASSIGN VSAMPO                                 DELLIDCH
               ORGANIZATION IS INDEXED                                  DELLIDCH
               ACCESS MODE IS DYNAMIC                                   DELLIDCH
               RECORD KEY IS VSAM-PO-PRIM                               DELLIDCH
               ALTERNATE RECORD KEY IS VSAM-ALT-KEY4 WITH DUPLICATES    DELLMNCH
               FILE STATUS IS VSAM-P2-FS.                               DELLIDCH
           SELECT VSAM-P3 ASSIGN VSAMP3                                 DELLMNCH
               ORGANIZATION IS INDEXED                                  DELLMNCH
               ACCESS MODE IS DYNAMIC                                   DELLMNCH
               RECORD KEY IS VSAM-P3-REC                                DELLMNCH
               FILE STATUS IS VSAM-P3-FS.                               DELLMNCH
           SELECT VSAM-RE ASSIGN VSAMRE                                 DELLIDCH
               ORGANIZATION IS INDEXED                                  DELLIDCH
               ACCESS MODE IS DYNAMIC                                   DELLIDCH
               RECORD KEY IS VSAM-RE-PRIM                               DELLIDCH
               FILE STATUS IS VSAM-RE-FS.                               DELLIDCH
           SELECT VSAM-WA ASSIGN VSAMWA                                 DELLIDCH
               ORGANIZATION IS INDEXED                                  DELLIDCH
               ACCESS MODE IS DYNAMIC                                   DELLIDCH
               RECORD KEY IS VSAM-WA-PRIM                               DELLIDCH
               ALTERNATE RECORD KEY IS VSAM-ALT-WA-KEY1 WITH DUPLICATES DELLMNCH
               FILE STATUS IS VSAM-WA-FS.                               DELLIDCH
           SELECT VSAM-W8 ASSIGN VSAMW8                                 DELLIDCH
               ORGANIZATION IS INDEXED                                  DELLIDCH
               ACCESS MODE IS DYNAMIC                                   DELLIDCH
               RECORD KEY IS VSAM-W8-PRIM                               DELLIDCH
               FILE STATUS IS VSAM-W8-FS.                               DELLIDCH
           SELECT VSAM-WF ASSIGN VSAMWF                                 DELLIDCH
               ORGANIZATION IS INDEXED                                  DELLIDCH
               ACCESS MODE IS DYNAMIC                                   DELLIDCH
               RECORD KEY IS VSAM-WF-PRIM                               DELLIDCH
               FILE STATUS IS VSAM-WF-FS.                               DELLIDCH
NK771      SELECT VSAM-FS ASSIGN VSAMFS   
NK771          ORGANIZATION IS INDEXED    
NK771          ACCESS MODE IS DYNAMIC     
NK771          RECORD KEY IS VSAM-FS-PRIM 
NK771          FILE STATUS IS VSAM-FS-FS. 
AS991      SELECT VSAM-PC ASSIGN VSAMPC   
AS991          ORGANIZATION IS INDEXED    
AS991          ACCESS MODE IS DYNAMIC     
AS991          RECORD KEY IS VSAM-PC-PRIM 
AS991          FILE STATUS IS VSAM-PC-FS. 
           SELECT READ1 ASSIGN UT-S-READ1                               DELLSQCH
           FILE STATUS IS WS-READ1-FS.                                  DELLSQCH
           SELECT READ2 ASSIGN UT-S-READ2                               DELLRT16
           FILE STATUS IS WS-READ2-FS.                                  DELLRT16
           SELECT WRITE1 ASSIGN UT-S-WRITE1                             DELLMNCH
           FILE STATUS IS WS-WRITE1-FS.                                 DELLMNCH
           SELECT WRITE2 ASSIGN UT-S-WRITE2                             DELLMNCH
           FILE STATUS IS WS-WRITE2-FS.                                 DELLMNCH
           SELECT WRITE3 ASSIGN UT-S-WRITE3                             DELLSQCH
           FILE STATUS IS WS-WRITE3-FS.                                 DELLSQCH
           SELECT WRITE4 ASSIGN UT-S-WRITE4                             DELLSQCH
           FILE STATUS IS WS-WRITE4-FS.                                 DELLSQCH
           SELECT WRITE5 ASSIGN UT-S-WRITE5                             DELLRET6
           FILE STATUS IS WS-WRITE5-FS.                                 DELLRET6
           SELECT WRITE6 ASSIGN UT-S-WRITE6                             DELLMNCH
           FILE STATUS IS WS-WRITE6-FS.                                 DELLRT12
           SELECT WRITE7 ASSIGN UT-S-WRITE7                             DELLRT16
           FILE STATUS IS WS-WRITE7-FS.                                 DELLRT16
           SELECT WRITE8 ASSIGN UT-S-WRITE8                             DELLRT16
           FILE STATUS IS WS-WRITE8-FS.                                 DELLRT16
DL461      SELECT WRITE9 ASSIGN UT-S-WRITE9
DL461      FILE STATUS IS WS-WRITE9-FS.
           SELECT DATEFILE ASSIGN UT-S-DATEFILE.                         0000175
           SELECT  NUSEQFL  ASSIGN TO NUSEQFL                           DELLMNCH
           FILE STATUS IS WS-NUSEQ-FS.                                  DELLMNCH
                                                                         0000176
       DATA DIVISION.                                                    0000177
       FILE SECTION.                                                     0000178
       FD  VSAM-AC  IS EXTERNAL                                         DELMODID
           RECORDING MODE F  BLOCK 0 RECORDS  LABEL                     DELMODID
           RECORDS STANDARD  DATA RECORDS VSAM-AC-REC.                  DELMODID
       01  VSAM-AC-REC.                                                 DELMODID
         05  VSAM-AC-PRIM            PIC X(28).                         DELMODID
         05  FILLER                  PIC X(272).                        DELMODID
       FD  VSAM-AG  IS EXTERNAL                                         DELMODID
           RECORDING MODE V  BLOCK 0 RECORDS  LABEL                     DELMODID
           RECORDS STANDARD RECORD IS VARYING                           DELMODID
           IN SIZE FROM 25 TO 2000                                      DELMODID
           DATA RECORDS VSAM-AG-REC.                                    DELMODID
       01  VSAM-AG-REC.                                                 DELMODID
         05  FILLER                  PIC X(4).                          DELMODID
         05  VSAM-AG-PRIM            PIC X(21).                         DELMODID
         05  FILLER                  PIC X(1975).                       DELMODID
       FD  VSAM-A3  IS EXTERNAL                                         DELLIXCH
           RECORDING MODE IS F                                          DELLIXCH
           DATA RECORDS VSAM-A3-REC.                                    DELLIXCH
       01  VSAM-A3-REC.                                                 DELLIXCH
         05 VSAM-A3-PRIM.
            10  VSAM-A4-ALT-KEY1        PIC X(31).                      DELLIXCH
            10  FILLER                  PIC X(21).                      DELLIXCH
       FD  VSAM-CS  IS EXTERNAL                                         DELMODID
           RECORDING MODE V  BLOCK 0 RECORDS  LABEL                     DELMODID
           RECORDS STANDARD RECORD IS VARYING                           DELMODID
           IN SIZE FROM 56 TO 2500                                      DELMODID
           DATA RECORDS VSAM-CS-REC.                                    DELMODID
       01  VSAM-CS-REC.                                                 DELMODID
         05  FILLER                  PIC X(4).                          DELMODID
         05  VSAM-CS-PRIM            PIC X(52).                         DELMODID
         05  FILLER                  PIC X(2444).                       DELMODID
       FD  VSAM-EM  IS EXTERNAL                                         DELMODID
           RECORDING MODE F  BLOCK 0 RECORDS  LABEL                     DELMODID
           RECORDS STANDARD  DATA RECORDS VSAM-EM-REC.                  DELMODID
       01  VSAM-EM-REC.                                                 DELMODID
         05  VSAM-EM-PRIM            PIC X(7).                          DELMODID
DG891    05  FILLER                  PIC X(10).                         DELMODID
DG891    05  VSAM-EM-ALT-E1          PIC X(6).                          DELMODID
DG891    05  FILLER                  PIC X(2477).                       DELMODID
       FD  VSAM-GS  IS EXTERNAL                                         DELMODID
           RECORDING MODE V  BLOCK 0 RECORDS  LABEL                     DELMODID
           RECORDS STANDARD                                             DELLMNCH
           RECORD  IS VARYING IN SIZE FROM 117 TO 2500                  DELLMNCH
           DATA RECORDS VSAM-GS-REC.                                    DELMODID
       01  VSAM-GS-REC.                                                 DELMODID
         05  FILLER                  PIC X(4).                          DELMODID
         05  VSAM-GS-PRIM.                                              DELMODID
           10  FILLER                PIC X(11).                         DELLMNCH
           10  VSAM-GS-ALT-KEY1      PIC X(27).                         DELLMNCH
           10  FILLER                PIC X(58).                         DELLMNCH
         05  VSAM-G5-ALT-KEY1        PIC X(17).                         DELLALTC
         05  FILLER                  PIC X(2383).                       DELLMNCH
       FD  VSAM-HD  IS EXTERNAL                                         DELMODID
           RECORDING MODE F  BLOCK 0 RECORDS  LABEL                     DELMODID
           RECORDS STANDARD  DATA RECORDS VSAM-HD-REC.                  DELMODID
       01  VSAM-HD-REC.                                                 DELMODID
         05  VSAM-HD-PRIM            PIC X(16).                         DELMODID
       FD  VSAM-NT  IS EXTERNAL                                         DELMODID
           RECORDING MODE F  BLOCK 0 RECORDS  LABEL                     DELMODID
           RECORDS STANDARD  DATA RECORDS VSAM-NT-REC.                  DELMODID
       01  VSAM-NT-REC.                                                 DELMODID
         05  VSAM-NT-PRIM            PIC X(12).                         DELMODID
         05  FILLER                  PIC X(2488).                       DELMODID
       FD  VSAM-NU  IS EXTERNAL                                         DELMODID
           RECORDING MODE F  BLOCK 0 RECORDS  LABEL                     DELMODID
           RECORDS STANDARD  DATA RECORDS VSAM-NU-REC.                  DELMODID
       01  VSAM-NU-REC.                                                 DELMODID
         05  VSAM-NU-PRIM            PIC X(50).                         DELMODID
         05  FILLER                  PIC X(8550).                       DELMODID
       FD  VSAM-PL  IS EXTERNAL                                         DELMODID
           RECORDING MODE F  BLOCK 0 RECORDS  LABEL                     DELMODID
           RECORDS STANDARD  DATA RECORDS VSAM-PL-REC.                  DELMODID
       01  VSAM-PL-REC.                                                 DELMODID
         05  VSAM-PL-PRIM            PIC X(23).                         DELMODID
         05  FILLER                  PIC X(527).                        DELMODID
       FD  VSAM-PM  IS EXTERNAL                                         DELMODID
           RECORDING MODE F  BLOCK 0 RECORDS  LABEL                     DELMODID
           RECORDS STANDARD  DATA RECORDS VSAM-PM-REC.                  DELMODID
       01  VSAM-PM-REC.                                                 DELMODID
         05  VSAM-PM-PRIM            PIC X(16).                         DELMODID
         05  FILLER                  PIC X(3984).                       DELMODID
       FD  VSAM-RT  IS EXTERNAL                                         DELMODID
           RECORDING MODE V  BLOCK 0 RECORDS  LABEL                     DELMODID
           RECORDS STANDARD RECORD IS VARYING                           DELMODID
           IN SIZE FROM 60 TO 568                                       DELMODID
           DATA RECORDS VSAM-RT-REC.                                    DELMODID
       01  VSAM-RT-REC.                                                 DELMODID
         05  FILLER                  PIC X(4).                          DELMODID
         05  VSAM-RT-PRIM            PIC X(56).                         DELMODID
         05  FILLER                  PIC X(508).                        DELMODID
       FD  VSAM-RV  IS EXTERNAL                                         DELMODID
           RECORDING MODE F  BLOCK 0 RECORDS  LABEL                     DELMODID
           RECORDS STANDARD  DATA RECORDS VSAM-RV-REC.                  DELMODID
       01  VSAM-RV-REC.                                                 DELMODID
         05  VSAM-RV-PRIM            PIC X(25).                         DELMODID
         05  FILLER                  PIC X(35).                         DELMODID
       FD  VSAM-SR  IS EXTERNAL                                         DELMODID
           RECORDING MODE F  BLOCK 0 RECORDS  LABEL                     DELMODID
           RECORDS STANDARD  DATA RECORDS VSAM-SR-REC.                  DELMODID
       01  VSAM-SR-REC.                                                 DELMODID
         05  VSAM-SR-PRIM            PIC X(22).                         DELMODID
         05  FILLER                  PIC X(378).                        DELMODID
       FD  VSAM-SS  IS EXTERNAL                                         DELMODID
           RECORDING MODE V  BLOCK 0 RECORDS  LABEL                     DELMODID
           RECORDS STANDARD RECORD IS VARYING                           DELMODID
           IN SIZE FROM 56 TO 2500                                      DELMODID
           DATA RECORDS VSAM-SS-REC.                                    DELMODID
       01  VSAM-SS-REC.                                                 DELMODID
         05  FILLER                  PIC X(4).                          DELMODID
         05  VSAM-SS-PRIM            PIC X(52).                         DELMODID
         05  FILLER                  PIC X(2444).                       DELMODID
       FD  VSAM-TC  IS EXTERNAL                                         DELMODID
           RECORDING MODE V  BLOCK 0 RECORDS  LABEL                     DELMODID
           RECORDS STANDARD RECORD IS VARYING                           DELMODID
           IN SIZE FROM 56 TO 2500                                      DELMODID
           DATA RECORDS VSAM-TC-REC.                                    DELMODID
       01  VSAM-TC-REC.                                                 DELMODID
         05  FILLER                  PIC X(4).                          DELMODID
         05  VSAM-TC-PRIM            PIC X(52).                         DELMODID
AS203    05  VSAM-TC-ALT-KEY         PIC X(30).
AS203    05  FILLER                  PIC X(2414).
       FD  VSAM-TH  IS EXTERNAL                                         DELMODID
           RECORDING MODE V  BLOCK 0 RECORDS  LABEL                     DELMODID
           RECORDS STANDARD RECORD IS VARYING                           DELMODID
           IN SIZE FROM 35 TO 2500                                      DELLMNCH
           DATA RECORDS VSAM-TH-REC.                                    DELMODID
       01  VSAM-TH-REC.                                                 DELMODID
         05  FILLER                  PIC X(6).                          DELMODID
         05  VSAM-TH-PRIM            PIC X(29).                         DELLMNCH
         05  FILLER                  PIC X(2465).                       DELLMNCH
       FD  VSAM-TK  IS EXTERNAL                                         DELMODID
           RECORDING MODE F  BLOCK 0 RECORDS  LABEL                     DELMODID
           RECORDS STANDARD  DATA RECORDS VSAM-TK-REC.                  DELMODID
       01  VSAM-TK-REC.                                                 DELMODID
         05  VSAM-TK-PRIM            PIC X(12).                         DELMODID
         05  FILLER                  PIC X(378).                        DELLAIXC
         05  VSAM-ALT-KEY5           PIC X(10).                         DELLAIXC
         05  FILLER                  PIC X(476).                        DELLAIXC
       FD  VSAM-AP  IS EXTERNAL                                         DELLIDCH
           RECORDING MODE F  BLOCK 0 RECORDS  LABEL                     DELLIDCH
           RECORDS STANDARD  DATA RECORDS VSAM-AP-REC.                  DELLIDCH
       01  VSAM-AP-REC.                                                 DELLIDCH
         05  VSAM-AP-PRIM            PIC X(20).                         DELLIDCH
         05  FILLER                  PIC X(567).                        DELLAIXC
         05  VSAM-ALT-N3KEY          PIC X(09).                         DELLAIXC
         05  FILLER                  PIC X(1056).                       DELLAIXC
         05  VSAM-AP-N5              PIC X(20).                         DELLAIXC
         05  FILLER                  PIC X(828).                        DELLMNCH
       FD  VSAM-GU  IS EXTERNAL                                         DELLIDCH
           RECORDING MODE F  BLOCK 0 RECORDS  LABEL                     DELLIDCH
           RECORDS STANDARD  DATA RECORDS VSAM-GU-REC.                  DELLIDCH
       01  VSAM-GU-REC.                                                 DELLIDCH
         05  VSAM-GU-PRIM            PIC X(36).                         DELLIDCH
         05  VSAM-ALT-GU-KEY1        PIC X(36).                         DELLMNCH
         05  VSAM-ALT-KEY2           PIC X(36).                         DELLMNCH
         05  FILLER                  PIC X(24).                         DELLMNCH
         05  VSAM-ALT-KEY3           PIC X(36).                         DELLMNCH
         05  FILLER                  PIC X(132).                        DELLIDCH
       FD  VSAM-MS  IS EXTERNAL                                         DELLMNCH
           RECORDING MODE F  BLOCK 0 RECORDS  LABEL                     DELLMNCH
           RECORDS STANDARD  DATA RECORDS VSAM-MS-REC.                  DELLMNCH
       01 VSAM-MS-REC.                                                  DELLMNCH
        03 VSAM-MS-PRIM.                                                DELLMNCH
         05 FILLER                   PIC X(77).                         DELLMNCH
         05 VSAM-MS-ALT-KEY          PIC X(15).                         DELLMNCH
        03 FILLER                   PIC X(208).                         DELLMNCH
       01 VSAM-MS-REC1.                                                 DELLMNCH
         05 FILLER                   PIC X(16).                         DELLMNCH
         05 VSAM-ALT-KEY1            PIC X(76).                         DELLMNCH
        03 FILLER                   PIC X(208).                         DELLMNCH
       FD  VSAM-NB  IS EXTERNAL                                         DELLIDCH
           RECORDING MODE V  BLOCK 0 RECORDS  LABEL                     DELLIDCH
           RECORDS STANDARD RECORD IS VARYING                           DELLIDCH
           IN SIZE FROM 18 TO 8500                                      DELLIDCH
           DEPENDING ON LMR-VARL-COMP                                   DELLMNCH
           DATA RECORDS VSAM-NB-REC.                                    DELLIDCH
       01  VSAM-NB-REC.                                                 DELLIDCH
         05  FILLER                  PIC X(6).                          DELLIDCH
         05  VSAM-NB-PRIM            PIC X(12).                         DELLIDCH
         05  FILLER                  PIC X(8482).                       DELLIDCH
       FD  VSAM-PO  IS EXTERNAL                                         DELLIDCH
           RECORDING MODE V  BLOCK 0 RECORDS  LABEL                     DELLIDCH
           RECORDS STANDARD RECORD IS VARYING                           DELLIDCH
           IN SIZE FROM 252 TO 8500                                     DELLMNCH
           DATA RECORDS VSAM-PO-REC.                                    DELLIDCH
       01  VSAM-PO-REC.                                                 DELLIDCH
         05  FILLER                  PIC X(6).                          DELLIDCH
         05  VSAM-PO-PRIM            PIC X(12).                         DELLIDCH
         05  FILLER                  PIC X(214).                        DELLMNCH
         05  VSAM-ALT-KEY4           PIC X(20).                         DELLMNCH
         05  FILLER                  PIC X(8248).                       DELLMNCH
       FD  VSAM-P3 IS EXTERNAL                                          DELLMNCH
           RECORDING MODE F  BLOCK 0 RECORDS  LABEL                     DELLMNCH
           RECORDS STANDARD  DATA RECORDS VSAM-P3-REC.                  DELLMNCH
       01  VSAM-P3-REC.                                                 DELLMNCH
           10 FILLER         PIC X(48).                                 DELLMNCH
           10 VSAM-P3-POLID  PIC X(12).                                 DELLMNCH
       FD  VSAM-RE  IS EXTERNAL                                         DELLIDCH
           RECORDING MODE F  BLOCK 0 RECORDS  LABEL                     DELLIDCH
           RECORDS STANDARD DATA RECORDS VSAM-RE-REC.                   DELLIDCH
       01  VSAM-RE-REC.                                                 DELLIDCH
         05  VSAM-RE-PRIM            PIC X(18).                         DELLIDCH
         05  FILLER                  PIC X(382).                        DELLIDCH
       FD  VSAM-WA  IS EXTERNAL                                         DELLIDCH
           RECORDING MODE F  BLOCK 0 RECORDS  LABEL                     DELLIDCH
           RECORDS STANDARD  DATA RECORDS VSAM-WA-REC.                  DELLIDCH
       01  VSAM-WA-REC.                                                 DELLIDCH
         05  VSAM-WA-PRIM            PIC X(54).                         DELLIDCH
         05  FILLER                  PIC X(1694).                       DELLIDCH
       01  VSAM-WA-REC2.                                                DELLIDCH
         05  FILLER                  PIC X(04).                         DELLRET2
         05  VSAM-ALT-WA-KEY1        PIC X(36).                         DELLRET2
         05  FILLER                  PIC X(1708).                       DELLIDCH
       FD  VSAM-W8  IS EXTERNAL                                         DELLIDCH
           RECORDING MODE F  BLOCK 0 RECORDS  LABEL                     DELLIDCH
           RECORDS STANDARD  DATA RECORDS VSAM-W8-REC.                  DELLIDCH
       01  VSAM-W8-REC.                                                 DELLIDCH
         05  VSAM-W8-PRIM            PIC X(74).                         DELLIDCH
         05  FILLER                  PIC X(1694).                       DELLIDCH
       FD  VSAM-WF  IS EXTERNAL                                         DELLIDCH
           RECORDING MODE F  BLOCK 0 RECORDS  LABEL                     DELLIDCH
           RECORDS STANDARD  DATA RECORDS VSAM-WF-REC.                  DELLIDCH
       01  VSAM-WF-REC.                                                 DELLIDCH
         05  VSAM-WF-PRIM            PIC X(16).                         DELLIDCH
         05  FILLER                  PIC X(2284).                       DELLIDCH
NK771  FD  VSAM-FS  IS EXTERNAL                              
NK771      RECORDING MODE F  BLOCK 0 RECORDS  LABEL          
NK771      RECORDS STANDARD  DATA RECORDS VSAM-FS-REC.       
NK771  01  VSAM-FS-REC.                                      
NK771    05  VSAM-FS-PRIM            PIC X(16).              
NK771    05  FILLER                  PIC X(64).   
AS991  FD  VSAM-PC  IS EXTERNAL                              
AS991      RECORDING MODE F  BLOCK 0 RECORDS  LABEL          
AS991      RECORDS STANDARD  DATA RECORDS VSAM-PC-REC.       
AS991  01  VSAM-PC-REC.                                      
AS991    05  VSAM-PC-PRIM            PIC X(13).              
AS991    05  FILLER                  PIC X(75).   
       FD  READ1 IS EXTERNAL                                            DELLSQCH
           RECORDING MODE F  BLOCK 0 RECORDS  LABEL                     DELLSQCH
           RECORDS STANDARD  DATA RECORDS READ1-REC.                    DELLSQCH
       01  READ1-REC     PIC X(80).                                     DELLSQCH
       FD  READ2 IS EXTERNAL                                            DELLRT16
           RECORDING MODE F  BLOCK 0 RECORDS  LABEL                     DELLRT16
           RECORDS STANDARD  DATA RECORDS READ2-REC.                    DELLRT16
       01  READ2-REC     PIC X(80).                                     DELLRT16
       FD  WRITE1 IS EXTERNAL                                           DELLMNCH
           RECORDING MODE V                                             DELLMNCH
           RECORD  IS VARYING IN SIZE FROM 1 TO 2500.                   DELLMNCH
       01  WRITE1-REC     PIC X(2500).                                  DELLMNCH
       FD  WRITE2 IS EXTERNAL                                           DELLMNCH
           RECORDING MODE V                                             DELLMNCH
           RECORD  IS VARYING IN SIZE FROM 1 TO 2500.                   DELLMNCH
       01  WRITE2-REC     PIC X(2500).                                  DELLMNCH
       FD  WRITE3 IS EXTERNAL                                           DELLSQCH
           RECORDING MODE F  BLOCK 0 RECORDS  LABEL                     DELLSQCH
           RECORDS STANDARD  DATA RECORDS WRITE3-REC.                   DELLSQCH
       01  WRITE3-REC     PIC X(2554).                                  DELLSQCH
       FD  WRITE4 IS EXTERNAL                                           DELLSQCH
           RECORDING MODE F  BLOCK 0 RECORDS  LABEL                     DELLSQCH
           RECORDS STANDARD  DATA RECORDS WRITE4-REC.                   DELLSQCH
       01  WRITE4-REC     PIC X(876).                                   DELLSQCH
       FD  WRITE5 IS EXTERNAL                                           DELLRET6
           RECORDING MODE F  BLOCK 0 RECORDS  LABEL                     DELLRET6
           RECORDS STANDARD  DATA RECORDS WRITE5-REC.                   DELLRET6
       01  WRITE5-REC     PIC X(1000).                                  DELLRET6
       FD  WRITE6 IS EXTERNAL                                           DELLRT12
           RECORDING MODE F  BLOCK 0 RECORDS  LABEL                     DELLRT12
           RECORDS STANDARD  DATA RECORDS WRITE6-REC.                   DELLMNCH
       01  WRITE6-REC     PIC X(250).                                   DELLRT12
       FD  WRITE7 IS EXTERNAL                                           DELLRT16
           RECORDING MODE F  BLOCK 0 RECORDS  LABEL                     DELLRT16
           RECORDS STANDARD  DATA RECORDS WRITE7-REC.                   DELLRT16
       01  WRITE7-REC     PIC X(560).                                   DELLRT16
       FD  WRITE8 IS EXTERNAL                                           DELLRT16
           RECORDING MODE F  BLOCK 0 RECORDS  LABEL                     DELLRT16
           RECORDS STANDARD  DATA RECORDS WRITE8-REC.                   DELLRT16
       01  WRITE8-REC     PIC X(4000).                                  DELLRT16
DL461  FD  WRITE9 IS EXTERNAL
DL461      RECORDING MODE F BLOCK 0 RECORDS   LABEL
DL461      RECORDS STANDARD  DATA RECORDS WRITE9-REC.
DL461  01  WRITE9-REC     PIC X(1000).                                   0000179
       FD  DATEFILE RECORDING MODE F  BLOCK 0 RECORDS  LABEL RECORDS     0000180
            STANDARD  DATA RECORDS DATE-REC.                             0000180
       01  DATE-REC                              PIC X(80).              0000181
       FD  NUSEQFL IS EXTERNAL                                          DELLMNCH
            RECORDING MODE F    BLOCK 0 RECORDS   LABEL                 DELLMNCH
            RECORDS STANDARD  DATA RECORDS  NUSEQFL-REC.                DELLMNCH
       01  NUSEQFL-REC.                                                 DELLMNCH
          05 NUSEQFL-PRIM             PIC X(50).                        DELLMNCH
          05 FILLER                   PIC X(8550).                      DELLMNCH
                                                                         0000182
       WORKING-STORAGE SECTION.                                          0000183
       01  MIGR-VSAM-FS-VAR.                                            DELLIDCH
         05  WS-INDX-FS-99               PIC 9(2).                      DELLIDCH
         05  VSAM-AC-FS                  PIC X(2).                      DELMODID
         05  VSAM-AG-FS                  PIC X(2).                      DELMODID
         05  VSAM-A3-FS                  PIC X(2).                      DELMODID
         05  VSAM-CS-FS                  PIC X(2).                      DELMODID
         05  VSAM-EM-FS                  PIC X(2).                      DELMODID
         05  VSAM-GS-FS                  PIC X(2).                      DELMODID
         05  VSAM-HD-FS                  PIC X(2).                      DELMODID
         05  VSAM-NT-FS                  PIC X(2).                      DELMODID
         05  VSAM-NU-FS                  PIC X(2).                      DELMODID
         05  VSAM-PL-FS                  PIC X(2).                      DELMODID
         05  VSAM-PM-FS                  PIC X(2).                      DELMODID
         05  VSAM-RT-FS                  PIC X(2).                      DELMODID
         05  VSAM-RV-FS                  PIC X(2).                      DELMODID
         05  VSAM-SR-FS                  PIC X(2).                      DELMODID
         05  VSAM-SS-FS                  PIC X(2).                      DELMODID
         05  VSAM-TC-FS                  PIC X(2).                      DELMODID
         05  VSAM-TH-FS                  PIC X(2).                      DELMODID
         05  VSAM-TK-FS                  PIC X(2).                      DELMODID
         05  VSAM-RE-FS                  PIC X(2).                      DELLIDCH
         05  VSAM-WF-FS                  PIC X(2).                      DELLIDCH
         05  VSAM-WA-FS                  PIC X(2).                      DELLIDCH
         05  VSAM-W8-FS                  PIC X(2).                      DELLMACH
         05  VSAM-PO-FS                  PIC X(2).                      DELLIDCH
         05  VSAM-P3-FS                  PIC X(2).                      DELLIDCH
         05  VSAM-AP-FS                  PIC X(2).                      DELLIDCH
         05  VSAM-GU-FS                  PIC X(2).                      DELLIDCH
         05  VSAM-NB-FS                  PIC X(2).                      DELLIDCH
         05  VSAM-N2-FS                  PIC X(2).                      DELLIDCH
         05  VSAM-MS-FS                  PIC X(2).                      DELLIDCH
         05  VSAM-P2-FS                  PIC X(2).                      DELLIDCH
         05  VSAM-U1-FS                  PIC X(2).                      DELLIDCH
NK771    05  VSAM-FS-FS                  PIC X(2).                      DELLIDCH
AS991    05  VSAM-PC-FS                  PIC X(2).                      DELLIDCH
         05  WS-NUSEQ-FS                 PIC X(2).                      DELLMNCH
       01  WS-WA-SW  PIC X(01).                                         DELLIDCH
       COPY 'TPSWNML'.                                                  DELLIDCH
       01  WS-READ1-FS     PIC X(02)    VALUE '00'.                     DELLSQCH
       01  WS-READ2-FS     PIC X(02)    VALUE '00'.                     DELLRT16
       01  WS-WRITE1-FS     PIC X(02)    VALUE '00'.                    DELLMNCH
       01  WS-WRITE2-FS     PIC X(02)    VALUE '00'.                    DELLMNCH
       01  WS-WRITE3-FS     PIC X(02)    VALUE '00'.                    DELLSQCH
       01  WS-WRITE4-FS     PIC X(02)    VALUE '00'.                    DELLSQCH
       01  WS-WRITE5-FS     PIC X(02)    VALUE '00'.                    DELLRET6
       01  WS-WRITE6-FS     PIC X(02)    VALUE '00'.                    DELLRT12
       01  WS-WRITE7-FS     PIC X(02)    VALUE '00'.                    DELLRT16
       01  WS-WRITE8-FS     PIC X(02)    VALUE '00'.                    DELLRT16
DL461  01  WS-WRITE9-FS     PIC X(02)    VALUE '00'.
       01  CPY-FS-CODE      PIC X(02)    VALUE '00'.                    DELLSQCH
       01  LMR-VARL.                                                    DELLMNCH
           05  LMR-VARL-COMP  PIC S9(4) COMP.                           DELLMNCH
       01  RC.                                                          DELLMNCH
            05  CONDITION-TOKEN-VALUE PIC 99.                           DELLMNCH
       01  WS-COMP-AREA                          COMP.                   0000184
PS134      05  ABEND-CODE                        PIC S9(9)  VALUE +0.    0000185
DL457      05  WS-REC-WRITE3                     PIC S9(9)  VALUE +0.   DELLRT17
DL457      05  WS-REC-WRITE4                     PIC S9(9)  VALUE +0.   DELLRT17
DL457      05  WS-REC-WRITE6                     PIC S9(9)  VALUE +0.   DELLRT17
DL457      05  WS-REC-WRITE7                     PIC S9(9)  VALUE +0.   DELLRT17
DL457      05  WS-REC-WRITE8                     PIC S9(9)  VALUE +0.   DELLRT17
DL461      05  WS-REC-WRITE9                     PIC S9(9)  VALUE +0.
DL457      05  WS-REC-READ                       PIC S9(9)  VALUE +0.   DELLRT17
DL457      05  WK-NB452-RECS-IN                  PIC S9(9)  VALUE +0.   DELLRT17
DL457      05  WK-NB452-PHS                      PIC S9(9)  VALUE +0.   DELLRT17
DL457      05  WK-NB452-NBS                      PIC S9(9)  VALUE +0.   DELLRT17
           05  WTR-TABLE-MAX                     PIC S9(4)  VALUE +50.   0000186
           05  HPSI-INFO-MAX                     PIC S9(4)  VALUE +20.   0000187
           05  AP-REC-MAX                        PIC S9(4)  VALUE +2.    0000188
           05  CV-REC-MAX                        PIC S9(4)  VALUE +4.    0000189
AK131      05  PM-REC-MAX                        PIC S9(4)  VALUE +3.    0000190
           05  TPMSG-MAX                         PIC S9(4)  VALUE +10.   0000191
BT132      05  SY-TABLE-MAX                      PIC S9(4)  VALUE +10.   0000192
           05  POLICY-MAX                        PIC S9(4)  VALUE +120.  0000193
           05  WRKDYS-MAX                        PIC S9(4)  VALUE +90.   0000194
           05  RTN-CODE                          PIC S9(4)  VALUE +0.    0000195
CL131      05  RTN-CODE-WF                       PIC S9(4)  VALUE +0.    0000196
CL131      05  AI-RTN-CODE                       PIC S9(4)  VALUE +0.    0000197
AS201      05  TC-RTNCODE                        PIC S9(4)  VALUE +0.
BT151      05  M1-RTN-CODE                       PIC S9(4)  VALUE +0.    0000198
BT183      05  WK-BRIDGE-LIMIT                   PIC S9(8)  VALUE        0000199
BT183       +49999.                                                      0000199
VC291      05  WK-BRIDGE-LIMIT-SS                PIC S9(8)  VALUE       DELLRET2
VC291       +40000.                                                     DELLRET2
BT361      05  WK-BRIDGE-LIMIT-02                PIC S9(8)  VALUE       DELLRET8
JJ731       +600000.
BT361 *     +100000.                                                    DELLRET8
CL131      05  ERR-INDX                          PIC S9(9).              0000200
CL131      05  WK-WAIT-TENTHS-SEC                PIC S9(9).              0000201
           05  REL-RTN-CODE                      PIC S9(4)  VALUE +0.    0000202
           05  CV-SUB                            PIC S9(4)  VALUE +0.    0000203
           05  WS-SUB                            PIC S9(4)  VALUE +0.    0000204
           05  WS-SUB1                           PIC S9(4)  VALUE +0.    0000205
           05  WS-SUB2                           PIC S9(4)  VALUE +0.    0000206
PS134      05  PARM-SUB                          PIC S9(4)  VALUE +0.    0000207
           05  HPSI-SUB                          PIC S9(4)  VALUE +0.    0000208
           05  MSG-SUB                           PIC S9(4)  VALUE +0.    0000209
           05  AP-REC-STATUS                     PIC S9(4)  VALUE +0.    0000210
MK131      05  RTNCD-CTL                         PIC S9(4)  VALUE +0.    0000211
MK131      05  READ-CNT                          PIC S9(4)  VALUE +0.    0000212
PB631      05  RE-RTN-CODE                       PIC S9(4)  VALUE +0.
                                                                         0000213
       01  WS-COMP3-AREA                         COMP-3.                 0000214
           05  WS-REC-COUNT                      PIC S9(3)  VALUE +0.    0000215
           05  WS-SY-COUNT                       PIC S9(3)  VALUE +0.    0000216
           05  WS-AP-COUNT                       PIC S9(3)  VALUE +0.    0000217
           05  WS-CV-COUNT                       PIC S9(3)  VALUE +0.    0000218
AK131      05  WS-PM-COUNT                       PIC S9(3)  VALUE +0.    0000219
           05  APP-COUNT                         PIC S9(5)  VALUE +0.    0000220
PS135      05  APP-COUNTER                       PIC 9(9)  VALUE 0.      0000221
                                                                         0000222
       01  WORK-FIELDS.                                                  0000223
           05  EOF-WA                            PIC X      VALUE        0000224
            SPACES.                                                      0000224
PS134      05  PARM-EOF                          PIC X      VALUE        0000225
PS134       SPACES.                                                      0000225
DL442      05  PROCESS-OPT-EOF                   PIC X      VALUE       DELLRT16
DL442       SPACES.                                                     DELLRT16
           05  MESSAGE-FIELD                     PIC X      VALUE        0000226
            SPACES.                                                      0000226
           05  NUM-ERR                           PIC X      VALUE        0000227
            SPACES.                                                      0000227
           05  ERROR-SWITCH                      PIC X      VALUE        0000228
            SPACES.                                                      0000228
AK131      05  WS-PM-FOUND                       PIC X      VALUE        0000229
AK131       SPACES.                                                      0000229
PS134      05  WS-BYPASS-FLAG                    PIC X      VALUE        0000230
PS134       SPACES.                                                      0000230
BT043      05  DUP-POL-SWITCH                    PIC X.                  0000231
NK081      05  NO-TASK-REC-SW                    PIC X.                  0000232
DL344      05  SV-SLA-TASK-STATUS                PIC X.                 DELLRET6
BT142      05  CHECK-NEW-BUSINESS-IND            PIC X      VALUE        0000233
BT142       SPACES.                                                      0000233
PS401      05  CALLING-PROGRAM                   PIC X(8).              DELLRT12
           05  NEG-NUM                           PIC S9(11)V9999         0000234
                VALUE -99999999998.9999.                                 0000235
           05  NUM-STATUS-AREA.                                          0000236
               10  NUM-AP                        PIC 9(3).               0000237
BT162          10  NUM-AC                        PIC 9(3).               0000238
BT231          10  NUM-OW                        PIC 9(3).               0000239
               10  NUM-CV                        PIC 9(3).               0000240
SS521          10  NUM-DI                        PIC 9(3).               0000240
               10  NUM-T1                        PIC 9(3).               0000241
               10  NUM-T2                        PIC 9(3).               0000242
               10  NUM-BN                        PIC 9(3).               0000243
CL131          10  NUM-AB                        PIC 9(3).               0000244
               10  NUM-SY                        PIC 9(3).               0000245
               10  NUM-NT                        PIC 9(3).               0000246
               10  NUM-NTE                       PIC 9(3).               0000247
               10  NUM-INV                       PIC 9(3).               0000248
AK131          10  NUM-PN                        PIC 9(3).               0000249
DD281          10  NUM-OR                        PIC 9(3).              DELLRET2
BT444          10  NUM-EI                        PIC 9(3).              DELLRT16
AR150          10  NUM-AL                        PIC 9(3).                      
AR150          10  NUM-GW                        PIC 9(3).                      
AR150          10  NUM-IB                        PIC 9(3).                      
           05  WK-SYSTEM-DATE.                                           0000250
               10  WK-SYSTEM-DATE-YYYY           PIC X(4).               0000251
               10  WK-SYSTEM-DATE-MM             PIC X(2).               0000252
               10  WK-SYSTEM-DATE-DD             PIC X(2).               0000253
           05  WK-SYSTEM-TIME.                                           0000254
               10  WK-SYSTEM-TIME-HH             PIC X(2).               0000255
               10  WK-SYSTEM-TIME-MM             PIC X(2).               0000256
               10  WK-SYSTEM-TIME-SS             PIC X(2).               0000257
               10  WK-SYSTEM-TIME-10TH           PIC X(1).               0000258
               10  WK-SYSTEM-TIME-100TH          PIC X(1).               0000259
           05  FILL-TYME-NUM REDEFINES WK-SYSTEM-TIME.                   0000260
               10  FTN-SYSTEM-TIME               PIC 9(8).               0000261
           05  WK-DATE                           PIC X(8).               0000262
           05  WORK-DAYS-ADVANCE                 PIC S9(9).              0000263
           05  WORK-DAYS-SAVE                    PIC S9(9).              0000264
           05  RESULT-FIELD                      PIC X(2).               0000265
           05  TS250-TBL-AREA                    PIC X(300).             0000266
DL344      05  SV-SLA-TK010-PS-REC               PIC X(2300).           DELLRET6
           05  FILE-NAME-WF.                                             0000267
               10  FILE-CO-WF                    PIC X(2).               0000268
               10  FILE-LIT-WF                   PIC X(2).               0000269
           05  AP-FILE-ID.                                               0000270
               10  AP-FILE-CO                    PIC X(2).               0000271
               10  AP-FILE-LIT                   PIC X(2).               0000272
           05  WA-AP-KEY.                                                0000273
               10  WA-AP-CO                      PIC X(2).               0000274
               10  WA-AP-POL.                                            0000275
                   15  WA-AP-POLICY              PIC X(8).               0000276
                   15  WA-AP-RIDER               PIC X(2).               0000277
               10  WA-AP-TYPE                    PIC X(2).               0000278
               10  FILLER                             PIC X(6).          0000279
           05  FILE-NAME.                                                0000280
               10  FILE-CO                       PIC X(2).               0000281
               10  FILE-LIT                      PIC X(2).               0000282
           05  CS-FILE-ID.                                               0000283
               10  CS-FILE-CO                    PIC X(2).               0000284
               10  CS-FILE-LIT                   PIC X(2).               0000285
           05  NB-FILE-ID.                                               0000286
               10  NB-FILE-CO                    PIC X(2).               0000287
               10  NB-FILE-LIT                   PIC X(2).               0000288
           05  PO-FILE-ID.                                               0000289
               10  PO-FILE-CO                    PIC X(2).               0000290
               10  PO-FILE-LIT                   PIC X(2).               0000291
           05  PL-FILE-ID.                                               0000292
               10  PL-FILE-CO                    PIC X(2).               0000293
               10  PL-FILE-LIT                   PIC X(2).               0000294
           05  RE-FILE-ID.                                               0000295
               10  RE-FILE-CO                    PIC X(2).               0000296
               10  RE-FILE-LIT                   PIC X(2).               0000297
           05  WA-FILE-IDZ.                                              0000298
               10  WA-FILE-CO                    PIC X(2).               0000299
               10  WA-FILE-LIT                   PIC X(2).               0000300
CL131      05  W9-FILE-ID.                                               0000301
CL131          10  W9-FILE-CO                    PIC X(2).               0000302
CL131          10  W9-FILE-LIT                   PIC X(2).               0000303
BT111      05  WK-SEARCH-WA-KEY.                                         0000304
BT111          10  SWA-TRANS-TYPE                PIC X(2).               0000305
BT111          10  SWA-CO                        PIC X(2).               0000306
BT111          10  SWA-DELL-GUID                 PIC X(36).              0000307
BT111          10  SWA-TOTAL-RECS                PIC 9(3).               0000308
BT111          10  SWA-DUP-SEQ                   PIC 9(3).               0000309
BT111          10  SWA-REC-TYPE.                                         0000310
BT111              15  SWA-FILE-ID               PIC X(2).               0000311
BT111              15  SWA-REC-ID                PIC X(2).               0000312
BT111              15  SWA-REC-APPID-X.                                  0000313
BT111                  20  SWA-REC-APID          PIC 9(2).               0000314
BT111              15  SWA-REC-SEQ-X.                                    0000315
BT111                  20  SWA-REC-SEQ           PIC 9(2).               0000316
BT111      05  WK-MATCH-BN-WA.                                           0000317
BT111          10  WK-BN-SEQ-WA                  PIC 9(4).               0000318
BT111          10  WK-BN-TYPE-WA                 PIC X(2).               0000319
SM241      05  WK-MATCH-TP-WA.                                          I       
SM241          10  WK-TP-SEQ-WA                  PIC 9(4).              I       
SM241          10  WK-TP-TYPE-WA                 PIC X(2).              I       
BT111                                                                    0000320
           05  WA-FILE-KEY.                                              0000321
               10  WA-WA-TRANS-TYPE              PIC X(2).               0000322
               10  WA-WA-CO                      PIC X(2).               0000323
               10  WA-WA-DELL-GUID               PIC X(36).              0000324
               10  WA-WA-TOTAL-RECS              PIC 9(3).               0000325
               10  WA-WA-DUP-SEQ.                                        0000326
                   15  WA-WA-DUP-SEQ9            PIC 9(3).               0000327
               10  WA-WA-REC-TYPE.                                       0000328
                   15  WA-WA-FILE-ID             PIC X(2).               0000329
                   15  WA-WA-REC-ID              PIC X(2).               0000330
                   15  WA-WA-REC-APPID           PIC 9(2).               0000331
                   15  WA-WA-REC-SEQ             PIC 9(2).               0000332
           05  GU-FILE-ID.                                               0000333
               10  GU-FILE-CO                    PIC X(2).               0000334
               10  GU-FILE-LIT                   PIC X(2).               0000335
           05  U1-FILE-ID.                                               0000336
               10  U1-FILE-CO                    PIC X(2).               0000337
               10  U1-FILE-LIT                   PIC X(2).               0000338
BT170      05  ALT-IDX-NAME.                                             0000339
BT170          10  ALT-IDX-CO                       PIC XX.              0000340
BT170          10  ALT-IDX-LIT                      PIC XX.              0000341
BT170      05  AI-N2-KEY.                                                0000342
BT170          10  AI-N2-OLD-POLICY.                                     0000343
BT170              15  AI-N2-OLDPOL-1-10            PIC X(10).           0000344
BT170              15  AI-N2-OLDPOL-11-20           PIC X(10).           0000345
BT170          10  AI-N2-POLID.                                          0000346
BT170              15  AI-N2-CO                     PIC X(2).            0000347
BT170              15  AI-N2-POLICY                 PIC X(8).            0000348
BT170              15  AI-N2-APPID                  PIC X(2).            0000349
BT170          10  AI-N2-REC                        PIC X(2).            0000350
BT170          10  FILLER                                PIC X(6).       0000351
           05  AI-GU-KEY.                                                0000352
CL131          10  AI-GUID-TYPE                  PIC X.                  0000353
CL131          10  AI-GU-VALUE                   PIC X(36).              0000354
CL131          10  AI-GUMST-PTR                  PIC X(36).              0000355
CL131      05  AI-W9-KEY.                                                0000356
               10  AI-W9-OS-SEQNUM               PIC S9(7) COMP-3.      DELLMACH
CL131          10  AI-W9-OS-DATE                 PIC X(8).               0000357
CL131          10  AI-W9-OS-TIME                 PIC X(8).               0000358
CL131          10  AI-W9-DELL-GUID               PIC X(54).             DELLMACH
           05 AI-WA-KEY.                                                DELLMNCH
               10 AI-WA-DELL-GUID                PIC X(54).             DELLMNCH
CL131      05  PRIMARY-KEY                       PIC X(54).              0000360
BT142      05  NBS340-RTN-CODE                   PIC 9(3).               0000361
BT142      05  NBS340-RTN-CODEX REDEFINES NBS340-RTN-CODE                0000362
BT142                                            PIC X(3).               0000363
BT142                                                                    0000364
BT151      05  M1-FILE-ID.                                               0000365
BT151          10  M1-FILE-CO                    PIC X(2).               0000366
BT151          10  M1-FILE-LIT                   PIC X(2).               0000367
BT151      05  ALTA-INDEX-REC.                                           0000368
BT151          10  AI-M1-KEY.                                            0000369
BT151              15  AI-M1-TYPE                PIC X.                  0000370
BT151              15  M1-TIER-BASE-IND          PIC X.                  0000371
BT151              15  M1-RATE-DESC              PIC X(60).              0000372
BT151              15  M1-RPT-NUM-X.                                     0000373
BT151                  20  M1-RPT-NUM            PIC 9(07).              0000374
BT151              15  M1-SUBCODE-X.                                     0000375
BT151                  20  M1-SUBCODE            PIC 9(04).              0000376
BT151              15  M1-CLAIM-BRANCH           PIC X(04).              0000377
BT151              15  AI-M1-PRIMARY-KEY         PIC X(92).              0000378
                                                                        DELLRET2
           05 AI-M1-FILE-KEY.                                           DELLMNCH
                 10 AI-M1-FILE-TIER-BASE-IND       PIC X.                   |   
                 10 AI-M1-FILE-RATE-DESC           PIC X(60).               |   
                 10 AI-M1-FILE-RPT-NUM             PIC 9(07).               |   
                 10 AI-M1-FILE-SUBCODE             PIC 9(04).               |   
                 10 AI-M1-FILE-CLAIM-BRANCH        PIC X(04).           DELLMNCH
BT151      05  AI-KEY.                                                   0000380
BT151          10  AI-TYPE                            PIC X.             0000381
BT151          10  AI-NAME.                                              0000382
BT151              15 A1-TO-X1-NAME-1                 PIC X(9).          0000383
BT151              15 FILLER                          PIC X(21).         0000384
BT151          10  AI-NAME-FLAGS.                                        0000385
BT151              15  AI-IN-NAME-FLAG                PIC 9.             0000386
BT151              15  AI-OW-NAME-FLAG                PIC 9.             0000387
BT151              15  AI-PY-NAME-FLAG                PIC 9.             0000388
BT151              15  AI-EM-NAME-FLAG                PIC 9.             0000389
BT151              15  AI-PB-NAME-FLAG                PIC 9.             0000390
BT151              15  AI-CB-NAME-FLAG                PIC 9.             0000391
BT151              15  AI-AC-NAME-FLAG                PIC 9.             0000392
BT151              15  AI-AS-NAME-FLAG                PIC 9.             0000393
BT151              15  FILLER                         PIC XX.            0000394
BT151          10  AI-SSN-FLAGS.                                         0000395
BT151              15  AI-AC-SSN-FLAG                 PIC 9.             0000396
BT151              15  AI-EM-SSN-FLAG                 PIC 9.             0000397
BT151              15  AI-AS-SSN-FLAG                 PIC 9.             0000398
BT151              15  AI-PB-SSN-FLAG                 PIC 9.             0000399
BT151              15  AI-CB-SSN-FLAG                 PIC 9.             0000400
BT151              15  AI-OW-SSN-FLAG                 PIC 9.             0000401
BT151              15  AI-IN-SSN-FLAG                 PIC 9.             0000402
BT151          10  AI-POLMST-PTR.                                        0000403
BT151              15  AI-POLMST-PTR-CO               PIC X(2).          0000404
BT151              15  AI-POLMST-PTR-POL              PIC X(8).          0000405
BT151              15  AI-POLMST-PTR-RDR              PIC X(2).          0000406
BT151      05  P2-FILEID.                                                0000407
BT151          10  P2-FILE-CO                         PIC XX.            0000408
BT151          10  P2-FILE-LIT                        PIC XX VALUE 'P2'. 0000409
BT151                                                                    0000410
AS201      05  TC-FILE-ID.
AS201          10  TC-FILE-CO          PIC XX.
AS201          10  TC-FILE-LIT         PIC XX      VALUE 'TC'.
AS201
PS133      05  HOLD-AI-W9-KEY.                                           0000411
PS133          10  HOLD-AI-W9-SEQNUM             PIC S9(7) COMP-3.
PS133          10  HOLD-AI-W9-OS-DATE            PIC X(8).               0000412
PS133          10  HOLD-AI-W9-OS-TIME            PIC X(8).               0000413
PS133          10  HOLD-AI-W9-DELL-GUID          PIC X(54).              0000414
           05  HOLD-VSAM-WA-PRIM                 PIC X(54) VALUE SPACES.DELLMNCH
CL131      05  WS-KEY.                                                   0000415
CL131          10  WS-TRANS-TYPE                 PIC X(2).               0000416
CL131          10  WS-CO                         PIC X(2).               0000417
CL131          10  WS-DELL-GUID                  PIC X(36).              0000418
CL131          10  WS-TOTAL-RECS                 PIC 9(3).               0000419
CL131          10  WS-DUP-SEQ                    PIC 9(3).               0000420
CL131          10  WS-REC-TYPE.                                          0000421
CL131              15  WS-FILE-ID                PIC X(2).               0000422
CL131              15  WS-REC-ID                 PIC X(2).               0000423
CL131              15  WS-REC-APPID-X.                                   0000424
CL131                  20  WS-REC-APPID          PIC 9(2).               0000425
CL131              15  WS-REC-SEQ-X.                                     0000426
CL131                  20  WS-REC-SEQ            PIC 9(2).               0000427
           05  HOLD-STATUSX.                                             0000428
               10  HOLD-STATUS9                  PIC 9(3).               0000429
           05  HOLD-STATUS-WA REDEFINES HOLD-STATUSX    PIC X(3).        0000430
           05  WK-REL.                                                   0000431
               10  WK-REL9                       PIC 9(2).               0000432
           05  WK-STATUS                         PIC X(3).               0000433
CL131      05  WK-ERROR-CODE                     PIC X(3).               0000434
KM101      05  SAVE-PAIND-WASY                   PIC X.                  0000435
BT231      05  SAVE-BLOCK-ID-WASY                PIC X(4).               0000436
KM101      05  SAVE-PAPOL-WASY                   PIC X(8).               0000437
KM101      05  SAVE-COMT1-WASY                   PIC X(68).              0000438
KM101      05  SAVE-COMT2-WASY                   PIC X(68).              0000439
KM101      05  SAVE-COMT3-WASY                   PIC X(68).              0000440
CL131      05  SAVE-LS-OVERRIDE-IND              PIC X(2).               0000441
DL461      05  SAVE-LS-PARAM-1                   PIC X(10). 
BT151      05  SAVE-LS-PARAM-3                   PIC X(10).              0000442
BT151      05  SAVE-LS-PARAM-4                   PIC X(10).              0000443
BT151      05  SAVE-LS-PARAM-5                   PIC X(10).              0000444
BT392      05  SAVE-LS-PARAM-8.                                         DELLRT10
BT392          10 SAVE-LS-PARAM-8-1              PIC X.                 DELLRT10
BT392          10 SAVE-LS-PARAM-8-9              PIC X(9).              DELLRT10
BT701      05  SAVE-LS-PARAM-8R REDEFINES SAVE-LS-PARAM-8.
BT701          10  SAVE-LS-PARAM-8-2R            PIC X(2).
BT701          10  SAVE-LS-PARAM-8-8R            PIC X(8). 
AS991      05  SAVE-LS-PARAM-34                  PIC X(10).             
BT151      05  SAVE-LS-PARAM-37                  PIC X(1).               0000445
DL90A      05  SAVE-UW-DCSN-NA                   PIC 9(3).
DL90A      05  SAVE-UW-SUB-DCSN-NA               PIC 9(3).
AS201      05  SAVE-REPL-INDICATOR-NA            PIC X.
AS203      05  SAVE-REPL-POL-NA                  PIC X(8).
AS203      05  SAVE-COMPANY-NAME                 PIC X(30).
BT151      05  HOLD-RATE-DESC                    PIC X(60).              0000446
BT151      05  HOLD-TIER-BASE-IND                PIC X.                  0000447
BT151      05  WS-POL-REC-FOUND                  PIC X(1)  VALUE 'N'.    0000448
BT151      05  HOLD-APAP1-RECORD                 PIC X(1748).            0000449
BT151      05  HOLD-APAP2-RECORD                 PIC X(1748).            0000450
BT184      05  HOLD-APCV1-RECORD                 PIC X(1748).            0000451
BT151      05  WS-REMARK3.                                               0000452
BT151          10  WS-REMARK3-MSG                PIC X(18).              0000453
BT151          10  WS-REMARK3-PARAM-37           PIC X.                  0000454
BT301      05  WS-DUP-REMARK3.                                          DELLRET2
BT301          10  WS-DUP-REMARK3-MSG            PIC X(18).             DELLRET2
BT301          10  WS-DUP-REMARK3-PARAM-37       PIC X.                 DELLRET2
BT301          10  WS-DUP-REMARK3-SPACE          PIC X.                 DELLRET2
BT301          10  WS-DUP-REMARK3-GROUP          PIC X(07).             DELLRET2
BT301          10  WS-DUP-REMARK3-SUBCODE        PIC X(04).             DELLRET2
BT301          10  WS-DUP-REMARK3-BRANCH         PIC X(04).             DELLRET2
BT302      05  WK-REMARK2.                                              DELLRET2
BT302          10  WK-REMARK-A                   PIC X(30).             DELLRET2
BT302          10  WK-REMARK-B                   PIC X(30).             DELLRET2
BT302      05  WS-SET-REMARK3.                                          DELLRET2
BT302          10  WS-SET-REMARK3-MSG            PIC X(18).             DELLRET2
BT302          10  WS-SET-REMARK3-PARAM-37       PIC X.                 DELLRET2
BT302          10  WS-SET-REMARK3-SPACE          PIC X.                 DELLRET2
BT302          10  WS-SET-REMARK3-GROUP          PIC X(07).             DELLRET2
BT302          10  WS-SET-REMARK3-SUBCODE        PIC X(04).             DELLRET2
BT302          10  WS-SET-REMARK3-BRANCH         PIC X(04).             DELLRET2
BT302      05  WK-STRUCT.                                               DELLRET2
BT302          10  WK-RPT-NUM-X                  PIC X(07).             DELLRET2
BT302          10  WK-SUBCODE-X                  PIC X(04).             DELLRET2
BT302          10  WK-CLAIM-BRANCH               PIC X(04).             DELLRET2
BT302      05  WK-PARM38                         PIC X.                 DELLRET2
BT132      05  WK-FINAL-STATUS                   PIC X(8).               0000455
BT132      05  LW-ERROR-CODE                     PIC X(3).               0000456
BT181      05  SAVE-EMPLOYER-ID                  PIC X(5).               0000457
CL131      05  BEFORE-REC-N340                   PIC X(2500).            0000458
CL131      05  WORK-AREA-N340                    PIC X(2500).            0000459
CL131      05  WORK-AREA1                        PIC X(2500).            0000460
           05  WK-NB-UPDATE-SW                   PIC X.                  0000461
           05  SV-TRANS-TYPE                     PIC X(2).               0000462
           05  SV-CO                             PIC X(2).               0000463
           05  SV-IMAGE-REFID                    PIC X(36).              0000464
           05  SV-DELL-GUID                      PIC X(36).              0000465
           05  SV-AP-FROM-PC                     PIC X.                  0000466
           05  SV-APPID-WA                       PIC 9(2).               0000467
           05  SV-LAST-WA-KEY                    PIC X(54).              0000468
           05  SAVE-REC-SEQX.                                            0000469
               10  SAVE-REC-SEQ                  PIC 9(2).               0000470
           05  SAVE-STATUSX.                                             0000471
               10  SAVE-STATUS9                  PIC 9(3).               0000472
           05  SAVE-STATUS-WA REDEFINES SAVE-STATUSX    PIC X(3).        0000473
           05  PASS-ACTION                       PIC X.                  0000474
BT193      05  SAVE-TASK-INDICATOR               PIC X.                  0000475
PS401      05  INFILE-STATUS-RECORD              PIC X(250).            DELLRT12
DL457      05  TOT-REC-READ                      PIC 9(09) VALUE 0.     DELLRT17
DL457      05  TOT-REC-WRITE                     PIC 9(09) VALUE 0.     DELLRT17
DL457      05  TOT-REC-DISPLAY                   PIC 9(09) VALUE 0.     DELLRT17
DL441      05  OB-HEADER-FIRST-TIME              PIC X VALUE 'Y'.       DELLRT16
DL441  01  WB-OVERRIDE-BENEFIT-HEADING.                                 DELLRT16
DL441      05  WB-REC-TYP               PIC X(11) VALUE 'RECORD TYPE'.  DELLRT16
DL441      05  WB-DELIMITER-1           PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-UIS-CUST-NUM          PIC X(20) VALUE                 DELLRT16
DL441                                   'UIS CUSTOMER NUMBER'.          DELLRT16
DL441      05  WB-DELIMITER-2           PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-EMP-KEY               PIC X(12) VALUE 'EMPLOYEE KEY'. DELLRT16
DL441      05  WB-DELIMITER-3           PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-POLNUM                PIC X(13) VALUE                 DELLRT16
DL441                                   'POLICY NUMBER'.                DELLRT16
DL441      05  WB-DELIMITER-4           PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-STATUS                PIC X(6)  VALUE 'STATUS'.       DELLRT16
DL441      05  WB-DELIMITER-5           PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-GROUP                 PIC X(5)  VALUE 'GROUP'.        DELLRT16
DL441      05  WB-DELIMITER-6           PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-GROUP-NAME            PIC X(30) VALUE                 DELLRT16
DL441                                   'GROUP NAME'.                   DELLRT16
DL441      05  WB-DELIMITER-7           PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-PLAN                  PIC X(8)  VALUE 'PLAN'.         DELLRT16
DL441      05  WB-DELIMITER-8           PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-CLASS                 PIC X(5)  VALUE 'CLASS'.        DELLRT16
DL441      05  WB-DELIMITER-9           PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-REPORT                PIC X(7)  VALUE 'REPORT'.       DELLRT16
DL441      05  WB-DELIMITER-10          PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-SUBCODE               PIC X(7)  VALUE 'SUBCODE'.      DELLRT16
DL441      05  WB-DELIMITER-11          PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-BRANCH                PIC X(6)  VALUE 'BRANCH'.       DELLRT16
DL441      05  WB-DELIMITER-12          PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-LAST-NAME             PIC X(30) VALUE                 DELLRT16
DL441                                   'LAST NAME'.                    DELLRT16
DL441      05  WB-DELIMITER-13          PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-FIRST-NAME            PIC X(30) VALUE                 DELLRT16
DL441                                   'FIRST NAME'.                   DELLRT16
DL441      05  WB-DELIMITER-14          PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-UIS-COV-IND           PIC X(11) VALUE 'UIS COV IND'.  DELLRT16
DL441      05  WB-DELIMITER-15          PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-COV-EFF-DATE          PIC X(12) VALUE 'COV EFF DATE'. DELLRT16
DL441      05  WB-DELIMITER-16          PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-COV-STOP-DATE         PIC X(13) VALUE                 DELLRT16
DL441                                   'COV STOP DATE'.                DELLRT16
DL441      05  WB-DELIMITER-17          PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-BEN-AMT-UIS           PIC X(11) VALUE 'BEN AMT UIS'.  DELLRT16
DL441      05  WB-DELIMITER-18          PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-PREM-AB-TYPE          PIC X(12) VALUE 'PREM AB TYPE'. DELLRT16
DL441      05  WB-DELIMITER-19          PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-SP-PREM-CALC-IND      PIC X(16) VALUE                 DELLRT16
DL441                                   'SP PREM CALC IND'.             DELLRT16
DL441      05  WB-DELIMITER-20          PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-CH-PREM-CALC-IND      PIC X(16) VALUE                 DELLRT16
DL441                                   'CH PREM CALC IND'.             DELLRT16
DL441      05  WB-DELIMITER-21          PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-PREM-BEN-OVR          PIC X(12) VALUE 'PREM BEN OVR'. DELLRT16
DL441      05  WB-DELIMITER-22          PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-PREM-BEN-UPDT         PIC X(13) VALUE                 DELLRT16
DL441                                   'PREM BEN UPDT'.                DELLRT16
DL441      05  WB-DELIMITER-23          PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-BEN-AB-TYPE           PIC X(11) VALUE 'BEN AB TYPE'.  DELLRT16
DL441      05  WB-DELIMITER-24          PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-BEN-AMT-OVR           PIC X(11) VALUE 'BEN AMT OVR'.  DELLRT16
DL441      05  WB-DELIMITER-25          PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-BEN-UPDT              PIC X(8)  VALUE 'BEN UPDT'.     DELLRT16
DL441      05  WB-DELIMITER-26          PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-EE-BEN                PIC X(7)  VALUE 'EE BEN'.       DELLRT16
DL441      05  WB-DELIMITER-27          PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-SP-PREM-PCT           PIC X(9)  VALUE 'SP PREM %'.    DELLRT16
DL441      05  WB-DELIMITER-28          PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-SP-PREM-AMT           PIC X(11) VALUE 'SP PREM AMT'.  DELLRT16
DL441      05  WB-DELIMITER-29          PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-SP-BEN-PCT            PIC X(8)  VALUE 'SP BEN %'.     DELLRT16
DL441      05  WB-DELIMITER-30          PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-SP-BEN-AMT            PIC X(10) VALUE 'SP BEN AMT'.   DELLRT16
DL441      05  WB-DELIMITER-31          PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-CH-PREM-PCT           PIC X(9)  VALUE 'CH PREM %'.    DELLRT16
DL441      05  WB-DELIMITER-32          PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-CH-PREM-AMT           PIC X(11) VALUE 'CH PREM AMT'.  DELLRT16
DL441      05  WB-DELIMITER-33          PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-CH-BEN-PCT            PIC X(8)  VALUE 'CH BEN %'.     DELLRT16
DL441      05  WB-DELIMITER-34          PIC X(1)  VALUE ';'.            DELLRT16
DL441      05  WB-CH-BEN-AMT            PIC X(10) VALUE 'CH BEN AMT'.   DELLRT16
DL441      05  FILLER                   PIC X(107) VALUE SPACES.        DELLRT16
                                                                        DELLRT16
       01  WK-POLICY-AREA.                                               0000477
           05  WK-POLICY-NUMBER.                                         0000478
               10  WK-POLICYX-1-1                PIC X(1).               0000479
               10  WK-POLICY9-2-8                PIC 9(7).               0000480
           05  F1 REDEFINES WK-POLICY-NUMBER.                            0000481
               10  WK-POLICYX-1-2                PIC X(2).               0000482
               10  WK-POLICY9-3-8                PIC 9(6).               0000483
           05  F2 REDEFINES F1.                                          0000484
               10  WK-POLICYX-1-3                PIC X(3).               0000485
               10  WK-POLICY9-4-8                PIC 9(5).               0000486
           05  F3 REDEFINES F2.                                          0000487
               10  WK-POLICYX-1-4                PIC X(4).               0000488
               10  WK-POLICY9-5-8                PIC 9(4).               0000489
           05  WK-POLICY9 REDEFINES F3                  PIC 9(8).        0000490
           05  SAVE-NEXT-POLICY.                                         0000491
               10 SAVE-NEXT-POLICYX-1-1          PIC X(1).               0000492
               10 SAVE-NEXT-POLICY9-2-8          PIC 9(7).               0000493
           05  F4 REDEFINES SAVE-NEXT-POLICY.                            0000494
               10 SAVE-NEXT-POLICYX-1-2          PIC X(2).               0000495
               10 SAVE-NEXT-POLICY9-3-8          PIC 9(6).               0000496
           05  F5 REDEFINES F4.                                          0000497
               10 SAVE-NEXT-POLICYX-1-3          PIC X(3).               0000498
               10 SAVE-NEXT-POLICY9-4-8          PIC 9(5).               0000499
           05  F6 REDEFINES F5.                                          0000500
               10 SAVE-NEXT-POLICYX-1-4          PIC X(4).               0000501
               10 SAVE-NEXT-POLICY9-5-8          PIC 9(4).               0000502
           05  SAVE-NEXT-POLICY9 REDEFINES F6           PIC 9(8).        0000503
           05  SAVE-PASS-POLICY                  PIC X(8).               0000504
           05  SAVE-TASK-POLICY                  PIC X(8).               0000505
           05  SAVE-NEW-POLICY                   PIC X(8).               0000506
           05  HOLD-TOTAL-RECS                   PIC 9(3).               0000507
                                                                         0000508
MK131  01  HOLD-GU-RECORD.                                               0000509
MK131      05  HOLD-GU-REC.                                              0000510
MK131          10  HOLD-GU-KEY.                                          0000511
MK131              15 HGU-CLIENT-GUID            PIC X(36).              0000512
MK131          10  HOLD-GU-DATA.                                         0000513
MK131              15 HGU-DELL-GUID              PIC X(36).              0000514
MK131              15 HGU-IMAGE-REFID            PIC X(36).              0000515
MK131              15 HGU-POLICY                 PIC X(8).               0000516
MK131              15 HGU-DATE                   PIC X(8).               0000517
MK131              15 HGU-TIME                   PIC X(8).               0000518
MK131              15 HGU-PREAPP-REFID           PIC X(36).              0000519
MK131              15 HGU-FINAL-DATE             PIC X(8).               0000520
MK131              15 HGU-FINAL-TIME             PIC X(8).               0000521
MK131              15 FILLER                          PIC X(116).        0000522
MK131                                                                    0000523
                                                                         0000524
PS134  01  PROCESS-CONTROL-PARMS.                                        0000525
PS134      05  PARM01-APP-LIMIT                  PIC 9(9).               0000526
PS134      05  PARM02-TABLE-MAX                  PIC 99 VALUE 10.        0000527
PS134      05  PARM02-TABLE-AREA.                                        0000528
PS134          10  PARM02-TABLE OCCURS 10.                               0000529
PS134              15  PARM02-BYPASS-DELLGUID    PIC X(36).              0000530
                                                                         0000531
PS134  01  PARM-RECORD.                                                  0000532
PS134      05  PARM-TYPE                         PIC XX.                 0000533
PS134      05  PARM-VALUE.                                               0000534
PS134          10  PARM-APP-LIMIT-IN             PIC 9(9).               0000535
PS134          10  FILLER                             PIC X(27).         0000536
PS134      05  FILLER                                 PIC X(42).         0000537
DL442  01  PROCESS-OPT.                                                 DELLRT16
DL442      05  PROCESS-OPT-TYPE                  PIC X(3).              DELLRT16
JJ711      05  PROCESS-RESET                     PIC X.
JJ711      05  FILLER                            PIC X(76).
DL442 *    05  FILLER                            PIC X(77).             DELLRT16
                                                                        DELLRT16
DL442  01  LK-PERSON-MASTER                      PIC X(4000).           DELLRT16
                                                                        
PS141  01  NB452-PASS-AREA.                                              0000539
PS141      05  NB452-PASS-WA-REC-TBL.                                    0000540
PS141          10  NB452-WA-TABLE-DATA                 PIC X(12100).     0000541
PS141          10  NB452-TAB-RECORD REDEFINES NB452-WA-TABLE-DATA        0000542
PS141       OCCURS 50 TIMES.                                             0000542
PS141             15  NB452-WTR-KEY.                                     0000543
PS141                 20  NB452-WTR-FILE-ID                  PIC X(2).   0000544
PS141                 20  NB452-WTR-REC-ID                   PIC X(2).   0000545
PS141                 20  NB452-WTR-APP-ID-X.                            0000546
PS141                     25  NB452-WTR-APP-ID               PIC 9(2).   0000547
PS141             15  NB452-WTR-REC-DATA  OCCURS 4 TIMES.                0000548
PS141                 20  NB452-WTR-REC-SEQ-X.                           0000549
PS141                     25  NB452-WTR-REC-SEQ              PIC 9(2).   0000550
PS141                 20  NB452-WTR-WA-KEY                   PIC X(54).  0000551
PS141                 20  NB452-WTR-STATUS                   PIC X(3).   0000552
PS141      05  NB452-PASS-CO                           PIC XX.           0000553
PS141      05  NB452-ERR-MESSAGE                       PIC X(45).        0000554
PS141      05  NB452-POLICY-FOUND                      PIC X.            0000555
PS141      05  NB452-TASK-NEEDED                       PIC X.            0000556
PS141      05  NB452-POLICY-NUMBER                     PIC X(8).         0000557
BT212      05  NB452-NFU-TASK                          PIC X.            0000558
BT212      05  NB452-REMARK3                           PIC X.            0000559
BT213      05  NB452-CDC-DATE                          PIC X(8).         0000560
BT214      05  NB452-EMPLOYER-ID                       PIC X(5).         0000561
BT301      05  NB452-DUP-TASK                          PIC X.           DELLRET2
BT345      05  NB452-STS-TASK                          PIC X.           DELLRET6
BT345      05  NB452-DTH-TASK                          PIC X.           DELLRET6
BT345      05  NB452-COC-TASK                          PIC X.           DELLRET6
BT345      05  NB452-ACT-TASK                          PIC X.           DELLRET6
BT345      05  NB452-LTR-TASK                          PIC X.           DELLRET6
BT345      05  NB452-ACTION-DATE                       PIC X(8).        DELLRET6
BT345      05  NB452-CDF-IND                           PIC X.           DELLRET6
BT371      05  NB452-CNX-TASK                          PIC X.           DELLRET9
DL441      05  NB452-POLICY-STATUS                     PIC X.           DELLRT16
DL441      05  NB452-BASE-LM-FACE-CHANGE               PIC X.           DELLRT16
DL451      05  NB452-AB-BEN-CHANGE-D01-031             PIC X.           DELLRT17
DL451      05  NB452-AB-BEN-CHANGE-D01-036             PIC X.           DELLRT17
DL451      05  NB452-AB-BEN-CHANGE-D02-032             PIC X.           DELLRT17
DL451      05  NB452-AB-BEN-CHANGE-D02-037             PIC X.           DELLRT17
DL455      05  NB452-AB-BEN-CHANGE-D01-031-A           PIC 9(11).       DELLRT17
DL455      05  NB452-AB-BEN-CHANGE-D01-036-A           PIC 9(11).       DELLRT17
DL455      05  NB452-AB-BEN-CHANGE-D02-032-A           PIC 9(11).       DELLRT17
DL455      05  NB452-AB-BEN-CHANGE-D02-037-A           PIC 9(11).       DELLRT17
DL442      05  NB452-PROCESS-OPT-TYPE                  PIC X(3).        DELLRT16
AS551      05  NB452-DIR-FLAG                          PIC X.
AS551      05  NB452-REV-FLAG                          PIC X.
PB881      05  NB452-EENRL-CLOSE                       PIC X.
PS141                                                                    0000562
       01  HOLD-WA-RECORD.                                               0000563
           05  HOLD-WA-REC.                                              0000564
               10  HOLD-WA-KEY.                                          0000565
                   15  HWA-TRANS-TYPE            PIC X(2).               0000566
                   15  HWA-CO                    PIC X(2).               0000567
                   15  HWA-DELL-GUID             PIC X(36).              0000568
                   15  HWA-TOTAL-RECS            PIC 9(3).               0000569
                   15  HWA-DUP-SEQ               PIC 9(3).               0000570
                   15  HWA-REC-TYPE.                                     0000571
                       20  HWA-FILE-ID           PIC X(2).               0000572
                       20  HWA-REC-ID            PIC X(2).               0000573
                       20  HWA-REC-APPID-X.                              0000574
                           25  HWA-REC-APPID     PIC 9(2).               0000575
                       20  HWA-REC-SEQ-X.                                0000576
                           25  HWA-REC-SEQ       PIC 9(2).               0000577
               10  HWA-DATA.                                             0000578
                   15  HWA-STATUS                PIC X(3).               0000579
                   15  HWA-CLIENT-GUID           PIC X(36).              0000580
                   15  HWA-IMAGE-REFID           PIC X(36).              0000581
                   15  HWA-OS-DATE               PIC X(8).               0000582
                   15  HWA-OS-TIME               PIC X(8).               0000583
                   15  HWA-MF-DATE               PIC X(8).               0000584
                   15  HWA-MF-TIME               PIC X(8).               0000585
BT132              15  HWA-POLCRECTYPE           PIC X.                  0000586
BT132              15  HWA-FILETYPE              PIC X.                  0000587
BT132              15  FILLER                         PIC X(37).         0000588
           05  HWA-RECORD-DATA.                                          0000589
               10  FILLER                             PIC X(1548).       0000590
                                                                         0000591
BT371  01  SAVE-WA-RECORD                        PIC X(1748).           DELLRET9
       01  WS-LOG-REC.                                                   0000592
           05  WS-LOG-LEN                        PIC S9(9) COMP  VALUE   0000593
            +2554.                                                       0000593
           05  WS-LOG-MESSID                     PIC S9(9) COMP  VALUE   0000594
            -3100.                                                       0000594
           05  WS-LOG-TASK                       PIC X(4)     VALUE      0000595
           'N402'.                                                       0000595
           05  WS-LOG-DATE                       PIC X(8).               0000596
           05  WS-LOG-TIME.                                              0000597
               10  WS-LOG-HH                     PIC X(2).               0000598
               10  FILLER                             PIC X        VALUE 0000599
           '/'.                                                          0000599
               10  WS-LOG-MM                     PIC X(2).               0000600
               10  FILLER                             PIC X        VALUE 0000601
           '/'.                                                          0000601
               10  WS-LOG-SS                     PIC X(2).               0000602
               10  FILLER                             PIC X        VALUE 0000603
           '.'.                                                          0000603
               10  WS-LOG-10TH                   PIC X.                  0000604
           05  WS-LOG-FILEID.                                            0000605
               10  WS-LOG-FILECO                 PIC X(2).               0000606
               10  WS-LOG-FILELIT                PIC X(2)     VALUE      0000607
           'AP'.                                                         0000607
           05  WS-LOG-KEY.                                               0000608
               10  WS-KEYCO                      PIC X(2).               0000609
               10  WS-KEYPOL                     PIC X(8).               0000610
               10  WS-KEYBLANK                   PIC X(2).               0000611
               10  WS-KEYAP                      PIC X(2).               0000612
               10  FILLER                             PIC X(6).          0000613
           05  WS-LOG-APREC                      PIC X(2500).            0000614
                                                                         0000615
       01  WA-RECORD-TABLE.                                              0000616
           05  WA-TABLE-DATA                     PIC X(12100).           0000617
           05  WA-TAB-RECORD REDEFINES WA-TABLE-DATA OCCURS 50 TIMES.    0000618
               10  WTR-KEY.                                              0000619
                   15  WTR-FILE-ID               PIC X(2).               0000620
                   15  WTR-REC-ID                PIC X(2).               0000621
                   15  WTR-APP-ID-X.                                     0000622
                       20  WTR-APP-ID            PIC 9(2).               0000623
               10  WTR-REC-DATA  OCCURS 4 TIMES.                         0000624
                   15  WTR-REC-SEQ-X.                                    0000625
                       20  WTR-REC-SEQ           PIC 9(2).               0000626
CL181              15  WTR-WA-KEY.                                       0000627
CL181                  20  FILLER                PIC X(50).              0000628
CL181                  20  WTR-REC-APPID-X.                              0000629
CL181                      25  WTR-REC-APPID     PIC 9(2).               0000630
CL181                  20  FILLER                PIC X(2).               0000631
                   15  WTR-STATUS                PIC X(3).               0000632
BT132                                                                    0000633
BT142  01  SY-ADDL-TASK-TABLE.                                           0000634
BT132      05  SY-TABLE-DATA                     PIC X(110).             0000635
BT132      05  SY-TAB-RECORD REDEFINES SY-TABLE-DATA OCCURS 10 TIMES.    0000636
BT132          10  SY-KEY.                                               0000637
BT132              15  SY-PROCESS                PIC X(6).               0000638
BT132              15  SY-STEP                   PIC X(3).               0000639
BT132              15  SY-STATUS                 PIC X.                  0000640
BT132              15  SY-PRIORITY               PIC X.                  0000641
                                                                         0000642
       01  HOLD-PROCESS-STEP-RECORD.                                     0000643
           05  HOLD-PROCESS-STEPS-INFO.                                  0000644
               10  HPSI-PS-KEY.                                          0000645
                   15  HPSI-PS-CO                PIC X(2).               0000646
                   15  HPSI-PS-ID                PIC X(2).               0000647
                   15  HPSI-PS-TYPE              PIC X(3).               0000648
                   15  HPSI-PS-PROCESS           PIC X(6).               0000649
                   15  HPSI-PS-STEP              PIC X(3).               0000650
               10  HPSI-PS-FUNCTION              PIC X(3).               0000651
               10  HPSI-PS-SUB-FUNCTION          PIC X(3).               0000652
               10  HPSI-PS-DESCRIPTION           PIC X(30).              0000653
               10  HPSI-PS-STEP-TABLE    OCCURS 20 TIMES.                0000654
                   15  HPSI-PS-STEP-DATA.                                0000655
                      20  HPSI-PS-NEXT-PROCESS   PIC X(6).               0000656
                      20  HPSI-PS-NEXT-STEP      PIC X(3).               0000657
                      20  HPSI-PS-ALWAYS         PIC X(1).               0000658
                      20  HPSI-PS-NEXT-STATUS    PIC X(1).               0000659
                      20  HPSI-PS-NEXT-DAYSX.                            0000660
                          25  HPSI-PS-NEXT-DAYS  PIC 9(3).               0000661
               10  FILLER                             PIC X(1968).       0000662
                                                                         0000663
NK771  01 NUM-FM                  PIC 9(3).
NK771 *01 WS-FM                   PIC X(2) VALUE 'FM'.
      *01  LIFE-MASTER-ZAP                       COPY 'LSNCS217'.        0000664
        COPY  'LSNCS217' REPLACING                                      DELLOCCH
            ==01 LIFE-MASTER-ZAP== BY                                   DELLOCCH
            ==01 LIFE-MASTER-ZAP==.                                     DELLOCCH
      *01  NEW-BUSINESS-RECORD                   COPY 'NBSCS039'.        0000665
        COPY  'NBSCS039' REPLACING                                      DELLOCCH
            ==01 NEW-BUSINESS-RECORD== BY                               DELLOCCH
            ==01 NEW-BUSINESS-RECORD==.                                 DELLOCCH
      *01  APPLICATION-RECORD-AP                 COPY 'NBSCS007'         0000666
      *        SUPPRESS REPLACING                                        0000667
        COPY  'NBSCS007' REPLACING                                      DELLOCCH
            ==01 APPLICATION-RECORD== BY                                DELLOCCH
            ==01 APPLICATION-RECORD-AP==                                DELLOCCH
             KEY-NA               BY KEY-NA-WA                           0000668
             CO-NA                BY CO-NA-WA                            0000669
             POLICY-NA            BY POLICY-NA-WA                        0000670
             AP-REC-NA            BY AP-REC-NA-WA                        0000671
CL131        CURR-STAT-NA         BY CURR-STAT-NA-WA                     0000672
CL131        LU-OPID-NA           BY LU-OPID-NA-WA                       0000673
CL131        LU-UPDATE-NA         BY LU-UPDATE-NA-WA                     0000674
CL131        LU-DATE-NA           BY LU-DATE-NA-WA                       0000675
CL131        REFUND-SUSP-FLAG-NA  BY REFUND-SUSP-FLAG-NA-WA              0000676
CL131        UW-DCSN-NA           BY UW-DCSN-NA-WA                       0000677
CL131        UW-SUB-DCSN-NA       BY UW-SUB-DCSN-NA-WA                   0000678
CL131        UW-DCSNDATE-NA       BY UW-DCSNDATE-NA-WA                   0000679
BT170        PYRL-EMPLOYERID-NA   BY PYRL-EMPLOYERID-NA-WA               0000680
BT184        OLD-POLICY-NA        BY OLD-POLICY-NA-WA                    0000681
BT184        FRATERNAL-INFO2-NA   BY FRATERNAL-INFO2-NA-WA               0000682
BT184        BASE-PLAN-PLANOPT-NA BY BASE-PLAN-PLANOPT-NA-WA            DELLRT11
DL           EAPPID-NA            BY EAPPID-NA-WA
DL391        ISS-DATE-NA          BY ISS-DATE-NA-WA.                    DELLRT11
      *01  COVERAGE-RECORD-CV                    COPY 'NBSCS041'         0000685
      *        SUPPRESS REPLACING                                        0000686
        COPY  'NBSCS041' REPLACING                                      DELLOCCH
            ==01 COVERAGE-RECORD== BY                                   DELLOCCH
            ==01 COVERAGE-RECORD-CV==                                   DELLOCCH
             KEY-N1               BY KEY-N1-WA                           0000687
             CO-N1                BY CO-N1-WA                            0000688
             POLICY-N1            BY POLICY-N1-WA                        0000689
             APPID-N1             BY APPID-N1-WA                         0000690
             INS-RES-N1           BY INS-RES-N1-WA                       0000691
BT184        N1-REC-N1            BY N1-REC-N1-WA                        0000692
BT184        OCC-CLASS-N1         BY OCC-CLASS-N1-WA                     0000693
BT195        APPIDX-N1            BY APPIDX-N1-WA                        0000694
BT195        N1-RECX-N1           BY N1-RECX-N1-WA.                      0000695
                                                                         0000696
      *01  CYCLE-DATE-CO-CONTROL                 COPY 'LSNCS212'.        0000697
        COPY  'LSNCS212' REPLACING                                      DELLOCCH
            ==01 CYCLE-DATE-CO-CONTROL== BY                             DELLOCCH
            ==01 CYCLE-DATE-CO-CONTROL==.                               DELLOCCH
      *01  LIFE-MASTER-RECORD                    COPY 'LSNCS808'.        0000698
        COPY  'LSNCS808' REPLACING                                      DELLOCCH
            ==01 LIFE-MASTER-RECORD== BY                                DELLOCCH
            ==01 LIFE-MASTER-RECORD==.                                  DELLOCCH
      *01  PLAN-REC                              COPY 'LSNCS019'.        0000699
        COPY  'LSNCS019' REPLACING                                      DELLOCCH
            ==01 PLAN-REC== BY                                          DELLOCCH
            ==01 PLAN-REC==.                                            DELLOCCH
      *01  XDATE-PARMS                           COPY 'XDATECS'.         0000700
        COPY  'XDATECS' REPLACING                                       DELLOCCH
            ==01 XDATE-PARMS== BY                                       DELLOCCH
            ==01 XDATE-PARMS==.                                         DELLOCCH
      *01  SY870-PASS                            COPY 'LSNCS870'.        0000701
        COPY  'LSNCS870' REPLACING                                      DELLOCCH
            ==01 SY870-PASS== BY                                        DELLOCCH
            ==01 SY870-PASS==.                                          DELLOCCH
      *01  GU-GUID-RECORD                        COPY 'NBSCS112'.        0000702
        COPY  'NBSCS112' REPLACING                                      DELLOCCH
            ==01 GU-GUID-RECORD== BY                                    DELLOCCH
            ==01 GU-GUID-RECORD==.                                      DELLOCCH
       01   WA-APPLICATION-RECORD-GRP.                                  DELLMACH
         02 WA-APPLICATION-RECORD-SEQ           PIC S9(7) COMP-3.       DELLMACH
         02 WA-APPLICATION-DATE                 PIC X(8).               DELLMACH
         02 WA-APPLICATION-TIME                 PIC X(8).               DELLMACH
      *01  WA-APPLICATION-RECORD                 COPY 'NBSCS111'.        0000703
        COPY  'NBSCS111' REPLACING                                      DELLOCCH
            ==01 WA-APPLICATION-RECORD== BY                             DELLOCCH
            ==02 WA-APPLICATION-RECORD==.                               DELLOCCH
      *01  WA-PASS-KEY-AREA                      COPY 'NBSCS113'.        0000704
        COPY  'NBSCS113' REPLACING                                      DELLOCCH
            ==01 WA-PASS-KEY-AREA== BY                                  DELLOCCH
            ==01 WA-PASS-KEY-AREA==.                                    DELLOCCH
      *01  NBS368-PASS-AREA                      COPY 'NBSCS368'.        0000705
        COPY  'NBSCS368' REPLACING                                      DELLOCCH
            ==01 NBS368-PASS-AREA== BY                                  DELLOCCH
            ==01 NBS368-PASS-AREA==.                                    DELLOCCH
      *01  PA169-PASS-AREA                       COPY 'LSNCS469'.        0000706
        COPY  'LSNCS469' REPLACING                                      DELLOCCH
            ==01 PA169-PASS-AREA== BY                                   DELLOCCH
            ==01 PA169-PASS-AREA==.                                     DELLOCCH
      *01  LSNNB080-PASS-RECORD                  COPY 'NBSCS180'.        0000707
        COPY  'NBSCS180' REPLACING                                      DELLOCCH
            ==01 LSNNB080-PASS-RECORD== BY                              DELLOCCH
            ==01 LSNNB080-PASS-RECORD==.                                DELLOCCH
      *01  TASK-RECORD                           COPY 'TSKCS001'.        0000708
        COPY  'TSKCS001' REPLACING                                      DELLOCCH
            ==01 TASK-RECORD== BY                                       DELLOCCH
            ==01 TASK-RECORD==.                                         DELLOCCH
      *01  TASK-PROCSS-STEP-RECORD               COPY 'TSKCS010'.        0000709
        COPY  'TSKCS010' REPLACING                                      DELLOCCH
            ==01 WORKFLOW-RECORD== BY                                   DELLOCCH
            ==01 TASK-PROCSS-STEP-RECORD==.                             DELLOCCH
      *01  TSKCS003-PASS-AREA                    COPY 'TSKCS003'.        0000710
        COPY  'TSKCS003' REPLACING                                      DELLOCCH
            ==01 TSKCS003-PASS-AREA== BY                                DELLOCCH
            ==01 TSKCS003-PASS-AREA==.                                  DELLOCCH
      *01  TSKCS250-PASS-AREA                    COPY 'TSKCS250'.        0000711
        COPY  'TSKCS250' REPLACING                                      DELLOCCH
            ==01 TSKCS250-PASS-AREA== BY                                DELLOCCH
            ==01 TSKCS250-PASS-AREA==.                                  DELLOCCH
      *01  REFERNCE-OPTION-RECORD                COPY 'NBSCS003'.        0000712
        COPY  'NBSCS003' REPLACING                                      DELLOCCH
            ==01 REFERNCE-OPTION-RECORD== BY                            DELLOCCH
            ==01 REFERNCE-OPTION-RECORD==.                              DELLOCCH
         02  FILLER                                   PIC X(3300).       0000713
         02  FILLER                                   PIC X(20).         0000714
         02  FILLER                                   PIC X(224).        0000715
CL131 *01  LS193-SCREEN                          COPY 'LS193'.           0000716
        COPY  'LS193' REPLACING                                         DELLOCCH
            ==02 LS193== BY                                             DELLOCCH
            ==01 LS193-SCREEN==.                                        DELLOCCH
BT132 *01  WS-LITERAL-CONSTANTS                  COPY 'NBSCS108'.        0000717
        COPY  'NBSCS108' REPLACING                                      DELLOCCH
            ==01 WS-CONSTANTS== BY                                      DELLOCCH
            ==01 WS-LITERAL-CONSTANTS==.                                DELLOCCH
BT151 *01  MASTER-STRUCTURE-RECORD               COPY 'LSNCE013'.        0000718
        COPY  'LSNCE013' REPLACING                                      DELLOCCH
            ==01 MASTER-STRUCTURE-RECORD== BY                           DELLOCCH
            ==01 MASTER-STRUCTURE-RECORD==.                             DELLOCCH
BT151 *01 LM-SEG-PR-LAYOUT                       COPY 'LSNCSPR'.         0000719
        COPY  'LSNCSPR' REPLACING                                       DELLOCCH
            ==01 LM-SEG-PR-LAYOUT== BY                                  DELLOCCH
            ==01 LM-SEG-PR-LAYOUT==.                                    DELLOCCH
BT151 *01 LMZ-SEG-PR-LAYOUT                      COPY 'LSNCSPRZ'.        0000720
        COPY  'LSNCSPRZ' REPLACING                                      DELLOCCH
            ==01 LMZ-SEG-PR-LAYOUT== BY                                 DELLOCCH
            ==01 LMZ-SEG-PR-LAYOUT==.                                   DELLOCCH
                                                                         0000721
DL441 *01  OVERRIDE-BENEFIT-REC                  COPY 'NBSCS547'.       DELLRT16
         COPY 'NBSCS547' REPLACING                                      DELLRT16
          ==01 OVERRIDE-BENEFIT-REC== BY                                DELLRT16
          ==01 OVERRIDE-BENEFIT-REC==.                                  DELLRT16
DL441  01  E00-OVERRIDE-BENEFIT-REC              PIC X(560).            DELLRT16
DL441  01  D01-OVERRIDE-BENEFIT-REC              PIC X(560).            DELLRT16
DL441  01  D02-OVERRIDE-BENEFIT-REC              PIC X(560).            DELLRT16
                                                                        DELLRT16
AS201    COPY LSNCS896.
DL461    COPY 'LSNCS802' REPLACING 
DL461     ==01  LW-REQUEST-REC== BY 
DL461     ==01  LW-REQUEST-REC==.
DL461    
PB631 *01  LM-SEG-DI-LAYOUT                      COPY 'LSNCSDI'.
PB631   COPY  'LSNCSDI' REPLACING
PB631      ==01  LM-SEG-DI-LAYOUT== BY
PB631      ==01  LM-SEG-DI-LAYOUT==.

PB631 *01  LMZ-SEG-DI-LAYOUT                     COPY 'LSNCSDIZ'.
PB631   COPY  'LSNCSDIZ' REPLACING
PB631      ==01  LMZ-SEG-DI-LAYOUT== BY
PB631      ==01  LMZ-SEG-DI-LAYOUT==.

NK771 *01  LM-SEG-FM-LAYOUT                      COPY 'LSNCSFM'.
NK771   COPY  'LSNCSFM' REPLACING
NK771      ==01  LM-SEG-FM-LAYOUT== BY
NK771      ==01  LM-SEG-FM-LAYOUT==.

NK771 *01  LMZ-SEG-FM-LAYOUT                     COPY 'LSNCSFMZ'.
NK771   COPY  'LSNCSFMZ' REPLACING
NK771      ==01  LMZ-SEG-FM-LAYOUT== BY
NK771      ==01  LMZ-SEG-FM-LAYOUT==.

       01  PGM-ERROR.                                                    0000722
           05  PGM-ERR                           PIC S9(4) COMP.         0000723
           05  PGM-MSG                           PIC X(30).              0000724
                                                                         0000725
PB631  01  WS-INFO11                             PIC X(3).
PB631  01  WS-OPT-141                            PIC X(1) VALUE SPACES.
PB631  01  RRK-KEY.
PB631      05  RRK-CO                            PIC X(2).
PB631      05  RRK-PARM                          PIC X(2).
PB631      05  RRK-TYPE                          PIC X(2).
PB631      05  RRK-KEYVAL                        PIC X(12).

       PROCEDURE DIVISION.                                               0000726
DL457      DISPLAY ' START OF LSNNB402'.                                DELLRT17
           PERFORM 1000-MIGR-OPEN-READ-WRITE THRU 1000-MIGR-EXIT.       DELLSQCH
           PERFORM 1099-MIGR-VSAM-OPEN-FILE  THRU 1099-MIGR-EXIT.       DELLIDCH
           PERFORM 1000-INIT-ROUTINE.                                    0000727
           PERFORM 1100-PROCESS-ROUTINE.                                 0000728
           PERFORM 1200-PROCESS-END.                                     0000729
           PERFORM 9000-CLOSE-FILES THRU 9000-CLOSE-EXIT.               DELLSQCH
           PERFORM 9099-CLOSE-VSAM-FILE THRU 9099-CLOSE-EXIT.           DELLIDCH
           STOP RUN.                                                     0000730
                                                                         0000731
       1000-INIT-ROUTINE  SECTION.                                       0000732
           OPEN INPUT DATEFILE.                                          0000733
           MOVE 00000000 TO WK-SYSTEM-TIME.                              0000734
       1005-INIT-TIME.                                                   0000735
           MOVE SPACES TO XDATE-PARMS.                                   0000736
           MOVE 'YYYYMMDD' TO XD-OUTPUT-FORMAT.                          0000737
           CALL 'XDATE' USING XDATE-PARMS.                               0000738
           IF XD-OUT-DATE = 'BAD DATE'                                   0000739
               MOVE 00000000 TO WK-SYSTEM-TIME                           0000740
               GO TO 1010-INIT-DATE-COMPANY.                             0000741
                                                                         0000742
           MOVE XD-OUT-CURR-TIME TO WK-SYSTEM-TIME.                      0000743
       1010-INIT-DATE-COMPANY.                                           0000744
           READ DATEFILE INTO CYCLE-DATE-CO-CONTROL                      0000745
               AT END                                                    0000746
                   DISPLAY '** DATEFILE MISSING - LSNNB402 ABENDING **'  0000747
                   CLOSE DATEFILE                                        0000748
                   PERFORM 9900-ABEND-RTN.                               0000749
                                                                         0000750
           MOVE CDC-CO TO SV-CO                                          0000751
                       WA-FILE-CO                                        0000752
CL131                  W9-FILE-CO                                        0000753
                       GU-FILE-CO                                        0000754
                       AP-FILE-CO                                        0000755
                       NB-FILE-CO                                        0000756
                       PO-FILE-CO                                        0000757
                       PL-FILE-CO                                        0000758
BT151                  M1-FILE-CO                                        0000759
                       U1-FILE-CO.                                       0000760
           MOVE CDC-CALC-DATEX TO WK-SYSTEM-DATE.                        0000761
           CLOSE DATEFILE.                                               0000762
      *    CALL 'OPENW1'.                                               DELLSQCH
      *    CALL 'OPENW2'.                                               DELLSQCH
      *    CALL 'OPENW3'.                                               DELLSQCH
SM211 *    CALL 'OPENW4'.                                               DELLSQCH
PS401 *    CALL 'OPENW6'.                                               DELLRT12
DL441 *    CALL 'OPENW7'.                                               DELLRT16
DL442 *    CALL 'OPENW8'.                                               DELLRT16
DL442 *    CALL 'READ2' USING PROCESS-OPT                               DELLRT16
DL442 *                PROCESS-OPT-EOF.                                 DELLRT16
           READ READ2 INTO PROCESS-OPT                                  DELLSQCH
                AT END MOVE 'Y' TO PROCESS-OPT-EOF                      DELLSQCH
           END-READ                                                     DELLSQCH
           MOVE WS-READ2-FS TO CPY-FS-CODE                              DELLSQCH
           PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.                 DELLSQCH

DL442      IF PROCESS-OPT-EOF = 'Y'                                     DELLRT16
DL442          GO TO 1015-INIT-LIT.                                     DELLRT16
DL442      IF PROCESS-OPT-TYPE = 'NBS' OR 'PHS'                         DELLRT16
DL458          DISPLAY ' PROCESS-OPT-TYPE = ' PROCESS-OPT-TYPE          DELLRT17
DL442          GO TO 1015-INIT-LIT.                                     DELLRT16
DL442      DISPLAY '** INVALID READ2 PROCESS OPT - LSNNB402 ABENDING **'DELLRT16
DL442      PERFORM 9900-ABEND-RTN.                                      DELLRT16
       1015-INIT-LIT.                                                    0000767
           MOVE 'WA' TO WA-FILE-LIT.                                     0000768
           MOVE 'GU' TO GU-FILE-LIT.                                     0000769
           MOVE 'AP' TO AP-FILE-LIT.                                     0000770
           MOVE 'NB' TO NB-FILE-LIT.                                     0000771
           MOVE 'PL' TO PL-FILE-LIT.                                     0000772
           MOVE 'PO' TO PO-FILE-LIT.                                     0000773
BT151      MOVE 'M1' TO M1-FILE-LIT.                                     0000774
           MOVE 'U1' TO U1-FILE-LIT.                                     0000775
CL131      MOVE 'W9' TO W9-FILE-LIT.                                     0000776
       1020-INIT-STATUS.                                                 0000777
           MOVE 000 TO NUM-AP                                            0000778
BT162               NUM-AC                                               0000779
BT231               NUM-OW                                               0000780
DD281               NUM-OR                                              DELLRET2
                    NUM-CV                                               0000781
SS521               NUM-DI
                    NUM-T1                                               0000782
                    NUM-T2                                               0000783
CL131               NUM-BN                                               0000784
CL131               NUM-AB                                               0000785
                    NUM-SY                                               0000786
                    NUM-NT                                               0000787
                    NUM-NTE                                              0000788
AK131               NUM-PN                                               0000789
BT444               NUM-EI                                              DELLRT16
AR150               NUM-AL                                                      
AR150               NUM-GW                                                      
AR150               NUM-IB  
NK771               NUM-FM                                                    
BT043               NUM-INV.                                             0000790
           MOVE +0 TO PGM-ERR                                            0000791
                   WORK-DAYS-SAVE                                        0000792
                   APP-COUNT.                                            0000793
           MOVE SPACES TO PGM-MSG                                        0000794
                  SV-DELL-GUID                                           0000795
BT185             SAVE-EMPLOYER-ID                                       0000796
                  MESSAGE-FIELD.                                         0000797
           MOVE 'N' TO EOF-WA                                            0000798
                    ERROR-SWITCH                                        DELLRET2
BT302               WK-PARM38.                                          DELLRET2
           MOVE SPACES TO WK-STATUS                                      0000800
                  HOLD-WA-RECORD.                                        0000801
       1030-INITIAL-WA-READ.                                             0000802
CL131      MOVE +0   TO AI-RTN-CODE.                                     0000803
CL131      MOVE SPACES TO WA-APPLICATION-RECORD.                         0000804
CL131      MOVE LOW-VALUES TO AI-W9-KEY.                                 0000805
CL131      MOVE AI-W9-KEY TO HOLD-AI-W9-KEY.                             0000806
           MOVE LOW-VALUES TO AI-WA-KEY.                                DELLMNCH
CL131      PERFORM 8200-READ-NEXT-ALT-KEY.                               0000807
BT132      IF (RTN-CODE NOT = +0)                                        0000808
BT132          GO TO 1099-INIT-ROUTINE-EXIT.                             0000809
CL131                                                                    0000810
CL131      MOVE SPACES TO WA-APPLICATION-RECORD.                         0000811
CL131      MOVE LOW-VALUES TO WS-KEY.                                    0000812
CL131      MOVE 'WA' TO WS-TRANS-TYPE.                                   0000813
BT132      MOVE SV-CO TO WS-CO.                                          0000814
      *    MOVE AI-W9-DELL-GUID TO WS-DELL-GUID.                         0000815
           MOVE AI-W9-DELL-GUID TO WS-KEY(1:40).                        DELLMACH
           PERFORM 8300-READ-NEXT-WA-RECS.                               0000816
           IF RTN-CODE = +12                                             0000817
               DISPLAY ' WA FILE I/O ERROR-PROGRAM RTN-CODE = +12 '      0000818
               DISPLAY ' ERROR IN 1030-INITIAL-WA-READ PARAGRAPH '       0000819
               DISPLAY ' CHECK WA FILE . . . LSNNB402 IS ABENDING  '     0000820
               MOVE +1030 TO ABEND-CODE                                  0000821
               PERFORM 9900-ABEND-RTN.                                   0000822
                                                                         0000823
           IF (RTN-CODE NOT = +0)                                        0000824
               GO TO 1099-INIT-ROUTINE-EXIT.                             0000825
                                                                         0000826
DL457      ADD +1 TO WS-REC-READ.                                       DELLRT17
           MOVE +1 TO APP-COUNT.                                         0000827
DL441      IF OB-HEADER-FIRST-TIME = 'Y'                                DELLRT16
DL441         MOVE 'N' TO OB-HEADER-FIRST-TIME                          DELLRT16
DL441 *       CALL 'WRITE7' USING WB-OVERRIDE-BENEFIT-HEADING.          DELLRT16
               WRITE WRITE7-REC FROM                                    DELLRT16
                 WB-OVERRIDE-BENEFIT-HEADING                            DELLRT16
               MOVE WS-WRITE7-FS TO CPY-FS-CODE                         DELLRT16
               PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.             DELLRT16
DL457         ADD +1 TO WS-REC-WRITE7.                                  DELLRT17
       1099-INIT-ROUTINE-EXIT.                                           0000828
           EXIT.                                                         0000829
                                                                         0000830
       1100-PROCESS-ROUTINE  SECTION.                                    0000831
PS134      PERFORM 1300-LOAD-PARMS.                                      0000832
           IF APP-COUNT = +0                                             0000833
               GO TO 1199-PROCESS-ROUTINE-EXIT.                          0000834
                                                                         0000835
       1110-VALIDATION-PROCESS.                                          0000836
PS134      IF APP-COUNTER > PARM01-APP-LIMIT                             0000837
PS134          GO TO 1199-PROCESS-ROUTINE-EXIT.                          0000838
           IF WA-STATUS = SPACES                                         0000840
             AND WA-DUP-SEQ = 000                                        0000841
               NEXT SENTENCE                                             0000842
           ELSE                                                          0000843
               GO TO 1195-WA-NEXT-READ.                                  0000844
                                                                         0000845
PS133      IF WA-REC-TYPE = 'APAP  01'                                   0000846
PS133          IF APAP1-DELL-GUID-PROCESSED-FLAG = 'Y'                   0000847
PS133              GO TO 1195-WA-NEXT-READ                               0000848
PS133          ELSE                                                      0000849
PS133              PERFORM 4300-SET-DELL-GUID-PROC-FLAG.                 0000850
                                                                         0000851
DL351      MOVE WA-APPLICATION-RECORD TO HOLD-WA-RECORD.                DELLRET7
DL351      ADD 00000101 TO FTN-SYSTEM-TIME.                             DELLRET7
DL351      MOVE +0 TO WS-SUB                                            DELLRET7
DL351              WS-SUB1                                              DELLRET7
DL351              WS-SUB2                                              DELLRET7
DL351              WS-REC-COUNT                                         DELLRET7
DL351              WS-SY-COUNT                                          DELLRET7
DL351              WS-CV-COUNT                                          DELLRET7
DL351              WS-PM-COUNT                                          DELLRET7
DL351              WS-AP-COUNT.                                         DELLRET7
DL351      MOVE 'N' TO ERROR-SWITCH.                                    DELLRET7
DL351      MOVE 'N' TO WS-PM-FOUND.                                     DELLRET7
DL351      MOVE SPACES TO SV-LAST-WA-KEY.                               DELLRET7
DL351      PERFORM 4200-LOAD-WA-KEY-TABLE.                              DELLRET7
DL351      IF ERROR-SWITCH = 'Y'                                        DELLRET7
DL351          GO TO 1115-VAL-ERR-SWITCH.                               DELLRET7
DL351                                                                   DELLRET7
PS401      IF WA-POLCRECTYPE = 'S'                                      DELLRT12
PS401          GO TO 1116-CONTINUE.                                     DELLRT12
                                                                        
DL351      IF SAVE-LS-OVERRIDE-IND = '05'                               DELLRET7
DL351          GO TO 1115-VAL-ERR-SWITCH.                               DELLRET7
DL351                                                                   DELLRET7
BT142      IF WA-FILETYPE = WS-E OR WS-M                                 0000852
CL131          PERFORM 2100E-VALIDATION-CHECK                            0000853
CL131          GO TO 1115-VAL-ERR-SWITCH.                                0000854
CL131                                                                    0000855
           PERFORM 2100-VALIDATION-CHECK.                                0000856
CL131                                                                    0000857
CL131  1115-VAL-ERR-SWITCH.                                              0000858
DL401      IF (SAVE-LS-OVERRIDE-IND = WS-01 OR WS-02) AND               DELLRT12
DL401         (SAVE-PAIND-WASY = WS-G)                                  DELLRT12
DL401          GO TO 1116-CONTINUE.                                     DELLRT12
BT411      IF WA-POLCRECTYPE = 'U' AND                                  DELLRT13
BT411         (SAVE-PAIND-WASY = SPACES AND (SAVE-PAPOL-WASY NOT =      DELLRT13
BT411       SPACES))                                                    DELLRT13
BT411          GO TO 1116-CONTINUE.                                     DELLRT13
DL401      IF (SAVE-PAIND-WASY = SPACES AND SAVE-PAPOL-WASY NOT =       DELLRT12
DL401       SPACES)    OR                                               DELLRT12
DL401         (SAVE-PAIND-WASY NOT = SPACES AND SAVE-PAPOL-WASY = SPACESDELLRT12
DL401                               AND SAVE-BLOCK-ID-WASY = SPACES)    DELLRT12
DL401        OR                                                         DELLRT12
DL401         (SAVE-PAIND-WASY = WS-G AND SAVE-PAPOL-WASY NOT =         DELLRT12
DL401       SPACES) OR                                                  DELLRT12
DL401         (SAVE-PAIND-WASY NOT = SPACES AND                         DELLRT12
DL401                        (SAVE-PAIND-WASY NOT = WS-G AND WS-P))     DELLRT12
DL401          MOVE 'IND' TO WK-STATUS                                  DELLRT12
DL401          PERFORM 7200-APPLICATION-ERROR                           DELLRT12
DL401          GO TO 1195-WA-NEXT-READ.                                 DELLRT12
DL401                                                                   DELLRT12
DL401  1116-CONTINUE.                                                   DELLRT12
DL401                                                                   01086000
           IF ERROR-SWITCH = 'Y'                                         0000859
               GO TO 1195-WA-NEXT-READ.                                  0000860
PS401      IF WA-POLCRECTYPE = 'S'                                      DELLRT12
PS401          MOVE SPACES TO WA-PASS-KEY-AREA                          DELLRT12
PS401          MOVE SAVE-PAPOL-WASY TO WPK-POLICY                       DELLRT12
PS401          MOVE +1 TO WS-SUB1                                       DELLRT12
PS401          MOVE WTR-FILE-ID (WS-SUB1) TO WPK-FILE-TYPE              DELLRT12
PS401          MOVE WTR-REC-ID (WS-SUB1) TO WPK-RECORD-TYPE             DELLRT12
PS401          MOVE +1 TO WS-SUB2                                       DELLRT12
PS401          MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WPK-KEY-ONE        DELLRT12
PS401          MOVE +0 TO PGM-ERR                                       DELLRT12
PS401          MOVE 'LSNNB402' TO CALLING-PROGRAM                       DELLRT12
PS401          CALL 'LSNNB459' USING WA-PASS-KEY-AREA                   DELLRT12
PS401                         WA-RECORD-TABLE                           DELLRT12
PS401                         PGM-ERROR                                 DELLRT12
PS401                         CALLING-PROGRAM                           DELLRT12
PS402                         INFILE-STATUS-RECORD                      DELLRT12
PS452                         TASK-RECORD                               
PS401 *        CALL 'WRITE6' USING INFILE-STATUS-RECORD                 DELLRT12
               WRITE WRITE6-REC FROM                                    DELLRT12
                 INFILE-STATUS-RECORD                                   DELLRT12
               MOVE WS-WRITE6-FS TO CPY-FS-CODE                         DELLRT12
               PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT              DELLRT12
DL457          ADD +1 TO WS-REC-WRITE6                                  DELLRT17
DL681          PERFORM 7100-WRITE-LTR
PS452          PERFORM 4000-TASK-DISKADD                                
PS453          MOVE SPACES TO TASK-RECORD                               
PS401          IF WPK-ERR-CODE > SPACES                                 DELLRT12
PS401             MOVE 'Y' TO ERROR-SWITCH                              DELLRT12
PS401             GO TO 1195-WA-NEXT-READ                               DELLRT12
PS401          ELSE                                                     DELLRT12
PS401             GO TO 1195-WA-NEXT-READ.                              DELLRT12
                                                                         0000861
DL343      IF SAVE-LS-OVERRIDE-IND = '05'                               DELLRET6
DL343          MOVE SPACES TO WA-PASS-KEY-AREA                          DELLRET6
DL343          MOVE SAVE-PAPOL-WASY TO WPK-POLICY                       DELLRET6
DL343          MOVE +1 TO WS-SUB1                                       DELLRET6
DL343          MOVE WTR-FILE-ID (WS-SUB1) TO WPK-FILE-TYPE              DELLRET6
DL343          MOVE WTR-REC-ID (WS-SUB1) TO WPK-RECORD-TYPE             DELLRET6
DL343          MOVE +1 TO WS-SUB2                                       DELLRET6
DL343          MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WPK-KEY-ONE        DELLRET6
DL343          MOVE +0 TO PGM-ERR                                       DELLRET6
DL343          CALL 'LSNNB458' USING WA-PASS-KEY-AREA                   DELLRET6
DL343                         WA-RECORD-TABLE                           DELLRET6
DL343                         PGM-ERROR                                 DELLRET6
DL343          IF WPK-ERR-CODE > SPACES                                 DELLRET6
DL343             MOVE 'Y' TO ERROR-SWITCH                              DELLRET6
DL343             GO TO 1195-WA-NEXT-READ                               DELLRET6
DL343          ELSE                                                     DELLRET6
DL354             GO TO 1195-WA-NEXT-READ.                              DELLRET8
                                                                        01014000
MK131      IF WA-POLCRECTYPE = 'U'                                       0000862
MK131          PERFORM 3800-INIT-AP-NB-REC                               0000863
MK131          MOVE GU-POLICY TO SAVE-PASS-POLICY                        0000864
MK131                         SAVE-NEXT-POLICY                           0000865
CL131          GO TO 1117-CHECK-SWITCH.                                  0000866
MK131                                                                    0000867
BT151      IF WA-FILETYPE = WS-M                                         0000868
BT151        AND SAVE-LS-OVERRIDE-IND = WS-02                            0000869
BT151        AND SAVE-LS-PARAM-37 = WS-D                                 0000870
BT151           PERFORM 4900-DEL-TASK                                    0000871
BT151           MOVE 'DEL' TO WK-STATUS                                  0000872
BT151           PERFORM 7000-STATUS-ERROR-WA                             0000873
BT151           GO TO 1195-WA-NEXT-READ.                                 0000874
BT151                                                                    0000875
CL131      IF WA-FILETYPE = 'E'                                          0000876
CL131        AND SAVE-LS-OVERRIDE-IND = '01'                             0000877
CL131          MOVE SPACES TO WA-PASS-KEY-AREA                           0000878
CL131          MOVE SAVE-PASS-POLICY TO WPK-POLICY                       0000879
CL131          MOVE +1 TO WS-SUB1                                        0000880
CL131          MOVE WTR-FILE-ID (WS-SUB1) TO WPK-FILE-TYPE               0000881
CL131          MOVE WTR-REC-ID (WS-SUB1) TO WPK-RECORD-TYPE              0000882
CL131          MOVE +1 TO WS-SUB2                                        0000883
CL131          MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WPK-KEY-ONE         0000884
CL131          MOVE +0 TO PGM-ERR                                        0000885
CL131          CALL 'LSNNB445' USING WA-PASS-KEY-AREA                    0000886
CL131                         WA-RECORD-TABLE                            0000887
CL131                         PGM-ERROR                                  0000888
CL131          IF WPK-ERR-CODE > SPACES                                  0000889
CL131             MOVE 'Y' TO ERROR-SWITCH                               0000890
CL131             GO TO 1195-WA-NEXT-READ.                               0000891
CL131                                                                    0000892
BT142      IF WA-FILETYPE = WS-M                                         0000893
BT142        AND SAVE-LS-OVERRIDE-IND = WS-02                            0000894
BT142          MOVE SPACES TO WA-PASS-KEY-AREA                           0000895
DL441                 E00-OVERRIDE-BENEFIT-REC                          DELLRT16
DL441                 D01-OVERRIDE-BENEFIT-REC                          DELLRT16
DL441                 D02-OVERRIDE-BENEFIT-REC                          DELLRT16
BT142          MOVE SAVE-PASS-POLICY TO WPK-POLICY                       0000896
BT142          MOVE +1 TO WS-SUB1                                        0000897
BT142          MOVE WTR-FILE-ID (WS-SUB1) TO WPK-FILE-TYPE               0000898
BT142          MOVE WTR-REC-ID (WS-SUB1) TO WPK-RECORD-TYPE              0000899
BT142          MOVE +1 TO WS-SUB2                                        0000900
BT142          MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WPK-KEY-ONE         0000901
BT142          MOVE +0 TO PGM-ERR                                        0000902
BT142          MOVE WS-LSNNB402 TO WPK-PGM-ORIGIN                        0000903
PS251          CALL 'LSNNB547' USING WA-PASS-KEY-AREA                   DELLRETC
BT142                         WA-RECORD-TABLE                            0000905
DL441                         E00-OVERRIDE-BENEFIT-REC                  DELLRT16
DL441                         D01-OVERRIDE-BENEFIT-REC                  DELLRT16
DL441                         D02-OVERRIDE-BENEFIT-REC                  DELLRT16
BT142                         PGM-ERROR.                                 0000906
BT142      IF (WA-FILETYPE NOT = WS-M)                                   0000907
BT142        AND (SAVE-LS-OVERRIDE-IND NOT = WS-02)                      0000908
BT142             GO TO 1115-CHK-POL-MASTER.                             0000909
BT142 **************************************                             0000910
BT142 ****   CHECK RTN CODE IF C CONTINUE                                0000911
BT142 ****   CHECK RTN CODE IF T TASK AND RECYCLE                        0000912
BT142 ****   CHECK RTN CODE IF R RECYCLE                                 0000913
BT142 **************************************                             0000914
BT142          IF WPK-ERR-CODE > SPACES                                  0000915
BT142             MOVE WS-Y TO ERROR-SWITCH                              0000916
BT142             GO TO 1195-WA-NEXT-READ.                               0000917
BT142          IF WPK-POST-IMPORT-IND = WS-C                             0000918
BT142             GO TO 1115-CHK-POL-MASTER.                             0000919
BT142          IF WPK-POST-IMPORT-IND = WS-R                             0000920
BT142             MOVE WS-Y TO ERROR-SWITCH                              0000921
BT142             GO TO 1195-WA-NEXT-READ.                               0000922
BT142          IF WPK-POST-IMPORT-IND = WS-T                             0000923
BT142             PERFORM 4400-SET-TASK-RECORD                           0000924
BT142             MOVE WS-Y TO ERROR-SWITCH                              0000925
BT142             GO TO 1195-WA-NEXT-READ                                0000926
BT142          ELSE                                                      0000927
BT142             MOVE WS-Y TO ERROR-SWITCH                              0000928
BT142             GO TO 1195-WA-NEXT-READ.                               0000929
BT142                                                                    0000930
BT142 ***  ***CHECK POLICY MASTER    *****************************       0000931
BT142  1115-CHK-POL-MASTER.                                              0000932
BT142      IF WA-FILETYPE = WS-M                                         0000933
BT142        AND SAVE-LS-OVERRIDE-IND = WS-02                            0000934
DL457      ADD +1 TO WK-NB452-RECS-IN                                   DELLRT17
BT142      MOVE SPACES TO NB452-PASS-AREA                                0000935
BT345      MOVE WPK-CDF-IND TO NB452-CDF-IND                            DELLRET6
BT142      MOVE WA-TABLE-DATA TO NB452-WA-TABLE-DATA                     0000936
BT142      MOVE CDC-CO TO NB452-PASS-CO                                  0000937
BT331      MOVE CDC-CALC-DATEX TO NB452-CDC-DATE                        DELLRET5
DL451      MOVE PROCESS-OPT-TYPE TO NB452-PROCESS-OPT-TYPE              DELLRT17
DL451      MOVE SPACES           TO LK-PERSON-MASTER                    DELLRT17
DL442      CALL 'LSNNB452' USING NB452-PASS-AREA                        DELLRT16
DL442                     LK-PERSON-MASTER.                             DELLRT16
DL442                                                                   DELLRT16
AS551      IF NB452-POLICY-FOUND = WS-G
AS551          PERFORM 4800-GAP-TASK-RECORD 
AS552          MOVE 'GAP' TO WK-STATUS
AS552          PERFORM 7000-STATUS-ERROR-WA
AS552          MOVE HOLD-WA-RECORD TO WA-APPLICATION-RECORD
AS551          GO TO 1195-WA-NEXT-READ. 
AS551      IF NB452-POLICY-FOUND = 'S'
AS551          PERFORM 4800-ACT-TASK-RECORD 
AS551          GO TO 1195-WA-NEXT-READ. 
AS551          
AS551      IF NB452-DIR-FLAG = 'Y'
AS551         PERFORM 4800-DIR-TASK-RECORD         
AS551          GO TO 1195-WA-NEXT-READ. 
AS551          
AS551      IF NB452-REV-FLAG = 'Y'
AS551         PERFORM 4800-REV-TASK-RECORD         
AS551          GO TO 1195-WA-NEXT-READ. 
AS551          
DL442      IF WA-FILETYPE = WS-M                                        DELLRT16
DL442         AND SAVE-LS-OVERRIDE-IND = WS-02                          DELLRT16
DL442         AND NB452-POLICY-FOUND = WS-Y                             DELLRT16
DL442         AND NB452-PROCESS-OPT-TYPE = 'NBS'                        DELLRT16
DL442          GO TO 1195-WA-NEXT-READ.                                 DELLRT16
BT641      IF WA-FILETYPE = WS-M
BT641         AND SAVE-LS-OVERRIDE-IND = WS-02
BT641         AND NB452-POLICY-FOUND = WS-N
BT641         AND NB452-PROCESS-OPT-TYPE = 'PHS'
BT641         PERFORM 4500-GET-SY-RECORD
BT641         IF (WASY-LS-PARAM-38 NOT = SPACES)
BT641             AND (WASY-ERROR-CODES-TABLE = SPACES)
BT641             PERFORM 5200-UPDATE-AB-STATUS
BT641             PERFORM 3800-DELETE-AB-RECS
BT641             MOVE 'E74' TO WK-ERROR-CODE
BT641             PERFORM 3700-SY-ERR-MSG
BT641             MOVE WS-Y TO ERROR-SWITCH
BT641             GO TO 1195-WA-NEXT-READ.
DL442      IF WA-FILETYPE = WS-M                                        DELLRT16
DL442         AND SAVE-LS-OVERRIDE-IND = WS-02                          DELLRT16
DL442         AND NB452-POLICY-FOUND = WS-N                             DELLRT16
DL442         AND NB452-PROCESS-OPT-TYPE = 'PHS'                        DELLRT16
DL442          GO TO 1195-WA-NEXT-READ.                                 DELLRT16
BT142          IF NB452-POLICY-FOUND = WS-Y                              0000939
BT142              MOVE NB452-POLICY-NUMBER TO SAVE-PASS-POLICY          0000940
BT214              MOVE NB452-EMPLOYER-ID TO SAVE-EMPLOYER-ID            0000941
BT142              PERFORM 4100-ADDL-TASKS                               0000942
BP931              PERFORM 4111-ADD-POLICY-CV01-WA
BT142              MOVE 'CPO' TO WK-STATUS                               0000943
BT212              PERFORM 7000-STATUS-ERROR-WA                          0000944
BT212              MOVE HOLD-WA-RECORD TO WA-APPLICATION-RECORD.         0000945
DL441      IF WA-FILETYPE = WS-M                                        DELLRT16
DL441         AND SAVE-LS-OVERRIDE-IND = WS-02                          DELLRT16
DL441         AND NB452-POLICY-FOUND = WS-Y                             DELLRT16
DL441          NEXT SENTENCE                                            DELLRT17
DL441      ELSE                                                         DELLRT17
DL441          GO TO 1115-CONTINUE.                                     DELLRT16
DL458      IF NB452-PROCESS-OPT-TYPE = 'PHS'                            DELLRT17
DL458         ADD +1 TO WK-NB452-PHS.                                   DELLRT17
DL442      IF LK-PERSON-MASTER NOT = SPACES                             DELLRT16
DL442 *       CALL 'WRITE8' USING LK-PERSON-MASTER                      DELLRT17
               WRITE WRITE8-REC FROM                                    DELLRT16
                 LK-PERSON-MASTER                                       DELLRT16
               MOVE WS-WRITE8-FS TO CPY-FS-CODE                         DELLRT16
               PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT              DELLRT17
DL457          ADD +1 TO WS-REC-WRITE8.                                 DELLRT17
DL441      MOVE E00-OVERRIDE-BENEFIT-REC TO OVERRIDE-BENEFIT-REC.       DELLRT16
DL454      IF OB-REC-TYP = 'E00U'                                       DELLRT17
DL451         NEXT SENTENCE                                             DELLRT17
DL451      ELSE                                                         DELLRT17
DL451         GO TO 1115-CHECK-D01.                                     DELLRT17
DL451      MOVE NB452-POLICY-NUMBER  TO OB-POLNUM.                      DELLRT17
DL451      MOVE NB452-POLICY-STATUS  TO OB-STATUS.                      DELLRT17
DL454      MOVE 'E00' TO  OB-REC-TYP.                                   DELLRT17
DL451      IF NB452-BASE-LM-FACE-CHANGE = 'Y'                           DELLRT17
DL451          MOVE 'Y'                   TO OB-PREM-BEN-UPDT           DELLRT17
DL451                                        OB-BEN-UPDT                DELLRT17
DL451      ELSE                                                         DELLRT17
DL451          MOVE 'N'                   TO OB-PREM-BEN-UPDT           DELLRT17
DL451                                        OB-BEN-UPDT.               DELLRT17
DL451 *    CALL 'WRITE7' USING OVERRIDE-BENEFIT-REC.                    DELLRT17
           WRITE WRITE7-REC FROM                                        DELLRT17
                 OVERRIDE-BENEFIT-REC                                   DELLRT17
           MOVE WS-WRITE7-FS TO CPY-FS-CODE                             DELLRT17
           PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.                 DELLRT17
DL457      ADD +1 TO WS-REC-WRITE7.                                     DELLRT17
DL451  1115-CHECK-D01.                                                  DELLRT17
DL441      MOVE D01-OVERRIDE-BENEFIT-REC TO OVERRIDE-BENEFIT-REC.       DELLRT16
DL456      IF OB-REC-TYP = 'D01U'                                       DELLRT17
DL451         NEXT SENTENCE                                             DELLRT17
DL451      ELSE                                                         DELLRT17
DL451         GO TO 1115-CHECK-D02.                                     DELLRT17
DL451      MOVE NB452-POLICY-NUMBER  TO OB-POLNUM.                      DELLRT17
DL451      MOVE NB452-POLICY-STATUS  TO OB-STATUS.                      DELLRT17
DL454      MOVE 'D01' TO  OB-REC-TYP.                                   DELLRT17
DL451      IF NB452-AB-BEN-CHANGE-D01-031 = 'Y'                         DELLRT17
DL451          MOVE 'Y'                   TO OB-PREM-BEN-UPDT           DELLRT17
DL451      ELSE                                                         DELLRT17
DL451          MOVE 'N'                   TO OB-PREM-BEN-UPDT.          DELLRT17
DL456      IF (NB452-AB-BEN-CHANGE-D01-031 = 'Y') AND                   DELLRT17
DL456         ((OB-SP-PREM-PCT > SPACES) OR (OB-SP-PREM-AMT > SPACES))  DELLRT17
DL455          MOVE NB452-AB-BEN-CHANGE-D01-031-A                       DELLRT17
DL455                                     TO OB-PREM-BEN-OVR.           DELLRT17
DL451      IF NB452-AB-BEN-CHANGE-D01-036 = 'Y'                         DELLRT17
DL451          MOVE 'Y'                   TO OB-BEN-UPDT                DELLRT17
DL451      ELSE                                                         DELLRT17
DL451          MOVE 'N'                   TO OB-BEN-UPDT.               DELLRT17
DL456      IF (NB452-AB-BEN-CHANGE-D01-036 = 'Y') AND                   DELLRT17
DL456         ((OB-SP-BEN-PCT > SPACES) OR (OB-SP-BEN-AMT > SPACES))    DELLRT17
DL455          MOVE NB452-AB-BEN-CHANGE-D01-036-A                       DELLRT17
DL455                                     TO OB-BEN-AMT-OVR.            DELLRT17
DL451 *    CALL 'WRITE7' USING OVERRIDE-BENEFIT-REC.                    DELLRT17
           WRITE WRITE7-REC FROM                                        DELLRT17
                 OVERRIDE-BENEFIT-REC                                   DELLRT17
           MOVE WS-WRITE7-FS TO CPY-FS-CODE                             DELLRT17
           PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.                 DELLRT17
DL457      ADD +1 TO WS-REC-WRITE7.                                     DELLRT17
DL451  1115-CHECK-D02.                                                  DELLRT17
DL441      MOVE D02-OVERRIDE-BENEFIT-REC TO OVERRIDE-BENEFIT-REC.       DELLRT16
DL457      IF OB-REC-TYP = 'D02U'                                       DELLRT17
DL451         NEXT SENTENCE                                             DELLRT17
DL451      ELSE                                                         DELLRT17
DL451         GO TO 1115-CONTINUE.                                      DELLRT17
DL451      MOVE NB452-POLICY-NUMBER  TO OB-POLNUM.                      DELLRT17
DL451      MOVE NB452-POLICY-STATUS  TO OB-STATUS.                      DELLRT17
DL454      MOVE 'D02' TO  OB-REC-TYP.                                   DELLRT17
DL451      IF NB452-AB-BEN-CHANGE-D02-032 = 'Y'                         DELLRT17
DL451          MOVE 'Y'                   TO OB-PREM-BEN-UPDT           DELLRT17
DL451      ELSE                                                         DELLRT17
DL451          MOVE 'N'                   TO OB-PREM-BEN-UPDT.          DELLRT17
DL456      IF (NB452-AB-BEN-CHANGE-D02-032 = 'Y') AND                   DELLRT17
DL456         ((OB-CH-PREM-PCT > SPACES) OR (OB-CH-PREM-AMT > SPACES))  DELLRT17
DL455          MOVE NB452-AB-BEN-CHANGE-D02-032-A                       DELLRT17
DL455                                     TO OB-PREM-BEN-OVR.           DELLRT17
DL451      IF NB452-AB-BEN-CHANGE-D02-037 = 'Y'                         DELLRT17
DL451          MOVE 'Y'                   TO OB-BEN-UPDT                DELLRT17
DL451      ELSE                                                         DELLRT17
DL451          MOVE 'N'                   TO OB-BEN-UPDT.               DELLRT17
DL456      IF (NB452-AB-BEN-CHANGE-D02-037 = 'Y') AND                   DELLRT17
DL456         ((OB-CH-BEN-PCT > SPACES) OR (OB-CH-BEN-AMT > SPACES))    DELLRT17
DL455          MOVE NB452-AB-BEN-CHANGE-D02-037-A                       DELLRT17
DL455                                     TO OB-BEN-AMT-OVR.            DELLRT17
DL451 *    CALL 'WRITE7' USING OVERRIDE-BENEFIT-REC.                    DELLRT17
           WRITE WRITE7-REC FROM                                        DELLRT17
                 OVERRIDE-BENEFIT-REC                                   DELLRT17
           MOVE WS-WRITE7-FS TO CPY-FS-CODE                             DELLRT17
           PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.                 DELLRT17
DL457      ADD +1 TO WS-REC-WRITE7.                                     DELLRT17
DL441  1115-CONTINUE.                                                   DELLRT16
BT212      IF WA-FILETYPE = WS-M                                         0000946
BT212        AND SAVE-LS-OVERRIDE-IND = WS-02                            0000947
BT212        AND NB452-POLICY-FOUND = WS-Y                               0000948
BT142              IF NB452-TASK-NEEDED = WS-Y                           0000949
BT212                  PERFORM 4800-SET-TASK-RECORD.                     0000950
PB881      IF WA-FILETYPE = WS-M
PB881        AND SAVE-LS-OVERRIDE-IND = WS-02
PB881        AND NB452-POLICY-FOUND = WS-Y  
PB881            IF NB452-EENRL-CLOSE = WS-Y
PB881               PERFORM 9730-SET-TASK-CLOSE.
BT301      IF WA-FILETYPE = WS-M                                        DELLRET2
BT301        AND SAVE-LS-OVERRIDE-IND = WS-02                           DELLRET2
BT301        AND NB452-POLICY-FOUND = WS-Y                              DELLRET2
BT301              IF NB452-DUP-TASK = WS-Y                             DELLRET2
BT301                  PERFORM 4800-DUP-TASK-RECORD.                    DELLRET2
BT345      IF WA-FILETYPE = WS-M                                        DELLRT12
BT345        AND SAVE-LS-OVERRIDE-IND = WS-02                           DELLRT12
BT345        AND NB452-POLICY-FOUND = WS-Y                              DELLRT12
BT345              IF NB452-STS-TASK = WS-Y                             DELLRET6
BT345                  PERFORM 4800-STS-TASK-RECORD.                    DELLRET6
BT345      IF WA-FILETYPE = WS-M                                        DELLRET6
BT345        AND SAVE-LS-OVERRIDE-IND = WS-02                           DELLRET6
BT345        AND NB452-POLICY-FOUND = WS-Y                              DELLRET6
BT345              IF NB452-DTH-TASK = WS-Y                             DELLRET6
BT345                  PERFORM 4800-DTH-TASK-RECORD.                    DELLRET6
BT345      IF WA-FILETYPE = WS-M                                        DELLRET6
BT345        AND SAVE-LS-OVERRIDE-IND = WS-02                           DELLRET6
BT345        AND NB452-POLICY-FOUND = WS-Y                              DELLRET6
BT345              IF NB452-COC-TASK = WS-Y                             DELLRET6
BT345                  PERFORM 4800-COC-TASK-RECORD.                    DELLRET6
AS551      IF WA-FILETYPE = WS-M                                        DELLRET6
AS551        AND SAVE-LS-OVERRIDE-IND = WS-02                           DELLRET6
AS551        AND NB452-POLICY-FOUND = WS-Y                              DELLRET6
AS551              IF NB452-ACT-TASK = WS-Y                             DELLRET6
AS551                  PERFORM 4800-ACT-TASK-RECORD.                    DELLRET6
BT345      IF WA-FILETYPE = WS-M                                        DELLRET6
BT345        AND SAVE-LS-OVERRIDE-IND = WS-02                           DELLRET6
BT345        AND NB452-POLICY-FOUND = WS-Y                              DELLRET6
BT345              IF NB452-LTR-TASK = WS-Y                             DELLRET6
BT345                  PERFORM 4800-LTR-TASK-RECORD.                    DELLRET6
BT371      IF WA-FILETYPE = WS-M                                        DELLRT12
BT371        AND SAVE-LS-OVERRIDE-IND = WS-02                           DELLRT12
BT371        AND NB452-POLICY-FOUND = WS-Y                              DELLRT12
BT371              IF NB452-CNX-TASK = WS-Y                             DELLRET9
BT371                  PERFORM 4800-CNX-TASK-RECORD.                    DELLRET9
BT212      IF WA-FILETYPE = WS-M                                        DELLRET9
BT212        AND SAVE-LS-OVERRIDE-IND = WS-02                           DELLRET9
BT212        AND NB452-POLICY-FOUND = WS-Y                              DELLRET9
BT212              IF NB452-NFU-TASK = WS-Y                              0000954
BT212                  PERFORM 4800-NFU-TASK-RECORD                      0000955
BT212                  GO TO 1195-WA-NEXT-READ                           0000956
BT212              ELSE                                                  0000957
BT212                  GO TO 1195-WA-NEXT-READ.                          0000958
BT142 ***  ***CHECK NEW BUSINESS     *****************************       0000959
BT142      IF WA-FILETYPE = WS-M                                         0000960
BT142        AND SAVE-LS-OVERRIDE-IND = WS-02                            0000961
BT142           MOVE WS-N TO CHECK-NEW-BUSINESS-IND                      0000962
BT142           PERFORM 4600-CHECK-NEW-BUSINESS                          0000963
BT142               IF CHECK-NEW-BUSINESS-IND = WS-Y                     0000964
BT142                   GO TO 1117-CHECK-SWITCH                          0000965
BT142               ELSE                                                 0000966
BT142                   IF ERROR-SWITCH = WS-Y                           0000967
BT142                   GO TO 1195-WA-NEXT-READ.                         0000968
BT142 ***  ***CHECK TRANS CODE IF A CONTINUE *********************       0000969
BT301 ***  ***CHECK PARM 38 IF POPULATED      ********************      DELLRET2
BT142  1115-CHK-TRAN-TYPE-A.                                             0000970
BT142      IF WA-FILETYPE = WS-M                                         0000971
BT142        AND SAVE-LS-OVERRIDE-IND = WS-02                            0000972
BT142          PERFORM 4500-GET-SY-RECORD                                0000973
BT302          IF (WASY-LS-PARAM-38 NOT = SPACES)                       DELLRET2
BT392            OR (SAVE-LS-PARAM-8-1 = 'C')                           DELLRT10
BT701            OR (SAVE-LS-PARAM-8-2R = 'TC')
BT251              PERFORM 5200-UPDATE-AB-STATUS                        DELLRETC
BT142              MOVE 'E74' TO WK-ERROR-CODE                           0000975
BT271              PERFORM 3800-DELETE-AB-RECS                          DELLRETC
BT142              PERFORM 3700-SY-ERR-MSG                               0000976
BT302              MOVE 'Y' TO WK-PARM38                                DELLRET2
BT142              PERFORM 4400-SET-TASK-RECORD                          0000977
BT142              MOVE WS-Y TO ERROR-SWITCH                             0000978
BT142              GO TO 1195-WA-NEXT-READ                               0000979
BT142          ELSE                                                      0000980
BT142              GO TO 1115-TRAN-TYPE-A-CONT.                          0000981
BT142                                                                    0000982
BT142 ***  *******************************************************       0000983
BT142  1115-TRAN-TYPE-A-CONT.                                            0000984
BT142 *                                                                  0000985
BT142      IF (WA-FILETYPE = 'E' AND SAVE-LS-OVERRIDE-IND = '01')        0000986
BT142        OR (WA-FILETYPE = 'M' AND SAVE-LS-OVERRIDE-IND = '02')      0000987
CL131          PERFORM 2100E-PLANCODE-CHECK                              0000988
CL131          IF ERROR-SWITCH = 'Y'                                     0000989
CL131              PERFORM 3800-DELETE-AB-RECS                           0000990
CL131 *  PERFORM REMOVE AB SEGS                                          0000991
CL131              GO TO 1195-WA-NEXT-READ.                              0000992
CL131                                                                    0000993
BT142      IF (WA-FILETYPE = 'E' AND SAVE-LS-OVERRIDE-IND = '01')        0000994
BT142        OR (WA-FILETYPE = 'M' AND SAVE-LS-OVERRIDE-IND = '02')      0000995
CL131          PERFORM 2200E-RETRIEVE-BLK-POLICY                         0000996
CL131          IF ERROR-SWITCH = 'Y'                                     0000997
CL131              PERFORM 3800-DELETE-AB-RECS                           0000998
CL131 *  PERFORM REMOVE AB SEGS                                          0000999
CL131              GO TO 1195-WA-NEXT-READ                               0001000
CL131          ELSE                                                      0001001
CL131              GO TO 1116-BLK-ERR-SWITCH.                            0001002
CL131                                                                    0001003
BT154      IF SAVE-LS-OVERRIDE-IND = WS-03                               0001004
BT154          MOVE SPACES TO WA-PASS-KEY-AREA                           0001005
BT154          MOVE SAVE-PASS-POLICY TO WPK-POLICY                       0001006
BT154          MOVE +1 TO WS-SUB1                                        0001007
BT154          MOVE WTR-FILE-ID (WS-SUB1) TO WPK-FILE-TYPE               0001008
BT154          MOVE WTR-REC-ID (WS-SUB1) TO WPK-RECORD-TYPE              0001009
BT154          MOVE +1 TO WS-SUB2                                        0001010
BT154          MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WPK-KEY-ONE         0001011
BT154          MOVE +0 TO PGM-ERR                                        0001012
BT154          MOVE WS-LSNNB402 TO WPK-PGM-ORIGIN                        0001013
PS251          CALL 'LSNNB547' USING WA-PASS-KEY-AREA                   DELLRETC
BT154                         WA-RECORD-TABLE                            0001015
BT191                         PGM-ERROR                                  0001016
BT191          IF WPK-ERR-CODE > SPACES                                  0001017
BT191             MOVE 'Y' TO ERROR-SWITCH                               0001018
BT191             GO TO 1195-WA-NEXT-READ.                               0001019
BT191                                                                    0001020
BT193                                                                    0001021
BT193      IF SAVE-LS-OVERRIDE-IND = WS-04                               0001022
BT193          MOVE SPACES TO WA-PASS-KEY-AREA                           0001023
BT193          MOVE +1 TO WS-SUB1                                        0001024
BT193          MOVE WTR-FILE-ID (WS-SUB1) TO WPK-FILE-TYPE               0001025
BT193          MOVE WTR-REC-ID (WS-SUB1) TO WPK-RECORD-TYPE              0001026
BT193          MOVE +1 TO WS-SUB2                                        0001027
BT193          MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WPK-KEY-ONE         0001028
BT193          MOVE +0 TO PGM-ERR                                        0001029
BT193          MOVE WS-LSNNB402 TO WPK-PGM-ORIGIN                        0001030
BT193          CALL 'LSNNB453' USING WA-PASS-KEY-AREA                    0001031
BT193                         WA-RECORD-TABLE                            0001032
BT193                         PGM-ERROR.                                 0001033
BT193                                                                    0001034
VC291      IF SAVE-LS-OVERRIDE-IND = 'SS'                               DELLRET2
VC291          MOVE SPACES TO WA-PASS-KEY-AREA                          DELLRET2
VC291          MOVE SAVE-PASS-POLICY TO WPK-POLICY                      DELLRET2
VC291          MOVE +1 TO WS-SUB1                                       DELLRET2
VC291          MOVE WTR-FILE-ID (WS-SUB1) TO WPK-FILE-TYPE              DELLRET2
VC291          MOVE WTR-REC-ID (WS-SUB1) TO WPK-RECORD-TYPE             DELLRET2
VC291          MOVE +1 TO WS-SUB2                                       DELLRET2
VC291          MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WPK-KEY-ONE        DELLRET2
VC291          MOVE +0 TO PGM-ERR                                       DELLRET2
VC291          MOVE WS-LSNNB402 TO WPK-PGM-ORIGIN                       DELLRET2
VC291          CALL 'LSNNB548' USING WA-PASS-KEY-AREA                   DELLRET2
VC291                         WA-RECORD-TABLE                           DELLRET2
VC291                         PGM-ERROR                                 DELLRET2
VC291          IF WPK-ERR-CODE > SPACES                                 DELLRET2
VC291             MOVE 'Y' TO ERROR-SWITCH                              DELLRET2
VC291                GO TO 1195-WA-NEXT-READ.                           DELLRET2
VC291                                                                   DELLRET2
BT231      IF SAVE-PAIND-WASY = WS-G                                     0001035
BT231        AND (SAVE-BLOCK-ID-WASY NOT = SPACES)                       0001036
BT231          PERFORM 2200E-RETRIEVE-BLK-POLICY                         0001037
BT231          GO TO 1116-BLK-ERR-SWITCH.                                0001038
BT231                                                                    0001039
           PERFORM 2200-RETRIEVE-POLICY.                                 0001040
CL131  1116-BLK-ERR-SWITCH.                                              0001041
           IF ERROR-SWITCH = 'Y'                                         0001042
               GO TO 1195-WA-NEXT-READ.                                  0001043
                                                                         0001044
           PERFORM 2300-INITIAL-RECORDS.                                 0001045
CL131  1117-CHECK-SWITCH.                                                0001046
           IF ERROR-SWITCH = 'Y'                                         0001047
               GO TO 1195-WA-NEXT-READ.                                  0001048
                                                                         0001049
BT371 *                                                                 DELLRET9
BT371      IF WA-FILETYPE = WS-M                                        DELLRET9
BT371        AND SAVE-LS-OVERRIDE-IND = WS-02                           DELLRET9
BT392        AND (SAVE-LS-PARAM-8-1 = 'C'                               DELLRT10
BT701             OR SAVE-LS-PARAM-8-2R = 'TC'
BT371             OR (WASY-LS-PARAM-38 NOT = SPACES))                   DELLRET9
BT371          PERFORM 5600-VOLUNTARY-CANCEL.                           DELLRET9
BT371 *                                                                 DELLRET9
BT371      IF ERROR-SWITCH = 'Y'                                        DELLRET9
BT371          GO TO 1195-WA-NEXT-READ.                                 DELLRET9
BT371 *                                                                 DELLRET9
           PERFORM 2400-PROCESS-WA-RECS.                                 0001050
           IF ERROR-SWITCH = 'Y'                                         0001051
               GO TO 1195-WA-NEXT-READ.                                  0001052
                                                                         0001053
DL441      IF WA-FILETYPE = WS-M                                        DELLRT16
DL441        AND SAVE-LS-OVERRIDE-IND = WS-02                           DELLRT16
DL441         NEXT SENTENCE                                             DELLRT16
DL441      ELSE                                                         DELLRT16
DL441         GO TO 1120-CONTINUE.                                      DELLRT16
DL458      IF NB452-PROCESS-OPT-TYPE = 'NBS'                            DELLRT17
DL458         ADD +1 TO WK-NB452-NBS.                                   DELLRT17
DL441      IF OB-HEADER-FIRST-TIME = 'Y'                                DELLRT16
DL441         MOVE 'N' TO OB-HEADER-FIRST-TIME                          DELLRT16
DL441 *       CALL 'WRITE7' USING WB-OVERRIDE-BENEFIT-HEADING           DELLRT17
               WRITE WRITE7-REC FROM                                    DELLRT16
                 WB-OVERRIDE-BENEFIT-HEADING                            DELLRT16
              MOVE WS-WRITE7-FS TO CPY-FS-CODE                          DELLRT16
              PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT               DELLRT16
DL457         ADD +1 TO WS-REC-WRITE7.                                  DELLRT17
DL441      MOVE E00-OVERRIDE-BENEFIT-REC TO OVERRIDE-BENEFIT-REC.       DELLRT16
DL454      IF OB-REC-TYP = 'E00U'                                       DELLRT17
DL454          MOVE 'E00' TO OB-REC-TYP                                 DELLRT17
DL441          MOVE SAVE-NEXT-POLICY     TO OB-POLNUM                   DELLRT16
DL441          MOVE 'Y'                  TO OB-PREM-BEN-UPDT            DELLRT16
DL441                                       OB-BEN-UPDT                 DELLRT16
DL441 *        CALL 'WRITE7' USING OVERRIDE-BENEFIT-REC                 DELLRT17
               WRITE WRITE7-REC FROM                                    DELLRT16
                 OVERRIDE-BENEFIT-REC                                   DELLRT16
               MOVE WS-WRITE7-FS TO CPY-FS-CODE                         DELLRT16
               PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT              DELLRT17
DL457          ADD +1 TO WS-REC-WRITE7.                                 DELLRT17
DL441      MOVE D01-OVERRIDE-BENEFIT-REC TO OVERRIDE-BENEFIT-REC.       DELLRT16
DL454      IF OB-REC-TYP = 'D01U'                                       DELLRT17
DL454          MOVE 'D01' TO OB-REC-TYP                                 DELLRT17
DL441          MOVE SAVE-NEXT-POLICY     TO OB-POLNUM                   DELLRT16
DL441          MOVE 'Y'                  TO OB-PREM-BEN-UPDT            DELLRT16
DL441                                       OB-BEN-UPDT                 DELLRT16
DL441 *        CALL 'WRITE7' USING OVERRIDE-BENEFIT-REC                 DELLRT17
               WRITE WRITE7-REC FROM                                    DELLRT16
                 OVERRIDE-BENEFIT-REC                                   DELLRT16
               MOVE WS-WRITE7-FS TO CPY-FS-CODE                         DELLRT16
               PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT              DELLRT17
DL457          ADD +1 TO WS-REC-WRITE7.                                 DELLRT17
DL441      MOVE D02-OVERRIDE-BENEFIT-REC TO OVERRIDE-BENEFIT-REC.       DELLRT16
DL454      IF OB-REC-TYP = 'D02U'                                       DELLRT17
DL454          MOVE 'D02' TO OB-REC-TYP                                 DELLRT17
DL441          MOVE SAVE-NEXT-POLICY     TO OB-POLNUM                   DELLRT16
DL441          MOVE 'Y'                  TO OB-PREM-BEN-UPDT            DELLRT16
DL441                                       OB-BEN-UPDT                 DELLRT16
DL441 *        CALL 'WRITE7' USING OVERRIDE-BENEFIT-REC                 DELLRT17
               WRITE WRITE7-REC FROM                                    DELLRT16
                 OVERRIDE-BENEFIT-REC                                   DELLRT16
               MOVE WS-WRITE7-FS TO CPY-FS-CODE                         DELLRT16
               PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT              DELLRT16
DL457          ADD +1 TO WS-REC-WRITE7.                                 DELLRT17
DL441                                                                   DELLRT16
DL441  1120-CONTINUE.                                                   DELLRT16
DL441                                                                   DELLRT16
           PERFORM 2500-TASK-RECORD.                                     0001054
BT132                                                                    0001055
BT186      PERFORM 4100-ADDL-TASKS.                                      0001056
AS201      IF WA-FILETYPE = WS-M
AS201        AND SAVE-LS-OVERRIDE-IND = WS-02
AS201        AND SAVE-LS-PARAM-1 = 'LO'
AS201        AND SAVE-REPL-INDICATOR-NA = 'E'
AS201        PERFORM 4800-LTC-TASK-RECORD.
BP931      PERFORM 4111-ADD-POLICY-CV01-WA.
BT186                                                                    0001057
           PERFORM 2800-CREATE-LOG-RECORD.                               0001058
PS134  1195-WA-NEXT-READ.                                                0001059
BT302      MOVE 'N' TO WK-PARM38.                                       DELLRET2
BT111      MOVE 000 TO NUM-AP                                            0001060
BT111               NUM-CV                                               0001061
SS521               NUM-DI
BT111               NUM-T1                                               0001062
BT111               NUM-T2                                               0001063
BT111               NUM-BN                                               0001064
CL131               NUM-AB                                               0001065
BT162               NUM-AC                                               0001066
BT231               NUM-OW                                               0001067
DD281               NUM-OR                                              DELLRET2
BT111               NUM-SY                                               0001068
BT111               NUM-NT                                               0001069
BT111               NUM-NTE                                              0001070
AK131               NUM-PN                                               0001071
AR150               NUM-AL
AR150               NUM-GW
AR150               NUM-IB
NK771               NUM-FM
BT111               NUM-INV.                                             0001072
CL131      MOVE +0   TO AI-RTN-CODE.                                     0001073
CL131      MOVE SPACES TO WA-APPLICATION-RECORD.                         0001074
BT185      MOVE SPACES TO SAVE-EMPLOYER-ID.                              0001075
CL131      MOVE HOLD-AI-W9-KEY TO AI-W9-KEY.                             0001076
           MOVE HOLD-AI-W9-DELL-GUID TO AI-WA-KEY.                      DELLMNCH
      *    PERFORM 8200-READ-NEXT-ALT-KEY.                              DELLMACH
           PERFORM 8210-READ-NEXT-ALT THRU 8299-READ-NEXT-ALT-KEY-EXIT. DELLMACH
BT132      IF RTN-CODE = +12                                             0001078
BT132          DISPLAY ' WA FILE I/O ALT KEY READ  RTN-CODE = +12 '      0001079
BT132          DISPLAY ' ERROR IN 1195-WA-NEXT-READ PARAGRAPH '          0001080
BT132          DISPLAY ' CHECK WA FILE . . . LSNNB402 IS ABENDING '      0001081
BT132          MOVE +1195 TO ABEND-CODE                                  0001082
BT132          PERFORM 9900-ABEND-RTN.                                   0001083
BT132                                                                    0001084
BT132      IF RTN-CODE = +0                                              0001085
BT132          GO TO 1196-READ-NEXT-WA.                                  0001086
BT132                                                                    0001087
BT132      GO TO 1199-PROCESS-ROUTINE-EXIT.                              0001088
BT132  1196-READ-NEXT-WA.                                                0001089
CL131      MOVE SPACES TO WA-APPLICATION-RECORD.                         0001090
CL131                                                                    0001091
CL131      MOVE LOW-VALUES TO WS-KEY.                                    0001092
CL131      MOVE 'WA' TO WS-TRANS-TYPE.                                   0001093
      *    MOVE AI-W9-DELL-GUID TO WS-DELL-GUID.                        DELLMACH
           MOVE AI-W9-DELL-GUID TO WS-KEY(1:40).                        DELLMACH
CL131      MOVE SV-CO TO WS-CO.                                          0001095
CL131      MOVE +0 TO RTN-CODE.                                          0001096
CL131      PERFORM 8300-READ-NEXT-WA-RECS.                               0001097
           IF RTN-CODE = +12                                             0001098
               DISPLAY ' WA FILE I/O ERROR-PROGRAM RTN-CODE = +12 '      0001099
               DISPLAY ' ERROR IN 1195-WA-NEXT-READ PARAGRAPH '          0001100
               DISPLAY ' CHECK WA FILE . . . LSNNB402 IS ABENDING '      0001101
               MOVE +1195 TO ABEND-CODE                                  0001102
               PERFORM 9900-ABEND-RTN.                                   0001103
                                                                         0001104
           IF RTN-CODE = +0                                              0001105
DL457          ADD +1 TO WS-REC-READ                                    DELLRT17
               GO TO 1110-VALIDATION-PROCESS.                            0001106
                                                                         0001107
       1199-PROCESS-ROUTINE-EXIT.                                        0001108
           EXIT.                                                         0001109
AS201  4800-LTC-TASK-RECORD SECTION.
AS201      MOVE SPACES TO TASK-RECORD.
AS201      MOVE 'REPL' TO TK010-PS-PROCESS.
AS201      MOVE 'LTC' TO TK010-PS-STEP.
AS201      PERFORM 3500-GET-WF-DESCRIPTION.
AS201      IF RTN-CODE > +0
AS201          GO TO 4899-LTC-TASK-RECORD-EXIT.
AS201
AS201      MOVE 'REPL' TO TASK-PROCESS.
AS201      MOVE 'LTC' TO TASK-STEP.
AS201      MOVE SV-CO TO TASK-POLICY-CO.
AS201      MOVE SAVE-PASS-POLICY TO TASK-POLICY-NUM.
AS201      MOVE SPACES TO TASK-POLICY-RDR.
AS201      MOVE SAVE-EMPLOYER-ID TO TASK-GROUP-NUMBER.
AS203      MOVE SAVE-REPL-POL-NA TO TASK-COMMENT1-A.
AS201      MOVE SPACES TO TASK-COMMENT2-A.
AS203      MOVE SAVE-COMPANY-NAME TO TASK-COMMENT2-A.
AS203      IF SAVE-COMPANY-NAME = SPACES
AS203          MOVE 'COMPANY NAME NOT FOUND' TO TASK-COMMENT2-A.
AS201      MOVE SPACES TO TASK-COMMENT3-A.
AS201      MOVE '5' TO TASK-PRIORITY.
AS201      MOVE SPACES TO TASK-ADDTO-DATABASE.
AS201      MOVE WS-P TO TASK-STATUS.
AS201      MOVE WS-STPS TO TASK-SENDER-OPID.
AS201
AS201      MOVE WK-SYSTEM-DATE TO TASK-PENDING-DATE.
AS201      MOVE WK-SYSTEM-TIME TO TASK-PENDING-TIME.
AS201      MOVE WK-SYSTEM-DATE TO TASK-ADDED-DATE.
AS201
AS201      MOVE 000000000 TO TASK-ID.
AS201      MOVE TASK-ID TO TASK-MASTER-ID.
AS201      MOVE WK-SYSTEM-DATE TO TASK-MSTR-PDATE.
AS201      MOVE HWA-IMAGE-REFID TO TASK-IMAGE-ID.
AS201      PERFORM 4000-TASK-DISKADD.
AS201
AS201  4899-LTC-TASK-RECORD-EXIT.
AS201      EXIT.
                                                                         0001110
       1200-PROCESS-END  SECTION.                                        0001111
PS134      PERFORM 9200-CLEAR-PROCESS-FLAG.                              0001112
BT132      MOVE SPACES TO COMPANY-OPTION-RECORD.                         0001113
BT132      MOVE SV-CO TO RE-FILE-CO.                                     0001114
BT132      MOVE 'RE' TO RE-FILE-LIT.                                     0001115
BT132      MOVE SPACES TO KEY-94.                                        0001116
BT132      MOVE SV-CO TO CO-94.                                          0001117
BT132      MOVE 94 TO PARM-94.                                           0001118
BT132      MOVE +0 TO RTN-CODE.                                          0001119
BT132      MOVE '9STP' TO LOB-94.                                        0001120
BT132 *    CALL 'DISKREAD' USING COMPANY-OPTION-RECORD                   0001121
BT132 *                   KEY-94                                         0001122
BT132 *                   RE-FILE-ID                                     0001123
BT132 *                   RTN-CODE.                                      0001124
           MOVE KEY-94 TO VSAM-RE-PRIM                                  DELLIDCH
           READ VSAM-RE                                                 DELLIDCH
                    INTO COMPANY-OPTION-RECORD                          DELLIDCH
           MOVE VSAM-RE-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE KEY-94 TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE RE-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKREAD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
BT132      IF RTN-CODE > +0                                              0001125
BT132          GO TO 1230-CLOSE-FILES.                                   0001126
BT132                                                                    0001127
BT132  1210-HOLD-RE-FILE.                                                0001128
BT132 *    CALL 'DISKHOLD' USING COMPANY-OPTION-RECORD                   0001129
BT132 *                   KEY-94                                         0001130
BT132 *                   RE-FILE-ID                                     0001131
BT132 *                   RTN-CODE.                                      0001132
           MOVE KEY-94 TO VSAM-RE-PRIM                                  DELLIDCH
           READ VSAM-RE                                                 DELLIDCH
                    INTO COMPANY-OPTION-RECORD                          DELLIDCH
           MOVE VSAM-RE-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE KEY-94 TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE RE-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
BT132      IF RTN-CODE > +0                                              0001133
BT132          GO TO 1230-CLOSE-FILES.                                   0001134
BT132                                                                    0001135
BT132  1220-UPDATE-RE-FILE.                                              0001136
JJ711     IF PROCESS-RESET  = 'R'
BT132        MOVE '00000001' TO LOB-BLOCK-NEXT-POLICYX-94
JJ711     ELSE
JJ711        GO TO 1230-CLOSE-FILES.
BT132 *    CALL 'DISKUP' USING COMPANY-OPTION-RECORD                     0001138
BT132 *                 KEY-94                                           0001139
BT132 *                 RE-FILE-ID                                       0001140
BT132 *                 RTN-CODE.                                        0001141
           MOVE KEY-94 TO VSAM-RE-PRIM                                  DELLIDCH
           REWRITE VSAM-RE-REC                                          DELLIDCH
                      FROM COMPANY-OPTION-RECORD                        DELLIDCH
           MOVE VSAM-RE-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE KEY-94 TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE RE-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE                       DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
BT132                                                                    0001142
BT132 *    IF (RTN-CODE NOT = +0)                                       DELLMNCH
BT132 *        CALL 'TPFIRLFN' USING RE-FILE-ID                         DELLMNCH
BT132 *                       REL-RTN-CODE.                             DELLMNCH
BT132  1230-CLOSE-FILES.                                                 0001146
      *    CALL 'CLOSEW1'.                                              DELLSQCH
      *    CALL 'CLOSEW2'.                                              DELLSQCH
      *    CALL 'CLOSEW3'.                                              DELLSQCH
SM211 *    CALL 'CLOSEW4'.                                              DELLSQCH
PS401 *    CALL 'CLOSEW6'.                                              DELLRT12
DL441 *    CALL 'CLOSEW7'.                                              DELLRT16
DL442 *    CALL 'CLOSEW8'.                                              DELLRT16
DL457      MOVE WS-REC-READ TO TOT-REC-READ.                            DELLRT17
DL457      DISPLAY ' '.                                                 DELLRT17
DL457      DISPLAY ' RECORDS READ FROM WA MAST FILE : ' TOT-REC-READ.   DELLRT17
DL457      DISPLAY ' '.                                                 DELLRT17
DL457      MOVE WS-REC-WRITE3 TO TOT-REC-DISPLAY.                       DELLRT17
DL457      DISPLAY ' LOG RECORDS WRITTEN W3 : ' TOT-REC-DISPLAY.        DELLRT17
DL457      MOVE WS-REC-WRITE4 TO TOT-REC-DISPLAY.                       DELLRT17
DL457      DISPLAY ' TASK RECORDS WRITTEN W4 : ' TOT-REC-DISPLAY.       DELLRT17
DL457      MOVE WS-REC-WRITE6 TO TOT-REC-DISPLAY.                       DELLRT17
DL457      DISPLAY ' INFILE RECORDS WRITTEN W6 : ' TOT-REC-DISPLAY.     DELLRT17
DL457      MOVE WS-REC-WRITE7 TO TOT-REC-DISPLAY.                       DELLRT17
DL457      DISPLAY ' OVERRIDE RECORDS WRITTEN W7 : ' TOT-REC-DISPLAY.   DELLRT17
DL457      MOVE WS-REC-WRITE8 TO TOT-REC-DISPLAY.                       DELLRT17
DL457      DISPLAY ' PERS MAST RECORDS WRITTEN W8 : ' TOT-REC-DISPLAY.  DELLRT17
DL461      MOVE WS-REC-WRITE9 TO TOT-REC-DISPLAY.
DL461      DISPLAY ' LTR089 RECORDS WRITTEN W9 : ' TOT-REC-DISPLAY.
DL457      DISPLAY ' '.                                                 DELLRT17
DL457      MOVE WK-NB452-RECS-IN TO TOT-REC-DISPLAY.                    DELLRT17
DL457      DISPLAY ' RECORDS SENT TO LSNNB452 : ' TOT-REC-DISPLAY.      DELLRT17
DL457      MOVE WK-NB452-PHS TO TOT-REC-DISPLAY.                        DELLRT17
DL457      DISPLAY ' RECORDS THAT WERE PHS    : ' TOT-REC-DISPLAY.      DELLRT17
DL457      MOVE WK-NB452-NBS TO TOT-REC-DISPLAY.                        DELLRT17
DL457      DISPLAY ' RECORDS THAT WERE NBS    : ' TOT-REC-DISPLAY.      DELLRT17
DL457      DISPLAY ' '.                                                 DELLRT17
       1299-PROCESS-END-EXIT.                                            0001151
           EXIT.                                                         0001152
                                                                         0001153
PS134  1300-LOAD-PARMS  SECTION.                                         0001154
PS134      MOVE SPACES TO PARM02-TABLE-AREA.                             0001155
PS134      MOVE 0 TO PARM01-APP-LIMIT.                                   0001156
PS134      MOVE +0 TO PARM-SUB.                                          0001157
PS134  1310-READ-NEXT-PARM.                                              0001158
PS134 *    CALL 'READ1' USING PARM-RECORD                                0001159
PS134 *                PARM-EOF.                                         0001160
           READ READ1 INTO PARM-RECORD                                  DELLSQCH
                AT END MOVE 'Y' TO PARM-EOF                             DELLSQCH
           END-READ                                                     DELLSQCH
           MOVE WS-READ1-FS TO CPY-FS-CODE                              DELLSQCH
           PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.                 DELLSQCH
PS134      IF PARM-EOF = 'Y'                                             0001161
PS134          GO TO 1375-SET-APP-LIMIT.                                 0001162
PS134 ***********************************************************        0001163
PS134 ***         IGNORE ALL UNEXPECTED PARM TYPES            ***        0001164
PS134 ***********************************************************        0001165
PS134      IF PARM-TYPE NOT = '01' AND '02'                              0001166
PS134          GO TO 1310-READ-NEXT-PARM.                                0001167
PS134      IF PARM-TYPE = '01'                                           0001168
PS134          NEXT SENTENCE                                             0001169
PS134      ELSE                                                          0001170
PS134          GO TO 1330-CHK-PARM02.                                    0001171
PS134      IF PARM-APP-LIMIT-IN NUMERIC                                  0001172
PS134          MOVE PARM-APP-LIMIT-IN TO PARM01-APP-LIMIT                0001173
PS134      ELSE                                                          0001174
PS134          DISPLAY 'PARM 01 NOT NUMERIC -- ABENDING PROGRAM'         0001175
PS134          MOVE +1310 TO ABEND-CODE                                  0001176
PS134          PERFORM 9900-ABEND-RTN.                                   0001177
PS134  1330-CHK-PARM02.                                                  0001178
PS134      IF PARM-TYPE = '02'                                           0001179
PS134          ADD +1 TO PARM-SUB                                        0001180
PS134      ELSE                                                          0001181
PS134          GO TO 1310-READ-NEXT-PARM.                                0001182
PS134      IF PARM-SUB > PARM02-TABLE-MAX                                0001183
PS134          DISPLAY                                                   0001184
PS134      'PARM 02 TABLE MAXIMUM EXCEEDED - ABENDING PROGRAM'           0001184
PS134          MOVE +1330 TO ABEND-CODE                                  0001185
PS134          PERFORM 9900-ABEND-RTN.                                   0001186
PS134      MOVE PARM-VALUE TO PARM02-BYPASS-DELLGUID (PARM-SUB).         0001187
PS134      GO TO 1310-READ-NEXT-PARM.                                    0001188
PS134  1375-SET-APP-LIMIT.                                               0001189
PS134 ***********************************************************        0001190
PS134 ***  IN THE ABSENCE OF A PARM01, LIMIT WILL BE SET TO   ***        0001191
PS134 ***  THE LARGEST VALUE ON THE COUNTER.                  ***        0001192
PS134 ***********************************************************        0001193
PS134      IF PARM01-APP-LIMIT = 0                                       0001194
PS134          MOVE 999999999 TO PARM01-APP-LIMIT.                       0001195
PS134                                                                    0001196
PS134  1399-LOAD-PARMS-EXIT.                                             0001197
PS134      EXIT.                                                         0001198
                                                                         0001199
       2100-VALIDATION-CHECK  SECTION.                                   0001200
                                                                         0001217
           MOVE +0 TO WS-SUB                                             0001218
                   CV-SUB                                                0001219
                   WS-SUB1                                               0001220
                   WS-SUB2.                                              0001221
       2105-TABLE-AP-AP-02.                                              0001222
           MOVE SPACES TO WA-APPLICATION-RECORD.                         0001223
           MOVE +1 TO WS-SUB1.                                           0001224
           IF WTR-FILE-ID (WS-SUB1) = 'AP'                               0001225
             AND WTR-REC-ID (WS-SUB1) = 'AP'                             0001226
               NEXT SENTENCE                                             0001227
           ELSE                                                          0001228
               GO TO 2150-TABLE-AP-CV.                                   0001229
                                                                         0001230
           MOVE +2 TO WS-SUB2.                                           0001231
           MOVE +0 TO RTN-CODE.                                          0001232
           MOVE SPACES TO WA-APPLICATION-RECORD.                         0001233
           MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WA-KEY.                 0001234
           PERFORM 8700-READ-KEY-WA.                                     0001235
           IF RTN-CODE > +0                                              0001236
               DISPLAY                                                   0001237
           'ERROR ON READ OF WA FILE - RTN-CODE = +' RTN-CODE            0001237
               DISPLAY '@ LOCATION 2105-TABLE-AP-AP-02 PARAGRAPH'        0001238
               DISPLAY                                                   0001239
           'KEY PROCESSED IS:  ' WTR-WA-KEY (WS-SUB1, WS-SUB2)           0001239
               DISPLAY 'CHECK WA FILE STATUS... LSNNB402 IS ABENDING '   0001240
               MOVE +2105 TO ABEND-CODE                                  0001241
               PERFORM 9900-ABEND-RTN.                                   0001242
                                                                         0001243
       2110-READ-FOR-WA-PLAN.                                            0001244
           MOVE +0 TO RTN-CODE.                                          0001245
           MOVE SPACES TO PLAN-REC.                                      0001246
           MOVE SV-CO TO PR-PLAN-CO.                                     0001247
           MOVE APAP2-BASE-PLAN-NA TO PR-PLAN.                           0001248
           MOVE APAP2-BASE-PLANOPT-NA TO PR-OPTX.                        0001249
           MOVE WK-SYSTEM-DATE TO PR-EFF-DATE.                           0001250
           MOVE '100' TO PR-TYPE.                                        0001251
           MOVE +0 TO PR-SUBTYPE.                                        0001252
           MOVE 'N' TO ERROR-SWITCH.                                     0001253
           PERFORM 8400-READ-PLAN.                                       0001254
           IF ERROR-SWITCH = 'Y'                                         0001255
               MOVE 'PCE' TO WK-STATUS                                   0001256
               PERFORM 7000-STATUS-ERROR-WA                              0001257
               MOVE '        ' TO SAVE-TASK-POLICY                       0001258
               PERFORM 2600-ERROR-TASKS                                  0001259
               GO TO 2199-VALIDATION-CHECK-EXIT.                         0001260
       2150-TABLE-AP-CV.                                                 0001261
           ADD +1 TO WS-SUB1.                                            0001262
           IF WS-SUB1 > WS-REC-COUNT                                     0001263
               GO TO 2199-VALIDATION-CHECK-EXIT.                         0001264
                                                                         0001265
           IF WTR-KEY (WS-SUB1) = SPACES                                 0001266
               GO TO 2199-VALIDATION-CHECK-EXIT.                         0001267
                                                                         0001268
           IF WTR-FILE-ID (WS-SUB1) = 'AP'                               0001269
             AND WTR-REC-ID (WS-SUB1) = 'CV'                             0001270
               GO TO 2160-CV-PLAN-SETS                                   0001271
           ELSE                                                          0001272
               GO TO 2199-VALIDATION-CHECK-EXIT.                         0001273
                                                                         0001274
           GO TO 2150-TABLE-AP-CV.                                       0001275
       2160-CV-PLAN-SETS.                                                0001276
           MOVE +0 TO CV-SUB.                                            0001277
           MOVE +2 TO WS-SUB2.                                           0001278
       2161-READ-CV-WA-REC.                                              0001279
           IF WS-SUB2 > +3                                               0001280
               GO TO 2150-TABLE-AP-CV.                                   0001281
                                                                         0001282
           MOVE SPACES TO WA-APPLICATION-RECORD.                         0001283
           MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WA-KEY.                 0001284
           PERFORM 8700-READ-KEY-WA.                                     0001285
           IF RTN-CODE > +0                                              0001286
               DISPLAY                                                   0001287
           'ERROR ON READ OF WA FILE - RTN-CODE = +' RTN-CODE            0001287
               DISPLAY '@ LOCATION 2161-READ-CV-WA-REC PARAGRAPH'        0001288
               DISPLAY                                                   0001289
           'KEY PROCESSED IS:  ' WTR-WA-KEY (WS-SUB1, WS-SUB2)           0001289
               DISPLAY 'CHECK WA FILE STATUS... LSNNB402 IS ABENDING '   0001290
               MOVE +2161 TO ABEND-CODE                                  0001291
               PERFORM 9900-ABEND-RTN.                                   0001292
                                                                         0001293
       2162-GET-CV-PLAN-SET-DATA.                                        0001294
           ADD +1 TO CV-SUB.                                             0001295
           IF CV-SUB > +5                                                0001296
               MOVE +0 TO CV-SUB                                         0001297
               ADD +1 TO WS-SUB2                                         0001298
               GO TO 2161-READ-CV-WA-REC.                                0001299
                                                                         0001300
           IF APCV2-PLAN-N1 (CV-SUB) = SPACES                            0001301
BT043          GO TO 2161-READ-CV-WA-REC.                                0001302
BT346      IF WTR-FILE-ID (WS-SUB1) = 'AP'                              DELLRET6
BT346        AND WTR-REC-ID (WS-SUB1) = 'CV'                            DELLRET6
BT346        AND WS-SUB2 = +2                                           DELLRET6
BT346        AND CV-SUB = +1                                            DELLRET6
BT346        AND (APCV2-PLANSBC-N1 (CV-SUB) NOT = SPACES)               DELLRET6
BT346          MOVE 'Y' TO ERROR-SWITCH                                 DELLRET6
BT346          MOVE 'PCE' TO WK-STATUS                                  DELLRET6
BT346          PERFORM 7000-STATUS-ERROR-WA                             DELLRET6
BT346          MOVE '        ' TO SAVE-TASK-POLICY                      DELLRET6
BT346          PERFORM 2600-ERROR-TASKS                                 DELLRET6
BT346          GO TO 2199-VALIDATION-CHECK-EXIT.                        DELLRET6
                                                                         0001303
           MOVE +0 TO RTN-CODE.                                          0001304
           MOVE SPACES TO PLAN-REC.                                      0001305
           MOVE SV-CO TO PR-PLAN-CO.                                     0001306
           MOVE APCV2-PLAN-N1 (CV-SUB) TO PR-PLAN.                       0001307
           MOVE APCV2-PLANOPT-N1 (CV-SUB) TO PR-OPTX.                    0001308
           MOVE WK-SYSTEM-DATE TO PR-EFF-DATE.                           0001309
           MOVE '100' TO PR-TYPE.                                        0001310
           MOVE +0 TO PR-SUBTYPE.                                        0001311
           MOVE 'N' TO ERROR-SWITCH.                                     0001312
           PERFORM 8400-READ-PLAN.                                       0001313
           IF ERROR-SWITCH = 'Y'                                         0001314
               MOVE 'PCE' TO WK-STATUS                                   0001315
               PERFORM 7000-STATUS-ERROR-WA                              0001316
               MOVE '        ' TO SAVE-TASK-POLICY                       0001317
               PERFORM 2600-ERROR-TASKS                                  0001318
               GO TO 2199-VALIDATION-CHECK-EXIT.                         0001319
                                                                         0001320
           GO TO 2162-GET-CV-PLAN-SET-DATA.                              0001321
       2199-VALIDATION-CHECK-EXIT.                                       0001322
           EXIT.                                                         0001323
                                                                         0001324
CL131  2100E-VALIDATION-CHECK  SECTION.                                  0001325
CL131 * UPDATE GU FILE WITH NEW DELL GUID                                0001341
CL131                                                                    0001342
CL131      PERFORM 9000-READ-GU.                                         0001343
CL131      IF RTN-CODE = +0                                              0001344
CL131          GO TO 2150E-HOLD-GU-FILE.                                 0001345
CL131                                                                    0001346
CL131      IF RTN-CODE > +0                                              0001347
CL131          MOVE '002' TO WPK-ERR-CODE                                0001348
CL131          GO TO 2199E-VALIDATION-CHECK-EXIT.                        0001349
CL131                                                                    0001350
CL131  2150E-HOLD-GU-FILE.                                               0001351
BT142      IF WA-FILETYPE = WS-M                                         0001352
BT142          GO TO 2199E-VALIDATION-CHECK-EXIT.                        0001353
CL131      IF HWA-DELL-GUID = GU-DELL-GUID                               0001354
CL131          GO TO 2199E-VALIDATION-CHECK-EXIT.                        0001355
CL131                                                                    0001356
CL131 *    CALL 'DISKHOLD' USING GU-GUID-RECORD                          0001357
CL131 *                   GU-KEY                                         0001358
CL131 *                   GU-FILE-ID                                     0001359
CL131 *                   RTN-CODE.                                      0001360
           MOVE GU-KEY TO VSAM-GU-PRIM                                  DELLIDCH
           READ VSAM-GU                                                 DELLIDCH
                    INTO GU-GUID-RECORD                                 DELLIDCH
           MOVE VSAM-GU-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE GU-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE GU-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
CL131                                                                    0001361
CL131      IF RTN-CODE > +0                                              0001362
CL131          MOVE '002' TO WPK-ERR-CODE                                0001363
CL131          GO TO 2199E-VALIDATION-CHECK-EXIT.                        0001364
CL131                                                                    0001365
CL131      MOVE HWA-DELL-GUID TO GU-DELL-GUID.                           0001366
CL131  2160E-UPDATE-GU-FILE.                                             0001367
CL131 *    CALL 'DISKUP' USING GU-GUID-RECORD                            0001368
CL131 *                 GU-KEY                                           0001369
CL131 *                 GU-FILE-ID                                       0001370
CL131 *                 RTN-CODE.                                        0001371
           MOVE GU-KEY TO VSAM-GU-PRIM                                  DELLIDCH
           REWRITE VSAM-GU-REC                                          DELLIDCH
                      FROM GU-GUID-RECORD                               DELLIDCH
           MOVE VSAM-GU-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE GU-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE GU-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE                       DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
CL131                                                                    0001372
CL131      IF (RTN-CODE NOT = +0)                                        0001373
CL131 *        CALL 'TPFIRLFN' USING GU-FILE-ID                         DELLMNCH
CL131 *                       REL-RTN-CODE                              DELLMNCH
CL131          MOVE '002' TO WPK-ERR-CODE.                               0001376
CL131                                                                    0001377
CL131  2199E-VALIDATION-CHECK-EXIT.                                      0001378
CL131      EXIT.                                                         0001379
CL131                                                                    0001380
CL131  2100E-PLANCODE-CHECK  SECTION.                                    0001381
CL131      MOVE WA-APPLICATION-RECORD TO HOLD-WA-RECORD.                 0001382
CL131      MOVE +0 TO WS-SUB                                             0001383
CL131              CV-SUB                                                0001384
CL131              WS-SUB1                                               0001385
CL131              WS-SUB2                                               0001386
CL131              WS-REC-COUNT                                          0001387
CL131              WS-SY-COUNT                                           0001388
CL131              WS-CV-COUNT                                           0001389
CL131              WS-AP-COUNT.                                          0001390
CL131      MOVE 'N' TO ERROR-SWITCH.                                     0001391
CL131                                                                    0001392
CL131  2105E-TABLE-AP-AP-02.                                             0001393
CL131      MOVE SPACES TO WA-APPLICATION-RECORD.                         0001394
CL131      MOVE +1 TO WS-SUB1.                                           0001395
CL131      IF WTR-FILE-ID (WS-SUB1) = 'AP'                               0001396
CL131        AND WTR-REC-ID (WS-SUB1) = 'AP'                             0001397
CL131          NEXT SENTENCE                                             0001398
CL131      ELSE                                                          0001399
CL131          GO TO 2150E-TABLE-AP-CV.                                  0001400
AS203      MOVE +1 TO WS-SUB2.
AS203      MOVE +0 TO RTN-CODE.
AS203      MOVE SPACES TO WA-APPLICATION-RECORD.
AS203      MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WA-KEY.
AS203      PERFORM 8700-READ-KEY-WA.
AS203      IF RTN-CODE > +0
AS203          MOVE '001' TO WPK-ERR-CODE
AS203          GO TO 2199E-PLANCODE-CHECK-EXIT.
AS203      MOVE APAP1-REPL-POL-NA TO SAVE-REPL-POL-NA.
CL131                                                                    0001401
CL131      MOVE +2 TO WS-SUB2.                                           0001402
CL131      MOVE +0 TO RTN-CODE.                                          0001403
CL131      MOVE SPACES TO WA-APPLICATION-RECORD.                         0001404
CL131      MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WA-KEY.                 0001405
CL131      PERFORM 8700-READ-KEY-WA.                                     0001406
CL131      IF RTN-CODE > +0                                              0001407
CL131          MOVE '001' TO WPK-ERR-CODE                                0001408
CL131          GO TO 2199E-PLANCODE-CHECK-EXIT.                          0001409
AS201      MOVE APAP2-REPL-INDICATOR-NA TO SAVE-REPL-INDICATOR-NA.
AS203      MOVE APAP2-COMPANY-NAME TO SAVE-COMPANY-NAME.
CL131                                                                    0001410
CL131  2110E-READ-FOR-WA-PLAN.                                           0001411
CL131      MOVE +0 TO RTN-CODE.                                          0001412
CL131      MOVE SPACES TO PLAN-REC.                                      0001413
CL131      MOVE SV-CO TO PR-PLAN-CO.                                     0001414
CL131      MOVE APAP2-BASE-PLAN-NA TO PR-PLAN.                           0001415
CL131      MOVE APAP2-BASE-PLANOPT-NA TO PR-OPTX.                        0001416
CL131      MOVE WK-SYSTEM-DATE TO PR-EFF-DATE.                           0001417
CL131      MOVE '100' TO PR-TYPE.                                        0001418
CL131      MOVE +0 TO PR-SUBTYPE.                                        0001419
CL131      MOVE 'N' TO ERROR-SWITCH.                                     0001420
CL131      PERFORM 8400-READ-PLAN.                                       0001421
CL131      IF ERROR-SWITCH = 'Y'                                         0001422
CL131          GO TO 2199E-PLANCODE-CHECK-EXIT.                          0001423
CL131  2150E-TABLE-AP-CV.                                                0001424
CL131      ADD +1 TO WS-SUB1.                                            0001425
CL131      IF WS-SUB1 > WS-REC-COUNT                                     0001426
CL131          GO TO 2199E-PLANCODE-CHECK-EXIT.                          0001427
CL131                                                                    0001428
CL131      IF WTR-KEY (WS-SUB1) = SPACES                                 0001429
CL131          GO TO 2199E-PLANCODE-CHECK-EXIT.                          0001430
CL131                                                                    0001431
CL131      IF WTR-FILE-ID (WS-SUB1) = 'AP'                               0001432
CL131        AND WTR-REC-ID (WS-SUB1) = 'CV'                             0001433
CL131          GO TO 2160E-CV-PLAN-SETS                                  0001434
CL131      ELSE                                                          0001435
CL131          GO TO 2199E-PLANCODE-CHECK-EXIT.                          0001436
CL131                                                                    0001437
CL131      GO TO 2150E-TABLE-AP-CV.                                      0001438
CL131  2160E-CV-PLAN-SETS.                                               0001439
CL131      MOVE +0 TO CV-SUB.                                            0001440
CL131      MOVE +2 TO WS-SUB2.                                           0001441
CL131  2161E-READ-CV-WA-REC.                                             0001442
CL131      IF WS-SUB2 > +3                                               0001443
CL131          GO TO 2150E-TABLE-AP-CV.                                  0001444
CL131                                                                    0001445
CL131      MOVE SPACES TO WA-APPLICATION-RECORD.                         0001446
CL131      MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WA-KEY.                 0001447
CL131      PERFORM 8700-READ-KEY-WA.                                     0001448
CL131      IF RTN-CODE > +0                                              0001449
CL131          MOVE '001' TO WPK-ERR-CODE                                0001450
CL131          GO TO 2199E-PLANCODE-CHECK-EXIT.                          0001451
CL131                                                                    0001452
CL131  2162E-GET-CV-PLAN-SET-DATA.                                       0001453
CL131      ADD +1 TO CV-SUB.                                             0001454
CL131      IF CV-SUB > +5                                                0001455
CL131          MOVE +0 TO CV-SUB                                         0001456
CL131          ADD +1 TO WS-SUB2                                         0001457
CL131          GO TO 2161E-READ-CV-WA-REC.                               0001458
CL131                                                                    0001459
CL131      IF APCV2-PLAN-N1 (CV-SUB) = SPACES                            0001460
CL131          GO TO 2161E-READ-CV-WA-REC.                               0001461
CL131                                                                    0001462
CL131      MOVE +0 TO RTN-CODE.                                          0001463
CL131      MOVE SPACES TO PLAN-REC.                                      0001464
CL131      MOVE SV-CO TO PR-PLAN-CO.                                     0001465
CL131      MOVE APCV2-PLAN-N1 (CV-SUB) TO PR-PLAN.                       0001466
CL131      MOVE APCV2-PLANOPT-N1 (CV-SUB) TO PR-OPTX.                    0001467
CL131      MOVE WK-SYSTEM-DATE TO PR-EFF-DATE.                           0001468
CL131      MOVE '100' TO PR-TYPE.                                        0001469
CL131      MOVE +0 TO PR-SUBTYPE.                                        0001470
CL131      MOVE 'N' TO ERROR-SWITCH.                                     0001471
CL131      PERFORM 8400-READ-PLAN.                                       0001472
CL131      IF ERROR-SWITCH = 'Y'                                         0001473
CL131          GO TO 2199E-PLANCODE-CHECK-EXIT.                          0001474
CL131                                                                    0001475
CL131      GO TO 2162E-GET-CV-PLAN-SET-DATA.                             0001476
CL131  2199E-PLANCODE-CHECK-EXIT.                                        0001477
CL131      EXIT.                                                         0001478
CL131                                                                    0001479
       2200-RETRIEVE-POLICY  SECTION.                                    0001480
KM101      MOVE 'N' TO ERROR-SWITCH.                                     0001481
KM101      MOVE SPACES TO SAVE-PASS-POLICY.                              0001482
KM101      IF (SAVE-PAIND-WASY NOT = SPACES AND 'P')                     0001483
KM101          GO TO 2205-POLICY-ERROR.                                  0001484
KM101      IF SAVE-PAIND-WASY = SPACES                                   0001485
KM101          GO TO 2219-PROCESS-RE-FILE.                               0001486
KM101      IF SAVE-PAIND-WASY = 'P'                                      0001487
KM101        AND SAVE-PAPOL-WASY = SPACES                                0001488
KM101          GO TO 2205-POLICY-ERROR.                                  0001489
BT154      IF (SAVE-LS-OVERRIDE-IND NOT = WS-03)                         0001490
BT154          GO TO 2204-CHECK-POLICY.                                  0001491
BT154      MOVE +0 TO WS-SUB1.                                           0001492
BT154      MOVE +1 TO WS-SUB2.                                           0001493
BT154  2201-LOCATE-SY-LOOP.                                              0001494
BT154      ADD +1 TO WS-SUB1.                                            0001495
BT154      IF WS-SUB1 > WTR-TABLE-MAX                                    0001496
BT154          GO TO 2204-CHECK-POLICY.                                  0001497
BT154                                                                    0001498
BT154      IF WTR-FILE-ID (WS-SUB1) = 'WA'                               0001499
BT154        AND WTR-REC-ID (WS-SUB1) = 'SY'                             0001500
BT154          GO TO 2202-READ-SY-REC.                                   0001501
BT154                                                                    0001502
BT154      GO TO 2201-LOCATE-SY-LOOP.                                    0001503
BT154  2202-READ-SY-REC.                                                 0001504
BT154      MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WA-KEY.                 0001505
BT154      PERFORM 8700-READ-KEY-WA.                                     0001506
BT154      IF (RTN-CODE NOT = +0)                                        0001507
BT154          GO TO 2204-CHECK-POLICY.                                  0001508
BT154      MOVE WASY-FINAL-STATUS TO WK-FINAL-STATUS.                    0001509
BT154      IF (WK-FINAL-STATUS NOT = WS-APPROVE AND WS-DECLINE)          0001510
BT154          GO TO 2204-CHECK-POLICY.                                  0001511
BT154      MOVE SPACES TO COMPANY-OPTION-RECORD.                         0001512
BT154      MOVE SV-CO TO RE-FILE-CO.                                     0001513
BT154      MOVE 'RE' TO RE-FILE-LIT.                                     0001514
BT154      MOVE SPACES TO KEY-94.                                        0001515
BT154      MOVE SV-CO TO CO-94.                                          0001516
BT154      MOVE 94 TO PARM-94.                                           0001517
BT154      MOVE +0 TO RTN-CODE.                                          0001518
BT154      MOVE '9STP' TO LOB-94.                                        0001519
BT154 *    CALL 'DISKREAD' USING COMPANY-OPTION-RECORD                   0001520
BT154 *                   KEY-94                                         0001521
BT154 *                   RE-FILE-ID                                     0001522
BT154 *                   RTN-CODE.                                      0001523
           MOVE KEY-94 TO VSAM-RE-PRIM                                  DELLIDCH
           READ VSAM-RE                                                 DELLIDCH
                    INTO COMPANY-OPTION-RECORD                          DELLIDCH
           MOVE VSAM-RE-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE KEY-94 TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE RE-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKREAD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
BT154      IF RTN-CODE = +12                                             0001524
BT154          MOVE 'E49' TO WK-ERROR-CODE                               0001525
BT154          PERFORM 3700-SY-ERR-MSG                                   0001526
BT154          MOVE 'Y' TO ERROR-SWITCH                                  0001527
BT154          GO TO 2299-RETRIEVE-POLICY-EXIT.                          0001528
BT154      IF RTN-CODE > +0                                              0001529
BT154          MOVE 'E49' TO WK-ERROR-CODE                               0001530
BT154          PERFORM 3700-SY-ERR-MSG                                   0001531
BT154          MOVE 'Y' TO ERROR-SWITCH                                  0001532
BT154          GO TO 2299-RETRIEVE-POLICY-EXIT.                          0001533
BT154                                                                    0001534
BT361      IF WA-FILETYPE = WS-M                                        DELLRET8
BT361        AND SAVE-LS-OVERRIDE-IND = WS-02                           DELLRET8
BT361        AND LOB-BLOCK-NEXT-POLICY-94 > WK-BRIDGE-LIMIT-02          DELLRET8
BT361          MOVE 'E50' TO WK-ERROR-CODE                              DELLRET8
BT361          PERFORM 3700-SY-ERR-MSG                                  DELLRET8
BT361          MOVE 'Y' TO ERROR-SWITCH                                 DELLRET8
BT361          GO TO 2299-RETRIEVE-POLICY-EXIT.                         DELLRET8
BT361                                                                   DELLRET8
BT361      IF WA-FILETYPE = WS-M                                        DELLRET8
BT361        AND SAVE-LS-OVERRIDE-IND = WS-02                           DELLRET8
BT361          GO TO 2204-CHECK-POLICY.                                 DELLRET8
BT361                                                                   DELLRET8
BT154      IF LOB-BLOCK-NEXT-POLICY-94 > WK-BRIDGE-LIMIT                 0001535
BT154          MOVE 'E50' TO WK-ERROR-CODE                               0001536
BT154          PERFORM 3700-SY-ERR-MSG                                   0001537
BT154          MOVE 'Y' TO ERROR-SWITCH                                  0001538
BT154          GO TO 2299-RETRIEVE-POLICY-EXIT.                          0001539
BT154                                                                    0001540
VC291      IF LOB-BLOCK-NEXT-POLICY-94 > WK-BRIDGE-LIMIT-SS             DELLRET2
VC291          MOVE 'E50' TO WK-ERROR-CODE                              DELLRET2
VC291          PERFORM 3700-SY-ERR-MSG                                  DELLRET2
VC291          MOVE 'Y' TO ERROR-SWITCH                                 DELLRET2
VC291          GO TO 2299-RETRIEVE-POLICY-EXIT.                         DELLRET2
VC291                                                                   DELLRET2
BT154  2204-CHECK-POLICY.                                                0001541
KM101      PERFORM 3300-ASSIGNED-POLICY.                                 0001542
KM101      IF ERROR-SWITCH = 'N'                                         0001543
KM101         GO TO 2240-CHECK-GU-FILE.                                  0001544
KM101      MOVE 'DUP' TO WK-STATUS.                                      0001545
KM101      PERFORM 7000-STATUS-ERROR-WA.                                 0001546
KM101      MOVE '        ' TO SAVE-TASK-POLICY.                          0001547
KM101      PERFORM 2600-ERROR-TASKS.                                     0001548
KM101      MOVE 'Y' TO ERROR-SWITCH.                                     0001549
KM101      GO TO 2299-RETRIEVE-POLICY-EXIT.                              0001550
KM101  2205-POLICY-ERROR.                                                0001551
KM101      MOVE 'UNK' TO WK-STATUS.                                      0001552
KM101      PERFORM 7000-STATUS-ERROR-WA.                                 0001553
KM101      MOVE '        ' TO SAVE-TASK-POLICY.                          0001554
KM101      PERFORM 2600-ERROR-TASKS.                                     0001555
KM101      MOVE 'Y' TO ERROR-SWITCH.                                     0001556
KM101      GO TO 2299-RETRIEVE-POLICY-EXIT.                              0001557
       2219-PROCESS-RE-FILE.                                             0001558
           MOVE 'N' TO ERROR-SWITCH.                                     0001559
      *--------------------------------------------------*               0001560
      *  READ COMPANY REFERENCE FILE FOR NEXT POLICY NO. *               0001561
      *--------------------------------------------------*               0001562
           MOVE SPACES TO SAVE-PASS-POLICY.                              0001563
           MOVE SV-CO TO RE-FILE-CO.                                     0001564
           MOVE 'RE' TO RE-FILE-LIT.                                     0001565
           MOVE SPACES TO KEY-C.                                         0001566
           MOVE SV-CO TO CO-C.                                           0001567
           MOVE 0 TO PARM-C.                                             0001568
      *    CALL 'DISKREAD' USING COMPANY-OPTION-RECORD                   0001569
      *                   KEY-C                                          0001570
      *                   RE-FILE-ID                                     0001571
      *                   RTN-CODE.                                      0001572
           MOVE KEY-C TO VSAM-RE-PRIM                                   DELLIDCH
           READ VSAM-RE                                                 DELLIDCH
                    INTO COMPANY-OPTION-RECORD                          DELLIDCH
           MOVE VSAM-RE-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE KEY-C TO TPSWNML-FILE-KEY                               DELLIDCH
           MOVE RE-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKREAD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
           IF RTN-CODE > +0                                              0001573
               DISPLAY 'ERROR ON READ OF RE FILE, RTN-CODE = +' RTN-CODE 0001574
               DISPLAY 'LOCATION IS 2200-RETRIEVE-POLICY SECTION '       0001575
               DISPLAY ' LSNNB402 IS ABENDING... CHECK RE FILE STATUS'   0001576
               MOVE +2200 TO ABEND-CODE                                  0001577
               PERFORM 9900-ABEND-RTN.                                   0001578
                                                                         0001579
           PERFORM 3000-VALID-POLICY-NUMBER.                             0001580
                                                                         0001581
       2220-HOLD-RE-FILE.                                                0001582
      *    CALL 'DISKHOLD' USING COMPANY-OPTION-RECORD                   0001583
      *                   KEY-C                                          0001584
      *                   RE-FILE-ID                                     0001585
      *                   RTN-CODE.                                      0001586
           MOVE KEY-C TO VSAM-RE-PRIM                                   DELLIDCH
           READ VSAM-RE                                                 DELLIDCH
                    INTO COMPANY-OPTION-RECORD                          DELLIDCH
           MOVE VSAM-RE-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE KEY-C TO TPSWNML-FILE-KEY                               DELLIDCH
           MOVE RE-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
           IF RTN-CODE = +0                                              0001587
               GO TO 2230-UPDATE-RE-FILE.                                0001588
                                                                         0001589
           IF RTN-CODE = +12                                             0001590
               DISPLAY 'ERROR DISKHOLD RE FILE, RTN-CODE = +' RTN-CODE   0001591
               DISPLAY 'LOCATION IS 2220-HOLD-RE-FILE PARAGRAPH '        0001592
               DISPLAY ' LSNNB402 IS ABENDING... CHECK RE FILE STATUS'   0001593
               MOVE +2220 TO ABEND-CODE                                  0001594
               PERFORM 9900-ABEND-RTN.                                   0001595
                                                                         0001596
           MOVE 'UNK' TO WK-STATUS.                                      0001597
           PERFORM 7000-STATUS-ERROR-WA.                                 0001598
           MOVE '        ' TO SAVE-TASK-POLICY.                          0001599
           PERFORM 2600-ERROR-TASKS.                                     0001600
BT043      MOVE 'Y' TO ERROR-SWITCH.                                     0001601
           GO TO 2299-RETRIEVE-POLICY-EXIT.                              0001602
       2230-UPDATE-RE-FILE.                                              0001603
           MOVE SAVE-NEW-POLICY TO POLICYX-C.                            0001604
      *    CALL 'DISKUP' USING COMPANY-OPTION-RECORD                     0001605
      *                 KEY-C                                            0001606
      *                 RE-FILE-ID                                       0001607
      *                 RTN-CODE.                                        0001608
           MOVE KEY-C TO VSAM-RE-PRIM                                   DELLIDCH
           REWRITE VSAM-RE-REC                                          DELLIDCH
                      FROM COMPANY-OPTION-RECORD                        DELLIDCH
           MOVE VSAM-RE-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE KEY-C TO TPSWNML-FILE-KEY                               DELLIDCH
           MOVE RE-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE                       DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
           IF RTN-CODE = +12                                             0001609
               DISPLAY 'ERROR DISKUP RE FILE, RTN-CODE = +12 '           0001610
               DISPLAY 'LOCATION IS 2230-UPDATE-RE-FILE PARAGRAPH '      0001611
               DISPLAY ' LSNNB402 IS ABENDING... CHECK RE FILE STATUS'   0001612
               MOVE +2230 TO ABEND-CODE                                  0001613
               PERFORM 9900-ABEND-RTN.                                   0001614
                                                                         0001615
           IF RTN-CODE = +0                                              0001616
               GO TO 2240-CHECK-GU-FILE.                                 0001617
                                                                         0001618
           IF (RTN-CODE NOT = +0)                                        0001619
      *        CALL 'TPFIRLFN' USING RE-FILE-ID                         DELLMNCH
      *                       REL-RTN-CODE                              DELLMNCH
               DISPLAY 'ERROR DISKUP RE FILE, RTN-CODE = +' RTN-CODE     0001622
               DISPLAY 'LOCATION IS 2230-UPDATE-RE-FILE PARAGRAPH '      0001623
               DISPLAY ' LSNNB402 IS ABENDING... CHECK RE FILE STATUS'   0001624
               MOVE +2230 TO ABEND-CODE                                  0001625
               PERFORM 9900-ABEND-RTN.                                   0001626
                                                                         0001627
       2240-CHECK-GU-FILE.                                               0001628
           MOVE +0 TO RTN-CODE.                                          0001629
           IF HWA-CLIENT-GUID = GU-CLIENT-GUID                           0001630
               GO TO 2250-HOLD-GU-FILE.                                  0001631
                                                                         0001632
           PERFORM 9000-READ-GU.                                         0001633
           IF RTN-CODE = +0                                              0001634
               GO TO 2250-HOLD-GU-FILE.                                  0001635
                                                                         0001636
           IF RTN-CODE > +0                                              0001637
               DISPLAY 'ERROR ON READ OF GU FILE, RTN-CODE = +' RTN-CODE 0001638
               DISPLAY 'LOCATION IS 2240-CHECK-GU-FILE PARAGRAPH '       0001639
               DISPLAY 'PROCESSING CLIENT GUID = ' HWA-CLIENT-GUID       0001640
               DISPLAY 'LSNNB402 IS ABENDING... CHECK GU FILE STATUS '   0001641
               MOVE +2240 TO ABEND-CODE                                  0001642
               PERFORM 9900-ABEND-RTN.                                   0001643
                                                                         0001644
       2250-HOLD-GU-FILE.                                                0001645
      *    CALL 'DISKHOLD' USING GU-GUID-RECORD                          0001646
      *                   GU-KEY                                         0001647
      *                   GU-FILE-ID                                     0001648
      *                   RTN-CODE.                                      0001649
           MOVE GU-KEY TO VSAM-GU-PRIM                                  DELLIDCH
           READ VSAM-GU                                                 DELLIDCH
                    INTO GU-GUID-RECORD                                 DELLIDCH
           MOVE VSAM-GU-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE GU-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE GU-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
                                                                         0001650
           IF RTN-CODE > +0                                              0001651
               DISPLAY 'ERROR DISKHOLD GU FILE, RTN-CODE = +' RTN-CODE   0001652
               DISPLAY 'LOCATION IS 2250-HOLD-GU-FILE PARAGRAPH '        0001653
               DISPLAY 'LSNNB402 IS ABENDING... CHECK GU FILE STATUS '   0001654
               MOVE +2250 TO ABEND-CODE                                  0001655
               PERFORM 9900-ABEND-RTN.                                   0001656
                                                                         0001657
           MOVE SAVE-NEXT-POLICY TO GU-POLICY                            0001658
                                 SAVE-PASS-POLICY.                       0001659
       2260-UPDATE-GU-FILE.                                              0001660
      *    CALL 'DISKUP' USING GU-GUID-RECORD                            0001661
      *                 GU-KEY                                           0001662
      *                 GU-FILE-ID                                       0001663
      *                 RTN-CODE.                                        0001664
           MOVE GU-KEY TO VSAM-GU-PRIM                                  DELLIDCH
           REWRITE VSAM-GU-REC                                          DELLIDCH
                      FROM GU-GUID-RECORD                               DELLIDCH
           MOVE VSAM-GU-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE GU-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE GU-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE                       DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
                                                                         0001665
           IF (RTN-CODE NOT = +0)                                        0001666
      *        CALL 'TPFIRLFN' USING GU-FILE-ID                         DELLMNCH
      *                       REL-RTN-CODE                              DELLMNCH
               DISPLAY 'ERROR DISKUP GU FILE, RTN-CODE = +' RTN-CODE     0001669
               DISPLAY 'LOCATION IS 2260-UPDATE-GU-FILE PARAGRAPH '      0001670
               DISPLAY 'LSNNB402 IS ABENDING... CHECK GU FILE STATUS '   0001671
               MOVE +2260 TO ABEND-CODE                                  0001672
               PERFORM 9900-ABEND-RTN.                                   0001673
                                                                         0001674
       2299-RETRIEVE-POLICY-EXIT.                                        0001675
           EXIT.                                                         0001676
                                                                         0001677
CL131  2200E-RETRIEVE-BLK-POLICY  SECTION.                               0001678
CL131      MOVE 'N' TO ERROR-SWITCH.                                     0001679
CL131      MOVE SPACES TO COMPANY-OPTION-RECORD.                         0001680
CL131      MOVE SV-CO TO RE-FILE-CO.                                     0001681
CL131      MOVE 'RE' TO RE-FILE-LIT.                                     0001682
CL131      MOVE SPACES TO KEY-94.                                        0001683
CL131      MOVE SV-CO TO CO-94.                                          0001684
CL131      MOVE 94 TO PARM-94.                                           0001685
CL131      MOVE +0 TO RTN-CODE.                                          0001686
CL131      MOVE '9STP' TO LOB-94.                                        0001687
CL131 *    CALL 'DISKREAD' USING COMPANY-OPTION-RECORD                   0001688
CL131 *                   KEY-94                                         0001689
CL131 *                   RE-FILE-ID                                     0001690
CL131 *                   RTN-CODE.                                      0001691
           MOVE KEY-94 TO VSAM-RE-PRIM                                  DELLIDCH
           READ VSAM-RE                                                 DELLIDCH
                    INTO COMPANY-OPTION-RECORD                          DELLIDCH
           MOVE VSAM-RE-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE KEY-94 TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE RE-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKREAD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
CL131      IF RTN-CODE = +12                                             0001692
CL131          MOVE 'E41' TO WK-ERROR-CODE                               0001693
CL131          PERFORM 3700-SY-ERR-MSG                                   0001694
CL131          MOVE 'Y' TO ERROR-SWITCH                                  0001695
CL131          GO TO 2299E-RET-BLK-POLICY-EXIT.                          0001696
CL131      IF RTN-CODE > +0                                              0001697
CL131          MOVE 'E41' TO WK-ERROR-CODE                               0001698
CL131          PERFORM 3700-SY-ERR-MSG                                   0001699
CL131          MOVE 'Y' TO ERROR-SWITCH                                  0001700
CL131          GO TO 2299E-RET-BLK-POLICY-EXIT.                          0001701
CL131                                                                    0001702
BT361      IF WA-FILETYPE = WS-M                                        DELLRET8
BT361        AND SAVE-LS-OVERRIDE-IND = WS-02                           DELLRET8
BT361        AND LOB-BLOCK-NEXT-POLICY-94 > WK-BRIDGE-LIMIT-02          DELLRET8
BT361          MOVE 'E50' TO WK-ERROR-CODE                              DELLRET8
BT361          PERFORM 3700-SY-ERR-MSG                                  DELLRET8
BT361          MOVE 'Y' TO ERROR-SWITCH                                 DELLRET8
BT361          GO TO 2299E-RET-BLK-POLICY-EXIT.                         DELLRET8
BT361                                                                   DELLRET8
BT361      IF WA-FILETYPE = WS-M                                        DELLRET8
BT361        AND SAVE-LS-OVERRIDE-IND = WS-02                           DELLRET8
BT361          GO TO 2210E-CHECK-POLICY.                                DELLRET8
BT361                                                                   DELLRET8
CL131      IF LOB-BLOCK-NEXT-POLICY-94 > WK-BRIDGE-LIMIT                 0001703
CL131          MOVE 'E42' TO WK-ERROR-CODE                               0001704
CL131          PERFORM 3700-SY-ERR-MSG                                   0001705
CL131          MOVE 'Y' TO ERROR-SWITCH                                  0001706
CL131          GO TO 2299E-RET-BLK-POLICY-EXIT.                          0001707
CL131                                                                    0001708
VC291      IF LOB-BLOCK-NEXT-POLICY-94 > WK-BRIDGE-LIMIT-SS             DELLRET2
VC291          MOVE 'E42' TO WK-ERROR-CODE                              DELLRET2
VC291          PERFORM 3700-SY-ERR-MSG                                  DELLRET2
VC291          MOVE 'Y' TO ERROR-SWITCH                                 DELLRET2
VC291          GO TO 2299E-RET-BLK-POLICY-EXIT.                         DELLRET2
VC291                                                                   DELLRET2
BT349  2210E-CHECK-POLICY.                                              DELLRET6
CL131      MOVE 'N' TO ERROR-SWITCH.                                     0001709
CL131      MOVE SPACES TO SAVE-PASS-POLICY                               0001710
CL131             COMPANY-OPTION-RECORD.                                 0001711
CL131      MOVE SV-CO TO RE-FILE-CO.                                     0001712
CL131      MOVE 'RE' TO RE-FILE-LIT.                                     0001713
CL131      MOVE SPACES TO KEY-94.                                        0001714
CL131      MOVE SV-CO TO CO-94.                                          0001715
CL131      MOVE 94 TO PARM-94.                                           0001716
CL131      MOVE +1 TO WS-SUB1.                                           0001717
CL131      MOVE +2 TO WS-SUB2.                                           0001718
CL131      MOVE +0 TO RTN-CODE.                                          0001719
CL131      MOVE SPACES TO WA-APPLICATION-RECORD.                         0001720
CL131      MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WA-KEY.                 0001721
CL131      PERFORM 8700-READ-KEY-WA.                                     0001722
CL131      IF RTN-CODE > +0                                              0001723
CL131          MOVE 'E43' TO WK-ERROR-CODE                               0001724
CL131          PERFORM 3700-SY-ERR-MSG                                   0001725
CL131          MOVE 'Y' TO ERROR-SWITCH                                  0001726
CL131          GO TO 2299E-RET-BLK-POLICY-EXIT.                          0001727
CL131      MOVE APAP2-BLOCK-OF-BUSINESS-NA TO LOB-94.                    0001728
CL131 *    CALL 'DISKREAD' USING COMPANY-OPTION-RECORD                   0001729
CL131 *                   KEY-94                                         0001730
CL131 *                   RE-FILE-ID                                     0001731
CL131 *                   RTN-CODE.                                      0001732
           MOVE KEY-94 TO VSAM-RE-PRIM                                  DELLIDCH
           READ VSAM-RE                                                 DELLIDCH
                    INTO COMPANY-OPTION-RECORD                          DELLIDCH
           MOVE VSAM-RE-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE KEY-94 TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE RE-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKREAD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
CL131      IF RTN-CODE = +12                                             0001733
CL131          MOVE 'E44' TO WK-ERROR-CODE                               0001734
CL131          PERFORM 3700-SY-ERR-MSG                                   0001735
CL131          MOVE 'Y' TO ERROR-SWITCH                                  0001736
CL131          GO TO 2299E-RET-BLK-POLICY-EXIT.                          0001737
CL131      IF RTN-CODE > +0                                              0001738
CL131          MOVE 'E44' TO WK-ERROR-CODE                               0001739
CL131          PERFORM 3700-SY-ERR-MSG                                   0001740
CL131          MOVE 'Y' TO ERROR-SWITCH                                  0001741
CL131          GO TO 2299E-RET-BLK-POLICY-EXIT.                          0001742
CL131                                                                    0001743
CL131      PERFORM 3000-VALID-POLICY-NUMBER.                             0001744
CL131      IF PGM-ERR = +125                                             0001745
CL131          MOVE 'E45' TO WK-ERROR-CODE                               0001746
CL131          PERFORM 3700-SY-ERR-MSG                                   0001747
CL131          MOVE 'Y' TO ERROR-SWITCH                                  0001748
CL131          GO TO 2299E-RET-BLK-POLICY-EXIT.                          0001749
CL131      MOVE +0 TO WS-SUB1.                                           0001750
CL131  2220E-HOLD-RE-FILE.                                               0001751
CL131 *    CALL 'DISKHOLD' USING COMPANY-OPTION-RECORD                   0001752
CL131 *                   KEY-94                                         0001753
CL131 *                   RE-FILE-ID                                     0001754
CL131 *                   RTN-CODE.                                      0001755
           MOVE KEY-94 TO VSAM-RE-PRIM                                  DELLIDCH
           READ VSAM-RE                                                 DELLIDCH
                    INTO COMPANY-OPTION-RECORD                          DELLIDCH
           MOVE VSAM-RE-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE KEY-94 TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE RE-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
CL131      IF RTN-CODE = +0                                              0001756
CL131          GO TO 2230E-UPDATE-RE-FILE.                               0001757
CL131                                                                    0001758
CL131 *    CALL 'TIMERIT' USING WK-WAIT-TENTHS-SEC.                     DELLMNCH
           MOVE +1 TO  WK-WAIT-TENTHS-SEC                               DELLMNCH
           CALL "CEE3DLY" USING  WK-WAIT-TENTHS-SEC, RC.                DELLMNCH
           IF CONDITION-TOKEN-VALUE <> 0                                DELLMNCH
             DISPLAY "CEE3DLY ERROR PARA-2220E CONDITION-TOKEN-VALUE =" DELLMNCH
                    CONDITION-TOKEN-VALUE                               DELLMNCH
           END-IF                                                       DELLMNCH
CL131      ADD +1 TO WS-SUB1.                                            0001760
CL131      IF WS-SUB1 < 4                                                0001761
CL131          GO TO 2220E-HOLD-RE-FILE.                                 0001762
CL131                                                                    0001763
CL131      IF RTN-CODE = +12                                             0001764
CL131          MOVE 'E46' TO WK-ERROR-CODE                               0001765
CL131          PERFORM 3700-SY-ERR-MSG                                   0001766
CL131          MOVE 'Y' TO ERROR-SWITCH                                  0001767
CL131          GO TO 2299E-RET-BLK-POLICY-EXIT.                          0001768
CL131      MOVE 'Y' TO ERROR-SWITCH.                                     0001769
CL131      GO TO 2299E-RET-BLK-POLICY-EXIT.                              0001770
CL131  2230E-UPDATE-RE-FILE.                                             0001771
CL131      MOVE SAVE-NEW-POLICY TO LOB-BLOCK-NEXT-POLICYX-94.            0001772
CL131 *    CALL 'DISKUP' USING COMPANY-OPTION-RECORD                     0001773
CL131 *                 KEY-94                                           0001774
CL131 *                 RE-FILE-ID                                       0001775
CL131 *                 RTN-CODE.                                        0001776
           MOVE KEY-94 TO VSAM-RE-PRIM                                  DELLIDCH
           REWRITE VSAM-RE-REC                                          DELLIDCH
                      FROM COMPANY-OPTION-RECORD                        DELLIDCH
           MOVE VSAM-RE-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE KEY-94 TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE RE-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE                       DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
CL131      IF RTN-CODE = +12                                             0001777
CL131          MOVE 'E47' TO WK-ERROR-CODE                               0001778
CL131          PERFORM 3700-SY-ERR-MSG                                   0001779
CL131          MOVE 'Y' TO ERROR-SWITCH                                  0001780
CL131          GO TO 2299E-RET-BLK-POLICY-EXIT.                          0001781
CL131                                                                    0001782
CL131      IF RTN-CODE = +0                                              0001783
CL131          GO TO 2240E-CHECK-GU-FILE.                                0001784
CL131                                                                    0001785
CL131      IF (RTN-CODE NOT = +0)                                        0001786
CL131 *        CALL 'TPFIRLFN' USING RE-FILE-ID                         DELLMNCH
CL131 *                       REL-RTN-CODE                              DELLMNCH
CL131          MOVE 'E47' TO WK-ERROR-CODE                               0001789
CL131          PERFORM 3700-SY-ERR-MSG                                   0001790
CL131          MOVE 'Y' TO ERROR-SWITCH                                  0001791
CL131          GO TO 2299E-RET-BLK-POLICY-EXIT.                          0001792
CL131                                                                    0001793
CL131  2240E-CHECK-GU-FILE.                                              0001794
CL131      MOVE +0 TO RTN-CODE.                                          0001795
CL131      IF HWA-CLIENT-GUID = GU-CLIENT-GUID                           0001796
CL131          GO TO 2250E-HOLD-GU-FILE.                                 0001797
CL131                                                                    0001798
CL131      PERFORM 9000-READ-GU.                                         0001799
CL131      IF RTN-CODE = +0                                              0001800
CL131          GO TO 2250E-HOLD-GU-FILE.                                 0001801
CL131                                                                    0001802
CL131      IF RTN-CODE > +0                                              0001803
CL131          MOVE '002' TO WPK-ERR-CODE                                0001804
CL131          GO TO 2299E-RET-BLK-POLICY-EXIT.                          0001805
CL131                                                                    0001806
CL131  2250E-HOLD-GU-FILE.                                               0001807
CL131 *    CALL 'DISKHOLD' USING GU-GUID-RECORD                          0001808
CL131 *                   GU-KEY                                         0001809
CL131 *                   GU-FILE-ID                                     0001810
CL131 *                   RTN-CODE.                                      0001811
           MOVE GU-KEY TO VSAM-GU-PRIM                                  DELLIDCH
           READ VSAM-GU                                                 DELLIDCH
                    INTO GU-GUID-RECORD                                 DELLIDCH
           MOVE VSAM-GU-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE GU-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE GU-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
CL131                                                                    0001812
CL131      IF RTN-CODE > +0                                              0001813
CL131          MOVE '002' TO WPK-ERR-CODE                                0001814
CL131          GO TO 2299E-RET-BLK-POLICY-EXIT.                          0001815
CL131                                                                    0001816
CL131      MOVE SAVE-NEXT-POLICY TO GU-POLICY                            0001817
CL131                            SAVE-PASS-POLICY.                       0001818
CL131  2260E-UPDATE-GU-FILE.                                             0001819
CL131 *    CALL 'DISKUP' USING GU-GUID-RECORD                            0001820
CL131 *                 GU-KEY                                           0001821
CL131 *                 GU-FILE-ID                                       0001822
CL131 *                 RTN-CODE.                                        0001823
           MOVE GU-KEY TO VSAM-GU-PRIM                                  DELLIDCH
           REWRITE VSAM-GU-REC                                          DELLIDCH
                      FROM GU-GUID-RECORD                               DELLIDCH
           MOVE VSAM-GU-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE GU-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE GU-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE                       DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
CL131                                                                    0001824
CL131      IF (RTN-CODE NOT = +0)                                        0001825
CL131 *        CALL 'TPFIRLFN' USING GU-FILE-ID                         DELLMNCH
CL131 *                       REL-RTN-CODE                              DELLMNCH
CL131          MOVE '002' TO WPK-ERR-CODE                                0001828
CL131          GO TO 2299E-RET-BLK-POLICY-EXIT.                          0001829
CL131                                                                    0001830
CL131                                                                    0001831
CL131  2299E-RET-BLK-POLICY-EXIT.                                        0001832
CL131      EXIT.                                                         0001833
CL131                                                                    0001834
       2300-INITIAL-RECORDS  SECTION.                                    0001835
           MOVE 'N' TO ERROR-SWITCH.                                     0001836
           MOVE SAVE-PASS-POLICY TO SAVE-TASK-POLICY.                    0001837
           PERFORM 3100-INITIAL-AP-REC.                                  0001838
           IF RTN-CODE = +0                                              0001839
               GO TO 2320-INITIAL-CV-REC.                                0001840
                                                                         0001841
           MOVE 'ERR' TO WK-STATUS.                                      0001842
           PERFORM 7200-APPLICATION-ERROR.                               0001843
           PERFORM 2600-ERROR-TASKS.                                     0001844
           GO TO 2399-INITIAL-RECORDS-EXIT.                              0001845
       2320-INITIAL-CV-REC.                                              0001846
           MOVE 01 TO SV-APPID-WA.                                       0001847
           PERFORM 3200-INITIAL-CV-REC.                                  0001848
           IF RTN-CODE = +0                                              0001849
               GO TO 2330-INITIAL-NB-REC.                                0001850
                                                                         0001851
           MOVE 'ERR' TO WK-STATUS.                                      0001852
           PERFORM 7200-APPLICATION-ERROR.                               0001853
           PERFORM 2600-ERROR-TASKS.                                     0001854
           GO TO 2399-INITIAL-RECORDS-EXIT.                              0001855
       2330-INITIAL-NB-REC.                                              0001856
           MOVE LIFE-MASTER-ZAP TO LIFE-MASTER-RECORD.                   0001857
           MOVE SV-CO TO LM-CO.                                          0001858
           MOVE SAVE-NEXT-POLICY TO LM-POLICY.                           0001859
      *    CALL 'DISKADD' USING LIFE-MASTER-RECORD                       0001860
      *                  LM-POLID                                        0001861
      *                  NB-FILE-ID                                      0001862
      *                  RTN-CODE.                                       0001863
           MOVE LM-POLID TO VSAM-NB-PRIM                                DELLIDCH
           MOVE LIFE-MASTER-RECORD(1:6) TO LMR-VARL                     DELLMNCH
           WRITE VSAM-NB-REC                                            DELLIDCH
                      FROM LIFE-MASTER-RECORD                           DELLIDCH
           MOVE VSAM-NB-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE LM-POLID TO TPSWNML-FILE-KEY                            DELLIDCH
           MOVE NB-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKADD' TO TPSWNML-FUNCTION-CODE                      DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
           IF RTN-CODE = +0                                              0001864
               GO TO 2340-NB-REC-CONTINUE.                               0001865
                                                                         0001866
           MOVE 'ERR' TO WK-STATUS.                                      0001867
           PERFORM 7200-APPLICATION-ERROR.                               0001868
           PERFORM 2600-ERROR-TASKS.                                     0001869
           GO TO 2399-INITIAL-RECORDS-EXIT.                              0001870
       2340-NB-REC-CONTINUE.                                             0001871
           MOVE SPACES TO LM-POLID.                                      0001872
           MOVE SV-CO TO LM-CO.                                          0001873
           MOVE SAVE-NEXT-POLICY TO LM-POLICY.                           0001874
      *    CALL 'DISKHOLD' USING LIFE-MASTER-RECORD                      0001875
      *                   LM-POLID                                       0001876
      *                   NB-FILE-ID                                     0001877
      *                   RTN-CODE.                                      0001878
           MOVE LM-POLID TO VSAM-NB-PRIM                                DELLIDCH
           READ VSAM-NB                                                 DELLIDCH
                    INTO LIFE-MASTER-RECORD                             DELLIDCH
           MOVE VSAM-NB-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE LM-POLID TO TPSWNML-FILE-KEY                            DELLIDCH
           MOVE NB-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
           IF RTN-CODE = +0                                              0001879
               GO TO 2350-NB-CALL-NBS368.                                0001880
                                                                         0001881
           MOVE 'ERR' TO WK-STATUS.                                      0001882
           PERFORM 7200-APPLICATION-ERROR.                               0001883
           PERFORM 2600-ERROR-TASKS.                                     0001884
           GO TO 2399-INITIAL-RECORDS-EXIT.                              0001885
       2350-NB-CALL-NBS368.                                              0001886
           MOVE SPACES TO NBS368-PASS-AREA.                              0001887
           MOVE 'N' TO CS368-CREATE-SEGS-FLAG.                           0001888
           MOVE SV-CO TO CS368-CO.                                       0001889
           MOVE SAVE-NEXT-POLICY TO CS368-POL.                           0001890
           CALL 'NBS368' USING COVERAGE-RECORD-CV                        0001891
                        APPLICATION-RECORD-AP                            0001892
                        LIFE-MASTER-RECORD                               0001893
                        REFERNCE-OPTION-RECORD                           0001894
                        NBS368-PASS-AREA.                                0001895
                                                                         0001896
      *    CALL 'DISKUP' USING LIFE-MASTER-RECORD                        0001897
      *                 LM-POLID                                         0001898
      *                 NB-FILE-ID                                       0001899
      *                 RTN-CODE.                                        0001900
           MOVE LM-POLID TO VSAM-NB-PRIM                                DELLIDCH
           MOVE LIFE-MASTER-RECORD(1:6) TO LMR-VARL                     DELLMNCH
           REWRITE VSAM-NB-REC                                          DELLIDCH
                      FROM LIFE-MASTER-RECORD                           DELLIDCH
           MOVE VSAM-NB-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE LM-POLID TO TPSWNML-FILE-KEY                            DELLIDCH
           MOVE NB-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE                       DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
           IF (RTN-CODE NOT = +0)                                        0001901
      *       CALL 'TPFIRLFN' USING NB-FILE-ID                          DELLMNCH
      *                       REL-RTN-CODE                              DELLMNCH
               MOVE 'ERR' TO WK-STATUS                                   0001904
               PERFORM 7200-APPLICATION-ERROR                            0001905
               PERFORM 2600-ERROR-TASKS                                  0001906
               GO TO 2399-INITIAL-RECORDS-EXIT.                          0001907
                                                                         0001908
           MOVE 003 TO WK-STATUS                                         0001909
                    NUM-SY.                                              0001910
           PERFORM 8500-UPDATE-SY-STATUS.                                0001911
       2399-INITIAL-RECORDS-EXIT.                                        0001912
           EXIT.                                                         0001913
                                                                         0001914
       2400-PROCESS-WA-RECS  SECTION.                                    0001915
           MOVE 'N' TO ERROR-SWITCH.                                     0001916
           MOVE +0 TO WS-SUB1                                            0001917
                   WS-SUB2                                               0001918
                   CV-SUB.                                               0001919
           MOVE SAVE-PASS-POLICY TO SAVE-TASK-POLICY.                    0001920
       2405-WA-TABLE-LOOP.                                               0001921
           ADD +1 TO WS-SUB1.                                            0001922
           IF WS-SUB1 > WTR-TABLE-MAX                                    0001923
               GO TO 2499-PROCESS-WA-RECS-EXIT.                          0001924
                                                                         0001925
           IF WTR-KEY (WS-SUB1) = SPACES                                 0001926
               GO TO 2499-PROCESS-WA-RECS-EXIT.                          0001927
                                                                         0001928
SM241      IF WTR-FILE-ID (WS-SUB1) = WS-AP                             R       
               GO TO 2410-APPLICATION-FILE.                              0001930
                                                                         0001931
SM241      IF WTR-FILE-ID (WS-SUB1) = WS-NB                             R       
               GO TO 2430-NEW-BUSINESS-FILE.                             0001933
                                                                         0001934
SM241      IF WTR-FILE-ID (WS-SUB1) = WS-PM                             R       
AK131          GO TO 2470-PM-FILE.                                       0001936
AK131                                                                    0001937
SM241      IF WTR-FILE-ID (WS-SUB1) = WS-TH                             R       
               GO TO 2480-TRANS-HIST-FILE.                               0001939
                                                                         0001940
SM241      IF WTR-FILE-ID (WS-SUB1) = WS-WA                             R       
SM241        AND WTR-REC-ID (WS-SUB1) = WS-SY                           R       
               GO TO 2405-WA-TABLE-LOOP.                                 0001943
                                                                         0001944
           MOVE 'INV' TO WK-STATUS.                                      0001945
BT043      MOVE 008 TO NUM-INV.                                          0001946
           PERFORM 7300-INVALID-RECORD.                                  0001947
           GO TO 2405-WA-TABLE-LOOP.                                     0001948
       2410-APPLICATION-FILE.                                            0001949
SM241      IF WTR-REC-ID (WS-SUB1) = WS-AP                              R       
               GO TO 2415-APPLICATION-RECORD.                            0001951
                                                                        DELLRET2
DD281      IF WTR-REC-ID (WS-SUB1) = WS-OR                              DELLRET2
DD281          GO TO 2415-ORDER-REQ-RECORD.                             DELLRET2
                                                                        DELLRET2
SM241      IF WTR-REC-ID (WS-SUB1) = WS-CV                              R       
               GO TO 2420-COVERAGE-RECORD.                               0001954
                                                                         0001955
           MOVE 'INV' TO WK-STATUS.                                      0001956
BT043      MOVE 008 TO NUM-INV.                                          0001957
           PERFORM 7300-INVALID-RECORD.                                  0001958
           GO TO 2405-WA-TABLE-LOOP.                                     0001959
       2415-APPLICATION-RECORD.                                          0001960
           MOVE SPACES TO WA-PASS-KEY-AREA.                              0001961
           MOVE SAVE-PASS-POLICY TO WPK-POLICY.                          0001962
           MOVE WTR-FILE-ID (WS-SUB1) TO WPK-FILE-TYPE.                  0001963
           MOVE WTR-REC-ID (WS-SUB1) TO WPK-RECORD-TYPE.                 0001964
           ADD +1 TO WS-SUB2.                                            0001965
           MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WPK-KEY-ONE.            0001966
           ADD +1 TO WS-SUB2.                                            0001967
           MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WPK-KEY-TWO.            0001968
           MOVE +0 TO PGM-ERR.                                           0001969
           CALL 'LSNNB406' USING WA-PASS-KEY-AREA                        0001970
                          AP-REC-STATUS                                  0001971
DL391                     SAVE-LS-OVERRIDE-IND                          DELLRT11
AS991                     WA-FILETYPE                                    
AS991                     SAVE-LS-PARAM-3                           
AS991                     SAVE-LS-PARAM-4                           
AS991                     SAVE-LS-PARAM-5                           
AS991                     SAVE-LS-PARAM-34                          
                          PGM-ERROR.                                     0001972
           IF WPK-ERR-CODE = SPACES                                      0001973
               MOVE AP-REC-STATUS TO NUM-AP                              0001974
BT181          MOVE WPK-EMPLOYER-ID TO SAVE-EMPLOYER-ID                  0001975
               GO TO 2405-WA-TABLE-LOOP.                                 0001976
                                                                         0001977
           DISPLAY 'ERROR RETURNED FROM CALL OF LSNNB406 '.              0001978
           DISPLAY 'LOCATION IS 2415-APPLICATION-RECORD PARAGRAPH '.     0001979
           DISPLAY 'APPLICATION POLICY IS:  ' SAVE-PASS-POLICY.          0001980
           DISPLAY 'LSNNB402 IS ABENDING...  '.                          0001981
           MOVE +2415 TO ABEND-CODE.                                     0001982
           PERFORM 9900-ABEND-RTN.                                       0001983
                                                                         0001984
DD281  2415-ORDER-REQ-RECORD.                                           DELLRET2
DD281      MOVE SPACES TO WA-PASS-KEY-AREA.                             DELLRET2
DD281      MOVE SAVE-PASS-POLICY TO WPK-POLICY.                         DELLRET2
DD281      MOVE WA-TOTAL-RECS TO WPK-TOTAL-RECS.                        DELLRET2
DD281      MOVE WTR-FILE-ID (WS-SUB1) TO WPK-FILE-TYPE.                 DELLRET2
DD281      MOVE WTR-REC-ID (WS-SUB1) TO WPK-RECORD-TYPE.                DELLRET2
DD281      MOVE +1 TO WS-SUB2.                                          DELLRET2
DD281      MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WPK-KEY-ONE.           DELLRET2
DD281      MOVE +0 TO PGM-ERR.                                          DELLRET2
DD281      CALL 'LSNNB416' USING WA-PASS-KEY-AREA                       DELLRET2
DD281                     PGM-ERROR.                                    DELLRET2
DD281      IF WPK-ERR-CODE = SPACES                                     DELLRET2
DD281          GO TO 2470-CHECK-OR-PGM-ERR.                             DELLRET2
DD281      DISPLAY 'ERROR RETURNED FROM CALL OF LSNNB416 '.             DELLRET2
DD281      DISPLAY 'LOCATION IS 2415-ORDER-REQ-RECORD PARAGRAPH'.       DELLRET2
DD281      DISPLAY 'APPLICATION POLICY IS:  ' SAVE-PASS-POLICY.         DELLRET2
DD281      DISPLAY 'LSNNB402 IS ABENDING...  '.                         DELLRET2
DD281      MOVE +2470 TO ABEND-CODE.                                    DELLRET2
DD281      PERFORM 9900-ABEND-RTN.                                      DELLRET2
DD281                                                                   DELLRET2
DD281  2470-CHECK-OR-PGM-ERR.                                           DELLRET2
DD281      IF PGM-ERR > +0                                              DELLRET2
DD281          MOVE +0 TO PGM-ERR                                       DELLRET2
DD281          MOVE 008 TO NUM-OR.                                      DELLRET2
DD281      IF NUM-OR = 008                                              DELLRET2
DD281          GO TO 2405-WA-TABLE-LOOP.                                DELLRET2
DD281      MOVE 009 TO NUM-OR.                                          DELLRET2
DD281      GO TO 2405-WA-TABLE-LOOP.                                    DELLRET2
                                                                        DELLRET2
       2420-COVERAGE-RECORD.                                             0001985
           MOVE +0 TO WS-SUB2.                                           0001986
           ADD +1 TO CV-SUB.                                             0001987
           MOVE SPACES TO WA-PASS-KEY-AREA.                              0001988
           MOVE SAVE-PASS-POLICY TO WPK-POLICY.                          0001989
           MOVE WTR-FILE-ID (WS-SUB1) TO WPK-FILE-TYPE.                  0001990
           MOVE WTR-REC-ID (WS-SUB1) TO WPK-RECORD-TYPE.                 0001991
       2421-COVERAGE-LOOP.                                               0001992
           ADD +1 TO WS-SUB2.                                            0001993
           IF WS-SUB2 > CV-REC-MAX                                       0001994
               GO TO 2422-GET-COVERAGE.                                  0001995
                                                                         0001996
           IF WS-SUB2 = +1                                               0001997
CL181          MOVE WTR-REC-APPID (WS-SUB1, WS-SUB2) TO SV-APPID-WA      0001998
               MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WPK-KEY-ONE.        0001999
                                                                         0002000
           IF WS-SUB2 = +2                                               0002001
               MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WPK-KEY-TWO.        0002002
                                                                         0002003
           IF WS-SUB2 = +3                                               0002004
               MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WPK-KEY-THREE.      0002005
                                                                         0002006
           IF WS-SUB2 = +4                                               0002007
               MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WPK-KEY-FOUR.       0002008
                                                                         0002009
           GO TO 2421-COVERAGE-LOOP.                                     0002010
       2422-GET-COVERAGE.                                                0002011
           IF CV-SUB = +1                                                0002012
               GO TO 2424-CALL-LSNNB407.                                 0002013
                                                                         0002014
           PERFORM 3200-INITIAL-CV-REC.                                  0002015
           IF RTN-CODE = +0                                              0002016
               GO TO 2424-CALL-LSNNB407.                                 0002017
                                                                         0002018
           MOVE 'ERR' TO WK-STATUS.                                      0002019
           PERFORM 7200-APPLICATION-ERROR.                               0002020
           PERFORM 2600-ERROR-TASKS.                                     0002021
           GO TO 2499-PROCESS-WA-RECS-EXIT.                              0002022
                                                                         0002023
       2424-CALL-LSNNB407.                                               0002024
           MOVE +0 TO PGM-ERR.                                           0002025
           CALL 'LSNNB407' USING WA-PASS-KEY-AREA                        0002026
                          AP-REC-STATUS                                  0002027
                          PGM-ERROR.                                     0002028
                                                                         0002029
           IF WPK-ERR-CODE = SPACES                                      0002030
               MOVE AP-REC-STATUS TO NUM-AP                              0002031
               GO TO 2425-CHECK-CV-PGM-ERR.                              0002032
                                                                         0002033
           DISPLAY 'ERROR RETURNED FROM CALL OF LSNNB407 '.              0002034
           DISPLAY 'LOCATION IS 2424-CALL-LSNNB407 PARAGRAPH '.          0002035
           DISPLAY 'APPLICATION POLICY IS:  ' SAVE-PASS-POLICY.          0002036
           DISPLAY 'LSNNB402 IS ABENDING...  '.                          0002037
           MOVE +2424 TO ABEND-CODE.                                     0002038
           PERFORM 9900-ABEND-RTN.                                       0002039
                                                                         0002040
       2425-CHECK-CV-PGM-ERR.                                            0002041
           IF PGM-ERR > +0                                               0002042
               MOVE 008 TO NUM-CV                                        0002043
           ELSE                                                          0002044
               MOVE 009 TO NUM-CV.                                       0002045
                                                                         0002046
           GO TO 2405-WA-TABLE-LOOP.                                     0002047
       2430-NEW-BUSINESS-FILE.                                           0002048
           MOVE SPACES TO WA-PASS-KEY-AREA.                              0002049
           MOVE SAVE-PASS-POLICY TO WPK-POLICY.                          0002050
CL131      MOVE WA-TOTAL-RECS TO WPK-TOTAL-RECS.                         0002051
           MOVE WTR-FILE-ID (WS-SUB1) TO WPK-FILE-TYPE.                  0002052
           MOVE WTR-REC-ID (WS-SUB1) TO WPK-RECORD-TYPE.                 0002053
           MOVE +1 TO WS-SUB2.                                           0002054
           MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WPK-KEY-ONE.            0002055
                                                                         0002056
SM241      IF WTR-REC-ID (WS-SUB1) = WS-T1                              R       
               GO TO 2440-T1-SEGMENT.                                    0002058
                                                                         0002059
SM241      IF WTR-REC-ID (WS-SUB1) = WS-BN                              R       
               GO TO 2460-BN-SEGMENT.                                    0002061
CL131                                                                    0002062
SM241      IF WTR-REC-ID (WS-SUB1) = WS-AB                              R       
CL131          GO TO 2470-AB-SEGMENT.                                    0002064
BT162                                                                    0002065
SM241      IF WTR-REC-ID (WS-SUB1) = WS-AC                              R       
BT162          GO TO 2470-AC-SEGMENT.                                    0002067
BT231                                                                    0002068
SM241      IF WTR-REC-ID (WS-SUB1) = WS-OW                              R       
BT231          GO TO 2470-OW-SEGMENT.                                    0002070
KM101                                                                    0002071
SS521      IF WTR-REC-ID (WS-SUB1) = WS-DI
SS521          GO TO 2450-DI-SEGMENT.
SS521
NK771      IF WTR-REC-ID (WS-SUB1) = WS-FM
NK771         GO TO 2479-FM-SEGMENT.
NK771
SM241      IF WTR-REC-ID (WS-SUB1) = WS-BD                              R       
KM101          GO TO 2405-WA-TABLE-LOOP.                                 0002073
                                                                         0002074
SM241      IF WTR-REC-ID (WS-SUB1) = WS-TP                              I       
SM241          GO TO 2405-WA-TABLE-LOOP.                                I       
SM241                                                                   I       
BT444      IF WTR-REC-ID (WS-SUB1) = WS-EI                              DELLRT16
BT444          GO TO 2475-EI-SEGMENT.                                   DELLRT16
BT444                                                                   DELLRT16
AR150      IF WTR-REC-ID (WS-SUB1) = WS-AL                                      
AR150          GO TO 2476-AL-SEGMENT.                                           
AR150                                                                           
AR150      IF WTR-REC-ID (WS-SUB1) = WS-GW                                      
AR150          GO TO 2477-GW-SEGMENT.                                           
AR150                                                                           
AR150      IF WTR-REC-ID (WS-SUB1) = WS-IB                                      
AR150          GO TO 2478-IB-SEGMENT.                                           
AR150                                                                           
           MOVE 'INV' TO WK-STATUS.                                      0002075
BT043      MOVE 008 TO NUM-INV.                                          0002076
           PERFORM 7300-INVALID-RECORD.                                  0002077
           GO TO 2405-WA-TABLE-LOOP.                                     0002078
       2440-T1-SEGMENT.                                                  0002079
           MOVE +0 TO PGM-ERR.                                           0002080
           CALL 'LSNNB414' USING WA-PASS-KEY-AREA                        0002081
                          PGM-ERROR.                                     0002082
                                                                         0002083
           IF WPK-ERR-CODE = SPACES                                      0002084
               GO TO 2445-CHECK-T1-PGM-ERR.                              0002085
                                                                         0002086
           DISPLAY 'ERROR RETURNED FROM CALL OF LSNNB414 '.              0002087
           DISPLAY 'LOCATION IS 2440-T1-SEGMENT PARAGRAPH '.             0002088
           DISPLAY 'APPLICATION POLICY IS:  ' SAVE-PASS-POLICY.          0002089
           DISPLAY 'LSNNB402 IS ABENDING...  '.                          0002090
           MOVE +2440 TO ABEND-CODE.                                     0002091
           PERFORM 9900-ABEND-RTN.                                       0002092
                                                                         0002093
       2445-CHECK-T1-PGM-ERR.                                            0002094
           IF PGM-ERR > +0                                               0002095
               MOVE 008 TO NUM-T1                                        0002096
           ELSE                                                          0002097
               MOVE 009 TO NUM-T1.                                       0002098
                                                                         0002099
           GO TO 2405-WA-TABLE-LOOP.                                     0002100
SS521  2450-DI-SEGMENT.
SS521      MOVE +0 TO PGM-ERR.
SS521      CALL 'LSNNB430' USING WA-PASS-KEY-AREA                
SS521                            PGM-ERROR.                             
SS521                                                            
SS521      IF WPK-ERR-CODE = SPACES                              
SS521          GO TO 2455-CHECK-DI-PGM-ERR.                      
SS521                                                            
SS521      DISPLAY 'ERROR RETURNED FROM CALL OF LSNNB430 '.      
SS521      DISPLAY 'LOCATION IS 2450-DI-SEGMENT PARAGRAPH '.     
SS521      DISPLAY 'APPLICATION POLICY IS:  ' SAVE-PASS-POLICY.  
SS521      DISPLAY 'LSNNB402 IS ABENDING...  '.                  
SS521      MOVE +2450 TO ABEND-CODE.                             
SS521      PERFORM 9900-ABEND-RTN.                               
SS521                                                            
SS521  2455-CHECK-DI-PGM-ERR.                                    
SS521      IF PGM-ERR > +0                                       
SS521         MOVE 008 TO NUM-DI                                
SS521      IF NUM-DI = 008
SS521         GO TO 2405-WA-TABLE-LOOP.
SS521      MOVE 009 TO NUM-DI.                               
SS521      GO TO 2405-WA-TABLE-LOOP.                             
       2460-BN-SEGMENT.                                                  0002101
           MOVE +0 TO PGM-ERR.                                           0002102
           CALL 'LSNNB408' USING WA-PASS-KEY-AREA                        0002103
                          PGM-ERROR.                                     0002104
           IF WPK-ERR-CODE = SPACES                                      0002105
               GO TO 2465-CHECK-BN-PGM-ERR.                              0002106
                                                                         0002107
           DISPLAY 'ERROR RETURNED FROM CALL OF LSNNB408 '.              0002108
           DISPLAY 'LOCATION IS 2460-BN-SEGMENT PARAGRAPH '.             0002109
           DISPLAY 'APPLICATION POLICY IS:  ' SAVE-PASS-POLICY.          0002110
           DISPLAY 'LSNNB402 IS ABENDING...  '.                          0002111
           MOVE +2460 TO ABEND-CODE.                                     0002112
           PERFORM 9900-ABEND-RTN.                                       0002113
                                                                         0002114
       2465-CHECK-BN-PGM-ERR.                                            0002115
           IF PGM-ERR > +0                                               0002116
BT111          MOVE 008 TO NUM-BN.                                       0002117
BT111      PERFORM 3400-SET-BD-STATUS.                                   0002118
SM241      PERFORM 3460-SET-TP-STATUS.                                  I       
BT111      IF NUM-BN = 008                                               0002119
BT111          GO TO 2405-WA-TABLE-LOOP.                                 0002120
BT111      MOVE 009 TO NUM-BN.                                           0002121
                                                                         0002122
           GO TO 2405-WA-TABLE-LOOP.                                     0002123
AK131  2470-PM-FILE.                                                     0002124
AK131      MOVE +0 TO WS-SUB2.                                           0002125
AK131      MOVE SPACES TO WA-PASS-KEY-AREA.                              0002126
AK131      MOVE SAVE-PASS-POLICY TO WPK-POLICY.                          0002127
AK131      MOVE WTR-FILE-ID (WS-SUB1) TO WPK-FILE-TYPE.                  0002128
AK131      MOVE WTR-REC-ID (WS-SUB1) TO WPK-RECORD-TYPE.                 0002129
BT262      IF (WASY-LS-PARAM-37 = 'U')                                  DELLRETC
BT262        AND (SAVE-LS-OVERRIDE-IND = '02')                          DELLRETC
BT262          MOVE 'U' TO WPK-POST-IMPORT-IND.                         DELLRETC
AK131  2471-PM-LOOP.                                                     0002130
AK131      ADD +1 TO WS-SUB2.                                            0002131
AK131      IF WS-SUB2 > PM-REC-MAX                                       0002132
AK131          GO TO 2472-GET-PN.                                        0002133
AK131                                                                    0002134
AK131      IF WS-SUB2 = +1                                               0002135
AK131          MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WPK-KEY-ONE.        0002136
AK131                                                                    0002137
AK131      IF WS-SUB2 = +2                                               0002138
AK131          MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WPK-KEY-TWO.        0002139
AK131                                                                    0002140
AK131      IF WS-SUB2 = +3                                               0002141
AK131          MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WPK-KEY-THREE.      0002142
AK131                                                                    0002143
AK131      GO TO 2471-PM-LOOP.                                           0002144
AK131  2472-GET-PN.                                                      0002145
AK131      IF WTR-REC-ID (WS-SUB1) = 'PN'                                0002146
AK131          GO TO 2473-CALL-LSNNB443.                                 0002147
AK131                                                                    0002148
AK131      MOVE 'INV' TO WK-STATUS.                                      0002149
AK131      MOVE 008 TO NUM-INV.                                          0002150
AK131      PERFORM 7300-INVALID-RECORD.                                  0002151
AK131      GO TO 2405-WA-TABLE-LOOP.                                     0002152
AK131  2473-CALL-LSNNB443.                                               0002153
AK131      MOVE +0 TO PGM-ERR.                                           0002154
AK131      CALL 'LSNNB443' USING WA-PASS-KEY-AREA                        0002155
AK131                     PGM-ERROR.                                     0002156
AK131                                                                    0002157
AK131      IF WPK-ERR-CODE = SPACES                                      0002158
AK131          GO TO 2474-CHECK-PN-PGM-ERR.                              0002159
AK131                                                                    0002160
AK131      DISPLAY 'ERROR RETURNED FROM CALL OF LSNNB443 '.              0002161
AK131      DISPLAY 'LOCATION IS 2473-CALL-LSNNB443 PARAGRAPH '.          0002162
AK131      DISPLAY 'APPLICATION POLICY IS:  ' SAVE-PASS-POLICY.          0002163
AK131      DISPLAY 'LSNNB402 IS ABENDING...  '.                          0002164
AK131      MOVE +2471 TO ABEND-CODE.                                     0002165
AK131      PERFORM 9900-ABEND-RTN.                                       0002166
AK131  2474-CHECK-PN-PGM-ERR.                                            0002167
AK131      IF PGM-ERR > +0                                               0002168
AK131          MOVE 008 TO NUM-PN.                                       0002169
AK131      IF NUM-PN = 008                                               0002170
AK131          GO TO 2405-WA-TABLE-LOOP.                                 0002171
AK131      MOVE 009 TO NUM-PN.                                           0002172
AK131      GO TO 2405-WA-TABLE-LOOP.                                     0002173
CL131  2470-AB-SEGMENT.                                                  0002174
CL131      MOVE +0 TO PGM-ERR.                                           0002175
CL131      CALL 'LSNNB508' USING WA-PASS-KEY-AREA                        0002176
CL131                     COMPANY-OPTION-RECORD                          0002177
CL131                     PGM-ERROR.                                     0002178
CL131      IF WPK-ERR-CODE = SPACES                                      0002179
CL131          GO TO 2475-CHECK-AB-PGM-ERR.                              0002180
CL131                                                                    0002181
CL131      DISPLAY 'ERROR RETURNED FROM CALL OF LSNNB508 '.              0002182
CL131      DISPLAY 'LOCATION IS 2470-AB-SEGMENT PARAGRAPH '.             0002183
CL131      DISPLAY 'APPLICATION POLICY IS:  ' SAVE-PASS-POLICY.          0002184
CL131      DISPLAY 'LSNNB402 IS ABENDING...  '.                          0002185
CL131      MOVE +2470 TO ABEND-CODE.                                     0002186
CL131      PERFORM 9900-ABEND-RTN.                                       0002187
CL131                                                                    0002188
CL131  2475-CHECK-AB-PGM-ERR.                                            0002189
CL131      IF PGM-ERR > +0                                               0002190
CL131          MOVE 008 TO NUM-AB.                                       0002191
CL131      IF NUM-AB = 008                                               0002192
CL131          GO TO 2405-WA-TABLE-LOOP.                                 0002193
CL131      MOVE 009 TO NUM-AB.                                           0002194
CL131      GO TO 2405-WA-TABLE-LOOP.                                     0002195
BT162  2470-AC-SEGMENT.                                                  0002196
BT162      MOVE +0 TO PGM-ERR.                                           0002197
BT162      CALL 'LSNNB418' USING WA-PASS-KEY-AREA                        0002198
BT162                     PGM-ERROR.                                     0002199
BT162      IF WPK-ERR-CODE = SPACES                                      0002200
BT162          GO TO 2470-CHECK-AC-PGM-ERR.                              0002201
BT162                                                                    0002202
BT162      DISPLAY 'ERROR RETURNED FROM CALL OF LSNNB408 '.              0002203
BT162      DISPLAY 'LOCATION IS 2470-AC-SEGMENT PARAGRAPH '.             0002204
BT162      DISPLAY 'APPLICATION POLICY IS:  ' SAVE-PASS-POLICY.          0002205
BT162      DISPLAY 'LSNNB402 IS ABENDING...  '.                          0002206
BT162      MOVE +2470 TO ABEND-CODE.                                     0002207
BT162      PERFORM 9900-ABEND-RTN.                                       0002208
BT162                                                                    0002209
BT162  2470-CHECK-AC-PGM-ERR.                                            0002210
BT162      IF PGM-ERR > +0                                               0002211
BT162          MOVE 008 TO NUM-AC.                                       0002212
BT162      IF NUM-AC = 008                                               0002213
BT162          GO TO 2405-WA-TABLE-LOOP.                                 0002214
BT162      MOVE 009 TO NUM-AC.                                           0002215
BT162      GO TO 2405-WA-TABLE-LOOP.                                     0002216
BT231                                                                    0002217
BT231  2470-OW-SEGMENT.                                                  0002218
BT231      MOVE +0 TO PGM-ERR.                                           0002219
BT231      CALL 'LSNNB410' USING WA-PASS-KEY-AREA                        0002220
BT231                     PGM-ERROR.                                     0002221
BT231      IF WPK-ERR-CODE = SPACES                                      0002222
BT231          GO TO 2470-CHECK-OW-PGM-ERR.                              0002223
BT231                                                                    0002224
BT231      DISPLAY 'ERROR RETURNED FROM CALL OF LSNNB408 '.              0002225
BT231      DISPLAY 'LOCATION IS 2470-OW-SEGMENT PARAGRAPH '.             0002226
BT231      DISPLAY 'APPLICATION POLICY IS:  ' SAVE-PASS-POLICY.          0002227
BT231      DISPLAY 'LSNNB402 IS ABENDING...  '.                          0002228
BT231      MOVE +2470 TO ABEND-CODE.                                     0002229
BT231      PERFORM 9900-ABEND-RTN.                                       0002230
BT231                                                                    0002231
BT231  2470-CHECK-OW-PGM-ERR.                                            0002232
BT231      IF PGM-ERR > +0                                               0002233
BT231          MOVE 008 TO NUM-OW.                                       0002234
BT231      IF NUM-OW = 008                                               0002235
BT231          GO TO 2405-WA-TABLE-LOOP.                                 0002236
BT231      MOVE 009 TO NUM-OW.                                           0002237
BT231      GO TO 2405-WA-TABLE-LOOP.                                     0002238
BT231                                                                    0002239
BT444                                                                   DELLRT16
BT444  2475-EI-SEGMENT.                                                 DELLRT16
BT444      MOVE +0 TO PGM-ERR.                                          DELLRT16
BT444      CALL 'LSNNB426' USING WA-PASS-KEY-AREA                       DELLRT16
BT444                     PGM-ERROR.                                    DELLRT16
BT444      IF WPK-ERR-CODE = SPACES                                     DELLRT16
BT444          GO TO 2475-CHECK-EI-PGM-ERR.                             DELLRT16
BT444                                                                   DELLRT16
BT444      DISPLAY 'ERROR RETURNED FROM CALL OF LSNNB426 '.             DELLRT16
BT444      DISPLAY 'LOCATION IS 2475-EI-SEGMENT PARAGRAPH '.            DELLRT16
BT444      DISPLAY 'APPLICATION POLICY IS:  ' SAVE-PASS-POLICY.         DELLRT16
BT444      DISPLAY 'LSNNB402 IS ABENDING...  '.                         DELLRT16
BT444      MOVE +2475 TO ABEND-CODE.                                    DELLRT16
BT444      PERFORM 9900-ABEND-RTN.                                      DELLRT16
BT444                                                                   DELLRT16
BT444  2475-CHECK-EI-PGM-ERR.                                           DELLRT16
BT444      IF PGM-ERR > +0                                              DELLRT16
BT444          MOVE 008 TO NUM-EI.                                      DELLRT16
BT444      IF NUM-EI = 008                                              DELLRT16
BT444          GO TO 2405-WA-TABLE-LOOP.                                DELLRT16
BT444      MOVE 009 TO NUM-EI.                                          DELLRT16
BT444      GO TO 2405-WA-TABLE-LOOP.                                    DELLRT16
BT444                                                                   DELLRT16
AR150  2476-AL-SEGMENT.                                                 
AR150      MOVE +0 TO PGM-ERR.                                          
AR150      CALL 'LSNNB427' USING WA-PASS-KEY-AREA                       
AR150                     PGM-ERROR.                                    
AR150      IF WPK-ERR-CODE = SPACES                                     
AR150          GO TO 2476-CHECK-AL-PGM-ERR.                             
AR150                                                                   
AR150      DISPLAY 'ERROR RETURNED FROM CALL OF LSNNB427 '.             
AR150      DISPLAY 'LOCATION IS 2476-AL-SEGMENT PARAGRAPH '.            
AR150      DISPLAY 'APPLICATION POLICY IS:  ' SAVE-PASS-POLICY.         
AR150      DISPLAY 'LSNNB402 IS ABENDING...  '.                         
AR150      MOVE +2476 TO ABEND-CODE.                                    
AR150      PERFORM 9900-ABEND-RTN.                                      
AR150                                                                   
AR150  2476-CHECK-AL-PGM-ERR.                                           
AR150      IF PGM-ERR > +0                                              
AR150          MOVE 008 TO NUM-AL.                                      
AR150      IF NUM-AL = 008                                              
AR150          GO TO 2405-WA-TABLE-LOOP.                                
AR150      MOVE 009 TO NUM-AL.                                          
AR150      GO TO 2405-WA-TABLE-LOOP.                                    
AR150                                                                   
AR150  2477-GW-SEGMENT.                                                 
AR150      MOVE +0 TO PGM-ERR.                                          
AR150      CALL 'LSNNB428' USING WA-PASS-KEY-AREA                       
AR150                     PGM-ERROR.                                    
AR150      IF WPK-ERR-CODE = SPACES                                     
AR150          GO TO 2477-CHECK-GW-PGM-ERR.                             
AR150                                                                   
AR150      DISPLAY 'ERROR RETURNED FROM CALL OF LSNNB428 '.             
AR150      DISPLAY 'LOCATION IS 2477-GW-SEGMENT PARAGRAPH '.            
AR150      DISPLAY 'APPLICATION POLICY IS:  ' SAVE-PASS-POLICY.         
AR150      DISPLAY 'LSNNB402 IS ABENDING...  '.                         
AR150      MOVE +2477 TO ABEND-CODE.                                    
AR150      PERFORM 9900-ABEND-RTN.                                      
AR150                                                                   
AR150  2477-CHECK-GW-PGM-ERR.                                           
AR150      IF PGM-ERR > +0                                              
AR150          MOVE 008 TO NUM-GW.                                      
AR150      IF NUM-GW = 008                                              
AR150          GO TO 2405-WA-TABLE-LOOP.                                
AR150      MOVE 009 TO NUM-GW.                                          
AR150      GO TO 2405-WA-TABLE-LOOP.                                    
AR150                                                                   
AR150  2478-IB-SEGMENT.                                                 
AR150      MOVE +0 TO PGM-ERR.                                          
AR150      CALL 'LSNNB429' USING WA-PASS-KEY-AREA                       
AR150                     PGM-ERROR.                                    
AR150      IF WPK-ERR-CODE = SPACES                                     
AR150          GO TO 2478-CHECK-IB-PGM-ERR.                             
AR150                                                                   
AR150      DISPLAY 'ERROR RETURNED FROM CALL OF LSNNB429 '.             
AR150      DISPLAY 'LOCATION IS 2478-IB-SEGMENT PARAGRAPH '.            
AR150      DISPLAY 'APPLICATION POLICY IS:  ' SAVE-PASS-POLICY.         
AR150      DISPLAY 'LSNNB402 IS ABENDING...  '.                         
AR150      MOVE +2478 TO ABEND-CODE.                                    
AR150      PERFORM 9900-ABEND-RTN.                                      
AR150                                                                   
AR150  2478-CHECK-IB-PGM-ERR.                                           
AR150      IF PGM-ERR > +0                                              
AR150          MOVE 008 TO NUM-IB.                                      
AR150      IF NUM-IB = 008                                              
AR150          GO TO 2405-WA-TABLE-LOOP.                                
AR150      MOVE 009 TO NUM-IB.                                          
AR150      GO TO 2405-WA-TABLE-LOOP.                                    
AR150  
NK771  2479-FM-SEGMENT.
NK771      MOVE +0 TO PGM-ERR.                       
NK771      CALL 'LSNNB431' USING WA-PASS-KEY-AREA                       
NK771                     PGM-ERROR.                   
NK771      IF WPK-ERR-CODE = SPACES                                     
NK771          GO TO 2479-CHECK-FM-PGM-ERR.
NK771                                                              
NK771      DISPLAY 'ERROR RETURNED FROM CALL OF LSNNB431 '.             
NK771      DISPLAY 'LOCATION IS 2479-FM-SEGMENT PARAGRAPH '.            
NK771      DISPLAY 'APPLICATION POLICY IS:  ' SAVE-PASS-POLICY.         
NK771      DISPLAY 'LSNNB402 IS ABENDING...  '.                         
NK771      MOVE +2479 TO ABEND-CODE.                                    
NK771      PERFORM 9900-ABEND-RTN.                                      
NK771                                                              
NK771  2479-CHECK-FM-PGM-ERR.                                           
NK771      IF PGM-ERR > +0                                              
NK771          MOVE 008 TO NUM-FM.                                      
NK771      IF NUM-FM = 008                                              
NK771          GO TO 2405-WA-TABLE-LOOP.                                
NK771      MOVE 009 TO NUM-FM.                                          
NK771      GO TO 2405-WA-TABLE-LOOP.       
NK771                                                                 
       2480-TRANS-HIST-FILE.                                             0002240
           MOVE SPACES TO WA-PASS-KEY-AREA.                              0002241
           MOVE SAVE-PASS-POLICY TO WPK-POLICY.                          0002242
           MOVE WTR-FILE-ID (WS-SUB1) TO WPK-FILE-TYPE.                  0002243
           MOVE WTR-REC-ID (WS-SUB1) TO WPK-RECORD-TYPE.                 0002244
           MOVE +1 TO WS-SUB2.                                           0002245
           MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WPK-KEY-ONE.            0002246
                                                                         0002247
           IF WTR-REC-ID (WS-SUB1) = 'NT'                                0002248
               GO TO 2490-NT-RECORD.                                     0002249
                                                                         0002250
           MOVE 'INV' TO WK-STATUS.                                      0002251
BT043      MOVE 008 TO NUM-INV.                                          0002252
           PERFORM 7300-INVALID-RECORD.                                  0002253
           GO TO 2405-WA-TABLE-LOOP.                                     0002254
       2490-NT-RECORD.                                                   0002255
           MOVE +0 TO PGM-ERR.                                           0002256
           CALL 'LSNNB437' USING WA-PASS-KEY-AREA                        0002257
                          PGM-ERROR.                                     0002258
           IF WPK-ERR-CODE = SPACES                                      0002259
               GO TO 2495-CHECK-NT-PGM-ERR.                              0002260
                                                                         0002261
           DISPLAY 'ERROR RETURNED FROM CALL OF LSNNB437 '.              0002262
           DISPLAY 'LOCATION IS 2490-NT-RECORD PARAGRAPH '.              0002263
           DISPLAY 'APPLICATION POLICY IS:  ' SAVE-PASS-POLICY.          0002264
           DISPLAY 'LSNNB402 IS ABENDING...  '.                          0002265
           MOVE +2490 TO ABEND-CODE.                                     0002266
           PERFORM 9900-ABEND-RTN.                                       0002267
                                                                         0002268
       2495-CHECK-NT-PGM-ERR.                                            0002269
           IF PGM-ERR > +0                                               0002270
               MOVE 008 TO NUM-NT                                        0002271
           ELSE                                                          0002272
               MOVE 009 TO NUM-NT.                                       0002273
                                                                         0002274
           GO TO 2405-WA-TABLE-LOOP.                                     0002275
       2499-PROCESS-WA-RECS-EXIT.                                        0002276
           EXIT.                                                         0002277
                                                                         0002278
       2500-TASK-RECORD  SECTION.                                        0002279
           MOVE 'N' TO ERROR-SWITCH.                                     0002280
           IF NUM-SY = 003                                               0002281
             AND NUM-AP = 000                                            0002282
               MOVE NUM-SY TO WK-STATUS                                  0002283
               GO TO 2520-SET-SY-STATUS.                                 0002284
                                                                         0002285
           IF NUM-AP = 014 OR 008                                        0002286
               MOVE NUM-AP TO WK-STATUS                                  0002287
               GO TO 2520-SET-SY-STATUS.                                 0002288
                                                                         0002289
           IF NUM-CV = 008                                               0002290
               MOVE NUM-CV TO WK-STATUS                                  0002291
               GO TO 2520-SET-SY-STATUS.                                 0002292

SS521      IF NUM-DI = 008
SS521          MOVE NUM-DI TO WK-STATUS
SS521          GO TO 2520-SET-SY-STATUS.
                                                                        DELLRET2
DD281      IF NUM-OR = 008                                              DELLRET2
DD281          MOVE NUM-OR TO WK-STATUS                                 DELLRET2
DD281          GO TO 2520-SET-SY-STATUS.                                DELLRET2
                                                                        DELLRET2
           IF NUM-T1 = 008                                               0002294
               MOVE NUM-T1 TO WK-STATUS                                  0002295
               GO TO 2520-SET-SY-STATUS.                                 0002296
                                                                         0002297
           IF NUM-T2 = 008                                               0002298
               MOVE NUM-T2 TO WK-STATUS                                  0002299
               GO TO 2520-SET-SY-STATUS.                                 0002300
                                                                         0002301
           IF NUM-BN = 008                                               0002302
               MOVE NUM-BN TO WK-STATUS                                  0002303
               GO TO 2520-SET-SY-STATUS.                                 0002304
                                                                         0002305
CL131      IF NUM-AB = 008                                               0002306
CL131          MOVE NUM-AB TO WK-STATUS                                  0002307
CL131          GO TO 2520-SET-SY-STATUS.                                 0002308
CL131                                                                    0002309
BT162      IF NUM-AC = 008                                               0002310
BT162          MOVE NUM-AC TO WK-STATUS                                  0002311
BT162          GO TO 2520-SET-SY-STATUS.                                 0002312
BT162                                                                    0002313
BT231      IF NUM-OW = 008                                               0002314
BT231          MOVE NUM-OW TO WK-STATUS                                  0002315
BT231          GO TO 2520-SET-SY-STATUS.                                 0002316
BT231                                                                    0002317
           IF NUM-NT = 008                                               0002318
               MOVE NUM-NT TO WK-STATUS                                  0002319
               GO TO 2520-SET-SY-STATUS.                                 0002320
                                                                         0002321
AK131      IF NUM-PN = 008                                               0002322
AK131          MOVE NUM-PN TO WK-STATUS                                  0002323
AK131          GO TO 2520-SET-SY-STATUS.                                 0002324
AK131                                                                    0002325
BT162      IF NUM-INV = 008                                              0002326
BT162          MOVE NUM-INV TO WK-STATUS                                 0002327
BT162          GO TO 2520-SET-SY-STATUS.                                 0002328
BT162                                                                    0002329
BT444      IF NUM-EI = 008                                              DELLRT16
BT444          MOVE NUM-EI TO WK-STATUS                                 DELLRT16
BT444          GO TO 2520-SET-SY-STATUS.                                DELLRT16
BT444                                                                   DELLRT16
AR150      IF NUM-AL = 008                                              
AR150          MOVE NUM-AL TO WK-STATUS                                 
AR150          GO TO 2520-SET-SY-STATUS.                                
AR150                                                                   
AR150      IF NUM-GW = 008                                              
AR150          MOVE NUM-GW TO WK-STATUS                                 
AR150          GO TO 2520-SET-SY-STATUS.                                
AR150                                                                   
AR150      IF NUM-IB = 008                                              
AR150          MOVE NUM-IB TO WK-STATUS                                 
AR150          GO TO 2520-SET-SY-STATUS.                                
AR150
NK771      IF NUM-FM = 008
NK771         MOVE NUM-FM TO WK-STATUS
NK771         GO TO 2520-SET-SY-STATUS.
NK771                                                                   
           MOVE '009' TO WK-STATUS.                                      0002330
       2520-SET-SY-STATUS.                                               0002331
BT132      MOVE +0 TO WS-SUB1.                                           0002332
BT132      MOVE +1 TO WS-SUB2.                                           0002333
BT132  2521-LOCATE-SY-LOOP.                                              0002334
BT132      ADD +1 TO WS-SUB1.                                            0002335
BT132      IF WS-SUB1 > WTR-TABLE-MAX                                    0002336
BT132          GO TO 2523-CONTINUE.                                      0002337
BT132                                                                    0002338
BT132      IF WTR-FILE-ID (WS-SUB1) = 'WA'                               0002339
BT132        AND WTR-REC-ID (WS-SUB1) = 'SY'                             0002340
BT132          GO TO 2522-READ-SY-REC.                                   0002341
BT132                                                                    0002342
BT132      GO TO 2521-LOCATE-SY-LOOP.                                    0002343
BT132  2522-READ-SY-REC.                                                 0002344
BT132      MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WA-KEY.                 0002345
BT132      PERFORM 8700-READ-KEY-WA.                                     0002346
BT132      IF (RTN-CODE NOT = +0)                                        0002347
BT132          GO TO 2523-CONTINUE.                                      0002348
BT302 *                                                                 DELLRET2
BT302      IF WA-FILETYPE = WS-M                                        DELLRET2
BT302        AND SAVE-LS-OVERRIDE-IND = WS-02                           DELLRET2
BT302        AND (WASY-LS-PARAM-38 NOT = SPACES)                        DELLRET2
BT302          MOVE '008' TO WK-STATUS.                                 DELLRET2
BT302 *                                                                 DELLRET2
BT311      IF WASY-FINAL-STATUS = '008'                                 DELLRET2
BT311          MOVE '008' TO WK-STATUS.                                 DELLRET2
PB631      MOVE SPACES TO REFERNCE-OPTION-RECORD.
PB631      MOVE CDC-CO TO CO-03
PB631                  RE-FILE-CO.
PB631      MOVE 03 TO PARM-03.
PB631      MOVE +0 TO RE-RTN-CODE.
PB631      MOVE KEY-03 TO VSAM-RE-PRIM
PB631      READ VSAM-RE
PB631               INTO REFERNCE-OPTION-RECORD
PB631      MOVE VSAM-RE-FS TO WS-INDX-FS-99
PB631      INITIALIZE TPSWNML-AREA
PB631      MOVE KEY-03 TO TPSWNML-FILE-KEY
PB631      MOVE RE-FILE-ID TO TPSWNML-FILE-ID
PB631      MOVE 'DISKREAD' TO TPSWNML-FUNCTION-CODE
PB631      PERFORM P9999-MAP-RESP-CODE
PB631      MOVE WS-INDX-FS-99 TO RE-RTN-CODE.
PB631      IF RE-RTN-CODE = +0
PB631         MOVE OPTX-03-2 (41) TO WS-OPT-141.
PB631      IF WS-OPT-141 = '2'
PB631         PERFORM 9600-UPDATE-INFO11.
BT132      IF WK-STATUS = 009                                            0002349    
BT142        AND (WASY-FINAL-STATUS = 'APPROVE' OR 'DECLINE')            0002350
BT154          MOVE WASY-FINAL-STATUS TO WK-FINAL-STATUS                 0002351
BT132          PERFORM 3600-BRIDGE-POLICY.                               0002352
DL461      IF WK-STATUS = 009
DL461        AND WK-FINAL-STATUS = 'CANCEL'
DL461          PERFORM 3600-BRIDGE-POLICY.
DL90A      IF (SAVE-LS-OVERRIDE-IND = 'SS')
DL90A        AND (WK-FINAL-STATUS = '019')
DL90A          PERFORM 5700-LOCATE-AP                                           
DL90A          PERFORM 3600-BRIDGE-POLICY.
BT132  2523-CONTINUE.                                                    0002353
           PERFORM 7500-SET-ALL-STATUS.                                  0002354
           PERFORM 7600-FINAL-AP-STATUS.                                 0002355
           PERFORM 8500-UPDATE-SY-STATUS.                                0002356
       2530-SET-TASK-RECORD.                                             0002357
           MOVE SPACES TO TASK-RECORD.                                   0002358
BT132      MOVE WASY-TASK-STEP TO TK010-PS-STEP.                         0002359
BT132      MOVE WASY-TASK-PROCESS TO TK010-PS-PROCESS.                   0002360
           PERFORM 3500-GET-WF-DESCRIPTION.                              0002361
NK081      IF NO-TASK-REC-SW = 'Y'                                       0002362
NK081        GO TO 2540-SET-SY-RECORD.                                   0002363
SS521      MOVE WASY-TASK-SOURCE TO TASK-SOURCE.
SS521      MOVE WASY-TASK-INITIATOR TO TASK-INITIATOR.
           MOVE WASY-TASK-PROCESS TO TASK-PROCESS.                       0002364
           MOVE WASY-TASK-STEP TO TASK-STEP.                             0002365
           MOVE SV-CO TO TASK-POLICY-CO.                                 0002366
           MOVE SAVE-PASS-POLICY TO TASK-POLICY-NUM.                     0002367
           MOVE SPACES TO TASK-POLICY-RDR.                               0002368
BT181      MOVE SAVE-EMPLOYER-ID TO TASK-GROUP-NUMBER.                   0002369
KM101      MOVE SAVE-COMT1-WASY TO TASK-COMMENT1-A.                      0002370
KM101      MOVE SAVE-COMT2-WASY TO TASK-COMMENT2-A.                      0002371
KM101      MOVE SAVE-COMT3-WASY TO TASK-COMMENT3-A.                      0002372
           MOVE '9' TO TASK-PRIORITY.                                    0002373
BT154      IF SAVE-LS-OVERRIDE-IND = WS-03                               0002374
BT154          MOVE '5' TO TASK-PRIORITY.                                0002375
           MOVE 'Y' TO TASK-ADDTO-DATABASE.                              0002376
BT142      IF WA-FILETYPE = WS-E OR WS-M                                 0002377
BT132          MOVE SPACES TO TASK-ADDTO-DATABASE.                       0002378
           MOVE WASY-TASK-INDICATOR TO TASK-STATUS.                      0002379
           IF WASY-TASK-INDICATOR = SPACES                               0002380
               MOVE 'P' TO TASK-STATUS.                                  0002381
                                                                         0002382
BT132      IF WK-STATUS = 009 OR 010 OR 020                              0002383
BT043          MOVE 'STP ' TO TASK-RECEIVER-OPID                         0002384
BT043          MOVE 'C' TO TASK-STATUS                                   0002385
BT043      ELSE                                                          0002386
BT043          MOVE 'STP ' TO TASK-SENDER-OPID                           0002387
BT043          MOVE 'P' TO TASK-STATUS.                                  0002388
BT154      IF WK-STATUS = 009                                            0002389
BT154        AND (WK-FINAL-STATUS = WS-APPROVE OR WS-DECLINE)            0002390
BT154          MOVE WS-STPS TO TASK-SENDER-OPID                          0002391
BT154          MOVE WS-4SPACE TO TASK-RECEIVER-OPID                      0002392
BT154          MOVE WS-P TO TASK-STATUS.                                 0002393
BT043                                                                    0002394
BT043      IF NUM-INV = 008                                              0002395
BT043          MOVE 'STP ' TO TASK-SENDER-OPID                           0002396
BT043          MOVE '    ' TO TASK-RECEIVER-OPID                         0002397
BT043          MOVE 'P' TO TASK-STATUS.                                  0002398
MK131      IF WA-POLCRECTYPE = 'P'                                       0002399
BT132          MOVE '    ' TO TASK-SENDER-OPID                           0002400
BT132          MOVE 'STP ' TO TASK-RECEIVER-OPID                         0002401
MK131          MOVE 'C' TO TASK-STATUS.                                  0002402
BT194                                                                    0002403
BT194      IF WK-FINAL-STATUS = WS-PEND-CT                               0002404
BT194          MOVE '    ' TO TASK-SENDER-OPID                           0002405
BT194          MOVE 'STP ' TO TASK-RECEIVER-OPID                         0002406
BT194          MOVE 'C' TO TASK-STATUS.                                  0002407
BT194                                                                    0002408
DL04A      IF WK-FINAL-STATUS = WS-PEND-CT
DL04A         IF (WASY-TASK-PROCESS = 'RPLREQ' AND
DL04A             WASY-TASK-STEP = 'OTH')
DL04A          MOVE WS-STPS TO TASK-SENDER-OPID
DL04A          MOVE WS-4SPACE TO TASK-RECEIVER-OPID
DL04A          MOVE WS-P TO TASK-STATUS.
DL04A
DL461      IF (WK-FINAL-STATUS = 'CANCEL') 
DL461         IF (WK-STATUS = 020)         
DL461             MOVE WS-STPS   TO TASK-SENDER-OPID
DL461             MOVE WS-4SPACE TO TASK-RECEIVER-OPID
DL461             MOVE WS-C      TO TASK-STATUS
DL461          ELSE
DL461             MOVE WS-STPS   TO TASK-SENDER-OPID
DL461             MOVE WS-4SPACE TO TASK-RECEIVER-OPID
DL461             MOVE WS-P      TO TASK-STATUS.                         0002409
           MOVE WK-SYSTEM-DATE TO TASK-PENDING-DATE.                     0002410
RS691      IF (WA-FILETYPE = 'E' AND SAVE-LS-OVERRIDE-IND = 'SS'        STAINT9S
RS691          AND TASK-PROCESS = 'EENRL' AND TASK-STEP = 'DAT')        STAINT9S
NK200A         MOVE WK-SYSTEM-DATE TO TASK-CLOSED-DATE
RS691          MOVE 'C' TO TASK-STATUS.                                 STAINT9S
           IF WASY-TASK-INDICATOR = 'C' OR 'H' OR 'E'                    0002411
NK192          MOVE WK-SYSTEM-TIME TO TASK-CLOSED-TIME                   0002412
               MOVE WK-SYSTEM-DATE TO TASK-CLOSED-DATE.                  0002413
                                                                         0002414
           IF WASY-TASK-INDICATOR = 'O'                                  0002415
NK192          MOVE WK-SYSTEM-TIME TO TASK-OPENED-TIME                   0002416
               MOVE WK-SYSTEM-DATE TO TASK-OPENED-DATE.                  0002417
                                                                         0002418
DL348      IF SAVE-LS-OVERRIDE-IND = WS-04                              DELLRET6
DL353          IF WASY-TASK-STEP = 'RPL'                                DELLRET7
DL348              MOVE 'P' TO TASK-STATUS                              DELLRET7
DL353          ELSE                                                     DELLRET7
NK200A             MOVE WK-SYSTEM-DATE TO TASK-CLOSED-DATE
DL353              MOVE 'C' TO TASK-STATUS.                             DELLRET7
DL651      IF (SAVE-LS-OVERRIDE-IND = '06') AND
DL651         (WASY-TASK-PROCESS = 'APPL')
NK200A         MOVE WK-SYSTEM-DATE TO TASK-CLOSED-DATE
DL651          MOVE WS-C TO TASK-STATUS.
           MOVE 000000000 TO TASK-ID.                                    0002419
           MOVE TASK-ID TO TASK-MASTER-ID.                               0002420
           MOVE WK-SYSTEM-TIME TO TASK-PENDING-TIME.                     0002421
           MOVE WK-SYSTEM-DATE TO TASK-ADDED-DATE.                       0002422
BT091      MOVE WK-SYSTEM-DATE TO TASK-MSTR-PDATE.                       0002423
BT091      MOVE WA-IMAGE-REFID TO TASK-IMAGE-ID.                         0002424
BT193      MOVE TASK-STATUS TO SAVE-TASK-INDICATOR.                      0002425
           PERFORM 4000-TASK-DISKADD.                                    0002426
DL344                                                                   02748000
DL344      IF (SAVE-LS-OVERRIDE-IND = '03') AND                         DELLRET6
DL344         (WK-STATUS = 010)                                         DELLRET6
DL344          GO TO 2532-CONTINUE                                      DELLRET6
DL344      ELSE                                                         DELLRET6
DL352          GO TO 2533-CONTINUE.                                     DELLRET7
DL344                                                                   DELLRET6
DL344  2532-CONTINUE.                                                   DELLRET6
DL344                                                                   DELLRET6
DL344      MOVE TASK-STATUS TO SV-SLA-TASK-STATUS.                      DELLRET6
DL344      MOVE TK010-PS-REC TO SV-SLA-TK010-PS-REC.                    DELLRET6
DL344      MOVE SPACES TO TASK-RECORD.                                  DELLRET6
DL344      MOVE 'POL' TO TK010-PS-STEP.                                 DELLRET6
DL344      MOVE 'DMPLAM' TO TK010-PS-PROCESS.                           DELLRET6
DL344      PERFORM 3500-GET-WF-DESCRIPTION.                             DELLRET6
DL344      IF NO-TASK-REC-SW = 'Y'                                      DELLRET6
DL344          GO TO 2540-SET-SY-RECORD.                                DELLRET6
DL344      MOVE 'DMPLAM' TO TASK-PROCESS.                               DELLRET6
DL344      MOVE 'POL' TO TASK-STEP.                                     DELLRET6
DL344      MOVE SV-CO TO TASK-POLICY-CO.                                DELLRET6
DL344      MOVE SAVE-PASS-POLICY TO TASK-POLICY-NUM.                    DELLRET6
DL344      MOVE SPACES TO TASK-POLICY-RDR.                              DELLRET6
DL344      MOVE SAVE-EMPLOYER-ID TO TASK-GROUP-NUMBER.                  DELLRET6
DL344      MOVE SPACES TO TASK-COMMENT1-A.                              DELLRET6
DL344      MOVE SPACES TO TASK-COMMENT2-A.                              DELLRET6
DL344      MOVE SPACES TO TASK-COMMENT3-A.                              DELLRET6
DL344      MOVE 'STP ' TO TASK-SENDER-OPID.                             DELLRET6
DL344      MOVE '5' TO TASK-PRIORITY.                                   DELLRET6
DL344      MOVE 'Y' TO TASK-ADDTO-DATABASE.                             DELLRET6
DL344      MOVE 'P' TO TASK-STATUS.                                     DELLRET6
DL344      MOVE 000000000 TO TASK-ID.                                   DELLRET6
DL344      MOVE TASK-ID TO TASK-MASTER-ID.                              DELLRET6
DL344      MOVE WK-SYSTEM-DATE TO TASK-PENDING-DATE.                    DELLRET6
DL344      MOVE WK-SYSTEM-TIME TO TASK-PENDING-TIME.                    DELLRET6
DL344      MOVE WK-SYSTEM-DATE TO TASK-ADDED-DATE.                      DELLRET6
DL344      MOVE WK-SYSTEM-DATE TO TASK-MSTR-PDATE.                      DELLRET6
DL344      MOVE WA-IMAGE-REFID TO TASK-IMAGE-ID.                        DELLRET6
DL344      PERFORM 4000-TASK-DISKADD.                                   DELLRET6
DL344      MOVE SV-SLA-TASK-STATUS TO TASK-STATUS.                      DELLRET6
DL344      MOVE SV-SLA-TK010-PS-REC TO TK010-PS-REC.                    DELLRET6
DL352      GO TO 2540-SET-SY-RECORD.                                    DELLRET7
DL352                                                                   DELLRET7
DL352  2533-CONTINUE.                                                   DELLRET7
DL352                                                                   DELLRET7
DL352      IF (SAVE-LS-OVERRIDE-IND = WS-04) AND                        DELLRET7
DL352         (WK-STATUS < 010)                                         DELLRET7
DL352          NEXT SENTENCE                                            DELLRET7
DL352      ELSE                                                         DELLRET7
DL352          GO TO 2540-SET-SY-RECORD.                                DELLRET7
DL352                                                                   DELLRET7
DL352      MOVE TASK-STATUS TO SV-SLA-TASK-STATUS.                      DELLRET7
DL352      MOVE TK010-PS-REC TO SV-SLA-TK010-PS-REC.                    DELLRET7
DL352      MOVE SPACES TO TASK-RECORD.                                  DELLRET7
DL352      MOVE 'EAPPL' TO TK010-PS-PROCESS.                            DELLRET7
DL352      IF WK-STATUS = 009                                           DELLRET7
DL352          MOVE 'DAT' TO TK010-PS-STEP                              DELLRET7
DL352      ELSE                                                         DELLRET7
DL352          MOVE 'NGO' TO TK010-PS-STEP.                             DELLRET7
DL352      PERFORM 3500-GET-WF-DESCRIPTION.                             DELLRET7
DL352      IF NO-TASK-REC-SW = 'Y'                                      DELLRET7
DL352          GO TO 2540-SET-SY-RECORD.                                DELLRET7
DL352      MOVE TK010-PS-PROCESS TO TASK-PROCESS.                       DELLRET7
DL352      MOVE TK010-PS-STEP TO TASK-STEP.                             DELLRET7
DL352      MOVE SV-CO TO TASK-POLICY-CO.                                DELLRET7
DL352      MOVE SAVE-PASS-POLICY TO TASK-POLICY-NUM.                    DELLRET7
DL352      MOVE SPACES TO TASK-POLICY-RDR.                              DELLRET7
DL352      MOVE SAVE-EMPLOYER-ID TO TASK-GROUP-NUMBER.                  DELLRET7
DL352      MOVE SPACES TO TASK-COMMENT1-A.                              DELLRET7
DL352      MOVE SPACES TO TASK-COMMENT2-A.                              DELLRET7
DL352      MOVE SPACES TO TASK-COMMENT3-A.                              DELLRET7
DL352      MOVE 'STP ' TO TASK-SENDER-OPID.                             DELLRET7
DL352      MOVE '5' TO TASK-PRIORITY.                                   DELLRET7
DL352      MOVE 'Y' TO TASK-ADDTO-DATABASE.                             DELLRET7
DL352      MOVE 'P' TO TASK-STATUS.                                     DELLRET7
DL352      MOVE 000000000 TO TASK-ID.                                   DELLRET7
DL352      MOVE TASK-ID TO TASK-MASTER-ID.                              DELLRET7
DL352      MOVE WK-SYSTEM-DATE TO TASK-PENDING-DATE.                    DELLRET7
DL352      MOVE WK-SYSTEM-TIME TO TASK-PENDING-TIME.                    DELLRET7
DL352      MOVE WK-SYSTEM-DATE TO TASK-ADDED-DATE.                      DELLRET7
DL352      MOVE WK-SYSTEM-DATE TO TASK-MSTR-PDATE.                      DELLRET7
DL352      MOVE WA-IMAGE-REFID TO TASK-IMAGE-ID.                        DELLRET7
DL352      PERFORM 4000-TASK-DISKADD.                                   DELLRET7
DL352      MOVE SV-SLA-TASK-STATUS TO TASK-STATUS.                      DELLRET7
DL352      MOVE SV-SLA-TK010-PS-REC TO TK010-PS-REC.                    DELLRET7
DL352      GO TO 2540-SET-SY-RECORD.                                    DELLRET7
DL352                                                                   DELLRET7
NK081  2540-SET-SY-RECORD.                                               0002427
           PERFORM 7700-FINAL-SY-STATUS.                                 0002428
NK081      IF NO-TASK-REC-SW = 'Y'                                       0002429
NK081          GO TO 2599-TASK-RECORD-EXIT.                              0002430
BT043      IF NUM-INV = 008                                              0002431
BT043          GO TO 2599-TASK-RECORD-EXIT.                              0002432
MK131      IF WA-POLCRECTYPE = 'P'                                       0002433
MK131          GO TO 2599-TASK-RECORD-EXIT.                              0002434
BT194      IF (WK-STATUS = 008 OR 009 OR 010 OR 020)                     0002435
BT154        AND (TASK-STATUS = WS-C)                                    0002436
               MOVE SPACES TO HOLD-PROCESS-STEP-RECORD                   0002437
               MOVE TK010-PS-REC TO HOLD-PROCESS-STEPS-INFO              0002438
               PERFORM 2700-SET-TASK-RECORDS.                            0002439
                                                                         0002440
       2599-TASK-RECORD-EXIT.                                            0002441
           EXIT.                                                         0002442
                                                                         0002443
       2600-ERROR-TASKS  SECTION.                                        0002444
           PERFORM 8500-UPDATE-SY-STATUS.                                0002445
           MOVE SPACES TO TASK-RECORD.                                   0002446
BT132      MOVE WASY-TASK-PROCESS TO TK010-PS-PROCESS.                   0002447
BT132      MOVE WASY-TASK-STEP TO TK010-PS-STEP.                         0002448
           PERFORM 3500-GET-WF-DESCRIPTION.                              0002449
NK081      IF NO-TASK-REC-SW = 'Y'                                       0002450
NK081        GO TO 2699-ERROR-TASKS-EXIT.                                0002451
SS521      MOVE WASY-TASK-SOURCE TO TASK-SOURCE.
SS521      MOVE WASY-TASK-INITIATOR TO TASK-INITIATOR.
           MOVE WASY-TASK-PROCESS TO TASK-PROCESS.                       0002452
           MOVE WASY-TASK-STEP TO TASK-STEP.                             0002453
           MOVE SV-CO TO TASK-POLICY-CO.                                 0002454
           MOVE SAVE-TASK-POLICY TO TASK-POLICY-NUM.                     0002455
           MOVE SPACES TO TASK-POLICY-RDR.                               0002456
BT181      MOVE SAVE-EMPLOYER-ID TO TASK-GROUP-NUMBER.                   0002457
KM101      MOVE SAVE-COMT1-WASY TO TASK-COMMENT1-A.                      0002458
KM101      MOVE SAVE-COMT2-WASY TO TASK-COMMENT2-A.                      0002459
KM101      MOVE SAVE-COMT3-WASY TO TASK-COMMENT3-A.                      0002460
           MOVE '9' TO TASK-PRIORITY.                                    0002461
BT154      IF SAVE-LS-OVERRIDE-IND = WS-03                               0002462
BT154          MOVE '5' TO TASK-PRIORITY.                                0002463
           MOVE 'Y' TO TASK-ADDTO-DATABASE.                              0002464
BT142      IF WA-FILETYPE = WS-E OR WS-M                                 0002465
BT132          MOVE SPACES TO TASK-ADDTO-DATABASE.                       0002466
           MOVE 'P' TO TASK-STATUS.                                      0002467
           MOVE 'STP ' TO TASK-SENDER-OPID.                              0002468
MK131      IF WA-POLCRECTYPE = 'P'                                       0002469
MK131          MOVE 'C' TO TASK-STATUS                                   0002470
NK192          MOVE WK-SYSTEM-DATE TO TASK-CLOSED-DATE                   0002471
NK192          MOVE WK-SYSTEM-TIME TO TASK-CLOSED-TIME                   0002472
BT132          MOVE 'STP ' TO TASK-RECEIVER-OPID                         0002473
BT132          MOVE '    ' TO TASK-SENDER-OPID.                          0002474
           MOVE WK-SYSTEM-DATE TO TASK-PENDING-DATE.                     0002475
           MOVE WK-SYSTEM-TIME TO TASK-PENDING-TIME.                     0002476
           MOVE WK-SYSTEM-DATE TO TASK-ADDED-DATE.                       0002477
           MOVE 000000000 TO TASK-ID.                                    0002478
           MOVE TASK-ID TO TASK-MASTER-ID.                               0002479
           MOVE WK-SYSTEM-DATE TO TASK-MSTR-PDATE.                       0002480
           MOVE WA-IMAGE-REFID TO TASK-IMAGE-ID.                         0002481
BT193      MOVE TASK-STATUS TO SAVE-TASK-INDICATOR.                      0002482
           PERFORM 4000-TASK-DISKADD.                                    0002483
       2699-ERROR-TASKS-EXIT.                                            0002484
           EXIT.                                                         0002485
                                                                         0002486
       2700-SET-TASK-RECORDS  SECTION.                                   0002487
           MOVE +0 TO RTN-CODE.                                          0002488
           MOVE +0 TO HPSI-SUB.                                          0002489
       2705-WF-DESCRIPTION-LOOP.                                         0002490
           ADD +1 TO HPSI-SUB.                                           0002491
           IF HPSI-SUB > HPSI-INFO-MAX                                   0002492
               MOVE 'Y' TO ERROR-SWITCH                                  0002493
               GO TO 2799-SET-TASK-RECORDS-EXIT.                         0002494
                                                                         0002495
           IF HPSI-PS-NEXT-PROCESS (HPSI-SUB) = SPACES                   0002496
               GO TO 2799-SET-TASK-RECORDS-EXIT.                         0002497
                                                                         0002498
           IF (HPSI-PS-ALWAYS (HPSI-SUB) NOT = 'Y')                      0002499
               GO TO 2705-WF-DESCRIPTION-LOOP.                           0002500
                                                                         0002501
           MOVE SPACES TO TASK-RECORD.                                   0002502
           PERFORM 2900-GET-WF-DESCRIPTION.                              0002503
           IF RTN-CODE > +0                                              0002504
               GO TO 2799-SET-TASK-RECORDS-EXIT.                         0002505
                                                                         0002506
           MOVE HPSI-PS-NEXT-PROCESS (HPSI-SUB) TO TASK-PROCESS.         0002507
           MOVE HPSI-PS-NEXT-STEP (HPSI-SUB) TO TASK-STEP.               0002508
           MOVE SV-CO TO TASK-POLICY-CO.                                 0002509
           MOVE SAVE-PASS-POLICY TO TASK-POLICY-NUM.                     0002510
           MOVE SPACES TO TASK-POLICY-RDR.                               0002511
BT181      MOVE SAVE-EMPLOYER-ID TO TASK-GROUP-NUMBER.                   0002512
KM101      MOVE SAVE-COMT1-WASY TO TASK-COMMENT1-A.                      0002513
KM101      MOVE SAVE-COMT2-WASY TO TASK-COMMENT2-A.                      0002514
KM101      MOVE SAVE-COMT3-WASY TO TASK-COMMENT3-A.                      0002515
           MOVE '9' TO TASK-PRIORITY.                                    0002516
BT154      IF SAVE-LS-OVERRIDE-IND = WS-03                               0002517
BT154          MOVE '5' TO TASK-PRIORITY.                                0002518
           MOVE 'Y' TO TASK-ADDTO-DATABASE.                              0002519
BT142      IF WA-FILETYPE = WS-E OR WS-M                                 0002520
BT132          MOVE SPACES TO TASK-ADDTO-DATABASE.                       0002521
           MOVE HPSI-PS-NEXT-STATUS (HPSI-SUB) TO TASK-STATUS.           0002522
           IF TASK-STATUS = 'P'                                          0002523
               MOVE 'STP ' TO TASK-SENDER-OPID                           0002524
           ELSE                                                          0002525
               MOVE 'STP ' TO TASK-RECEIVER-OPID.                        0002526
                                                                         0002527
           MOVE WK-SYSTEM-DATE TO TASK-PENDING-DATE.                     0002528
           IF TASK-STATUS = 'C' OR 'H' OR 'E'                            0002529
NK192          MOVE WK-SYSTEM-TIME TO TASK-CLOSED-TIME                   0002530
               MOVE WK-SYSTEM-DATE TO TASK-CLOSED-DATE.                  0002531
                                                                         0002532
           IF TASK-STATUS = 'O'                                          0002533
NK192          MOVE WK-SYSTEM-TIME TO TASK-OPENED-TIME                   0002534
               MOVE WK-SYSTEM-DATE TO TASK-OPENED-DATE.                  0002535
                                                                         0002536
           MOVE WK-SYSTEM-TIME TO TASK-PENDING-TIME.                     0002537
           MOVE WK-SYSTEM-DATE TO TASK-ADDED-DATE.                       0002538
                                                                         0002539
           PERFORM 5000-CALC-ACTION-DATE.                                0002540
           MOVE 000000000 TO TASK-ID.                                    0002541
           MOVE TASK-ID TO TASK-MASTER-ID.                               0002542
           MOVE WK-SYSTEM-DATE TO TASK-MSTR-PDATE.                       0002543
           MOVE WA-IMAGE-REFID TO TASK-IMAGE-ID.                         0002544
           PERFORM 4000-TASK-DISKADD.                                    0002545
           GO TO 2705-WF-DESCRIPTION-LOOP.                               0002546
       2799-SET-TASK-RECORDS-EXIT.                                       0002547
           EXIT.                                                         0002548
                                                                         0002549
       2800-CREATE-LOG-RECORD  SECTION.                                  0002550
           MOVE WK-SYSTEM-TIME-HH TO WS-LOG-HH.                          0002551
           MOVE WK-SYSTEM-TIME-MM TO WS-LOG-MM.                          0002552
           MOVE WK-SYSTEM-TIME-SS TO WS-LOG-SS.                          0002553
           MOVE WK-SYSTEM-TIME-10TH TO WS-LOG-10TH.                      0002554
           MOVE SV-CO TO WS-LOG-FILECO.                                  0002555
           MOVE WK-SYSTEM-DATE TO WS-LOG-DATE.                           0002556
           MOVE KEY-NA-WA TO WS-LOG-KEY.                                 0002557
           MOVE APPLICATION-RECORD-AP TO WS-LOG-APREC.                   0002558
      *    CALL 'WRITE3' USING WS-LOG-REC.                               0002559
           WRITE WRITE3-REC FROM                                        DELLSQCH
                 WS-LOG-REC                                             DELLSQCH
            MOVE WS-WRITE3-FS TO CPY-FS-CODE                            DELLSQCH
            PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.                DELLSQCH
                                                                         0002560
       2899-CREATE-LOG-RECORD-EXIT.                                      0002561
           EXIT.                                                         0002562
                                                                         0002563
       2900-GET-WF-DESCRIPTION  SECTION.                                 0002564
           MOVE 'N' TO ERROR-SWITCH.                                     0002565
           MOVE +0 TO RTN-CODE.                                          0002566
           MOVE 'WF' TO FILE-LIT-WF.                                     0002567
           MOVE 'PS' TO TK010-PS-ID.                                     0002568
           MOVE 'LI' TO TK010-PS-TYPE.                                   0002569
           MOVE HPSI-PS-CO TO FILE-CO-WF                                 0002570
                        TK010-PS-CO.                                     0002571
           MOVE HPSI-PS-NEXT-PROCESS (HPSI-SUB) TO TK010-PS-PROCESS.     0002572
           MOVE HPSI-PS-NEXT-STEP (HPSI-SUB) TO TK010-PS-STEP.           0002573
      *    CALL 'DISKREAD' USING TK010-PS-REC                            0002574
      *                   TK010-PS-KEY                                   0002575
      *                   FILE-NAME-WF                                   0002576
      *                   RTN-CODE.                                      0002577
           MOVE TK010-PS-KEY TO VSAM-WF-PRIM                            DELLIDCH
           READ VSAM-WF                                                 DELLIDCH
                    INTO TK010-PS-REC                                   DELLIDCH
           MOVE VSAM-WF-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE TK010-PS-KEY TO TPSWNML-FILE-KEY                        DELLIDCH
           MOVE FILE-NAME-WF TO TPSWNML-FILE-ID                         DELLIDCH
           MOVE 'DISKREAD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
           IF RTN-CODE = +0                                              0002578
               MOVE TK010-PS-DESCRIPTION TO TASK-DESC                    0002579
               GO TO 2999-GET-WF-DESCRIPTION-EXIT.                       0002580
                                                                         0002581
           DISPLAY 'ERROR ON READ OF WORKFLOW DESCRIPTION '.             0002582
           DISPLAY 'LOCATION IS 2900-GET-WF-DESCRIPTION SECTION '.       0002583
           DISPLAY 'PROCESS IS:  ' HPSI-PS-NEXT-PROCESS (HPSI-SUB).      0002584
           DISPLAY 'STEP IS:  ' HPSI-PS-NEXT-STEP (HPSI-SUB).            0002585
           DISPLAY 'LSNNB402 IS ABENDING...  '.                          0002586
           MOVE +2900 TO ABEND-CODE.                                     0002587
           PERFORM 9900-ABEND-RTN.                                       0002588
                                                                         0002589
       2999-GET-WF-DESCRIPTION-EXIT.                                     0002590
           EXIT.                                                         0002591
                                                                         0002592
       3000-VALID-POLICY-NUMBER  SECTION.                                0002593
BT043      MOVE 'N' TO DUP-POL-SWITCH.                                   0002594
           MOVE POLICYX-C TO SAVE-NEXT-POLICY                            0002595
                          WK-POLICY-NUMBER.                              0002596
BT231      IF SAVE-PAIND-WASY = WS-G                                     0002597
BT231        AND (SAVE-BLOCK-ID-WASY NOT = SPACES)                       0002598
BT231          MOVE LOB-BLOCK-NEXT-POLICYX-94 TO SAVE-NEXT-POLICY        0002599
BT231                                         WK-POLICY-NUMBER.          0002600
BT142      IF (WA-FILETYPE = 'E' AND SAVE-LS-OVERRIDE-IND = '01')        0002601
BT142        OR (WA-FILETYPE = 'M' AND SAVE-LS-OVERRIDE-IND = '02')      0002602
CL131          MOVE LOB-BLOCK-NEXT-POLICYX-94 TO SAVE-NEXT-POLICY        0002603
CL131                                         WK-POLICY-NUMBER.          0002604
           MOVE SPACES TO SAVE-NEW-POLICY.                               0002605
       3005-LOOK-ON-PO-FILE.                                             0002606
           MOVE +0 TO RTN-CODE.                                          0002607
           MOVE SPACES TO LIFE-MASTER-RECORD.                            0002608
BT043      MOVE SV-CO TO LM-CO.                                          0002609
           MOVE SAVE-NEXT-POLICY TO LM-POLICY.                           0002610
           PERFORM 8800-READ-POLMST.                                     0002611
           IF RTN-CODE = +1                                              0002612
             GO TO 3010-LOOK-ON-AP-FILE.                                 0002613
                                                                         0002614
           IF RTN-CODE = +0                                              0002615
BT043          MOVE 'Y' TO DUP-POL-SWITCH                                0002616
BT043          GO TO 3020-VALIDATE-POLICY.                               0002617
                                                                         0002618
           IF RTN-CODE = +12                                             0002619
               DISPLAY ' WA FILE I/O ERROR-PROGRAM RTN-CODE = +12 '      0002620
               DISPLAY ' ERROR IN 3005-LOOK-ON-PO-FILE PARAGRAPH '       0002621
               DISPLAY                                                   0002622
           ' CHECK PO FILE STATUS. . . LSNNB402 IS ABENDING  '           0002622
               MOVE +3005 TO ABEND-CODE                                  0002623
               PERFORM 9900-ABEND-RTN.                                   0002624
                                                                         0002625
       3010-LOOK-ON-AP-FILE.                                             0002626
           MOVE +0 TO RTN-CODE.                                          0002627
           MOVE SPACES TO WA-AP-KEY.                                     0002628
           MOVE SV-CO TO WA-AP-CO.                                       0002629
           MOVE SAVE-NEXT-POLICY TO WA-AP-POLICY.                        0002630
           MOVE 'AP' TO WA-AP-TYPE.                                      0002631
           PERFORM 8900-READ-APPREC.                                     0002632
           IF RTN-CODE = +1                                              0002633
             GO TO 3020-VALIDATE-POLICY.                                 0002634
                                                                         0002635
           IF RTN-CODE = +0                                              0002636
BT043          MOVE 'Y' TO DUP-POL-SWITCH                                0002637
BT043          GO TO 3020-VALIDATE-POLICY.                               0002638
                                                                         0002639
           IF RTN-CODE = +12                                             0002640
               DISPLAY 'I/O ERROR ON READ OF AP FILE - RTN-CODE = +12 '  0002641
               DISPLAY 'LOCATION IS 3010-LOOK-ON-AP-FILE PARAGRAPH '     0002642
               DISPLAY 'PROCESSING POLICY:  ' SAVE-NEXT-POLICY           0002643
               DISPLAY ' . . . LSNNB402 IS ABENDING  '                   0002644
               MOVE +3010 TO ABEND-CODE                                  0002645
               PERFORM 9900-ABEND-RTN.                                   0002646
                                                                         0002647
       3020-VALIDATE-POLICY.                                             0002648
           IF WK-POLICY9 NUMERIC                                         0002649
               GO TO 3030-EIGHT-NUMBERS.                                 0002650
                                                                         0002651
           IF WK-POLICY9-2-8 NUMERIC                                     0002652
               GO TO 3040-SEVEN-NUMBERS.                                 0002653
                                                                         0002654
           IF WK-POLICY9-3-8 NUMERIC                                     0002655
               GO TO 3050-SIX-NUMBERS.                                   0002656
                                                                         0002657
           IF WK-POLICY9-4-8 NUMERIC                                     0002658
               GO TO 3060-FIVE-NUMBERS.                                  0002659
                                                                         0002660
           IF WK-POLICY9-5-8 NUMERIC                                     0002661
               GO TO 3072-FOUR-NUMBERS.                                  0002662
                                                                         0002663
       3030-EIGHT-NUMBERS.                                               0002664
           IF WK-POLICY9 > 99999995                                      0002665
               MOVE 'Y' TO ERROR-SWITCH                                  0002666
               DISPLAY 'POLICY NUMBER MAX AT THE LIMIT - RESET RE FILE ' 0002667
               DISPLAY 'POLICY NUMBER TRIED TO USE:  ' WK-POLICY-NUMBER  0002668
               DISPLAY 'LOCATION IS 3030-EIGHT-NUMBERS '                 0002669
               MOVE +3030 TO ABEND-CODE                                  0002670
               PERFORM 9900-ABEND-RTN.                                   0002671
                                                                         0002672
           ADD 1 TO WK-POLICY9.                                          0002673
           GO TO 3090-SET-NEXT-POL.                                      0002674
       3040-SEVEN-NUMBERS.                                               0002675
           IF WK-POLICY9-2-8 > 9999995                                   0002676
               MOVE 'Y' TO ERROR-SWITCH                                  0002677
               DISPLAY 'POLICY NUMBER MAX AT THE LIMIT - RESET RE FILE ' 0002678
               DISPLAY 'POLICY NUMBER TRIED TO USE:  ' WK-POLICY-NUMBER  0002679
               DISPLAY 'LOCATION IS 3040-SEVEN-NUMBERS '                 0002680
               MOVE +3040 TO ABEND-CODE                                  0002681
               PERFORM 9900-ABEND-RTN.                                   0002682
                                                                         0002683
           ADD 1 TO WK-POLICY9-2-8.                                      0002684
           GO TO 3090-SET-NEXT-POL.                                      0002685
       3050-SIX-NUMBERS.                                                 0002686
           IF WK-POLICY9-3-8 > 999995                                    0002687
               MOVE 'Y' TO ERROR-SWITCH                                  0002688
               DISPLAY 'POLICY NUMBER MAX AT THE LIMIT - RESET RE FILE ' 0002689
               DISPLAY 'POLICY NUMBER TRIED TO USE:  ' WK-POLICY-NUMBER  0002690
               DISPLAY 'LOCATION IS 3050-SIX-NUMBERS '                   0002691
               MOVE +3050 TO ABEND-CODE                                  0002692
               PERFORM 9900-ABEND-RTN.                                   0002693
                                                                         0002694
           ADD 1 TO WK-POLICY9-3-8.                                      0002695
           GO TO 3090-SET-NEXT-POL.                                      0002696
       3060-FIVE-NUMBERS.                                                0002697
           IF WK-POLICY9-4-8 > 99995                                     0002698
               MOVE 'Y' TO ERROR-SWITCH                                  0002699
               DISPLAY 'POLICY NUMBER MAX AT THE LIMIT - RESET RE FILE ' 0002700
               DISPLAY 'POLICY NUMBER TRIED TO USE:  ' WK-POLICY-NUMBER  0002701
               DISPLAY 'LOCATION IS 3060-FIVE-NUMBERS '                  0002702
               MOVE +3060 TO ABEND-CODE                                  0002703
               PERFORM 9900-ABEND-RTN.                                   0002704
                                                                         0002705
           ADD 1 TO WK-POLICY9-4-8.                                      0002706
           GO TO 3090-SET-NEXT-POL.                                      0002707
       3072-FOUR-NUMBERS.                                                0002708
           IF WK-POLICY9-5-8 > 9995                                      0002709
               MOVE 'Y' TO ERROR-SWITCH                                  0002710
               DISPLAY 'POLICY NUMBER MAX AT THE LIMIT - RESET RE FILE ' 0002711
               DISPLAY 'POLICY NUMBER TRIED TO USE:  ' WK-POLICY-NUMBER  0002712
               DISPLAY 'LOCATION IS 3072-FOUR-NUMBERS '                  0002713
               MOVE +3072 TO ABEND-CODE                                  0002714
               PERFORM 9900-ABEND-RTN.                                   0002715
                                                                         0002716
           ADD 1 TO WK-POLICY9-5-8.                                      0002717
       3090-SET-NEXT-POL.                                                0002718
BT043      IF DUP-POL-SWITCH = 'Y'                                       0002719
BT043         MOVE 'N' TO DUP-POL-SWITCH                                 0002720
BT043         MOVE WK-POLICY-NUMBER TO SAVE-NEXT-POLICY                  0002721
BT043         GO TO 3005-LOOK-ON-PO-FILE.                                0002722
           MOVE WK-POLICY-NUMBER TO SAVE-NEW-POLICY.                     0002723
       3099-VALID-POLICY-NUMBER-EXIT.                                    0002724
           EXIT.                                                         0002725
                                                                         0002726
       3100-INITIAL-AP-REC  SECTION.                                     0002727
           MOVE SPACES TO APPLICATION-RECORD-AP.                         0002728
           CALL 'NBS047' USING APPLICATION-RECORD-AP.                    0002729
           MOVE +0 TO RTN-CODE.                                          0002730
           MOVE SV-CO TO CO-NA-WA.                                       0002731
           MOVE SAVE-NEXT-POLICY TO POLICY-NA-WA.                        0002732
MK131      IF WA-POLCRECTYPE = 'U'                                       0002733
MK131          MOVE GU-POLICY TO POLICY-NA-WA.                           0002734
           MOVE 'AP' TO AP-REC-NA-WA.                                    0002735
           MOVE 004 TO CURR-STAT-NA-WA.                                  0002736
      *    CALL 'DISKADD' USING APPLICATION-RECORD-AP                    0002737
      *                  KEY-NA-WA                                       0002738
      *                  AP-FILE-ID                                      0002739
      *                  RTN-CODE.                                       0002740
           MOVE KEY-NA-WA TO VSAM-AP-PRIM                               DELLIDCH
           WRITE VSAM-AP-REC                                            DELLIDCH
                      FROM APPLICATION-RECORD-AP                        DELLIDCH
           MOVE VSAM-AP-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE KEY-NA-WA TO TPSWNML-FILE-KEY                           DELLIDCH
           MOVE AP-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKADD' TO TPSWNML-FUNCTION-CODE                      DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
           IF RTN-CODE = +0                                              0002741
               MOVE SPACES TO NB080-PASS-AREA                            0002742
               MOVE APPLICATION-RECORD-AP TO NB080-RECORD                0002743
      *        MOVE AP-FILE-ID TO NB080-FILE-ID                          0002744
               MOVE AP-FILE-CO TO NB080-FILE-CO                         DELLMACH
               MOVE AP-FILE-LIT TO NB080-FILE-LIT                       DELLMACH
               MOVE KEY-NA-WA TO NB080-REC-KEY                           0002745
               MOVE 'A' TO NB080-ACTION                                  0002746
               CALL 'LSNNB080' USING LSNNB080-PASS-RECORD.               0002747
                                                                         0002748
       3199-INITIAL-AP-REC-EXIT.                                         0002749
           EXIT.                                                         0002750
                                                                         0002751
       3200-INITIAL-CV-REC  SECTION.                                     0002752
           MOVE SPACES TO COVERAGE-RECORD                                0002753
                  COVERAGE-RECORD-CV.                                    0002754
           CALL 'NBS048' USING NEW-BUSINESS-RECORD                       0002755
                        APPLICATION-RECORD-AP.                           0002756
           MOVE +0 TO RTN-CODE.                                          0002757
           MOVE COVERAGE-RECORD TO COVERAGE-RECORD-CV.                   0002758
           MOVE SAVE-NEXT-POLICY TO POLICY-N1-WA.                        0002759
MK131      IF WA-POLCRECTYPE = 'U'                                       0002760
MK131          MOVE GU-POLICY TO POLICY-N1-WA.                           0002761
           MOVE SV-CO TO CO-N1-WA.                                       0002762
           MOVE SV-APPID-WA TO APPID-N1-WA.                              0002763
           MOVE 01 TO N1-REC-N1-WA.                                      0002764
      *    CALL 'DISKADD' USING COVERAGE-RECORD-CV                       0002765
      *                  KEY-N1-WA                                       0002766
      *                  AP-FILE-ID                                      0002767
      *                  RTN-CODE.                                       0002768
           MOVE KEY-N1-WA TO VSAM-AP-PRIM                               DELLIDCH
           WRITE VSAM-AP-REC                                            DELLIDCH
                      FROM COVERAGE-RECORD-CV                           DELLIDCH
           MOVE VSAM-AP-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE KEY-N1-WA TO TPSWNML-FILE-KEY                           DELLIDCH
           MOVE AP-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKADD' TO TPSWNML-FUNCTION-CODE                      DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
           MOVE COVERAGE-RECORD-CV TO COVERAGE-RECORD.                   0002769
           IF RTN-CODE = +0                                              0002770
               MOVE SPACES TO NB080-PASS-AREA                            0002771
               MOVE COVERAGE-RECORD TO NB080-RECORD                      0002772
      *        MOVE AP-FILE-ID TO NB080-FILE-ID                          0002773
               MOVE AP-FILE-CO TO NB080-FILE-CO                         DELLMACH
               MOVE AP-FILE-LIT TO NB080-FILE-LIT                       DELLMACH
BT042          MOVE KEY-N1 TO NB080-REC-KEY                              0002774
               MOVE 'A' TO NB080-ACTION                                  0002775
               CALL 'LSNNB080' USING LSNNB080-PASS-RECORD.               0002776
                                                                         0002777
       3299-INITIAL-CV-REC-EXIT.                                         0002778
           EXIT.                                                         0002779
                                                                         0002780
KM101  3300-ASSIGNED-POLICY  SECTION.                                    0002781
KM101      MOVE 'N' TO ERROR-SWITCH.                                     0002782
KM101      MOVE 'N' TO DUP-POL-SWITCH.                                   0002783
KM101      MOVE SPACES TO SAVE-NEXT-POLICY.                              0002784
KM101  3305-CHECK-PO-FILE.                                               0002785
KM101      MOVE +0 TO RTN-CODE.                                          0002786
KM101      MOVE SPACES TO LIFE-MASTER-RECORD.                            0002787
KM101      MOVE SV-CO TO LM-CO.                                          0002788
KM101      MOVE SAVE-PAPOL-WASY TO LM-POLICY.                            0002789
KM101      PERFORM 8800-READ-POLMST.                                     0002790
KM101      IF RTN-CODE = +1                                              0002791
KM101        GO TO 3310-CHECK-AP-FILE.                                   0002792
KM101                                                                    0002793
KM101      IF RTN-CODE = +0                                              0002794
KM101          MOVE 'Y' TO ERROR-SWITCH                                  0002795
KM101          MOVE 'Y' TO DUP-POL-SWITCH                                0002796
KM101          GO TO 3399-ASSIGNED-POLICY-EXIT.                          0002797
KM101                                                                    0002798
KM101      IF RTN-CODE = +12                                             0002799
KM101          DISPLAY 'I/O ERROR ON READ OF PO FILE - RTN-CODE = +12 '  0002800
KM101          DISPLAY 'LOCATION IS 3305-CHECK-PO-FILE PARAGRAPH '       0002801
KM101          DISPLAY 'PROCESSING POLICY:  ' SAVE-PAPOL-WASY            0002802
KM101          DISPLAY ' . . . LSNNB402 IS ABENDING  '                   0002803
KM101          MOVE +3305 TO ABEND-CODE                                  0002804
KM101          PERFORM 9900-ABEND-RTN.                                   0002805
KM101                                                                    0002806
KM101  3310-CHECK-AP-FILE.                                               0002807
KM101      MOVE +0 TO RTN-CODE.                                          0002808
KM101      MOVE SPACES TO WA-AP-KEY.                                     0002809
KM101      MOVE SV-CO TO WA-AP-CO.                                       0002810
KM101      MOVE SAVE-PAPOL-WASY TO WA-AP-POLICY.                         0002811
KM101      MOVE 'AP' TO WA-AP-TYPE.                                      0002812
KM101      PERFORM 8900-READ-APPREC.                                     0002813
KM101      IF RTN-CODE = +1                                              0002814
KM101          MOVE SAVE-PAPOL-WASY TO SAVE-NEXT-POLICY                  0002815
KM101          GO TO 3399-ASSIGNED-POLICY-EXIT.                          0002816
KM101                                                                    0002817
KM101      IF RTN-CODE = +0                                              0002818
KM101          MOVE 'Y' TO ERROR-SWITCH                                  0002819
KM101          MOVE 'Y' TO DUP-POL-SWITCH                                0002820
KM101          GO TO 3399-ASSIGNED-POLICY-EXIT.                          0002821
KM101                                                                    0002822
KM101      IF RTN-CODE = +12                                             0002823
KM101          DISPLAY 'I/O ERROR ON READ OF AP FILE - RTN-CODE = +12 '  0002824
KM101          DISPLAY 'LOCATION IS 3310-CHECK-AP-FILE PARAGRAPH '       0002825
KM101          DISPLAY 'PROCESSING POLICY:  ' SAVE-PAPOL-WASY            0002826
KM101          DISPLAY ' . . . LSNNB402 IS ABENDING  '                   0002827
KM101          MOVE +3310 TO ABEND-CODE                                  0002828
KM101          PERFORM 9900-ABEND-RTN.                                   0002829
KM101                                                                    0002830
KM101  3399-ASSIGNED-POLICY-EXIT.                                        0002831
KM101      EXIT.                                                         0002832
KM101                                                                    0002833
BT111  3400-SET-BD-STATUS  SECTION.                                      0002834
BT111  3410-GET-BN-REC.                                                  0002835
BT111      MOVE +0 TO RTN-CODE.                                          0002836
BT111      MOVE SPACES TO WA-APPLICATION-RECORD.                         0002837
BT111      MOVE WPK-KEY-ONE TO WA-KEY.                                   0002838
BT111      PERFORM 8700-READ-KEY-WA.                                     0002839
BT111      IF RTN-CODE > +0                                              0002840
SM241          GO TO 3459-SET-BD-STATUS-EXIT.                           R       
BT111      IF (NBBN-LM-SEG-BN-SEQ NOT NUMERIC)                           0002842
BT111        OR (NBBN-LM-SEG-BN-SEQ = 0000)                              0002843
SM241          GO TO 3459-SET-BD-STATUS-EXIT.                           R       
BT111      MOVE NBBN-LM-BN-TYPE TO WK-BN-TYPE-WA.                        0002845
BT111      MOVE NBBN-LM-SEG-BN-SEQ TO WK-BN-SEQ-WA.                      0002846
BT111  3415-GET-BD-REC.                                                  0002847
BT111      MOVE SPACES TO WA-APPLICATION-RECORD.                         0002848
BT111      MOVE WPK-KEY-ONE TO WK-SEARCH-WA-KEY.                         0002849
BT111      MOVE +0 TO RTN-CODE.                                          0002850
DL431 *    MOVE 01 TO SWA-REC-SEQ.                                      DELLRT15
BT111      MOVE 'BD' TO SWA-REC-ID.                                      0002852
BT111      MOVE WK-SEARCH-WA-KEY TO WA-KEY.                              0002853
BT111  3420-INITIAL-WA-NBBD-READ.                                        0002854
BT111      PERFORM 8700-READ-KEY-WA.                                     0002855
BT111                                                                    0002856
BT111      IF RTN-CODE > +0                                              0002857
SM241          GO TO 3459-SET-BD-STATUS-EXIT.                           R       
BT111                                                                    0002859
BT111      GO TO 3440-CHECK-NBBD-REC.                                    0002860
BT111  3430-READ-WA-LOOP.                                                0002861
BT111      PERFORM 8300-READ-NEXT-WA-RECS.                               0002862
BT111                                                                    0002863
BT111      IF RTN-CODE > +0                                              0002864
SM241          GO TO 3459-SET-BD-STATUS-EXIT.                           R       
DL457      ADD +1 TO WS-REC-READ.                                       DELLRT17
BT111                                                                    0002866
BT111  3440-CHECK-NBBD-REC.                                              0002867
BT111      IF WA-DELL-GUID = SWA-DELL-GUID                               0002868
BT111        AND WA-CO = SWA-CO                                          0002869
BT111        AND WA-TRANS-TYPE = SWA-TRANS-TYPE                          0002870
BT111        AND WA-FILE-ID = 'NB'                                       0002871
BT111        AND WA-REC-ID = 'BD'                                        0002872
BT111          GO TO 3445-BD-BN-MATCH.                                   0002873
BT111                                                                    0002874
SM241      GO TO 3459-SET-BD-STATUS-EXIT.                               R       
BT111  3445-BD-BN-MATCH.                                                 0002876
BT111      IF NBBD-LM-SEG-BD-SEQ = WK-BN-SEQ-WA                          0002877
BT111        AND NBBD-LM-BD-TYPE = WK-BN-TYPE-WA                         0002878
BT111          GO TO 3450-BUILD-SEG-BD.                                  0002879
BT111                                                                    0002880
BT111      GO TO 3430-READ-WA-LOOP.                                      0002881
BT111  3450-BUILD-SEG-BD.                                                0002882
BT111 *    CALL 'DISKHOLD' USING WA-APPLICATION-RECORD                   0002883
BT111 *                   WA-KEY                                         0002884
BT111 *                   WA-FILE-IDZ                                    0002885
BT111 *                   RTN-CODE.                                      0002886
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           READ VSAM-WA                                                 DELLIDCH
                    INTO WA-APPLICATION-RECORD                          DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
BT111                                                                    0002887
BT111      IF RTN-CODE > +0                                              0002888
SM241          GO TO 3459-SET-BD-STATUS-EXIT.                           R       
BT111                                                                    0002890
BT111      IF NUM-BN = 008                                               0002891
BT111          MOVE 008 TO WA-STATUS                                     0002892
BT111      ELSE                                                          0002893
BT111          MOVE 009 TO WA-STATUS.                                    0002894
BT111                                                                    0002895
BT111  3455-WA-DISKUP.                                                   0002896
BT111 *    CALL 'DISKUP' USING WA-APPLICATION-RECORD                     0002897
BT111 *                 WA-KEY                                           0002898
BT111 *                 WA-FILE-IDZ                                      0002899
BT111 *                 RTN-CODE.                                        0002900
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           REWRITE VSAM-WA-REC                                          DELLIDCH
                      FROM WA-APPLICATION-RECORD                        DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE                       DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
BT111                                                                    0002901
BT111 *    IF (RTN-CODE NOT = +0                                        DELLMNCH
BT111 *        CALL 'TPFIRLFN' USING WA-FILE-IDZ.                       DELLMNCH
BT111                                                                    0002904
SM241  3459-SET-BD-STATUS-EXIT.                                         R       
BT111      EXIT.                                                         0002906
SM241                                                                   R       
SM241  3460-SET-TP-STATUS  SECTION.                                     R       
SM241  3460-GET-BN-TP-REC.                                              R       
SM241      MOVE +0 TO RTN-CODE.                                         R       
SM241      MOVE SPACES TO WA-APPLICATION-RECORD.                        R       
SM241      MOVE WPK-KEY-ONE TO WA-KEY.                                  R       
SM241      PERFORM 8700-READ-KEY-WA.                                    R       
SM241      IF RTN-CODE > +0                                             R       
SM241          GO TO 3499-SET-TP-STATUS-EXIT.                           R       
SM241      IF (NBBN-LM-SEG-BN-SEQ NOT NUMERIC)                          R       
SM241        OR (NBBN-LM-SEG-BN-SEQ = 0000)                             R       
SM241          GO TO 3499-SET-TP-STATUS-EXIT.                           R       
SM241      MOVE NBBN-LM-BN-TYPE TO WK-BN-TYPE-WA.                       R       
SM241      MOVE NBBN-LM-SEG-BN-SEQ TO WK-BN-SEQ-WA.                     R       
SM241  3465-GET-TP-REC.                                                 R       
SM241      MOVE SPACES TO WA-APPLICATION-RECORD.                        R       
SM241      MOVE WPK-KEY-ONE TO WK-SEARCH-WA-KEY.                        R       
SM241      MOVE +0 TO RTN-CODE.                                         R       
SM241      MOVE 01 TO SWA-REC-SEQ.                                      R       
SM241      MOVE 'TP' TO SWA-REC-ID.                                     R       
SM241      MOVE WK-SEARCH-WA-KEY TO WA-KEY.                             R       
SM241  3470-INITIAL-WA-NBTP-READ.                                       R       
SM241      PERFORM 8700-READ-KEY-WA.                                    R       
SM241                                                                   R       
SM241      IF RTN-CODE > +0                                             R       
SM241          GO TO 3499-SET-TP-STATUS-EXIT.                           R       
SM241                                                                   R       
SM241      GO TO 3480-CHECK-NBTP-REC.                                   R       
SM241  3475-READ-WA-LOOP.                                               R       
SM241      PERFORM 8300-READ-NEXT-WA-RECS.                              R       
SM241                                                                   R       
SM241      IF RTN-CODE > +0                                             R       
SM241          GO TO 3499-SET-TP-STATUS-EXIT.                           R       
DL457      ADD +1 TO WS-REC-READ.                                       DELLRT17
SM241                                                                   R       
SM241  3480-CHECK-NBTP-REC.                                             R       
SM241      IF WA-DELL-GUID = SWA-DELL-GUID                              R       
SM241        AND WA-CO = SWA-CO                                         R       
SM241        AND WA-TRANS-TYPE = SWA-TRANS-TYPE                         R       
SM241        AND WA-FILE-ID = 'NB'                                      R       
SM241        AND WA-REC-ID = 'TP'                                       R       
SM241          GO TO 3485-TP-BN-MATCH.                                  R       
SM241                                                                   R       
SM241      GO TO 3499-SET-TP-STATUS-EXIT.                               R       
SM241  3485-TP-BN-MATCH.                                                R       
SM241      IF (NBTP-LM-SEG-TP-SEQ = WK-BN-SEQ-WA)                       R       
SM241        AND (NBTP-LM-TP-TYPE = WK-BN-TYPE-WA)                      R       
SM241          GO TO 3490-BUILD-SEG-TP.                                 R       
SM241                                                                   R       
SM241      GO TO 3475-READ-WA-LOOP.                                     R       
SM241  3490-BUILD-SEG-TP.                                               R       
SM241 *    CALL 'DISKHOLD' USING WA-APPLICATION-RECORD                  R       
SM241 *                   WA-KEY                                        R       
SM241 *                   WA-FILE-IDZ                                   R       
SM241 *                   RTN-CODE.                                     R       
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           READ VSAM-WA                                                 DELLIDCH
                    INTO WA-APPLICATION-RECORD                          DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
SM241                                                                   R       
SM241      IF RTN-CODE > +0                                             R       
SM241          GO TO 3499-SET-TP-STATUS-EXIT.                           R       
SM241                                                                   R       
SM241      IF NUM-BN = 008                                              R       
SM241          MOVE 008 TO WA-STATUS                                    R       
SM241      ELSE                                                         R       
SM241          MOVE 009 TO WA-STATUS.                                   R       
SM241                                                                   R       
SM241  3495-WA-DISKUP.                                                  R       
SM241 *    CALL 'DISKUP' USING WA-APPLICATION-RECORD                    R       
SM241 *                 WA-KEY                                          R       
SM241 *                 WA-FILE-IDZ                                     R       
SM241 *                 RTN-CODE.                                       R       
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           REWRITE VSAM-WA-REC                                          DELLIDCH
                      FROM WA-APPLICATION-RECORD                        DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE                       DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
SM241                                                                   R       
SM241 *    IF (RTN-CODE NOT = +0)                                       DELLMNCH
SM241 *        CALL 'TPFIRLFN' USING WA-FILE-IDZ.                       DELLMNCH
SM241                                                                   R       
SM241  3499-SET-TP-STATUS-EXIT.                                         R       
SM241      EXIT.                                                        R       
SM241                                                                   R       
       3500-GET-WF-DESCRIPTION  SECTION.                                 0002908
BT132      MOVE 'N' TO NO-TASK-REC-SW.                                   0002909
           MOVE +0 TO RTN-CODE-WF                                        0002910
                   RTN-CODE.                                             0002911
           MOVE 'WF' TO FILE-LIT-WF.                                     0002912
           MOVE 'PS' TO TK010-PS-ID.                                     0002913
           MOVE 'LI' TO TK010-PS-TYPE.                                   0002914
           MOVE SV-CO TO FILE-CO-WF                                      0002915
                      TK010-PS-CO.                                       0002916
      *    CALL 'DISKREAD' USING TK010-PS-REC                            0002917
      *                   TK010-PS-KEY                                   0002918
      *                   FILE-NAME-WF                                   0002919
      *                   RTN-CODE-WF.                                   0002920
           MOVE TK010-PS-KEY TO VSAM-WF-PRIM                            DELLIDCH
           READ VSAM-WF                                                 DELLIDCH
                    INTO TK010-PS-REC                                   DELLIDCH
           MOVE VSAM-WF-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE TK010-PS-KEY TO TPSWNML-FILE-KEY                        DELLIDCH
           MOVE FILE-NAME-WF TO TPSWNML-FILE-ID                         DELLIDCH
           MOVE 'DISKREAD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE-WF.                           DELLIDCH
           MOVE RTN-CODE-WF TO RTN-CODE.                                 0002921
           IF RTN-CODE = +0                                              0002922
               MOVE TK010-PS-DESCRIPTION TO TASK-DESC                    0002923
               GO TO 3599-GET-WF-DESCRIPTION-EXIT.                       0002924
                                                                         0002925
NK081      MOVE 'Y' TO NO-TASK-REC-SW.                                   0002926
NK081      MOVE 'E' TO WASY-TASK-INDICATOR.                              0002927
                                                                         0002928
       3599-GET-WF-DESCRIPTION-EXIT.                                     0002929
           EXIT.                                                         0002930
                                                                         0002931
CL131  3600-BRIDGE-POLICY  SECTION.                                      0002932
CL131      MOVE SPACES TO COMPANY-OPTION-RECORD.                         0002933
CL131      MOVE SV-CO TO RE-FILE-CO.                                     0002934
CL131      MOVE 'RE' TO RE-FILE-LIT.                                     0002935
CL131      MOVE SPACES TO KEY-94.                                        0002936
CL131      MOVE SV-CO TO CO-94.                                          0002937
CL131      MOVE 94 TO PARM-94.                                           0002938
CL131      MOVE +0 TO RTN-CODE.                                          0002939
CL131      MOVE '9STP' TO LOB-94.                                        0002940
CL131 *    CALL 'DISKREAD' USING COMPANY-OPTION-RECORD                   0002941
CL131 *                   KEY-94                                         0002942
CL131 *                   RE-FILE-ID                                     0002943
CL131 *                   RTN-CODE.                                      0002944
           MOVE KEY-94 TO VSAM-RE-PRIM                                  DELLIDCH
           READ VSAM-RE                                                 DELLIDCH
                    INTO COMPANY-OPTION-RECORD                          DELLIDCH
           MOVE VSAM-RE-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE KEY-94 TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE RE-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKREAD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
CL131      IF RTN-CODE = +12                                             0002945
CL131          MOVE 'E49' TO WK-ERROR-CODE                               0002946
CL131          PERFORM 3700-SY-ERR-MSG                                   0002947
CL131          MOVE 'Y' TO ERROR-SWITCH                                  0002948
CL131          GO TO 3699-BRIDGE-POLICY-EXIT.                            0002949
CL131      IF RTN-CODE > +0                                              0002950
CL131          MOVE 'E49' TO WK-ERROR-CODE                               0002951
CL131          PERFORM 3700-SY-ERR-MSG                                   0002952
CL131          MOVE 'Y' TO ERROR-SWITCH                                  0002953
CL131          GO TO 3699-BRIDGE-POLICY-EXIT.                            0002954
CL131                                                                    0002955
BT361      IF WA-FILETYPE = WS-M                                        DELLRET8
BT361        AND SAVE-LS-OVERRIDE-IND = WS-02                           DELLRET8
BT361        AND LOB-BLOCK-NEXT-POLICY-94 > WK-BRIDGE-LIMIT-02          DELLRET8
BT361          MOVE 'E50' TO WK-ERROR-CODE                              DELLRET8
BT361          PERFORM 3700-SY-ERR-MSG                                  DELLRET8
BT361          MOVE 'Y' TO ERROR-SWITCH                                 DELLRET8
BT361          GO TO 3699-BRIDGE-POLICY-EXIT.                           DELLRET8
BT361                                                                   DELLRET8
BT361      IF WA-FILETYPE = WS-M                                        DELLRET8
BT361        AND SAVE-LS-OVERRIDE-IND = WS-02                           DELLRET8
BT361          GO TO 3610-CHECK-POLICY.                                 DELLRET8
BT361                                                                   DELLRET8
CL131      IF LOB-BLOCK-NEXT-POLICY-94 > WK-BRIDGE-LIMIT                 0002956
CL131          MOVE 'E50' TO WK-ERROR-CODE                               0002957
CL131          PERFORM 3700-SY-ERR-MSG                                   0002958
CL131          MOVE 'Y' TO ERROR-SWITCH                                  0002959
CL131          GO TO 3699-BRIDGE-POLICY-EXIT.                            0002960
CL131                                                                    0002961
VC291      IF LOB-BLOCK-NEXT-POLICY-94 > WK-BRIDGE-LIMIT-SS             DELLRET2
VC291          MOVE 'E50' TO WK-ERROR-CODE                              DELLRET2
VC291          PERFORM 3700-SY-ERR-MSG                                  DELLRET2
VC291          MOVE 'Y' TO ERROR-SWITCH                                 DELLRET2
VC291          GO TO 3699-BRIDGE-POLICY-EXIT.                           DELLRET2
VC291                                                                   DELLRET2
BT349  3610-CHECK-POLICY.                                               DELLRET6
CL131      MOVE SPACES TO APPLICATION-RECORD-AP.                         0002962
CL131      MOVE SV-CO TO CO-NA-WA.                                       0002963
CL131      MOVE 'AP' TO AP-REC-NA-WA.                                    0002964
CL131      MOVE SPACES TO WORK-AREA-N340.                                0002965
CL131      MOVE +0 TO SCRN-CURSOR                                        0002966
CL131              SCRN-AID                                              0002967
CL131              SCRN-ERR.                                             0002968
CL131      MOVE SPACES TO COMPANY-OPTION-RECORD.                         0002969
CL131      MOVE +0 TO RTN-CODE.                                          0002970
CL131      MOVE SV-CO TO CO-C.                                           0002971
CL131      MOVE 0 TO PARM-C.                                             0002972
CL131 *    CALL 'DISKREAD' USING COMPANY-OPTION-RECORD                   0002973
CL131 *                   KEY-C                                          0002974
CL131 *                   RE-FILE-ID                                     0002975
CL131 *                   RTN-CODE.                                      0002976
           MOVE KEY-C TO VSAM-RE-PRIM                                   DELLIDCH
           READ VSAM-RE                                                 DELLIDCH
                    INTO COMPANY-OPTION-RECORD                          DELLIDCH
           MOVE VSAM-RE-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE KEY-C TO TPSWNML-FILE-KEY                               DELLIDCH
           MOVE RE-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKREAD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
CL131      IF RTN-CODE > 1                                               0002977
CL131          MOVE '003' TO WPK-ERR-CODE                                0002978
BT142          MOVE 'E66' TO WK-ERROR-CODE                               0002979
BT142          PERFORM 3700-SY-ERR-MSG                                   0002980
CL131          GO TO 3699-BRIDGE-POLICY-EXIT.                            0002981
CL131      IF RTN-CODE = +1                                              0002982
CL131          MOVE '003' TO WPK-ERR-CODE                                0002983
BT142          MOVE 'E67' TO WK-ERROR-CODE                               0002984
BT142          PERFORM 3700-SY-ERR-MSG                                   0002985
BT142          GO TO 3699-BRIDGE-POLICY-EXIT.                            0002986
CL131                                                                    0002987
CL131      MOVE +0 TO RTN-CODE.                                          0002988
CL131      MOVE SV-CO TO AP-FILE-CO.                                     0002989
CL131      MOVE 'AP' TO AP-FILE-LIT.                                     0002990
CL131      MOVE SAVE-NEXT-POLICY TO POLICY-NA-WA.                        0002991
CL131 *    CALL 'DISKHOLD' USING APPLICATION-RECORD-AP                   0002992
CL131 *                   KEY-NA-WA                                      0002993
CL131 *                   AP-FILE-ID                                     0002994
CL131 *                   RTN-CODE.                                      0002995
           MOVE KEY-NA-WA TO VSAM-AP-PRIM                               DELLIDCH
           READ VSAM-AP                                                 DELLIDCH
                    INTO APPLICATION-RECORD-AP                          DELLIDCH
           MOVE VSAM-AP-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE KEY-NA-WA TO TPSWNML-FILE-KEY                           DELLIDCH
           MOVE AP-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
CL131      IF RTN-CODE > +0                                              0002996
CL131          MOVE '003' TO WPK-ERR-CODE                                0002997
BT142          MOVE 'E68' TO WK-ERROR-CODE                               0002998
BT142          PERFORM 3700-SY-ERR-MSG                                   0002999
CL131          GO TO 3699-BRIDGE-POLICY-EXIT.                            0003000
CL131                                                                    0003001
CL131      MOVE APPLICATION-RECORD-AP TO BEFORE-REC-N340.                0003002
CL131      MOVE '03' TO LS193-CHOICE.                                    0003003
CL131      MOVE 'U' TO LS193-ACTION.                                     0003004
CL131      MOVE SAVE-NEXT-POLICY TO POLICY-NA-WA.                        0003005
CL131      MOVE 'STP ' TO LU-OPID-NA-WA.                                 0003006
CL131      MOVE 'SO' TO LU-UPDATE-NA-WA.                                 0003007
CL131      MOVE WK-SYSTEM-DATE TO LU-DATE-NA-WA.                         0003008
CL131      MOVE REFUND-SUSP-FLAG-C TO REFUND-SUSP-FLAG-NA-WA.            0003009
CL131      IF WASY-FINAL-STATUS = 'APPROVE'                              0003010
CL131          MOVE 010 TO UW-DCSN-NA-WA                                 0003011
CL131          MOVE 110 TO UW-SUB-DCSN-NA-WA.                            0003012
CL131      IF WASY-FINAL-STATUS = 'DECLINE'                              0003013
CL131          MOVE 020 TO UW-DCSN-NA-WA                                 0003014
CL131          MOVE 220 TO UW-SUB-DCSN-NA-WA.                            0003015
DL461      IF WASY-FINAL-STATUS = 'CANCEL'
DL461          MOVE 030 TO UW-DCSN-NA-WA
DL461          MOVE 330 TO UW-SUB-DCSN-NA-WA
DL461          PERFORM 7400-WRITE-LTR089.
DL90A      IF WK-FINAL-STATUS = '019'
DL90A          MOVE SAVE-UW-DCSN-NA     TO UW-DCSN-NA-WA
DL90A          MOVE SAVE-UW-SUB-DCSN-NA TO UW-SUB-DCSN-NA-WA
DL90A      END-IF.
CL131      MOVE WK-SYSTEM-DATE TO UW-DCSNDATE-NA-WA.                     0003016
CL131      CALL 'NBS340' USING WORK-AREA-N340                            0003017
CL131                   BEFORE-REC-N340                                  0003018
CL131                   APPLICATION-RECORD-AP                            0003019
CL131                   LS193-SCREEN                                     0003020
CL131                   LS193-MSG                                        0003021
CL131                   REFERNCE-OPTION-RECORD                           0003022
CL131                   WORK-AREA1.                                      0003023
CL131      IF RTNCD-TEMP-C > +50                                         0003024
BT142          MOVE 'E69' TO WK-ERROR-CODE                               0003025
BT142          PERFORM 3700-SY-ERR-MSG                                   0003026
BT142          MOVE RTNCD-TEMP-C TO NBS340-RTN-CODE                      0003027
BT142          MOVE NBS340-RTN-CODEX TO WK-ERROR-CODE                    0003028
BT142          PERFORM 3700-SY-ERR-MSG                                   0003029
CL131          GO TO 3610-RELEASE-RECORD.                                0003030
CL131 *-------------------------------------------------*                0003031
CL131 * NBS340 DOES NOT DISKUP AP RECORD SO DO IT HERE. *                0003032
CL131 *-------------------------------------------------*                0003033
CL131      MOVE +0 TO RTN-CODE.                                          0003034
CL131      MOVE SV-CO TO AP-FILE-CO.                                     0003035
CL131      MOVE 'AP' TO AP-FILE-LIT.                                     0003036
CL131 *    CALL 'DISKUP' USING APPLICATION-RECORD-AP                     0003037
CL131 *                 KEY-NA-WA                                        0003038
CL131 *                 AP-FILE-ID                                       0003039
CL131 *                 RTN-CODE.                                        0003040
           MOVE KEY-NA-WA TO VSAM-AP-PRIM                               DELLIDCH
           REWRITE VSAM-AP-REC                                          DELLIDCH
                      FROM APPLICATION-RECORD-AP                        DELLIDCH
           MOVE VSAM-AP-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE KEY-NA-WA TO TPSWNML-FILE-KEY                           DELLIDCH
           MOVE AP-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE                       DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
CL131      IF RTN-CODE > +0                                              0003041
BT142          MOVE 'E70' TO WK-ERROR-CODE                               0003042
BT142          PERFORM 3700-SY-ERR-MSG                                   0003043
CL131          GO TO 3610-RELEASE-RECORD.                                0003044
CL131      IF CURR-STAT-NA-WA = 019                                      0003045
CL131         MOVE '020' TO WK-STATUS.                                   0003046
CL131      IF CURR-STAT-NA-WA = 010                                      0003047
CL131         MOVE '010' TO WK-STATUS.                                   0003048
CL131      MOVE SPACES TO NB080-PASS-AREA.                               0003049
CL131      MOVE APPLICATION-RECORD-AP TO NB080-RECORD.                   0003050
CL131 *    MOVE AP-FILE-ID TO NB080-FILE-ID.                             0003051
           MOVE AP-FILE-CO TO NB080-FILE-CO                             DELLMACH
           MOVE AP-FILE-LIT TO NB080-FILE-LIT                           DELLMACH
CL131      MOVE KEY-NA-WA TO NB080-REC-KEY.                              0003052
CL131      MOVE 'U' TO NB080-ACTION.                                     0003053
CL131      CALL 'LSNNB080' USING LSNNB080-PASS-RECORD.                   0003054
CL131                                                                    0003055
CL131      MOVE SPACES TO COMPANY-OPTION-RECORD.                         0003056
CL131      MOVE SV-CO TO RE-FILE-CO.                                     0003057
CL131      MOVE 'RE' TO RE-FILE-LIT.                                     0003058
CL131      MOVE SPACES TO KEY-94.                                        0003059
CL131      MOVE SV-CO TO CO-94.                                          0003060
CL131      MOVE 94 TO PARM-94.                                           0003061
CL131      MOVE +0 TO RTN-CODE.                                          0003062
CL131      MOVE '9STP' TO LOB-94.                                        0003063
CL131 *    CALL 'DISKREAD' USING COMPANY-OPTION-RECORD                   0003064
CL131 *                   KEY-94                                         0003065
CL131 *                   RE-FILE-ID                                     0003066
CL131 *                   RTN-CODE.                                      0003067
           MOVE KEY-94 TO VSAM-RE-PRIM                                  DELLIDCH
           READ VSAM-RE                                                 DELLIDCH
                    INTO COMPANY-OPTION-RECORD                          DELLIDCH
           MOVE VSAM-RE-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE KEY-94 TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE RE-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKREAD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
CL131      IF RTN-CODE = +12                                             0003068
CL131          MOVE '003' TO WPK-ERR-CODE                                0003069
CL131          GO TO 3699-BRIDGE-POLICY-EXIT.                            0003070
CL131      IF RTN-CODE > +0                                              0003071
CL131          MOVE '003' TO WPK-ERR-CODE                                0003072
BT142          MOVE 'E71' TO WK-ERROR-CODE                               0003073
BT142          PERFORM 3700-SY-ERR-MSG                                   0003074
CL131          GO TO 3699-BRIDGE-POLICY-EXIT.                            0003075
CL131                                                                    0003076
CL131      MOVE +0 TO WS-SUB1.                                           0003077
CL131  3620E-HOLD-RE-FILE.                                               0003078
CL131 *    CALL 'DISKHOLD' USING COMPANY-OPTION-RECORD                   0003079
CL131 *                   KEY-94                                         0003080
CL131 *                   RE-FILE-ID                                     0003081
CL131 *                   RTN-CODE.                                      0003082
           MOVE KEY-94 TO VSAM-RE-PRIM                                  DELLIDCH
           READ VSAM-RE                                                 DELLIDCH
                    INTO COMPANY-OPTION-RECORD                          DELLIDCH
           MOVE VSAM-RE-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE KEY-94 TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE RE-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
CL131      IF RTN-CODE = +0                                              0003083
CL131          GO TO 3630E-UPDATE-RE-FILE.                               0003084
CL131                                                                    0003085
CL131 *    CALL 'TIMERIT' USING WK-WAIT-TENTHS-SEC.                      0003086
           MOVE +1 TO  WK-WAIT-TENTHS-SEC                               DELLMNCH
           CALL "CEE3DLY" USING  WK-WAIT-TENTHS-SEC, RC.                DELLMNCH
CL131      ADD +1 TO WS-SUB1.                                            0003087
CL131      IF WS-SUB1 < 4                                                0003088
CL131          GO TO 3620E-HOLD-RE-FILE.                                 0003089
CL131                                                                    0003090
CL131      IF RTN-CODE = +12                                             0003091
CL131          MOVE '003' TO WPK-ERR-CODE                                0003092
CL131          GO TO 3699-BRIDGE-POLICY-EXIT.                            0003093
CL131      GO TO 3699-BRIDGE-POLICY-EXIT.                                0003094
CL131                                                                    0003095
CL131  3630E-UPDATE-RE-FILE.                                             0003096
CL131      IF LOB-BLOCK-NEXT-POLICYX-94 NUMERIC                          0003097
CL131          ADD 1 TO LOB-BLOCK-NEXT-POLICY-94                         0003098
CL131      ELSE                                                          0003099
CL131 *        CALL 'TPFIRLFN' USING RE-FILE-ID                         DELLMNCH
CL131 *                       REL-RTN-CODE                              DELLMNCH
CL131          MOVE '003' TO WPK-ERR-CODE                                0003102
CL131          GO TO 3699-BRIDGE-POLICY-EXIT.                            0003103
CL131 *    CALL 'DISKUP' USING COMPANY-OPTION-RECORD                     0003104
CL131 *                 KEY-94                                           0003105
CL131 *                 RE-FILE-ID                                       0003106
CL131 *                 RTN-CODE.                                        0003107
           MOVE KEY-94 TO VSAM-RE-PRIM                                  DELLIDCH
           REWRITE VSAM-RE-REC                                          DELLIDCH
                      FROM COMPANY-OPTION-RECORD                        DELLIDCH
           MOVE VSAM-RE-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE KEY-94 TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE RE-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE                       DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
CL131      IF RTN-CODE = +12                                             0003108
CL131          MOVE '003' TO WPK-ERR-CODE                                0003109
CL131          GO TO 3699-BRIDGE-POLICY-EXIT.                            0003110
CL131                                                                    0003111
CL131                                                                    0003112
CL131      IF (RTN-CODE NOT = +0)                                        0003113
CL131 *        CALL 'TPFIRLFN' USING RE-FILE-ID                         DELLMNCH
CL131 *                       REL-RTN-CODE                              DELLMNCH
CL131          MOVE '003' TO WPK-ERR-CODE                                0003116
CL131          GO TO 3699-BRIDGE-POLICY-EXIT.                            0003117
CL131      GO TO 3699-BRIDGE-POLICY-EXIT.                                0003118
CL131  3610-RELEASE-RECORD.                                              0003119
CL131 *     CALL 'TPFIRLFN' USING AP-FILE-ID                            DELLMNCH
CL131 *                    RTN-CODE.                                    DELLMNCH
CL131                                                                    0003122
CL131  3699-BRIDGE-POLICY-EXIT.                                          0003123
CL131      EXIT.                                                         0003124
CL131                                                                    0003125
CL131  3700-SY-ERR-MSG  SECTION.                                         0003126
CL131      MOVE +0 TO WS-SUB1.                                           0003127
CL131      MOVE +1 TO WS-SUB2.                                           0003128
CL131  3710-LOCATE-SY-LOOP.                                              0003129
CL131      ADD +1 TO WS-SUB1.                                            0003130
CL131      IF WS-SUB1 > WTR-TABLE-MAX                                    0003131
CL131          MOVE 'Y' TO ERROR-SWITCH                                  0003132
CL131          GO TO 3799-SY-ERR-MSG-EXIT.                               0003133
CL131                                                                    0003134
CL131      IF WTR-FILE-ID (WS-SUB1) = 'WA'                               0003135
CL131        AND WTR-REC-ID (WS-SUB1) = 'SY'                             0003136
CL131          GO TO 3720-READ-SY-REC.                                   0003137
CL131                                                                    0003138
CL131      GO TO 3710-LOCATE-SY-LOOP.                                    0003139
CL131  3720-READ-SY-REC.                                                 0003140
CL131      MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WA-KEY.                 0003141
CL131      PERFORM 8700-READ-KEY-WA.                                     0003142
CL131      IF RTN-CODE = +0                                              0003143
CL131          GO TO 3730-HAVE-SY-REC.                                   0003144
CL131      IF RTN-CODE > +0                                              0003145
CL131          MOVE 'Y' TO ERROR-SWITCH                                  0003146
CL131 *        M '001' TO WPK-ERR-CODE                                   0003147
CL131          GO TO 3799-SY-ERR-MSG-EXIT.                               0003148
CL131  3730-HAVE-SY-REC.                                                 0003149
CL131      MOVE +0 TO ERR-INDX.                                          0003150
CL131  3740-UPDATE-ERROR-LOOP.                                           0003151
CL131      ADD +1 TO ERR-INDX                                            0003152
CL131      IF ERR-INDX > +50                                             0003153
CL131          GO TO 3799-SY-ERR-MSG-EXIT.                               0003154
CL131      IF WASY-ERROR-CODE (ERR-INDX) = WK-ERROR-CODE                 0003155
CL131          GO TO 3799-SY-ERR-MSG-EXIT.                               0003156
CL131      IF WASY-ERROR-CODE (ERR-INDX) NOT = SPACES                    0003157
CL131          GO TO 3740-UPDATE-ERROR-LOOP.                             0003158
CL131 *    CALL 'DISKHOLD' USING WA-APPLICATION-RECORD                   0003159
CL131 *                   WA-KEY                                         0003160
CL131 *                   WA-FILE-IDZ                                    0003161
CL131 *                   RTN-CODE.                                      0003162
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           READ VSAM-WA                                                 DELLIDCH
                    INTO WA-APPLICATION-RECORD                          DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
CL131                                                                    0003163
CL131                                                                    0003164
CL131      IF RTN-CODE > +0                                              0003165
CL131          MOVE 'Y' TO ERROR-SWITCH                                  0003166
CL131          GO TO 3799-SY-ERR-MSG-EXIT.                               0003167
CL131      MOVE WK-ERROR-CODE TO WASY-ERROR-CODE (ERR-INDX).             0003168
CL131 *    CALL 'DISKUP' USING WA-APPLICATION-RECORD                     0003169
CL131 *                 WA-KEY                                           0003170
CL131 *                 WA-FILE-IDZ                                      0003171
CL131 *                 RTN-CODE.                                        0003172
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           REWRITE VSAM-WA-REC                                          DELLIDCH
                      FROM WA-APPLICATION-RECORD                        DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE                       DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
CL131      IF RTN-CODE > +0                                              0003173
CL131          MOVE 'Y' TO ERROR-SWITCH                                  0003174
CL131          GO TO 3799-SY-ERR-MSG-EXIT.                               0003175
CL131  3799-SY-ERR-MSG-EXIT.                                             0003176
CL131      EXIT.                                                         0003177
CL131                                                                    0003178
CL131  3800-DELETE-AB-RECS  SECTION.                                     0003179
CL131      MOVE +0 TO RTN-CODE                                           0003180
CL131              WS-SUB1.                                              0003181
CL131      MOVE +1 TO WS-SUB2.                                           0003182
CL131  3810-LOCATE-AB-LOOP.                                              0003183
CL131      ADD +1 TO WS-SUB1.                                            0003184
CL131      IF WS-SUB1 > WTR-TABLE-MAX                                    0003185
CL131          GO TO 3899-DELETE-AB-RECS-EXIT.                           0003186
CL131                                                                    0003187
CL131      IF WTR-FILE-ID (WS-SUB1) = 'NB'                               0003188
CL131        AND WTR-REC-ID (WS-SUB1) = 'AB'                             0003189
CL131          GO TO 3820-CHECK-AB-REC.                                  0003190
CL131                                                                    0003191
CL131      GO TO 3810-LOCATE-AB-LOOP.                                    0003192
CL131                                                                    0003193
CL131  3820-CHECK-AB-REC.                                                0003194
CL131      MOVE SPACES TO WA-APPLICATION-RECORD.                         0003195
CL131      MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WA-KEY.                 0003196
CL131      IF WA-TOTAL-RECS = 999                                        0003197
CL131          GO TO 3830-DELETE-AB-REC                                  0003198
CL131      ELSE                                                          0003199
CL131          GO TO 3810-LOCATE-AB-LOOP.                                0003200
CL131                                                                    0003201
CL131  3830-DELETE-AB-REC.                                               0003202
CL131                                                                    0003203
CL131 *    CALL 'DISKDEL' USING WA-APPLICATION-RECORD                    0003204
CL131 *                  WA-KEY                                          0003205
CL131 *                  WA-FILE-IDZ                                     0003206
CL131 *                  RTN-CODE.                                       0003207
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           DELETE VSAM-WA RECORD                                        DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKDEL' TO TPSWNML-FUNCTION-CODE                      DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
CL131                                                                    0003208
CL131      IF RTN-CODE = +0                                              0003209
CL131          GO TO 3810-LOCATE-AB-LOOP.                                0003210
CL131      IF RTN-CODE > +0                                              0003211
CL131          MOVE 'Y' TO ERROR-SWITCH                                  0003212
CL131          MOVE '001' TO WPK-ERR-CODE                                0003213
CL131          GO TO 3899-DELETE-AB-RECS-EXIT.                           0003214
CL131  3899-DELETE-AB-RECS-EXIT.                                         0003215
CL131      EXIT.                                                         0003216
CL131                                                                    0003217
MK131  3800-INIT-AP-NB-REC  SECTION.                                     0003218
MK131      PERFORM 9000-READ-GU.                                         0003219
MK131      IF GU-POLICY = SAVE-PAPOL-WASY                                0003220
MK131          NEXT SENTENCE                                             0003221
MK131      ELSE                                                          0003222
MK131          MOVE 'PIP' TO WK-STATUS                                   0003223
MK131          PERFORM 7000-STATUS-ERROR-WA                              0003224
MK131          MOVE GU-POLICY TO SAVE-TASK-POLICY                        0003225
MK131          PERFORM 2600-ERROR-TASKS                                  0003226
MK131          MOVE 'Y' TO ERROR-SWITCH                                  0003227
MK131          GO TO 3899-INIT-AP-NB-REC-EXIT.                           0003228
MK131                                                                    0003229
MK131  3805-LOOK-ON-PO-FILE.                                             0003230
MK131      MOVE +0 TO RTN-CODE.                                          0003231
MK131      MOVE SPACES TO LIFE-MASTER-RECORD.                            0003232
MK131      MOVE SV-CO TO LM-CO.                                          0003233
MK131      MOVE GU-POLICY TO LM-POLICY.                                  0003234
MK131      PERFORM 8800-READ-POLMST.                                     0003235
MK131      IF RTN-CODE = +0                                              0003236
MK131          MOVE 'PNU' TO WK-STATUS                                   0003237
MK131          PERFORM 7000-STATUS-ERROR-WA                              0003238
MK131          MOVE GU-POLICY TO SAVE-TASK-POLICY                        0003239
MK131          PERFORM 2600-ERROR-TASKS                                  0003240
MK131          MOVE 'Y' TO ERROR-SWITCH                                  0003241
MK131          GO TO 3899-INIT-AP-NB-REC-EXIT.                           0003242
MK131                                                                    0003243
MK131      IF RTN-CODE = +12                                             0003244
MK131          DISPLAY ' WA FILE I/O ERROR-PROGRAM RTN-CODE = +12 '      0003245
MK131          DISPLAY ' ERROR IN 3805-LOOK-ON-PO-FILE PARAGRAPH '       0003246
MK131          DISPLAY                                                   0003247
MK131      ' CHECK PO FILE STATUS. . . LSNNB402 IS ABENDING  '           0003247
MK131          MOVE +3805 TO ABEND-CODE                                  0003248
MK131          PERFORM 9900-ABEND-RTN.                                   0003249
MK131                                                                    0003250
MK131  3810-LOOK-ON-AP-FILE.                                             0003251
MK131      MOVE +0 TO RTN-CODE.                                          0003252
MK131      MOVE SPACES TO WA-AP-KEY.                                     0003253
MK131      MOVE SV-CO TO WA-AP-CO.                                       0003254
MK131      MOVE GU-POLICY TO WA-AP-POLICY.                               0003255
MK131      MOVE 'AP' TO WA-AP-TYPE.                                      0003256
MK131      PERFORM 8900-READ-APPREC.                                     0003257
MK131      IF RTN-CODE = +1                                              0003258
MK131          MOVE 'PNU' TO WK-STATUS                                   0003259
MK131          PERFORM 7000-STATUS-ERROR-WA                              0003260
MK131          MOVE GU-POLICY TO SAVE-TASK-POLICY                        0003261
MK131          PERFORM 2600-ERROR-TASKS                                  0003262
MK131          MOVE 'Y' TO ERROR-SWITCH                                  0003263
MK131          GO TO 3899-INIT-AP-NB-REC-EXIT.                           0003264
MK131                                                                    0003265
MK131      IF RTN-CODE = +12                                             0003266
MK131          DISPLAY ' I/O ERROR ON READ OF AP FILE - RTN-CODE = +12 ' 0003267
MK131          DISPLAY ' LOCATION IS 3810-LOOK-ON-AP-FILE PARAGRAPH '    0003268
MK131          DISPLAY ' PROCESSING POLICY:  ' SAVE-NEXT-POLICY          0003269
MK131          DISPLAY '  . . . LSNNB402 IS ABENDING  '                  0003270
MK131          MOVE +3810 TO ABEND-CODE                                  0003271
MK131          PERFORM 9900-ABEND-RTN.                                   0003272
MK131                                                                    0003273
MK131      IF (RTN-CODE = +0)                                            0003274
MK131        AND (CURR-STAT-NA-WA = 008 OR 009)                          0003275
MK131          NEXT SENTENCE                                             0003276
MK131      ELSE                                                          0003277
MK131          MOVE 'PNU' TO WK-STATUS                                   0003278
MK131          PERFORM 7000-STATUS-ERROR-WA                              0003279
MK131          MOVE GU-POLICY TO SAVE-TASK-POLICY                        0003280
MK131          PERFORM 2600-ERROR-TASKS                                  0003281
MK131          MOVE 'Y' TO ERROR-SWITCH                                  0003282
MK131          GO TO 3899-INIT-AP-NB-REC-EXIT.                           0003283
MK131 ******DELETE OTHER COVERAGE, OR , 02 AND CA RECORDS.               0003284
MK131      PERFORM 7800-DEL-ALL-AP-RECORDS.                              0003285
MK131 ******INITIALIZE AP, 01 AND NB RECORDS.                            0003286
MK131      PERFORM 3900-INITIALIZE-RECORDS.                              0003287
BT153      MOVE +0 TO RTN-CODE.                                          0003288
BT153      MOVE SPACES TO GU-KEY.                                        0003289
BT153      MOVE HWA-CLIENT-GUID TO GU-CLIENT-GUID.                       0003290
BT153  3815-GU-DISKHOLD.                                                 0003291
BT153 *    CALL 'DISKHOLD' USING GU-GUID-RECORD                          0003292
BT153 *                   GU-KEY                                         0003293
BT153 *                   GU-FILE-ID                                     0003294
BT153 *                   RTN-CODE.                                      0003295
           MOVE GU-KEY TO VSAM-GU-PRIM                                  DELLIDCH
           READ VSAM-GU                                                 DELLIDCH
                    INTO GU-GUID-RECORD                                 DELLIDCH
           MOVE VSAM-GU-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE GU-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE GU-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
BT153                                                                    0003296
BT153      IF RTN-CODE > +0                                              0003297
BT153          DISPLAY 'ERROR DISKHOLD GU FILE, RTN-CODE = +' RTN-CODE   0003298
BT153          DISPLAY 'LOCATION IS 3815-GU-DISKHOLD  PARAGRAPH '        0003299
BT153          DISPLAY 'LSNNB402 IS ABENDING... CHECK GU FILE STATUS '   0003300
BT153          MOVE +3815 TO ABEND-CODE                                  0003301
BT153          PERFORM 9900-ABEND-RTN.                                   0003302
BT153                                                                    0003303
BT153      MOVE HWA-IMAGE-REFID TO GU-IMAGE-REFID.                       0003304
BT153      MOVE WK-SYSTEM-DATE TO GU-FINAL-DATE.                         0003305
BT153      MOVE WK-SYSTEM-TIME TO GU-FINAL-TIME.                         0003306
BT153                                                                    0003307
BT153  3820-GU-DISKUP.                                                   0003308
BT153 *    CALL 'DISKUP' USING GU-GUID-RECORD                            0003309
BT153 *                 GU-KEY                                           0003310
BT153 *                 GU-FILE-ID                                       0003311
BT153 *                 RTN-CODE.                                        0003312
           MOVE GU-KEY TO VSAM-GU-PRIM                                  DELLIDCH
           REWRITE VSAM-GU-REC                                          DELLIDCH
                      FROM GU-GUID-RECORD                               DELLIDCH
           MOVE VSAM-GU-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE GU-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE GU-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE                       DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
BT153                                                                    0003313
BT153      IF (RTN-CODE NOT = +0)                                        0003314
BT153 *        CALL 'TPFIRLFN' USING GU-FILE-ID                         DELLMNCH
BT153 *                       REL-RTN-CODE                              DELLMNCH
BT153          DISPLAY 'ERROR DISKUP GU FILE, RTN-CODE = +' RTN-CODE     0003317
BT153          DISPLAY 'LOCATION IS 3820-GU-DISKUP      PARAGRAPH '      0003318
BT153          DISPLAY 'LSNNB402 IS ABENDING... CHECK GU FILE STATUS '   0003319
BT153          MOVE +3820 TO ABEND-CODE                                  0003320
BT153          PERFORM 9900-ABEND-RTN.                                   0003321
BT153                                                                    0003322
MK131  3899-INIT-AP-NB-REC-EXIT.                                         0003323
MK131      EXIT.                                                         0003324
MK131                                                                    0003325
MK131  3900-INITIALIZE-RECORDS  SECTION.                                 0003326
MK131      PERFORM 3100-INITIAL-AP-REC.                                  0003327
MK131      IF RTN-CODE = +0                                              0003328
MK131          GO TO 3920-INITIALIZE-COV-REC.                            0003329
MK131                                                                    0003330
MK131      MOVE 'ERR' TO WK-STATUS.                                      0003331
MK131      PERFORM 7200-APPLICATION-ERROR.                               0003332
MK131      PERFORM 2600-ERROR-TASKS.                                     0003333
MK131      GO TO 3999-INITIALIZE-RECORDS-EXIT.                           0003334
MK131  3920-INITIALIZE-COV-REC.                                          0003335
MK131      MOVE 01 TO SV-APPID-WA.                                       0003336
MK131      PERFORM 3200-INITIAL-CV-REC.                                  0003337
MK131      IF RTN-CODE = +0                                              0003338
MK131          GO TO 3930-INITIALIZE-NB-REC.                             0003339
MK131                                                                    0003340
MK131      MOVE 'ERR' TO WK-STATUS.                                      0003341
MK131      PERFORM 7200-APPLICATION-ERROR.                               0003342
MK131      PERFORM 2600-ERROR-TASKS.                                     0003343
MK131      GO TO 3999-INITIALIZE-RECORDS-EXIT.                           0003344
MK131  3930-INITIALIZE-NB-REC.                                           0003345
MK131      MOVE LIFE-MASTER-ZAP TO LIFE-MASTER-RECORD.                   0003346
MK131      MOVE SV-CO TO LM-CO.                                          0003347
MK131      MOVE SAVE-PAPOL-WASY TO LM-POLICY.                            0003348
MK131 *    CALL 'DISKHOLD' USING LIFE-MASTER-RECORD                      0003349
MK131 *                   LM-POLID                                       0003350
MK131 *                   NB-FILE-ID                                     0003351
MK131 *                   RTN-CODE.                                      0003352
           MOVE LM-POLID TO VSAM-NB-PRIM                                DELLIDCH
           READ VSAM-NB                                                 DELLIDCH
                    INTO LIFE-MASTER-RECORD                             DELLIDCH
           MOVE VSAM-NB-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE LM-POLID TO TPSWNML-FILE-KEY                            DELLIDCH
           MOVE NB-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
MK131      IF RTN-CODE = +0                                              0003353
MK131          GO TO 3950-NB-CALL-NBS368.                                0003354
MK131                                                                    0003355
MK131      MOVE 'ERR' TO WK-STATUS.                                      0003356
MK131      PERFORM 7200-APPLICATION-ERROR.                               0003357
MK131      PERFORM 2600-ERROR-TASKS.                                     0003358
MK131      GO TO 3999-INITIALIZE-RECORDS-EXIT.                           0003359
MK131  3950-NB-CALL-NBS368.                                              0003360
MK131      MOVE SPACES TO NBS368-PASS-AREA.                              0003361
MK131      MOVE 'N' TO CS368-CREATE-SEGS-FLAG.                           0003362
MK131      MOVE SV-CO TO CS368-CO.                                       0003363
MK131      MOVE SAVE-PAPOL-WASY TO CS368-POL.                            0003364
MK131      MOVE 'Y' TO CS368-ZAP-FLAG.                                   0003365
MK131      CALL 'NBS368' USING COVERAGE-RECORD-CV                        0003366
MK131                   APPLICATION-RECORD-AP                            0003367
MK131                   LIFE-MASTER-RECORD                               0003368
MK131                   REFERNCE-OPTION-RECORD                           0003369
MK131                   NBS368-PASS-AREA.                                0003370
MK131                                                                    0003371
MK131      MOVE SPACES TO LM-POLID.                                      0003372
MK131      MOVE SV-CO TO LM-CO.                                          0003373
MK131      MOVE +0 TO RTN-CODE.                                          0003374
MK131      MOVE SAVE-PAPOL-WASY TO LM-POLICY.                            0003375
MK131 *    CALL 'DISKUP' USING LIFE-MASTER-RECORD                        0003376
MK131 *                 LM-POLID                                         0003377
MK131 *                 NB-FILE-ID                                       0003378
MK131 *                 RTN-CODE.                                        0003379
           MOVE LM-POLID TO VSAM-NB-PRIM                                DELLIDCH
           MOVE LIFE-MASTER-RECORD(1:6) TO LMR-VARL                     DELLMNCH
           REWRITE VSAM-NB-REC                                          DELLIDCH
                      FROM LIFE-MASTER-RECORD                           DELLIDCH
           MOVE VSAM-NB-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE LM-POLID TO TPSWNML-FILE-KEY                            DELLIDCH
           MOVE NB-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE                       DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
MK131      IF (RTN-CODE NOT = +0)                                        0003380
MK131 *        CALL 'TPFIRLFN' USING NB-FILE-ID                         DELLMNCH
MK131 *                       REL-RTN-CODE                              DELLMNCH
MK131          MOVE 'ERR' TO WK-STATUS                                   0003383
MK131          PERFORM 7200-APPLICATION-ERROR                            0003384
MK131          PERFORM 2600-ERROR-TASKS.                                 0003385
MK131      MOVE 003 TO WK-STATUS                                         0003386
MK131               NUM-SY.                                              0003387
MK131      PERFORM 8500-UPDATE-SY-STATUS.                                0003388
MK131  3999-INITIALIZE-RECORDS-EXIT.                                     0003389
MK131      EXIT.                                                         0003390
                                                                         0003391
       4000-TASK-DISKADD  SECTION.                                       0003392
PS452      IF TASK-RECORD > SPACES                                      
PS452          NEXT SENTENCE                                            
PS452      ELSE                                                         
PS452          GO TO 4099-TASK-DISKADD-EXIT.                            
AS541      MOVE CDC-CO TO TASK-COMPANY.
SM211 *    CALL 'WRITE4' USING TASK-RECORD.                              0003393
           WRITE WRITE4-REC FROM                                        DELLSQCH
                 TASK-RECORD                                            DELLSQCH
            MOVE WS-WRITE4-FS TO CPY-FS-CODE                            DELLSQCH
            PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.                DELLSQCH
DL457      ADD +1 TO WS-REC-WRITE4.                                     DELLRT17
       4099-TASK-DISKADD-EXIT.                                           0003394
           EXIT.                                                         0003395
                                                                         0003396
BT132  4100-ADDL-TASKS  SECTION.                                         0003397
BT132      MOVE SPACES TO SY-ADDL-TASK-TABLE.                            0003398
BT132      MOVE +0 TO WS-SUB1.                                           0003399
BT132      MOVE +1 TO WS-SUB2.                                           0003400
BT132  4110-LOCATE-SY-LOOP.                                              0003401
BT132      ADD +1 TO WS-SUB1.                                            0003402
BT132      IF WS-SUB1 > WTR-TABLE-MAX                                    0003403
BT132          MOVE WS-Y TO ERROR-SWITCH                                 0003404
BT132          GO TO 4199-ADDL-TASKS-EXIT.                               0003405
BT132                                                                    0003406
BT132      IF WTR-FILE-ID (WS-SUB1) = WS-WA                              0003407
BT132        AND WTR-REC-ID (WS-SUB1) = WS-SY                            0003408
BT132          GO TO 4120-READ-SY-REC.                                   0003409
BT132                                                                    0003410
BT132      GO TO 4110-LOCATE-SY-LOOP.                                    0003411
BT132  4120-READ-SY-REC.                                                 0003412
BT132      MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WA-KEY.                 0003413
BT132      PERFORM 8700-READ-KEY-WA.                                     0003414
BT132      IF RTN-CODE = +0                                              0003415
BT132          GO TO 4130-HAVE-SY-REC.                                   0003416
BT132      IF RTN-CODE > +0                                              0003417
BT132          DISPLAY                                                   0003418
BT132      'ERROR ON READ OF WA FILE - RTN-CODE = +' RTN-CODE            0003418
BT132          DISPLAY '@ LOCATION 4120-READ-SY-REC    PARAGRAPH'        0003419
BT132          DISPLAY                                                   0003420
BT132      'KEY PROCESSED IS:  ' WTR-WA-KEY (WS-SUB1, WS-SUB2)           0003420
BT132          DISPLAY 'CHECK WA FILE STATUS... LSNNB402 IS ABENDING '   0003421
BT132          MOVE +4120 TO ABEND-CODE                                  0003422
BT132          PERFORM 9900-ABEND-RTN.                                   0003423
BT132  4130-HAVE-SY-REC.                                                 0003424
BT132      MOVE WASY-LS-ADD-TASK-1 TO SY-KEY (1).                        0003425
BT132      MOVE WASY-LS-ADD-TASK-2 TO SY-KEY (2).                        0003426
BT132      MOVE WASY-LS-ADD-TASK-3 TO SY-KEY (3).                        0003427
BT132      MOVE WASY-LS-ADD-TASK-4 TO SY-KEY (4).                        0003428
BT132      MOVE WASY-LS-ADD-TASK-5 TO SY-KEY (5).                        0003429
BT132      MOVE WASY-LS-ADD-TASK-6 TO SY-KEY (6).                        0003430
BT132      MOVE WASY-LS-ADD-TASK-7 TO SY-KEY (7).                        0003431
BT132      MOVE WASY-LS-ADD-TASK-8 TO SY-KEY (8).                        0003432
BT132      MOVE WASY-LS-ADD-TASK-9 TO SY-KEY (9).                        0003433
BT132      MOVE WASY-LS-ADD-TASK-10 TO SY-KEY (10).                      0003434
BT132                                                                    0003435
BT132      MOVE +0 TO WS-SUB1.                                           0003436
BT132                                                                    0003437
BT132  4140-TK-LOOP.                                                     0003438
BT132      ADD +1 TO WS-SUB1.                                            0003439
BT132                                                                    0003440
BT132      IF WS-SUB1 > SY-TABLE-MAX                                     0003441
BT132          GO TO 4199-ADDL-TASKS-EXIT.                               0003442
BT132      IF SY-KEY (WS-SUB1) = SPACES                                  0003443
BT132          GO TO 4140-TK-LOOP.                                       0003444
PB882      IF (SY-KEY (3) NOT = SPACES)
PB882         AND (NB452-EENRL-CLOSE = WS-Y)
PB882          GO TO 4140-TK-LOOP.
BT132                                                                    0003445
BT132      MOVE SPACES TO TASK-RECORD.                                   0003446
BT132      MOVE SY-PROCESS (WS-SUB1) TO TK010-PS-PROCESS.                0003447
BT132      MOVE SY-STEP (WS-SUB1) TO TK010-PS-STEP.                      0003448
BT132      PERFORM 3500-GET-WF-DESCRIPTION.                              0003449
BT132      IF RTN-CODE > +0                                              0003450
BT132          GO TO 4140-TK-LOOP.                                       0003451
BT132                                                                    0003452
BT132      MOVE SY-PROCESS (WS-SUB1) TO TASK-PROCESS.                    0003453
BT132      MOVE SY-STEP (WS-SUB1) TO TASK-STEP.                          0003454
BT132      MOVE SV-CO TO TASK-POLICY-CO.                                 0003455
BT132      MOVE SAVE-PASS-POLICY TO TASK-POLICY-NUM.                     0003456
BT132      MOVE SPACES TO TASK-POLICY-RDR.                               0003457
BT181      MOVE SAVE-EMPLOYER-ID TO TASK-GROUP-NUMBER.                   0003458
BT132      MOVE SAVE-COMT1-WASY TO TASK-COMMENT1-A.                      0003459
PB883      IF TASK-PROCESS = 'EENRL'
PB883         AND TASK-STEP = 'CHG'
PB883         MOVE HWA-DELL-GUID TO TASK-COMMENT1-A. 
BT132      MOVE SAVE-COMT2-WASY TO TASK-COMMENT2-A.                      0003460
BT132      MOVE SAVE-COMT3-WASY TO TASK-COMMENT3-A.                      0003461
BT132      MOVE SY-PRIORITY (WS-SUB1) TO TASK-PRIORITY.                  0003462
BT132      MOVE WS-Y TO TASK-ADDTO-DATABASE.                             0003463
BT142      IF WA-FILETYPE = WS-E OR WS-M                                 0003464
BT132          MOVE SPACES TO TASK-ADDTO-DATABASE.                       0003465
BT132      MOVE SY-STATUS (WS-SUB1) TO TASK-STATUS.                      0003466
BT132      IF TASK-STATUS = WS-C                                         0003467
BT132          MOVE WS-STPS TO TASK-RECEIVER-OPID                        0003468
BT132      ELSE                                                          0003469
BT132          MOVE WS-STPS TO TASK-SENDER-OPID.                         0003470
BT132                                                                    0003471
BT132      MOVE WK-SYSTEM-DATE TO TASK-PENDING-DATE.                     0003472
BT132      IF TASK-STATUS = WS-C                                         0003473
NK192          MOVE WK-SYSTEM-TIME TO TASK-CLOSED-TIME                   0003474
BT132          MOVE WK-SYSTEM-DATE TO TASK-CLOSED-DATE.                  0003475
BT132                                                                    0003476
BT132      IF TASK-STATUS = WS-O                                         0003477
NK192          MOVE WK-SYSTEM-TIME TO TASK-OPENED-TIME                   0003478
BT132          MOVE WK-SYSTEM-DATE TO TASK-OPENED-DATE.                  0003479
BT132                                                                    0003480
BT132      MOVE WK-SYSTEM-TIME TO TASK-PENDING-TIME.                     0003481
BT132      MOVE WK-SYSTEM-DATE TO TASK-ADDED-DATE.                       0003482
BT132                                                                    0003483
DL651      IF SAVE-LS-OVERRIDE-IND = '06' AND TASK-PROCESS = 'IFMAIL'
DL651          IF ((WK-FINAL-STATUS = '009') OR 
DL651              (WK-FINAL-STATUS = 'APPROVE' 
DL651                AND CURR-STAT-NA-WA NOT = '010') OR
DL651              (WK-FINAL-STATUS = 'DECLINE' 
DL651                AND CURR-STAT-NA-WA NOT = '019') OR
DL651              (WK-FINAL-STATUS = 'CANCEL' 
DL651                AND CURR-STAT-NA-WA NOT = '019'))
DL651       	   NEXT SENTENCE
DL651          ELSE
DL651              GO TO 4140-TK-LOOP
DL651          END-IF
DL651      END-IF.
DL391      IF SAVE-LS-OVERRIDE-IND = '06'                               DELLRT11
DL391        AND TASK-STATUS = WS-H                                     DELLRT11
AS951        AND 'POLICY' = SY-PROCESS (WS-SUB1)
AS951        AND 'QA ' = SY-STEP (WS-SUB1)
AS951          PERFORM 5100-CALC-ACTION-DATE
AS951 *        MOVE ISS-DATE-NA-WA TO TASK-ACTION-DATE                  DELLRT11
DL391          GO TO 4150-CONT.                                         DELLRT11
BT154      IF SAVE-LS-OVERRIDE-IND = WS-03                               0003484
BT154        AND TASK-STATUS = WS-H                                      0003485
BT154          PERFORM 5100-CALC-ACTION-DATE                            DELLRET2
BT292          GO TO 4150-CONT.                                         DELLRET2
BT292      IF TASK-STATUS = WS-H                                        DELLRET2
BT292          PERFORM 5100-CALC-ACTION-DATE.                           DELLRET2
BT292  4150-CONT.                                                       DELLRET2
BT132      MOVE 000000000 TO TASK-ID.                                    0003487
BT132      MOVE TASK-ID TO TASK-MASTER-ID.                               0003488
BT132      MOVE WK-SYSTEM-DATE TO TASK-MSTR-PDATE.                       0003489
BT132      MOVE WA-IMAGE-REFID TO TASK-IMAGE-ID.                         0003490
BT132      PERFORM 4000-TASK-DISKADD.                                    0003491
BT132      IF RTN-CODE > +0                                              0003492
BT132          GO TO 4140-TK-LOOP.                                       0003493
BT132      GO TO 4140-TK-LOOP.                                           0003494
BT132                                                                    0003495
BT132  4199-ADDL-TASKS-EXIT.                                             0003496
BT132      EXIT.                                                         0003497
BT132                                                                    0003498
BP931  4111-ADD-POLICY-CV01-WA  SECTION.
BP931      MOVE SPACES TO WA-APPLICATION-RECORD
                WS-KEY.
BP931      MOVE +0 TO RTN-CODE.
BP931      MOVE HWA-TRANS-TYPE TO WS-TRANS-TYPE.
BP931      MOVE SV-CO TO WS-CO.
BP931      MOVE HWA-DELL-GUID TO WS-DELL-GUID.
BP931      MOVE HWA-TOTAL-RECS TO WS-TOTAL-RECS
BP931                    HOLD-TOTAL-RECS.
BP931      MOVE 000 TO WS-DUP-SEQ.
BP931 * set to read wacv0101 record
BP931      MOVE 'AP' TO WS-FILE-ID.
BP931      MOVE 'CV' TO WS-REC-ID.
BP931      MOVE '01' TO WS-REC-APPID-X.
BP931      MOVE '01' TO WS-REC-SEQ-X.
BP931      MOVE WS-KEY TO WA-KEY.
BP931      PERFORM 8700-READ-KEY-WA.
BP931      IF RTN-CODE > +0
BP931         GO TO 4199-ADD-POLICY-CV01-WA-EXIT.
BP931  4112-CONTINUE.
BP931 *    CALL 'DISKHOLD' USING WA-APPLICATION-RECORD
BP931 *                        WA-KEY
BP931 *                        WA-FILE-IDZ
BP931 *                        RTN-CODE.
BP931      MOVE WA-KEY TO VSAM-WA-PRIM
BP931      READ VSAM-WA
BP931           INTO WA-APPLICATION-RECORD
BP931      MOVE VSAM-WA-FS TO WS-INDX-FS-99
BP931      INITIALIZE TPSWNML-AREA
BP931      MOVE WA-KEY TO TPSWNML-FILE-KEY
BP931      MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID
BP931      MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE
BP931      PERFORM P9999-MAP-RESP-CODE
BP931      MOVE WS-INDX-FS-99 TO RTN-CODE.
BP931      IF RTN-CODE > +0
BP931         DISPLAY
BP931     ' WA FILE I/O ERROR-PROGRAM RTN-CODE > +0 ' RTN-CODE
BP931         DISPLAY ' ERROR IN 4112-CONTINUE PARAGRAPH '
BP931         DISPLAY
BP931     ' ERROR ON DISKHOLD - WA-DELL-GUID ' HWA-DELL-GUID
BP931         DISPLAY
BP931     'PROGRAM LSNNB402 IS ABENDING . . . CHECK WA STATUS '
BP931         MOVE +2260 TO ABEND-CODE
BP931         PERFORM 9900-ABEND-RTN.
BP931     MOVE SAVE-PASS-POLICY TO APCV1-POLICY.
BP931  4113-WA-DISKUP.
BP931 *    CALL 'DISKUP' USING WA-APPLICATION-RECORD
BP931 *                     WA-KEY
BP931 *                     WA-FILE-IDZ
BP931 *                     RTN-CODE.
BP931      MOVE WA-KEY TO VSAM-WA-PRIM
BP931      REWRITE VSAM-WA-REC
BP931                 FROM WA-APPLICATION-RECORD
BP931      MOVE VSAM-WA-FS TO WS-INDX-FS-99
BP931      INITIALIZE TPSWNML-AREA
BP931      MOVE WA-KEY TO TPSWNML-FILE-KEY
BP931      MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID
BP931      MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE
BP931      PERFORM P9999-MAP-RESP-CODE
BP931      MOVE WS-INDX-FS-99 TO RTN-CODE.
BP931      IF (RTN-CODE NOT = +0)
BP931 *        CALL 'TPFIRLFN' USING WA-FILE-IDZ
BP931 *                        REL-RTN-CODE
BP931          DISPLAY
BP931      ' WA FILE I/O ERROR-PROGRAM RTN-CODE > +0 ' RTN-CODE
BP931          DISPLAY ' ERROR IN 4113-WA-DISKUP PARAGRAPH '
BP931          DISPLAY ' ERROR ON DISKUP - WA-DELL-GUID ' HWA-DELL-GUID
BP931          DISPLAY
BP931      'PROGRAM LSNNB402 IS ABENDING . . . CHECK WA STATUS '
BP931          MOVE +2260 TO ABEND-CODE
BP931          PERFORM 9900-ABEND-RTN.
BP931
BP931  4199-ADD-POLICY-CV01-WA-EXIT.
BP931      EXIT.

       4200-LOAD-WA-KEY-TABLE  SECTION.                                  0003499
           MOVE SPACES TO WA-TABLE-DATA.                                 0003500
           MOVE WA-APPLICATION-RECORD TO HOLD-WA-RECORD.                 0003501
BT140      MOVE +5 TO WS-PM-COUNT.                                       0003502
           MOVE +0 TO RTN-CODE                                           0003503
                   WS-SUB1                                               0003504
                   WS-SUB2.                                              0003505
           ADD +1 TO WS-REC-COUNT.                                       0003506
           MOVE +1 TO WS-SUB1.                                           0003507
           GO TO 4220-LOAD-TABLE-LOOP.                                   0003508
       4210-VALID-REC.                                                   0003509
           PERFORM 8300-READ-NEXT-WA-RECS.                               0003510
           IF RTN-CODE = +12                                             0003511
               DISPLAY 'I/O ERROR ON WA FILE W/RTN-CODE = +' RTN-CODE    0003512
               DISPLAY ' AT 4210-VALID-REC PARAGRAPH '                   0003513
               MOVE +4210 TO ABEND-CODE                                  0003514
               PERFORM 9900-ABEND-RTN.                                   0003515
                                                                         0003516
           IF RTN-CODE = +1                                              0003517
               GO TO 4290-LOAD-WA-KEY-FINISH.                            0003518
                                                                         0003519
DL457      ADD +1 TO WS-REC-READ.                                       DELLRT17
           IF HWA-DELL-GUID = WA-DELL-GUID                               0003520
             AND HWA-CO = WA-CO                                          0003521
             AND HWA-TOTAL-RECS = WA-TOTAL-RECS                          0003522
               NEXT SENTENCE                                             0003523
           ELSE                                                          0003524
               GO TO 4290-LOAD-WA-KEY-FINISH.                            0003525
                                                                         0003526
           IF HWA-DELL-GUID = WA-DELL-GUID                               0003527
             AND HWA-CO = WA-CO                                          0003528
             AND WA-DUP-SEQ = 000                                        0003529
             AND WA-STATUS = SPACES                                      0003530
               ADD +1 TO WS-REC-COUNT                                    0003531
               MOVE WA-KEY TO SV-LAST-WA-KEY                             0003532
               GO TO 4220-LOAD-TABLE-LOOP.                               0003533
                                                                         0003534
PS401      IF HWA-DELL-GUID = WA-DELL-GUID                              DELLRT12
PS401        AND WA-POLCRECTYPE = 'S'                                   DELLRT12
PS401        AND WA-STATUS = SPACES                                     DELLRT12
PS401          ADD +1 TO WS-REC-COUNT                                   DELLRT12
PS401          MOVE WA-KEY TO SV-LAST-WA-KEY                            DELLRT12
PS401          GO TO 4220-LOAD-TABLE-LOOP.                              DELLRT12
                                                                        
           GO TO 4210-VALID-REC.                                         0003535
       4220-LOAD-TABLE-LOOP.                                             0003536
           IF WS-SUB1 = WTR-TABLE-MAX                                    0003537
               DISPLAY                                                   0003538
           ' WTR-TABLE-MAX IS REACHED - CONTACT DEVELOPMENT '            0003538
               DISPLAY ' TABLE MUST BE EXPANDED IN LSNNB401 AS WELL '    0003539
               DISPLAY                                                   0003540
           ' DELL-GUID OF LAST WA RECORD READ:  ' WA-DELL-GUID           0003540
               MOVE +4220 TO ABEND-CODE                                  0003541
               PERFORM 9900-ABEND-RTN.                                   0003542
                                                                         0003543
           IF WA-FILE-ID = 'AP'                                          0003544
             AND WA-REC-ID = 'AP'                                        0003545
               GO TO 4221-LOAD-APP-REC.                                  0003546
                                                                         0003547
           IF WA-FILE-ID = 'AP'                                          0003548
             AND WA-REC-ID = 'CV'                                        0003549
               GO TO 4222-LOAD-COV-REC.                                  0003550
                                                                         0003551
           IF WA-FILE-ID = 'NB'                                          0003552
BT162        AND (WA-REC-ID = 'BN' OR 'T1' OR 'BD' OR 'AB' OR 'AC'       0003553
AR150                      OR 'EI' OR 'OW' OR 'TP' OR 'AL' OR 'GW'              
DL791                      OR 'IB' OR 'FM' OR 'DI')
               GO TO 4223-LOAD-NB-NOTE-RECS.                             0003555
                                                                         0003556
           IF WA-FILE-ID = 'TH'                                          0003557
             AND WA-REC-ID = 'NT'                                        0003558
               GO TO 4223-LOAD-NB-NOTE-RECS.                             0003559
                                                                         0003560
DD281      IF WA-FILE-ID = 'AP'                                         DELLRET2
DD281        AND WA-REC-ID = 'OR'                                       DELLRET2
DD281          GO TO 4223-LOAD-NB-NOTE-RECS.                            DELLRET2
                                                                        
           IF WA-FILE-ID = 'WA'                                          0003561
             AND WA-REC-ID = 'SY'                                        0003562
               GO TO 4224-LOAD-SYS-REC.                                  0003563
                                                                         0003564
AK131      IF WA-FILE-ID = 'PM'                                          0003565
AK131        AND WA-REC-ID = 'PN'                                        0003566
AK131          MOVE 'Y' TO WS-PM-FOUND                                   0003567
AK131          GO TO 4226-LOAD-PM-REC.                                   0003568
AK131                                                                    0003569
           GO TO 4225-LOAD-INV-REC.                                      0003570
       4221-LOAD-APP-REC.                                                0003571
           IF (WA-REC-SEQ > 2)                                           0003572
             OR (WA-REC-SEQ < 1)                                         0003573
               GO TO 4225-LOAD-INV-REC.                                  0003574
                                                                         0003575
           MOVE +1 TO WS-SUB1.                                           0003576
           ADD +1 TO WS-SUB2.                                            0003577
           MOVE WA-FILE-ID TO WTR-FILE-ID (WS-SUB1).                     0003578
           MOVE WA-REC-ID TO WTR-REC-ID (WS-SUB1).                       0003579
           MOVE WA-REC-APPID TO WTR-APP-ID (WS-SUB1).                    0003580
           MOVE WA-REC-SEQ TO WTR-REC-SEQ (WS-SUB1, WS-SUB2).            0003581
           MOVE WA-KEY TO WTR-WA-KEY (WS-SUB1, WS-SUB2).                 0003582
           MOVE WA-STATUS TO WTR-STATUS (WS-SUB1, WS-SUB2).              0003583
           ADD +1 TO WS-AP-COUNT.                                        0003584
           GO TO 4210-VALID-REC.                                         0003585
       4222-LOAD-COV-REC.                                                0003586
      ****************************************************************   0003587
      *   THIS WILL PROCESS 4 TIMES FOR EACH TABLE ENTRY OF THE      *   0003588
      * COVERAGE RECORD APPID (PERSON) AS THERE ARE A TOTAL OF 4     *   0003589
      * 'WA' RECORDS THAT ARE USED TO PROPAGATE 1 COVERAGE RECORD    *   0003590
      ****************************************************************   0003591
           IF WS-SUB1 = +1                                               0003592
               MOVE +2 TO WS-SUB1                                        0003593
               MOVE +0 TO WS-SUB2.                                       0003594
                                                                         0003595
           IF (WA-REC-SEQ > 4)                                           0003596
             OR (WA-REC-SEQ < 1)                                         0003597
               GO TO 4225-LOAD-INV-REC.                                  0003598
                                                                         0003599
           IF (WA-REC-APPID > 49)                                        0003600
             OR (WA-REC-APPID < 1)                                       0003601
               GO TO 4225-LOAD-INV-REC.                                  0003602
                                                                         0003603
           ADD +1 TO WS-CV-COUNT.                                        0003604
           IF WS-CV-COUNT > +4                                           0003605
               ADD +1 TO WS-SUB1                                         0003606
               MOVE +1 TO WS-SUB2                                        0003607
               MOVE +1 TO WS-CV-COUNT.                                   0003608
                                                                         0003609
           IF WS-CV-COUNT = +1                                           0003610
               MOVE WA-FILE-ID TO WTR-FILE-ID (WS-SUB1)                  0003611
               MOVE WA-REC-ID TO WTR-REC-ID (WS-SUB1)                    0003612
               MOVE WA-REC-APPID TO WTR-APP-ID (WS-SUB1).                0003613
                                                                         0003614
           IF WS-SUB2 = +0                                               0003615
               MOVE +1 TO WS-SUB2.                                       0003616
                                                                         0003617
       4222-CV-CONTINUE.                                                 0003618
           MOVE WA-REC-SEQ TO WTR-REC-SEQ (WS-SUB1, WS-SUB2).            0003619
           MOVE WA-KEY TO WTR-WA-KEY (WS-SUB1, WS-SUB2).                 0003620
           MOVE WA-STATUS TO WTR-STATUS (WS-SUB1, WS-SUB2).              0003621
           ADD +1 TO WS-SUB2.                                            0003622
           GO TO 4210-VALID-REC.                                         0003623
       4223-LOAD-NB-NOTE-RECS.                                           0003624
           ADD +1 TO WS-SUB1.                                            0003625
           MOVE +1 TO WS-SUB2.                                           0003626
           MOVE WA-FILE-ID TO WTR-FILE-ID (WS-SUB1).                     0003627
           MOVE WA-REC-ID TO WTR-REC-ID (WS-SUB1).                       0003628
           MOVE WA-REC-APPID TO WTR-APP-ID (WS-SUB1).                    0003629
           MOVE WA-REC-SEQ TO WTR-REC-SEQ (WS-SUB1, WS-SUB2).            0003630
           MOVE WA-KEY TO WTR-WA-KEY (WS-SUB1, WS-SUB2).                 0003631
           GO TO 4210-VALID-REC.                                         0003632
       4224-LOAD-SYS-REC.                                                0003633
           ADD +1 TO WS-SY-COUNT.                                        0003634
           ADD +1 TO WS-SUB1.                                            0003635
           MOVE +1 TO WS-SUB2.                                           0003636
           MOVE WA-FILE-ID TO WTR-FILE-ID (WS-SUB1).                     0003637
           MOVE WA-REC-ID TO WTR-REC-ID (WS-SUB1).                       0003638
           MOVE WA-REC-APPID TO WTR-APP-ID (WS-SUB1).                    0003639
           MOVE WA-REC-SEQ TO WTR-REC-SEQ (WS-SUB1, WS-SUB2).            0003640
           MOVE WA-KEY TO WTR-WA-KEY (WS-SUB1, WS-SUB2).                 0003641
KM101      MOVE WASY-POLASSIGNIND TO SAVE-PAIND-WASY.                    0003642
BT231      MOVE WASY-BLOCK-ID TO SAVE-BLOCK-ID-WASY.                     0003643
BT231      MOVE WASY-FINAL-STATUS TO WK-FINAL-STATUS.                    0003644
KM101      MOVE WASY-POLNUM TO SAVE-PAPOL-WASY.                          0003645
KM101      MOVE WASY-TASK-COMMENT1-A TO SAVE-COMT1-WASY.                 0003646
KM101      MOVE WASY-TASK-COMMENT2-A TO SAVE-COMT2-WASY.                 0003647
KM101      MOVE WASY-TASK-COMMENT3-A TO SAVE-COMT3-WASY.                 0003648
CL131      MOVE WASY-LS-OVERRIDE-IND TO SAVE-LS-OVERRIDE-IND.            0003649
BT151      MOVE WASY-LS-PARAM-3 TO SAVE-LS-PARAM-3.                      0003650
BT151      MOVE WASY-LS-PARAM-4 TO SAVE-LS-PARAM-4.                      0003651
BT151      MOVE WASY-LS-PARAM-5 TO SAVE-LS-PARAM-5.                      0003652
BT392      MOVE WASY-LS-PARAM-8 TO SAVE-LS-PARAM-8.                     DELLRT10
AS991      MOVE WASY-LS-PARAM-34 TO SAVE-LS-PARAM-34.                    0003653
BT151      MOVE WASY-LS-PARAM-37 TO SAVE-LS-PARAM-37.                    0003653
DL461      MOVE WASY-LS-PARAM-1 TO SAVE-LS-PARAM-1.
           GO TO 4210-VALID-REC.                                         0003654
       4225-LOAD-INV-REC.                                                0003655
           ADD +1 TO WS-SUB1.                                            0003656
           MOVE +1 TO WS-SUB2.                                           0003657
           MOVE WA-FILE-ID TO WTR-FILE-ID (WS-SUB1).                     0003658
           MOVE WA-REC-ID TO WTR-REC-ID (WS-SUB1).                       0003659
           MOVE WA-REC-APPID TO WTR-APP-ID (WS-SUB1).                    0003660
           MOVE WA-REC-SEQ TO WTR-REC-SEQ (WS-SUB1, WS-SUB2).            0003661
           MOVE WA-KEY TO WTR-WA-KEY (WS-SUB1, WS-SUB2).                 0003662
           MOVE 'INV' TO WTR-STATUS (WS-SUB1, WS-SUB2).                  0003663
      *    CALL 'DISKHOLD' USING WA-APPLICATION-RECORD                   0003664
      *                   WA-KEY                                         0003665
      *                   WA-FILE-IDZ                                    0003666
      *                   RTN-CODE.                                      0003667
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           READ VSAM-WA                                                 DELLIDCH
                    INTO WA-APPLICATION-RECORD                          DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
           IF RTN-CODE > +0                                              0003668
               DISPLAY 'RTN-CODE FROM DISKHOLD OF WA FILE > +0 '         0003669
               DISPLAY 'ERROR @ 4225-LOAD-INV-REC PARAGRAPH '            0003670
               DISPLAY 'KEY OF PROCESSING WA RECORD:  ' WA-KEY           0003671
               DISPLAY 'CHECK WA FILE STATUS...'                         0003672
               MOVE +4225 TO ABEND-CODE                                  0003673
               PERFORM 9900-ABEND-RTN.                                   0003674
                                                                         0003675
           MOVE WK-SYSTEM-DATE TO WA-MF-DATE.                            0003676
           MOVE WK-SYSTEM-TIME TO WA-MF-TIME.                            0003677
           MOVE WTR-STATUS (WS-SUB1, WS-SUB2) TO WA-STATUS.              0003678
      *    CALL 'DISKUP' USING WA-APPLICATION-RECORD                     0003679
      *                 WA-KEY                                           0003680
      *                 WA-FILE-IDZ                                      0003681
      *                 RTN-CODE.                                        0003682
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           REWRITE VSAM-WA-REC                                          DELLIDCH
                      FROM WA-APPLICATION-RECORD                        DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE                       DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
           IF RTN-CODE > +0                                              0003683
      *        CALL 'TPFIRLFN' USING WA-FILE-IDZ                        DELLMNCH
      *                       REL-RTN-CODE                              DELLMNCH
               DISPLAY                                                   0003686
           'ERROR ON DISKUP - WA FILE W/RTN-CODE = +' RTN-CODE           0003686
               DISPLAY 'ERROR @ 4225-LOAD-INV-REC PARAGRAPH '            0003687
               DISPLAY 'KEY OF PROCESSING WA RECORD:  ' WA-KEY           0003688
               DISPLAY 'CHECK STATUS OF WA FILE...'                      0003689
               MOVE +4225 TO ABEND-CODE                                  0003690
               PERFORM 9900-ABEND-RTN.                                   0003691
                                                                         0003692
           GO TO 4210-VALID-REC.                                         0003693
AK131  4226-LOAD-PM-REC.                                                 0003694
AK131      IF WS-SUB1 = +1                                               0003695
AK131          MOVE +2 TO WS-SUB1                                        0003696
AK131          MOVE +0 TO WS-SUB2.                                       0003697
AK131                                                                    0003698
AK131      IF (WA-REC-SEQ > 3)                                           0003699
AK131        OR (WA-REC-SEQ < 1)                                         0003700
AK131          MOVE 'Y' TO ERROR-SWITCH                                  0003701
AK131          GO TO 4225-LOAD-INV-REC.                                  0003702
AK131                                                                    0003703
AK131      ADD +1 TO WS-PM-COUNT.                                        0003704
AK131      IF WS-PM-COUNT > +3                                           0003705
AK131          ADD +1 TO WS-SUB1                                         0003706
AK131          MOVE +1 TO WS-SUB2                                        0003707
AK131          MOVE +1 TO WS-PM-COUNT.                                   0003708
AK131                                                                    0003709
AK131      IF WS-PM-COUNT = +1                                           0003710
AK131          MOVE WA-FILE-ID TO WTR-FILE-ID (WS-SUB1)                  0003711
AK131          MOVE WA-REC-ID TO WTR-REC-ID (WS-SUB1)                    0003712
AK131          MOVE WA-REC-APPID TO WTR-APP-ID (WS-SUB1).                0003713
AK131                                                                    0003714
AK131      IF WS-SUB2 = +0                                               0003715
AK131          MOVE +1 TO WS-SUB2.                                       0003716
AK131                                                                    0003717
AK131  4227-PM-CONTINUE.                                                 0003718
AK131      MOVE WA-REC-SEQ TO WTR-REC-SEQ (WS-SUB1, WS-SUB2).            0003719
AK131      MOVE WA-KEY TO WTR-WA-KEY (WS-SUB1, WS-SUB2).                 0003720
AK131      MOVE WA-STATUS TO WTR-STATUS (WS-SUB1, WS-SUB2).              0003721
AK131      ADD +1 TO WS-SUB2.                                            0003722
AK131      GO TO 4210-VALID-REC.                                         0003723
       4290-LOAD-WA-KEY-FINISH.                                          0003724
AK131      IF ERROR-SWITCH = 'Y'                                         0003725
AK131          GO TO 4291-SY-COUNT.                                      0003726
AK131                                                                    0003727
AK131      IF WS-PM-FOUND = 'Y'                                          0003728
AK131         IF WS-PM-COUNT = +3                                        0003729
AK131             NEXT SENTENCE                                          0003730
AK131         ELSE                                                       0003731
AK131             GO TO 4291-SY-COUNT.                                   0003732
AK131                                                                    0003733
           IF WS-SY-COUNT = +1                                           0003734
             AND WS-AP-COUNT = +2                                        0003735
             AND WS-CV-COUNT = +4                                        0003736
             AND WS-REC-COUNT = HWA-TOTAL-RECS                           0003737
               GO TO 4298-LOAD-LAST-WA-KEY.                              0003738
                                                                         0003739
PS443      IF HWA-POLCRECTYPE = 'S'                                     DELLRT16
PS401        AND WS-AP-COUNT = +1                                       DELLRT12
PS401        AND WS-SY-COUNT = +1                                       DELLRT12
PS401        AND WS-REC-COUNT = HWA-TOTAL-RECS                          DELLRT12
PS401          GO TO 4298-LOAD-LAST-WA-KEY.                             DELLRT12
                                                                        
AK131  4291-SY-COUNT.                                                    0003740
           IF WS-SY-COUNT = +1                                           0003741
               GO TO 4295-RSE-ERROR-TASK.                                0003742
                                                                         0003743
           PERFORM 5500-DUMMY-SY-REC.                                    0003744
       4295-RSE-ERROR-TASK.                                              0003745
           MOVE 'Y' TO ERROR-SWITCH.                                     0003746
           MOVE 'RSE' TO WK-STATUS.                                      0003747
           PERFORM 7000-STATUS-ERROR-WA.                                 0003748
           MOVE '        ' TO SAVE-TASK-POLICY.                          0003749
BT153      PERFORM 2600-ERROR-TASKS.                                     0003750
BT153      GO TO 4299-LOAD-WA-KEY-TABLE-EXIT.                            0003751
                                                                         0003752
       4298-LOAD-LAST-WA-KEY.                                            0003753
           MOVE SPACES TO WA-APPLICATION-RECORD.                         0003754
           MOVE SV-LAST-WA-KEY TO WA-KEY.                                0003755
           PERFORM 8700-READ-KEY-WA.                                     0003756
           IF RTN-CODE > +0                                              0003757
               DISPLAY                                                   0003758
           'ERROR ON DISKREAD OF WA FILE RTN-CODE = +' RTN-CODE          0003758
               DISPLAY 'IN 4298-LOAD-LAST-WA-KEY PARAGRAPH'              0003759
               DISPLAY                                                   0003760
           'TRYING TO PROCESS THE KEY VALUE - ' SV-LAST-WA-KEY           0003760
               DISPLAY 'CHECK WA FILE STATUS...'                         0003761
               PERFORM 9900-ABEND-RTN.                                   0003762
                                                                         0003763
       4299-LOAD-WA-KEY-TABLE-EXIT.                                      0003764
           EXIT.                                                         0003765
                                                                         0003766
PS133  4300-SET-DELL-GUID-PROC-FLAG  SECTION.                            0003767
PS133      IF WA-REC-TYPE = 'APAP  01'                                   0003768
PS133          NEXT SENTENCE                                             0003769
PS133      ELSE                                                          0003770
PS133          GO TO 4399-SET-DELL-GUID-EXIT.                            0003771
PS133      MOVE +0 TO RTN-CODE.                                          0003772
PS133 *    CALL 'DISKHOLD' USING WA-APPLICATION-RECORD                   0003773
PS133 *                   WA-KEY                                         0003774
PS133 *                   WA-FILE-IDZ                                    0003775
PS133 *                   RTN-CODE.                                      0003776
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           READ VSAM-WA                                                 DELLIDCH
                    INTO WA-APPLICATION-RECORD                          DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
PS133                                                                    0003777
PS133      IF RTN-CODE = +0                                              0003778
PS133          GO TO 4350-WA-DISKUP.                                     0003779
PS133                                                                    0003780
PS133 ***********************************************************        0003781
PS133 *** IF RECORD NOT FOUND:  ABEND THE PROGRAM             ***        0003782
PS133 ***********************************************************        0003783
PS133      IF RTN-CODE = +12                                             0003784
PS133          MOVE +4300 TO ABEND-CODE                                  0003785
PS133          DISPLAY WA-KEY ' NOT FOUND ON WA FILE'                    0003786
PS133          PERFORM 9900-ABEND-RTN.                                   0003787
PS133                                                                    0003788
PS133 ***********************************************************        0003789
PS133 *** IF RECORD IS ALREADY HELD, ASSUME LSNNB402 HAS IT   ***        0003790
PS133 ***********************************************************        0003791
PS133      IF RTN-CODE = +2                                              0003792
PS133          GO TO 4350-WA-DISKUP.                                     0003793
PS133                                                                    0003794
PS133 ***********************************************************        0003795
PS133 *** ON I/O ERROR ABEND THE PROGRAM                      ***        0003796
PS133 ***********************************************************        0003797
PS133      IF RTN-CODE = +12                                             0003798
PS133          MOVE +4300 TO ABEND-CODE                                  0003799
PS133          DISPLAY WA-KEY ' I/O ERROR ON WA FILE - CHECK IF FULL'    0003800
PS133          PERFORM 9900-ABEND-RTN.                                   0003801
PS133                                                                    0003802
PS133  4350-WA-DISKUP.                                                   0003803
PS133      MOVE 'Y' TO APAP1-DELL-GUID-PROCESSED-FLAG                    0003804
PS133      MOVE +0 TO RTN-CODE.                                          0003805
PS133 *    CALL 'DISKUP' USING WA-APPLICATION-RECORD                     0003806
PS133 *                 WA-KEY                                           0003807
PS133 *                 WA-FILE-IDZ                                      0003808
PS133 *                 RTN-CODE.                                        0003809
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           REWRITE VSAM-WA-REC                                          DELLIDCH
                      FROM WA-APPLICATION-RECORD                        DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE                       DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
PS133                                                                    0003810
PS133 ***********************************************************        0003811
PS133 *** IF DISKUP FAILS, RELEASE THE RECORD THEN CONTINUE   ***        0003812
PS133 ***********************************************************        0003813
PS133      IF (RTN-CODE NOT = +0)                                        0003814
PS133          DISPLAY WA-KEY ' FAILED DISKUP IN 4350 - RC = ' RTN-CODE  0003815
PS133          MOVE +0 TO RTN-CODE.                                      0003816
PS133 *        CALL 'TPFIRLFN' USING WA-FILE-IDZ                        DELLMNCH
PS133 *                       RTN-CODE.                                 DELLMNCH
PS133                                                                    0003819
PS133  4399-SET-DELL-GUID-EXIT.                                          0003820
PS133      EXIT.                                                         0003821
BT142                                                                    0003822
BT142  4400-SET-TASK-RECORD SECTION.                                     0003823
BT142      MOVE SPACES TO TASK-RECORD.                                   0003824
BT142      MOVE WS-EENRL TO TK010-PS-PROCESS.                            0003825
BT142      MOVE WS-CHG  TO TK010-PS-STEP.                                0003826
BT142      PERFORM 3500-GET-WF-DESCRIPTION.                              0003827
BT142      IF RTN-CODE > +0                                              0003828
BT142          GO TO 4499-SET-TASK-RECORD-EXIT.                          0003829
BT142                                                                    0003830
BT142      MOVE WS-EENRL TO TASK-PROCESS.                                0003831
BT142      MOVE WS-CHG TO TASK-STEP.                                     0003832
BT142      MOVE SV-CO TO TASK-POLICY-CO.                                 0003833
BT142      MOVE SPACES TO TASK-POLICY-NUM.                               0003834
BT142      MOVE SPACES TO TASK-POLICY-RDR.                               0003835
BT301      MOVE WPK-EMPLOYER-ID TO TASK-GROUP-NUMBER.                   DELLRET2
BT142      MOVE WA-DELL-GUID TO TASK-COMMENT1-A.                         0003837
BT212      MOVE 'RECORD NOT FOUND - KEY OR PLAN UPDATE REQUIRED'         0003838
BT142         TO TASK-COMMENT2-A.                                        0003839
BT302                                                                   DELLRET2
BT302      IF WK-PARM38 = 'Y'                                           DELLRET2
BT302         MOVE 'RECORD NOT FOUND - NEW KEY WIT'                     DELLRET2
BT302          TO WK-REMARK-A                                           DELLRET2
BT302         MOVE 'H COVERAGE STOP DATE RECEIVED '                     DELLRET2
BT302          TO WK-REMARK-B                                           DELLRET2
BT302         MOVE WK-REMARK2 TO TASK-COMMENT2-A.                       DELLRET2
BT302      MOVE SAVE-LS-PARAM-3 TO WK-RPT-NUM-X.                        DELLRET2
BT302      MOVE SAVE-LS-PARAM-4 TO WK-SUBCODE-X.                        DELLRET2
BT302      MOVE SAVE-LS-PARAM-5 TO WK-CLAIM-BRANCH.                     DELLRET2
BT302      MOVE WK-STRUCT TO TASK-COMMENT3-A.                           DELLRET2
BT142      MOVE '1' TO TASK-PRIORITY.                                    0003841
BT142      MOVE SPACES TO TASK-ADDTO-DATABASE.                           0003842
BT142      MOVE WS-P TO TASK-STATUS.                                     0003843
BT142      MOVE WS-STPS TO TASK-SENDER-OPID.                             0003844
BT142                                                                    0003845
BT142      MOVE WK-SYSTEM-DATE TO TASK-PENDING-DATE.                     0003846
BT142      MOVE WK-SYSTEM-TIME TO TASK-PENDING-TIME.                     0003847
BT142      MOVE WK-SYSTEM-DATE TO TASK-ADDED-DATE.                       0003848
BT142                                                                    0003849
BT142      MOVE 000000000 TO TASK-ID.                                    0003850
BT142      MOVE TASK-ID TO TASK-MASTER-ID.                               0003851
BT142      MOVE WK-SYSTEM-DATE TO TASK-MSTR-PDATE.                       0003852
BT142      MOVE WA-IMAGE-REFID TO TASK-IMAGE-ID.                         0003853
BT142      PERFORM 4000-TASK-DISKADD.                                    0003854
BT142                                                                    0003855
BT142  4499-SET-TASK-RECORD-EXIT.                                        0003856
BT142      EXIT.                                                         0003857
BT142                                                                    0003858
BT142  4500-GET-SY-RECORD SECTION.                                       0003859
BT142      MOVE SPACES TO TASK-RECORD.                                   0003860
BT142      MOVE +0 TO WS-SUB1.                                           0003861
BT142      MOVE +1 TO WS-SUB2.                                           0003862
BT142  4510-LOCATE-SY-LOOP.                                              0003863
BT142      ADD +1 TO WS-SUB1.                                            0003864
BT142      IF WS-SUB1 > WTR-TABLE-MAX                                    0003865
BT142          MOVE WS-Y TO ERROR-SWITCH                                 0003866
BT142          GO TO 4599-GET-SY-RECORD-EXIT.                            0003867
BT142                                                                    0003868
BT142      IF WTR-FILE-ID (WS-SUB1) = WS-WA                              0003869
BT142        AND WTR-REC-ID (WS-SUB1) = WS-SY                            0003870
BT142          GO TO 4520-READ-SY-REC.                                   0003871
BT142                                                                    0003872
BT142      GO TO 4510-LOCATE-SY-LOOP.                                    0003873
BT142  4520-READ-SY-REC.                                                 0003874
BT142      MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WA-KEY.                 0003875
BT142      PERFORM 8700-READ-KEY-WA.                                     0003876
BT142      IF RTN-CODE = +0                                              0003877
BT142          GO TO 4599-GET-SY-RECORD-EXIT.                            0003878
BT142      IF RTN-CODE > +0                                              0003879
BT142          DISPLAY                                                   0003880
BT142      'ERROR ON READ OF WA FILE - RTN-CODE = +' RTN-CODE            0003880
BT142          DISPLAY '@ LOCATION 4520-READ-SY-REC    PARAGRAPH'        0003881
BT142          DISPLAY                                                   0003882
BT142      'KEY PROCESSED IS:  ' WTR-WA-KEY (WS-SUB1, WS-SUB2)           0003882
BT142          DISPLAY 'CHECK WA FILE STATUS... LSNNB402 IS ABENDING '   0003883
BT142          MOVE +4520 TO ABEND-CODE                                  0003884
BT142          PERFORM 9900-ABEND-RTN.                                   0003885
BT142                                                                    0003886
BT142  4599-GET-SY-RECORD-EXIT.                                          0003887
BT142      EXIT.                                                         0003888
BT142                                                                    0003889
BT142  4600-CHECK-NEW-BUSINESS  SECTION.                                 0003890
BT142      MOVE 'N' TO ERROR-SWITCH.                                     0003891
BT142      MOVE SV-CO TO RE-FILE-CO.                                     0003897
BT142      MOVE 'RE' TO RE-FILE-LIT.                                     0003898
BT142      MOVE SPACES TO KEY-C.                                         0003899
BT142      MOVE SV-CO TO CO-C.                                           0003900
BT142      MOVE 0 TO PARM-C.                                             0003901
BT142 *    CALL 'DISKREAD' USING COMPANY-OPTION-RECORD                   0003902
BT142 *                   KEY-C                                          0003903
BT142 *                   RE-FILE-ID                                     0003904
BT142 *                   RTN-CODE.                                      0003905
           MOVE KEY-C TO VSAM-RE-PRIM                                   DELLIDCH
           READ VSAM-RE                                                 DELLIDCH
                    INTO COMPANY-OPTION-RECORD                          DELLIDCH
           MOVE VSAM-RE-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE KEY-C TO TPSWNML-FILE-KEY                               DELLIDCH
           MOVE RE-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKREAD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
BT142      IF RTN-CODE > +0                                              0003906
BT142          DISPLAY 'ERROR ON READ OF RE FILE, RTN-CODE = +' RTN-CODE 0003907
BT142          DISPLAY 'LOCATION IS 4600-CHECK NEW BUSINESS SECTION'     0003908
BT142          DISPLAY ' LSNNB402 IS ABENDING... CHECK RE FILE STATUS'   0003909
BT142          MOVE +4600 TO ABEND-CODE                                  0003910
BT142          PERFORM 9900-ABEND-RTN.                                   0003911
BT170 * ADD CODE TO FIND MATCH BY OLD-POLICY AND EMPLOYER ID             0003912
BT170      MOVE +1 TO WS-SUB1.                                           0003913
BT170      IF WTR-FILE-ID (WS-SUB1) = 'AP'                               0003914
BT170        AND WTR-REC-ID (WS-SUB1) = 'AP'                             0003915
BT170          NEXT SENTENCE                                             0003916
BT170      ELSE                                                          0003917
BT170          GO TO 4699-CHECK-NEW-BUSINESS-EXIT.                       0003918
BT170                                                                    0003919
BT170      MOVE +2 TO WS-SUB2.                                           0003920
BT170      MOVE +0 TO RTN-CODE.                                          0003921
BT170      MOVE SPACES TO WA-APPLICATION-RECORD.                         0003922
BT170      MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WA-KEY.                 0003923
BT170                                                                    0003924
BT170      PERFORM 8700-READ-KEY-WA.                                     0003925
BT170      IF RTN-CODE = +12                                             0003926
BT170          GO TO 4699-CHECK-NEW-BUSINESS-EXIT.                       0003927
BT170                                                                    0003928
BT170      IF RTN-CODE = +1                                              0003929
BT170          GO TO 4699-CHECK-NEW-BUSINESS-EXIT.                       0003930
BT170                                                                    0003931
BT170      MOVE WA-APPLICATION-RECORD TO HOLD-APAP2-RECORD.              0003932
BT170 ************************************************************       0003933
BT170 ***  THIS SECTION READS THE WA FILE AND MOVES VALUES     ***       0003934
BT170 ***  FROM THE APAP1 RECORD TO THE NOTE AND TASK RECORDS. ***       0003935
BT170 ************************************************************       0003936
BT170      MOVE +1 TO WS-SUB1.                                           0003937
BT170      IF WTR-FILE-ID (WS-SUB1) = 'AP'                               0003938
BT170        AND WTR-REC-ID (WS-SUB1) = 'AP'                             0003939
BT170          NEXT SENTENCE                                             0003940
BT170      ELSE                                                          0003941
BT170          GO TO 4699-CHECK-NEW-BUSINESS-EXIT.                       0003942
BT170                                                                    0003943
BT170      MOVE +1 TO WS-SUB2.                                           0003944
BT170      MOVE +0 TO RTN-CODE.                                          0003945
BT170      MOVE SPACES TO WA-APPLICATION-RECORD.                         0003946
BT170      MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WA-KEY.                 0003947
BT170                                                                    0003948
BT170      PERFORM 8700-READ-KEY-WA.                                     0003949
BT170                                                                    0003950
BT170      IF RTN-CODE = +12                                             0003951
BT170          GO TO 4699-CHECK-NEW-BUSINESS-EXIT.                       0003952
BT170                                                                    0003953
BT170      IF RTN-CODE = +1                                              0003954
BT170          GO TO 4699-CHECK-NEW-BUSINESS-EXIT.                       0003955
BT170                                                                    0003956
BT170      MOVE WA-APPLICATION-RECORD TO HOLD-APAP1-RECORD.              0003957
BT184      MOVE +2 TO WS-SUB1.                                           0003958
BT184      IF WTR-FILE-ID (WS-SUB1) = 'AP'                               0003959
BT184        AND WTR-REC-ID (WS-SUB1) = 'CV'                             0003960
BT184          NEXT SENTENCE                                             0003961
BT184      ELSE                                                          0003962
BT184          GO TO 4699-CHECK-NEW-BUSINESS-EXIT.                       0003963
BT184                                                                    0003964
BT184      MOVE +1 TO WS-SUB2.                                           0003965
BT184      MOVE +0 TO RTN-CODE.                                          0003966
BT184      MOVE SPACES TO WA-APPLICATION-RECORD.                         0003967
BT184      MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WA-KEY.                 0003968
BT184                                                                    0003969
BT184      PERFORM 8700-READ-KEY-WA.                                     0003970
BT184                                                                    0003971
BT184      IF RTN-CODE = +12                                             0003972
BT184          GO TO 4699-CHECK-NEW-BUSINESS-EXIT.                       0003973
BT184                                                                    0003974
BT184      IF RTN-CODE = +1                                              0003975
BT184          GO TO 4699-CHECK-NEW-BUSINESS-EXIT.                       0003976
BT184                                                                    0003977
BT184      MOVE WA-APPLICATION-RECORD TO HOLD-APCV1-RECORD.              0003978
BT170      MOVE HOLD-APAP2-RECORD TO WA-APPLICATION-RECORD.              0003979
BT170      MOVE SPACES TO AI-N2-KEY                                      0003980
BT170             NEW-BUSINESS-RECORD                                    0003981
BT170             ALT-IDX-NAME.                                          0003982
BT170      MOVE SV-CO TO ALT-IDX-CO.                                     0003983
BT170      MOVE 'N2' TO ALT-IDX-LIT.                                     0003984
BT170      MOVE APAP2-OLD-POLICY-NA TO AI-N2-OLD-POLICY.                 0003985
           MOVE SPACES TO VSAM-AP-N5.                                   DELLMNCH
      *    MOVE APAP2-OLD-POLICY-NA TO VSAM-AP-N5.                      DELLMNCH
BT170  4610-READNEXT-NB-POL-LOOP.                                        0003986
BT170      MOVE +0 TO AI-RTN-CODE.                                       0003987
BT170 *    CALL 'RDNXTDAT' USING NEW-BUSINESS-RECORD                     0003988
BT170 *                   AI-N2-KEY                                      0003989
BT170 *                   ALT-IDX-NAME                                   0003990
BT170 *                   AI-RTN-CODE.                                   0003991
           IF AI-N2-OLD-POLICY <> VSAM-AP-N5                            DELLMNCH
           MOVE AI-N2-OLD-POLICY TO VSAM-AP-N5                          DELLMNCH
            START VSAM-AP                                               DELLMNCH
                  KEY IS >=  VSAM-AP-N5                                 DELLMNCH
            END-START                                                   DELLIDCH
            MOVE VSAM-AP-FS TO WS-INDX-FS-99                            DELLIDCH
            MOVE VSAM-AP-REC TO NEW-BUSINESS-RECORD                     DELLMNCH
            INITIALIZE TPSWNML-AREA                                     DELLIDCH
            MOVE OLD-POLICY-NA TO TPSWNML-FILE-KEY                      DELLMNCH
            MOVE ALT-IDX-NAME TO TPSWNML-FILE-ID                        DELLIDCH
            MOVE 'RDNXTDAT' TO TPSWNML-FUNCTION-CODE                    DELLIDCH
            PERFORM P9999-MAP-RESP-CODE                                 DELLIDCH
            MOVE WS-INDX-FS-99 TO AI-RTN-CODE                           DELLIDCH
           END-IF                                                       DELLIDCH
           READ VSAM-AP NEXT RECORD WITH NO LOCK                        DELLMNCH
                  INTO NEW-BUSINESS-RECORD                              DELLIDCH
           MOVE VSAM-AP-FS TO WS-INDX-FS-99                             DELLIDCH
           MOVE OLD-POLICY-NA TO TPSWNML-FILE-KEY                       DELLMNCH
           MOVE ALT-IDX-NAME TO TPSWNML-FILE-ID                         DELLMNCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO AI-RTN-CODE                            DELLIDCH
           MOVE VSAM-AP-N5 TO AI-N2-OLD-POLICY                          DELLMNCH
BT170      IF (AI-RTN-CODE NOT = +0)                                     0003992
BT170          GO TO 4699-CHECK-NEW-BUSINESS-EXIT.                       0003993
BT170 *                                                                  0003994
BT170      MOVE HOLD-APAP2-RECORD TO WA-APPLICATION-RECORD.              0003995
BT170      IF OLD-POLICY-NA = APAP2-OLD-POLICY-NA                        0003996
BT170          GO TO 4615-CHK-EMPLOYER                                   0003997
BT170      ELSE                                                          0003998
BT170          GO TO 4699-CHECK-NEW-BUSINESS-EXIT.                       0003999
BT170 *                                                                  0004000
BT170  4615-CHK-EMPLOYER.                                                0004001
BT170      MOVE HOLD-APAP1-RECORD TO WA-APPLICATION-RECORD.              0004002
BT170      IF PYRL-EMPLOYERID-NA = APAP1-PYRL-EMPLOYERID-NA              0004003
BT302          GO TO 4617-CHK-INFO2                                     DELLRET2
BT184      ELSE                                                          0004005
BT184          GO TO 4610-READNEXT-NB-POL-LOOP.                          0004006
BT184 *4616-CHK-OCC-CLASS.                                               0004007
BT184 *    M HOLD-APCV1-RECORD TO WA-APPLICATION-RECORD.                 0004008
BT184 *    IF OCC-CLASS-N1 = APCV1-OCC-CLASS-N1                          0004009
BT184 *        GO TO 4617-CHK-PLAN                                       0004010
BT184 *    ELSE                                                          0004011
BT184 *        GO TO 4610-READNEXT-NB-POL-LOOP.                          0004012
BT302  4617-CHK-INFO2.                                                  DELLRET2
BT302                                                                   DELLRET2
BT302      MOVE HOLD-APAP2-RECORD TO WA-APPLICATION-RECORD.             DELLRET2
BT302      IF FRATERNAL-INFO2-NA = APAP2-FRATERNAL-INFO2-NA             DELLRET2
BT302          NEXT SENTENCE                                            DELLRET2
BT302      ELSE                                                         DELLRET2
BT302          GO TO 4610-READNEXT-NB-POL-LOOP.                         DELLRET2
DL821      IF SAVE-LS-PARAM-1 = 'LO'
DL821          MOVE HOLD-APAP2-RECORD TO WA-APPLICATION-RECORD
DL821          IF EAPPID-NA = APAP2-EAPPID-NA
DL821              NEXT SENTENCE
DL821          ELSE
DL821              GO TO 4610-READNEXT-NB-POL-LOOP
DL821          END-IF
DL821      END-IF.
BT302      MOVE WS-Y TO CHECK-NEW-BUSINESS-IND.                         DELLRET2
BT302                                                                   DELLRET2
BT195 *                                                                  0004032
BT302  4620-CONTINUE.                                                   DELLRET2
BT170      PERFORM 9000-READ-GU.                                         0004035
BT170      IF RTN-CODE = +0                                              0004036
BT170          GO TO 4625-CHK-GUPOL.                                     0004037
BT170                                                                    0004038
BT170      IF RTN-CODE > +0                                              0004039
BT170          GO TO 4640-CONTINUE.                                      0004040
BT170                                                                    0004041
BT170  4625-CHK-GUPOL.                                                   0004042
BT170      IF  GU-POLICY = SPACES                                        0004043
BT170          GO TO 4630-CONTINUE.                                      0004044
BT170  4630-CONTINUE.                                                    0004047
BT170 *    CALL 'DISKHOLD' USING GU-GUID-RECORD                          0004048
BT170 *                   GU-KEY                                         0004049
BT170 *                   GU-FILE-ID                                     0004050
BT170 *                   RTN-CODE.                                      0004051
           MOVE GU-KEY TO VSAM-GU-PRIM                                  DELLIDCH
           READ VSAM-GU                                                 DELLIDCH
                    INTO GU-GUID-RECORD                                 DELLIDCH
           MOVE VSAM-GU-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE GU-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE GU-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
BT170                                                                    0004052
BT170      IF RTN-CODE > +0                                              0004053
BT170          GO TO 4640-CONTINUE.                                      0004054
BT170                                                                    0004055
BT170      MOVE POLICY-NA TO GU-POLICY.                                  0004056
BT170  4635-UPDATE-GU-FILE.                                              0004057
BT170 *    CALL 'DISKUP' USING GU-GUID-RECORD                            0004058
BT170 *                 GU-KEY                                           0004059
BT170 *                 GU-FILE-ID                                       0004060
BT170 *                 RTN-CODE.                                        0004061
           MOVE GU-KEY TO VSAM-GU-PRIM                                  DELLIDCH
           REWRITE VSAM-GU-REC                                          DELLIDCH
                      FROM GU-GUID-RECORD                               DELLIDCH
           MOVE VSAM-GU-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE GU-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE GU-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE                       DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
BT170                                                                    0004062
BT170 *    IF (RTN-CODE NOT = +0)                                       DELLMNCH
BT170 *        CALL 'TPFIRLFN' USING GU-FILE-ID                         DELLMNCH
BT170 *                       REL-RTN-CODE.                             DELLMNCH
BT170                                                                    0004066
BT170  4640-CONTINUE.                                                    0004067
BT170      IF CURR-STAT-NA = 010                                         0004068
BT170          MOVE 'Y' TO ERROR-SWITCH                                  0004069
BT170              MOVE 'E75' TO WK-ERROR-CODE                           0004070
BT170              PERFORM 3700-SY-ERR-MSG                               0004071
DL321              PERFORM 5200-UPDATE-AB-STATUS                         DELLRET3
DL322              PERFORM 3800-DELETE-AB-RECS                           DELLRET3
BT170          GO TO 4699-CHECK-NEW-BUSINESS-EXIT.                       0004072
BT170 * ADD CODE TO CHECK IF POLICY STATUS 010 ON AP FILE RECYCLE WA     0004073
BT170 * TRANSACTIONS  ELSE CONTINUE TO RESET RECORDS                     0004074
BT170  4645-DELETE-AP-RECS.                                              0004075
BT170      MOVE GU-POLICY TO SAVE-PAPOL-WASY                             0004076
BT142                     SAVE-PASS-POLICY                               0004077
BT142                     SAVE-NEXT-POLICY.                              0004078
BT142      PERFORM 7800-DEL-ALL-AP-RECORDS.                              0004079
BT261      PERFORM 7900-DEL-ALL-NB-RECORDS.                             DELLRETC
BT282      IF ERROR-SWITCH = WS-Y                                       DELLRET2
BT282          GO TO 4699-CHECK-NEW-BUSINESS-EXIT.                      DELLRET2
BT142      PERFORM 3900-INITIALIZE-RECORDS.                              0004080
BT142                                                                    0004081
BT142  4699-CHECK-NEW-BUSINESS-EXIT.                                     0004082
BT142      EXIT.                                                         0004083
BT142                                                                    0004084
BT142  4700-CHECK-DUPLICATE   SECTION.                                   0004085
BT142      PERFORM 9000-READ-GU.                                         0004086
BT142      IF RTN-CODE = +0                                              0004087
BT142          GO TO 4710-POLICY-BLANK.                                  0004088
BT142                                                                    0004089
BT142      IF RTN-CODE > +0                                              0004090
BT142          GO TO 4799-CHECK-DUPLICATE-EXIT.                          0004091
BT142  4710-POLICY-BLANK.                                                0004092
BT142      IF GU-POLICY = SPACES                                         0004093
BT142          GO TO 4799-CHECK-DUPLICATE-EXIT.                          0004094
BT142                                                                    0004095
BT142      MOVE 'Y' TO ERROR-SWITCH.                                     0004096
BT142      MOVE SPACES TO WA-APPLICATION-RECORD                          0004097
BT142             WS-KEY.                                                0004098
BT142      MOVE +0 TO RTN-CODE                                           0004099
BT142              WS-SUB1                                               0004100
BT142              WS-SUB2.                                              0004101
BT142      MOVE HWA-TRANS-TYPE TO WS-TRANS-TYPE.                         0004102
BT142      MOVE SV-CO TO WS-CO.                                          0004103
BT142      MOVE HWA-DELL-GUID TO WS-DELL-GUID.                           0004104
BT142      MOVE HWA-TOTAL-RECS TO WS-TOTAL-RECS                          0004105
BT142                          HOLD-TOTAL-RECS.                          0004106
BT142      MOVE 000 TO WS-DUP-SEQ.                                       0004107
BT142      MOVE LOW-VALUES TO WS-REC-TYPE.                               0004108
BT142  4720-READ-VALID-WA.                                               0004109
BT142      PERFORM 8300-READ-NEXT-WA-RECS.                               0004110
BT142      IF RTN-CODE = +12                                             0004111
BT142          DISPLAY ' WA FILE I/O ERROR-PROGRAM RTN-CODE = +12'       0004112
BT142          DISPLAY ' ERROR IN 4720-READ-VALID-WA PARAGRAPH '         0004113
BT142          DISPLAY ' ERROR ON WA READ - WA-DELL-GUID ' HWA-DELL-GUID 0004114
BT142          DISPLAY                                                   0004115
BT142      'PROGRAM LSNNB402 IS ABENDING . . . CHECK WA STATUS '         0004115
BT142          MOVE +4720 TO ABEND-CODE                                  0004116
BT142          PERFORM 9900-ABEND-RTN.                                   0004117
BT142                                                                    0004118
BT142      IF RTN-CODE = +1                                              0004119
BT142          GO TO 4799-CHECK-DUPLICATE-EXIT.                          0004120
BT142                                                                    0004121
DL457      ADD +1 TO WS-REC-READ.                                       DELLRT17
BT142      IF HWA-DELL-GUID = WA-DELL-GUID                               0004122
BT142       AND HWA-TRANS-TYPE = WA-TRANS-TYPE                           0004123
BT142       AND HWA-CO = WA-CO                                           0004124
BT142       AND HWA-TOTAL-RECS = WS-REC-COUNT                            0004125
BT142          NEXT SENTENCE                                             0004126
BT142      ELSE                                                          0004127
BT142          GO TO 4799-CHECK-DUPLICATE-EXIT.                          0004128
BT142                                                                    0004129
BT142      IF HWA-DELL-GUID = WA-DELL-GUID                               0004130
BT142       AND HWA-TRANS-TYPE = WA-TRANS-TYPE                           0004131
BT142       AND HWA-CO = WA-CO                                           0004132
BT142       AND WA-DUP-SEQ = 000                                         0004133
BT142       AND WA-STATUS = SPACES                                       0004134
BT142          GO TO 4730-SET-ERROR.                                     0004135
BT142                                                                    0004136
BT142      GO TO 4720-READ-VALID-WA.                                     0004137
BT142  4730-SET-ERROR.                                                   0004138
BT142 *    CALL 'DISKHOLD' USING WA-APPLICATION-RECORD                   0004139
BT142 *                   WA-KEY                                         0004140
BT142 *                   WA-FILE-IDZ                                    0004141
BT142 *                   RTN-CODE.                                      0004142
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           READ VSAM-WA                                                 DELLIDCH
                    INTO WA-APPLICATION-RECORD                          DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
BT142                                                                    0004143
BT142      IF RTN-CODE > +0                                              0004144
BT142          DISPLAY                                                   0004145
BT142      ' WA FILE I/O ERROR-PROGRAM RTN-CODE > +0 ' RTN-CODE          0004145
BT142          DISPLAY ' ERROR IN 4730-SET-ERROR PARAGRAPH '             0004146
BT142          DISPLAY                                                   0004147
BT142      ' ERROR ON DISKHOLD - WA-DELL-GUID ' HWA-DELL-GUID            0004147
BT142          DISPLAY                                                   0004148
BT142      'PROGRAM LSNNB402 IS ABENDING . . . CHECK WA STATUS '         0004148
BT142          MOVE +4730 TO ABEND-CODE                                  0004149
BT142          PERFORM 9900-ABEND-RTN.                                   0004150
BT142                                                                    0004151
BT142      MOVE WK-SYSTEM-DATE TO WA-MF-DATE.                            0004152
BT142      MOVE WK-SYSTEM-TIME TO WA-MF-TIME.                            0004153
BT142      MOVE 'DCG' TO WA-STATUS.                                      0004154
BT142  4740-WA-DISKUP.                                                   0004155
BT142 *    CALL 'DISKUP' USING WA-APPLICATION-RECORD                     0004156
BT142 *                 WA-KEY                                           0004157
BT142 *                 WA-FILE-IDZ                                      0004158
BT142 *                 RTN-CODE.                                        0004159
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           REWRITE VSAM-WA-REC                                          DELLIDCH
                      FROM WA-APPLICATION-RECORD                        DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE                       DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
BT142                                                                    0004160
BT142      IF (RTN-CODE NOT = +0)                                        0004161
BT142 *        CALL 'TPFIRLFN' USING WA-FILE-IDZ                        DELLMNCH
BT142 *                       REL-RTN-CODE                              DELLMNCH
BT142          DISPLAY                                                   0004164
BT142      ' WA FILE I/O ERROR-PROGRAM RTN-CODE > +0 ' RTN-CODE          0004164
BT142          DISPLAY ' ERROR IN 4740-WA-DISKUP PARAGRAPH '             0004165
BT142          DISPLAY ' ERROR ON DISKUP - WA-DELL-GUID ' HWA-DELL-GUID  0004166
BT142          DISPLAY                                                   0004167
BT142      'PROGRAM LSNNB402 IS ABENDING . . . CHECK WA STATUS '         0004167
BT142          MOVE +4740 TO ABEND-CODE                                  0004168
BT142          PERFORM 9900-ABEND-RTN.                                   0004169
BT142                                                                    0004170
BT142      GO TO 4720-READ-VALID-WA.                                     0004171
BT142                                                                    0004172
BT142  4799-CHECK-DUPLICATE-EXIT.                                        0004173
BT142      EXIT.                                                         0004174
BT142                                                                    0004175
BT142  4800-SET-TASK-RECORD SECTION.                                     0004176
BT142      MOVE SPACES TO TASK-RECORD.                                   0004177
BT142      MOVE WS-EENRL TO TK010-PS-PROCESS.                            0004178
BT142      MOVE WS-CHG  TO TK010-PS-STEP.                                0004179
BT142      PERFORM 3500-GET-WF-DESCRIPTION.                              0004180
BT142      IF RTN-CODE > +0                                              0004181
BT142          GO TO 4899-SET-TASK-RECORD-EXIT.                          0004182
BT142                                                                    0004183
BT142      MOVE WS-EENRL TO TASK-PROCESS.                                0004184
BT142      MOVE WS-CHG TO TASK-STEP.                                     0004185
BT142      MOVE SV-CO TO TASK-POLICY-CO.                                 0004186
BT142      MOVE NB452-POLICY-NUMBER TO TASK-POLICY-NUM.                  0004187
BT142      MOVE SPACES TO TASK-POLICY-RDR.                               0004188
BT181      MOVE SAVE-EMPLOYER-ID TO TASK-GROUP-NUMBER.                   0004189
BT170      MOVE HWA-DELL-GUID TO TASK-COMMENT1-A.                        0004190
BT142      MOVE 'CHANGE REQUIRED'                                        0004191
BT142         TO TASK-COMMENT2-A.                                        0004192
BT302      MOVE 'CHANGE INDICATOR: ' TO WS-SET-REMARK3-MSG.             DELLRET2
BT302      MOVE SAVE-LS-PARAM-37 TO WS-SET-REMARK3-PARAM-37.            DELLRET2
BT302      MOVE SPACES TO WS-SET-REMARK3-SPACE.                         DELLRET2
BT302      MOVE SAVE-LS-PARAM-3 TO WS-SET-REMARK3-GROUP.                DELLRET2
BT302      MOVE SAVE-LS-PARAM-4 TO WS-SET-REMARK3-SUBCODE.              DELLRET2
BT302      MOVE SAVE-LS-PARAM-5 TO WS-SET-REMARK3-BRANCH.               DELLRET2
BT302      MOVE WS-SET-REMARK3 TO TASK-COMMENT3-A.                      DELLRET2
BT212      IF NB452-REMARK3 = 'Y'                                        0004196
BT212          MOVE SPACES TO TASK-COMMENT3-A                            0004197
BT212          MOVE 'CREATE LETTER FOR PORT ' TO TASK-COMMENT3-A.        0004198
BT142      MOVE '5' TO TASK-PRIORITY.                                    0004199
BT142      MOVE SPACES TO TASK-ADDTO-DATABASE.                           0004200
BT142      MOVE WS-P TO TASK-STATUS.                                     0004201
BT142      MOVE WS-STPS TO TASK-SENDER-OPID.                             0004202
BT142                                                                    0004203
BT142      MOVE WK-SYSTEM-DATE TO TASK-PENDING-DATE.                     0004204
BT142      MOVE WK-SYSTEM-TIME TO TASK-PENDING-TIME.                     0004205
BT142      MOVE WK-SYSTEM-DATE TO TASK-ADDED-DATE.                       0004206
BT142                                                                    0004207
BT142      MOVE 000000000 TO TASK-ID.                                    0004208
BT142      MOVE TASK-ID TO TASK-MASTER-ID.                               0004209
BT142      MOVE WK-SYSTEM-DATE TO TASK-MSTR-PDATE.                       0004210
BT170      MOVE HWA-IMAGE-REFID TO TASK-IMAGE-ID.                        0004211
BT142      PERFORM 4000-TASK-DISKADD.                                    0004212
BT142                                                                    0004213
BT142  4899-SET-TASK-RECORD-EXIT.                                        0004214
BT142      EXIT.                                                         0004215
BT212                                                                    0004216
BT212  4800-NFU-TASK-RECORD SECTION.                                     0004217
BT212      MOVE SPACES TO TASK-RECORD.                                   0004218
BT212      MOVE WS-EENRL TO TK010-PS-PROCESS.                            0004219
BT212      MOVE 'NFU' TO TK010-PS-STEP.                                  0004220
BT212      PERFORM 3500-GET-WF-DESCRIPTION.                              0004221
BT212      IF RTN-CODE > +0                                              0004222
BT212          GO TO 4899-NFU-TASK-RECORD-EXIT.                          0004223
BT212                                                                    0004224
BT212      MOVE WS-EENRL TO TASK-PROCESS.                                0004225
BT212      MOVE 'NFU' TO TASK-STEP.                                      0004226
BT212      MOVE SV-CO TO TASK-POLICY-CO.                                 0004227
BT212      MOVE NB452-POLICY-NUMBER TO TASK-POLICY-NUM.                  0004228
BT212      MOVE SPACES TO TASK-POLICY-RDR.                               0004229
BT212      MOVE SAVE-EMPLOYER-ID TO TASK-GROUP-NUMBER.                   0004230
BT212      MOVE HWA-DELL-GUID TO TASK-COMMENT1-A.                        0004231
BT212      MOVE 'NFU UPDATE FAILED '                                     0004232
BT212         TO TASK-COMMENT2-A.                                        0004233
BT212      MOVE 'CHANGE INDICATOR: ' TO WS-REMARK3-MSG.                  0004234
BT212      MOVE SAVE-LS-PARAM-37 TO WS-REMARK3-PARAM-37.                 0004235
BT212      MOVE WS-REMARK3 TO TASK-COMMENT3-A.                           0004236
BT212      MOVE '5' TO TASK-PRIORITY.                                    0004237
BT212      MOVE SPACES TO TASK-ADDTO-DATABASE.                           0004238
BT212      MOVE WS-P TO TASK-STATUS.                                     0004239
BT212      MOVE WS-STPS TO TASK-SENDER-OPID.                             0004240
BT212                                                                    0004241
BT212      MOVE WK-SYSTEM-DATE TO TASK-PENDING-DATE.                     0004242
BT212      MOVE WK-SYSTEM-TIME TO TASK-PENDING-TIME.                     0004243
BT212      MOVE WK-SYSTEM-DATE TO TASK-ADDED-DATE.                       0004244
BT212                                                                    0004245
BT212      MOVE 000000000 TO TASK-ID.                                    0004246
BT212      MOVE TASK-ID TO TASK-MASTER-ID.                               0004247
BT212      MOVE WK-SYSTEM-DATE TO TASK-MSTR-PDATE.                       0004248
BT212      MOVE HWA-IMAGE-REFID TO TASK-IMAGE-ID.                        0004249
BT212      PERFORM 4000-TASK-DISKADD.                                    0004250
BT212                                                                    0004251
BT212  4899-NFU-TASK-RECORD-EXIT.                                        0004252
BT212      EXIT.                                                         0004253
BT142                                                                    0004254
BT301                                                                   DELLRET2
BT301  4800-DUP-TASK-RECORD SECTION.                                    DELLRET2
BT301      MOVE SPACES TO TASK-RECORD.                                  DELLRET2
BT301      MOVE 'UPDATE' TO TK010-PS-PROCESS.                           DELLRET2
BT301      MOVE 'DUP' TO TK010-PS-STEP.                                 DELLRET2
BT301      PERFORM 3500-GET-WF-DESCRIPTION.                             DELLRET2
BT301      IF RTN-CODE > +0                                             DELLRET2
BT301          GO TO 4899-DUP-TASK-RECORD-EXIT.                         DELLRET2
BT301                                                                   DELLRET2
BT301      MOVE 'UPDATE' TO TASK-PROCESS.                               DELLRET2
BT301      MOVE 'DUP' TO TASK-STEP.                                     DELLRET2
BT301      MOVE SV-CO TO TASK-POLICY-CO.                                DELLRET2
BT301      MOVE NB452-POLICY-NUMBER TO TASK-POLICY-NUM.                 DELLRET2
BT301      MOVE SPACES TO TASK-POLICY-RDR.                              DELLRET2
BT301      MOVE SAVE-EMPLOYER-ID TO TASK-GROUP-NUMBER.                  DELLRET2
BT301      MOVE HWA-DELL-GUID TO TASK-COMMENT1-A.                       DELLRET2
BT301      MOVE 'CHANGE REQUIRED'                                       DELLRET2
BT301         TO TASK-COMMENT2-A.                                       DELLRET2
BT301      MOVE 'CHANGE INDICATOR: ' TO WS-DUP-REMARK3-MSG.             DELLRET2
BT301      MOVE SAVE-LS-PARAM-37 TO WS-DUP-REMARK3-PARAM-37.            DELLRET2
BT301      MOVE SPACES TO WS-DUP-REMARK3-SPACE.                         DELLRET2
BT301      MOVE SAVE-LS-PARAM-3 TO WS-DUP-REMARK3-GROUP.                DELLRET2
BT301      MOVE SAVE-LS-PARAM-4 TO WS-DUP-REMARK3-SUBCODE.              DELLRET2
BT301      MOVE SAVE-LS-PARAM-5 TO WS-DUP-REMARK3-BRANCH.               DELLRET2
BT301      MOVE WS-DUP-REMARK3 TO TASK-COMMENT3-A.                      DELLRET2
BT301      MOVE '5' TO TASK-PRIORITY.                                   DELLRET2
BT301      MOVE SPACES TO TASK-ADDTO-DATABASE.                          DELLRET2
BT301      MOVE WS-P TO TASK-STATUS.                                    DELLRET2
BT301      MOVE WS-STPS TO TASK-SENDER-OPID.                            DELLRET2
BT301                                                                   DELLRET2
BT301      MOVE WK-SYSTEM-DATE TO TASK-PENDING-DATE.                    DELLRET2
BT301      MOVE WK-SYSTEM-TIME TO TASK-PENDING-TIME.                    DELLRET2
BT301      MOVE WK-SYSTEM-DATE TO TASK-ADDED-DATE.                      DELLRET2
BT301                                                                   DELLRET2
BT301      MOVE 000000000 TO TASK-ID.                                   DELLRET2
BT301      MOVE TASK-ID TO TASK-MASTER-ID.                              DELLRET2
BT301      MOVE WK-SYSTEM-DATE TO TASK-MSTR-PDATE.                      DELLRET2
BT301      MOVE HWA-IMAGE-REFID TO TASK-IMAGE-ID.                       DELLRET2
BT301      PERFORM 4000-TASK-DISKADD.                                   DELLRET2
BT301                                                                   DELLRET2
BT301  4899-DUP-TASK-RECORD-EXIT.                                       DELLRET2
BT301      EXIT.                                                        DELLRET2
BT301                                                                   DELLRET2
BT345  4800-LTR-TASK-RECORD SECTION.                                    DELLRET6
BT345      MOVE SPACES TO TASK-RECORD.                                  DELLRET6
BT345      MOVE 'EENRL ' TO TK010-PS-PROCESS.                           DELLRET6
BT345      MOVE 'LTR' TO TK010-PS-STEP.                                 DELLRET6
BT345      PERFORM 3500-GET-WF-DESCRIPTION.                             DELLRET6
BT345      IF RTN-CODE > +0                                             DELLRET6
BT345          GO TO 4899-LTR-TASK-RECORD-EXIT.                         DELLRET6
BT345                                                                   DELLRET6
BT345      MOVE 'EENRL ' TO TASK-PROCESS.                               DELLRET6
BT345      MOVE 'LTR' TO TASK-STEP.                                     DELLRET6
BT345      MOVE SV-CO TO TASK-POLICY-CO.                                DELLRET6
BT345      MOVE NB452-POLICY-NUMBER TO TASK-POLICY-NUM.                 DELLRET6
BT345      MOVE SPACES TO TASK-POLICY-RDR.                              DELLRET6
BT345      MOVE SAVE-EMPLOYER-ID TO TASK-GROUP-NUMBER.                  DELLRET6
BT345      MOVE HWA-DELL-GUID TO TASK-COMMENT1-A.                       DELLRET6
BT345      MOVE '               '                                       DELLRET6
BT345         TO TASK-COMMENT2-A.                                       DELLRET6
BT345      MOVE '            ' TO TASK-COMMENT3-A.                      DELLRET6
BT345      MOVE '5' TO TASK-PRIORITY.                                   DELLRET6
BT345      MOVE SPACES TO TASK-ADDTO-DATABASE.                          DELLRET6
BT345      MOVE WS-H TO TASK-STATUS.                                    DELLRET6
BT345      MOVE WS-STPS TO TASK-SENDER-OPID.                            DELLRET6
BT345                                                                   DELLRET6
BT345      MOVE WK-SYSTEM-DATE TO TASK-PENDING-DATE.                    DELLRET6
BT345      MOVE WK-SYSTEM-TIME TO TASK-PENDING-TIME.                    DELLRET6
BT345      MOVE WK-SYSTEM-DATE TO TASK-ADDED-DATE.                      DELLRET6
BT345                                                                   DELLRET6
BT345      MOVE NB452-ACTION-DATE TO TASK-ACTION-DATE                   DELLRET6
BT345      MOVE 000000000 TO TASK-ID.                                   DELLRET6
BT345      MOVE TASK-ID TO TASK-MASTER-ID.                              DELLRET6
BT345      MOVE WK-SYSTEM-DATE TO TASK-MSTR-PDATE.                      DELLRET6
BT345      MOVE HWA-IMAGE-REFID TO TASK-IMAGE-ID.                       DELLRET6
BT345      PERFORM 4000-TASK-DISKADD.                                   DELLRET6
BT345                                                                   DELLRET6
BT345  4899-LTR-TASK-RECORD-EXIT.                                       DELLRET6
BT345      EXIT.                                                        DELLRET6
BT345                                                                   DELLRET6
BT345  4800-STS-TASK-RECORD SECTION.                                    DELLRET6
BT345      MOVE SPACES TO TASK-RECORD.                                  DELLRET6
BT345      MOVE 'EENRL ' TO TK010-PS-PROCESS.                           DELLRET6
BT345      MOVE 'STS' TO TK010-PS-STEP.                                 DELLRET6
BT345      PERFORM 3500-GET-WF-DESCRIPTION.                             DELLRET6
BT345      IF RTN-CODE > +0                                             DELLRET6
BT345          GO TO 4899-STS-TASK-RECORD-EXIT.                         DELLRET6
BT345                                                                   DELLRET6
BT345      MOVE 'EENRL ' TO TASK-PROCESS.                               DELLRET6
BT345      MOVE 'STS' TO TASK-STEP.                                     DELLRET6
BT345      MOVE SV-CO TO TASK-POLICY-CO.                                DELLRET6
BT345      MOVE NB452-POLICY-NUMBER TO TASK-POLICY-NUM.                 DELLRET6
BT345      MOVE SPACES TO TASK-POLICY-RDR.                              DELLRET6
BT345      MOVE SAVE-EMPLOYER-ID TO TASK-GROUP-NUMBER.                  DELLRET6
BT345      MOVE HWA-DELL-GUID TO TASK-COMMENT1-A.                       DELLRET6
BT345      MOVE '               '                                       DELLRET6
BT345         TO TASK-COMMENT2-A.                                       DELLRET6
BT345      MOVE '            ' TO TASK-COMMENT3-A.                      DELLRET6
BT345      MOVE '5' TO TASK-PRIORITY.                                   DELLRET6
BT345      MOVE SPACES TO TASK-ADDTO-DATABASE.                          DELLRET6
BT345      MOVE WS-P TO TASK-STATUS.                                    DELLRET6
BT345      MOVE WS-STPS TO TASK-SENDER-OPID.                            DELLRET6
BT345                                                                   DELLRET6
BT345      MOVE WK-SYSTEM-DATE TO TASK-PENDING-DATE.                    DELLRET6
BT345      MOVE WK-SYSTEM-TIME TO TASK-PENDING-TIME.                    DELLRET6
BT345      MOVE WK-SYSTEM-DATE TO TASK-ADDED-DATE.                      DELLRET6
BT345                                                                   DELLRET6
BT345      MOVE 000000000 TO TASK-ID.                                   DELLRET6
BT345      MOVE TASK-ID TO TASK-MASTER-ID.                              DELLRET6
BT345      MOVE WK-SYSTEM-DATE TO TASK-MSTR-PDATE.                      DELLRET6
BT345      MOVE HWA-IMAGE-REFID TO TASK-IMAGE-ID.                       DELLRET6
BT345      PERFORM 4000-TASK-DISKADD.                                   DELLRET6
BT345                                                                   DELLRET6
BT345  4899-STS-TASK-RECORD-EXIT.                                       DELLRET6
BT345      EXIT.                                                        DELLRET6
BT345                                                                   DELLRET6
BT345  4800-DTH-TASK-RECORD SECTION.                                    DELLRET6
BT345      MOVE SPACES TO TASK-RECORD.                                  DELLRET6
BT345      MOVE 'EENRL ' TO TK010-PS-PROCESS.                           DELLRET6
BT345      MOVE 'DTH' TO TK010-PS-STEP.                                 DELLRET6
BT345      PERFORM 3500-GET-WF-DESCRIPTION.                             DELLRET6
BT345      IF RTN-CODE > +0                                             DELLRET6
BT345          GO TO 4899-DTH-TASK-RECORD-EXIT.                         DELLRET6
BT345                                                                   DELLRET6
BT345      MOVE 'EENRL ' TO TASK-PROCESS.                               DELLRET6
BT345      MOVE 'DTH' TO TASK-STEP.                                     DELLRET6
BT345      MOVE SV-CO TO TASK-POLICY-CO.                                DELLRET6
BT345      MOVE NB452-POLICY-NUMBER TO TASK-POLICY-NUM.                 DELLRET6
BT345      MOVE SPACES TO TASK-POLICY-RDR.                              DELLRET6
BT345      MOVE SAVE-EMPLOYER-ID TO TASK-GROUP-NUMBER.                  DELLRET6
BT345      MOVE HWA-DELL-GUID TO TASK-COMMENT1-A.                       DELLRET6
BT345      MOVE '               '                                       DELLRET6
BT345         TO TASK-COMMENT2-A.                                       DELLRET6
BT345      MOVE '            ' TO TASK-COMMENT3-A.                      DELLRET6
BT345      MOVE '5' TO TASK-PRIORITY.                                   DELLRET6
BT345      MOVE SPACES TO TASK-ADDTO-DATABASE.                          DELLRET6
BT345      MOVE WS-P TO TASK-STATUS.                                    DELLRET6
BT345      MOVE WS-STPS TO TASK-SENDER-OPID.                            DELLRET6
BT345                                                                   DELLRET6
BT345      MOVE WK-SYSTEM-DATE TO TASK-PENDING-DATE.                    DELLRET6
BT345      MOVE WK-SYSTEM-TIME TO TASK-PENDING-TIME.                    DELLRET6
BT345      MOVE WK-SYSTEM-DATE TO TASK-ADDED-DATE.                      DELLRET6
BT345                                                                   DELLRET6
BT345      MOVE 000000000 TO TASK-ID.                                   DELLRET6
BT345      MOVE TASK-ID TO TASK-MASTER-ID.                              DELLRET6
BT345      MOVE WK-SYSTEM-DATE TO TASK-MSTR-PDATE.                      DELLRET6
BT345      MOVE HWA-IMAGE-REFID TO TASK-IMAGE-ID.                       DELLRET6
BT345      PERFORM 4000-TASK-DISKADD.                                   DELLRET6
BT345                                                                   DELLRET6
BT345  4899-DTH-TASK-RECORD-EXIT.                                       DELLRET6
BT345      EXIT.                                                        DELLRET6
BT345                                                                   DELLRET6
BT345  4800-COC-TASK-RECORD SECTION.                                    DELLRET6
BT345      MOVE SPACES TO TASK-RECORD.                                  DELLRET6
BT345      MOVE 'EENRL ' TO TK010-PS-PROCESS.                           DELLRET6
BT345      MOVE 'COC' TO TK010-PS-STEP.                                 DELLRET6
BT345      PERFORM 3500-GET-WF-DESCRIPTION.                             DELLRET6
BT345      IF RTN-CODE > +0                                             DELLRET6
BT345          GO TO 4899-COC-TASK-RECORD-EXIT.                         DELLRET6
BT345                                                                   DELLRET6
BT345      MOVE 'EENRL ' TO TASK-PROCESS.                               DELLRET6
BT345      MOVE 'COC' TO TASK-STEP.                                     DELLRET6
BT345      MOVE SV-CO TO TASK-POLICY-CO.                                DELLRET6
BT345      MOVE NB452-POLICY-NUMBER TO TASK-POLICY-NUM.                 DELLRET6
BT345      MOVE SPACES TO TASK-POLICY-RDR.                              DELLRET6
BT345      MOVE SAVE-EMPLOYER-ID TO TASK-GROUP-NUMBER.                  DELLRET6
BT345      MOVE HWA-DELL-GUID TO TASK-COMMENT1-A.                       DELLRET6
BT345      MOVE '               '                                       DELLRET6
BT345         TO TASK-COMMENT2-A.                                       DELLRET6
BT345      MOVE '            ' TO TASK-COMMENT3-A.                      DELLRET6
BT345      MOVE '5' TO TASK-PRIORITY.                                   DELLRET6
BT345      MOVE SPACES TO TASK-ADDTO-DATABASE.                          DELLRET6
BT345      MOVE WS-P TO TASK-STATUS.                                    DELLRET6
BT345      MOVE WS-STPS TO TASK-SENDER-OPID.                            DELLRET6
BT345                                                                   DELLRET6
BT345      MOVE WK-SYSTEM-DATE TO TASK-PENDING-DATE.                    DELLRET6
BT345      MOVE WK-SYSTEM-TIME TO TASK-PENDING-TIME.                    DELLRET6
BT345      MOVE WK-SYSTEM-DATE TO TASK-ADDED-DATE.                      DELLRET6
BT345                                                                   DELLRET6
BT345      MOVE 000000000 TO TASK-ID.                                   DELLRET6
BT345      MOVE TASK-ID TO TASK-MASTER-ID.                              DELLRET6
BT345      MOVE WK-SYSTEM-DATE TO TASK-MSTR-PDATE.                      DELLRET6
BT345      MOVE HWA-IMAGE-REFID TO TASK-IMAGE-ID.                       DELLRET6
BT345      PERFORM 4000-TASK-DISKADD.                                   DELLRET6
BT345                                                                   DELLRET6
BT345  4899-COC-TASK-RECORD-EXIT.                                       DELLRET6
BT345      EXIT.                                                        DELLRET6
BT345                                                                   DELLRET6
BT345  4800-ACT-TASK-RECORD SECTION.                                    DELLRET6
BT345      MOVE SPACES TO TASK-RECORD.                                  DELLRET6
BT345      MOVE 'EENRL ' TO TK010-PS-PROCESS.                           DELLRET6
BT345      MOVE 'ACT' TO TK010-PS-STEP.                                 DELLRET6
BT345      PERFORM 3500-GET-WF-DESCRIPTION.                             DELLRET6
BT345      IF RTN-CODE > +0                                             DELLRET6
BT345          GO TO 4899-ACT-TASK-RECORD-EXIT.                         DELLRET6
BT345                                                                   DELLRET6
BT345      MOVE 'EENRL ' TO TASK-PROCESS.                               DELLRET6
BT345      MOVE 'ACT' TO TASK-STEP.                                     DELLRET6
BT345      MOVE SV-CO TO TASK-POLICY-CO.                                DELLRET6
BT345      MOVE NB452-POLICY-NUMBER TO TASK-POLICY-NUM.                 DELLRET6
BT345      MOVE SPACES TO TASK-POLICY-RDR.                              DELLRET6
BT345      MOVE SAVE-EMPLOYER-ID TO TASK-GROUP-NUMBER.                  DELLRET6
DL631      IF TASK-GROUP-NUMBER = SPACES
DL631          MOVE NB452-EMPLOYER-ID TO TASK-GROUP-NUMBER.
BT345      MOVE HWA-DELL-GUID TO TASK-COMMENT1-A.                       DELLRET6
BT345      MOVE '               '                                       DELLRET6
BT345         TO TASK-COMMENT2-A.                                       DELLRET6
BT345      MOVE '            ' TO TASK-COMMENT3-A.                      DELLRET6
BT345      MOVE '5' TO TASK-PRIORITY.                                   DELLRET6
BT345      MOVE SPACES TO TASK-ADDTO-DATABASE.                          DELLRET6
AS551      IF NB452-POLICY-FOUND = 'S'
AS551         MOVE 'C' TO TASK-STATUS
NK200A        MOVE WK-SYSTEM-DATE TO TASK-CLOSED-DATE
AS551      ELSE
AS551         MOVE WS-P TO TASK-STATUS.                                 DELLRET6
BT345      MOVE WS-STPS TO TASK-SENDER-OPID.                            DELLRET6
BT345                                                                   DELLRET6
BT345      MOVE WK-SYSTEM-DATE TO TASK-PENDING-DATE.                    DELLRET6
BT345      MOVE WK-SYSTEM-TIME TO TASK-PENDING-TIME.                    DELLRET6
BT345      MOVE WK-SYSTEM-DATE TO TASK-ADDED-DATE.                      DELLRET6
BT345                                                                   DELLRET6
BT345      MOVE 000000000 TO TASK-ID.                                   DELLRET6
BT345      MOVE TASK-ID TO TASK-MASTER-ID.                              DELLRET6
BT345      MOVE WK-SYSTEM-DATE TO TASK-MSTR-PDATE.                      DELLRET6
BT345      MOVE HWA-IMAGE-REFID TO TASK-IMAGE-ID.                       DELLRET6
BT345      PERFORM 4000-TASK-DISKADD.                                   DELLRET6
BT345                                                                   DELLRET6
BT345  4899-ACT-TASK-RECORD-EXIT.                                       DELLRET6
BT345      EXIT.                                                        DELLRET6
BT345                                                                   04933000
AS551  4800-GAP-TASK-RECORD SECTION.                                     
AS551      MOVE SPACES TO TASK-RECORD.                                   
AS551      MOVE 'EENRL ' TO TK010-PS-PROCESS.                            
AS551      MOVE 'GAP' TO TK010-PS-STEP.                                  
AS551      PERFORM 3500-GET-WF-DESCRIPTION.                              
AS551      IF RTN-CODE > +0                                              
AS551          GO TO 4899-GAP-TASK-RECORD-EXIT.                          
AS551                                                                    
AS551      MOVE 'EENRL ' TO TASK-PROCESS.                                
AS551      MOVE 'GAP' TO TASK-STEP.                                      
AS551      MOVE SV-CO TO TASK-POLICY-CO.                                 
AS551      MOVE NB452-POLICY-NUMBER TO TASK-POLICY-NUM.                  
AS551      MOVE SPACES TO TASK-POLICY-RDR.                               
AS551      MOVE SAVE-EMPLOYER-ID TO TASK-GROUP-NUMBER.                   
DL631      IF TASK-GROUP-NUMBER = SPACES
DL631          MOVE NB452-EMPLOYER-ID TO TASK-GROUP-NUMBER.
AS551      MOVE HWA-DELL-GUID TO TASK-COMMENT1-A.                        
AS551      MOVE '               '                                        
AS551         TO TASK-COMMENT2-A.                                        
AS551      MOVE '            ' TO TASK-COMMENT3-A.                       
AS551      MOVE '5' TO TASK-PRIORITY.                                    
AS551      MOVE SPACES TO TASK-ADDTO-DATABASE.                           
AS551      MOVE WS-C TO TASK-STATUS.                                     
AS551      MOVE WS-STPS TO TASK-SENDER-OPID.                             
AS551                                                                    
AS551      MOVE WK-SYSTEM-DATE TO TASK-PENDING-DATE.                     
AS551      MOVE WK-SYSTEM-TIME TO TASK-PENDING-TIME.                     
AS551      MOVE WK-SYSTEM-DATE TO TASK-ADDED-DATE.                       
NK200A     MOVE WK-SYSTEM-DATE TO TASK-CLOSED-DATE.
AS551                                                                    
AS551      MOVE 000000000 TO TASK-ID.                                    
AS551      MOVE TASK-ID TO TASK-MASTER-ID.                               
AS551      MOVE WK-SYSTEM-DATE TO TASK-MSTR-PDATE.                       
AS551      MOVE HWA-IMAGE-REFID TO TASK-IMAGE-ID.                        
AS551      PERFORM 4000-TASK-DISKADD.                                    
AS551                                                                    
AS551  4899-GAP-TASK-RECORD-EXIT.                                        
AS551      EXIT.                                                         
AS551                                                                   
AS551  4800-DIR-TASK-RECORD SECTION.                                     
AS551      MOVE SPACES TO TASK-RECORD.                                   
AS551      MOVE 'EENRL ' TO TK010-PS-PROCESS.                            
AS551      MOVE 'DIR' TO TK010-PS-STEP.                                  
AS551      PERFORM 3500-GET-WF-DESCRIPTION.                              
AS551      IF RTN-CODE > +0                                              
AS551          GO TO 4899-DIR-TASK-RECORD-EXIT.                          
AS551                                                                    
AS551      MOVE 'EENRL ' TO TASK-PROCESS.                                
AS551      MOVE 'DIR' TO TASK-STEP.                                      
AS551      MOVE SV-CO TO TASK-POLICY-CO.                                 
AS551      MOVE NB452-POLICY-NUMBER TO TASK-POLICY-NUM.                  
AS551      MOVE SPACES TO TASK-POLICY-RDR.                               
AS551      MOVE SAVE-EMPLOYER-ID TO TASK-GROUP-NUMBER.                   
DL631      IF TASK-GROUP-NUMBER = SPACES
DL631          MOVE NB452-EMPLOYER-ID TO TASK-GROUP-NUMBER.
AS551      MOVE HWA-DELL-GUID TO TASK-COMMENT1-A.                        
AS551      MOVE '               '                                        
AS551         TO TASK-COMMENT2-A.                                        
AS551      MOVE '            ' TO TASK-COMMENT3-A.                       
AS551      MOVE '5' TO TASK-PRIORITY.                                    
AS551      MOVE SPACES TO TASK-ADDTO-DATABASE.                           
AS551      MOVE WS-P TO TASK-STATUS.                                  
AS551      MOVE WS-STPS TO TASK-SENDER-OPID.                             
AS551                                                                    
AS551      MOVE WK-SYSTEM-DATE TO TASK-PENDING-DATE.                     
AS551      MOVE WK-SYSTEM-TIME TO TASK-PENDING-TIME.                     
AS551      MOVE WK-SYSTEM-DATE TO TASK-ADDED-DATE.                       
AS551                                                                    
AS551      MOVE 000000000 TO TASK-ID.                                    
AS551      MOVE TASK-ID TO TASK-MASTER-ID.                               
AS551      MOVE WK-SYSTEM-DATE TO TASK-MSTR-PDATE.                       
AS551      MOVE HWA-IMAGE-REFID TO TASK-IMAGE-ID.                        
AS551      PERFORM 4000-TASK-DISKADD.                                    
AS551                                                                    
AS551  4899-DIR-TASK-RECORD-EXIT.                                        
AS551      EXIT.                                                         
AS551          
AS551  4800-REV-TASK-RECORD SECTION.                                     
AS551      MOVE SPACES TO TASK-RECORD.                                   
AS551      MOVE 'EENRL ' TO TK010-PS-PROCESS.                            
AS551      MOVE 'REV' TO TK010-PS-STEP.                                  
AS551      PERFORM 3500-GET-WF-DESCRIPTION.                              
AS551      IF RTN-CODE > +0                                              
AS551          GO TO 4899-REV-TASK-RECORD-EXIT.                          
AS551                                                                    
AS551      MOVE 'EENRL ' TO TASK-PROCESS.                                
AS551      MOVE 'REV' TO TASK-STEP.                                      
AS551      MOVE SV-CO TO TASK-POLICY-CO.                                 
AS551      MOVE NB452-POLICY-NUMBER TO TASK-POLICY-NUM.                  
AS551      MOVE SPACES TO TASK-POLICY-RDR.                               
AS551      MOVE SAVE-EMPLOYER-ID TO TASK-GROUP-NUMBER.                   
DL631      IF TASK-GROUP-NUMBER = SPACES
DL631          MOVE NB452-EMPLOYER-ID TO TASK-GROUP-NUMBER.
AS551      MOVE HWA-DELL-GUID TO TASK-COMMENT1-A.                        
AS551      MOVE '               '                                        
AS551         TO TASK-COMMENT2-A.                                        
AS551      MOVE '            ' TO TASK-COMMENT3-A.                       
AS551      MOVE '5' TO TASK-PRIORITY.                                    
AS551      MOVE SPACES TO TASK-ADDTO-DATABASE.                           
AS551      MOVE WS-P TO TASK-STATUS.                                  
AS551      MOVE WS-STPS TO TASK-SENDER-OPID.                             
AS551                                                                    
AS551      MOVE WK-SYSTEM-DATE TO TASK-PENDING-DATE.                     
AS551      MOVE WK-SYSTEM-TIME TO TASK-PENDING-TIME.                     
AS551      MOVE WK-SYSTEM-DATE TO TASK-ADDED-DATE.                       
AS551                                                                    
AS551      MOVE 000000000 TO TASK-ID.                                    
AS551      MOVE TASK-ID TO TASK-MASTER-ID.                               
AS551      MOVE WK-SYSTEM-DATE TO TASK-MSTR-PDATE.                       
AS551      MOVE HWA-IMAGE-REFID TO TASK-IMAGE-ID.                        
AS551      PERFORM 4000-TASK-DISKADD.                                    
AS551                                                                    
AS551  4899-REV-TASK-RECORD-EXIT.                                        
AS551      EXIT.                                                         
AS551                                                                   
BT371                                                                   DELLRET9
BT371  4800-CNX-TASK-RECORD SECTION.                                    DELLRET9
BT371      MOVE SPACES TO TASK-RECORD.                                  DELLRET9
BT371      MOVE 'EENRL ' TO TK010-PS-PROCESS.                           DELLRET9
BT371      MOVE 'CNX' TO TK010-PS-STEP.                                 DELLRET9
BT371      PERFORM 3500-GET-WF-DESCRIPTION.                             DELLRET9
BT371      IF RTN-CODE > +0                                             DELLRET9
BT371          GO TO 4899-CNX-TASK-RECORD-EXIT.                         DELLRET9
BT371                                                                   DELLRET9
BT371      MOVE 'EENRL ' TO TASK-PROCESS.                               DELLRET9
BT371      MOVE 'CNX' TO TASK-STEP.                                     DELLRET9
BT371      MOVE SV-CO TO TASK-POLICY-CO.                                DELLRET9
BT371      MOVE NB452-POLICY-NUMBER TO TASK-POLICY-NUM.                 DELLRET9
BT371      MOVE SPACES TO TASK-POLICY-RDR.                              DELLRET9
BT371      MOVE SAVE-EMPLOYER-ID TO TASK-GROUP-NUMBER.                  DELLRET9
BT371      MOVE HWA-DELL-GUID TO TASK-COMMENT1-A.                       DELLRET9
BT371      MOVE '               '                                       DELLRET9
BT371         TO TASK-COMMENT2-A.                                       DELLRET9
BT371      MOVE '            ' TO TASK-COMMENT3-A.                      DELLRET9
BT371      MOVE '5' TO TASK-PRIORITY.                                   DELLRET9
BT371      MOVE SPACES TO TASK-ADDTO-DATABASE.                          DELLRET9
BT371      MOVE WS-P TO TASK-STATUS.                                    DELLRET9
BT371      MOVE WS-STPS TO TASK-SENDER-OPID.                            DELLRET9
BT371                                                                   DELLRET9
BT371      MOVE WK-SYSTEM-DATE TO TASK-PENDING-DATE.                    DELLRET9
BT371      MOVE WK-SYSTEM-TIME TO TASK-PENDING-TIME.                    DELLRET9
BT371      MOVE WK-SYSTEM-DATE TO TASK-ADDED-DATE.                      DELLRET9
BT371                                                                   DELLRET9
BT371      MOVE 000000000 TO TASK-ID.                                   DELLRET9
BT371      MOVE TASK-ID TO TASK-MASTER-ID.                              DELLRET9
BT371      MOVE WK-SYSTEM-DATE TO TASK-MSTR-PDATE.                      DELLRET9
BT371      MOVE HWA-IMAGE-REFID TO TASK-IMAGE-ID.                       DELLRET9
BT371      PERFORM 4000-TASK-DISKADD.                                   DELLRET9
BT371                                                                   DELLRET9
BT371  4899-CNX-TASK-RECORD-EXIT.                                       DELLRET9
BT371      EXIT.                                                        DELLRET9
BT371                                                                   DELLRET9
BT151  4900-DEL-TASK SECTION.                                            0004255
BT151      MOVE WS-N TO WS-POL-REC-FOUND.                                0004256
BT151      MOVE +0 TO M1-RTN-CODE.                                       0004257
BT151      MOVE SPACES TO MASTER-STRUCTURE-RECORD                        0004258
BT151             SAVE-PASS-POLICY.                                      0004259
BT151      MOVE LOW-VALUES TO AI-M1-KEY.                                 0004260
           MOVE LOW-VALUES TO AI-M1-FILE-KEY.                           DELLMNCH
BT151      MOVE 'S' TO AI-M1-TYPE.                                       0004261
BT151      MOVE 'B' TO M1-TIER-BASE-IND, HOLD-TIER-BASE-IND.             0004262
           MOVE 'B' TO AI-M1-FILE-TIER-BASE-IND.                        DELLMNCH
BT151      MOVE 'EE ONLY' TO M1-RATE-DESC, HOLD-RATE-DESC.               0004263
           MOVE 'EE ONLY' TO AI-M1-FILE-RATE-DESC.                      DELLMNCH
BT151      MOVE SAVE-LS-PARAM-3 TO M1-RPT-NUM-X.                         0004264
BT151      MOVE SAVE-LS-PARAM-4 TO M1-SUBCODE-X.                         0004265
BT151      MOVE SAVE-LS-PARAM-5 TO M1-CLAIM-BRANCH.                      0004266
           MOVE SAVE-LS-PARAM-3 TO AI-M1-FILE-RPT-NUM.                  DELLMNCH
           MOVE SAVE-LS-PARAM-4 TO AI-M1-FILE-SUBCODE.                  DELLMNCH
           MOVE SAVE-LS-PARAM-5 TO AI-M1-FILE-CLAIM-BRANCH.             DELLMNCH
BT151  4910-M1-READ-LOOP.                                                0004267
BT151 *    CALL 'RDNXTDAT' USING MASTER-STRUCTURE-RECORD,                0004268
BT151 *                   AI-M1-KEY,                                     0004269
BT151 *                   M1-FILE-ID,                                    0004270
BT151 *                   M1-RTN-CODE.                                   0004271
      *     MOVE AI-M1-KEY TO VSAM-M1-PRIM                              DELLIDCH
           IF AI-M1-FILE-KEY <> VSAM-ALT-KEY1                           DELLMNCH
           MOVE AI-M1-FILE-KEY TO VSAM-ALT-KEY1                         DELLMNCH
            START VSAM-MS                                               DELLMNCH
      *           KEY IS >= VSAM-M1-PRIM                                DELLIDCH
                  KEY IS >  VSAM-ALT-KEY1                               DELLMNCH
            END-START                                                   DELLIDCH
            MOVE VSAM-MS-FS TO WS-INDX-FS-99                            DELLMNCH
            MOVE VSAM-MS-REC TO MASTER-STRUCTURE-RECORD                 DELLIDCH
            INITIALIZE TPSWNML-AREA                                     DELLIDCH
            MOVE AI-M1-FILE-KEY  TO TPSWNML-FILE-KEY                    DELLMNCH
            MOVE M1-FILE-ID TO TPSWNML-FILE-ID                          DELLIDCH
            MOVE 'RDNXTDAT' TO TPSWNML-FUNCTION-CODE                    DELLIDCH
            PERFORM P9999-MAP-RESP-CODE                                 DELLIDCH
            MOVE WS-INDX-FS-99 TO M1-RTN-CODE                           DELLIDCH
           END-IF                                                       DELLIDCH
           READ VSAM-MS NEXT RECORD                                     DELLMNCH
                  INTO MASTER-STRUCTURE-RECORD                          DELLIDCH
           MOVE VSAM-MS-FS TO WS-INDX-FS-99                             DELLIDCH
      *    MOVE AI-M1-KEY TO TPSWNML-FILE-KEY                           DELLIDCH
           MOVE MS-RATE-DESC TO TPSWNML-FILE-KEY(1:60)                  DELLMNCH
           MOVE MS-RPT-NUM   TO TPSWNML-FILE-KEY(61:7)                  DELLMNCH
           MOVE MS-SUBCODE   TO TPSWNML-FILE-KEY(68:4)                  DELLMNCH
           MOVE MS-CLAIM-BRANCH TO TPSWNML-FILE-KEY(73:4)               DELLMNCH
           MOVE VSAM-ALT-KEY1 TO AI-M1-FILE-KEY                         DELLMNCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO M1-RTN-CODE                            DELLIDCH
BT151      IF M1-RTN-CODE > +0                                           0004272
BT151             MOVE WS-N TO WS-POL-REC-FOUND                          0004273
BT151             GO TO 4930-SET-TASK-RECORD.                            0004274
BT151      IF (HOLD-TIER-BASE-IND NOT = MS-TIER-BASE-IND)                0004275
BT151          MOVE WS-N TO WS-POL-REC-FOUND                             0004276
BT151          GO TO 4930-SET-TASK-RECORD.                               0004277
BT151      IF (HOLD-RATE-DESC NOT = MS-RATE-DESC)                        0004278
BT151          MOVE WS-N TO WS-POL-REC-FOUND                             0004279
BT151          GO TO 4930-SET-TASK-RECORD.                               0004280
BT151      IF (SAVE-LS-PARAM-3 NOT = MS-RPT-NUM-X)                       0004281
BT151          MOVE WS-N TO WS-POL-REC-FOUND                             0004282
BT151          GO TO 4930-SET-TASK-RECORD.                               0004283
BT151      IF (SAVE-LS-PARAM-4 NOT = MS-SUBCODE-X)                       0004284
BT151          MOVE WS-N TO WS-POL-REC-FOUND                             0004285
BT151          GO TO 4930-SET-TASK-RECORD.                               0004286
BT151      IF (SAVE-LS-PARAM-5 NOT = MS-CLAIM-BRANCH)                    0004287
BT151          MOVE WS-N TO WS-POL-REC-FOUND                             0004288
BT151          GO TO 4930-SET-TASK-RECORD.                               0004289
BT151 ******  CHECK IF ON POLICY MASTER  ******************************  0004290
BT151 ************************************************************       0004291
BT151 ***  THIS SECTION READS THE WA FILE AND MOVES VALUES     ***       0004292
BT151 ***  FROM THE APAP2 RECORD TO THE NOTE AND TASK RECORDS. ***       0004293
BT151 ************************************************************       0004294
BT151      MOVE +1 TO WS-SUB1.                                           0004295
BT151      IF WTR-FILE-ID (WS-SUB1) = 'AP'                               0004296
BT151        AND WTR-REC-ID (WS-SUB1) = 'AP'                             0004297
BT151          NEXT SENTENCE                                             0004298
BT151      ELSE                                                          0004299
BT151          MOVE WS-N TO WS-POL-REC-FOUND                             0004300
BT151          GO TO 4930-SET-TASK-RECORD.                               0004301
BT151                                                                    0004302
BT151      MOVE +2 TO WS-SUB2.                                           0004303
BT151      MOVE +0 TO RTN-CODE.                                          0004304
BT151      MOVE SPACES TO WA-APPLICATION-RECORD.                         0004305
BT151      MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WA-KEY.                 0004306
BT151                                                                    0004307
BT151      PERFORM 8700-READ-KEY-WA.                                     0004308
BT151      IF RTN-CODE = +12                                             0004309
BT151          MOVE WS-N TO WS-POL-REC-FOUND                             0004310
BT151          GO TO 4930-SET-TASK-RECORD.                               0004311
BT151                                                                    0004312
BT151      IF RTN-CODE = +1                                              0004313
BT151          MOVE WS-N TO WS-POL-REC-FOUND                             0004314
BT151          GO TO 4930-SET-TASK-RECORD.                               0004315
BT151                                                                    0004316
BT151      MOVE WA-APPLICATION-RECORD TO HOLD-APAP2-RECORD.              0004317
BT151 ************************************************************       0004318
BT151 ***  THIS SECTION READS THE WA FILE AND MOVES VALUES     ***       0004319
BT151 ***  FROM THE APAP1 RECORD TO THE NOTE AND TASK RECORDS. ***       0004320
BT151 ************************************************************       0004321
BT151      MOVE +1 TO WS-SUB1.                                           0004322
BT151      IF WTR-FILE-ID (WS-SUB1) = 'AP'                               0004323
BT151        AND WTR-REC-ID (WS-SUB1) = 'AP'                             0004324
BT151          NEXT SENTENCE                                             0004325
BT151      ELSE                                                          0004326
BT151          MOVE WS-N TO WS-POL-REC-FOUND                             0004327
BT151          GO TO 4930-SET-TASK-RECORD.                               0004328
BT151                                                                    0004329
BT151      MOVE +1 TO WS-SUB2.                                           0004330
BT151      MOVE +0 TO RTN-CODE.                                          0004331
BT151      MOVE SPACES TO WA-APPLICATION-RECORD.                         0004332
BT151      MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WA-KEY.                 0004333
BT151                                                                    0004334
BT151      PERFORM 8700-READ-KEY-WA.                                     0004335
BT151                                                                    0004336
BT151      IF RTN-CODE = +12                                             0004337
BT151          MOVE WS-N TO WS-POL-REC-FOUND                             0004338
BT151          GO TO 4930-SET-TASK-RECORD.                               0004339
BT151                                                                    0004340
BT151      IF RTN-CODE = +1                                              0004341
BT151          MOVE WS-N TO WS-POL-REC-FOUND                             0004342
BT151          GO TO 4930-SET-TASK-RECORD.                               0004343
BT151                                                                    0004344
BT151      MOVE WA-APPLICATION-RECORD TO HOLD-APAP1-RECORD.              0004345
BT151                                                                    0004346
BT151      MOVE HOLD-APAP2-RECORD TO WA-APPLICATION-RECORD.              0004347
BT151      MOVE SPACES TO AI-KEY.                                        0004348
BT151      MOVE +0 TO RTN-CODE.                                          0004349
BT151      MOVE 'L' TO AI-TYPE.                                          0004350
BT151      MOVE WA-CO TO P2-FILE-CO.                                     0004351
BT151      MOVE APAP2-OLD-POLICY-NA TO AI-NAME.                          0004352
BT151  4915-NEXT-P2.                                                     0004353
BT151 *    CALL 'RDNXTDAT' USING LIFE-MASTER-RECORD                      0004354
BT151 *                   AI-KEY                                         0004355
BT151 *                   P2-FILEID                                      0004356
BT151 *                   RTN-CODE.                                      0004357
           IF AI-NAME(1:20) <> VSAM-ALT-KEY4                            DELLMNCH
           MOVE AI-NAME(1:20) TO VSAM-ALT-KEY4                          DELLMNCH
            START VSAM-PO                                               DELLMNCH
                  KEY IS >  VSAM-ALT-KEY4                               DELLMNCH
            END-START                                                   DELLIDCH
            MOVE VSAM-P2-FS TO WS-INDX-FS-99                            DELLIDCH
            MOVE VSAM-PO-REC TO LIFE-MASTER-RECORD                      DELLMNCH
            INITIALIZE TPSWNML-AREA                                     DELLIDCH
            MOVE VSAM-ALT-KEY4 TO TPSWNML-FILE-KEY                      DELLMNCH
            MOVE P2-FILEID TO TPSWNML-FILE-ID                           DELLIDCH
            MOVE 'RDNXTDAT' TO TPSWNML-FUNCTION-CODE                    DELLIDCH
            PERFORM P9999-MAP-RESP-CODE                                 DELLIDCH
            MOVE WS-INDX-FS-99 TO RTN-CODE                              DELLIDCH
           END-IF                                                       DELLIDCH
           READ VSAM-PO NEXT RECORD                                     DELLMNCH
                  INTO LIFE-MASTER-RECORD                               DELLIDCH
           MOVE VSAM-P2-FS TO WS-INDX-FS-99                             DELLIDCH
           MOVE LM-OLD-POLICY TO TPSWNML-FILE-KEY                       DELLMNCH
           MOVE VSAM-ALT-KEY4 TO AI-NAME                                DELLMNCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE                               DELLIDCH
BT151      IF RTN-CODE NOT = +0                                          0004358
BT151          GO TO 4915-CHECK-NEW-BUSINESS.                            0004359
BT151                                                                    0004360
           MOVE LM-OLD-POLICY TO AI-NAME.                               DELLMNCH
BT151      IF AI-TYPE = 'L'                                              0004361
BT151        AND AI-NAME = APAP2-OLD-POLICY-NA                           0004362
BT151          NEXT SENTENCE                                             0004363
BT151      ELSE                                                          0004364
BT151          GO TO 4915-CHECK-NEW-BUSINESS.                            0004365
BT151                                                                    0004366
BT151 ***********************************************************        0004367
BT151 *** NOW CHECK THE PR SEGMENT FOR MATCHING EMPLOYER ID   ***        0004368
BT151 ***********************************************************        0004369
BT151      MOVE LMZ-SEG-PR-LAYOUT TO LM-SEG-PR-LAYOUT.                   0004370
BT151      CALL 'SEGGETN' USING LIFE-MASTER-RECORD                       0004371
BT151                    LM-SEG-PR-LAYOUT.                               0004372
BT151      IF LM-SEG-PR-RC = +0                                          0004373
BT151          NEXT SENTENCE                                             0004374
BT151      ELSE                                                          0004375
BT151          GO TO 4915-NEXT-P2.                                       0004376
BT152                                                                    0004377
BT151      IF LM-PYRL-EMPLOYERID = MS-GROUP-ID                           0004378
BT151          NEXT SENTENCE                                             0004379
BT151      ELSE                                                          0004380
BT151          GO TO 4915-NEXT-P2.                                       0004381
BT151                                                                    0004382
BT151      MOVE WS-Y TO WS-POL-REC-FOUND.                                0004383
BT151      MOVE LM-POL TO SAVE-PASS-POLICY.                              0004384
BT151      GO TO 4930-SET-TASK-RECORD.                                   0004385
BT151                                                                    0004386
BT151 ******  CHECK IF IN NEW BUSINESS   ******************************  0004387
BT151  4915-CHECK-NEW-BUSINESS.                                          0004388
BT151      PERFORM 9000-READ-GU.                                         0004389
BT151      IF RTN-CODE = +0                                              0004390
BT151          GO TO 4920-LOOK-ON-AP-FILE.                               0004391
BT151                                                                    0004392
BT151      IF RTN-CODE > +0                                              0004393
BT151          MOVE WS-N TO WS-POL-REC-FOUND                             0004394
BT151          GO TO 4930-SET-TASK-RECORD.                               0004395
BT151  4920-LOOK-ON-AP-FILE.                                             0004396
BT151      MOVE +0 TO RTN-CODE.                                          0004397
BT151      MOVE SPACES TO WA-AP-KEY.                                     0004398
BT151      MOVE SV-CO TO WA-AP-CO.                                       0004399
BT151      MOVE GU-POLICY TO WA-AP-POLICY.                               0004400
BT151      MOVE 'AP' TO WA-AP-TYPE.                                      0004401
BT151      PERFORM 8900-READ-APPREC.                                     0004402
BT151      IF RTN-CODE = +1                                              0004403
BT151          GO TO 4930-SET-TASK-RECORD.                               0004404
BT151                                                                    0004405
BT151      IF RTN-CODE = +0                                              0004406
BT151          MOVE GU-POLICY TO SAVE-PASS-POLICY.                       0004407
BT151          MOVE WS-Y TO WS-POL-REC-FOUND.                            0004408
BT151                                                                    0004409
BT151  4930-SET-TASK-RECORD.                                             0004410
BT151      MOVE SPACES TO TASK-RECORD.                                   0004411
BT151      MOVE WS-EENRL TO TK010-PS-PROCESS.                            0004412
BT151      MOVE 'DEL' TO TK010-PS-STEP.                                  0004413
BT151      PERFORM 3500-GET-WF-DESCRIPTION.                              0004414
BT151      IF RTN-CODE > +0                                              0004415
BT151          GO TO 4999-DEL-TASK-EXIT.                                 0004416
BT151                                                                    0004417
BT151      MOVE WS-EENRL TO TASK-PROCESS.                                0004418
BT151      MOVE 'DEL' TO TASK-STEP.                                      0004419
BT151      MOVE SV-CO TO TASK-POLICY-CO.                                 0004420
BT151      MOVE SAVE-PASS-POLICY TO TASK-POLICY-NUM.                     0004421
BT151      MOVE SPACES TO TASK-POLICY-RDR.                               0004422
BT181      MOVE SAVE-EMPLOYER-ID TO TASK-GROUP-NUMBER.                   0004423
BT151      MOVE WA-DELL-GUID TO TASK-COMMENT1-A.                         0004424
BT151      MOVE 'DELETE REQUESTED'                                       0004425
BT151         TO TASK-COMMENT2-A.                                        0004426
BT151      MOVE SPACES TO TASK-COMMENT3-A.                               0004427
BT151      IF WS-POL-REC-FOUND = WS-N                                    0004428
BT151          MOVE 'MATCH NOT FOUND'                                    0004429
BT151             TO TASK-COMMENT3-A.                                    0004430
BT151      MOVE '5' TO TASK-PRIORITY.                                    0004431
BT151      MOVE SPACES TO TASK-ADDTO-DATABASE.                           0004432
BT151      MOVE WS-P TO TASK-STATUS.                                     0004433
BT151      MOVE WS-STPS TO TASK-SENDER-OPID.                             0004434
BT151                                                                    0004435
BT151      MOVE WK-SYSTEM-DATE TO TASK-PENDING-DATE.                     0004436
BT151      MOVE WK-SYSTEM-TIME TO TASK-PENDING-TIME.                     0004437
BT151      MOVE WK-SYSTEM-DATE TO TASK-ADDED-DATE.                       0004438
BT151                                                                    0004439
BT151      MOVE 000000000 TO TASK-ID.                                    0004440
BT151      MOVE TASK-ID TO TASK-MASTER-ID.                               0004441
BT151      MOVE WK-SYSTEM-DATE TO TASK-MSTR-PDATE.                       0004442
BT151      MOVE WA-IMAGE-REFID TO TASK-IMAGE-ID.                         0004443
BT193      MOVE TASK-STATUS TO SAVE-TASK-INDICATOR.                      0004444
BT151      PERFORM 4000-TASK-DISKADD.                                    0004445
BT151  4999-DEL-TASK-EXIT.                                               0004446
BT151      EXIT.                                                         0004447
                                                                         0004448
       5000-CALC-ACTION-DATE  SECTION.                                   0004449
      ****************************************************************** 0004450
      *  THIS SECTION WILL CALCULATE THE ACTION DAYS FOR NUMBER SPECIFIE 0004451
      *  IN THE 'DAYS' FIELD ON THE NEXT PROCESS STEP WORKFLOW SCREEN.   0004452
      ****************************************************************** 0004453
            IF HPSI-PS-NEXT-DAYS (HPSI-SUB) NUMERIC                      0004454
                NEXT SENTENCE                                            0004455
            ELSE                                                         0004456
                GO TO 5099-CALC-ACTION-DATE-EXIT.                        0004457
                                                                         0004458
            IF HPSI-PS-NEXT-DAYS (HPSI-SUB) = +0                         0004459
                MOVE +0 TO WORK-DAYS-ADVANCE                             0004460
                MOVE WK-SYSTEM-DATE TO TASK-ACTION-DATE                  0004461
                GO TO 5020-SAVE-DAYS.                                    0004462
                                                                         0004463
            IF HPSI-PS-NEXT-DAYS (HPSI-SUB) = WORK-DAYS-SAVE             0004464
                MOVE XD-OUT-DATE9 TO TASK-ACTION-DATE                    0004465
                GO TO 5099-CALC-ACTION-DATE-EXIT.                        0004466
                                                                         0004467
            IF HPSI-PS-NEXT-DAYS (HPSI-SUB) > WRKDYS-MAX                 0004468
                GO TO 5010-CALC-DAYS.                                    0004469
                                                                         0004470
            MOVE SV-CO TO SY870-COMPANY.                                 0004471
            MOVE 'Y' TO SY870-CK-HOLIDAY.                                0004472
            MOVE 'N' TO SY870-CK-BANK-HOLIDAY.                           0004473
            MOVE 'Y' TO SY870-CK-SATURDAY-SUNDAY.                        0004474
            MOVE ' ' TO SY870-ROLL-BACK.                                 0004475
            MOVE HPSI-PS-NEXT-DAYS (HPSI-SUB)                            0004476
                TO SY870-WORKING-DAYS-TO-ROLL.                           0004477
            MOVE WK-SYSTEM-DATE TO SY870-PASS-DATE.                      0004478
            MOVE +0 TO SY870-RTN-CODE.                                   0004479
            CALL 'LSNSY870' USING SY870-PASS.                            0004480
            IF SY870-RTN-CODE > +0                                       0004481
                MOVE WK-SYSTEM-DATE TO TASK-ACTION-DATE                  0004482
                GO TO 5020-SAVE-DAYS.                                    0004483
                                                                         0004484
            MOVE SY870-RETURN-DATE TO TASK-ACTION-DATE.                  0004485
            GO TO 5020-SAVE-DAYS.                                        0004486
       5010-CALC-DAYS.                                                   0004487
            COMPUTE WORK-DAYS-ADVANCE =                                  0004488
              (HPSI-PS-NEXT-DAYS (HPSI-SUB) * +365) / +254.              0004489
            MOVE SPACES TO XDATE-PARMS.                                  0004490
            MOVE WK-SYSTEM-DATE TO XD-IN-DATE.                           0004491
            MOVE 'YYYYMMDD  ' TO XD-IN-FORMAT.                           0004492
            MOVE 'YYYYMMDD  ' TO XD-OUTPUT-FORMAT.                       0004493
            MOVE WORK-DAYS-ADVANCE TO XD-IN-ROLL-DAYS.                   0004494
            CALL 'XDATETP' USING XDATE-PARMS.                            0004495
            IF XD-OUT-DATE = 'BAD-DATE'                                  0004496
                MOVE WK-SYSTEM-DATE TO TASK-ACTION-DATE                  0004497
            ELSE                                                         0004498
                MOVE XD-OUT-DATE9 TO TASK-ACTION-DATE.                   0004499
                                                                         0004500
       5020-SAVE-DAYS.                                                   0004501
            MOVE HPSI-PS-NEXT-DAYS (HPSI-SUB) TO WORK-DAYS-SAVE.         0004502
       5099-CALC-ACTION-DATE-EXIT.                                       0004503
            EXIT.                                                        0004504
                                                                         0004505
BT154  5100-CALC-ACTION-DATE  SECTION.                                   0004506
BT154 *  THIS SECTION WILL CALCULATE THE ACTION DAYS FOR 5 DAYS          0004507
BT154                                                                    0004508
BT154       MOVE SV-CO TO SY870-COMPANY.                                 0004509
BT154       MOVE WS-Y TO SY870-CK-HOLIDAY.                               0004510
BT154       MOVE WS-N TO SY870-CK-BANK-HOLIDAY.                          0004511
BT154       MOVE WS-Y TO SY870-CK-SATURDAY-SUNDAY.                       0004512
BT154       MOVE SPACES    TO SY870-ROLL-BACK.                           0004513
BT154       MOVE 5 TO SY870-WORKING-DAYS-TO-ROLL.                        0004514
AS951       IF SAVE-LS-OVERRIDE-IND = '06'                              
AS951        AND 'POLICY' = SY-PROCESS (WS-SUB1)
AS951        AND 'QA ' = SY-STEP (WS-SUB1)
AS951         MOVE 2 TO SY870-WORKING-DAYS-TO-ROLL.
BT154       MOVE WK-SYSTEM-DATE TO SY870-PASS-DATE.                      0004515
BT154       MOVE +0 TO SY870-RTN-CODE.                                   0004516
BT154       CALL 'LSNSY870' USING SY870-PASS.                            0004517
BT154       IF SY870-RTN-CODE > +0                                       0004518
BT154           MOVE WK-SYSTEM-DATE TO TASK-ACTION-DATE                  0004519
BT154           GO TO 5199-CALC-ACTION-DATE-EXIT.                        0004520
BT154                                                                    0004521
BT154       MOVE SY870-RETURN-DATE TO TASK-ACTION-DATE.                  0004522
BT154  5199-CALC-ACTION-DATE-EXIT.                                       0004523
BT154       EXIT.                                                        0004524
BT154                                                                    0004525
BT251  5200-UPDATE-AB-STATUS  SECTION.                                  DELLRETC
BT251      MOVE +0 TO WS-SUB1.                                          DELLRETC
BT251      MOVE +1 TO WS-SUB2.                                          DELLRETC
BT251  5210-LOCATE-AB-LOOP.                                             DELLRETC
BT251      ADD +1 TO WS-SUB1.                                           DELLRETC
BT251      IF WS-SUB1 > WTR-TABLE-MAX                                   DELLRETC
BT251          GO TO 5299-UPDATE-AB-STATUS-EXIT.                        DELLRETC
BT251                                                                   DELLRETC
BT251      IF WTR-FILE-ID (WS-SUB1) = 'WA'                              DELLRETC
BT251        AND WTR-REC-ID (WS-SUB1) = 'SY'                            DELLRETC
BT251          GO TO 5299-UPDATE-AB-STATUS-EXIT.                        DELLRETC
BT251                                                                   DELLRETC
BT251      IF WTR-FILE-ID (WS-SUB1) = 'NB'                              DELLRETC
BT251        AND WTR-REC-ID (WS-SUB1) = 'AB'                            DELLRETC
BT251          GO TO 5220-READ-AB-REC.                                  DELLRETC
BT251      GO TO 5210-LOCATE-AB-LOOP.                                   DELLRETC
BT251  5220-READ-AB-REC.                                                DELLRETC
BT251      MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WA-KEY.                DELLRETC
BT251      PERFORM 8700-READ-KEY-WA.                                    DELLRETC
BT251      IF RTN-CODE = +0                                             DELLRETC
BT251          GO TO 5230-HOLD-AB-REC.                                  DELLRETC
BT251      IF RTN-CODE > +0                                             DELLRETC
BT251          DISPLAY                                                  DELLRETC
BT251      'ERROR READING NBAB RECORD - RTN-CODE = +' RTN-CODE          DELLRETC
BT251          DISPLAY 'LOCATION IS 5220-READ-AB-REC PARAGRAPH '        DELLRETC
BT251          DISPLAY 'WA KEY IS:  ' WTR-WA-KEY (WS-SUB1, WS-SUB2)     DELLRETC
BT251          DISPLAY 'CHECK WA FILE STATUS . . . '                    DELLRETC
BT251          MOVE +5220 TO ABEND-CODE                                 DELLRETC
BT251          PERFORM 9900-ABEND-RTN.                                  DELLRETC
BT251                                                                   DELLRETC
BT251  5230-HOLD-AB-REC.                                                DELLRETC
BT251      IF WA-STATUS = SPACES                                        DELLRETC
BT251          GO TO 5210-LOCATE-AB-LOOP.                               DELLRETC
BT251 *    CALL 'DISKHOLD' USING WA-APPLICATION-RECORD                  DELLRETC
BT251 *                   WA-KEY                                        DELLRETC
BT251 *                   WA-FILE-IDZ                                   DELLRETC
BT251 *                   RTN-CODE.                                     DELLRETC
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLRETC
           READ VSAM-WA                                                 DELLRETC
                    INTO WA-APPLICATION-RECORD                          DELLRETC
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLRETC
           INITIALIZE TPSWNML-AREA                                      DELLRETC
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLRETC
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLRETC
           MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE                     DELLRETC
           PERFORM P9999-MAP-RESP-CODE                                  DELLRETC
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLRETC
BT251      IF RTN-CODE > +0                                             DELLRETC
BT251          DISPLAY 'ERROR DISKHOLD WA FILE - RTN-CODE = +' RTN-CODE DELLRETC
BT251          DISPLAY 'LOCATION IS 5230-HOLD-AB-REC PARAGRAPH  '       DELLRETC
BT251          DISPLAY 'WA KEY IS:  ' WTR-WA-KEY (WS-SUB1, WS-SUB2)     DELLRETC
BT251          DISPLAY 'CHECK WA FILE STATUS . . . '                    DELLRETC
BT251          MOVE +5230 TO ABEND-CODE                                 DELLRETC
BT251          PERFORM 9900-ABEND-RTN.                                  DELLRETC
BT251                                                                   DELLRETC
BT251      MOVE SPACES TO WA-STATUS                                     DELLRETC
BT251             NBAB-DONOT-CREATE-SEG.                                DELLRETC
BT251  5240-DISKUP-AB-REC.                                              DELLRETC
BT251 *    CALL 'DISKUP' USING WA-APPLICATION-RECORD                    DELLRETC
BT251 *                 WA-KEY                                          DELLRETC
BT251 *                 WA-FILE-IDZ                                     DELLRETC
BT251 *                 RTN-CODE.                                       DELLRETC
           REWRITE VSAM-WA-REC FROM WA-APPLICATION-RECORD               DELLRETC
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLRETC
           INITIALIZE TPSWNML-AREA                                      DELLRETC
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLRETC
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLRETC
           MOVE 'DISKUP  ' TO TPSWNML-FUNCTION-CODE                     DELLRETC
           PERFORM P9999-MAP-RESP-CODE                                  DELLRETC
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLRETC
BT251      IF RTN-CODE > +0                                             DELLRETC
BT251 *        CALL 'TPFIRLFN' USING WA-FILE-IDZ                        DELLRETC
BT251 *                       REL-RTN-CODE                              DELLRETC
BT251          DISPLAY 'ERROR DISKUP WA FILE - RTN-CODE = +' RTN-CODE   DELLRETC
BT251          DISPLAY 'LOCATION IS 5240-DISKUP-AB-REC PARAGRAPH '      DELLRETC
BT251          DISPLAY 'WA KEY IS:  ' WTR-WA-KEY (WS-SUB1, WS-SUB2)     DELLRETC
BT251          DISPLAY 'CHECK WA FILE STATUS . . . '                    DELLRETC
BT251          MOVE +5240 TO ABEND-CODE                                 DELLRETC
BT251          PERFORM 9900-ABEND-RTN.                                  DELLRETC
BT251                                                                   DELLRETC
BT251      GO TO 5210-LOCATE-AB-LOOP.                                   DELLRETC
BT251                                                                   DELLRETC
BT251  5299-UPDATE-AB-STATUS-EXIT.                                      DELLRETC
BT251      EXIT.                                                        DELLRETC
       5500-DUMMY-SY-REC  SECTION.                                       0004526
           MOVE SPACES TO WA-APPLICATION-RECORD.                         0004527
           MOVE HWA-TRANS-TYPE TO WA-TRANS-TYPE.                         0004528
           MOVE HWA-CO TO WA-CO.                                         0004529
           MOVE HWA-TOTAL-RECS TO WA-TOTAL-RECS.                         0004530
           MOVE 000 TO WA-DUP-SEQ.                                       0004531
           MOVE 'WA' TO WA-FILE-ID.                                      0004532
           MOVE 'SY' TO WA-REC-ID.                                       0004533
           MOVE 01 TO WA-REC-SEQ.                                        0004534
           MOVE HWA-DELL-GUID TO WA-DELL-GUID.                           0004535
           MOVE HWA-CLIENT-GUID TO WA-CLIENT-GUID.                       0004536
           MOVE HWA-IMAGE-REFID TO WA-IMAGE-REFID.                       0004537
           MOVE WK-SYSTEM-DATE TO WA-MF-DATE.                            0004538
           MOVE WK-SYSTEM-TIME TO WA-MF-TIME.                            0004539
           MOVE 'RSE' TO WASY-FINAL-STATUS.                              0004540
           MOVE 'EAPPL' TO WASY-TASK-PROCESS.                            0004541
           MOVE 'NGO' TO WASY-TASK-STEP.                                 0004542
           MOVE 'P' TO WASY-TASK-INDICATOR.                              0004543
           MOVE WA-KEY TO SV-LAST-WA-KEY.                                0004544
           ADD +1 TO WS-SUB1.                                            0004545
           MOVE +1 TO WS-SUB2.                                           0004546
           MOVE WA-FILE-ID TO WTR-FILE-ID (WS-SUB1).                     0004547
           MOVE WA-REC-ID TO WTR-REC-ID (WS-SUB1).                       0004548
           MOVE WA-REC-APPID TO WTR-APP-ID (WS-SUB1).                    0004549
           MOVE WA-REC-SEQ TO WTR-REC-SEQ (WS-SUB1, WS-SUB2).            0004550
           MOVE WA-KEY TO WTR-WA-KEY (WS-SUB1, WS-SUB2).                 0004551
BT161      PERFORM 8700-READ-KEY-WA.                                     0004552
BT161      IF RTN-CODE = +0                                              0004553
BT161         GO TO 5599-DUMMY-SY-REC-EXIT.                              0004554
      *    CALL 'DISKADD' USING WA-APPLICATION-RECORD                    0004555
      *                  WA-KEY                                          0004556
      *                  WA-FILE-IDZ                                     0004557
      *                  RTN-CODE.                                       0004558
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           WRITE VSAM-WA-REC                                            DELLIDCH
                      FROM WA-APPLICATION-RECORD                        DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKADD' TO TPSWNML-FUNCTION-CODE                      DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
           IF RTN-CODE > +0                                              0004559
               DISPLAY 'RTN-CODE FROM DISKADD OF WA FILE > +0 '          0004560
               DISPLAY 'ERROR @ 5500-DUMMY-SY-REC SECTION '              0004561
               DISPLAY                                                   0004562
           'DELL-GUID OF PROCESSING WA RECORD:  ' WA-DELL-GUID           0004562
               DISPLAY 'CHECK WA FILE STATUS...'                         0004563
               MOVE +5500 TO ABEND-CODE                                  0004564
               PERFORM 9900-ABEND-RTN.                                   0004565
                                                                         0004566
       5599-DUMMY-SY-REC-EXIT.                                           0004567
           EXIT.                                                         0004568
BT371 *                                                                 DELLRET9
BT371  5600-VOLUNTARY-CANCEL  SECTION.                                  DELLRET9
BT371      MOVE WA-APPLICATION-RECORD TO SAVE-WA-RECORD.                DELLRET9
BT371      MOVE SPACES TO WA-APPLICATION-RECORD.                        DELLRET9
BT371      MOVE +1 TO WS-SUB1                                           DELLRET9
BT371              WS-SUB2.                                             DELLRET9
BT371      MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WA-KEY.                DELLRET9
BT371      MOVE 999 TO WA-TOTAL-RECS.                                   DELLRET9
BT371      MOVE 'AP' TO WA-FILE-ID.                                     DELLRET9
BT371      MOVE 'OR' TO WA-REC-ID.                                      DELLRET9
BT371      MOVE '  ' TO WA-REC-APPID-X.                                 DELLRET9
BT371      MOVE '01' TO WA-REC-SEQ.                                     DELLRET9
BT371      MOVE '20' TO APOR-REQ-TYPE-NO (WS-SUB1).                     DELLRET9
BT371      MOVE '0188' TO APOR-REQ-ID-NO (WS-SUB1).                     DELLRET9
BT373      MOVE 'O' TO APOR-REQ-STAT-NO (WS-SUB1).                      DELLRET9
BT371      MOVE '01' TO APOR-REQ-APPID-NO (WS-SUB1).                    DELLRET9
BT371      PERFORM 9500-WA-DISKADD.                                     DELLRET9
BT371      IF RTN-CODE > +0                                             DELLRET9
BT371          MOVE WS-Y TO ERROR-SWITCH                                DELLRET9
BT371          MOVE 'E88' TO LW-ERROR-CODE                              DELLRET9
BT371          PERFORM 3700-SY-ERR-MSG                                  DELLRET9
BT371          MOVE SAVE-WA-RECORD TO WA-APPLICATION-RECORD             DELLRET9
BT371          GO TO 5699-VOLUNTARY-CANCEL-EXIT.                        DELLRET9
BT371      MOVE +0 TO WS-SUB1.                                          DELLRET9
BT371  5610-ADD-TO-PASSED-LOOP.                                         DELLRET9
BT371      ADD +1 TO WS-SUB1.                                           DELLRET9
BT371      IF WS-SUB1 > +50                                             DELLRET9
BT371         MOVE SAVE-WA-RECORD TO WA-APPLICATION-RECORD              DELLRET9
BT371         GO TO 5699-VOLUNTARY-CANCEL-EXIT.                         DELLRET9
BT371      IF WA-TAB-RECORD (WS-SUB1) > SPACES                          DELLRET9
BT371         GO TO 5610-ADD-TO-PASSED-LOOP.                            DELLRET9
BT371      MOVE 'AP' TO WTR-FILE-ID (WS-SUB1).                          DELLRET9
BT371      MOVE 'OR' TO WTR-REC-ID (WS-SUB1).                           DELLRET9
BT371      MOVE +1 TO WS-SUB2.                                          DELLRET9
BT371      MOVE WA-REC-APPID-X TO WTR-APP-ID-X (WS-SUB1).               DELLRET9
BT371      MOVE WA-REC-SEQ TO WTR-REC-SEQ-X (WS-SUB1, WS-SUB2).         DELLRET9
BT371      MOVE WA-KEY TO WTR-WA-KEY (WS-SUB1, WS-SUB2).                DELLRET9
BT371      MOVE SAVE-WA-RECORD TO WA-APPLICATION-RECORD.                DELLRET9
BT371  5699-VOLUNTARY-CANCEL-EXIT.                                      DELLRET9
BT371      EXIT.                                                        DELLRET9
BT371 *                                                                 DELLRET9

DL90A  5700-LOCATE-AP  SECTION.
DL90A      MOVE +0 TO WS-SUB1.                                         
DL90A      MOVE +0 TO WS-SUB2.                                         
DL90A  5710-LOCATE-AP-LOOP.                                             
DL90A      ADD +1 TO WS-SUB1.
DL90A      IF WS-SUB1 > WTR-TABLE-MAX                                  
DL90A          MOVE WS-Y TO ERROR-SWITCH                               
DL90A          GO TO 5799-LOCATE-AP-EXIT.                              
DL90A                                                                  
DL90A      IF WTR-FILE-ID (WS-SUB1) = WS-AP                            
DL90A        AND WTR-REC-ID (WS-SUB1) = WS-AP                          
DL90A          GO TO 5720-READ-AP-REC.                                 
DL90A                                                                   
DL90A      GO TO 5710-LOCATE-AP-LOOP.                                  
DL90A  5720-READ-AP-REC.                                                
DL90A      ADD +1 TO WS-SUB2.
DL90A      MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WA-KEY.               
DL90A      PERFORM 8700-READ-KEY-WA.                                  
DL90A      IF RTN-CODE = +0                                            
DL90A          IF WA-REC-SEQ = 01
DL90A              MOVE APAP1-UW-DCSN-NA TO SAVE-UW-DCSN-NA
DL90A          END-IF
DL90A          IF WA-REC-SEQ = 02
DL90A              MOVE APAP2-UW-SUB-DCSN-NA TO SAVE-UW-SUB-DCSN-NA
DL90A          END-IF
DL90A          GO TO 5720-READ-AP-REC
DL90A      END-IF.
DL90A      IF RTN-CODE > +1                                         
DL90A          DISPLAY                                                  
DL90A           ' WA FILE I/O ERROR-PROGRAM RTN-CODE > +0 ' RTN-CODE    
DL90A          DISPLAY ' ERROR IN 5700-LOCATE-AP PARAGRAPH '           
DL90A          DISPLAY ' ERROR ON READ WA - WA-KEY ' WA-KEY            
DL90A          DISPLAY                                                  
DL90A           'PROGRAM LSNNB402 IS ABENDING . . . CHECK WA STATUS '  
DL90A          MOVE +5700 TO ABEND-CODE                                 
DL90A          PERFORM 9900-ABEND-RTN                                   
DL90A      END-IF.    
DL90A  5799-LOCATE-AP-EXIT.
DL90A      EXIT.
                                                                         0004569
       6000-CHECK-DATE  SECTION.                                         0004570
           IF WK-DATE = SPACES                                           0004571
               GO TO 6099-CHECK-DATE-EXIT.                               0004572
           MOVE SPACES TO XDATE-PARMS.                                   0004573
           MOVE WK-DATE TO XD-IN-DATE.                                   0004574
           MOVE 'YYYYMMDD' TO XD-IN-FORMAT                               0004575
                           XD-OUTPUT-FORMAT.                             0004576
           CALL 'XDATETP' USING XDATE-PARMS.                             0004577
           IF XD-OUT-DATE = 'BAD-DATE'                                   0004578
               MOVE SPACES TO XD-OUT-DATE.                               0004579
           MOVE XD-OUT-DATE TO WK-DATE.                                  0004580
       6099-CHECK-DATE-EXIT.                                             0004581
           EXIT.                                                         0004582
                                                                         0004583
       7000-STATUS-ERROR-WA  SECTION.                                    0004584
BT132      MOVE SPACES TO WA-APPLICATION-RECORD                          0004585
BT132             WS-KEY.                                                0004586
           MOVE +0 TO RTN-CODE                                           0004587
                   WS-SUB1                                               0004588
                   WS-SUB2.                                              0004589
BT132      MOVE HWA-TRANS-TYPE TO WS-TRANS-TYPE.                         0004590
BT142      MOVE SV-CO TO WS-CO.                                          0004591
BT132      MOVE HWA-DELL-GUID TO WS-DELL-GUID.                           0004592
BT132      MOVE HWA-TOTAL-RECS TO WS-TOTAL-RECS                          0004593
                               HOLD-TOTAL-RECS.                          0004594
BT132      MOVE 000 TO WS-DUP-SEQ.                                       0004595
BT132      MOVE LOW-VALUES TO WS-REC-TYPE.                               0004596
       7010-READ-VALID-WA.                                               0004597
           PERFORM 8300-READ-NEXT-WA-RECS.                               0004598
           IF RTN-CODE = +12                                             0004599
               DISPLAY ' WA FILE I/O ERROR-PROGRAM RTN-CODE = +12'       0004600
               DISPLAY ' ERROR IN 7010-READ-VALID-WA PARAGRAPH '         0004601
               DISPLAY ' ERROR ON WA READ - WA-DELL-GUID ' HWA-DELL-GUID 0004602
               DISPLAY                                                   0004603
           'PROGRAM LSNNB402 IS ABENDING . . . CHECK WA STATUS '         0004603
               MOVE +7010 TO ABEND-CODE                                  0004604
               PERFORM 9900-ABEND-RTN.                                   0004605
                                                                         0004606
           IF RTN-CODE = +1                                              0004607
               GO TO 7050-UPDATE-GU-FILE.                                0004608
                                                                         0004609
DL457      ADD +1 TO WS-REC-READ.                                       DELLRT17
BT142      IF WK-STATUS = 'RSE'                                          0004610
BT142       AND HWA-DELL-GUID = WA-DELL-GUID                             0004611
BT142       AND HWA-TRANS-TYPE = WA-TRANS-TYPE                           0004612
BT142       AND HWA-CO = WA-CO                                           0004613
BT142       AND (HWA-TOTAL-RECS NOT = WS-REC-COUNT)                      0004614
BT142          GO TO 7011-CONTINUE.                                      0004615
BT142                                                                    0004616
           IF HWA-DELL-GUID = WA-DELL-GUID                               0004617
            AND HWA-TRANS-TYPE = WA-TRANS-TYPE                           0004618
            AND HWA-CO = WA-CO                                           0004619
            AND HWA-TOTAL-RECS = WS-REC-COUNT                            0004620
               NEXT SENTENCE                                             0004621
           ELSE                                                          0004622
               GO TO 7050-UPDATE-GU-FILE.                                0004623
                                                                         0004624
BT142  7011-CONTINUE.                                                    0004625
BT142                                                                    0004626
           IF HWA-DELL-GUID = WA-DELL-GUID                               0004627
            AND HWA-TRANS-TYPE = WA-TRANS-TYPE                           0004628
            AND HWA-CO = WA-CO                                           0004629
            AND WA-DUP-SEQ = 000                                         0004630
            AND WA-STATUS = SPACES                                       0004631
               GO TO 7015-SET-ERROR.                                     0004632
                                                                         0004633
           GO TO 7010-READ-VALID-WA.                                     0004634
       7015-SET-ERROR.                                                   0004635
      *    CALL 'DISKHOLD' USING WA-APPLICATION-RECORD                   0004636
      *                   WA-KEY                                         0004637
      *                   WA-FILE-IDZ                                    0004638
      *                   RTN-CODE.                                      0004639
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           READ VSAM-WA                                                 DELLIDCH
                    INTO WA-APPLICATION-RECORD                          DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
                                                                         0004640
           IF RTN-CODE > +0                                              0004641
               DISPLAY                                                   0004642
           ' WA FILE I/O ERROR-PROGRAM RTN-CODE > +0 ' RTN-CODE          0004642
               DISPLAY ' ERROR IN 7015-SET-ERROR PARAGRAPH '             0004643
               DISPLAY                                                   0004644
           ' ERROR ON DISKHOLD - WA-DELL-GUID ' HWA-DELL-GUID            0004644
               DISPLAY                                                   0004645
           'PROGRAM LSNNB402 IS ABENDING . . . CHECK WA STATUS '         0004645
               MOVE +7015 TO ABEND-CODE                                  0004646
               PERFORM 9900-ABEND-RTN.                                   0004647
                                                                         0004648
           MOVE WK-SYSTEM-DATE TO WA-MF-DATE.                            0004649
           MOVE WK-SYSTEM-TIME TO WA-MF-TIME.                            0004650
           MOVE WK-STATUS TO WA-STATUS.                                  0004651
DL831      IF WK-STATUS = 'CPO' AND WA-REC-ID = 'SY'
DL831          MOVE NB452-POLICY-NUMBER TO WASY-POLNUM.
BT151      IF WK-STATUS = 'DEL' AND WA-REC-ID = 'SY'                     0004652
BT151          MOVE WS-P TO WASY-TASK-INDICATOR.                         0004653
       7020-WA-DISKUP.                                                   0004654
      *    CALL 'DISKUP' USING WA-APPLICATION-RECORD                     0004655
      *                 WA-KEY                                           0004656
      *                 WA-FILE-IDZ                                      0004657
      *                 RTN-CODE.                                        0004658
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           REWRITE VSAM-WA-REC                                          DELLIDCH
                      FROM WA-APPLICATION-RECORD                        DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE                       DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
                                                                         0004659
           IF (RTN-CODE NOT = +0)                                        0004660
      *        CALL 'TPFIRLFN' USING WA-FILE-IDZ                        DELLMNCH
      *                       REL-RTN-CODE                              DELLMNCH
               DISPLAY                                                   0004663
           ' WA FILE I/O ERROR-PROGRAM RTN-CODE > +0 ' RTN-CODE          0004663
               DISPLAY ' ERROR IN 7020-WA-DISKUP PARAGRAPH '             0004664
               DISPLAY ' ERROR ON DISKUP - WA-DELL-GUID ' HWA-DELL-GUID  0004665
               DISPLAY                                                   0004666
           'PROGRAM LSNNB402 IS ABENDING . . . CHECK WA STATUS '         0004666
               MOVE +7020 TO ABEND-CODE                                  0004667
               PERFORM 9900-ABEND-RTN.                                   0004668
                                                                         0004669
           GO TO 7010-READ-VALID-WA.                                     0004670
       7050-UPDATE-GU-FILE.                                              0004671
AS552      IF WK-STATUS = 'CPO' OR 'DEL' OR 'GAP'                        0004672
BT142          GO TO 7099-STATUS-ERROR-WA-EXIT.                          0004673
           PERFORM 9000-READ-GU.                                         0004674
           IF RTN-CODE = +0                                              0004675
               GO TO 7060-SET-UNKNOWN.                                   0004676
                                                                         0004677
           IF RTN-CODE > +0                                              0004678
               DISPLAY                                                   0004679
           ' GU FILE I/O ERROR-PROGRAM RTN-CODE > +0 ' RTN-CODE          0004679
               DISPLAY ' ERROR IN 7050-UPDATE-GU-FILE PARAGRAPH '        0004680
               DISPLAY ' ERROR ON GU FILE READ ' HWA-CLIENT-GUID         0004681
               DISPLAY                                                   0004682
           'PROGRAM LSNNB402 IS ABENDING . . . CHECK GU STATUS '         0004682
               MOVE +7050 TO ABEND-CODE                                  0004683
               PERFORM 9900-ABEND-RTN.                                   0004684
                                                                         0004685
       7060-SET-UNKNOWN.                                                 0004686
MK131      MOVE GU-GUID-RECORD TO HOLD-GU-RECORD.                        0004687
      *    CALL 'DISKHOLD' USING GU-GUID-RECORD                          0004688
      *                   GU-KEY                                         0004689
      *                   GU-FILE-ID                                     0004690
      *                   RTN-CODE.                                      0004691
           MOVE GU-KEY TO VSAM-GU-PRIM                                  DELLIDCH
           READ VSAM-GU                                                 DELLIDCH
                    INTO GU-GUID-RECORD                                 DELLIDCH
           MOVE VSAM-GU-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE GU-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE GU-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
                                                                         0004692
           IF RTN-CODE > +0                                              0004693
               DISPLAY                                                   0004694
           ' GU FILE I/O ERROR-PROGRAM RTN-CODE > +0 ' RTN-CODE          0004694
               DISPLAY ' ERROR IN 7060-SET-UNKNOWN PARAGRAPH '           0004695
               DISPLAY ' ERROR ON DISKHOLD OF GU FILE ' HWA-CLIENT-GUID  0004696
               DISPLAY                                                   0004697
           'PROGRAM LSNNB402 IS ABENDING . . . CHECK GU STATUS '         0004697
               MOVE +7060 TO ABEND-CODE                                  0004698
               PERFORM 9900-ABEND-RTN.                                   0004699
                                                                         0004700
           MOVE WK-SYSTEM-DATE TO GU-DATE.                               0004701
           MOVE WK-SYSTEM-TIME TO GU-TIME.                               0004702
BT282      IF GU-POLICY = SPACES                                        05088000
           MOVE 'UNKNOWN' TO GU-POLICY.                                  0004703
KM101      IF DUP-POL-SWITCH = 'Y'                                       0004704
KM101         MOVE 'DUPLICAT' TO GU-POLICY.                              0004705
                                                                         0004706
BT153      IF HWA-POLCRECTYPE = 'P'                                      0004707
MK131          MOVE GU-PREAPP-REFID TO GU-IMAGE-REFID.                   0004708
BT153      IF HWA-POLCRECTYPE = 'U'                                      0004709
BT153          MOVE HWA-IMAGE-REFID TO GU-IMAGE-REFID                    0004710
BT153          MOVE HGU-POLICY TO GU-POLICY                              0004711
BT153          MOVE HGU-DATE TO GU-DATE                                  0004712
BT153          MOVE HGU-TIME TO GU-TIME                                  0004713
BT153          MOVE WK-SYSTEM-DATE TO GU-FINAL-DATE                      0004714
BT153          MOVE WK-SYSTEM-TIME TO GU-FINAL-TIME.                     0004715
BT153      IF (HWA-POLCRECTYPE = 'U')                                    0004716
MK131        AND (WK-STATUS = 'PIP' OR 'PNU' OR 'PCE')                   0004717
MK131          MOVE HGU-POLICY TO GU-POLICY.                             0004718
       7070-GU-DISKUP.                                                   0004719
      *    CALL 'DISKUP' USING GU-GUID-RECORD                            0004720
      *                 GU-KEY                                           0004721
      *                 GU-FILE-ID                                       0004722
      *                 RTN-CODE.                                        0004723
           MOVE GU-KEY TO VSAM-GU-PRIM                                  DELLIDCH
           REWRITE VSAM-GU-REC                                          DELLIDCH
                      FROM GU-GUID-RECORD                               DELLIDCH
           MOVE VSAM-GU-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE GU-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE GU-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE                       DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
                                                                         0004724
           IF (RTN-CODE NOT = +0)                                        0004725
      *        CALL 'TPFIRLFN' USING GU-FILE-ID                         DELLMNCH
      *                       REL-RTN-CODE                              DELLMNCH
               DISPLAY                                                   0004728
           ' GU FILE I/O ERROR-PROGRAM RTN-CODE > +0 ' RTN-CODE          0004728
               DISPLAY ' ERROR IN 7070-GU-DISKUP PARAGRAPH '             0004729
               DISPLAY ' ERROR ON DISKUP OF GU FILE ' HWA-CLIENT-GUID    0004730
               DISPLAY                                                   0004731
           'PROGRAM LSNNB402 IS ABENDING . . . CHECK GU STATUS '         0004731
               MOVE +7070 TO ABEND-CODE                                  0004732
               PERFORM 9900-ABEND-RTN.                                   0004733
                                                                         0004734
       7099-STATUS-ERROR-WA-EXIT.                                        0004735
           EXIT.                                                         0004736
                                                                         0004737
DL681
DL681  7100-WRITE-LTR    SECTION.
DL681
DL681      IF SAVE-LS-PARAM-1 = SPACES
DL681          GO TO 7199-WRITE-LTR-EXIT.

DL681      MOVE SPACES TO LW-REQUEST-REC.
DL681      IF SAVE-LS-PARAM-1 = 'LTR279' 
DL681          MOVE '279'  TO LWR-LTRX  
DL681          GO TO 7100-CONT. 
DL681      IF SAVE-LS-PARAM-1 = 'LTR280'
DL681          MOVE '280'  TO LWR-LTRX
DL681          GO TO 7100-CONT.
DL681      IF SAVE-LS-PARAM-1 = 'LTR281'
DL681          MOVE '281'  TO LWR-LTRX
DL681          GO TO 7100-CONT.
DL681      IF SAVE-LS-PARAM-1 = 'LTR282'
DL681          MOVE '282'  TO LWR-LTRX
DL681          GO TO 7100-CONT.
DL681      IF SAVE-LS-PARAM-1 = 'LTR283'  
DL681          MOVE '283'  TO LWR-LTRX   
DL681          GO TO 7100-CONT. 
DL681      IF SAVE-LS-PARAM-1 = 'LTR284'  
DL681          MOVE '284'  TO LWR-LTRX   
DL681          GO TO 7100-CONT. 
DL681      IF SAVE-LS-PARAM-1 = 'LTR285'  
DL681          MOVE '285'  TO LWR-LTRX   
DL681          GO TO 7100-CONT. 
DL681      IF SAVE-LS-PARAM-1 = 'LTR286'  
DL681          MOVE '286'  TO LWR-LTRX   
DL681          GO TO 7100-CONT. 
DL681      IF SAVE-LS-PARAM-1 = 'LTR287'  
DL681          MOVE '287'  TO LWR-LTRX   
DL681          GO TO 7100-CONT. 
DL681      IF SAVE-LS-PARAM-1 = 'LTR288'  
DL681          MOVE '288'  TO LWR-LTRX   
DL681          GO TO 7100-CONT. 
DL681      IF SAVE-LS-PARAM-1 = 'LTR289'  
DL681          MOVE '289'  TO LWR-LTRX   
DL681          GO TO 7100-CONT. 
DL681      IF SAVE-LS-PARAM-1 = 'LTR290'  
DL681          MOVE '290'  TO LWR-LTRX   
DL681          GO TO 7100-CONT. 
DL681      IF SAVE-LS-PARAM-1 = 'LTR291'  
DL681          MOVE '291'  TO LWR-LTRX   
DL681          GO TO 7100-CONT. 
DL681      IF SAVE-LS-PARAM-1 = 'LTR292'  
DL681          MOVE '292'  TO LWR-LTRX   
DL681          GO TO 7100-CONT. 
DL681      IF SAVE-LS-PARAM-1 = 'LTR293'  
DL681          MOVE '293'  TO LWR-LTRX   
DL681          GO TO 7100-CONT. 
DL681      IF SAVE-LS-PARAM-1 = 'LTR294'  
DL681          MOVE '294'  TO LWR-LTRX   
DL681          GO TO 7100-CONT. 
DL681      MOVE 'INV'  TO LWR-LTRX.   
DL681  7100-CONT. 

DL681          MOVE SV-CO  TO LWR-CO.
DL681          MOVE SAVE-PAPOL-WASY TO LWR-KEY.
DL681          MOVE ZEROS  TO LWR-TIME.
DL681          MOVE 'STP'  TO LWR-OPID.
DL681          MOVE 01     TO LWR-COPY.
DL681          ADD +1      TO WS-REC-WRITE9.
DL681          WRITE WRITE9-REC FROM LW-REQUEST-REC.                   
DL681  7199-WRITE-LTR-EXIT.
DL681      EXIT.

       7200-APPLICATION-ERROR  SECTION.                                  0004738
BT132      MOVE SPACES TO WA-APPLICATION-RECORD                          0004739
BT132             WS-KEY.                                                0004740
           MOVE +0 TO RTN-CODE                                           0004741
                   WS-SUB1                                               0004742
                   WS-SUB2.                                              0004743
BT132      MOVE HWA-TRANS-TYPE TO WS-TRANS-TYPE.                         0004744
BT132      MOVE SV-CO TO WS-CO.                                          0004745
BT132      MOVE HWA-DELL-GUID TO WS-DELL-GUID.                           0004746
BT132      MOVE HWA-TOTAL-RECS TO WS-TOTAL-RECS                          0004747
                               HOLD-TOTAL-RECS.                          0004748
BT132      MOVE 000 TO WS-DUP-SEQ.                                       0004749
BT132      MOVE LOW-VALUES TO WS-REC-TYPE.                               0004750
       7210-READ-VALID-WA-RECS.                                          0004751
           PERFORM 8300-READ-NEXT-WA-RECS.                               0004752
           IF RTN-CODE = +1                                              0004753
               GO TO 7299-APPLICATION-ERROR-EXIT.                        0004754
                                                                         0004755
DL457      ADD +1 TO WS-REC-READ.                                       DELLRT17
           IF HWA-DELL-GUID = WA-DELL-GUID                               0004756
            AND HWA-TRANS-TYPE = WA-TRANS-TYPE                           0004757
            AND SV-CO = WA-CO                                            0004758
            AND HWA-TOTAL-RECS = WS-REC-COUNT                            0004759
               NEXT SENTENCE                                             0004760
           ELSE                                                          0004761
               GO TO 7299-APPLICATION-ERROR-EXIT.                        0004762
                                                                         0004763
           IF HWA-DELL-GUID = WA-DELL-GUID                               0004764
            AND HWA-TRANS-TYPE = WA-TRANS-TYPE                           0004765
            AND SV-CO = WA-CO                                            0004766
            AND WA-DUP-SEQ = 000                                         0004767
               GO TO 7215-SET-STATUS-ERROR.                              0004768
                                                                         0004769
           GO TO 7210-READ-VALID-WA-RECS.                                0004770
       7215-SET-STATUS-ERROR.                                            0004771
      *    CALL 'DISKHOLD' USING WA-APPLICATION-RECORD                   0004772
      *                   WA-KEY                                         0004773
      *                   WA-FILE-IDZ                                    0004774
      *                   RTN-CODE.                                      0004775
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           READ VSAM-WA                                                 DELLIDCH
                    INTO WA-APPLICATION-RECORD                          DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
                                                                         0004776
           IF RTN-CODE > +0                                              0004777
               DISPLAY                                                   0004778
           ' WA FILE I/O ERROR-PROGRAM RTN-CODE > +0 ' RTN-CODE          0004778
               DISPLAY ' ERROR IN 7215-SET-STATUS-ERROR PARAGRAPH  '     0004779
               DISPLAY ' ERROR ON DISKHOLD '  HWA-DELL-GUID              0004780
               DISPLAY                                                   0004781
           'PROGRAM LSNNB402 IS ABENDING . . . CHECK WA STATUS '         0004781
               MOVE +7215 TO ABEND-CODE                                  0004782
               PERFORM 9900-ABEND-RTN.                                   0004783
                                                                         0004784
           MOVE WK-SYSTEM-DATE TO WA-MF-DATE.                            0004785
           MOVE WK-SYSTEM-TIME TO WA-MF-TIME.                            0004786
           MOVE WK-STATUS TO WA-STATUS.                                  0004787
       7220-WA-DISKUP-STATUS.                                            0004788
      *    CALL 'DISKUP' USING WA-APPLICATION-RECORD                     0004789
      *                 WA-KEY                                           0004790
      *                 WA-FILE-IDZ                                      0004791
      *                 RTN-CODE.                                        0004792
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           REWRITE VSAM-WA-REC                                          DELLIDCH
                      FROM WA-APPLICATION-RECORD                        DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE                       DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
                                                                         0004793
           IF RTN-CODE > +0                                              0004794
      *        CALL 'TPFIRLFN' USING NB-FILE-ID                         DELLMNCH
      *                       REL-RTN-CODE                              DELLMNCH
               DISPLAY                                                   0004797
           ' WA FILE I/O ERROR-PROGRAM RTN-CODE > +0 ' RTN-CODE          0004797
               DISPLAY ' ERROR IN 7220-WA-DISKUP-STATUS PARAGRAPH '      0004798
               DISPLAY ' ERROR ON DISKUP ' HWA-DELL-GUID                 0004799
               DISPLAY                                                   0004800
           'PROGRAM LSNNB402 IS ABENDING . . . CHECK WA STATUS '         0004800
               MOVE +7220 TO ABEND-CODE                                  0004801
               PERFORM 9900-ABEND-RTN.                                   0004802
                                                                         0004803
           GO TO 7210-READ-VALID-WA-RECS.                                0004804
       7299-APPLICATION-ERROR-EXIT.                                      0004805
           EXIT.                                                         0004806
                                                                         0004807
       7300-INVALID-RECORD  SECTION.                                     0004808
           MOVE +1 TO WS-SUB2.                                           0004809
           MOVE SPACES TO WA-APPLICATION-RECORD.                         0004810
           MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WA-KEY.                 0004811
           PERFORM 8700-READ-KEY-WA.                                     0004812
           IF RTN-CODE > +0                                              0004813
               DISPLAY                                                   0004814
           ' WA FILE I/O ERROR-PROGRAM RTN-CODE > +0 ' RTN-CODE          0004814
               DISPLAY ' ERROR IN 7300-INVALID-RECORD SECTION '          0004815
               DISPLAY                                                   0004816
           ' ERROR ON WA READ ' WTR-WA-KEY (WS-SUB1, WS-SUB2)            0004816
               DISPLAY                                                   0004817
           'PROGRAM LSNNB402 IS ABENDING . . . CHECK WA STATUS '         0004817
               MOVE +7300 TO ABEND-CODE                                  0004818
               PERFORM 9900-ABEND-RTN.                                   0004819
                                                                         0004820
      *    CALL 'DISKHOLD' USING WA-APPLICATION-RECORD                   0004821
      *                   WA-KEY                                         0004822
      *                   WA-FILE-IDZ                                    0004823
      *                   RTN-CODE.                                      0004824
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           READ VSAM-WA                                                 DELLIDCH
                    INTO WA-APPLICATION-RECORD                          DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
                                                                         0004825
           IF RTN-CODE > +0                                              0004826
               DISPLAY                                                   0004827
           ' WA FILE I/O ERROR-PROGRAM RTN-CODE > +0 ' RTN-CODE          0004827
               DISPLAY ' ERROR IN 7300-INVALID-RECORD SECTION '          0004828
               DISPLAY                                                   0004829
           ' ERROR ON DISKHOLD ' WTR-WA-KEY (WS-SUB1, WS-SUB2)           0004829
               DISPLAY                                                   0004830
           'PROGRAM LSNNB402 IS ABENDING . . . CHECK WA STATUS '         0004830
               MOVE +7300 TO ABEND-CODE                                  0004831
               PERFORM 9900-ABEND-RTN.                                   0004832
                                                                         0004833
           MOVE WK-SYSTEM-DATE TO WA-MF-DATE.                            0004834
           MOVE WK-SYSTEM-TIME TO WA-MF-TIME.                            0004835
           MOVE WK-STATUS TO WA-STATUS.                                  0004836
       7320-WA-DISKUP-INV.                                               0004837
      *    CALL 'DISKUP' USING WA-APPLICATION-RECORD                     0004838
      *                 WA-KEY                                           0004839
      *                 WA-FILE-IDZ                                      0004840
      *                 RTN-CODE.                                        0004841
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           REWRITE VSAM-WA-REC                                          DELLIDCH
                      FROM WA-APPLICATION-RECORD                        DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE                       DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
                                                                         0004842
           IF RTN-CODE > +0                                              0004843
      *        CALL 'TPFIRLFN' USING NB-FILE-ID                         DELLMNCH
      *                       REL-RTN-CODE                              DELLMNCH
               DISPLAY                                                   0004846
           ' WA FILE I/O ERROR-PROGRAM RTN-CODE > +0 ' RTN-CODE          0004846
               DISPLAY ' ERROR IN 7320-WA-DISKUP-INV PARAGRAPH '         0004847
               DISPLAY ' ERROR ON DISKUP ' WTR-WA-KEY (WS-SUB1, WS-SUB2) 0004848
               DISPLAY                                                   0004849
           'PROGRAM LSNNB402 IS ABENDING . . . CHECK WA STATUS '         0004849
               MOVE +7320 TO ABEND-CODE                                  0004850
               PERFORM 9900-ABEND-RTN.                                   0004851
                                                                         0004852
       7399-INVALID-RECORD-EXIT.                                         0004853
           EXIT.                                                         0004854
DL461
DL461  7400-WRITE-LTR089    SECTION.
DL461 
DL461      IF SAVE-LS-PARAM-1 = 'LTR089'
DL461          MOVE SPACES TO LW-REQUEST-REC
DL461          MOVE LM-CO  TO LWR-CO
DL461          MOVE LM-POL TO LWR-KEY
DL461          MOVE ZEROS  TO LWR-TIME
DL461          MOVE 'STP'  TO LWR-OPID
DL461          MOVE '089'  TO LWR-LTRX                            
DL461          MOVE 01     TO LWR-COPY
DL461          ADD +1      TO WS-REC-WRITE9 
DL461          WRITE WRITE9-REC FROM LW-REQUEST-REC.                                    
DL461  7499-WRITE-LRT089-EXIT.
DL461      EXIT.
                                                                         0004855
       7500-SET-ALL-STATUS  SECTION.                                     0004856
BT132      MOVE SPACES TO WA-APPLICATION-RECORD                          0004857
BT132             WS-KEY.                                                0004858
           MOVE +0 TO RTN-CODE                                           0004859
                   WS-SUB1                                               0004860
                   WS-SUB2.                                              0004861
BT132      MOVE HWA-TRANS-TYPE TO WS-TRANS-TYPE.                         0004862
BT132      MOVE SV-CO TO WS-CO.                                          0004863
BT132      MOVE HWA-DELL-GUID TO WS-DELL-GUID.                           0004864
BT132      MOVE HWA-TOTAL-RECS TO WS-TOTAL-RECS                          0004865
                               HOLD-TOTAL-RECS.                          0004866
BT132      MOVE 000 TO WS-DUP-SEQ.                                       0004867
BT132      MOVE LOW-VALUES TO WS-REC-TYPE.                               0004868
       7510-READ-VALID-WA-RECS.                                          0004869
           PERFORM 8300-READ-NEXT-WA-RECS.                               0004870
           IF RTN-CODE = +1                                              0004871
               GO TO 7599-SET-ALL-STATUS-EXIT.                           0004872
                                                                         0004873
           IF RTN-CODE = +12                                             0004874
               DISPLAY ' WA FILE I/O ERROR-PROGRAM RTN-CODE = +12'       0004875
               DISPLAY ' ERROR IN 7510-READ-VALID-WA-RECS PARAGRAPH '    0004876
               DISPLAY ' ERROR ON DISKUP - WA-DELL-GUID ' HWA-DELL-GUID  0004877
               DISPLAY                                                   0004878
           'PROGRAM LSNNB402 IS ABENDING . . . CHECK WA STATUS '         0004878
               MOVE +7510 TO ABEND-CODE                                  0004879
               PERFORM 9900-ABEND-RTN.                                   0004880
                                                                         0004881
DL457      ADD +1 TO WS-REC-READ.                                       DELLRT17
           IF (HWA-DELL-GUID NOT = WA-DELL-GUID)                         0004882
               GO TO 7599-SET-ALL-STATUS-EXIT.                           0004883
                                                                         0004884
           IF WA-STATUS = 'INV' OR 'NTE'                                 0004885
               GO TO 7510-READ-VALID-WA-RECS.                            0004886
                                                                         0004887
           IF HWA-DELL-GUID = WA-DELL-GUID                               0004888
               GO TO 7515-SET-STATUS.                                    0004889
                                                                         0004890
           GO TO 7510-READ-VALID-WA-RECS.                                0004891
       7515-SET-STATUS.                                                  0004892
      *    CALL 'DISKHOLD' USING WA-APPLICATION-RECORD                   0004893
      *                   WA-KEY                                         0004894
      *                   WA-FILE-IDZ                                    0004895
      *                   RTN-CODE.                                      0004896
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           READ VSAM-WA                                                 DELLIDCH
                    INTO WA-APPLICATION-RECORD                          DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
                                                                         0004897
           IF RTN-CODE > +0                                              0004898
               DISPLAY ' WA FILE I/O ERROR-PROGRAM RTN-CODE > +0 '       0004899
               DISPLAY ' ERROR IN 7515-SET-STATUS SECTION '              0004900
               DISPLAY                                                   0004901
           ' ERROR ON DISKHOLD - WA-DELL-GUID ' HWA-DELL-GUID            0004901
               DISPLAY                                                   0004902
           'PROGRAM LSNNB402 IS ABENDING . . . CHECK WA STATUS '         0004902
               MOVE +7515 TO ABEND-CODE                                  0004903
               PERFORM 9900-ABEND-RTN.                                   0004904
                                                                         0004905
           MOVE WK-SYSTEM-DATE TO WA-MF-DATE.                            0004906
           MOVE WK-SYSTEM-TIME TO WA-MF-TIME.                            0004907
BT043      IF WA-FILE-ID = 'WA' AND WA-REC-ID = 'SY'                     0004908
DL831          MOVE GU-POLICY TO WASY-POLNUM
BT043          MOVE WK-STATUS TO WA-STATUS                               0004909
BT043          GO TO 7520-WA-DISKUP-STATUS.                              0004910
BT043      IF WA-FILE-ID = 'AP' AND WA-REC-ID = 'AP'                     0004911
BT043        AND NUM-SY = 003 AND NUM-AP = 000                           0004912
BT043          MOVE NUM-SY TO WA-STATUS                                  0004913
BT043          GO TO 7520-WA-DISKUP-STATUS.                              0004914
BT043                                                                    0004915
BT043      IF WA-FILE-ID = 'AP' AND WA-REC-ID = 'AP'                     0004916
BT043          MOVE NUM-AP TO WA-STATUS                                  0004917
BT043          GO TO 7520-WA-DISKUP-STATUS.                              0004918
BT043                                                                    0004919
BT043      IF WA-FILE-ID = 'AP' AND WA-REC-ID = 'CV'                     0004920
BT043          MOVE NUM-CV TO WA-STATUS                                  0004921
BT043          GO TO 7520-WA-DISKUP-STATUS.                              0004922
BT043                                                                    0004923
BT043      IF WA-FILE-ID = 'NB' AND WA-REC-ID = 'T1'                     0004924
BT043          MOVE NUM-T1 TO WA-STATUS                                  0004925
BT043          GO TO 7520-WA-DISKUP-STATUS.                              0004926
BT043                                                                    0004927
BT043      IF WA-FILE-ID = 'NB' AND WA-REC-ID = 'BN'                     0004928
BT043          MOVE NUM-BN TO WA-STATUS                                  0004929
BT043          GO TO 7520-WA-DISKUP-STATUS.                              0004930
BT043                                                                    0004931
CL131      IF WA-FILE-ID = 'NB' AND WA-REC-ID = 'AB'                     0004932
CL131          MOVE NUM-AB TO WA-STATUS                                  0004933
CL131          GO TO 7520-WA-DISKUP-STATUS.                              0004934
CL131                                                                    0004935
BT162      IF WA-FILE-ID = 'NB' AND WA-REC-ID = 'AC'                     0004936
BT162          MOVE NUM-AC TO WA-STATUS                                  0004937
BT162          GO TO 7520-WA-DISKUP-STATUS.                              0004938
BT162                                                                    0004939
BT231      IF WA-FILE-ID = 'NB' AND WA-REC-ID = 'OW'                     0004940
BT231          MOVE NUM-OW TO WA-STATUS                                  0004941
BT231          GO TO 7520-WA-DISKUP-STATUS.                              0004942
BT231                                                                    0004943
BT043      IF WA-FILE-ID = 'TH' AND WA-REC-ID = 'NT'                     0004944
BT043          MOVE NUM-NT TO WA-STATUS.                                 0004945
AK131                                                                    0004946
AK131      IF WA-FILE-ID = 'PM' AND WA-REC-ID = 'PN'                     0004947
AK131          MOVE NUM-PN TO WA-STATUS                                  0004948
AK131          GO TO 7520-WA-DISKUP-STATUS.                              0004949
  
DD281      IF WA-FILE-ID = 'AP' AND WA-REC-ID = 'OR'                    05337000
DD281          MOVE NUM-OR TO WA-STATUS                                 05338000
DD281          GO TO 7520-WA-DISKUP-STATUS.                             05339000
AR150                                                                   DELLRT16
AR150      IF WA-FILE-ID = WS-NB AND WA-REC-ID = WS-AL                          
AR150          MOVE NUM-AL TO WA-STATUS                                         
AR150          GO TO 7520-WA-DISKUP-STATUS.                                     
AR150                                                                   
AR150      IF WA-FILE-ID = WS-NB AND WA-REC-ID = WS-GW                          
AR150          MOVE NUM-GW TO WA-STATUS                                         
AR150          GO TO 7520-WA-DISKUP-STATUS.                                     
AR150                                                                   
AR150      IF WA-FILE-ID = WS-NB AND WA-REC-ID = WS-IB                          
AR150          MOVE NUM-IB TO WA-STATUS                                         
AR150          GO TO 7520-WA-DISKUP-STATUS.                                     
BT444                                                                   DELLRT16
SS521      IF WA-FILE-ID = WS-NB AND WA-REC-ID = WS-DI
SS521          MOVE NUM-DI TO WA-STATUS
SS521          GO TO 7520-WA-DISKUP-STATUS.
SS521 
BT444      IF WA-FILE-ID = WS-NB AND WA-REC-ID = WS-EI                  DELLRT16
BT444          MOVE NUM-EI TO WA-STATUS.                                DELLRT16

NK771      IF WA-FILE-ID = WS-NB AND WA-REC-ID = WS-FM
NK771          MOVE NUM-FM TO WA-STATUS.  
NK771                                                                     
BT111      IF WA-STATUS = SPACES                                         0004951
BT111          MOVE '008' TO WA-STATUS                                   0004952
BT111                     WK-STATUS.                                     0004953
                                                                         0004954
       7520-WA-DISKUP-STATUS.                                            0004955
      *    CALL 'DISKUP' USING WA-APPLICATION-RECORD                     0004956
      *                 WA-KEY                                           0004957
      *                 WA-FILE-IDZ                                      0004958
      *                 RTN-CODE.                                        0004959
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           REWRITE VSAM-WA-REC                                          DELLIDCH
                      FROM WA-APPLICATION-RECORD                        DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE                       DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
                                                                         0004960
           IF RTN-CODE > +0                                              0004961
      *        CALL 'TPFIRLFN' USING WA-FILE-IDZ                        DELLMNCH
      *                       REL-RTN-CODE                              DELLMNCH
               DISPLAY                                                   0004964
           ' WA FILE I/O ERROR-PROGRAM RTN-CODE > +0 ' RTN-CODE          0004964
               DISPLAY ' ERROR IN 7520-WA-DISKUP-STATUS PARAGRAPH '      0004965
               DISPLAY ' ERROR ON DISKUP - WA-DELL-GUID ' HWA-DELL-GUID  0004966
               DISPLAY                                                   0004967
           'PROGRAM LSNNB402 IS ABENDING . . . CHECK WA STATUS '         0004967
               MOVE +7520 TO ABEND-CODE                                  0004968
               PERFORM 9900-ABEND-RTN.                                   0004969
                                                                         0004970
           GO TO 7510-READ-VALID-WA-RECS.                                0004971
       7599-SET-ALL-STATUS-EXIT.                                         0004972
           EXIT.                                                         0004973
                                                                         0004974
       7600-FINAL-AP-STATUS   SECTION.                                   0004975
           MOVE +0 TO RTN-CODE.                                          0004976
           MOVE SPACES TO WA-AP-KEY.                                     0004977
           MOVE SV-CO TO WA-AP-CO.                                       0004978
           MOVE SAVE-NEXT-POLICY TO WA-AP-POLICY.                        0004979
           MOVE 'AP' TO WA-AP-TYPE.                                      0004980
           PERFORM 8900-READ-APPREC.                                     0004981
           IF RTN-CODE > +0                                              0004982
               DISPLAY                                                   0004983
           ' AP FILE I/O ERROR-PROGRAM RTN-CODE > +0 ' RTN-CODE          0004983
               DISPLAY ' ERROR IN 7600-FINAL-AP-STATUS SECTION '         0004984
               DISPLAY ' ERROR ON READ - POLICY: ' SAVE-NEXT-POLICY      0004985
               DISPLAY                                                   0004986
           'PROGRAM LSNNB402 IS ABENDING . . . CHECK AP STATUS '         0004986
               MOVE +7600 TO ABEND-CODE                                  0004987
               PERFORM 9900-ABEND-RTN.                                   0004988
                                                                         0004989
           IF CURR-STAT-NA-WA = WK-STATUS                                0004990
               GO TO 7699-FINAL-AP-STATUS-EXIT.                          0004991
                                                                         0004992
BT132      IF CURR-STAT-NA-WA = 009 OR 020 OR 010                        0004993
             AND WK-STATUS = 008                                         0004994
               GO TO 7610-HOLD-AP-REC.                                   0004995
                                                                         0004996
           GO TO 7699-FINAL-AP-STATUS-EXIT.                              0004997
       7610-HOLD-AP-REC.                                                 0004998
      *    CALL 'DISKHOLD' USING APPLICATION-RECORD-AP                   0004999
      *                   KEY-NA-WA                                      0005000
      *                   AP-FILE-ID                                     0005001
      *                   RTN-CODE.                                      0005002
           MOVE KEY-NA-WA TO VSAM-AP-PRIM                               DELLIDCH
           READ VSAM-AP                                                 DELLIDCH
                    INTO APPLICATION-RECORD-AP                          DELLIDCH
           MOVE VSAM-AP-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE KEY-NA-WA TO TPSWNML-FILE-KEY                           DELLIDCH
           MOVE AP-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
                                                                         0005003
           IF RTN-CODE > +0                                              0005004
               DISPLAY ' AP FILE I/O ERROR-PROGRAM RTN-CODE > +0 '       0005005
               DISPLAY ' ERROR IN 7610-HOLD-AP-REC PARAGRAPH '           0005006
               DISPLAY ' ERROR ON DISKHOLD - KEY-NA  ' KEY-NA-WA         0005007
               DISPLAY                                                   0005008
           'PROGRAM LSNNB402 IS ABENDING . . . CHECK AP STATUS '         0005008
               MOVE +7610 TO ABEND-CODE                                  0005009
               PERFORM 9900-ABEND-RTN.                                   0005010
                                                                         0005011
       7620-AP-DISKUP-STATUS.                                            0005012
           MOVE WK-STATUS TO CURR-STAT-NA-WA.                            0005013
      *    CALL 'DISKUP' USING APPLICATION-RECORD-AP                     0005014
      *                 KEY-NA-WA                                        0005015
      *                 AP-FILE-ID                                       0005016
      *                 RTN-CODE.                                        0005017
           MOVE KEY-NA-WA TO VSAM-AP-PRIM                               DELLIDCH
           REWRITE VSAM-AP-REC                                          DELLIDCH
                      FROM APPLICATION-RECORD-AP                        DELLIDCH
           MOVE VSAM-AP-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE KEY-NA-WA TO TPSWNML-FILE-KEY                           DELLIDCH
           MOVE AP-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE                       DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
                                                                         0005018
           IF RTN-CODE > +0                                              0005019
               GO TO 7695-RELEASE-AP.                                    0005020
                                                                         0005021
           MOVE SPACES TO NB080-PASS-AREA.                               0005022
KM041      MOVE APPLICATION-RECORD-AP TO NB080-RECORD.                   0005023
      *    MOVE AP-FILE-ID TO NB080-FILE-ID.                             0005024
           MOVE AP-FILE-CO TO NB080-FILE-CO                             DELLMACH
           MOVE AP-FILE-LIT TO NB080-FILE-LIT                           DELLMACH
           MOVE KEY-NA-WA TO NB080-REC-KEY.                              0005025
           MOVE 'U' TO NB080-ACTION.                                     0005026
           CALL 'LSNNB080' USING LSNNB080-PASS-RECORD.                   0005027
           GO TO 7699-FINAL-AP-STATUS-EXIT.                              0005028
       7695-RELEASE-AP.                                                  0005029
      *    CALL 'TPFIRLFN' USING AP-FILE-ID                             DELLMNCH
      *                   REL-RTN-CODE                                  DELLMNCH
               DISPLAY ' AP FILE I/O ERROR-PROGRAM RTN-CODE > +0 '       0005032
               DISPLAY ' ERROR IN 7695-RELEASE-AP PARAGRAPH '            0005033
               DISPLAY ' ERROR ON DISKUP - KEY-NA ' KEY-NA-WA            0005034
               DISPLAY                                                   0005035
           'PROGRAM LSNNB402 IS ABENDING . . . CHECK AP STATUS '         0005035
               MOVE +7695 TO ABEND-CODE                                  0005036
               PERFORM 9900-ABEND-RTN.                                   0005037
                                                                         0005038
       7699-FINAL-AP-STATUS-EXIT.                                        0005039
           EXIT.                                                         0005040
                                                                         0005041
       7700-FINAL-SY-STATUS  SECTION.                                    0005042
           MOVE +0 TO WS-SUB1.                                           0005043
           MOVE +1 TO WS-SUB2.                                           0005044
       7710-LOCATE-SY-LOOP.                                              0005045
           ADD +1 TO WS-SUB1.                                            0005046
           IF WS-SUB1 > WTR-TABLE-MAX                                    0005047
               MOVE 'Y' TO ERROR-SWITCH                                  0005048
               GO TO 7799-FINAL-SY-STATUS-EXIT.                          0005049
                                                                         0005050
           IF WTR-FILE-ID (WS-SUB1) = 'WA'                               0005051
             AND WTR-REC-ID (WS-SUB1) = 'SY'                             0005052
               GO TO 7720-READ-SY-REC.                                   0005053
                                                                         0005054
           GO TO 7710-LOCATE-SY-LOOP.                                    0005055
       7720-READ-SY-REC.                                                 0005056
           MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WA-KEY.                 0005057
           PERFORM 8700-READ-KEY-WA.                                     0005058
           IF RTN-CODE = +0                                              0005059
               GO TO 7730-HOLD-SY-REC.                                   0005060
           IF RTN-CODE > +0                                              0005061
               DISPLAY ' WA FILE I/O ERROR-PROGRAM RTN-CODE > +0 '       0005062
               DISPLAY ' ERROR IN 7720-READ-SY-REC PARAGRAPH '           0005063
               DISPLAY                                                   0005064
           ' PROCESSING KEY : ' WTR-WA-KEY (WS-SUB1, WS-SUB2)            0005064
               DISPLAY                                                   0005065
           'PROGRAM LSNNB402 IS ABENDING . . . CHECK WA STATUS '         0005065
               MOVE +7720 TO ABEND-CODE                                  0005066
               PERFORM 9900-ABEND-RTN.                                   0005067
                                                                         0005068
       7730-HOLD-SY-REC.                                                 0005069
      *    CALL 'DISKHOLD' USING WA-APPLICATION-RECORD                   0005070
      *                   WA-KEY                                         0005071
      *                   WA-FILE-IDZ                                    0005072
      *                   RTN-CODE.                                      0005073
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           READ VSAM-WA                                                 DELLIDCH
                    INTO WA-APPLICATION-RECORD                          DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
           IF RTN-CODE > +0                                              0005074
               DISPLAY ' WA FILE I/O ERROR-PROGRAM RTN-CODE > +0 '       0005075
               DISPLAY ' ERROR IN 7730-HOLD-SY-REC PARAGRAPH '           0005076
               DISPLAY                                                   0005077
           ' PROCESSING KEY : ' WTR-WA-KEY (WS-SUB1, WS-SUB2)            0005077
               DISPLAY                                                   0005078
           'PROGRAM LSNNB402 IS ABENDING . . . CHECK WA STATUS '         0005078
               MOVE +7730 TO ABEND-CODE                                  0005079
               PERFORM 9900-ABEND-RTN.                                   0005080
                                                                         0005081
           MOVE WK-SYSTEM-DATE TO WA-MF-DATE.                            0005082
           MOVE WK-SYSTEM-TIME TO WA-MF-TIME.                            0005083
           MOVE WK-STATUS TO WA-STATUS.                                  0005084
BT193      MOVE SAVE-TASK-INDICATOR TO WASY-TASK-INDICATOR.              0005085
NK081      IF NO-TASK-REC-SW = 'Y'                                       0005086
NK081         MOVE 'E' TO WASY-TASK-INDICATOR                            0005087
NK081         GO TO 7740-DISKUP-SY-REC.                                  0005088

RS691      IF (WA-FILETYPE = 'E' AND SAVE-LS-OVERRIDE-IND = 'SS')       STAINT9S
RS691         GO TO 7740-DISKUP-SY-REC.                                 STAINT9S

           IF WK-STATUS = 'PCE'                                          0005089
               MOVE 'P' TO WASY-TASK-INDICATOR                           0005090
               GO TO 7740-DISKUP-SY-REC.                                 0005091
                                                                         0005092
BT043      IF NUM-INV = 008                                              0005093
BT043          MOVE 'P' TO WASY-TASK-INDICATOR                           0005094
BT043          GO TO 7740-DISKUP-SY-REC.                                 0005095
BT043                                                                    0005096
                                                                         0005097
                                                                         0005098
       7740-DISKUP-SY-REC.                                               0005099
      *    CALL 'DISKUP' USING WA-APPLICATION-RECORD                     0005100
      *                 WA-KEY                                           0005101
      *                 WA-FILE-IDZ                                      0005102
      *                 RTN-CODE.                                        0005103
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           REWRITE VSAM-WA-REC                                          DELLIDCH
                      FROM WA-APPLICATION-RECORD                        DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE                       DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
           IF RTN-CODE > +0                                              0005104
      *        CALL 'TPFIRLFN' USING WA-FILE-IDZ                        DELLMNCH
      *                       REL-RTN-CODE                              DELLMNCH
               DISPLAY ' WA FILE I/O ERROR-PROGRAM RTN-CODE > +0 '       0005107
               DISPLAY ' ERROR IN 7740-DISKUP-SY-REC PARAGRAPH '         0005108
               DISPLAY                                                   0005109
           ' PROCESSING KEY : ' WTR-WA-KEY (WS-SUB1, WS-SUB2)            0005109
               DISPLAY                                                   0005110
           'PROGRAM LSNNB402 IS ABENDING . . . CHECK WA STATUS '         0005110
               MOVE +7740 TO ABEND-CODE                                  0005111
               PERFORM 9900-ABEND-RTN.                                   0005112
                                                                         0005113
       7799-FINAL-SY-STATUS-EXIT.                                        0005114
           EXIT.                                                         0005115
                                                                         0005116
MK131  7800-DEL-ALL-AP-RECORDS  SECTION.                                 0005117
MK131      IF POLICY-NA-WA = SAVE-PAPOL-WASY                             0005118
MK131        AND AP-REC-NA-WA = 'AP'                                     0005119
MK131          GO TO 7805-DELETE-AP-REC-LOOP.                            0005120
MK131                                                                    0005121
MK131      MOVE +0 TO RTN-CODE.                                          0005122
MK131      MOVE SPACES TO WA-AP-KEY.                                     0005123
MK131      MOVE SV-CO TO WA-AP-CO.                                       0005124
MK131      MOVE GU-POLICY TO WA-AP-POLICY.                               0005125
MK131      MOVE 'AP' TO WA-AP-TYPE.                                      0005126
MK131      PERFORM 8900-READ-APPREC.                                     0005127
MK131      IF RTN-CODE = +1                                              0005128
MK131          MOVE 'ERR' TO WK-STATUS                                   0005129
MK131          PERFORM 7000-STATUS-ERROR-WA                              0005130
MK131          MOVE GU-POLICY TO SAVE-TASK-POLICY                        0005131
MK131          PERFORM 2600-ERROR-TASKS                                  0005132
MK131          MOVE 'Y' TO ERROR-SWITCH                                  0005133
MK131          GO TO 7899-DEL-ALL-AP-RECORDS-EXIT.                       0005134
MK131                                                                    0005135
MK131      IF RTN-CODE = +12                                             0005136
MK131          DISPLAY ' I/O ERROR ON READ OF AP FILE - RTN-CODE = +12 ' 0005137
MK131          DISPLAY ' LOCATION IS 7800-DEL-ALL-AP-RECORDS SECTION'    0005138
MK131          DISPLAY ' PROCESSING POLICY:  ' GU-POLICY                 0005139
MK131          DISPLAY '  . . . LSNNB402 IS ABENDING  '                  0005140
MK131          MOVE +7800 TO ABEND-CODE                                  0005141
MK131          PERFORM 9900-ABEND-RTN.                                   0005142
MK131                                                                    0005143
MK131      MOVE +0 TO RTNCD-CTL.                                         0005144
MK131  7805-DELETE-AP-REC-LOOP.                                          0005145
MK131 *    CALL 'DISKDEL' USING APPLICATION-RECORD-AP                    0005146
MK131 *                  WA-AP-KEY                                       0005147
MK131 *                  AP-FILE-ID                                      0005148
MK131 *                  RTNCD-CTL.                                      0005149
           MOVE WA-AP-KEY TO VSAM-AP-PRIM                               DELLIDCH
           DELETE VSAM-AP RECORD                                        DELLIDCH
           MOVE VSAM-AP-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-AP-KEY TO TPSWNML-FILE-KEY                           DELLIDCH
           MOVE AP-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKDEL' TO TPSWNML-FUNCTION-CODE                      DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTNCD-CTL.                             DELLIDCH
MK131      IF RTNCD-CTL = 0                                              0005150
MK131          MOVE SPACES TO NB080-PASS-AREA                            0005151
MK131          MOVE NEW-BUSINESS-RECORD TO NB080-RECORD                  0005152
MK131 *        MOVE AP-FILE-ID TO NB080-FILE-ID                          0005153
               MOVE AP-FILE-CO TO NB080-FILE-CO                         DELLMACH
               MOVE AP-FILE-LIT TO NB080-FILE-LIT                       DELLMACH
MK131          MOVE KEY-NA TO NB080-REC-KEY                              0005154
MK131          MOVE 'A' TO NB080-ACTION                                  0005155
MK131          CALL 'LSNNB080' USING LSNNB080-PASS-RECORD                0005156
MK131          GO TO 7810-READ-ALL-AP-LOOP.                              0005157
MK131      MOVE 'ERR' TO WK-STATUS.                                      0005158
MK131      PERFORM 7200-APPLICATION-ERROR.                               0005159
MK131      PERFORM 2600-ERROR-TASKS.                                     0005160
MK131      GO TO 7899-DEL-ALL-AP-RECORDS-EXIT.                           0005161
MK131  7810-READ-ALL-AP-LOOP.                                            0005162
MK131 *    CALL 'RDNXTDAT' USING APPLICATION-RECORD-AP                   0005163
MK131 *                   WA-AP-KEY                                      0005164
MK131 *                   AP-FILE-ID                                     0005165
MK131 *                   RTNCD-CTL.                                     0005166
           IF WA-AP-KEY <> VSAM-AP-PRIM                                 DELLMNCH
           MOVE WA-AP-KEY TO VSAM-AP-PRIM                               DELLIDCH
            START VSAM-AP                                               DELLIDCH
                  KEY IS >  VSAM-AP-PRIM                                DELLMNCH
            END-START                                                   DELLIDCH
            MOVE VSAM-AP-FS TO WS-INDX-FS-99                            DELLIDCH
            MOVE VSAM-AP-REC TO APPLICATION-RECORD-AP                   DELLIDCH
            INITIALIZE TPSWNML-AREA                                     DELLIDCH
            MOVE WA-AP-KEY TO TPSWNML-FILE-KEY                          DELLIDCH
            MOVE AP-FILE-ID TO TPSWNML-FILE-ID                          DELLIDCH
            MOVE 'RDNXTDAT' TO TPSWNML-FUNCTION-CODE                    DELLIDCH
            PERFORM P9999-MAP-RESP-CODE                                 DELLIDCH
            MOVE WS-INDX-FS-99 TO RTNCD-CTL                             DELLIDCH
           END-IF                                                       DELLIDCH
           READ VSAM-AP NEXT RECORD                                     DELLIDCH
                  INTO APPLICATION-RECORD-AP                            DELLIDCH
           MOVE VSAM-AP-FS TO WS-INDX-FS-99                             DELLIDCH
           MOVE WA-AP-KEY TO TPSWNML-FILE-KEY                           DELLIDCH
           MOVE VSAM-AP-PRIM TO WA-AP-KEY                               DELLMNCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTNCD-CTL                              DELLIDCH
MK131      IF RTNCD-CTL = +0                                             0005167
MK131          NEXT SENTENCE                                             0005168
MK131      ELSE                                                          0005169
MK131          GO TO 7899-DEL-ALL-AP-RECORDS-EXIT.                       0005170
MK131                                                                    0005171
MK131      IF POLICY-NA-WA > SAVE-PAPOL-WASY                             0005172
MK131         GO TO 7899-DEL-ALL-AP-RECORDS-EXIT.                        0005173
MK131                                                                    0005174
MK131      GO TO 7805-DELETE-AP-REC-LOOP.                                0005175
MK131  7899-DEL-ALL-AP-RECORDS-EXIT.                                     0005176
MK131      EXIT.                                                         0005177
MK131                                                                    0005178
BT261  7900-DEL-ALL-NB-RECORDS  SECTION.                                DELLRETC
BT261      MOVE +0 TO RTN-CODE.                                         DELLRETC
BT261      MOVE SPACES TO LIFE-MASTER-RECORD.                           DELLRETC
BT261      MOVE SV-CO TO LM-CO.                                         DELLRETC
BT261      MOVE SAVE-NEXT-POLICY TO LM-POLICY.                          DELLRETC
BT261      PERFORM 9400-READ-NB-MST.                                    DELLRETC
BT261      IF (RTN-CODE NOT = +0)                                       DELLRETC
BT261          GO TO 7999-DEL-ALL-NB-RECORDS-EXIT.                      DELLRETC
BT261      IF LM-RIDER = SPACES                                         DELLRETC
BT261          GO TO 7910-READ-ALL-NB-LOOP.                             DELLRETC
BT261  7905-DELETE-NB-REC-LOOP.                                         DELLRETC
BT261 *    CALL 'DISKDEL' USING LIFE-MASTER-RECORD                      DELLRETC
BT261 *                  LM-POLID                                       DELLRETC
BT261 *                  NB-FILE-ID                                     DELLRETC
BT261 *                  RTN-CODE.                                      DELLRETC
           MOVE LM-POLID  TO VSAM-NB-PRIM                               DELLRETC
           DELETE VSAM-NB RECORD                                        DELLRETC
           MOVE VSAM-NB-FS TO WS-INDX-FS-99                             DELLRETC
           INITIALIZE TPSWNML-AREA                                      DELLRETC
           MOVE LM-POLID TO TPSWNML-FILE-KEY                            DELLRETC
           MOVE NB-FILE-ID TO TPSWNML-FILE-ID                           DELLRETC
           MOVE 'DISKDEL ' TO TPSWNML-FUNCTION-CODE                     DELLRETC
           PERFORM P9999-MAP-RESP-CODE                                  DELLRETC
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLRETC
BT261      IF RTN-CODE = +0                                             DELLRETC
BT261          NEXT SENTENCE                                            DELLRETC
BT261      ELSE                                                         DELLRETC
BT261          GO TO 7999-DEL-ALL-NB-RECORDS-EXIT.                      DELLRETC
BT261  7910-READ-ALL-NB-LOOP.                                           DELLRETC
BT261 *    CALL 'RDNXTDAT' USING LIFE-MASTER-RECORD                     DELLRETC
BT261 *                   LM-POLID                                      DELLRETC
BT261 *                   NB-FILE-ID                                    DELLRETC
BT261 *                   RTN-CODE.                                     DELLRETC
           IF LM-POLID <> VSAM-NB-PRIM                                  DELLMNCH
            MOVE LM-POLID TO VSAM-NB-PRIM                               DELLIDCH
            START VSAM-NB                                               DELLIDCH
                  KEY IS >  VSAM-NB-PRIM                                DELLMNCH
            END-START                                                   DELLIDCH
            MOVE VSAM-NB-FS TO WS-INDX-FS-99                            DELLIDCH
            INITIALIZE TPSWNML-AREA                                     DELLIDCH
            MOVE LM-POLID  TO TPSWNML-FILE-KEY                          DELLIDCH
            MOVE NB-FILE-ID TO TPSWNML-FILE-ID                          DELLIDCH
            MOVE 'RDNXTDAT' TO TPSWNML-FUNCTION-CODE                    DELLIDCH
            PERFORM P9999-MAP-RESP-CODE                                 DELLIDCH
            MOVE WS-INDX-FS-99 TO RTN-CODE                              DELLIDCH
           END-IF                                                       DELLIDCH
           READ VSAM-NB NEXT RECORD                                     DELLIDCH
                  INTO LIFE-MASTER-RECORD                               DELLIDCH
           MOVE VSAM-NB-FS TO WS-INDX-FS-99                             DELLIDCH
           MOVE VSAM-NB-PRIM TO TPSWNML-FILE-KEY                        DELLIDCH
           MOVE VSAM-NB-PRIM TO LM-POLID                                DELLMNCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLRETC
BT261      IF RTN-CODE = +0                                             DELLRETC
BT261          NEXT SENTENCE                                            DELLRETC
BT261      ELSE                                                         DELLRETC
BT261          GO TO 7999-DEL-ALL-NB-RECORDS-EXIT.                      DELLRETC
BT261                                                                   DELLRETC
BT261      IF (LM-POL NOT = SAVE-NEXT-POLICY)                           DELLRETC
BT261          GO TO 7999-DEL-ALL-NB-RECORDS-EXIT.                      DELLRETC
BT261                                                                   DELLRETC
BT261      GO TO 7905-DELETE-NB-REC-LOOP.                               DELLRETC
BT261                                                                   DELLRETC
BT261  7999-DEL-ALL-NB-RECORDS-EXIT.                                    DELLRETC
BT261      EXIT.                                                        DELLRETC
       8000-READ-PREV-WA   SECTION.                                      0005179
           MOVE +0 TO RTN-CODE.                                          0005180
           MOVE SPACES TO WA-APPLICATION-RECORD.                         0005181
           MOVE HOLD-WA-KEY TO WA-FILE-KEY.                              0005182
           MOVE HIGH-VALUES TO WA-WA-DUP-SEQ.                            0005183
      *    CALL 'TPFIRDP' USING WA-APPLICATION-RECORD                    0005184
      *                  WA-FILE-KEY                                     0005185
      *                  WA-FILE-IDZ                                     0005186
      *                  RTN-CODE.                                       0005187
           IF WA-FILE-KEY <> VSAM-WA-PRIM                               DELLMNCH
           MOVE WA-FILE-KEY TO VSAM-WA-PRIM                             DELLIDCH
            START VSAM-WA                                               DELLIDCH
                  KEY IS <  VSAM-WA-PRIM                                DELLMNCH
            END-START                                                   DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           MOVE VSAM-WA-REC TO WA-APPLICATION-RECORD                    DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-FILE-KEY TO TPSWNML-FILE-KEY                         DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'TPFIRDP' TO TPSWNML-FUNCTION-CODE                      DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE                               DELLIDCH
           END-IF                                                       DELLIDCH
           READ VSAM-WA PREVIOUS RECORD                                 DELLIDCH
                   INTO WA-APPLICATION-RECORD                           DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           MOVE WA-FILE-KEY TO TPSWNML-FILE-KEY                         DELLIDCH
           MOVE VSAM-WA-PRIM TO WA-FILE-KEY                             DELLMNCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
                                                                         0005188
       8099-READ-PREV-WA-EXIT.                                           0005189
           EXIT.                                                         0005190
                                                                         0005191
       8100-UPDT-REC-WA   SECTION.                                       0005192
      *    CALL 'DISKHOLD' USING WA-APPLICATION-RECORD                   0005193
      *                   WA-KEY                                         0005194
      *                   WA-FILE-IDZ                                    0005195
      *                   RTN-CODE.                                      0005196
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           READ VSAM-WA                                                 DELLIDCH
                    INTO WA-APPLICATION-RECORD                          DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
           IF RTN-CODE > +0                                              0005197
               DISPLAY ' WA FILE I/O ERROR-PROGRAM RTN-CODE > +0 '       0005198
               DISPLAY ' ERROR IN 8100-UPDT-REC-WA SECTION  '            0005199
               DISPLAY ' ERROR ON DISKHOLD - WA-DELL-GUID ' WA-DELL-GUID 0005200
               MOVE +8100 TO ABEND-CODE                                  0005201
               PERFORM 9900-ABEND-RTN.                                   0005202
                                                                         0005203
           MOVE SPACES TO WA-APPLICATION-RECORD.                         0005204
           MOVE +0 TO RTN-CODE.                                          0005205
           MOVE WK-SYSTEM-TIME TO HWA-MF-TIME.                           0005206
           MOVE WK-SYSTEM-DATE TO HWA-MF-DATE.                           0005207
           MOVE HOLD-WA-RECORD TO WA-APPLICATION-RECORD.                 0005208
      *    CALL 'DISKUP' USING WA-APPLICATION-RECORD                     0005209
      *                 WA-KEY                                           0005210
      *                 WA-FILE-IDZ                                      0005211
      *                 RTN-CODE.                                        0005212
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           REWRITE VSAM-WA-REC                                          DELLIDCH
                      FROM WA-APPLICATION-RECORD                        DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE                       DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
           IF RTN-CODE > +0                                              0005213
      *        CALL 'TPFIRLFN' USING WA-FILE-IDZ                        DELLMNCH
      *                       REL-RTN-CODE                              DELLMNCH
               DISPLAY ' WA FILE I/O ERROR-PROGRAM RTN-CODE > +0 '       0005216
               DISPLAY ' ERROR IN 8100-REC-WA SECTION  '                 0005217
               DISPLAY ' ERROR ON DISKUP - WA-DELL-GUID ' HWA-DELL-GUID  0005218
               MOVE +8100 TO ABEND-CODE                                  0005219
               PERFORM 9900-ABEND-RTN.                                   0005220
                                                                         0005221
       8199-UPDT-REC-WA-EXIT.                                            0005222
           EXIT.                                                         0005223
                                                                         0005224
CL131  8200-READ-NEXT-ALT-KEY   SECTION.                                 0005225
CL131 *8210-READ-NEXT-ALT.                                              DELLMNCH
CL131      MOVE +0 TO AI-RTN-CODE.                                       0005227
CL131 *    CALL 'RDNXTDAT' USING WA-APPLICATION-RECORD, AI-W9-KEY        0005228
CL131 *                  W9-FILE-ID, AI-RTN-CODE.                        0005229
           IF VSAM-W8-PRIM <> AI-W9-KEY                                 DELLMACH
           MOVE AI-W9-KEY TO VSAM-W8-PRIM                               DELLIDCH
      *    MOVE HOLD-VSAM-WA-PRIM TO VSAM-WA-PRIM                       DELLMNCH
           START VSAM-W8                                                DELLIDCH
                 KEY IS > VSAM-W8-PRIM                                  DELLIDCH
           END-START                                                    DELLIDCH
           MOVE VSAM-W8-FS TO WS-INDX-FS-99                             DELLIDCH
           MOVE VSAM-W8-REC TO WA-APPLICATION-RECORD-GRP                DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE AI-W9-KEY TO TPSWNML-FILE-KEY                           DELLIDCH
           MOVE W9-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'RDNXTDAT' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO AI-RTN-CODE.                           DELLIDCH
CL131  8210-READ-NEXT-ALT.
           READ VSAM-W8 NEXT RECORD  WITH NO LOCK                       DELLIDCH
                  INTO WA-APPLICATION-RECORD-GRP                        DELLIDCH
           MOVE VSAM-W8-FS TO WS-INDX-FS-99                             DELLIDCH
           MOVE WA-DELL-GUID TO TPSWNML-FILE-KEY                        DELLMNCH
           MOVE VSAM-ALT-WA-KEY1 TO AI-WA-KEY                           DELLMNCH
           MOVE VSAM-WA-PRIM to  HOLD-VSAM-WA-PRIM                      DELLMNCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO AI-RTN-CODE                            DELLIDCH
CL131      MOVE AI-RTN-CODE TO RTN-CODE.                                 0005230
CL131                                                                    0005231
BT132      IF (RTN-CODE NOT = +0)                                        0005232
BT132          GO TO 8299-READ-NEXT-ALT-KEY-EXIT.                        0005233
PS134 ***********************************************************        0005234
PS134 ***  ADVANCE THE APP-COUNTER WITH EACH APAP  01 RECORD  ***        0005235
PS134 ***********************************************************        0005236
PS134      IF WA-REC-TYPE = 'APAP  01'                                   0005237
PS134          ADD +1 TO APP-COUNTER.                                    0005238
PS134      PERFORM 9100-CHECK-FOR-BYPASS.                                0005239
PS134      IF WS-BYPASS-FLAG = 'Y'                                       0005240
PS134          GO TO 8210-READ-NEXT-ALT.                                 0005241
PS133 ***********************************************************        0005242
PS133 ***       LOOK FOR A CHANGE IN THE ALTERNATE KEY        ***        0005243
PS133 ***********************************************************        0005244
           IF WA-KEY(1:40) = AI-W9-DELL-GUID(1:40)                      DELLMACH
               GO TO 8210-READ-NEXT-ALT.                                DELLMACH
           MOVE WA-KEY       TO AI-W9-DELL-GUID                         DELLMNCH
           MOVE WA-OS-DATEX  TO AI-W9-OS-DATE                           DELLMNCH
           MOVE WA-OS-TIME   TO AI-W9-OS-TIME                           DELLMNCH
           MOVE WA-OS-DATEX  TO WA-APPLICATION-DATE                     DELLMACH
           MOVE WA-OS-TIME  TO  WA-APPLICATION-TIME                     DELLMACH 
CL131      IF HOLD-AI-W9-KEY = AI-W9-KEY                                DELLMNCH
CL131          GO TO 8210-READ-NEXT-ALT.                                DELLMACH
CL131                                                                    0005247
CL131      MOVE AI-W9-KEY TO HOLD-AI-W9-KEY.                            DELLMNCH
CL131                                                                    0005249
CL131  8299-READ-NEXT-ALT-KEY-EXIT.                                      0005250
CL131      EXIT.                                                         0005251
CL131                                                                    0005252
       8300-READ-NEXT-WA-RECS   SECTION.                                 0005253
BT132 *    CALL 'RDNXTDAT' USING WA-APPLICATION-RECORD                   0005254
BT132 *                   WS-KEY                                         0005255
BT132 *                   WA-FILE-IDZ                                    0005256
BT132 *                   RTN-CODE.                                      0005257
           IF WS-KEY <> VSAM-WA-PRIM                                    DELLMNCH
           MOVE WS-KEY TO VSAM-WA-PRIM                                  DELLIDCH
            START VSAM-WA                                               DELLIDCH
                  KEY IS >  VSAM-WA-PRIM                                DELLMNCH
            END-START                                                   DELLIDCH
            MOVE VSAM-WA-FS TO WS-INDX-FS-99                            DELLIDCH
            MOVE VSAM-WA-REC TO WA-APPLICATION-RECORD                   DELLIDCH
            INITIALIZE TPSWNML-AREA                                     DELLIDCH
            MOVE WS-KEY TO TPSWNML-FILE-KEY                             DELLIDCH
            MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                         DELLIDCH
            MOVE 'RDNXTDAT' TO TPSWNML-FUNCTION-CODE                    DELLIDCH
            PERFORM P9999-MAP-RESP-CODE                                 DELLIDCH
            MOVE WS-INDX-FS-99 TO RTN-CODE                              DELLIDCH
           END-IF                                                       DELLIDCH
           READ VSAM-WA NEXT RECORD                                     DELLIDCH
                  INTO WA-APPLICATION-RECORD                            DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           MOVE WS-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE                               DELLIDCH
           MOVE VSAM-WA-PRIM TO WS-KEY.                                 DELLMNCH
       8399-READ-NEXT-WA-RECS-EXIT.                                      0005258
           EXIT.                                                         0005259
                                                                         0005260
       8400-READ-PLAN   SECTION.                                         0005261
           CALL 'READPLAN' USING PLAN-REC                                0005262
                          PL-FILE-ID                                     0005263
                          RTN-CODE.                                      0005264
           IF RTN-CODE = +0                                              0005265
               GO TO 8499-READ-PLAN-EXIT.                                0005266
                                                                         0005267
           MOVE 'Y' TO ERROR-SWITCH.                                     0005268
       8499-READ-PLAN-EXIT.                                              0005269
           EXIT.                                                         0005270
                                                                         0005271
       8500-UPDATE-SY-STATUS  SECTION.                                   0005272
           MOVE +0 TO WS-SUB1.                                           0005273
           MOVE +1 TO WS-SUB2.                                           0005274
       8510-LOCATE-SY-LOOP.                                              0005275
           ADD +1 TO WS-SUB1.                                            0005276
           IF WS-SUB1 > WTR-TABLE-MAX                                    0005277
BT043          MOVE 'Y' TO ERROR-SWITCH                                  0005278
               GO TO 8599-UPDATE-SY-STATUS-EXIT.                         0005279
                                                                         0005280
           IF WTR-FILE-ID (WS-SUB1) = 'WA'                               0005281
             AND WTR-REC-ID (WS-SUB1) = 'SY'                             0005282
               GO TO 8520-READ-SY-REC.                                   0005283
                                                                         0005284
           GO TO 8510-LOCATE-SY-LOOP.                                    0005285
       8520-READ-SY-REC.                                                 0005286
           MOVE WTR-WA-KEY (WS-SUB1, WS-SUB2) TO WA-KEY.                 0005287
           PERFORM 8700-READ-KEY-WA.                                     0005288
           IF RTN-CODE = +0                                              0005289
               GO TO 8530-HOLD-SY-REC.                                   0005290
           IF RTN-CODE > +0                                              0005291
               DISPLAY                                                   0005292
           'ERROR READING WASY RECORD - RTN-CODE = +' RTN-CODE           0005292
               DISPLAY 'LOCATION IS 8520-READ-SY-REC PARAGRAPH '         0005293
               DISPLAY 'WA KEY IS:  ' WTR-WA-KEY (WS-SUB1, WS-SUB2)      0005294
               DISPLAY 'CHECK WA FILE STATUS . . . '                     0005295
               MOVE +8520 TO ABEND-CODE                                  0005296
               PERFORM 9900-ABEND-RTN.                                   0005297
                                                                         0005298
       8530-HOLD-SY-REC.                                                 0005299
      *    CALL 'DISKHOLD' USING WA-APPLICATION-RECORD                   0005300
      *                   WA-KEY                                         0005301
      *                   WA-FILE-IDZ                                    0005302
      *                   RTN-CODE.                                      0005303
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           READ VSAM-WA                                                 DELLIDCH
                    INTO WA-APPLICATION-RECORD                          DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
           IF RTN-CODE > +0                                              0005304
               DISPLAY 'ERROR DISKHOLD WA FILE - RTN-CODE = +' RTN-CODE  0005305
               DISPLAY 'LOCATION IS 8530-HOLD-SY-REC PARAGRAPH  '        0005306
               DISPLAY 'WA KEY IS:  ' WTR-WA-KEY (WS-SUB1, WS-SUB2)      0005307
               DISPLAY 'CHECK WA FILE STATUS . . . '                     0005308
               MOVE +8530 TO ABEND-CODE                                  0005309
               PERFORM 9900-ABEND-RTN.                                   0005310
                                                                         0005311
           MOVE WK-SYSTEM-DATE TO WA-MF-DATE.                            0005312
           MOVE WK-SYSTEM-TIME TO WA-MF-TIME.                            0005313
           MOVE WK-STATUS TO WA-STATUS.                                  0005314
MK131      IF WK-STATUS = 'PCE' OR 'DUP' OR 'PNU'                        0005315
BT043          MOVE 'P' TO WASY-TASK-INDICATOR                           0005316
BT043          GO TO 8540-DISKUP-SY-REC.                                 0005317
MK131      IF WA-POLCRECTYPE = 'P'                                       0005318
MK131          MOVE 'C' TO WASY-TASK-INDICATOR                           0005319
MK131          GO TO 8540-DISKUP-SY-REC.                                 0005320
       8540-DISKUP-SY-REC.                                               0005321
      *    CALL 'DISKUP' USING WA-APPLICATION-RECORD                     0005322
      *                 WA-KEY                                           0005323
      *                 WA-FILE-IDZ                                      0005324
      *                 RTN-CODE.                                        0005325
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           REWRITE VSAM-WA-REC                                          DELLIDCH
                      FROM WA-APPLICATION-RECORD                        DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE                       DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
           IF RTN-CODE > +0                                              0005326
      *        CALL 'TPFIRLFN' USING WA-FILE-IDZ                        DELLMNCH
      *                       REL-RTN-CODE                              DELLMNCH
               DISPLAY 'ERROR DISKUP WA FILE - RTN-CODE = +' RTN-CODE    0005329
               DISPLAY 'LOCATION IS 8540-DISKUP-SY-REC PARAGRAPH '       0005330
               DISPLAY 'WA KEY IS:  ' WTR-WA-KEY (WS-SUB1, WS-SUB2)      0005331
               DISPLAY 'CHECK WA FILE STATUS . . . '                     0005332
               MOVE +8540 TO ABEND-CODE                                  0005333
               PERFORM 9900-ABEND-RTN.                                   0005334
                                                                         0005335
       8599-UPDATE-SY-STATUS-EXIT.                                       0005336
           EXIT.                                                         0005337
                                                                         0005338
       8600-READ-ALT-GU  SECTION.                                        0005339
      *    CALL 'RDNXTDAT' USING GU-GUID-RECORD                          0005340
      *                   AI-GU-KEY                                      0005341
      *                   U1-FILE-ID                                     0005342
      *                   RTN-CODE.                                      0005343
           IF AI-GU-VALUE <> VSAM-ALT-GU-KEY1                           DELLMNCH
           MOVE AI-GU-VALUE TO VSAM-ALT-GU-KEY1                         DELLMNCH
            START VSAM-GU                                               DELLMNCH
                  KEY IS >  VSAM-ALT-GU-KEY1                            DELLMNCH
            END-START                                                   DELLIDCH
            MOVE VSAM-GU-FS TO WS-INDX-FS-99                            DELLIDCH
            MOVE VSAM-GU-REC TO GU-GUID-RECORD                          DELLMNCH
            INITIALIZE TPSWNML-AREA                                     DELLIDCH
            MOVE AI-GU-VALUE TO TPSWNML-FILE-KEY                        DELLMNCH
            MOVE U1-FILE-ID TO TPSWNML-FILE-ID                          DELLIDCH
            MOVE 'RDNXTDAT' TO TPSWNML-FUNCTION-CODE                    DELLIDCH
            PERFORM P9999-MAP-RESP-CODE                                 DELLIDCH
            MOVE WS-INDX-FS-99 TO RTN-CODE                              DELLIDCH
           END-IF                                                       DELLIDCH
           READ VSAM-GU NEXT RECORD                                     DELLMNCH
                  INTO GU-GUID-RECORD                                   DELLIDCH
           MOVE VSAM-GU-FS TO WS-INDX-FS-99                             DELLIDCH
           MOVE GU-DELL-GUID TO TPSWNML-FILE-KEY                        DELLMNCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE                               DELLIDCH
           MOVE VSAM-ALT-GU-KEY1 TO AI-GU-VALUE.                        DELLMNCH
       8699-READ-ALT-GU-EXIT.                                            0005344
           EXIT.                                                         0005345
                                                                         0005346
       8700-READ-KEY-WA  SECTION.                                        0005347
      *    CALL 'DISKREAD' USING WA-APPLICATION-RECORD                   0005348
      *                   WA-KEY                                         0005349
      *                   WA-FILE-IDZ                                    0005350
      *                   RTN-CODE.                                      0005351
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           READ VSAM-WA                                                 DELLIDCH
                    INTO WA-APPLICATION-RECORD                          DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKREAD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
       8799-READ-KEY-WA-EXIT.                                            0005352
           EXIT.                                                         0005353
                                                                         0005354
       8800-READ-POLMST  SECTION.                                        0005355
      *    CALL 'DISKREAD' USING LIFE-MASTER-RECORD                      0005356
      *                   LM-POLID                                       0005357
      *                   PO-FILE-ID                                     0005358
      *                   RTN-CODE.                                      0005359
           MOVE LM-POLID TO VSAM-PO-PRIM                                DELLIDCH
           READ VSAM-PO                                                 DELLIDCH
                    INTO LIFE-MASTER-RECORD                             DELLIDCH
           MOVE VSAM-P2-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE LM-POLID TO TPSWNML-FILE-KEY                            DELLIDCH
           MOVE PO-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKREAD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
       8899-READ-POLMST-EXIT.                                            0005360
           EXIT.                                                         0005361
                                                                         0005362
       8900-READ-APPREC  SECTION.                                        0005363
      *    CALL 'DISKREAD' USING APPLICATION-RECORD-AP                   0005364
      *                       WA-AP-KEY                                  0005365
      *                       AP-FILE-ID                                 0005366
      *                       RTN-CODE.                                  0005367
           MOVE WA-AP-KEY TO VSAM-AP-PRIM                               DELLIDCH
           READ VSAM-AP                                                 DELLIDCH
                    INTO APPLICATION-RECORD-AP                          DELLIDCH
           MOVE VSAM-AP-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-AP-KEY TO TPSWNML-FILE-KEY                           DELLIDCH
           MOVE AP-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKREAD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
       8999-READ-APPREC-EXIT.                                            0005368
           EXIT.                                                         0005369
                                                                         0005370
       9000-READ-GU  SECTION.                                            0005371
           MOVE +0 TO RTN-CODE.                                          0005372
           MOVE SPACES TO GU-KEY.                                        0005373
           MOVE HWA-CLIENT-GUID TO GU-CLIENT-GUID.                       0005374
      *    CALL 'DISKREAD' USING GU-GUID-RECORD                          0005375
      *                   GU-KEY                                         0005376
      *                   GU-FILE-ID                                     0005377
      *                   RTN-CODE.                                      0005378
           MOVE GU-KEY TO VSAM-GU-PRIM                                  DELLIDCH
           READ VSAM-GU                                                 DELLIDCH
                    INTO GU-GUID-RECORD                                 DELLIDCH
           MOVE VSAM-GU-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE GU-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE GU-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKREAD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
       9099-READ-GU-EXIT.                                                0005379
           EXIT.                                                         0005380
                                                                         0005381
PS134  9100-CHECK-FOR-BYPASS  SECTION.                                   0005382
PS134      MOVE +0 TO PARM-SUB.                                          0005383
PS134      MOVE 'N' TO WS-BYPASS-FLAG.                                   0005384
PS134  9110-PARM02-TABLE-LOOP.                                           0005385
PS134      ADD +1 TO PARM-SUB.                                           0005386
PS134      IF PARM-SUB > PARM02-TABLE-MAX                                0005387
PS134          GO TO 9199-CHECK-FOR-BYPASS-EXIT.                         0005388
PS134      IF PARM02-BYPASS-DELLGUID (PARM-SUB) = WA-DELL-GUID           0005389
PS134          MOVE 'Y' TO WS-BYPASS-FLAG                                0005390
PS134          GO TO 9199-CHECK-FOR-BYPASS-EXIT.                         0005391
PS134      GO TO 9110-PARM02-TABLE-LOOP.                                 0005392
PS134  9199-CHECK-FOR-BYPASS-EXIT.                                       0005393
PS134      EXIT.                                                         0005394
                                                                         0005395
PS134  9200-CLEAR-PROCESS-FLAG  SECTION.                                 0005396
PS134      MOVE LOW-VALUES TO WA-APPLICATION-RECORD.                     0005397
PS134  9210-READ-NEXT-WA.                                                0005398
PS134      MOVE +0 TO RTN-CODE.                                          0005399
PS134 *    CALL 'RDNXTDAT' USING WA-APPLICATION-RECORD                   0005400
PS134 *                   WA-KEY                                         0005401
PS134 *                   WA-FILE-IDZ                                    0005402
PS134 *                   RTN-CODE.                                      0005403
           IF WA-KEY <> VSAM-WA-PRIM                                    DELLMNCH
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
            START VSAM-WA                                               DELLIDCH
                  KEY IS >  VSAM-WA-PRIM                                DELLMNCH
            END-START                                                   DELLIDCH
            MOVE VSAM-WA-FS TO WS-INDX-FS-99                            DELLIDCH
            MOVE VSAM-WA-REC TO WA-APPLICATION-RECORD                   DELLIDCH
            INITIALIZE TPSWNML-AREA                                     DELLIDCH
            MOVE WA-KEY TO TPSWNML-FILE-KEY                             DELLIDCH
            MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                         DELLIDCH
            MOVE 'RDNXTDAT' TO TPSWNML-FUNCTION-CODE                    DELLIDCH
            PERFORM P9999-MAP-RESP-CODE                                 DELLIDCH
            MOVE WS-INDX-FS-99 TO RTN-CODE                              DELLIDCH
           END-IF                                                       DELLIDCH
           READ VSAM-WA NEXT RECORD                                     DELLIDCH
                  INTO WA-APPLICATION-RECORD                            DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE VSAM-WA-PRIM TO WA-KEY                                  DELLMNCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE                               DELLIDCH
PS134      IF RTN-CODE = +0                                              0005404
PS134          NEXT SENTENCE                                             0005405
PS134      ELSE                                                          0005406
PS134          GO TO 9299-CLEAR-PROCESS-FLAG-EXIT.                       0005407
PS134      IF WA-REC-TYPE = 'APAP  01'                                   0005408
PS134          NEXT SENTENCE                                             0005409
PS134      ELSE                                                          0005410
PS134          GO TO 9210-READ-NEXT-WA.                                  0005411
PS134 ***********************************************************        0005412
PS134 ***  CHECK THE FLAG FOR A "Y" TO CUT DOWN ON I/O        ***        0005413
PS134 ***********************************************************        0005414
PS134      IF APAP1-DELL-GUID-PROCESSED-FLAG = SPACES                    0005415
PS134          GO TO 9210-READ-NEXT-WA.                                  0005416
PS134      MOVE +0 TO RTN-CODE.                                          0005417
PS134 *    CALL 'DISKHOLD' USING WA-APPLICATION-RECORD                   0005418
PS134 *                   WA-KEY                                         0005419
PS134 *                   WA-FILE-IDZ                                    0005420
PS134 *                   RTN-CODE.                                      0005421
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           READ VSAM-WA                                                 DELLIDCH
                    INTO WA-APPLICATION-RECORD                          DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
PS134                                                                    0005422
PS134      IF RTN-CODE = +0                                              0005423
PS134          NEXT SENTENCE                                             0005424
PS134      ELSE                                                          0005425
PS134          GO TO 9210-READ-NEXT-WA.                                  0005426
PS134                                                                    0005427
PS134      MOVE SPACES TO APAP1-DELL-GUID-PROCESSED-FLAG                 0005428
PS134      MOVE +0 TO RTN-CODE.                                          0005429
PS134 *    CALL 'DISKUP' USING WA-APPLICATION-RECORD                     0005430
PS134 *                 WA-KEY                                           0005431
PS134 *                 WA-FILE-IDZ                                      0005432
PS134 *                 RTN-CODE.                                        0005433
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           REWRITE VSAM-WA-REC                                          DELLIDCH
                      FROM WA-APPLICATION-RECORD                        DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY TO TPSWNML-FILE-KEY                              DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE                       DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
PS134                                                                    0005434
PS134      GO TO 9210-READ-NEXT-WA.                                      0005435
PS134                                                                    0005436
PS134  9299-CLEAR-PROCESS-FLAG-EXIT.                                     0005437
PS134      EXIT.                                                         0005438
BT195                                                                    0005439
BT195  9300-READ-CV-REC  SECTION.                                        0005440
BT195      MOVE +0 TO RTN-CODE.                                          0005441
BT195 *    CALL 'DISKREAD' USING COVERAGE-RECORD-CV                      0005442
BT195 *                       KEY-N1-WA                                  0005443
BT195 *                       AP-FILE-ID                                 0005444
BT195 *                       RTN-CODE.                                  0005445
           MOVE KEY-N1-WA TO VSAM-AP-PRIM                               DELLIDCH
           READ VSAM-AP                                                 DELLIDCH
                    INTO COVERAGE-RECORD-CV                             DELLIDCH
           MOVE VSAM-AP-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE KEY-N1-WA TO TPSWNML-FILE-KEY                           DELLIDCH
           MOVE AP-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKREAD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
BT195  9399-READ-CV-REC-EXIT.                                            0005446
BT195      EXIT.                                                         0005447
BT261  9400-READ-NB-MST  SECTION.                                       DELLRETC
BT261 *    CALL 'DISKREAD' USING LIFE-MASTER-RECORD                     DELLRETC
BT261 *                   LM-POLID                                      DELLRETC
BT261 *                   NB-FILE-ID                                    DELLRETC
BT261 *                   RTN-CODE.                                     DELLRETC
           MOVE LM-POLID TO VSAM-NB-PRIM                                DELLIDCH
           READ VSAM-NB                                                 DELLIDCH
                    INTO LIFE-MASTER-RECORD                             DELLIDCH
           MOVE VSAM-NB-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE LM-POLID TO TPSWNML-FILE-KEY                            DELLIDCH
           MOVE NB-FILE-ID TO TPSWNML-FILE-ID                           DELLIDCH
           MOVE 'DISKREAD' TO TPSWNML-FUNCTION-CODE                     DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH
BT261  9499-READ-POLMST-EXIT.                                           DELLRETC
BT261      EXIT.                                                        DELLRETC
BT261                                                                   06411000
                                                                        06412000
BT371                                                                   DELLRET9
BT371  9500-WA-DISKADD   SECTION.                                       DELLRET9
BT371 *    CALL 'DISKADD' USING WA-APPLICATION-RECORD                   DELLRET9
BT371 *                  WA-KEY                                         DELLRET9
BT371 *                  WA-FILE-IDZ                                    DELLRET9
BT371 *                  RTN-CODE.                                      DELLRET9
           MOVE WA-KEY TO VSAM-WA-PRIM                                  DELLIDCH
           WRITE VSAM-WA-REC                                            DELLIDCH
                      FROM WA-APPLICATION-RECORD                        DELLIDCH
           MOVE VSAM-WA-FS TO WS-INDX-FS-99                             DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLIDCH
           MOVE WA-KEY     TO TPSWNML-FILE-KEY                          DELLIDCH
           MOVE WA-FILE-IDZ TO TPSWNML-FILE-ID                          DELLIDCH
           MOVE 'DISKADD' TO TPSWNML-FUNCTION-CODE                      DELLIDCH
           PERFORM P9999-MAP-RESP-CODE                                  DELLIDCH
           MOVE WS-INDX-FS-99 TO RTN-CODE.                              DELLIDCH

BT371  9599-WA-DISKADD-EXIT.                                            DELLRET9
BT371      EXIT.                                                        DELLRET9
BT371                                                                   DELLRET9
PB631  9600-UPDATE-INFO11 SECTION.
PB631      IF WK-STATUS = '009'
PB631         AND WASY-FINAL-STATUS = 'APPROVE' OR 'DECLINE' OR 'CANCEL'
PB631         GO TO 9699-INFO11-EXIT
PB631      END-IF.
PB631      PERFORM 9710-DISKHOLD-NB.
PB631      MOVE SPACES            TO WS-INFO11.
PB631      IF WK-STATUS = '008'
PB631         MOVE '005' TO WS-INFO11
PB631      END-IF.
PB631      IF WK-STATUS = '009'
PB631         MOVE '006' TO WS-INFO11
PB631      END-IF.
PB631      MOVE LMZ-SEG-DI-LAYOUT TO LM-SEG-DI-LAYOUT.
PB631  9600-NEXT-SEG.
PB631      CALL 'SEGGETN' USING LIFE-MASTER-RECORD
PB631                           LM-SEG-DI-LAYOUT.
PB631      IF LM-SEG-DI-RC = +0 AND LM-DI-TYPE = '11'
PB631           GO TO 9610-UPD-DI-SEG
PB631      END-IF.
PB631      IF LM-SEG-DI-RC > +0
PB631           GO TO 9620-ADD-DI-SEG
PB631      END-IF.
PB631      GO TO 9600-NEXT-SEG. 
PB631  9610-UPD-DI-SEG.
PB631      MOVE  WS-INFO11  TO LM-DI-INFO.    
PB631      PERFORM 9700-READ-REF-FILE
PB631      CALL 'SEGUPD' USING LIFE-MASTER-RECORD
PB631                          LM-SEG-DI-LAYOUT.
PB631      IF LM-SEG-DI-RC = +0
PB631           PERFORM 9720-DISKUP-NB
PB631           GO TO 9699-INFO11-EXIT.
PB631  9620-ADD-DI-SEG.
PB631      PERFORM 9700-READ-REF-FILE.
PB631      MOVE 'DI'       TO LM-SEG-DI-ID.
PB631      MOVE +220       TO LM-SEG-DI-SEQ.
PB631      MOVE '11'       TO LM-DI-TYPE.  
PB631      MOVE WS-INFO11  TO LM-DI-INFO.
PB631      CALL 'SEGADD' USING LIFE-MASTER-RECORD
PB631                          LM-SEG-DI-LAYOUT.
PB631      IF LM-SEG-DI-RC = +0
PB631         PERFORM 9720-DISKUP-NB
PB631         GO TO 9699-INFO11-EXIT
PB631      ELSE
PB631         DISPLAY 'ADD DI SEG FAILED FOR POLICY: ' LM-POLICY
PB631         DISPLAY 'BAD RETURN CODE: ' LM-SEG-DI-RC
PB631         MOVE +9620 TO ABEND-CODE
PB631         PERFORM 9900-ABEND-RTN.
PB631  9699-INFO11-EXIT.
PB631      EXIT.
PB631  9700-READ-REF-FILE SECTION.
PB631      MOVE +0      TO RE-RTN-CODE.
PB631      MOVE SPACES  TO RRK-KEY.
PB631      MOVE LM-CO   TO RRK-CO.
PB631      MOVE '96'    TO RRK-PARM.
PB631      MOVE '11'    TO RRK-TYPE.
PB631      MOVE WS-INFO11 TO RRK-KEYVAL.
PB631      MOVE RRK-KEY TO VSAM-RE-PRIM
PB631      READ VSAM-RE
PB631               INTO   REFERNCE-OPTION-RECORD
PB631      MOVE VSAM-RE-FS TO WS-INDX-FS-99
PB631      INITIALIZE TPSWNML-AREA
PB631      MOVE RRK-KEY TO TPSWNML-FILE-KEY
PB631      MOVE RE-FILE-ID TO TPSWNML-FILE-ID
PB631      MOVE 'DISKREAD' TO TPSWNML-FUNCTION-CODE
PB631      PERFORM P9999-MAP-RESP-CODE
PB631      MOVE WS-INDX-FS-99 TO RE-RTN-CODE.
PB631      IF RE-RTN-CODE > +0
PB631          GO TO 9700-EXIT.
PB631      MOVE DESCRIP-96 TO LM-DI-DESC.
PB631  9700-EXIT.
PB631      EXIT.
PB631  9710-DISKHOLD-NB SECTION.
PB631 *    CALL 'DISKHOLD' USING LIFE-MASTER-RECORD
PB631 *                   LM-POLID
PB631 *                   NB-FILE-ID
PB631 *                   RTN-CODE.
PB631      MOVE LM-POLID TO VSAM-NB-PRIM
PB631      READ VSAM-NB
PB631               INTO LIFE-MASTER-RECORD
PB631      MOVE VSAM-NB-FS TO WS-INDX-FS-99
PB631      INITIALIZE TPSWNML-AREA
PB631      MOVE LM-POLID TO TPSWNML-FILE-KEY
PB631      MOVE NB-FILE-ID TO TPSWNML-FILE-ID
PB631      MOVE 'DISKHOLD' TO TPSWNML-FUNCTION-CODE
PB631      PERFORM P9999-MAP-RESP-CODE
PB631      MOVE WS-INDX-FS-99 TO RTN-CODE.
PB631      IF RTN-CODE NOT = +0
PB631         GO TO 9719-DISKHOLD-NB-EXIT.
PB631  9719-DISKHOLD-NB-EXIT.
PB631      EXIT.
PB631  9720-DISKUP-NB SECTION.
PB631      MOVE LM-POLID TO VSAM-NB-PRIM
PB631      MOVE LIFE-MASTER-RECORD(1:6) TO LMR-VARL
PB631      REWRITE VSAM-NB-REC
PB631               FROM LIFE-MASTER-RECORD
PB631      MOVE VSAM-NB-FS TO WS-INDX-FS-99
PB631      INITIALIZE TPSWNML-AREA
PB631      MOVE LM-POLID TO TPSWNML-FILE-KEY
PB631      MOVE NB-FILE-ID TO TPSWNML-FILE-ID
PB631      MOVE 'DISKUP' TO TPSWNML-FUNCTION-CODE
PB631      PERFORM P9999-MAP-RESP-CODE
PB631      MOVE WS-INDX-FS-99 TO RTN-CODE.
PB631      IF RTN-CODE NOT = 0
PB631         GO TO 9729-UPDATE-NB-EXIT.
PB631  9729-UPDATE-NB-EXIT.
PB631       EXIT.
PB831 *
PB881  9730-SET-TASK-CLOSE SECTION.
PB881      MOVE SPACES TO TASK-RECORD.                           
PB881      MOVE WS-EENRL TO TK010-PS-PROCESS.                    
PB881      MOVE WS-CHG  TO TK010-PS-STEP.                        
PB881      PERFORM 3500-GET-WF-DESCRIPTION.                      
PB881      IF RTN-CODE > +0                                      
PB881          GO TO 4899-SET-TASK-RECORD-EXIT.                  
PB881                                                            
PB881      MOVE WS-EENRL TO TASK-PROCESS.                        
PB881      MOVE WS-CHG TO TASK-STEP.                             
PB881      MOVE SV-CO TO TASK-POLICY-CO.                         
PB881      MOVE NB452-POLICY-NUMBER TO TASK-POLICY-NUM.          
PB881      MOVE SPACES TO TASK-POLICY-RDR.                       
PB881      MOVE SAVE-EMPLOYER-ID TO TASK-GROUP-NUMBER.           
PB881      MOVE HWA-DELL-GUID TO TASK-COMMENT1-A.
AS204      MOVE 'NO CHANGE REQUIRED' TO TASK-COMMENT2-A.
AS204      MOVE SPACES TO TASK-COMMENT3-A.               
PB881      IF NB452-REMARK3 = 'Y'                                
PB881          MOVE SPACES TO TASK-COMMENT3-A                    
PB881          MOVE 'CREATE LETTER FOR PORT ' TO TASK-COMMENT3-A.
PB881      MOVE '5' TO TASK-PRIORITY.                            
PB881      MOVE SPACES TO TASK-ADDTO-DATABASE.                   
PB881      MOVE WS-C TO TASK-STATUS.                             
PB881      MOVE WS-STPS TO TASK-SENDER-OPID.                     
PB881                                                            
PB882      MOVE WK-SYSTEM-DATE TO TASK-PENDING-DATE
PB882                             TASK-CLOSED-DATE.
PB882      MOVE WK-SYSTEM-TIME TO TASK-PENDING-TIME
PB882                             TASK-CLOSED-TIME.
PB881      MOVE WK-SYSTEM-DATE TO TASK-ADDED-DATE.               
PB881                                                            
PB881      MOVE 000000000 TO TASK-ID.                            
PB881      MOVE TASK-ID TO TASK-MASTER-ID.                       
PB881      MOVE WK-SYSTEM-DATE TO TASK-MSTR-PDATE.               
PB881      MOVE HWA-IMAGE-REFID TO TASK-IMAGE-ID.                
PB881      PERFORM 4000-TASK-DISKADD.                            
PB881  9739-SET-TASK-CLOSE-EXIT.
PB881      EXIT.
       9900-ABEND-RTN SECTION.                                           0005449
           PERFORM 1200-PROCESS-END.                                     0005450
           CALL 'Z102' USING ABEND-CODE.                                 0005451
       9999-ABEND-RTN-EXIT.                                              0005452
           EXIT.                                                         0005453
                                                                         0005454
       1000-MIGR-OPEN-READ-WRITE SECTION.                               DELLSQCH
           OPEN INPUT READ1.                                            DELLSQCH
           MOVE WS-READ1-FS TO CPY-FS-CODE.                             DELLSQCH
           PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.                 DELLSQCH
           OPEN INPUT READ2.                                            DELLRT16
           MOVE WS-READ2-FS TO CPY-FS-CODE.                             DELLRT16
           PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.                 DELLRT16
           OPEN OUTPUT WRITE1.                                          DELLMNCH
           MOVE WS-WRITE1-FS TO CPY-FS-CODE.                            DELLMNCH
           PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.                 DELLMNCH
           OPEN OUTPUT WRITE2.                                          DELLMNCH
           MOVE WS-WRITE2-FS TO CPY-FS-CODE.                            DELLMNCH
           PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.                 DELLMNCH
           OPEN OUTPUT WRITE3.                                          DELLSQCH
           MOVE WS-WRITE3-FS TO CPY-FS-CODE.                            DELLSQCH
           PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.                 DELLSQCH
           OPEN OUTPUT WRITE4.                                          DELLSQCH
           MOVE WS-WRITE4-FS TO CPY-FS-CODE.                            DELLSQCH
           PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.                 DELLSQCH
           OPEN OUTPUT WRITE5.                                          DELLRET6
           MOVE WS-WRITE5-FS TO CPY-FS-CODE.                            DELLRET6
           PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.                 DELLRET6
           OPEN OUTPUT WRITE6.                                          DELLRT12
           MOVE WS-WRITE6-FS TO CPY-FS-CODE.                            DELLRT12
           PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.                 DELLRT12
           OPEN OUTPUT WRITE7.                                          DELLRT16
           MOVE WS-WRITE7-FS TO CPY-FS-CODE.                            DELLRT16
           PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.                 DELLRT16
           OPEN OUTPUT WRITE8.                                          DELLRT16
           MOVE WS-WRITE8-FS TO CPY-FS-CODE.                            DELLRT16
           PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.                 DELLRT16
DL461      OPEN OUTPUT WRITE9.
DL461      MOVE WS-WRITE9-FS TO CPY-FS-CODE. 
DL461      PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.
           OPEN OUTPUT NUSEQFL.                                         DELLMNCH
           MOVE WS-NUSEQ-FS TO CPY-FS-CODE.                             DELLMNCH
           PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.                 DELLMNCH
                                                                        DELLMNCH
       1000-MIGR-EXIT. EXIT.                                            DELLSQCH
                                                                        DELLSQCH
       8900-CHK-FILE-STATUS SECTION.                                    DELLSQCH
           COPY SEREMAP.                                                DELLSQCH
       8900-EXIT. EXIT.                                                 DELLSQCH
                                                                        DELLSQCH
       9000-CLOSE-FILES SECTION.                                        DELLSQCH
                                                                        DELLSQCH
           CLOSE READ1.                                                 DELLSQCH
           MOVE WS-READ1-FS TO CPY-FS-CODE.                             DELLSQCH
           PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.                 DELLSQCH
                                                                        DELLSQCH
           CLOSE READ2.                                                 DELLRT16
           MOVE WS-READ2-FS TO CPY-FS-CODE.                             DELLRT16
           PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.                 DELLRT16
                                                                        DELLSQCH
           CLOSE WRITE1.                                                DELLMNCH
           MOVE WS-WRITE1-FS TO CPY-FS-CODE.                            DELLMNCH
           PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.                 DELLMNCH
                                                                        DELLMNCH
           CLOSE WRITE2.                                                DELLMNCH
           MOVE WS-WRITE2-FS TO CPY-FS-CODE.                            DELLMNCH
           PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.                 DELLMNCH
                                                                        DELLSQCH
           CLOSE WRITE3.                                                DELLSQCH
           MOVE WS-WRITE3-FS TO CPY-FS-CODE.                            DELLSQCH
           PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.                 DELLSQCH
                                                                        DELLSQCH
           CLOSE WRITE4.                                                DELLSQCH
           MOVE WS-WRITE4-FS TO CPY-FS-CODE.                            DELLSQCH
           PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.                 DELLSQCH
                                                                        DELLRET6
           CLOSE WRITE5.                                                DELLRET6
           MOVE WS-WRITE5-FS TO CPY-FS-CODE.                            DELLRET6
           PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.                 DELLRET6

           CLOSE WRITE6.                                                DELLRT12
           MOVE WS-WRITE6-FS TO CPY-FS-CODE.                            DELLRT12
           PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.                 DELLRT12

           CLOSE WRITE7.                                                DELLRT16
           MOVE WS-WRITE7-FS TO CPY-FS-CODE.                            DELLRT16
           PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.                 DELLRT16

           CLOSE WRITE8.                                                DELLRT16
           MOVE WS-WRITE8-FS TO CPY-FS-CODE.                            DELLRT16
           PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.                 DELLRT16

DL461      CLOSE WRITE9.
DL461      MOVE WS-WRITE9-FS TO CPY-FS-CODE.
DL461      PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.      

           CLOSE NUSEQFL.
           MOVE WS-NUSEQ-FS TO CPY-FS-CODE.
           PERFORM 8900-CHK-FILE-STATUS THRU 8900-EXIT.

       9000-CLOSE-EXIT. EXIT.                                           DELLSQCH
                                                                        DELLRET2
       1099-MIGR-VSAM-OPEN-FILE SECTION.                                DELLIDCH
                                                                        DELLRET2
           INITIALIZE MIGR-VSAM-FS-VAR.                                 DELLIDCH
           OPEN I-O   VSAM-AP                                           DELLMNCH
           INITIALIZE TPSWNML-AREA                                      DELLVOCH
           MOVE 'AP'        TO TPSWNML-FILE-ID                          DELLVOCH
           MOVE 'OPENVSAM'  TO TPSWNML-FUNCTION-CODE                    DELLVOCH
           MOVE  VSAM-AP-FS TO WS-INDX-FS-99.                           DELLIDCH
           PERFORM P9999-MAP-RESP-CODE.                                 DELLIDCH
                                                                        DELLIDCH
           OPEN INPUT  VSAM-A3                                          DELLMNCH
           INITIALIZE TPSWNML-AREA                                      DELLVOCH
           MOVE 'A3'        TO TPSWNML-FILE-ID                          DELLVOCH
           MOVE 'OPENVSAM'  TO TPSWNML-FUNCTION-CODE                    DELLVOCH
           MOVE  VSAM-A3-FS TO WS-INDX-FS-99.                           DELLIDCH
           PERFORM P9999-MAP-RESP-CODE.                                 DELLIDCH
                                                                        DELLIDCH
           OPEN I-O   VSAM-GU                                           DELLMNCH
           INITIALIZE TPSWNML-AREA                                      DELLVOCH
           MOVE 'GU'        TO TPSWNML-FILE-ID                          DELLVOCH
           MOVE 'OPENVSAM'  TO TPSWNML-FUNCTION-CODE                    DELLVOCH
           MOVE  VSAM-GU-FS TO WS-INDX-FS-99.                           DELLIDCH
           PERFORM P9999-MAP-RESP-CODE.                                 DELLIDCH
                                                                        DELLIDCH
           OPEN INPUT VSAM-MS                                           DELLMNCH
           INITIALIZE TPSWNML-AREA                                      DELLVOCH
           MOVE 'MS'        TO TPSWNML-FILE-ID                          DELLVOCH
           MOVE 'OPENVSAM'  TO TPSWNML-FUNCTION-CODE                    DELLVOCH
           MOVE  VSAM-MS-FS TO WS-INDX-FS-99.                           DELLMNCH
           PERFORM P9999-MAP-RESP-CODE.                                 DELLIDCH
                                                                        DELLIDCH
           OPEN I-O   VSAM-NB                                           DELLMNCH
           INITIALIZE TPSWNML-AREA                                      DELLVOCH
           MOVE 'NB'        TO TPSWNML-FILE-ID                          DELLVOCH
           MOVE 'OPENVSAM'  TO TPSWNML-FUNCTION-CODE                    DELLVOCH
           MOVE  VSAM-NB-FS TO WS-INDX-FS-99.                           DELLIDCH
           PERFORM P9999-MAP-RESP-CODE.                                 DELLIDCH
                                                                        DELLIDCH
           OPEN INPUT VSAM-PO                                           DELLMNCH
           INITIALIZE TPSWNML-AREA                                      DELLVOCH
           MOVE 'PO'        TO TPSWNML-FILE-ID                          DELLVOCH
           MOVE 'OPENVSAM'  TO TPSWNML-FUNCTION-CODE                    DELLVOCH
           MOVE  VSAM-P2-FS TO WS-INDX-FS-99.                           DELLIDCH
           PERFORM P9999-MAP-RESP-CODE.                                 DELLIDCH
                                                                        DELLIDCH
           OPEN INPUT VSAM-P3                                           DELLIDCH
           INITIALIZE TPSWNML-AREA.                                     DELLIDCH
           MOVE  'P3'       TO TPSWNML-FILE-ID.                         DELLIDCH
           MOVE  'OPENVSAM' TO TPSWNML-FUNCTION-CODE.                   DELLIDCH
           MOVE  VSAM-P3-FS TO WS-INDX-FS-99.                           DELLIDCH
           PERFORM P9999-MAP-RESP-CODE.                                 DELLIDCH

           OPEN I-O   VSAM-RE                                           DELLMNCH
           INITIALIZE TPSWNML-AREA                                      DELLVOCH
           MOVE 'RE'        TO TPSWNML-FILE-ID                          DELLVOCH
           MOVE 'OPENVSAM'  TO TPSWNML-FUNCTION-CODE                    DELLVOCH
           MOVE  VSAM-RE-FS TO WS-INDX-FS-99.                           DELLIDCH
           PERFORM P9999-MAP-RESP-CODE.                                 DELLIDCH
                                                                        DELLMNCH
           OPEN I-O   VSAM-WA                                           DELLMNCH
           INITIALIZE TPSWNML-AREA                                      DELLVOCH
           MOVE 'WA'        TO TPSWNML-FILE-ID                          DELLVOCH
           MOVE 'OPENVSAM'  TO TPSWNML-FUNCTION-CODE                    DELLVOCH
           MOVE  VSAM-WA-FS TO WS-INDX-FS-99.                           DELLIDCH
           PERFORM P9999-MAP-RESP-CODE.                                 DELLIDCH
           OPEN I-O   VSAM-W8                                           DELLMNCH
           INITIALIZE TPSWNML-AREA                                      DELLVOCH
           MOVE 'W8'        TO TPSWNML-FILE-ID                          DELLVOCH
           MOVE 'OPENVSAM'  TO TPSWNML-FUNCTION-CODE                    DELLVOCH
           MOVE  VSAM-W8-FS TO WS-INDX-FS-99.                           DELLIDCH
           PERFORM P9999-MAP-RESP-CODE.                                 DELLIDCH
                                                                        DELLIDCH
           OPEN INPUT VSAM-WF                                           DELLIDCH
           INITIALIZE TPSWNML-AREA                                      DELLVOCH
           MOVE 'WF'        TO TPSWNML-FILE-ID                          DELLVOCH
           MOVE 'OPENVSAM'  TO TPSWNML-FUNCTION-CODE                    DELLVOCH
           MOVE  VSAM-WF-FS TO WS-INDX-FS-99.                           DELLIDCH
           PERFORM P9999-MAP-RESP-CODE.                                 DELLIDCH
                                                                        DELLIDCH
           OPEN INPUT VSAM-AC                                           DELMODID
           INITIALIZE TPSWNML-AREA                                      DELLVOCH
           MOVE 'AC'        TO TPSWNML-FILE-ID                          DELLVOCH
           MOVE 'OPENVSAM'  TO TPSWNML-FUNCTION-CODE                    DELLVOCH
           MOVE  VSAM-AC-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           OPEN INPUT VSAM-AG                                           DELMODID
           INITIALIZE TPSWNML-AREA                                      DELLVOCH
           MOVE 'AG'        TO TPSWNML-FILE-ID                          DELLVOCH
           MOVE 'OPENVSAM'  TO TPSWNML-FUNCTION-CODE                    DELLVOCH
           MOVE  VSAM-AG-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           OPEN INPUT VSAM-CS                                           DELMODID
           INITIALIZE TPSWNML-AREA                                      DELLVOCH
           MOVE 'CS'        TO TPSWNML-FILE-ID                          DELLVOCH
           MOVE 'OPENVSAM'  TO TPSWNML-FUNCTION-CODE                    DELLVOCH
           MOVE  VSAM-CS-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           OPEN INPUT VSAM-EM                                           DELMODID
           INITIALIZE TPSWNML-AREA                                      DELLVOCH
           MOVE 'EM'        TO TPSWNML-FILE-ID                          DELLVOCH
           MOVE 'OPENVSAM'  TO TPSWNML-FUNCTION-CODE                    DELLVOCH
           MOVE  VSAM-EM-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           OPEN INPUT VSAM-GS                                           DELMODID
           INITIALIZE TPSWNML-AREA                                      DELLVOCH
           MOVE 'GS'        TO TPSWNML-FILE-ID                          DELLVOCH
           MOVE 'OPENVSAM'  TO TPSWNML-FUNCTION-CODE                    DELLVOCH
           MOVE  VSAM-GS-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           OPEN INPUT VSAM-HD                                           DELMODID
           INITIALIZE TPSWNML-AREA                                      DELLVOCH
           MOVE 'HD'        TO TPSWNML-FILE-ID                          DELLVOCH
           MOVE 'OPENVSAM'  TO TPSWNML-FUNCTION-CODE                    DELLVOCH
           MOVE  VSAM-HD-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           OPEN INPUT VSAM-NT                                           DELMODID
           INITIALIZE TPSWNML-AREA                                      DELLVOCH
           MOVE 'NT'        TO TPSWNML-FILE-ID                          DELLVOCH
           MOVE 'OPENVSAM'  TO TPSWNML-FUNCTION-CODE                    DELLVOCH
           MOVE  VSAM-NT-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           OPEN I-O VSAM-NU                                             DELMODID
           INITIALIZE TPSWNML-AREA                                      DELLVOCH
           MOVE 'NU'        TO TPSWNML-FILE-ID                          DELLVOCH
           MOVE 'OPENVSAM'  TO TPSWNML-FUNCTION-CODE                    DELLVOCH
           MOVE  VSAM-NU-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           OPEN INPUT VSAM-PL                                           DELMODID
           INITIALIZE TPSWNML-AREA                                      DELLVOCH
           MOVE 'PL'        TO TPSWNML-FILE-ID                          DELLVOCH
           MOVE 'OPENVSAM'  TO TPSWNML-FUNCTION-CODE                    DELLVOCH
           MOVE  VSAM-PL-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           OPEN I-O VSAM-PM                                             DELMODID
           INITIALIZE TPSWNML-AREA                                      DELLVOCH
           MOVE 'PM'        TO TPSWNML-FILE-ID                          DELLVOCH
           MOVE 'OPENVSAM'  TO TPSWNML-FUNCTION-CODE                    DELLVOCH
           MOVE  VSAM-PM-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           OPEN INPUT VSAM-RT                                           DELMODID
           INITIALIZE TPSWNML-AREA                                      DELLVOCH
           MOVE 'RT'        TO TPSWNML-FILE-ID                          DELLVOCH
           MOVE 'OPENVSAM'  TO TPSWNML-FUNCTION-CODE                    DELLVOCH
           MOVE  VSAM-RT-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           OPEN INPUT VSAM-RV                                           DELMODID
           INITIALIZE TPSWNML-AREA                                      DELLVOCH
           MOVE 'RV'        TO TPSWNML-FILE-ID                          DELLVOCH
           MOVE 'OPENVSAM'  TO TPSWNML-FUNCTION-CODE                    DELLVOCH
           MOVE  VSAM-RV-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           OPEN INPUT VSAM-SR                                           DELMODID
           INITIALIZE TPSWNML-AREA                                      DELLVOCH
           MOVE 'SR'        TO TPSWNML-FILE-ID                          DELLVOCH
           MOVE 'OPENVSAM'  TO TPSWNML-FUNCTION-CODE                    DELLVOCH
           MOVE  VSAM-SR-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           OPEN INPUT VSAM-SS                                           DELMODID
           INITIALIZE TPSWNML-AREA                                      DELLVOCH
           MOVE 'SS'        TO TPSWNML-FILE-ID                          DELLVOCH
           MOVE 'OPENVSAM'  TO TPSWNML-FUNCTION-CODE                    DELLVOCH
           MOVE  VSAM-SS-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           OPEN INPUT VSAM-TC                                           DELMODID
           INITIALIZE TPSWNML-AREA                                      DELLVOCH
           MOVE 'TC'        TO TPSWNML-FILE-ID                          DELLVOCH
           MOVE 'OPENVSAM'  TO TPSWNML-FUNCTION-CODE                    DELLVOCH
           MOVE  VSAM-TC-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           OPEN INPUT VSAM-TH                                           DELMODID
           INITIALIZE TPSWNML-AREA                                      DELLVOCH
           MOVE 'TH'        TO TPSWNML-FILE-ID                          DELLVOCH
           MOVE 'OPENVSAM'  TO TPSWNML-FUNCTION-CODE                    DELLVOCH
           MOVE  VSAM-TH-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           OPEN INPUT VSAM-TK                                           DELMODID
           INITIALIZE TPSWNML-AREA                                      DELLVOCH
           MOVE 'TK'        TO TPSWNML-FILE-ID                          DELLVOCH
           MOVE 'OPENVSAM'  TO TPSWNML-FUNCTION-CODE                    DELLVOCH
           MOVE  VSAM-TK-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
NK771      OPEN INPUT VSAM-FS                                           DELMODID
NK771      INITIALIZE TPSWNML-AREA                                      DELLVOCH
NK771      MOVE 'FS'        TO TPSWNML-FILE-ID                          DELLVOCH
NK771      MOVE 'OPENVSAM'  TO TPSWNML-FUNCTION-CODE                    DELLVOCH
NK771      MOVE  VSAM-FS-FS TO WS-INDX-FS-99.                           DELMODID
NK771      PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
AS991      OPEN INPUT VSAM-PC                                           DELMODID
AS991      INITIALIZE TPSWNML-AREA                                      DELLVOCH
AS991      MOVE 'PC'        TO TPSWNML-FILE-ID                          DELLVOCH
AS991      MOVE 'OPENVSAM'  TO TPSWNML-FUNCTION-CODE                    DELLVOCH
AS991      MOVE  VSAM-PC-FS TO WS-INDX-FS-99.                           DELMODID
AS991      PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
       1099-MIGR-EXIT. EXIT.                                            DELLIDCH
                                                                        DELLIDCH
                                                                        DELLIDCH
       9099-CLOSE-VSAM-FILE SECTION.                                    DELLIDCH
           CLOSE VSAM-AP.                                               DELLIDCH
           MOVE  VSAM-AP-FS TO WS-INDX-FS-99.                           DELLIDCH
           PERFORM P9999-MAP-RESP-CODE.                                 DELLIDCH
                                                                        DELLIDCH
           CLOSE VSAM-A3.                                               DELLIDCH
           MOVE  VSAM-A3-FS TO WS-INDX-FS-99.                           DELLIDCH
           PERFORM P9999-MAP-RESP-CODE.                                 DELLIDCH
                                                                        DELLIDCH
           CLOSE VSAM-GU.                                               DELLIDCH
           MOVE  VSAM-GU-FS TO WS-INDX-FS-99.                           DELLIDCH
           PERFORM P9999-MAP-RESP-CODE.                                 DELLIDCH
                                                                        DELLIDCH
           CLOSE VSAM-MS.                                               DELLMNCH
           MOVE  VSAM-MS-FS TO WS-INDX-FS-99.                           DELLMNCH
           PERFORM P9999-MAP-RESP-CODE.                                 DELLIDCH
                                                                        DELLIDCH
           CLOSE VSAM-NB.                                               DELLIDCH
           MOVE  VSAM-NB-FS TO WS-INDX-FS-99.                           DELLIDCH
           PERFORM P9999-MAP-RESP-CODE.                                 DELLIDCH
                                                                        DELLIDCH
           CLOSE VSAM-PO.                                               DELLMNCH
           MOVE  VSAM-P2-FS TO WS-INDX-FS-99.                           DELLIDCH
           PERFORM P9999-MAP-RESP-CODE.                                 DELLIDCH
                                                                        DELLIDCH
           CLOSE VSAM-P3.                                               DELLIDCH
           MOVE  VSAM-P3-FS TO WS-INDX-FS-99.                           DELLIDCH
           PERFORM P9999-MAP-RESP-CODE.                                 DELLIDCH

           CLOSE VSAM-RE.                                               DELLIDCH
           MOVE  VSAM-RE-FS TO WS-INDX-FS-99.                           DELLIDCH
           PERFORM P9999-MAP-RESP-CODE.                                 DELLIDCH
                                                                        DELLIDCH
           CLOSE VSAM-WA.                                               DELLIDCH
           MOVE  VSAM-WA-FS TO WS-INDX-FS-99.                           DELLIDCH
           PERFORM P9999-MAP-RESP-CODE.                                 DELLIDCH
           CLOSE VSAM-W8.                                               DELLIDCH
           MOVE  VSAM-W8-FS TO WS-INDX-FS-99.                           DELLIDCH
           PERFORM P9999-MAP-RESP-CODE.                                 DELLIDCH
                                                                        DELLIDCH
           CLOSE VSAM-WF.                                               DELLIDCH
           MOVE  VSAM-WF-FS TO WS-INDX-FS-99.                           DELLIDCH
           PERFORM P9999-MAP-RESP-CODE.                                 DELLIDCH
                                                                        DELLIDCH
                                                                        DELMODID
           CLOSE VSAM-AC.                                               DELMODID
           MOVE  VSAM-AC-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           CLOSE VSAM-AG.                                               DELMODID
           MOVE  VSAM-AG-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           CLOSE VSAM-CS.                                               DELMODID
           MOVE  VSAM-CS-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           CLOSE VSAM-EM.                                               DELMODID
           MOVE  VSAM-EM-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           CLOSE VSAM-GS.                                               DELMODID
           MOVE  VSAM-GS-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           CLOSE VSAM-HD.                                               DELMODID
           MOVE  VSAM-HD-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           CLOSE VSAM-NT.                                               DELMODID
           MOVE  VSAM-NT-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           CLOSE VSAM-NU.                                               DELMODID
           MOVE  VSAM-NU-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           CLOSE VSAM-PL.                                               DELMODID
           MOVE  VSAM-PL-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           CLOSE VSAM-PM.                                               DELMODID
           MOVE  VSAM-PM-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           CLOSE VSAM-RT.                                               DELMODID
           MOVE  VSAM-RT-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           CLOSE VSAM-RV.                                               DELMODID
           MOVE  VSAM-RV-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           CLOSE VSAM-SR.                                               DELMODID
           MOVE  VSAM-SR-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           CLOSE VSAM-SS.                                               DELMODID
           MOVE  VSAM-SS-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           CLOSE VSAM-TC.                                               DELMODID
           MOVE  VSAM-TC-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           CLOSE VSAM-TH.                                               DELMODID
           MOVE  VSAM-TH-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
           CLOSE VSAM-TK.                                               DELMODID
           MOVE  VSAM-TK-FS TO WS-INDX-FS-99.                           DELMODID
           PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
NK771      CLOSE VSAM-FS.                                               DELMODID
NK771      MOVE  VSAM-FS-FS TO WS-INDX-FS-99.                           DELMODID
NK771      PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
AS991      CLOSE VSAM-PC.                                               DELMODID
AS991      MOVE  VSAM-PC-FS TO WS-INDX-FS-99.                           DELMODID
AS991      PERFORM P9999-MAP-RESP-CODE.                                 DELMODID
                                                                        DELMODID
       9099-CLOSE-EXIT. EXIT.                                           DELLIDCH
       COPY RESPMAP.                                                    DELLIDCH
