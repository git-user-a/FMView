ZZFMVIEW ; HOIFO/WAA/AA - GUI INTERFACE ROUTINE ; 23/08/2012 11:54
 ;;1.40;ZZFMVIEW;;May 31, 2025@16:45;;
 ; based on MD* routines ***
 ;
RPC(RESULT,OPTION,ARRAY) ; RPC processing entry point
 ; 
 I $$VALIDOPT(OPTION)=0 S RESULT(0)="-1^Option "_OPTION_" is not supported by this RPC" Q
 D CLEAN^DILF
 S RESULT=$NA(^TMP("ZZFM00",$J)) K @RESULT
 I '($T(@OPTION)]"") S RESULT(0)="-1^Option '"_OPTION_"' not found in routine '"_$T(+0)_"'." Q
 D @OPTION
 I '$D(RESULT(0)) S RESULT(0)="-1^Unspecified Error"
 K ^TMP("ZZFM00",$J)
 D CLEAN^DILF
 Q
 ;
VALIDOPT(OPTNAME) ;
 ; option name validator
 N G,OUT,FOUND S G="",OUT=0,FOUND=0
 F I=1:1 D  Q:OUT=1
 . S G=$T(HELP+I)
 . S:G["VALID OPTIONS LIST START" FOUND=1
 . S:G["VALID OPTIONS LIST END" OUT=1,G=""
 . Q:'FOUND
 . S:$E(G,1,3)=" ;;" G=$P(G,";;",2),G=$P(G," ")
 . S:G="" OUT=1
 . I (G'="")&(OPTNAME=G) S OUT=1
 Q (G'="")&(OPTNAME=G)
 ;
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; GENERAL OPTIONS
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;
FINDCTX ;
 ; implements ^DIC(19,:,"RPC","B",+RPCIEN,*)
 ;
 N FINDRPC
 S FINDRPC=ARRAY(0)
 I FINDRPC'=+FINDRPC S FINDRPC(1)=$O(^XWB(8994,"B",FINDRPC,""))
 E  S FINDRPC(1)=FINDRPC
 Q:'FINDRPC(1)
 N D0,D1,K S K=0
 S D0=0 F  S D0=$O(^DIC(19,D0)) Q:D0'=+D0  DO
 . S D1=0 F  S D1=$O(^DIC(19,D0,"RPC","B",FINDRPC(1),D1)) Q:D1=""  DO
 . . S K=K+1
 . . S RESULT(K)=$P($G(^DIC(19,D0,0)),U)_U_D0
 S RESULT(0)=K
 QUIT
 ;
NULL ;
 S RESULT(0)="0^NULL"
 Q
 ; 
VERSION ;
 S RESULT(0)="1",RESULT(1)=$P($T(+2),";",3)
 Q
 ; 
ECHO ;
 N G,OUT S G="",OUT=0
 F I=1:1 D  Q:OUT=1
 . S G=$O(ARRAY(G))
 . S:G="" OUT=1
 . S:G'="" RESULT(G)=ARRAY(G)
 Q
 ;
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; OPTIONS COMMON FOR FMVIEW AND FMCOMPARE
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;
OS ;
 ; to call from M: 
 ; DO RPC^ZZFMVIEW(.RESULT,"OS") ; - must use RESULT as the name!
 ; IF +$PIECE($GET(RESULT(0)),"^",2)=19 DO
 ; .some code for GT.M usage
 ; IF +$PIECE($GET(RESULT(0)),"^",2)=18 DO
 ; .some code for Cache usage
 I $D(^%ZOSF("OS")) S RESULT(0)=$G(^%ZOSF("OS"))
 E  S RESULT(0)="Unknown"
 Q
 ;
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; ROUTINES
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
REXISTS ; Verifies if the routine exists
 ;
 S X=ARRAY(0) X ^%ZOSF("TEST") I '$T S RESULT(0)="-1^Routine "_ARRAY(0)_" not found" Q
 E  S RESULT(0)="0^"_ARRAY(0)_" found"
 Q
 ;  
RSOURCE ; Returns Routine Source
 ; based on MD* routines
 N DIF,X,XCNP,ZZNAME
 S ZZNAME=ARRAY(0)
 S X=ZZNAME X ^%ZOSF("TEST") I '$T S RESULT(0)="-1^Routine "_ARRAY(0)_" not found" Q
 K ^TMP("ZZAAED",$J)
 S XCNP=0,DIF="^TMP(""ZZAAED"",$J,",X=ZZNAME
 X ^%ZOSF("LOAD")
 S RESULT(0)=XCNP-1
 F X=1:1:RESULT(0)  S RESULT(X)=^TMP("ZZAAED",$J,X,0)
 I '$D(RESULT) S RESULT(0)="-1^Unspecified Error"
 E  S RESULT(0)=RESULT(0)
 Q
 ;
RLIST ;Modifcation-adding GTm to RLIST 01062016 jeb update 20220618 djw
 ; Returns list of routines from a given starting point to and end range of x
 I $G(^%ZOSF("OS"))["GTM"!(+$P($G(^%ZOSF("OS")),"^",2)=19) D  Q
 . N X,I,cnt,%ZE,%ZR,ctrapd,delim,exc,from,k,last,mtch,out,r,rd,RTN,add,beg,end,i,pct,scwc
 . F I=1:1:5 S X=$T(SRC+I^%RSEL) X X
 . D init^%RSEL
 . k stack s mtch="__" d start^%RSEL(0)
 . S %ZR=ARRAY(0) D work^%RSEL S ARRAY(0)=$P(ARRAY(0),"*")
 . K RESULT S RTN="",cnt=0
 . F  Q:cnt>=$G(ARRAY(1),9999)  S RTN=$O(%ZR(RTN)) Q:RTN=""!(""'=$P(RTN,ARRAY(0)))  D
 . . I '$$HAS(RTN,$G(ARRAY(2))) Quit 
 . . S cnt=cnt+1,RESULT(cnt)=RTN_"^"_%ZR(RTN)_RTN_".m"
 . S RESULT(0)=cnt
 . K %ZR,^%RSET($j)
 ;
 S ZZAAR="F  S X=$O(^$ROUTINE(X)) Q:($L(X)=0)!(""'=$P(X,Z))  S CNT=CNT+1,RESULT(CNT)=X I Y>0 Q:Y=CNT"
 N X,Y,CNT,Z
 S CNT=0,X=ARRAY(0),Z=ARRAY(0)
 S:X["*" X=$P(X,"*")
 S X=$O(^$R(ARRAY(0)),-1),Y=ARRAY(1)
 X ZZAAR
 S RESULT(0)=CNT
 Q
HAS(ROU,TEXT) ;
 I $G(ROU)=""!($G(TEXT)="") Q +"1true"
 N LN S LN(0)=+"0false" 
 F LN=1:1 Q:$T(+LN^@ROU)=""  I $T(+LN^@ROU)[TEXT S LN(0)=+"1true" Q
 Q LN(0)
 ; 
RCHKSUM ; Returns routine list with checksums based on provided target. 
 N I,J
 S J=ARRAY(0)
 F I=1:1:ARRAY(0) D
 . S X=ARRAY(I) 
 . X ^%ZOSF("TEST") 
 . I $T X ^%ZOSF("RSUM") S RESULT(I)=X_"^"_Y_"^"_$$LOAD2L(X)
 . I '$T S RESULT(I)=X_"^?"
 S RESULT(0)=ARRAY(0)_"^rtName~rtChecksum~rtLine~rtLine"
 Q
LOAD2L(X)  ;Load routine first lines
 N DIF,XCNP,R K ^TMP($J)
 S DIF="^TMP($J,",XCNP=0,R="" X ^%ZOSF("LOAD")
 I $D(R) S R=$G(^TMP($J,1,0))_"~"_$G(^TMP($J,2,0))
 K ^TMP($J)
 Q R 
 ;
RCHKSUM1(NAME) ; Returns routine checksum 
 N I,J,R
 S J=ARRAY(0)
 S X=NAME
 X ^%ZOSF("TEST") 
 I $T X ^%ZOSF("RSUM") S R=X_"^"_Y_"^"_$$LOAD2L(X)
 I '$T S R=X_"^?"
 Q R
 ;
RCHKSUM2 ; updated version 
 D RLIST
 N I,NAME
 F I=1:1:RESULT(0) D
 . S NAME=$P(RESULT(I),U,1)
 . S RESULT(I)=$$RCHKSUM1(NAME)
 Q
 ;
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; GLOBALS LISTER
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ISNEEDED(ANODE,ANODE0) ;
 ;;ANODE=Current Global refeence (Example: ^DIC(2,0,"GL')
 ;;ANODE0=Required Prefix (Example: ^DIC(2,0))
 N RC,LL,SS S RC=0,LL=$L(ANODE0),SS=$E(ANODE,LL,LL)
 S:($E(ANODE0,1,LL-1)=$E(ANODE,1,LL-1))&((SS=",")!(SS=")")) RC=1
 Q RC
 ;
%ISNEEDED(CURRENT,WANTED,PATTERN) ;
 ;;CURRENT=Current Global refeence (Example: ^DIC(2,0,"GL')
 ;;WANTED=Required Prefix (Example: ^DIC(2,0))
 S RESULT(201)="%ISNEEDE",RESULT(201)="CURRENT="_CURRENT,RESULT(202)="WANTED="_WANTED,RESULT(203)="PATTERN="_PATTERN,RESULT(204)=""
 N RC,LL,SS S RC=0,LL=$L(WANTED),SS=$E(CURRENT,LL,LL)
 I ($E(WANTED,1,LL-1)=$E(CURRENT,1,LL-1))&((SS=",")!(SS=")")) RC=1 Q RC
 I CURRENT?@PATTERN S RC=1
 Q RC
 ;
LISTDD ;
 N I,FOUND,G,NODE,J ;,LL,LLL,SS
 S I=0,FOUND=0,J=1
 S NODE=ARRAY(0)
 S G=$D(@NODE),RESULT(0)="-1^Global "_NODE_" Not found"
 I G#10=1 D SHOW
 F I=1:1:ARRAY(1) S NODE=$Q(@NODE) Q:(NODE="")!($$ISNEEDED(NODE,ARRAY(0))=0)  D
 . S J=J+1,RESULT(J)=J-1_"|"_NODE_"|"_@NODE
 S RESULT(0)=I_"|"_NODE
 Q
 ; 
LISTMTCH ;
 ;from a given starting point (ARRAY(0))
 ; count of nodes to produce ARRAY(1)
 N I,FOUND,G,NODE,J,JJ,PREFIX
 S I=0,FOUND=0,J=0,JJ=ARRAY(1)
 ; ^DIC(13,0)
 S NODE=$P(ARRAY(0),"*") S X="W "_NODE,PREFIX=$E(NODE,1,$L(NODE)-1) D ^DIM I $D(X) G LISTMT0
 ;^DIC(11,*) ; ARRAY(0)
 S RESULT(0)="-1^Global "_NODE_" Not found!!!!!!!" QUIT
 ;
LISTMT0 ;
 S RESULT(100)=X
 S PATTERN="" F I=1:1:$L(ARRAY(0),"*") S PATTERN=PATTERN_"1"_$$Q^DIQGU($P(ARRAY(0),"*",I))_".E"
 I $E(AR0RAY(0),$L(ARRAY(0)))="*",$E(PATTERN,$L(PATTERN)-1,$L(PATTERN))=".E" S $E(PATTERN,$L(PATTERN)-1,$L(PATTERN))=""
 S RESULT(101)="PATTERN="_PATTERN,RESULT(102)="NODE="_NODE,RESULT(103)="PREFIX="_PREFIX
 S G=$D(@NODE),RESULT(0)="-1^Global "_NODE_" Not found"
 F I=1:1 S NODE=$Q(@NODE) Q:(NODE="")!(I>JJ) I $$%ISNEEDED(NODE,PREFIX,PATTERN)  D  ;SHOW
 . S J=J+1,RESULT(J)=J_"|"_NODE_"|"_@NODE
 S RESULT(0)=I_"|"_NODE
 Q
TESTMTCH ;
 S ARRAY(0)="^DIC(13)",ARRAY(1)=12
 D LISTMTCH ZWR RESULT(*)
 Q
 ;
LISTGLBE ;
 ;from a given starting point (ARRAY(0)) to and end range of x (ARRAY(1))
 N I,FOUND,G,NODE,J,JJ,K
 S I=0,FOUND=0,J=0,JJ=ARRAY(1)
 S NODE=ARRAY(0)
 S G=$D(@NODE),RESULT(0)="-1^Global "_NODE_" Not found"
 Q:G=0
 F I=1:1:JJ S NODE=$Q(@NODE) Q:NODE=""  D  ;SHOW
 . S G=@NODE I G?.E1C.E F K=0:1:31,127:1:255 S G=$TR(G,$C(K),"")
 . S J=J+1,RESULT(J)=J_"|"_NODE_"|"_G
 S RESULT(0)=I_"|"_NODE
 Q
 ;
LISTGLBL ;
 ;from a given starting point (ARRAY(0)) to and end range of x (ARRAY(1))
 N I,FOUND,G,NODE,J,JJ,K,L,M,MM
 S I=0,FOUND=0,J=0,JJ=ARRAY(1)
 S NODE=ARRAY(0)
 S G=$D(@NODE),RESULT(0)="-1^Global "_NODE_" Not found"
 Q:G=0
 F I=1:1:JJ S NODE=$Q(@NODE) Q:NODE=""  D  ;SHOW
 . S (G,MM)=@NODE I G?.E1C.E D 
 .. S MM="" F L=1:1:$L(G) D  S MM=MM_M ;  
 ... S M=$A(G,L) F K=0:1:31,127:1:255 I K=M S M="$C("_K_")" Q 
 . S J=J+1,RESULT(J)=J_"|"_NODE_"|"_MM
 S RESULT(0)=I_"|"_NODE
 Q
 ;
SHOW ; Local. Not used as an OPTION
 S J=J+1,RESULT(J)=J-1_"|"_NODE_"|"_@NODE,FOUND=1
 Q
 ; 
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; FileMan Files
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;
FMFNAME ;
 ;File global name and header by file number
 N FNAME,GNAME
 S RESULT(0)="-1^File "_ARRAY(0)_" Not found"
 Q:$$VFILE^DILFD(ARRAY(0))=0
 S GNAME=$G(^DIC(ARRAY(0),0,"GL"))
 Q:GNAME=""
 S RESULT(0)=2,RESULT(1)=GNAME
 S:$D(@($$ROOT^DILFD(ARRAY(0),,0)_"0)"))#10'=0 RESULT(2)=@($$ROOT^DILFD(ARRAY(0),,0)_"0)")
 Q
 ;
FFCHAR(FNUM,FFNUM,CHAR) ; 
 ; internal. Used by FMFLDDEF. Field Char by File (FNUM) and Field (FFNUM)
 N FFC  S FFC=""
 S:$D(^DD(FNUM,FFNUM,CHAR))#10=1 FFC=FNUM_"^"_FFNUM_"^"_CHAR_"^"_^DD(FNUM,FFNUM,CHAR)
 Q FFC
 ;
FMFLDDEF ; Local, not an OPTION name
 ; internal. Used by FMFIELDS. Field FFNUM Characteristics for file FNUM
 F I=0,".1",1,2,3,4,5,7.5,8,9,9.01,9.02,9.03,9.04,9.05,9.06,9.07,9.08,9.09,10,11,10,12.1,20,21,22,23 D
 .S FC=$$FFCHAR(FNUM,FFNUM,I)  S:FC'="" IND=IND+1,RESULT(IND)=FC
 F I="AUDIT","AX","DEL","DT","LATGO" D
 .S FC=$$FFCHAR(FNUM,FFNUM,I)  S:FC'="" IND=IND+1,RESULT(IND)=FC
 Q
 ;
FMFLDDE2 ; One field characteristics
 ;N I,IND,AFILE,AFIELD S I="",IND=0,AFILE=ARRAY(0),AFIELD=ARRAY(1)
 ;F  S I=$O(^DD(AFILE,AFIELD,I))  Q:'+I  D 
 ;. S FC=$$FFCHAR(AFILE,AFIELD,I)  S:FC'="" IND=IND+1,RESULT(IND)=FC
 ;S RESULT(0)=IND
 ;Q
 N I,FC,IND S IND=0
 S FNUM=ARRAY(0),FFNUM=ARRAY(1)
 D FMFLDDEF
 S RESULT(0)=IND
 Q
 ;
FLDDEF(FF,FLD) ; local 
 N G,RSLT S G="0",RSLT=""
 F  S G=$O(^DD(FF,FLD,G))  Q:'+G  D FFCHAR(FF,FLD,G)
 Q
 ; 
FMFIELDS ;
 ; Characteristics of all Fields of the FileMan file FNUM
 N G,FC,I,IND S G="",IND=0,FNUM=ARRAY(0)
 F  S G=$O(^DD(FNUM,G))  Q:G=""  S FFNUM=G D FMFLDDEF
 ;F  S G=$O(^DD(FNUM,G))  Q:G=""  D FMFLDDE2(FNUM,G)
 S RESULT(0)=IND
 Q
 ;
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; OPTIONS FOR FMVIEW ONLY
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;
 ;
ISMULT(FN,SBFNUM,FLD) ;
 ; internal checks if the FNUM field FLDNUM is multiple
 N RZ S RZ=-1
 S:$D(^DD(FN,"SB",SBFNUM,FLD))#10=1 RZ=1
 Q RZ_"^ "_($D(^DD(FN,"SB",SBFNUM,FLD))#10)_"  $D(^DD("_FN_",""SB"","_SBFNUM_","_FLD_"))="_$D(^DD(FN,"SB",SBFNUM,FLD))
 ;    
FMFFLDS1 ;
 ; Fields of the FileMan file
 N FN,I S FN=ARRAY(0),I=0
 D FMFFLDS2(FN)
 Q
 ;
FMFFLDS2(FNUM) ;
 ; internal browses DD for FNUM fields definitions (including multiple)
 N G,RSLT S G=0,RSLT=""
 F  S G=$O(^DD(FNUM,G))  Q:'+G  D
 . S:$D(^DD(FNUM,G,0))#10=1 RSLT=$P(^DD(FNUM,G,0),"^",2)
 . S I=I+1,RESULT(I)=I_"^"_FNUM_"^"_G_"^"_^DD(FNUM,G,0)_"^ ---"_(+RSLT)_" IS MULT = "_$$ISMULT(FNUM,+RSLT,G)
 . I +RSLT&(+$$ISMULT(FNUM,+RSLT,G)=1) D FMFFLDS2(+RSLT)
 S RESULT(0)=I
 Q
 ;
FMFFLDS3(FNUM) ;
 ; internal browses DD for FNUM fields definitions (including multiple)
 N G,RSLT S G="",RSLT=""
 F  S G=$O(^DD(FNUM,0,G))  Q:'+G  D
 . S:$D(^DD(FNUM,0,G))'=0 RSLT=^DD(FNUM,0,G)
 . S I=I+1,RESULT(I)=I_"^"_FNUM_"^0^"_G_"^"_^DD(FNUM,0,G) ;_"^ ---"_(+RSLT)_" IS MULT = "_$$ISMULT(FNUM,+RSLT,G)
 ;. I +RSLT&(+$$ISMULT(FNUM,+RSLT,G)=1) D FMFFLDS3(+RSLT)
 S RESULT(0)=I
 Q
 ;
FMFFLDSA ;
 ; Fields of the FileMan file
 N FN,I S FN=ARRAY(0),I=0
 D FMFFLDS3(FN)
 Q
 ;
FMFFLDS ;
 ; Fields of the FileMan file
 N I,G S G="",I=1
 F  S G=$O(^DD(ARRAY(0),G))  Q:G=""  S I=I+1  S RESULT(I)=G
 S RESULT(0)=I
 Q
 ;
FDICCHAR(FNUM,CHAR) ;
 ; internal function used by FCHARS option
 N FDC  S FDC=""
 S:$D(^DIC(FNUM,0,CHAR))#10=1 FDC=$J(CHAR,10)_" : "_^DIC(FNUM,0,CHAR)
 Q FDC
 ;
FCHAR(FNUM,CHAR) ;
 ; internal function used by FCHARS option
 N FC  S FC=""
 S:$D(^DD(FNUM,0,CHAR))#10=1 FC=$J(CHAR,10)_" : "_^DD(FNUM,0,CHAR)
 Q FC
 ;
FMFCHRS ;
 ; FileMan FIle characteristics
 N F,P1,I
 S P1=ARRAY(0),I=0
 S:$D(^DIC(P1,0))'=0 RESULT(I)="0^FILE "_P1_" CHARS",I=I+1
 ;
 F J="ACT","DDA","DIC","SCR","VR","VRPK","VRRV" D
 . S F=$$FCHAR(P1,J)  S:F'="" RESULT(I)=F,I=I+1
 ;
 S:$D(^DD(P1,0,"ID","WRITE"))#10=1 RESULT(I)=^DD(FNUM,0,"ID","WRITE"),I=I+1
 ;
 F J="GL","AUDIT","DD","DEL","LAYGO","RD","WR" D
 . S F=$$FDICCHAR(P1,J)  S:F'="" RESULT(I)=F,I=I+1
 ;
 S:$D(^DIC(P1,"%"))#10=1 RESULT(I)="Application Group: <"_^DIC(P1,"%")_">",I=I+1
 S:$D(^DIC(P1,"%A"))#10=1 RESULT(I)="DUZ file creation date: <"_^DIC(P1,"%A")_">",I=I+1
 S:$D(^DIC(P1,"%D"))#10=1 RESULT(I)="Description: <"_^DIC(P1,"%D")_">",I=I+1
 ;
 S RESULT(0)=I-1
 Q
 ;
FMMFLDS ;
 ; Characteristics of all Fields of the FileMan file FNUM
 N G,FN,IND S G=0,IND=0,FN=ARRAY(0)
 F  S G=$O(^DD(FN,G))  Q:'+G  S IND=IND+1,RESULT(IND)=G_"^"_$$ISMULT(FN,G)
 S RESULT(0)=IND
 Q
 ; 
FMFINDXS ;
 ; index names by file number
 N G,I,P1 S G="",I=0,P1=ARRAY(0)
 F  S G=$O(^DD(P1,0,"IX",G))  Q:G=""  S I=I+1  S RESULT(I)=G
 S RESULT(0)=I
 Q
 ;  S N=0 F  S N=$O(^DD(TARGET,"SB",N)) Q:'+N  S SD(N)=""
FMMULTS ;
 ; subfiles by file number
 N G,I,P1 S G=0,I=0,P1=ARRAY(0)
 F  S G=$O(^DD(P1,"SB",G))  Q:'+G  S I=I+1  S RESULT(I)=G
 S RESULT(0)=I
 Q
 ; 
FMFIELD ;
 ; Characteristics of the one Field FFNUM of the FileMan file FNUM
 N FC,IND S IND=0,FNUM=ARRAY(0),FFNUM=ARRAY(1)
 D FMFLDDEF
 S RESULT(0)=IND ; total number of records
 Q
 ;
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; ROUTINES EDITING
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;
RSAVE ; 
 ;RSAVE ; This subroutine will save a routine to M from the indicated array
 ;  The routine name and line count should be provided in the first record of array as
 ;  RoutineName^LineCount
 N X,XCN,DIE,CNT,ROU,ZZNAME,ZZCOUNT,FLG
 S ZZRNAME=$P(ARRAY(0),"^"),ZZCOUNT=$P(ARRAY(0),"^",2)
 S RESULT(0)="1^PROBLEM WITH ROUTINE NAME or LINE COUNT: "_ARRAY(0)
 ;Q:AUDREY="" ;not defined to generate error and test reconnection of GUI 
 Q:ZZRNAME=""
 Q:ZZCOUNT<1
 S CNT=0,XCN=0,ROU="ROU",FLG=0
 F I=1:1:ZZCOUNT S:ARRAY(I)'="" ^UTILITY($J,"ROU",I,0)=ARRAY(I) I ARRAY(I)="" S RESULT(0)="2^BLANK LINE "_I_" FOUND",FLG=1 Q
 I FLG Q
 I I'=ZZCOUNT S RESULT(0)="3^BAD LINE COUNT" Q
 S DIE="^UTILITY($J,"_ROU_",",X=ZZRNAME
 X ^%ZOSF("SAVE")
 I $D(^UTILITY(ROU,ZZRNAME)) S CNT=1
 K ^UTILITY(ROU,ZZRNAME),^UTILITY($J,ROU)
 S RESULT(0)="0^"_ZZRNAME_" SAVED"
 Q
 ;
RDELETE ; This subroutine will delete a routine from M
 ;
 N DIF,X,XCNP
 S RESULT(0)="1^FAILED"
 S X=ARRAY(0)
 X ^%ZOSF("DEL")
 S RESULT(0)="0^"_ARRAY(0)_" DELETED"
 Q
 ; 
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;
TEST ; Use TEST^ZZFMVIEW as the debug target in Studio
 ;
 ZBREAK TEST0^ZZFMVIEW
 S DUZ=66 D P^DI
TEST0 ;
 S $ZSTEP="W !,$ZPOS,! ZP @$ZPOS W ! D TESTZWR^"_$T(+0)_" BREAK"
 N ZZAND,ZZAA
 D RPC(.ZZAND,"XXX",.ZZAA)
 D RPC(.ZZAND,"HELP")
 D RPC(.ZZAND,"VERSION")
 S ZZAA(0)="ECHO TEST Line 1"
 S ZZAA(1)="ECHO TEST Line 2"
 D RPC(.ZZAND,"ECHO",.ZZAA)
 W !,"TEST complete"
 Q
TESTZWR ;
 ZWRITE:$D(ZZAND) ZZAND(*) ZWRITE:$D(ZZAA) ZZAA(*)
 ZWRITE:$D(RESULT) RESULT(*)
 ZWRITE:$D(OPTION) OPTION(*) ZWRITE:$D(ARRAY) ARRAY(*)
 QUIT
 ;
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; HELP and SAMPLES
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;
HELP ;
 ; Description and samples
 N G,OUT,J,I S G="",OUT=0,J=1
 F I=1:1 D  Q:OUT=1
 . S G=$T(HELP+9+I)
 . S:G="" OUT=1
 . S:G["HELP END" OUT=1
 . S:(G?.(1" ",1C)1";;".E) RESULT(J)=$P(G,";;",2),J=J+1
 S RESULT(0)=(J-1)
 Q
 ; 
ZZFMVRS ;(RESULT,STR,ROUS)        ;
 ;The "me"
 K RESULT,P1,P2,P3,P4,P5,^UTILITY($J),AR,ARR
 N FILE,ZHD,ZRD
 S STR=$G(ARRAY(0)),ROUS=$G(ARRAY(1))
 I '$L(STR) D  Q
 . S RESULT(0)=-1
 . S RESULT(1)="DUDE!, NO STRING(...g or otherwise)"
 I '$L(ROUS) D  Q
 . S RESULT(0)=-2
 . S RESULT(1)="DUDE! You want me to search every routine"
 . S RESULT(2)="on the PLANET??!!"
 S HUH=+$P($G(^%ZOSF("OS")),"^",2)
 I HUH'=18&(HUH'=19) D  Q
 . S RESULT(0)=1
 . S RESULT(1)="WHO THE HELL ARE YOU????"
 I HUH=18 G ZZFMVRC  ;To Cache/Iris/Vivian/Whatever - Uses same arguments
 S FILE="zzfmvrs"_$J_".txt"
 S ZRD=$ZRO,ZRD=$P($P(ZRD,")"),"(",2)
 ;The Whitten:
 ;ZSY "grep -Hin '"_STR_"' "_ZRD_"/"_ROUS_"*.m 2>&1 > "_FILE
 ZSY "grep -H '"_STR_"' "_ZRD_"/"_ROUS_"*.m 2>&1 > "_FILE
 ;The Sam H:
 D FTG^%ZISH($ZD,FILE,$NA(^UTILITY($J,1)),2,"")
 ;The "me & Whitten & Nancy"
 ZSY "rm -fr "_FILE
 I '$D(^UTILITY($J)) S RESULT(0)=-3 Q
 K ARR ;Consolidate text names
 M AR($J)=^UTILITY($J) K ^UTILITY($J)
 S N="AR" F  S N=$Q(@N) Q:N=""  S X=@N D
 . S X=$P(X,":")
 . S X=$P(X,"/",$L(X,"/"))
 . S ARR(X)=""
 S N="" F  S N=$O(ARR(N)) Q:N=""  S RESULT($O(RESULT(" "),-1)+1)=N
 S RESULT(0)=$O(RESULT(" "),-1)
 K AR,ARR,X,N
 Q
SYMTAB(REF) ; Return the current symbol table
 N X K ^TMP($J,"SAV"),^TMP($J,"SND")
 S X="^TMP($J,""SAV""," D DOLRO^%ZOSV
 N N,I,L S X="^TMP($J,""SAV"")",L=0
 S L=L+1,RESULT(L)="$I="_$I_"  $J="_$J_"  $S="_$S
 F  S X=$Q(@X) Q:$QL(X)<3  Q:$QS(X,1)'=$J  Q:$QS(X,2)'="SAV"  D
 . S N=$QS(X,3)
 . I $QL(X)=3 D  Q
 . . S L=L+1,RESULT(L)=N_"="_@X
 . E  D
 . . S N=N_"(" F I=4:1:$QL(X) S N=N_$QS(X,I)_","
 . . S N=$E(N,1,$L(N)-1)_")"
 . . S L=L+1,RESULT(L)=N_"="_@X
 S RESULT(0)=L
 Q 
 ;
 ;*****************************************************************
 ;* Lines not started with " ;;" are not included in the output 
 ;*****************************************************************
 ;*
 ;;This RPC provides data for ZZFMVIEW GUI
 ;;RPC requires 2 parameters and returns the result array
 ;;
 ;;Input parameters
 ;;  - OPTION (#1: literal, size=8, required)
 ;;  - ARRAY  (#2: array, size=32000, optional)
 ;;
 ;;  OPTION literal defines action to execute
 ;;  ARRAY contains additional parameters required by OPTION
 ;;
 ;;Output
 ;;The first line of the results array RESULT contains number of lines returned
 ;;the rest of the result array contains data if any:
 ;;
 ;;    RESULT(0)=RC
 ;;    RESULT(1)=data_1
 ;;    RESULT(2)=data_2
 ;;    ...
 ;;    RESULT(RC)=data_RC 
 ;;
 ;;Zero or positive RC identifies the last index of the RESULT array
 ;;For negative RC description of the error is in the second piece ("^" delimeter) of the RESULT(0)  
 ;;
 ;;The next actions (OPTIONS) are supported by this version:
 ;*** VALID OPTIONS LIST STARTS HERE ***
 ;;HELP     - this text
 ;;NULL     - returns "0^NULL"
 ;;VERSION  - returns version of the RPC 
 ;;ECHO     - returns array sent as the parameter
 ;
 ;;REXISTS  - verifies if the routine exists
 ;;RLIST    - lists routines matchiing target
 ;;RSOURCE  - returns source code of routine
 ;;RSAVE    - saves routine by name
 ;;RDELETE  - delets routine by name
 ;
 ;;FMFNAME  - file global name and info by file number
 ;;FMFFLDS  - fields of the file. Field numbers only
 ;;FMFCHRS  - file chars
 ;;FMFINDXS - file index names
 ;
 ;;FMFIELD  - one field definitions
 ;;FMFIELDS - all file fields with field definition
 ;
 ;;LISTGLBE - List Global E
 ;;LISTGLBL - List Global
 ;;LISTMTCH - List Global matching target
 ;;LISTGR   - List Global in reverse
 ;;DIM      - Code validation
 ;
 ;;RCHKSUM  - Checksum of list of routines
 ;;RCHKSUM2 - test version
 ;
 ;;FILEDD   - DD data for a file
 ;;FMMULTS  - Subfiles of the file
 ;;FMFFLDS1 - Filed definitions of FM file. (Including definitions of multiples)
 ;;FMFLDDE2 - Characteristics of the file field
 ;FMFFLDSA - Under dev. File fields
 ;;LISTDD   - DD Lister
 ;
 ;;FINDCTX  - implements ^DIC(19,:,"RPC","B",+RPCIEN,*)
 ;
 ;FMFDATA   - file records - logic error!
 ;;FILELIST - list of files - should be verfied - logic error!
 ;;SHAHASH  - test
 ;
 ;*** FMCOMPARE - OPTIONS VALID FOR FMCOMPARE ***
 ;
 ;;OS       - OS
 ;;SYMTAB   - SYMBOL TABLE 
 ;
 ;;ZZFMVRS  - Routines ARRAY(1) containing string ARRAY(0)
 ;
 ;*** VALID OPTIONS LIST ENDS HERE *** ;;
 ; HELP ENDS HERE ***********************************************************
 ;
FILEDD ;DD EXTRACTIONS
 K SD ;N N,N2,N3,N4,N5
 N TARGET,J
 S TARGET="^DD("_ARRAY(0)_")",J=1
 ;Prime Target
 ;S X=TARGET F  S X=$Q(@X) Q:+$P(X,"(",2)>+$P(TARGET,"(",2)!(+$P(X,"(",2)'=+$P(TARGET,"(",2))  W !,X,"=",@X
 S X=TARGET F  S X=$Q(@X) Q:+$P(X,"(",2)>+$P(TARGET,"(",2)!(+$P(X,"(",2)'=+$P(TARGET,"(",2))  S RESULT(J)=X_"="_@X,J=J+1
 ;Q
 ;SB XFRS - Find all the in file multiples
 S TARGET=+$P(TARGET,"(",2)
 S N=0 F  S N=$O(^DD(TARGET,"SB",N)) Q:'+N  S SD(N)=""
 S N=0 F  S N=$O(SD(N)) Q:'+N  D
 . Q:'$D(^DD(N,"SB"))
 . S N2=0 F  S N2=$O(^DD(N,"SB",N2)) Q:'+N2  S SD(N2)=""
 . Q:N2=""
 . Q:'$D(^DD(N2,"SB"))
 . S N3=0 F  S N3=$O(^DD(N2,"SB",N3)) Q:'+N3  S SD(N3)=""
 . Q:N3=""
 . Q:'$D(^DD(N3,"SB"))
 . S N4=0 F  S N4=$O(^DD(N3,"SB",N4)) Q:'+N4  S SD(N4)=""
 . Q:N4=""
 . Q:'$D(^DD(N4,"SB"))
 . S N5=0 F  S N5=$O(^DD(N4,"SB",N5)) Q:'+N5  S SD(N5)=""
 ;Display any DD target multiples
 S MX=0 F  S MX=$O(SD(MX)) Q:'+MX  D
 . S X="^DD("_MX_")" F  S X=$Q(@X) Q:+$P(X,"(",2)>MX!(+$P(X,"(",2)'=MX)  S RESULT(J)=X_"="_@X,J=J+1
 S RESULT(0)=J-1
 Q
 ;
FILELIST ;Returns List of files
 ;START: First file to find
 ;LIMIT: Number of files to return
 ;KEYLEN: Length of search key
 NEW:0 G,I,J,START,LIMIT,P3
 S I=0,G=+$G(ARRAY(0)),START=$G(ARRAY(0))
 S LIMIT=+$G(ARRAY(1)),KEYLEN=+$G(ARRAY(2),99)
 S STARTatZERO=(+START=0)
 I 1!STARTatZERO D
 . D FLSAVE(.G,.I)
 . FOR  S G=$O(^DD(G))  Q:G<1E-5  Q:I+1>LIMIT  I $D(^DIC(G,0))#2 D
 .. Q:G<1E-5  D FLSAVE(.G,.I)
 I 0&'STARTatZERO D
 . D FLSAVE(.G,.I)
 . FOR  S G=$O(^DD(G))  Q:G<1E-5  Q:I=LIMIT  I $D(^DIC(G,0))#2 D
 .. Q:$E(START,1,KEYLEN)'=$E(G,1,KEYLEN)
 .. D FLSAVE(.G,.I)
 S RESULT(0)=I
 Q
FLSAVE(F,I) ;
 Q:$D(^DD(G,0))[0
 S RESULT(I+1)=G_"^"_$P($G(^DIC(G,0)),"^")_$G(^DIC(G,0,"GL")),I=I+1 
 QUIT
FLTEST ;
 ZBREAK FILELIST^ZZFMVIEW S $ZSTEP="W ! ZPRINT @$ZPOS BREAK"
 s ARRAY(0)=.404,ARRAY(1)=5,ARRAY(2)=15 D FILELIST^ZZFMVIEW
 ;s ARRAY(0)="",ARRAY(1)=5,ARRAY(2)=15 D FILELIST^ZZFMVIEW
 ZWR
 Q
 ;
FMFDATA ;
 ; FileMan File Data records.
 ; returns P3 records of file P1 starting from IEN>(P2-1)
 ;
 N G,I,J,P1,P2,P3
 S P1=ARRAY(0),P2=ARRAY(1),P3=ARRAY(2)
 Q:'$$VFILE^DILFD(P1)
 S G=$$ROOT^DILFD(P1,,1) ;File root
 S I=1,J=P2-1
 F  S J=$O(@G@(J))  Q:'J  Q:I=(P3+1)  D
 .S:$D(@G@(J,0)) RESULT(I)=J_"|"_@G@(J,0)
 .S I=I+1
 .S RESULT(I)="",I=I+1
 S RESULT(0)=I-1
 Q
 ;
SHOW1 ; Local. Not used as an OPTION
 S RESULT(J)=J_"|"_NODE_"|"_@NODE,FOUND=1,J=J+1
 Q
 ; 
LISTGR ;
 ;Lists global. ARRAY(0) -starting node, ARRAY(1) -Nodes to return, ARRAY(2) - Direction
 N I,FOUND,G,NODE,J,DIR,CNT,II
 S I=0,FOUND=0,J=0,CNT=10,II=1
 S NODE=ARRAY(0),DIR=-1 ; default direction 
 I $G(ARRAY(1)) S CNT=ARRAY(1) ; set default count
 I $G(ARRAY(2)) S DIR=ARRAY(2) ; set direction if specified
 S G=$D(@NODE),RESULT(0)="-1^Global "_NODE_" Not found"  ;_"DIR="_DIR
 I G#10=1 S J=1,II=2 D SHOW1
 Q:II>CNT
 ;F I=1:1:CNT S NODE=$$Q^VWUTIL($NA(@NODE),DIR) Q:NODE=""  D SHOW1
 ;F I=1:1:CNT S NODE=$$Q($NA(@NODE),DIR) Q:NODE=""  D SHOW1
 F I=II:1:CNT S NODE=$$NODEUP(NODE) Q:NODE=""  D NDSHOW(NODE)
 I 'FOUND Q
 S RESULT(0)=J-1_"|"_NODE
 Q
 ; 
NDSHOW(NODE) 
 Q:'$D(NODE)
 S RESULT(J)=J_"|"_NODE
 I $D(NODE)#2=1 S RESULT(J)=RESULT(J)_"|"_@NODE
 S J=J+1 
 Q
 ;
NDNAME(NODE,X) ; Replaces last subscript of NODE with X
 N TMP
 S TMP=$NA(@NODE,$QL(NODE)-1)
 Q $NA(@TMP@(X))
 ;
NDDOWN(NODE,NDLIMIT) ; Finds next node starting with NODE up to LIMIT 
 N TMP,TMPOLD,I,III
 S TMPOLD=NODE,TMP=$Q(@NODE),III=2000
 I TMP=NDLIMIT Q TMPOLD
 F I=1:1:III Q:(TMP="")!(TMP=NDLIMIT)  D
 . S TMPOLD=TMP,TMP=$Q(@TMP)
 .; S RESULT(J)="    NDLIMIT="_NDLIMIT_" TMPOLD="_TMPOLD_" TMP="_TMP,J=J+1
 ;I I=III S RESULT(J)="Limit "_III,J=J+1 ; debug
 Q TMPOLD
 ;
NODEUP(NODEIN) ; 
 N TMP,NN,TMPN,NDLIMIT
 S TMPN=NODEIN,NDLIMIT=NODEIN,TMP=""
START 
 ;S RESULT(J)="  TMPN="_TMPN,J=J+1
 S TMP=$O(@TMPN,-1)       ; same level prev subscript
 ;S RESULT(J)="  * TMPN="_TMPN_" TMP="_TMP_" $O(TMPN,-1)="_$O(@TMPN,-1),J=J+1
 I TMP'="" S TMP=$$NDNAME(TMPN,TMP) Q $$NDDOWN(TMP,TMPN)  ; not blank - find down
 I $QL(TMPN)=1 Q "" ; quit if it is the first level        ; blank. leave if first one
 S NN=$NA(@TMPN,$QL(TMPN)-1)   ; level up 
 I $D(@NN)#10=1 Q $$NDDOWN(NN,NDLIMIT) ; check if the node exists
 S TMP=$O(@NN,-1)              ; prev subscript 
 ;S RESULT(J)="    NN="_NN_" $O(@NN,-1)="_$O(@NN,-1),J=J+1 
 I TMP="" S TMPN=NN G START    ; if blank - search on prev level
 I TMP'="" D                   ; not blank - search down
 .; S RESULT(J)="  NN="_NN_" TMP="_TMP_" $$NDNAME(NN,TMP)="_$$NDNAME(NN,TMP),J=J+1 
 . S NN=$$NDNAME(NN,TMP),TMP=$$NDDOWN(NN,NDLIMIT)
 Q TMP
 ;
DIM ; Code validation
 N G,OUT,L,C,IND S G="",OUT=0,C="",IND=""
 F I=1:1 D  Q:OUT=1 
 . S G=$O(ARRAY(G))
 . I G="" S OUT=1
 . I G'="" D 
 . . S X=ARRAY(G),C=ARRAY(G),IND=G,L=""
 . . S RESULT(IND)="IND="_IND_"  "_L_" code: """_C_""""
 . . D ^DIM
 . . I '$D(X) S L="Invalid "
 . . E  S L="  Valid "
 . . S RESULT(IND)=RESULT(IND)_"  ---- "_L
 Q
 ;
SHAHASH ; 
 S RESULT(0)=$$SHAHASH^XUSHSH(256,ARRAY(0),"B")
 ;S RESULT(0)=ARRAY(0)
 Q
 ; 