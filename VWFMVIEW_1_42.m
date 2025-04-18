VWFMVIEW ; WAA/AA - FMVIEW GUI INTERFACE ROUTINE ; AUG 23, 2012@11:54
 ;;1.42;ZZFMVIEW;;Jan 10, 2025@13:31;;
 ;
 ; (C) 2025 WorldVistA.
 ;
 ; This program is free software: you can redistribute it and/or modify
 ; it under the terms of the GNU Affero General Public License as
 ; published by the Free Software Foundation, either version 3 of the
 ; License or any later versions (<http://www.gnu.org/licenses/>).
 ; 
 ; This program is distributed WITHOUT ANY WARRANTY;
 ;
 ; This code implements RPC used by FMVIEW GUI application.
 ;
 ; Input parameters
 ;  - OPT   (#1: literal, size=8, required) defines action to execute
 ;  - ARRAY (#2: array, size=32000, optional) additional info required by OPT
 ;
 ; Output
 ;  - RESULT (Array)
 ;      The first line of the result array contains number of lines returned
 ;      the rest of the array contains data if any:
 ;           RESULT(0)=RC
 ;           RESULT(1)=data_1
 ;           RESULT(2)=data_2
 ;           ...
 ;           RESULT(RC)=data_RC 
 ;
 ; Negative RC supposed to provide description of the error placed in 
 ; the second piece ("^" delimeter) of the RESULT(0)  
 ;
 ; Only tags commented with ;;; can be used as options 
 ;
RPC(RESULT,OPT,ARRAY) ; RPC processing entry point
 I $$ZVALID(OPT)=0 S RESULT(0)="-1^Option "_OPT_" is not supported by this RPC" Q
 D CLEAN^DILF
 S RESULT=$NA(^TMP("ZZFM00",$J)) K @RESULT
 I '($T(@OPT)]"") S RESULT(0)="-1^Option '"_OPT_"' not found in routine '"_$T(+0)_"'." Q
 D @OPT
 I '$D(RESULT(0)) S RESULT(0)="-1^Unspecified Error"
 K ^TMP("ZZFM00",$J)
 D CLEAN^DILF
 Q
ZVALID(OPT) ; Option name validation
 N RZLT,I,R,L,TN,COMM
 S I=0,R=0
 D ZRSOURCE(.RZLT,$T(+0))
 F  S I=$O(RZLT(I)) Q:I=""  D
 . S L=RZLT(I)
 . I $E(L,1)'=" " D
 . . S TN=$P(L," ",1)
 . . S COMM=$P(L,";;;",2)
 . . S:$L(COMM)&(TN=OPT) R=1 Q
 Q R
ZRSOURCE(RSLT,RNAME) ; Routine Source
 N DIF,X,XCNP
 S X=RNAME X ^%ZOSF("TEST") I '$T S RESULT(0)="-1^Routine "_RNAME_" not found" Q
 K ^TMP("ZZAAED",$J)
 S XCNP=0,DIF="^TMP(""ZZAAED"",$J,",X=RNAME
 X ^%ZOSF("LOAD")
 S RSLT(0)=XCNP-1
 F X=1:1:RSLT(0)  S RSLT(X)=^TMP("ZZAAED",$J,X,0)
 I '$D(RSLT) S RSLT(0)="-1^Unspecified Error"
 Q
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ECHO ;;; Returns array sent as the parameter
 N G,OUT S G="",OUT=0
 F I=1:1 D  Q:OUT=1
 . S G=$O(ARRAY(G))
 . S:G="" OUT=1
 . S:G'="" RESULT(G)=ARRAY(G)
 Q
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FILEDD ;;; DD info for a file
 K SD
 N TG,J,N,N2,N3,N4,N5,MX,TGQL
 S TG="^DD("_ARRAY(0)_")",J=1,TGQL=$QL(TG)
 S X=TG F  S X=$Q(@X) Q:$NA(@X,TGQL)'=TG  Q:X=""  S RESULT(J)=X_"="_@X,J=J+1
 S TG=$QS(TG,1)
 S N=0 F  S N=$O(^DD(TG,"SB",N)) Q:'N  S SD(N)=""
 S N=0 F  S N=$O(SD(N)) Q:'N  D
 . Q:'$D(^DD(N,"SB"))
 . S N2=0 F  S N2=$O(^DD(N,"SB",N2)) Q:'N2  S SD(N2)=""
 . ;Q:N2=""
 . ;Q:'$D(^DD(N2,"SB"))
 . ;S N3=0 F  S N3=$O(^DD(N2,"SB",N3)) Q:'N3  S SD(N3)=""
 . ;Q:N3=""
 . ;Q:'$D(^DD(N3,"SB"))
 . ;S N4=0 F  S N4=$O(^DD(N3,"SB",N4)) Q:'N4  S SD(N4)=""
 . ;Q:N4=""
 . ;Q:'$D(^DD(N4,"SB"))
 . ;S N5=0 F  S N5=$O(^DD(N4,"SB",N5)) Q:'N5  S SD(N5)=""
 ;Display any DD target multiples
 S MX=0 F  S MX=$O(SD(MX)) Q:'MX  D
 . S X="^DD("_MX_")" F  S X=$Q(@X) Q:+$P(X,"(",2)>MX!(+$P(X,"(",2)'=MX)  S RESULT(J)=X_"="_@X,J=J+1
 S RESULT(0)=J-1
 Q
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FINDCTX ;;; Lists Context names the RPC is included (Implements ^DIC(19,:,"RPC","B",+RPCIEN,*))
 N TN
 S TN=ARRAY(0)
 I TN'=+TN S TN(1)=$O(^XWB(8994,"B",TN,""))
 E  S TN(1)=TN
 Q:'TN(1)
 N D0,D1,K S K=0
 S D0=0 F  S D0=$O(^DIC(19,D0)) Q:'D0  DO
 . S D1=0 F  S D1=$O(^DIC(19,D0,"RPC","B",TN(1),D1)) Q:D1=""  DO
 . . S K=K+1
 . . S RESULT(K)=$P($G(^DIC(19,D0,0)),U)_U_D0
 S RESULT(0)=K
 Q
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FMFCHRS ;;; FileMan File characteristics
 N F,P1,I
 S P1=ARRAY(0),I=0
 S:$D(^DIC(P1,0))#2 RESULT(I)="0^FILE "_P1_" CHARS",I=I+1
 F J="ACT","DDA","DIC","SCR","VR","VRPK","VRRV" D
 . S F=$$ZFCHAR(P1,J)  S:F'="" RESULT(I)=F,I=I+1
 S:$D(^DD(P1,0,"ID","WRITE"))#2 RESULT(I)=^DD(FNUM,0,"ID","WRITE"),I=I+1
 F J="GL","AUDIT","DD","DEL","LAYGO","RD","WR" D
 . S F=$$ZFDICCHA(P1,J)  S:F'="" RESULT(I)=F,I=I+1
 S:$D(^DIC(P1,"%"))#2 RESULT(I)="Application Group: <"_^DIC(P1,"%")_">",I=I+1
 S:$D(^DIC(P1,"%A"))#2 RESULT(I)="DUZ file creation date: <"_^DIC(P1,"%A")_">",I=I+1
 S:$D(^DIC(P1,"%D"))#2 RESULT(I)="Description: <"_^DIC(P1,"%D")_">",I=I+1
 S RESULT(0)=I-1
 Q
ZFCHAR(FNUM,CHAR) ; internal function used by FMFCHRS option
 N FC  S FC=""
 S:$D(^DD(FNUM,0,CHAR))#2 FC=$J(CHAR,10)_" : "_^DD(FNUM,0,CHAR)
 Q FC
ZFDICCHA(FNUM,CHAR) ; internal function used by FMFCHRS option
 N FC  S FC=""
 S:$D(^DIC(FNUM,0,CHAR))#2 FC=$J(CHAR,10)_" : "_^DIC(FNUM,0,CHAR)
 Q FC
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FMFFLDS ;;; Fields of the FileMan file
 N I,G S G="",I=1
 F  S G=$O(^DD(ARRAY(0),G))  Q:G=""  S I=I+1  S RESULT(I)=G
 S RESULT(0)=I
 Q
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FMFFLDS1 ;;; Fields of the FileMan file
 N FN,I S FN=ARRAY(0),I=0
 D ZFMFFLDS(FN)
 Q 
ZFMFFLDS(FNUM) ;; internal browses DD for FNUM fields definitions (including multiple)
 N G,RSLT S G=0,RSLT=""
 F  S G=$O(^DD(FNUM,G))  Q:'+G  D
 . S:$D(^DD(FNUM,G,0))#2 RSLT=$P(^DD(FNUM,G,0),"^",2)
 . S I=I+1,RESULT(I)=I_"^"_FNUM_"^"_G_"^"_^DD(FNUM,G,0)_"^ ---"_(+RSLT)_" IS MULT = "_$$ZISMULT(FNUM,+RSLT,G)
 . I +RSLT&(+$$ZISMULT(FNUM,+RSLT,G)=1) D ZFMFFLDS(+RSLT)
 S RESULT(0)=I
 Q
ZISMULT(FN,SBFNUM,FLD) ;; internal checks if the FNUM field FLDNUM is multiple
 N RZ S RZ=-1
 S:$D(^DD(FN,"SB",SBFNUM,FLD))#2 RZ=1
 Q RZ_"^ "_($D(^DD(FN,"SB",SBFNUM,FLD))#2)_"  $D(^DD("_FN_",""SB"","_SBFNUM_","_FLD_"))="_$D(^DD(FN,"SB",SBFNUM,FLD))
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FMFIELD ;;; Characteristics of the one Field FFNUM of the FileMan file FNUM
 N FC,IND S IND=0,FNUM=ARRAY(0),FFNUM=ARRAY(1)
 D FMFLDDEF
 Q
FFCHAR(FNUM,FFNUM,CHAR) ; Used by FMFLDDEF. Field Char by File (FNUM) and Field (FFNUM)
 N FFC  S FFC=""
 S:$D(^DD(FNUM,FFNUM,CHAR))#2 FFC=FNUM_"^"_FFNUM_"^"_CHAR_"^"_^DD(FNUM,FFNUM,CHAR)
 Q FFC
FMFLDDEF ; internal. Field FFNUM Characteristics for file FNUM. Used by FMFIELD, FMFIELDS
 F I=0,".1",1,2,3,4,5,7.5,8,9,9.01,9.02,9.03,9.04,9.05,9.06,9.07,9.08,9.09,10,11,10,12.1,20,21,22,23 D
 . S FC=$$FFCHAR(FNUM,FFNUM,I)  S:FC'="" IND=IND+1,RESULT(IND)=FC
 F I="AUDIT","AX","DEL","DT","LATGO" D
 . S FC=$$FFCHAR(FNUM,FFNUM,I)  S:FC'="" IND=IND+1,RESULT(IND)=FC
 S RESULT(0)=IND
 Q
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FMFIELDS ;;; Characteristics of all Fields of the FileMan file FNUM
 N G,FC,I,IND S G="",IND=0,FNUM=ARRAY(0)
 F  S G=$O(^DD(FNUM,G))  Q:G=""  S FFNUM=G D FMFLDDEF
 Q
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
HELP ;;; Valid Options List
 N I,LN,TN,CM,J,RZLT
 S I=0,J=0
 D ZRSOURCE(.RZLT,$T(+0))
 F  S I=$O(RZLT(I)) Q:I=""  D
 . S LN=RZLT(I)
 . I $E(LN,1)'=" " D
 . . S TN=$P(LN," ",1)
 . . S CM=$P(LN,";;;",2)
 . . I $L(CM) D
 . . . S J=J+1,RESULT(J)=$J(TN,10)_" -- "_CM
 S RESULT(0)=J
 Q
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LISTGLBL ;;; From a given (ARRAY(0)) to and end range (ARRAY(1))
 N I,G,NODE,J,JJ,K,L,M,MM
 S I=0,J=0,JJ=ARRAY(1),NODE=ARRAY(0)
 S G=$D(@NODE),RESULT(0)="-1^Global "_NODE_" Not found"
 Q:G=0
 F I=1:1:JJ S NODE=$Q(@NODE) Q:NODE=""  D  ;SHOW
 . S (G,MM)=@NODE I G?.E1C.E D 
 .. S MM="" F L=1:1:$L(G) D  S MM=MM_M ;  
 ... S M=$A(G,L) F K=0:1:31,127:1:255 I K=M S M="$C("_K_")" Q 
 . S J=J+1,RESULT(J)=J_"|"_NODE_"|"_MM
 S RESULT(0)=I_"|"_NODE
 Q
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LISTDD ;;; DD Lister
 N I,G,NODE,J,FOUND
 S I=0,J=1,NODE=ARRAY(0)
 S G=$D(@NODE),RESULT(0)="-1^Global "_NODE_" Not found"
 I G#2 S J=J+1,RESULT(J)=J-1_"|"_NODE_"|"_@NODE,FOUND=1
 F I=1:1:ARRAY(1) S NODE=$Q(@NODE) Q:(NODE="")!($$ZNEEDED(NODE,ARRAY(0))=0)  D
 . S J=J+1,RESULT(J)=J-1_"|"_NODE_"|"_@NODE
 S RESULT(0)=I_"|"_NODE
 Q
ZNEEDED(ND,ND0) ;
 ;;ND=Current Global refeence (Example: ^DIC(2,0,"GL')
 ;;ND0=Required Prefix (Example: ^DIC(2,0))
 N RC,LL,SS S RC=0,LL=$L(ND0),SS=$E(ND,LL,LL)
 S:($E(ND0,1,LL-1)=$E(ND,1,LL-1))&((SS=",")!(SS=")")) RC=1
 Q RC
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LISTGR ;;; Lists global. ARRAY(0) - starting node, ARRAY(1) - nodes to return, ARRAY(2) - Direction
 N I,FOUND,G,NODE,J,DIR,CNT,II
 S I=0,FOUND=0,J=0,CNT=10,II=1
 S NODE=ARRAY(0),DIR=-1 ; default direction 
 I $G(ARRAY(1)) S CNT=ARRAY(1) ; default count
 I $G(ARRAY(2)) S DIR=ARRAY(2) ; direction if specified
 S G=$D(@NODE),RESULT(0)="-1^Global "_NODE_" Not found"  ;_"DIR="_DIR
 I G#2 S J=1,II=2,RESULT(J)=J_"|"_NODE_"|"_@NODE,FOUND=1,J=J+1
 Q:II>CNT
 F I=II:1:CNT S NODE=$$NODEUP(NODE) Q:NODE=""  D NDSHOW(NODE)
 I 'FOUND Q
 S RESULT(0)=J-1_"|"_NODE
 Q
NDSHOW(NODE) ;
 Q:'$D(NODE)
 S RESULT(J)=J_"|"_NODE
 I $D(NODE)#2 S RESULT(J)=RESULT(J)_"|"_@NODE
 S J=J+1
 Q
NDNAME(NODE,X) ; Replaces last subscript of NODE with X
 N TMP
 S TMP=$NA(@NODE,$QL(NODE)-1)
 Q $NA(@TMP@(X))
NDDOWN(NODE,NDLIMIT) ; Finds next node starting with NODE up to LIMIT 
 N TMP,TMPOLD,I,III
 S TMPOLD=NODE,TMP=$Q(@NODE),III=2000
 I TMP=NDLIMIT Q TMPOLD
 F I=1:1:III Q:(TMP="")!(TMP=NDLIMIT)  D
 . S TMPOLD=TMP,TMP=$Q(@TMP)
 Q TMPOLD
NODEUP(NODEIN) ; 
 N TMP,NN,TMPN,NDLIMIT
 S TMPN=NODEIN,NDLIMIT=NODEIN,TMP=""
START ;
 S TMP=$O(@TMPN,-1)       ; same level prev subscript
 I TMP'="" S TMP=$$NDNAME(TMPN,TMP) Q $$NDDOWN(TMP,TMPN)  ; not blank - find down
 I $QL(TMPN)=1 Q "" ; quit if it is the first level        ; blank. leave if first one
 S NN=$NA(@TMPN,$QL(TMPN)-1)   ; level up 
 I $D(@NN)#2 Q $$NDDOWN(NN,NDLIMIT) ; check if the node exists
 S TMP=$O(@NN,-1)              ; prev subscript 
 I TMP="" S TMPN=NN G START    ; if blank - search on prev level
 I TMP'="" D                   ; not blank - search down
 . S NN=$$NDNAME(NN,TMP),TMP=$$NDDOWN(NN,NDLIMIT)
 Q TMP
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
OS ;;; OS info
 I $D(^%ZOSF("OS")) S RESULT(0)=$G(^%ZOSF("OS"))
 E  S RESULT(0)="Unknown"
 Q
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RDELETE ;;; This subroutine will delete a routine from M
 N DIF,X,XCNP
 S RESULT(0)="1^FAILED"
 S X=ARRAY(0)
 X ^%ZOSF("DEL")
 S RESULT(0)="0^"_ARRAY(0)_" DELETED"
 Q
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
REXISTS ;;; Verifies if the routine exists
 S X=ARRAY(0) X ^%ZOSF("TEST") I '$T S RESULT(0)="-1^Routine "_ARRAY(0)_" not found" Q
 E  S RESULT(0)="0^"_ARRAY(0)_" found"
 Q
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RLIST ;;; Modification-adding GTm to RLIST 01062016 jeb update 20220618 djw
 ; Returns list of routines from a given starting point to and end range of x
 I $G(^%ZOSF("OS"))["GTM"!(+$P($G(^%ZOSF("OS")),"^",2)=19) D  Q
 . N %ZR S %ZR=ARRAY(0)
 . D SILENT^%RSEL(%ZR) ;Result will be in %ZR - loop through 
 . N J S J=""
 . F CNT=0:1 S J=$O(%ZR(J)) Q:J=""  D
 . . S RESULT(J)=J_"^"_%ZR(J)_J_".m"
 . S RESULT(0)=CNT
 . Q
 ;
 N ZZAA S ZZAA="F  S X=$O(^$ROUTINE(X)) Q:($L(X)=0)!(""""'=$P(X,Z))  S CNT=CNT+1,RESULT(CNT)=X I Y>0 Q:Y=CNT"
 N X,Y,CNT,Z
 S CNT=0,X=ARRAY(0),Z=ARRAY(0)
 S:X["*" X=$P(X,"*")
 S X=$O(^$R(ARRAY(0)),-1),Y=ARRAY(1)
 X ZZAA
 S RESULT(0)=CNT
 Q
ZHAS(ROU,TEXT) ;
 I $G(ROU)=""!($G(TEXT)="") Q +"1true"
 N LN S LN(0)=+"0false"
 F LN=1:1 Q:$T(+LN^@ROU)=""  I $T(+LN^@ROU)[TEXT S LN(0)=+"1true" Q
 Q LN(0)
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RSOURCE ;;; Returns source of routine (provide routine name as ARRAY(0))
 D ZRSOURCE(.RESULT,ARRAY(0))
 Q
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RSAVE ;;; Saves Routine (provide RoutineName^LineCount as ARRAY(0))
 N X,XCN,DIE,CNT,ROU,ZZRNAME,ZZCOUNT,FLG
 S ZZRNAME=$P(ARRAY(0),"^"),ZZCOUNT=$P(ARRAY(0),"^",2)
 S RESULT(0)="1^PROBLEM WITH ROUTINE NAME or LINE COUNT: "_ARRAY(0)
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
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SYMTAB ;;; Returns the current symbol table
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
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VERSION ;;; Version of this RPC
 S RESULT(0)="1",RESULT(1)=$P($T(+2),";",3)
 Q
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; Checksums Tags ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RCHKSUM ;;; Returns routine list with checksums based on provided target. 
 N I,J
 F I=1:1:ARRAY(0) D
 . S X=ARRAY(I)
 . X ^%ZOSF("TEST")
 . I $T X ^%ZOSF("RSUM") S RESULT(I)=X_"^"_Y_"^"_$$ZLOAD2L(X)
 . I '$T S RESULT(I)=X_"^?"
 S RESULT(0)=ARRAY(0)_"^rtName~rtChecksum~rtLine~rtLine"
 Q
ZLOAD2L(X)  ;Load routine first lines
 N DIF,XCNP,R K ^TMP($J)
 S DIF="^TMP($J,",XCNP=0,R="" X ^%ZOSF("LOAD")
 I $D(R) S R=$G(^TMP($J,1,0))_"~"_$G(^TMP($J,2,0))
 K ^TMP($J)
 Q R
 ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RCHKSUM2 ;;; Returns checksum for routines found by RLIST
 D RLIST
 N I,NAME
 F I=1:1:RESULT(0) D
 . S NAME=$P(RESULT(I),U,1)
 . S RESULT(I)=$$ZRCHKSUM(NAME)
 Q
ZRCHKSUM(NAME) ;Returns checksum of routine 
 N I,J,R
 S J=ARRAY(0)
 S X=NAME
 X ^%ZOSF("TEST")
 I $T X ^%ZOSF("RSUM") S R=X_"^"_Y_"^"_$$ZLOAD2L(X)
 I '$T S R=X_"^?"
 Q R