! -------------------------------------------------------------------------------
! -                    UGENS Subroutine for Static simulation                   -
! -                                                                             -
! -                            Mohammadreza Moeini                              -                                                                          *
! -                   			    RunModelX                                   -
! -                                                                             -
! -                      Objective: Simple elastic model                        -
! -                 		     UNITS: MPa., mm        					    -
! -------------------------------------------------------------------------------

      SUBROUTINE UGENS(DDNDDE,FORCE,STATEV,SSE,SPD,PNEWDT,STRAN,
     1 DSTRAN,TSS,TIME,DTIME,TEMP,DTEMP,PREDEF,DPRED,CENAME,NDI,
     2 NSHR,NSECV,NSTATV,PROPS,JPROPS,NPROPS,NJPROP,COORDS,CELENT,
     3 THICK,DFGRD,CURV,BASIS,NOEL,NPT,KSTEP,KINC,NIT,LINPER)
C
      INCLUDE 'ABA_PARAM.INC'

C
      CHARACTER*80 CENAME
      DIMENSION DDNDDE(NSECV,NSECV),FORCE(NSECV),STATEV(NSTATV),
     1 STRAN(NSECV),DSTRAN(NSECV),TSS(2),TIME(2),PREDEF(*),
     2 DPRED(*),PROPS(*),JPROPS(*),COORDS(3),DFGRD(3,3),
     3 CURV(2,2),BASIS(3,3)
	 
	 
	!=======================================================================
	!							Variable Decleration
	!======================================================================= 
	! Properties of the shell --------------------------------------------
	 REAL*8 :: Cij ![N/mm] Tensile and bending stiffness
	 	 
	!=======================================================================
	!							Get the perameters
	!=======================================================================
	 Cij = PROPS(1)
	 	 
	!=======================================================================
	!		Compute force resultants and its derivative (forceNew, ddndde)
	!=======================================================================
	 
	 Do K1=1, NSECV
		FORCE(K1)=0.0d0
	 Do K2=1, NSECV 
		DDNDDE(K1,K2)=0.0d0
	 ENDDO
	 ENDDO
	
	 Do K1=1, NSECV
	  	DDNDDE(K1,K1)=Cij
	 ENDDO
	
	 Do K1=1, NSECV
		FORCE(K1)=(Cij)*(STRAN(K1) + DSTRAN(K1))
   	 ENDDO
	 

	!=======================================================================
	!				Store the results in ddndde & forceNew
	!=======================================================================
	 STATEV(1)= FORCE(1)
		 
	 
	 	 
	! --------------------------------------------------------------------
	!			Print what you need (it will be saved in .log file)
	! --------------------------------------------------------------------
	 PRINT *, "-------------------------------------"
	 PRINT *, "COORDS=", COORDS
	 PRINT *, "SHAPE(DDNDDE)=", SHAPE(DDNDDE)
	 PRINT *, "epsilon_11=", epsilon_11
	 PRINT *, "NSECV=", NSECV
	 PRINT *, "DSTRAN(1)=", DSTRAN(1)
	 
	 

     
      RETURN
      END
