SUBROUTINE GRID2D_TRASF

    USE GEOM_2D
    USE GEOM_2D_TYPES
    USE GRID

    !----------AUXILIARY VARIABLES ------------------!

    INTEGER  :: IPT,IPT1,IPT2,IPT3,IPT4               ! INDEX FOR VERTICES
    INTEGER  :: NX, NY                              ! NUMBER OF CELLS ON X AND Y
    INTEGER  :: NC, NS, NCM,NCP                       ! NUMBER OF  CELL AND
    ! MATRIX OF THE Y-COORD OF CENTER CELL AND VERTICES

     ! TRANSFORMED VECTOR OF THE ETA-COORD OF CENTER CELL AND VERTICES
    real, parameter :: pi=3.14


!----------------TEMP ALLOCATION,WILL BE REMONED IN THE FUTURE RELEASE----------------!
    !ALLOCATABLE(XC(NXC,NYC))          !-----------------> POINTING TO CL%XC(1)
    !ALLOCATABLE(XV(NXC+1,NYC+1))      !-----------------> POINTING TO BF%IPT(1)
    !ALLOCATABLE(YC(NXC,NYC))          !-----------------> POINTING TO CL%XC(2)
    !ALLOCATABLE(YV(NXC+1,NYC+1))      !-----------------> POINTING TO BF%IPT(2)

    !ALLOCATABLE(CHIC(NXC))            !-----------------> POINTING TO TCL%XC(1)
    !ALLOCATABLE(CHIV(NXC+1))          !-----------------> POINTING TO TBF%IPT(1)
    !ALLOCATABLE(ETAC(NYC))            !-----------------> POINTING TO TCL%XC(2)
    !ALLOCATABLE(ETAV(NYC+1))          !-----------------> POINTING TO TBF%IPT(2)


!******************************GRID NODES ******************************************!

IPT = 0
 DO NY=0,NYC
    DO NX=0,NXC
        IPT = IPT+1
        PT(IPT,1)= 3*DFLOAT(NX)/DFLOAT(NXC)  ! X-COORD
        IF ((PT(IPT,1) .LE. 1) .OR. (PT(IPT,1) .GE. 2)) THEN
        PT(IPT,2)= DFLOAT(NY)/DFLOAT(NYC)   ! Y-COORD
        ELSE
        Y_DIV=(H_G-H_P*SIN((PT(IPT,1)-1)*pi))/DFLOAT(NYC)           ! control on the first row of the mesh
         IF (NY .EQ. 0) THEN                                          ! in every line the upper point has the index j+NXC
          PT(IPT,2) = H_P*SIN((PT(IPT,1)-1)*pi)
         ELSE
           PT(IPT,2) =  PT(NX+NXC,2)+Y_DIV
         END IF
        END IF
    END DO
 END DO

IPT=0
DO NY=0,NYC
    DO NX=0,NXC
                                                            !               ----
        IPT = IPT+1                                         !             /      \
        TPT(IPT,1) = PT(IPT,1)/(3.D0)                       !            /        \
        bx = max(0D0,(H_P*sin(pi*(PT(IPT,1)-1))))              ! b_x= -----/          \---------, the curvature is sin-like
        TPT(IPT,2) = (PT(IPT,2)- bx)/(1-bx)
    END DO
END DO

!----------INDEX PROCESS STARTED------------------------------------------------!
!STRUCTURED AND UNSTRUCTURED GRID INDEX CAN  BE USED......
!       ^
!    NY |
!       |
!       |
!       |        NX
!       +-------->
!**********************CELLs***************************!
  NC = 0
  DO NY=1,NYC
    DO NX=1,NXC

       !--------CELL CENTRE INDEXING IN THE X-Y AND CHI,ETA---------!
       NC=NC+1
       CL(NC)%ICC(1) = NX
       CL(NC)%ICC(2) = NY


       TCL(NC)%ICC(1) = NX
       TCL(NC)%ICC(2) = NY

       IC(NX,NY) = NC

       !--------FINDING VERTICES INDEXING------------!
       CALL FIND_VERTPTS(NY-1,NX-1,IPT1)
       CALL FIND_VERTPTS(NY-1,NX  ,IPT2)
       CALL FIND_VERTPTS(NY  ,NX  ,IPT3)
	   CALL FIND_VERTPTS(NY  ,NX-1,IPT4)
       !--------ASSIGNING THE EXACT INDEX------------!
       CL(NC)%IPT(1) =IPT1      !        3
       CL(NC)%IPT(2) =IPT2      !  4+--------+3
       CL(NC)%IPT(3) =IPT3      !   |        |
       CL(NC)%IPT(4) =IPT4      !   |4      2|
                                !   |        |
                                !  1+--------+2
       TCL(NC)%IPT(1) =IPT1             ! 1
       TCL(NC)%IPT(2) =IPT2
       TCL(NC)%IPT(3) =IPT3
       TCL(NC)%IPT(4) =IPT4

    END DO
  END DO

!**********************INTERNAL SURFACE***************************!







!*********************BORDER WITH NORMAL UNIT VECTOR IN DIRECTION X AND CHI**********************!
NS = 0
DO NY=1,NYC
    DO NX=1,NXC-1
        NS  = NS+1
        NCM = IC(NX,NY)
        NCP = IC(NX+1,NY)

        BF(NS)%ICM=NCM
        BF(NS)%ICP=NCP

        TBF(NS)%ICM=NCM
        TBF(NS)%ICP=NCP

        BF(NS)%IPT(1) = CL(NCM)%IPT(2)
        BF(NS)%IPT(2) = CL(NCM)%IPT(3)

        TBF(NS)%IPT(1) = CL(NCM)%IPT(2)
        TBF(NS)%IPT(2) = CL(NCM)%IPT(3)

        BF(NS)%IDIRP   = 1
        BF(NS)%IDIRM   = 1

        TBF(NS)%IDIRP   = 1
        TBF(NS)%IDIRM   = 1

        TCL(NCM)%IS(2) = NS
        TCL(NCP)%IS(4) = NS
        TCL(NCM)%ISD(2) = 1
        TCL(NCP)%ISD(4) =-1

    END DO
END DO

NS = 0
!****************************SURFACES WITH NORMAL UNIT VECTOR IN THE Y-DIRECTION**********************
	DO NX=1,NXC
		DO NY=1,NYC-1
			NCM = IC(NX,NY)
			NCP = IC(NX,NY+1)
			NS=NS+1

			BF(NS)%ICM=NCM
			BF(NS)%ICP=NCP

			TBF(NS)%ICM=NCM
			TBF(NS)%ICP=NCP

			BF(NS)%IPT(1) = CL(NCM)%IPT(3)
			BF(NS)%IPT(2) = CL(NCM)%IPT(4)

			TBF(NS)%IPT(1) = TCL(NCM)%IPT(3)
			TBF(NS)%IPT(2) = TCL(NCM)%IPT(4)

			BF(NS)%IDIRP   = 2
			BF(NS)%IDIRM   = 2

			TBF(NS)%IDIRP   = 2
			TBF(NS)%IDIRM   = 2

			CL(NCM)%IS(3) = NS
			CL(NCP)%IS(1) = NS

            TCL(NCM)%IS(3) = NS
			TCL(NCP)%IS(1) = NS

			CL(NCM)%ISD(3) = 1
			CL(NCP)%ISD(1) =-1

			TCL(NCM)%ISD(3) = 1
			TCL(NCP)%ISD(1) =-1


		ENDDO
	ENDDO



































    RETURN
END SUBROUTINE

! **************************************************************************
      SUBROUTINE FIND_VERTPTS(N,M,IPT)

	USE GEOM_2D


	INTEGER IPT,N,M     ! N IS IN THE Y-DIRECTION
                        ! M IS IN THE X-DIRECTION

	IPT=(M+1)+N*(NXC+1)

	RETURN
	END
! **************************************************************************
