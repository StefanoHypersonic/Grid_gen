
SUBROUTINE READ_INPUT

    USE GRID
	USE GEOM_2D_TYPES
	USE GEOM_2D
	USE FILENAMES

	INTEGER LENFILE

	 OPEN(UNIT=1,FILE='input.dat')

     READ(1,*)NXC
	 READ(1,*)NYC

     READ(1,*)H_G
     READ(1,*)H_P

	READ(1,*)FILE_NAME
    CLOSE(1)

!***************************This will be the grid output*****************************!
    LENFILE = LEN(TRIM(FILE_NAME))
      OUT_FILE(1:5)='grid_'
      OUT_FILE(6:5+LENFILE)=FILE_NAME
      OUT_FILE(6+LENFILE:6+LENFILE+4)='.plt'
      OUT_FILE=TRIM(OUT_FILE)
!***************************THis is the confirmation of what I read******************!
      LOG_FILE(1:4)='out_'
      LOG_FILE(5:4+LENFILE)=FILE_NAME
	LOG_FILE(5+LENFILE:5+LENFILE+4)='.log'
      LOG_FILE=TRIM(LOG_FILE)


   OPEN(UNIT=123,FILE=LOG_FILE,STATUS='UNKNOWN')
    WRITE(*,*)'THESE ARE THE INPUT DATA I READ:'
	WRITE(123,*)'THESE ARE THE INPUT DATA I READ:'
	WRITE(*,*)'Grid size:'
	WRITE(123,*)'Grid size:'
	WRITE(*,*)'NXC=',NXC
	WRITE(123,*)'NXC=',NXC
	WRITE(*,*)'NYC=',NYC
	WRITE(123,*)'NYC=',NYC
    WRITE(*,*)'Conduct height:'
    WRITE(123,*)'Conduct height:'
	WRITE(*,*)H_G
	WRITE(123,*)H_G
	WRITE(*,*)'Throat height:'
	WRITE(123,*)'Throat height:'
	WRITE(*,*)H_P
	WRITE(123,*)H_P
  CLOSE(123)


END SUBROUTINE
