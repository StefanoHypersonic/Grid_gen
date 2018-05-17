 SUBROUTINE WRITE_PLT

    USE GEOM_2D_TYPES
	USE GEOM_2D

    USE FILENAMES

    character titolo*60
    character*5 grid
    character*160 variabili
    character gridplt*60

    gridplt="gridtk_"//FILE_NAME//".plt"


    open(45,file=gridplt,status="unknown")
    rewind(45)
    ib=0

    titolo='TITLE="'//FILE_NAME//'"'
        write(45,'(a60)') titolo

    variabili='VARIABLES="X","Y"'

        write(45,'(a160)') variabili

	  ib=ib+1
        write (grid,fmt='(i5)') ib
           write (45,*) 'ZONE T="' ,grid , '",' ,',I=' , NXC+1,',J=' , NYC+1,',F="POINT"'


            DO N=1,NPT
			  write (45,*) PT(n,1),PT(n,2)
            END DO


 END SUBROUTINE
