! A fortran95 program for G95
! By WQY
program geo_gen_2D
  implicit none
   REAL, DIMENSION(:), ALLOCATABLE :: XC, XV
   REAL, DIMENSION(:,:), ALLOCATABLE :: YC, YV
   REAL, parameter :: pi=3.141593,dominio=3.0
   REAL :: dx,dy
   real :: h,h_g=1
   real :: NXC=0,NYC=0
   real :: NXC_r,NYC_r
   integer :: i,j,re_i




   write(*,*) "Quante celle vuoi lungo x???"
   read(*,*)NXC

   write(*,*)"Quante celle vuoi lungo y???"
   read(*,*)NYC

   write(*,*)"Inserisci altezza massima profilo : 0 < h < 1"
   read(*,*)h
   !write(*,*)"Inserisci altezza Dominio esterno, ricorda almeno 10 volte h piccolo"
   !read(*,*) h_g

   NXC_r=NXC-1
   NYC_r=NYC-1


   allocate(XC(0:NXC_r)) !l'indice va  da 0 a NC-1... ma attenzione la dimensione e sempre NC
   allocate(XV(0:NXC))
   allocate(YC(0:NXc_r,0:NYC_r))
   allocate(YV(0:NXC,0:NYC))



  !------------------------Definizione vettore dei Vertici lungo x-------------------------!
   dx=dominio/NXC  !3 vuol dire 1+1+1, dove il primo 1 è avanti al profilo , 1 e la corda  e 1 e il pezzo di dientro
   do i=0,NXC
    XV(i)=i*dx
   end do
   !-----------------------Definizione centri cella lungo x----------------------------------!
  do i=0,NXC-1
    XC(i)=XV(i)+dx*0.5
  end do

  !------------------------Definizione Vertici e centri lungo y-------------------------------!
do i=0,NXC
    if  ((XV(i).lt.1 ).or. (XV(i).gt.2)) then
        dy=h_g/NYC
         do j=0,NYC
         YV(i,j)=j*dy
           end do
           do j=0,NYC-1
            YC(i,j)=YV(i,j)+dy*0.5
          end do

    else
        dy=(h_g-h*sin((XV(i)-1)*pi))/NYC

    !---------------------Ciclo per i vertici delle celle---------------!
    do j=0,NYC
      YV(i,j)=h*sin(((XV(i)-1)*pi))+j*dy
    end do

    !--------------------Ciclo per i centri cella-----------------------!
    do j=0,NYC-1
        YC(i,j)=YV(i,j)+dy*0.5
    end do
end if

end do

 re_i=system("del punti_griglia_vertici.dat ")
 OPEN(UNIT=1,file= 'punti_griglia_vertici.dat') !,type=unknown)
 !write(1,*) "Coord_x Coord_y "

 do i=0,NXC
    do j=0,NYC
        write(1,*)XV(i),YV(i,j)
    end do
 end do

 close(1)
end
