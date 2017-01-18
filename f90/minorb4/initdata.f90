module initdata

implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Fixed parameters.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! precision
integer, parameter :: precis=kind(1.0D0)
real(precis), parameter :: epsilon=1.0D-6, &
                          & smallepsilon=1.0D-12 , &
                          HOWSOLUTIONEPSILON=epsilon, &
                          GOLD=1.618033988749895, &
                          GLIMIT=100.0,&
                          TINY=1.d-20,&
                          CGOLD=0.3819660112501053,&
                          ZEPS=1.0d-20


real(precis) :: pi

integer, parameter :: fdu1=6,fdu4=6

integer :: statuscounter=0, msg, numberofsolutions
integer, parameter:: MAXSTATUSCOUNTER=100


integer :: brent_itmax=100

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Parameters...
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! number of bodies
integer :: NOB, dim, steps, phasedim , multi_phasedim, NOP, printsteps=50
integer :: NTOTALVAR

real(precis)  :: ALPHA
real(precis)  :: shift=0.d0, scale,tmpscale
real(precis)  :: sqomega, omega(3)

integer :: MAXNUMBEROFSOLUTIONS, ccgiterations, subiterations=1,&
subcount

integer:: MAXTRIES

! startingpath method
integer  :: SPMETHOD, ALG, TIPO_DI_POTENZIALE=-1
! need to find a list...                                                       
real(precis) :: timeone, timetwo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!
!!! after the CONFIG
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer  :: cyclic_order 


! masses
real(precis), allocatable, dimension(:) :: m

integer :: action_type, sizekerT, fundamental_domains 

integer, dimension(:,:), allocatable :: perm
integer, dimension(:,:), allocatable :: cyclic_perm
integer, dimension(:), allocatable :: first_perm, second_perm


real(precis), dimension(:,:,:) , allocatable :: matrix
real(precis), dimension(:,:,:) , allocatable :: cyclic_matrix
real(precis), dimension(:,:) , allocatable :: first_matrix, second_matrix 
real(precis) :: delta_time

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!
!!! non-configuration parameters and variables
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


real(precis), dimension (:,:,:,:,:,:), allocatable :: bigmat_K
real(precis), dimension (:,:), allocatable ::  sin_kth
real(precis) :: dTOL

!!!=====================================================================
!!! CONFIG
!!!=====================================================================

logical :: CHECKSOLUTION=.FALSE. , PRINTALLSOLUTIONS=.FALSE., SHOOTSOLUTION=.FALSE.
!! namelist/CONFIG/ MAXNUMBEROFSOLUTIONS, ccgiterations, SPMETHOD, &
!! & MAXTRIES, steps, ALPHA, ALG, scale,  &
!! & CHECKSOLUTION, PRINTALLSOLUTIONS, SHOOTSOLUTION
logical :: VERBOSE = .FALSE.
character(len=24) :: COMMAND
namelist/CONFIG/COMMAND,SPMETHOD,ALG,steps,ALPHA


real(precis) ::  ccg_linmin_TOL, ccg_relax_ATOL, ccg_relax_RTOL
 

contains


subroutine inizializza
use imslf77
implicit none
integer :: i,randomseed
!! real :: R1MACH

integer, allocatable :: seed0(:)
integer :: seed_size=47

if ( VERBOSE ) then
 write(unit=0,fmt=*) "# inizializza"
end if


call mytimer(timeone)

call random_seed(size=seed_size)
allocate(seed0(1:seed_size))
call random_seed ()

! set verbosity
!! call erset ( 0, 1, 0 ) !!! this print all
call erset ( 0, 0, 0 ) !!! TODO: define it properly


pi = 2.0d0*asin(1.0d0)

dTOL=SQRT(DMACH(1))
write (unit=0,fmt=*) "# dTOL=", dTOL
!! dTOL=1.0D-16
statuscounter=0
numberofsolutions=0
 !! ccg_linmin_TOL=1.0D-12 !! epsilon !! dTOL
 !! ccg_relax_RTOL=1.0D-12 !! epsilon !! dTOL
 !! ccg_relax_ATOL=dTOL    !! epsilon
 ccg_linmin_TOL=epsilon !! dTOL
 ccg_relax_RTOL=epsilon !! dTOL
 ccg_relax_ATOL=epsilon



omega=0.

!! write (unit=0,fmt='(a,a)') "reading CONFIG... "
read (unit=*,nml=CONFIG)
!! write (unit=0,fmt='(a)', advance='YES') "feeding a new random seed... "

do i=1,seed_size
call system_clock (count=randomseed)
seed0(i)=randomseed
! write (unit=0, fmt='(a20,i12,a2)', advance='YES') " [ system clock  = ", randomseed, " ]"

end do

call random_seed (put=seed0)



end subroutine inizializza

subroutine init_print_values
implicit none

write (0,*) "!!======================================================================"
write (0,*) "!! action_type          = ", action_type
write (0,*) "!! steps                = ", steps
write (0,*) "!! NOP                  = ", NOP
write (0,*) "!! SPMETHOD             = ", SPMETHOD
write (0,*) "!! ALG                  = ", ALG
write (0,*) "!! cyclic_order         = ", cyclic_order
write (0,*) "!! m                    = ", m
write (0,*) "!! MAXTRIES             = ", MAXTRIES
write (0,*) "!! ccgiterations        = ",  ccgiterations
write (0,*) "!! ALPHA                = ", ALPHA
write (0,*) "!! NOB                  = ", NOB
write (0,*) "!! dim                  = ", dim
write (0,*) "!! sizekerT             = ", sizekerT
write (0,*) "!! scale                = ", scale
write (0,*) "!! PRINTALLSOLUTIONS    = ", PRINTALLSOLUTIONS
write (0,*) "!! CHECKSOLUTION        = ", CHECKSOLUTION 
write (0,*) "!! SHOOTSOLUTION        = ", SHOOTSOLUTION 
write (0,*) "!! omega=               = ", omega 
!! write (0,*) "!! delta_time=          = ", delta_time
!! write (0,*) "!!======================================================================"


end subroutine init_print_values


subroutine init_allocate
implicit none
! AFTER init_values!!!
if ( VERBOSE ) then
 write(unit=0,fmt=*) "# init_allocate"
 end if

allocate(bigmat_K(NOB,dim,0:steps+1,NOB,dim,0:steps+1)) 
bigmat_K=0.0d0

allocate ( sin_kth(steps,NOP))

sin_kth=0.0d0

if (action_type == 1 .OR. action_type == 2) then
fundamental_domains=2*cyclic_order
else if (action_type==0) then
fundamental_domains=cyclic_order
end if

delta_time= 2.*pi/real(fundamental_domains*(steps+1),precis)
!! write (0,fmt=*) "!! delta_time           = ", delta_time
!! write (0,*) "!!======================================================================"

return
end subroutine init_allocate

subroutine status
implicit none
!! integer tmp
! real r1mach
! this subroutine write the progress status
statuscounter=statuscounter+1

if (statuscounter.eq.MAXSTATUSCOUNTER) then
        write (0,*) ""
        write (0,*) "*******************************************************"
        write (0,*) "*******************************************************"
statuscounter=0
        
end if

end subroutine status



subroutine fine
implicit none
call mytimer(timetwo)
!! write (unit=0,fmt="('END: process took ', 1pd12.6,' CPU seconds')") &
!! write (unit=0,fmt="('END: process took: ',1pd12.6,' CPU seconds')") &
!! timetwo-timeone
end subroutine fine


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11
 subroutine mytimer(ttime)
 real(precis), intent(OUT):: ttime
 integer :: tticks	
 !! real ,dimension(2) :: ta
 !! real :: temp
 !! call etime(ta,temp)
 !! ttime=ta(1)
 !! call timer(tticks) 
 tticks=100 !!!__HERE__ TEMP
 ttime = tticks / 100.d0 
 return
! CAREFUL: IMPLEMENTATION DEPENDENT
 end subroutine mytimer 


end module initdata

