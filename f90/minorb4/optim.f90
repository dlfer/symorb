module optim
use initdata
use tempfunc
use functions
use mod_project

implicit none
!!! 
!!! wrapper for the IMSL function dlcong
!!!
private
integer :: NVAR !! number of variables
integer :: NCON !! number of linear constraints
integer :: NEQ  !! number of linear equality contraints
real(precis), dimension(:,:), allocatable ::  A !! matrix of contraints
integer :: LDA !! leading dimension of A
real(precis), dimension(:), allocatable :: B !! right-hand side
real(precis), dimension(:), allocatable :: XLB 
real(precis), dimension(:), allocatable :: XUB
real(precis), dimension (:), allocatable :: XGUESS
real(precis) :: ACC
integer :: MAXFCN
real(precis), dimension(:), allocatable :: SOL
real(precis) :: OBJ
integer :: NACT
integer, dimension(:), allocatable :: IACT
real(precis), dimension(:), allocatable :: ALAMBDA


public optim_init, optim_relax

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine optim_init
implicit none
real(precis), dimension(:), allocatable :: tmpx
real(precis), dimension(NOB,dim,0:steps+1) :: tmpgamma, tmpdelta
integer :: i,j,k, index
real(precis) :: res

NVAR = NOB * dim * (steps+2)
NEQ = 2 * NOB * dim
NCON = NEQ
LDA = NCON
ACC = 1.0D-8
MAXFCN = 1000 !! maximum number of function evaluation !! TODO
OBJ=0.0d0 !! output 
NACT=0  !! output 

allocate(tmpx(NVAR))
allocate (A(NCON,NVAR))
allocate (B(NCON))
allocate (XLB(NVAR))
allocate (XUB(NVAR))
allocate (XGUESS(NVAR))
allocate (SOL(NVAR)) !! output
allocate (IACT(NCON + 2*NVAR)) !! output
allocate (ALAMBDA(NVAR)) !! output 

A=0.0d0
!! definition of A

do k=1,NVAR
	tmpx=0.0d0
	tmpx(k)=1.0d0
	call s_emboss(tmpx,tmpgamma)
	tmpdelta = tmpgamma
	call project(tmpgamma)
	tmpdelta = tmpgamma - tmpdelta
	index = 0 
	do i=1,NOB; do j=1,dim
		index = index + 1
		A(index,k) = tmpdelta(i,j,0)
	end do; end do;
	do i=1,NOB; do j=1,dim
		index = index + 1
		A(index,k) = tmpdelta(i,j,steps+1)
	end do; end do;

end do !! k

B=0.0d0

SOL=0.0d0 !! output


call random_number(tmpgamma)
call project(tmpgamma)
call s_flatten(tmpgamma,tmpx)

do i=1,NVAR; do j=1,NCON
	res = res + A(j,i) * tmpx(i)
end do; end do;

if (res > 1.0D-15) then
	write (unit=0,fmt=*) "Warning: exiting from optim_init with res= ", res
	!! stop
end if

end subroutine optim_init

subroutine optim_relax(myg)
implicit none
real(precis), dimension(NOB,dim,0:steps+1), intent(INOUT) :: myg
call s_flatten(myg,XGUESS)
XLB(:)=XGUESS(:)-0.5D0
XUB(:)=XGUESS(:)+0.5D0




call dlcong (optim_fcn, optim_grad, NVAR, NCON, NEQ, A, LDA, B, &
            XLB, XUB, XGUESS, ACC, MAXFCN, SOL, OBJ, NACT, IACT, &
	    ALAMBDA)
write (unit=0,fmt=*) "dlcong esce con OBJ = ", OBJ
call s_emboss(SOL,myg)

return 
end subroutine optim_relax


end module optim


