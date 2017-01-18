module qnewton
use initdata
use qnewton_func
use functions
!! use imslf77

implicit none
!!! 
!!! wrapper for the IMSL function DUMING
!!!
!!! qnewton_fcn
!!! qnewton_grad ... see below
private
integer :: N !! dimension of the problem
real(precis), dimension (:), allocatable :: XGUESS, SOL, GRAD_SOL, DFPRED !! initial guess, solution
real(precis), dimension (:), allocatable :: XSCALE,FSCALE !! set it to 1
integer, dimension(7) :: IPARAM
real(precis), dimension(7) :: RPARAM
real(precis) :: FVALUE, GRADTL
integer :: MAXFN


public qnewton_init, qnewton_relax, qnewton_hess_relax, qnewton_dumcgg, qnewton_dumpol, &
qnewton_dumidh

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine qnewton_init
implicit none


N=NTOTALVAR

allocate(XGUESS(N))
allocate(SOL(N))
allocate(XSCALE(N))
allocate(FSCALE(N))
allocate(DFPRED(N))
allocate(GRAD_SOL(N))


GRADTL=dTOL
MAXFN=4000

FSCALE=1.0d0
XSCALE=1.0d0

IPARAM(1) =  0 !! default values

!! call DU4INF (IPARAM, RPARAM)

!! IPARAM(3)=1000
!! IPARAM(4)=4000
!! IPARAM(5)=4000





XGUESS=0.0d0
SOL=0.0d0
FVALUE=-1.0d0

!! call qnewton_check

return
end subroutine qnewton_init

subroutine qnewton_relax(myg)
implicit none
real(precis), dimension(NOB,dim,0:steps+1), intent(INOUT) :: myg

call s_flatten(myg,XGUESS)

call DUMING ( qnewton_fcn, qnewton_grad, N , XGUESS, XSCALE, FSCALE, IPARAM, RPARAM, SOL, FVALUE ) 

call s_emboss(SOL,myg)

return 
end subroutine qnewton_relax





subroutine qnewton_hess_relax(myg)
implicit none
real(precis), dimension(NOB,dim,0:steps+1), intent(INOUT) :: myg

call s_flatten(myg,XGUESS)

call DUMIAH ( qnewton_fcn, qnewton_grad, qnewton_hess, N , XGUESS, XSCALE, FSCALE, IPARAM, RPARAM, SOL, FVALUE ) 

call s_emboss(SOL,myg)

return 
end subroutine qnewton_hess_relax



subroutine qnewton_dumidh(myg)
implicit none
real(precis), dimension(NOB,dim,0:steps+1), intent(INOUT) :: myg

call s_flatten(myg,XGUESS)

call DUMIDH ( qnewton_fcn, qnewton_grad, N , XGUESS, XSCALE, FSCALE, IPARAM, RPARAM, SOL, FVALUE ) 

call s_emboss(SOL,myg)

return 
end subroutine qnewton_dumidh

subroutine qnewton_dumcgg(myg)
implicit none
real(precis), dimension(NOB,dim,0:steps+1), intent(INOUT) :: myg


call s_flatten(myg,XGUESS)

myg=grad_action(myg)
call s_flatten(myg,DFPRED)

call DUMCGG ( qnewton_fcn, qnewton_grad, N , XGUESS, GRADTL, MAXFN, DFPRED, SOL,GRAD_SOL, FVALUE ) 

call s_emboss(SOL,myg)

return 
end subroutine qnewton_dumcgg


subroutine qnewton_dumpol(myg)
implicit none
real(precis), dimension(NOB,dim,0:steps+1), intent(INOUT) :: myg
real(precis) :: S, FTOL
S=1.0d0
FTOL=dTOL

call s_flatten(myg,XGUESS)

call DUMPOL ( qnewton_fcn,  N , XGUESS, S, FTOL, MAXFN, SOL, FVALUE ) 

call s_emboss(SOL,myg)

return 
end subroutine qnewton_dumpol



subroutine qnewton_check
real(precis), dimension(NTOTALVAR) :: tmpx, grad_tmpx, grad_tmpx_num
real(precis), dimension(NTOTALVAR,NTOTALVAR) :: hess_tmpx, hess_tmpx_num
integer, dimension(NTOTALVAR) :: info
integer, dimension(NTOTALVAR,NTOTALVAR) :: hess_info
integer :: LDINFO
integer :: i,j
real(precis) :: FC

LDINFO=NTOTALVAR

call  random_number(tmpx)
call qnewton_grad(NTOTALVAR, tmpx,grad_tmpx)
call qnewton_fcn(NTOTALVAR, tmpx, FC)
call DFDGRD( qnewton_fcn, NTOTALVAR, tmpx, XSCALE, FC, dTOL, grad_tmpx_num)

call DCHGRD (qnewton_fcn, qnewton_grad,NTOTALVAR, tmpx,info)


write (unit=0,fmt=*) "checking gradient..."
do i=1,NTOTALVAR
	if (info(i) == 0) then
	write (unit=0,fmt=*) "info(", i, "): ", grad_tmpx(i) - grad_tmpx_num(i)
	end if
end do



call DFDHES ( qnewton_fcn, NTOTALVAR, tmpx, XSCALE, FC, dTOL, hess_tmpx_num, NTOTALVAR)
call qnewton_hess(NTOTALVAR, tmpx, hess_tmpx, NTOTALVAR)

call DCHHES (qnewton_grad, qnewton_hess, NTOTALVAR, tmpx, hess_info, LDINFO)

write (unit=0,fmt=*) "checking hessian..."
do i=1,NTOTALVAR; do j=1,NTOTALVAR
	if (hess_info(i,j) == 0) then
	write (unit=0,fmt=*) "Hinfo(", i, ",",j,"): ", hess_tmpx(i,j) - hess_tmpx_num(i,j)
        write (unit=0,fmt=*) "-->", hess_tmpx(i,j), hess_tmpx(j,i),  ":", hess_tmpx_num(i,j), hess_tmpx_num(j,i)
	end if

end do; end do;

end subroutine qnewton_check



end module qnewton







