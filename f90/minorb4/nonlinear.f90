module mod_nonlinear 
use initdata
use functions
use nonlinear_func
implicit none


private
integer :: N, ITMAX
real(precis), dimension (:), allocatable :: XGUESS, SOL, XSCALE, FSCALE, FVEC
real(precis) :: ERREL, FNORM
integer, dimension(6) :: IPARAM
real(precis), dimension(5) :: RPARAM

public nonlinear_init, nonlinear_exit, nonlinear_dneqnf, nonlinear_dneqnj, nonlinear_dneqbf, nonlinear_dneqbj


contains

!!
!!
!!
subroutine nonlinear_init
use initdata
implicit none
 !!  write (unit=0,fmt=*) "initializing mod_nonlinear workspaces...", NTOTALVAR
  N=NTOTALVAR
  allocate(XGUESS(N))
  allocate(SOL(N))
  allocate(XSCALE(N))
  allocate(FSCALE(N))
  allocate(FVEC(N))

  ITMAX=200
  ERREL=dTOL
  XGUESS=0.0d0
  SOL=0.0d0
  FNORM=0.0d0
  XSCALE=1.0d0
  FSCALE=1.0d0
  IPARAM(1)=0
  FVEC=0.0d0

return
end subroutine nonlinear_init


subroutine nonlinear_exit
implicit none
  deallocate(XGUESS)
  deallocate(SOL)
  deallocate(XSCALE)
  deallocate(FSCALE)
  deallocate(FVEC)
return
end subroutine nonlinear_exit




!!
!!
!!
subroutine nonlinear_dneqnf(myg)
implicit none
real(precis), dimension(NOB,dim,0:steps+1), intent(INOUT) :: myg
call s_flatten(myg,XGUESS)
!! write (unit=0,fmt=*) N
call DNEQNF(nonlinear_grad_dneqnf, ERREL, N, ITMAX, XGUESS, SOL, FNORM)
!! write (unit=0,fmt=*) N
call s_emboss(SOL,myg)
return
end subroutine nonlinear_dneqnf

subroutine nonlinear_dneqnj(myg)
implicit none
real(precis), dimension(NOB,dim,0:steps+1), intent(INOUT) :: myg
call s_flatten(myg,XGUESS)
call DNEQNJ(nonlinear_grad_dneqnj, nonlinear_hess_dneqnj, ERREL, N, ITMAX, XGUESS, SOL, FNORM)
call s_emboss(SOL,myg)
return
end subroutine nonlinear_dneqnj


subroutine nonlinear_dneqbf(myg)
implicit none
real(precis), dimension(NOB,dim,0:steps+1), intent(INOUT) :: myg
  call s_flatten(myg,XGUESS)
  call DNEQBF(nonlinear_grad_dneqbf, N, XGUESS, XSCALE, FSCALE, IPARAM, RPARAM, SOL, FVEC)

  call s_emboss(SOL,myg) !! HERE


end subroutine nonlinear_dneqbf

subroutine nonlinear_dneqbj(myg)
implicit none
real(precis), dimension(NOB,dim,0:steps+1), intent(INOUT) :: myg
  call s_flatten(myg,XGUESS)
  call DNEQBJ(nonlinear_grad_dneqbj, nonlinear_hess_dneqbj, N, XGUESS, XSCALE, FSCALE, IPARAM, RPARAM, SOL, FVEC)
  call s_emboss(SOL,myg)


end subroutine nonlinear_dneqbj




end module mod_nonlinear
!!!
!!!
!!!
!!!
!!!
!!!
