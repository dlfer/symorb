module nonlinear_func
use initdata
use qnewton_func
use functions
implicit none

contains


subroutine nonlinear_grad_dneqnf(X,G,N)
implicit none
integer, intent(IN) :: N 
real(precis), dimension(N), intent(IN) :: X
real(precis), dimension(N), intent(OUT) :: G
call qnewton_grad(N,X,G)
return
end subroutine nonlinear_grad_dneqnf

subroutine nonlinear_grad_dneqnj(X,G,N)
implicit none
integer, intent(IN) :: N 
real(precis), dimension(N), intent(IN) :: X
real(precis), dimension(N), intent(OUT) :: G
call qnewton_grad(N,X,G)
return
end subroutine nonlinear_grad_dneqnj


subroutine nonlinear_grad_dneqbf(N,X,G)
implicit none
integer, intent(IN) :: N 
real(precis), dimension(N), intent(IN) :: X
real(precis), dimension(N), intent(OUT) :: G
call qnewton_grad(N,X,G)
return
end subroutine nonlinear_grad_dneqbf


subroutine nonlinear_grad_dneqbj(N,X,G)
implicit none
integer, intent(IN) :: N 
real(precis), dimension(N), intent(IN) :: X
real(precis), dimension(N), intent(OUT) :: G
call qnewton_grad(N,X,G)
return
end subroutine nonlinear_grad_dneqbj




subroutine nonlinear_hess_dneqnj(N,X,H)
implicit none
integer, intent(IN) :: N 
real(precis), dimension(N), intent(IN) :: X
real(precis), dimension(N,N), intent(OUT) :: H
call qnewton_hess(N,X,H,N)
return
end subroutine nonlinear_hess_dneqnj







subroutine nonlinear_hess_dneqbj(N,X,H,LDFJAC)
implicit none
integer, intent(IN) :: N,LDFJAC 
real(precis), dimension(N), intent(IN) :: X
real(precis), dimension(N,N), intent(OUT) :: H
call qnewton_hess(N,X,H,LDFJAC)
return
end subroutine nonlinear_hess_dneqbj



end module nonlinear_func
