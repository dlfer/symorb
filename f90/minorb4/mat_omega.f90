module mod_mat_omega
  use initdata
  use pars
  implicit none

  real(precis), dimension(3,3) :: mat_omega, mat_omega_square

  contains

  subroutine def_mat_omegas
     mat_omega(1,1)=0.0d0
     mat_omega(1,2)=-omega(3)
     mat_omega(1,3)=omega(2)
     mat_omega(2,1)=omega(3)
     mat_omega(2,2)=0.0d0
     mat_omega(2,3)=-omega(1)
     mat_omega(3,1)=-omega(2)
     mat_omega(3,2)=omega(1)
     mat_omega(3,3)=0.0d0

     mat_omega_square = matmul(mat_omega,mat_omega)
  end subroutine def_mat_omegas
end module mod_mat_omega
