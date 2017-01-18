module mod_kinetic
  use initdata
  use functions
  
  implicit none
  
  contains

function kinetic_energy (mygamma)
real(precis), dimension (NOB,dim,0:steps+1), intent(IN) :: mygamma
real(precis), dimension (NOB,dim,0:steps+1) :: grad
real(precis) :: kinetic_energy

grad = grad_kinetic_energy(mygamma)
kinetic_energy = 0.5d0*prodotto_scalare(grad,mygamma)
return
end function kinetic_energy

function grad_kinetic_energy (mygamma)
real(precis), dimension (NOB,dim,0:steps+1), intent(IN) :: mygamma
real(precis), dimension (NOB,dim,0:steps+1) :: grad_kinetic_energy

grad_kinetic_energy = lin_appl_st(bigmat_K,mygamma) 

return
end function grad_kinetic_energy

  
end module mod_kinetic
