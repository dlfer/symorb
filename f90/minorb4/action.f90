module mod_action
  
  use initdata
  use mod_kinetic !! kinetic energy and its grad
  use mod_potential
  implicit none
  
  contains
 
  function action(mygamma) 
    real(precis) :: action
    real(precis), dimension(NOB,dim,0:steps+1) :: mygamma
      action =  kinetic_energy(mygamma) +   UU(mygamma) !!!TEMP
  return
  end function action

  function grad_action(mygamma)
    real(precis), dimension(NOB,dim,0:steps+1) :: mygamma, grad_action
      grad_action =  grad_kinetic_energy(mygamma) +    grad_UU(mygamma)
  return  
  end function grad_action


function grad2_action(mygamma)
real(precis), dimension(NOB,dim,0:steps+1), intent(IN) :: mygamma
real(precis), dimension(NOB,dim,0:steps+1,NOB,dim,0:steps+1) :: grad2_action 

grad2_action=  bigmat_K  +  grad2_UU(mygamma)
return
end function grad2_action

end module mod_action
