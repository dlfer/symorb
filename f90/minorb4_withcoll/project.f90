module mod_project
use initdata
implicit none

contains

subroutine center(conf)
  real(precis),dimension(NOB,dim), intent(INOUT) :: conf
  real(precis),dimension(dim) :: centerofmass
  integer :: i
  centerofmass=0.0d0
  do i=1,NOB
     centerofmass=centerofmass+m(i)*conf(i,:)
  end do
  centerofmass=centerofmass/sum(m)
  do i=1,NOB
     conf(i,:) = conf(i,:) - centerofmass(:)
  end do
end subroutine center

subroutine moltiplica(matrix,vector,mulmatvec)
  real(precis),dimension(dim,dim), intent(IN) :: matrix
  real(precis), dimension(dim) , intent (IN):: vector
  real(precis), dimension(dim) , intent (OUT) ::  mulmatvec
  integer i,j
  do i=1,dim
    mulmatvec(i)=0.0d0
    do j=1,dim
      mulmatvec(i)=mulmatvec(i) + matrix(i,j) * vector(j)
    end do
  end do
end subroutine moltiplica

subroutine s_sourceproj(matrix,permutation,conf,sourceproj)
  implicit none
  real(precis),dimension(NOB,dim),intent(IN) :: conf
  real(precis),dimension(NOB,dim),intent(OUT) :: sourceproj! needed for the force
  real(precis),dimension(dim,dim),intent(IN) :: matrix
  integer, dimension (NOB),intent(IN) :: permutation
  !! real(precis),dimension(dim) :: point
  integer i
  sourceproj=0.0d0
  do i=1,NOB
     call moltiplica (matrix, conf(permutation(i),:)  , sourceproj(i,:) )
  end do
end subroutine s_sourceproj

subroutine project_onto_kerT(conf)
  real(precis),dimension(NOB,dim), intent(INOUT) :: conf
  real(precis),dimension(NOB,dim) :: dtmpconf, dconf
  integer :: i
  dtmpconf=conf
  conf=0.0d0
  do i=1,sizekerT
    call s_sourceproj(matrix(i,:,:),perm(i,:),dtmpconf, dconf)
    conf=conf + 1.0d0/real(sizekerT,precis) * dconf
  end do
end subroutine project_onto_kerT



subroutine project_onto_first(conf)
  real(precis),dimension(NOB,dim), intent(INOUT) :: conf
  real(precis),dimension(NOB,dim) :: dconf
  if (action_type == 0) then
    write (unit=0,fmt=*) "FATAL ERROR: project_onto_first not defined for action_type= ", action_type
    stop
  end if
  call s_sourceproj(first_matrix(:,:),first_perm(:),conf, dconf)
  conf=0.5d0 * ( conf+dconf )
end subroutine project_onto_first

subroutine project_onto_second (conf)  
  real(precis),dimension(NOB,dim), intent(INOUT) :: conf
  real(precis),dimension(NOB,dim) :: dconf
  if (action_type == 0) then
   write (unit=0,fmt=*) "FATAL ERROR: project_onto_second not defined for action_type= ", action_type
   stop
  end if

  call s_sourceproj(second_matrix(:,:),second_perm(:),conf, dconf)
  conf=0.5d0 * ( conf+dconf )

end subroutine project_onto_second

subroutine project(mygamma)
real(precis), dimension(NOB,dim,0:steps+1), intent(INOUT) :: mygamma
real(precis), dimension(NOB,dim) ::  conf1,conf2, symmconf1,symmconf2
integer :: k

do k=0,steps+1
call center(mygamma(:,:,k))
call project_onto_kerT(mygamma(:,:,k))
end do

if ((action_type .EQ. 2) .OR. (action_type .EQ. 1)) then 

    conf1(:,:) = mygamma(:,:,0)
    conf2(:,:) = mygamma(:,:,steps+1)     
    call s_htildeone(mygamma(:,:,0),symmconf1(:,:))
    call s_htildetwo(mygamma(:,:,steps+1),symmconf2(:,:))
    mygamma(:,:,0) = 0.5d0 *( conf1(:,:) + symmconf1(:,:) )
    mygamma(:,:,steps+1) = 0.5d0 *( conf2(:,:) + symmconf2(:,:) )


else if ( action_type .EQ. 0 ) then 

    conf1(:,:) = mygamma(:,:,0)   
    conf2(:,:) = mygamma(:,:,steps+1)     
    call inverse_rotation(conf2,symmconf2)
    call s_rotation(conf1,symmconf1) 
    mygamma(:,:,0) = 0.5d0 * (conf1(:,:) + symmconf2(:,:))
    mygamma(:,:,steps+1) = 0.5d0 * (conf2(:,:) + symmconf1(:,:))

else
   write (unit=0,fmt=*) "FATAL ERROR: action type ", action_type, " not understood!\n"
   stop
end if

return 
end subroutine project



subroutine s_htildeone(conf,htildeone)
real(precis),dimension(NOB,dim),intent(IN) :: conf
real(precis),dimension(NOB,dim),intent(OUT) :: htildeone! needed for the force
if (action_type == 0) then
write (unit=0,fmt=*) "FATAL ERROR: s_htildeone not defined for action_type= ", action_type
stop
end if
call s_sourceproj(first_matrix,first_perm,conf,htildeone)
end subroutine s_htildeone


subroutine s_htildetwo(conf,htildetwo)
real(precis),dimension(NOB,dim),intent(IN) :: conf
real(precis),dimension(NOB,dim),intent(OUT) :: htildetwo
if (action_type == 0) then
write (unit=0,fmt=*) "FATAL ERROR: s_htildetwo not defined for action_type= ", action_type
stop
end if
call s_sourceproj(second_matrix,second_perm,conf,htildetwo)
end subroutine s_htildetwo


subroutine s_cyclic_rotation(conf,power,rotated_conf)
  real(precis),dimension(NOB,dim) , intent(IN):: conf
  integer, intent (IN) :: power
  real(precis), dimension(NOB,dim) , intent(OUT) :: rotated_conf

  if (cyclic_order>1) then
    call s_sourceproj( cyclic_matrix(power,:,:) , cyclic_perm(power,:) , conf , rotated_conf )
  else 
    rotated_conf = conf
  end if    
  
end subroutine s_cyclic_rotation


subroutine s_rotation(conf,rotation)
  real(precis),dimension(NOB,dim) , intent(IN):: conf
  real(precis),dimension(NOB,dim) ,intent(OUT):: rotation
  !! real(precis),dimension(NOB,dim) :: tmpconf
!!! call s_htildeone(conf,tmpconf)
!!! call s_htildetwo(tmpconf, rotation)
!!! TODO
!! call s_sourceproj( (cyclic_matrix(1,:,:)) , cyclic_perm(1,:) , conf , rotation )
  call s_cyclic_rotation( conf , 1 , rotation)
end subroutine s_rotation

subroutine inverse_rotation(conf,rotation)
  real(precis),dimension(NOB,dim) , intent(IN):: conf
  real(precis),dimension(NOB,dim) ,intent(OUT):: rotation
  real(precis),dimension(NOB,dim) :: tmpconf
!! call s_htildetwo(conf,tmpconf)
!! call s_htildeone(tmpconf, rotation)
!!! TODO!!!
  if (cyclic_order>1) then
    call s_cyclic_rotation( conf , (cyclic_order-1) , tmpconf )
  else 
    rotation = conf
  end if    
  rotation=tmpconf
end subroutine inverse_rotation                                                 
end module mod_project
