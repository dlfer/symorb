module integrali
  use initdata
  implicit none

  contains

  function int_sin(k)
    implicit none
    integer :: k
    real(precis) :: int_sin
    int_sin = (1-(-1)**k) / real(k,precis)
    return
  end function int_sin

  function int_tsin(k)
    implicit none
    integer :: k
    real(precis) :: int_tsin
    int_tsin = - pi*(-1)**(k)/ real(k,precis)
    return
  end function int_tsin

  function int_tcos(k)
    implicit none
    integer :: k
    real(precis) :: int_tcos
    int_tcos =  (-1+(-1)**k)/real(k,precis)**2
  end function int_tcos

  function int_coscos(k,h)
    implicit none
    integer :: h,k
    real(precis) :: int_coscos
    if (h==k) then
       int_coscos = pi/2.
    else
       int_coscos = 0.
    end if
  end function int_coscos

  function int_sinsin(k,h)
    implicit none
    integer :: k,h
    real(precis) :: int_sinsin
    if (h==k) then
       int_sinsin = pi/2.
    else
       int_sinsin = 0.
    end if
  end function int_sinsin

  function int_sincos(k,h)
    implicit none
    integer :: k, h
    real(precis) :: int_sincos
    if ( .NOT. k == h ) then
      int_sincos = real(k,precis)/(real(k,precis)**2 - &
      & real(h,precis)**2) * (1 - (-1)**(k+h))
    else 
      int_sincos = 0.
    end if
  end function int_sincos

end module integrali
