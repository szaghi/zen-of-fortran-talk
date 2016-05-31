program array_computations
integer, parameter              :: elems_number=10
real, parameter                 :: delta=1.0/elems_number
real, dimension(0:elems_number) :: abscissa
real, dimension(0:elems_number) :: temperature
integer                         :: e

! array computations
abscissa = [(delta*e, e=0, elems_number)]
temperature = sin(abscissa)/2.0

print "(A)", "Abscissa, Temperature"
do e=0, elems_number
  print "(E15.6, 1X, E15.6)", abscissa(e), temperature(e)
enddo
end program array_computations
