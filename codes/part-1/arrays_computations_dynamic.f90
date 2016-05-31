program array_computations_dynamic
integer                         :: elems_number
real                            :: delta
real, allocatable, dimension(:) :: abscissa
real, allocatable, dimension(:) :: temperature
integer                         :: e

! input from user
print "(A)", "Enter the number of elements"
read*, elems_number
delta=1.0/elems_number

! allocate arrays
allocate(abscissa(0:elems_number))
allocate(temperature(0:elems_number))

! array computations

! array constructor
abscissa = [(delta*e, e=0, elems_number)]

! elemental function operating on each array element
temperature = sin(abscissa)/2.0

print "(A)", "Abscissa, Temperature"
do e=0, elems_number
  print "(E15.6, 1X, E15.6)", abscissa(e), temperature(e)
enddo
end program array_computations_dynamic
