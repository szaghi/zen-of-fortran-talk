program kinds
use, intrinsic :: iso_fortran_env, only : R8 => real64

implicit none
real,          parameter :: pi_greek_1=3.14159265358979323
real*8,        parameter :: pi_greek_2=3.14159265358979323
real(kind=R8), parameter :: pi_greek_3=atan(1.)*4.
real(kind=R8), parameter :: pi_greek_4=atan(1._R8)*4._R8

print '(E23.15E3)', pi_greek_1
print '(E23.15E3)', pi_greek_2
print '(E23.15E3)', pi_greek_3
print '(E23.15E3)', pi_greek_4

end program kinds
