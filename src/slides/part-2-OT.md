### Unexpected Behaviors

#### Pay Attention to Optional Argument

$columns

$column[width:50%]

$note
$caption(none){Bad Practice}
$content[font-size:90%;]{
```fortran
subroutine print_char(this, header)
character(len=*),  intent(in) :: this
logical, optional, intent(in) :: header
if (present(header).and.header) then
  print *, 'This is the header'
endif
print *, this
end subroutine print_char
```
}
$endnote

$column[width:50%]

$note
$caption(none){Good Practice}
$content[font-size:90%;]{
```fortran
subroutine print_char(this, header)
character(len=*),  intent(in) :: this
logical, optional, intent(in) :: header
if (present(header)) then
  if (header) print *, 'This is the header'
endif
print *, this
end subroutine print_char
```
}
$endnote

$endcolumns

$note
$caption(none){Why?}
$content[font-size:100%;]{
The compiler is allowed to evaluate the header argument before the present function is evaluated. If the header argument is not in fact present an out of bounds memory reference could occur, which could cause a failure.
}
$endnote

#### Use INTENT with Consciousness

$columns

$column[width:50%]

$note
$caption(none){Bad Practice}
$content[font-size:100%;]{
```fortran
program intent_gotcha
type mytype
  integer :: x
  real    :: y
end type mytype
type (mytype) :: a
a%x = 1 ; a%y = 2.
call assign(a)
! a%y COULD BE UNDEFINED HERE
print *, a
contains
  subroutine assign(this)
  type (mytype), intent (out) :: this
  this%x = 2
  end subroutine assign
end program intent_gotcha
```
}
$endnote

$column[width:50%]

$note
$caption(none){Good Practice}
$content[font-size:100%;]{
```fortran
program intent_gotcha
type mytype
  integer :: x
  real    :: y
end type mytype
type (mytype) :: a
a%x = 1 ; a%y = 2.
call assign(a)
print *, a
contains
  subroutine assign(this)
  type (mytype), intent (out) :: this
  this%x = 2 ; this%y = 2.
  end subroutine assign
end program intent_gotcha
```
}
$endnote

$endcolumns

$note
$caption(none){Why?}
$content[font-size:100%;]{
When intent(out) is used with a derived type, any component not assigned in a procedure could become undefined on exit. For example, even though a%y was defined on entry to this routine, it could become undefined on exit because it was never assigned within the routine.
}
$endnote

#### Fortran Odds... Automatic SAVE

$columns

$column[width:50%]

$note
$caption(none){Bad Practice}
$content[font-size:90%;]{
```fortran
real function kinetic_energy(v)
real, dimension(:), intent(in) :: v
integer                        :: i
real                           :: ke = 0.0
do i = 1, size(v)
   ke = ke + v(i)**2
enddo
kinetic_energy = .5*ke
end function kinetic_energy
```
}
$endnote

$column[width:50%]

$note
$caption(none){Good Practice}
$content[font-size:90%;]{
```fortran
real function kinetic_energy(v)
real, dimension(:), intent(in) :: v
integer                        :: i
real                           :: ke
ke = 0.
do i = 1, size(v)
   ke = ke + v(i)**2
enddo
kinetic_energy = .5*ke
end function kinetic_energy
```
}
$endnote

$endcolumns

$note
$caption(none){Why?}
$content[font-size:100%;]{
A local variable that is initialized when declared has an implicit SAVE attribute. ke is initialized only the first time the function is called. On subsequent calls the old value of ke is retained.
}
$endnote
