## Modern Fortran

### Recent Standards

#### Fortran 90/95 and beyond, the new Era

$note
$caption(none){Modern Style}
$content[font-size:120%;]{
+ free format
+ attributes
+ implicit none
+ do, exit, cycle, case
}
$endnote

$note
$caption(none){Fixing the flaws}
$content[font-size:120%;]{
+ allocatable arrays
+ derived types
}
$endnote

$note
$caption(none){Module-oriented Programming}
$content[font-size:120%;]{
+ internal subprograms
+ private, public, protected
+ contains
+ use
+ explicit interfaces
+ optional arguments & intent
}
$endnote

$note
$caption(none){Formula translation}
$content[font-size:120%;]{
+ array syntax, where statement
+ extended & user-defined operators
}
$endnote

$note
$caption(none){Odds}
$content[font-size:120%;]{
+ fortran pointers
+ command line arguments
+ environment variables
+ preprocessor
+ interoperability with C (binding)
}
$endnote

#### Fortran 2003, the most exciting features

$note
$caption(none){A lot of new features...}
$content[font-size:98%;]{
+ Derived type enhancements: parameterized derived types, improved control of accessibility, improved structure constructors, and finalizers.
+ Object oriented programming support: type extension and inheritance, polymorphism, dynamic type allocation, and type-bound procedures.
+ Data manipulation enhancements: allocatable components, deferred type parameters, VOLATILE attribute, explicit type specification in array constructors and allocate statements, pointer enhancements, extended initialization expressions, and enhanced intrinsic procedures.
+ Input/output enhancements: asynchronous transfer, stream access, user specified transfer operations for derived types, user specified control of rounding during format conversions, named constants for preconnected units, the flush statement, regularization of keywords, and access to error messages.
+ Procedure pointers.
+ Support for the exceptions of the IEEE Floating Point Standard (IEEE 1989).
+ Interoperability with the C programming language.
+ Support for international usage: access to ISO 10646 4-byte characters and choice of decimal or comma in numeric formatted input/output.
+ Enhanced integration with the host operating system: access to command line arguments, environment variables, and processor error messages.
}
$endnote

#### Fortran 2008, the most exciting features

$note
$caption(none){A minor revision of Fortran 2003, but still A lot of new features...}
$content[font-size:98%;]{
+ do concurrent construct
+ contiguous attribute
+ block construct
+ error stop statement
+ internal procedures can be passed as actual arguments
+ procedure pointers can point to an internal procedure
+ maximum rank increased to 15
+ `newunit=` in open statement
+ `g0` edit descriptor
+ unlimited format item
+ new intrinsic (acosh, asinh...)
+ shell commands: `execute_command_line`
+ `iso_fortran_env`: `compiler_version` and `compiler_options`
}
$endnote

$note
$caption(none){... but the 2 revolutions are}
$content[font-size:98%;]{
+ submodules
+ coarrays
}
$endnote

### Module-Oriented Programming

#### Module-Oriented Programming

$note
$caption(none){Module, what?}
$content[font-size:105%;]{
+ introduced in Fortran 90
+ allow to write object based code
+ a `module` is a program unit whose functionality can be exploited by other programs which attaches to it via the `use` statement.
+ can contain
    + global object declaration: replaces Fortran 77 `COMMON` and `INCLUDE` statements
    + interface declaration
    + procedure declaration (since modules already contain explicit interface, an interface statement is not required)
+ within a `module`, functions and subroutines are called module procedures
+ `module` procedures can contain internal procedures
+ `module` objects that retain their values should be given a `save` attribute
+ `module`s can be used by procedures and other modules
+ `module`s can be compiled separately
+ by default, all module entities are public
+ to restrict the visibility of the module procedure only to the module, use the `private` statement
}
$endnote

#### Be modular!

$columns

$column[width:45%]

$note
$style[width:100%]
$caption(none){Bad Practice}
$content[font-size:100%;]{
Old-fashioned Fortran **library**
```fortran
subroutine foo(arg1, arg2, arg3)
real    :: arg1, agr2(*), arg3
integer :: i
! GOD is real...
god = sum(arg2)
i = bar(arg2)
arg3 = god/i
end subroutine foo

integer function bar(arg)
real :: arg(*)
bar = size(arg, dim=1)
end function bar
```
}
$endnote

$column[width:55%]

$note
$style[width:100%]
$caption(none){Good Practice}
$content[font-size:100%;]{
Modern, robust Fortran **module library**
```fortran
module library
implicit none
private
public :: foo
contains
  pure subroutine foo(arg1, arg2, arg3)
    real, intent(in)  :: arg1
    real, intent(in)  :: agr2(*)
    real, intent(out) :: arg3
    real              :: god
    integer           :: i
    god = sum(arg2)
    i = bar(arg2)
    arg3 = god/i
  end subroutine foo

  pure function bar(arg) result(bar_size)
    real, intent(in) :: arg(*)
    integer          :: bar_size
    bar_size = size(arg, dim=1)
  end function bar
end module library
```
}
$endnote

$endcolumns

#### Module key-ingredients

$note
$style[width:100%]
$caption(none){Must-have of Module}
$content[font-size:95%;]{
Always start with `Implicit none` statement
```fortran
module library
! optional use-association of other modules
implicit none
end module library
```
Hide ALL! Expose only the minimum
```fortran
module library
private ! all PRIVATE!
public :: what_really_concern ! expose only what really concern
protected :: read_only_objects
end module library
```
Rename entities on use-association for names-collision handling
```fortran
module library
use another_library, only : external_concern => what_really_concern
private
public :: what_really_concern
end module library
```
}
$endnote

#### Module as Globals

$columns

$column[width:50%]

$note
$style[width:100%]
$caption(none){Avoid Globals, as much as possible!}
$content[font-size:95%;]{
+ once upon a time there was **COMMON BLOCK**... the evil!
+ now Module can be used as a more robust **GLOBALS CONTAINER**
```fortran
module globals
implicit none
private
public :: foo, bar
protected :: baz

integer :: foo = 0
real    :: bar = 0.0
logical :: baz = .false.

contains
  subroutine initialize
  foo = 1
  bar = 2.0
  baz = .true.
  end subroutine initialize
end module globals
```
}
$endnote

$column[width:50%]

$note
$style[width:100%]
$caption(none){Pros}
$content[font-size:95%;]{
+ all details of the data structure & order is isolated to a single source file. No need to replicate COMMON BLOCKs throughout each subroutine or use the non-Fortran 77 INCLUDE as a work-around.
+ avoid problems with unequal COMMON BLOCK definitions.
+ can use all data variables by default, or select and rename a subset.
+ encapsulate data together methods!
}
$endnote

$note
$style[width:100%]
$caption(none){Cons}
$content[font-size:95%;]{
+ all the cons of Globals...
    + thread-un-safe: all use-associated **users** can (concurrently) modify public data;
    + passing globals by use-association make obscure the signature of procedures;
}
$endnote
$endcolumns

### Derived Type

#### Derived Type

$note
$style[width:100%]
$caption(none){The revolution}
$content[font-size:95%;]{
+ extending the language!
+ strongly couple data-structure definition and **methods** operating on them!
    + safety
    + clearness
    + conciseness
    + reusability
}
$endnote

$note
$style[width:100%]
$caption(none){How?}
$content[font-size:95%;]{
+ defined by user
+ can include different intrinsic types and other derived types
+ components are accessed using the percent operator (%)
+ only assignment operator (=) is defined for derived types
```fortran
type :: line
  real :: x(2) ! abscissa
  real :: y(2) ! ordinate
end type line

type(line) :: a_line
```
}
$endnote

#### Derived Type for Memory Management

$note
$style[width:100%]
$caption(none){Pack related-data}
$content[font-size:95%;]{
Exploit derived types for a less error-prone procedures usage
}
$endnote

$columns

$column[width:50%]

$note
$style[width:100%]
$caption(none){Bad Practice}
$content[font-size:95%;]{
```fortran
integer           :: Ni
integer           :: Nj
integer           :: Nk
real, allocatable :: centers(:,:,:)
real, allocatable :: nodes(:,:,:)
real, allocatable :: normals(:,:,:)
real, allocatable :: faces(:,:,:)
real, allocatable :: volumes(:,:,:)

call compute_metrics(Ni, Nj, Nk,    &
                     centers,nodes, &
                     normals, fases &
                     volumes)
```
}
$endnote

$column[width:50%]

$note
$style[width:100%]
$caption(none){Good Practice}
$content[font-size:95%;]{
```fortran
type :: mesh
  integer           :: Ni = 0
  integer           :: Nj = 0
  integer           :: Nk = 0
  real, allocatable :: centers(:,:,:)
  real, allocatable :: nodes(:,:,:)
  real, allocatable :: normals(:,:,:)
  real, allocatable :: faces(:,:,:)
  real, allocatable :: volumes(:,:,:)
end type mesh
type(mesh) :: a_mesh
call compute_metrics(grid=a_mesh)
```
}
$endnote

$endcolumns

#### Derived Type for Memory Management

$note
$style[width:100%]
$caption(none){Data hiding and Encapsulation}
$content[font-size:95%;]{
Exploit derived types for avoid globals
}
$endnote

$columns

$column[width:50%]

$note
$style[width:100%]
$caption(none){Bad Practice}
$content[font-size:95%;]{
```fortran
module globals
implicit none
integer           :: Ni
integer           :: Nj
integer           :: Nk
real, allocatable :: centers(:,:,:)
real, allocatable :: nodes(:,:,:)
real, allocatable :: normals(:,:,:)
real, allocatable :: faces(:,:,:)
real, allocatable :: volumes(:,:,:)
end module globals

program bad
use globals
implicit none
call compute_metrics(Ni, Nj, Nk,    &
                     centers,nodes, &
                     normals, fases &
                     volumes)
end program bad
```
}
$endnote

$column[width:50%]

$note
$style[width:100%]
$caption(none){Good Practice}
$content[font-size:90%;]{
```fortran
module mesh_t
implicit none
private
public :: mesh
type :: mesh
  integer           :: Ni = 0
  integer           :: Nj = 0
  integer           :: Nk = 0
  real, allocatable :: centers(:,:,:)
  real, allocatable :: nodes(:,:,:)
  real, allocatable :: normals(:,:,:)
  real, allocatable :: faces(:,:,:)
  real, allocatable :: volumes(:,:,:)
end type mesh
end module mesh_t
program good
use mesh_t, only: mesh
implicit none
type(mesh) :: a_mesh
call compute_metrics(grid=a_mesh)
end program good
```
}
$endnote

$endcolumns

### Generic Names

#### Generic Names

$note
$style[width:100%]
$caption(none){The Problem}
$content[font-size:80%;]{
Apply the same PROCEDURE to different arguments (for type, kind and or rank), e.g. convert **number to string**
```fortran
integer :: step_number
real    :: time
real    :: delta
integer :: unit
integer :: t
delta = 0.1
time  = 0.0
do t=1, 100
  time = time + delta
  open(newunit=unit, file='clock-'//string(t)//'.dat')
  write(unit, "(A)")'current time: '//string(time)
  close(unit)
enddo
```
Say, you want the `STRING` function to accept both the integer `t` and the real `time`, in the same of the many built-ins like `cos, sin, etc...` You can develop a `STRING_INTEGER, STRING_REAL`, but if you need to accept also different kinds? You then need a `STRING_INTEGER_8, STRING_INTEGER_16, STRING_INTEGER_32...` and you need a different call for each argument type/kind/rank,

```fortran
... file='clock-'//string_integer_32(t)//'.dat')
... 'current time: '//string_real_32(time)
```
Too much ERROR PRONE!
}
$endnote

#### Generic Names
$columns

$column[width:55%]
$note
$style[width:100%]
$caption(none){The Solution}
$content[font-size:85%;]{
**Generic Names** avoid the *plethora* of different names exploiting *dynamic dispatching* at compile-time (no penalty!)
```fortran
module generic
implicit none
private
public :: string
interface string
  module procedure string_integer, string_real
endinterface
contains
  elemental function string_integer(n) result(str)
  integer, intent(in) :: n
  character(11)       :: str
  write(str, '(I11)') n
  end function string_integer
  elemental function string_real(n) result(str)
  real, intent(in) :: n
  character(13)    :: str
  write(str, '(E13.6E2)') n
  end function string_real
end module generic
```
Very **CONCISE, CLEAR and ROBUST**
}
$endnote
$column[width:45%]
$note
$style[width:100%]
$caption(none){}
$content[font-size:80%;]{
```fortran
use generic
integer :: step_number
real    :: time
real    :: delta
integer :: unit
integer :: t
delta = 0.1
time  = 0.0
do t=1, 100
  time = time + delta
  open(newunit=unit, &
    file='clock-'//string(t)//'.dat')
  write(unit, "(A)") &
    'current time: '//string(time)
  close(unit)
enddo
```
}
$endnote

$note
$style[width:100%]
$caption(none){PENF, a concrete application}
$content[font-size:100%;]{
Portability Environment for Fortran poor people

A KISS library for exploiting codes portability for modern (2003+) Fortran projects

[https://github.com/szaghi/PENF](https://github.com/szaghi/PENF)
}
$endnote

$endcolumns

### Operators Overloading

#### Operators Overloading

$note
$style[width:100%]
$caption(none){The Problem}
$content[font-size:100%;]{
Create an **algebra** for your new shining type, e.g. for your vector class
```fortran
type, public :: Vector
  real :: x = 0.0 ! Cartesian component in x direction.
  real :: y = 0.0 ! Cartesian component in y direction.
  real :: z = 0.0 ! Cartesian component in z direction.
end type Vector
```
It would be fantastic to be able to do **VECTORIAL COMPUTATION**... isn't it?
```fortran
type(vector) :: vector1
type(vector) :: vector2
type(vector) :: vector3
...
vector3 = vector1 + vector2
vector3 = vector1 - vector2
vector3 = 2.0*vector1 + vector2
vector3 = vector1 - 1.4
vector3 = vector1.cross.vector2
vector3 = vector1.dot.vector2
```
Well, you can! Exploit **OPERATORS OVERLOADING** (without penalties!)
}
$endnote

#### Operators Overloading

$columns

$column[width:55%]

$note
$style[width:100%]
$caption(none){The Solution}
$content[font-size:70%;]{
```fortran
module vector_t
implicit none
private
public :: vector, operator (+), operator(.cross.)
type, public :: Vector
  real :: x = 0.0 ! Cartesian component in x direction.
  real :: y = 0.0 ! Cartesian component in y direction.
  real :: z = 0.0 ! Cartesian component in z direction.
end type Vector
interface operator (+)
  module procedure add
end interface
interface operator (.cross.)
  module procedure crossproduct
end interface
contains
  elemental function add(left, rigth) result(summ)
  type(Vector), intent(in) :: left
  type(Vector), intent(in) :: rigth
  type(Vector)             :: summ
  summ%x = left%x + rigth%x
  summ%y = left%y + rigth%y
  summ%z = left%z + rigth%z
  end function add
  elemental function crossproduct(vec1, vec2) result(cross)
  type(Vector), intent(in) :: vec1
  type(Vector), intent(in) :: vec2
  type(Vector)             :: cross
  cross%x = (vec1%y * vec2%z) - (vec1%z * vec2%y)
  cross%y = (vec1%z * vec2%x) - (vec1%x * vec2%z)
  cross%z = (vec1%x * vec2%y) - (vec1%y * vec2%x)
  end function crossproduct
end module vector_t
```
}
$endnote

$column[width:45%]

$note
$style[width:100%]
$caption(none){VecFor, a concrete application}
$content[font-size:100%;]{
Vector algebra class for Fortran poor people

A KISS pure Fortran OOD class for computing Vectorial (3D) algebra

[https://github.com/szaghi/VecFor](https://github.com/szaghi/VecFor)
}
$endnote
$endcolumns
