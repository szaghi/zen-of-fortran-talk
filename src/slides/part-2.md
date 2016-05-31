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

### Operators Overloading

#### Operators Overloading
