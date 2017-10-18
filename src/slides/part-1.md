## Fortran for newbies

### A short intro

#### Why Fortran?

$note

$caption(none){Fortran is **NOT** a legacy language!}
$content[font-size:140%;]{

+ Fortran is the **BEST** general-purpose programming language for **SCIENTIFIC COMPUTING**:
    + near the best performance with easy;
    + the best **ARRAYS HANDLING** you will ever see;
    + **HPC** ready;
    + near all the math functions you will ever want built-in the language;
+ the first high-level language to be **STANDARDIZED**;

}
$endnote

$note

$caption(none){Believe me...}
$content[font-size:140%;]{
Matlab is a **TOY** compared to Fortran!
}
$endnote

#### Once upon a time...

Fortran was originally developed by IBM (the **FOR**mula **TRAN**slating system) in the mid of 50'

+ FORTRAN, first released by IBM in 1956
+ FORTRAN II, released by IBM in 1958
+ FORTRAN IV, released in 1962, standardized
+ FORTRAN 66, appeared in 1966 as an ANSI standard
+ FORTRAN 77, appeared in 1977, structured features
+ Fortran 90, 1992 ANSI standard, free form, modules
+ Fortran 95, a few extensions
+ Fortran 2003, object oriented programming
+ Fortran 2008, a few extensions
+ Fortran 2015, incoming standard, mainly parallel features

$note

$caption(none){Fortran is not dead, it is worth to learn it!}
$content[font-size:110%;]{

Three main reasons to learn Fortran:

1. it is a de-facto standard in scientific computing;
1. there are tons of codes for doing anything;
1. it has a brilliant future.

}
$endnote

#### So many Fortran flavors...

$note
$caption(none){which one I should use?}
$content[font-size:130%;]{
Obviously, the last one!
}
$endnote

+ free-format source code;
+ names can consists of up to 31 characters;
+ dynamic memory handling:
    + ability to operate on arrays (or array sections) as a whole;
+ sane programming style:
    + generic names;
    + optional arguments and calls with keywords;
    + operator overloading;
+ recursive, pure and elemental procedures;
+ derived types:
    + Object Oriented Programming.

$note
$caption(none){Modern Fortran compilers are backwards compatible!}
$content[font-size:130%;]{
Develop new codes in Modern Fortran mixing-in old legacy codes is effortless!
}
$endnote

### The Basics

#### Sources Format

$note
$caption(none){Use Free Format!}
$content[font-size:110%;]{
+ ASCII text up to 132 columns;
+ case insensitive i.e. `PROGRAM` is the same as `ProGram`;
+ using mixed case for statements and variables is not considered a good programming practice; be considerate to your collaborators who will be modifying the code.
+ use whatever convention you are comfortable...
+ ... but **ADOPT** a convention! Do not code **as it comes**;
+ be consistent with your convention throughout.
}
$endnote

$columns
$column[width:45%]
$note
$caption(none){Bad practice}
$content[font-size:90%;]{
```fortran
program My_bAD
  integer :: NabCD
     parameter :: naBcd = 10
character(nabcD) :: abc = 'hello'
print*, ABC
program MY_bad
```
}
$endnote
$column[width:50%]
$note
$caption(none){Good practice}
$content[font-size:90%;]{
```fortran
program my_good

  integer, parameter      :: chars_number=10
  character(chars_number) :: greetings='hello'

  print '(A)', trim(greetings)
program my_good
```
}
$endnote
$endcolumns

#### Fortran Program Units

$note
$caption(none){(Dis)Sectioning a Fortran Program}
$content[font-size:130%;]{
A Fortran program consists of one or more **PROGRAM UNITS**
}
$endnote

$columns
$column[width:30%]
```fortran
program
...
end program
```

```fortran
module
...
end module
```

```fortran
subroutine
...
end subroutine
```

```fortran
function
...
end function
```
$column[width:70%]

+ the `PROGRAM` unit is the main, there should be only one main unit;
+ the `PROGRAM` unit can contain one or more **subprogram** units such as `SUBROUTINE` or `FUNCTION`;
+ the subprogram units should be used to perform simple tasks: function or subroutine of more than 50/100 lines are bad designed!
+ the subprogram units related each other should be **wrapped** into a `MODULE`:
    + be **MODULAR** should be an imperative for you!
+ inside each program (sub)unit define **data and operations on data**.

$endcolumns

#### Hello World!

$note
$caption(none){Your First Fortran Program}
$content[font-size:100%;]{
```fortran
program the_greeter
print '(A)', 'Hello World!'
end program the_greeter
```
}
$endnote

$note
$caption(none){Your First MODULAR Fortran Program}
$content[font-size:100%;]{
```fortran
module the_greeter_worker
contains
  subroutine say_something(what)
  character(*), intent(in) :: what !< What I have to say?
  print '(A)', what
  end subroutine say_something
endmodule the_greeter_worker

program the_modular_greeter
use the_greeter_worker
call say_something(what='Hello modular World!')
end program the_modular_greeter
```
}
$endnote

The standard Fortran file extension is `.f90` (for standard 90+), e.g. `the_greeter.f90`

#### Compile Fortran

$note
$caption(none){Fortran is a compiled language}
$content[font-size:110%;]{

Building a Fortran application means to pass through 3 steps:

1. write the sources;
2. generate the compiled objects;
3. link the executable.
}
$endnote

```bash
# generate the_greeter.o compiled object
gfortran -c the_greeter.f90

# generate the_greeter executable
gfortran the_greeter.o -o the_greeter

# generate the_greeter executable in one command
gfortran the_greeter.f90 -o the_greeter
```

$note
$caption(none){Compilers}
$content[font-size:70%;]{

The best Fortran compilers you will ever see are also FREE!

1. [GNU gfortran, https://gcc.gnu.org/wiki/GFortran](https://gcc.gnu.org/wiki/GFortran);
1. [Intel Fortran, https://software.intel.com/en-us/fortran-compilers](https://software.intel.com/en-us/fortran-compilers), free for **non commercial use**;
}
$endnote

### Intrinsic types

#### Intrinsic types

$note
$caption(none){How to define data containers}
$content[font-size:110%;]{Fortran provides 5 fundamental, intrinsic data-types:}
$endnote

$columns
$column[width:50%]
$note
$caption(none){INTEGERs}
$content[font-size:90%;]{`INTEGER`s are integer values, which can be represented exactly by the computer}
$endnote

$note
$caption(none){REALs}
$content[font-size:90%;]{`REAL`s are floating point numbers, representing real numbers; they are not exact representations of real numbers, having only a finite number of decimal places, the **FINITE PRECISION** issue}
$endnote
$column[width:50%]
$note
$caption(none){CHARACTERs}
$content[font-size:90%;]{`CHARACTER`s are text characters, usually encoded as ASCII}
$endnote

$note
$caption(none){LOGICALs}
$content[font-size:90%;]{`LOGICAL`s are boolean variables, which can store a value of either `.TRUE.` or `.FALSE.`}
$endnote
$endcolumns

$note
$caption(none){COMPLEX}
$content[font-size:90%;]{`COMPLEX` variables are effectively just two real variables, one storing the real component of a number, the other storing the imaginary component}
$endnote

#### Explicit and Implicit Typing

$note
$caption(none){The BAD coming from the past...}
$content[font-size:90%;]{

Fortran is capable of implicit typing of variables, i.e. variables obtain a type on the basis of **ITS FIRST CHARACTER**... a nightmare!

```
|ABCDEFGH|IJKLMN |OPQRSTUVWXYZ|
|________|_______|____________|
| real   |integer| real       |
```

}
$endnote

$note
$caption(none){Explicit is **BETTER** than implicit!}
$content[font-size:90%;]{

For backward compatibility reasons, Fortran default is **IMPLICIT**... always start program units with the statement `implicit none` to disable the default implicitly
}
$endnote

$columns
$column[width:33%]
```fortran
program explicit

implicit none

end program explicit
```
$column[width:33%]
```fortran
module explicit

implicit none

end module explicit
```
$column[width:33%]
```fortran
subroutine explicit
! if not contained into
! a program/module with
! "implicit none"

implicit none

end subroutine explicit
```
$endcolumns

#### Define a variable

$note
$caption(none){Explicit declare all variables}
$content[font-size:100%;]{

```fortran
TYPE[, attributes] [::] variable
```
where `TYPE` is an intrinsic type (or a derived one not yet seen), `attributes` are optional attributes of the variable declaration and `variables` is the, obviously, the variable name. The double semicolon `::` are optional, but highly recommended!

}
$endnote

$note
$caption(none){Initialize Variable at its declaration}
$content[font-size:100%;]{

It is possible and highly recommended to assign and initial value to all variables. It could be very helpful, to initialize them into their definition

```fortran
integer, parameter :: cars_number=100
```

The assignment on definition is **NOT ALWAYS POSSIBLE**, neither **DESIRABLE**.

}
$endnote

#### Constants

$note
$style[width:100%]
$caption(none){Constants types}
$content[font-size:100%;]{

Constants are classified into two categories:

+ **literal** constants that have not a name;
+ **named** constants that have obviously a name.
}
$endnote

$note
$caption(none){Literal Constants}
$content[font-size:80%;]{
```fortran
100
3.14159265
'Hello World!'
-3.134324390984353454e23 ! subtle mistakes...
```

Avoid to use literal constants: **MAGIC NUMBERS** are not clear!

}
$endnote

$note
$caption(none){Named Constants}
$content[font-size:80%;]{
```fortran
integer,       parameter :: cars_number=100
real,          parameter :: pi=3.14159265
character(12), parameter :: greetings='Hello World!'
real,          parameter :: wrong=-3.134324390984353454e23 ! subtle mistakes...
```

The `PARAMETER` attribute is your **CONSTANT** friend!

}
$endnote

#### Arrays

$columns

$column[width:65%]

$note
$caption(none){What?}
$content[font-size:80%;]{Arrays are *collections* of values of the same type}
$endnote

$note
$caption(none){How?}
$content[font-size:80%;]{
Arrays are declared or by means of **dimension** attribute, either by directly define a dimension along-side the array name, e.g.
```fortran
integer, dimension(2:15) :: array_with_explicit_bounds
integer, dimension(11)   :: array_with_implicit_bounds
integer                  :: array_with_explicit_dimensions(3)
```
}
$endnote

$column[width:33%]

$note
$caption(none){Why?}
$content[font-size:80%;]{Highly efficient handling of collections of values, especially for contiguous (in memory) elements}
$endnote

$note
$style[width:100%]
$caption(none){Defaults}
$content[font-size:80%;]{
+ individual elements are accessed by *subscripting* the array;
+ default lower bound is 1 and can be omitted;
+ up to 7 dimensions;
+ in contrast to C/C++, Fortran arrays are column major.
}
$endnote

$endcolumns

$columns
$column[width:47%]

$note
$style[width:100%]
$caption(none){Bad Practice}
$content[font-size:65%;]{
```fortran
integer, dimension(5, 3) :: array ! avoid magic numbers
integer                  :: c
integer                  :: r
do r=1, 5
  do c=1, 3
    print*, array(r, c) ! wrong memory access
  enddo
enddo
```
}
$endnote

$column[width:53%]

$note
$style[width:100%]
$caption(none){Good Practice}
$content[font-size:65%;]{
```fortran
integer, parameter                           :: rows_number=5
integer, parameter                           :: cols_number=3
integer, dimension(rows_number, cols_number) :: array
integer                                      :: c
integer                                      :: r
do c=1, cols_number
  do r=1, rows_number
    print*, array(r, c) ! right column-major memory access
  enddo
enddo
```
}
$endnote

$endcolumns

#### Array Terminology

```fortran
real :: a(0:20), b(3, 0:5, -10:10)
```

$note
$style[width:100%]
$caption(none){Terminlogy}
$content[font-size:78%;]{
+ `rank`: number of dimensions;
```fortran
`a` has rank 1 and `b` has rank 3
```
+ `bounds`: upper and lower limits of each dimension of the array;
```fortran
`a` has bounds 0:20 and `b` has bounds 1:3, 0:5 and -10:10
```
+ `extent`: number of element in each dimension;
```fortran
`a` has extent 21 and `b` has extents 3,6 and 21
```
+ `size`: total number of elements;
```fortran
`a` has size 21 and `b` has 30
```
+ `shape`: the shape of an array is its rank and extent;
```fortran
`a` has shape 21 and `b` has shape (3, 6, 21)
```

Note that

+ arrays are **conformable** if they share a **shape**;
+  the bounds do not have to be the same
```fortran
c(4:6) = d(1:3)
```
}
$endnote

#### Arrays Computations

$note
$style[width:100%]
$caption(none){Arrays Computations, (one of the place) where Fortran wins!}
$content[font-size:100%;]{

The arrays handling is superb in Fortran, computations on them are very efficient

```fortran
integer, parameter              :: elems_number=10
real, parameter                 :: delta=1.0/elems_number
real, dimension(0:elems_number) :: abscissa
real, dimension(0:elems_number) :: temperature
integer                         :: e

! array computations

! array constructor
abscissa = [(delta*e, e=0, elems_number)]

! elemental function operating on each array element
temperature = sin(abscissa)/2.0

print "(A)", "Abscissa, Temperature"
do e=0, elems_number
  print "(E15.6, 1X, E15.6)", abscissa(e), temperature(e)
enddo
```
}
$endnote

#### Arrays Allocations

$note
$style[width:100%]
$caption(none){Arrays Allocations, dynamic memory handling}
$content[font-size:75%;]{

Arrays can have **static** defined sized (at compile time) or a **dynamic** size (at run time). Dynamic allocation allow efficient handling of memory with a very little (negligible or null) overhead: dynamic arrays allocation is not the evil!

```fortran
integer                         :: elems_number
real                            :: delta
real, allocatable, dimension(:) :: abscissa
real, allocatable, dimension(:) :: temperature
integer                         :: e

! input from user
print "(A)", "Enter the number of elements"
read *, elems_number
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
```
}
$endnote

#### Kinds

$note
$style[width:100%]
$caption(none){Finite Precision Issue}
$content[font-size:100%;]{

+ computers have a **finite memory dimension** => **finite precision representation** of data
+ particularly important for numbers: algorithms (especially the ones not so robust/well posed) could be affected by round-off related problems
}
$endnote

$note
$style[width:100%]
$caption(none){Subtle mistakes}
$content[font-size:85%;]{
```fortran
real,    parameter :: pi_greek=3.14159265358979323    ! you will not get what you expect...
real*8,  parameter :: pi_greek=3.14159265358979323    ! still wrong...
real*16, parameter :: pi_greek=3.14159265358979323    ! no improvements, still wrong...
real*16, parameter :: pi_greek=3.14159265358979323_16 ! ok, sometimes works, other fails...
```
}
$endnote

$note
$style[width:100%]
$caption(none){How to Deal with Finite Precision?}
$content[font-size:85%;]{
+ **BE Parametric**;
+ **BE Portable**;
+ **BE Conscious** (of what you are doing).

**USE KIND SPECIFICATION!**
}
$endnote

#### Kinds

$note
$style[width:100%]
$caption(none){Specify the KIND of all intrinsic types, BE Parametric!}
$content[font-size:100%;]{
```fortran
integer(kind=...)            :: a_parametric_integer
real(kind=...)               :: a_parametric_real
character(kind=..., len=...) :: a_parametric_character
logical(kind=...)            :: a_parametric_logical
complex(kind=...)            :: a_parametric_complex
```
The default value of the kind of each intrinsic type is **PROCESSOR DEPENDENT**: do not expect to obtain the same default kind on all OS/Compiler/CPU combinations (hereafter *architecture*).

Which Parameter I have to use?
}
$endnote

$note
$style[width:100%]
$caption(none){Again, Magic Numbers are the Evil also for Kinds!}
$content[font-size:90%;]{
```fortran
integer(kind=4) :: a_bad_parametric_integer
real(kind=4)    :: a_bad_parametric_real
...
```
The definition `KIND=4` is not standard portable definition, it does not mean anything on which you can rely on: on some (the most part) architectures it means **4-bytes**, but it is not ensured to happen.
}
$endnote

#### Kinds

$note
$style[width:100%]
$caption(none){Let Compiler select kinds for you, BE Portable!}
$content[font-size:95%;]{

By means of the built-in functions `selected_xxx_kind`

```fortran
! parametric portable kinds
integer, parameter        :: I4 = selected_int_kind(9)             ! 4-bytes
integer, parameter        :: R8 = selected_real_kind(15,307)       ! 8-bytes
integer, parameter        :: CK = selected_char_kind(name='ASCII') ! ascii kind

! parametrized portable definition of variables
integer(kind=I4)          :: a_good_parametric_integer
real(kind=R8)             :: a_good_parametric_real
character(kind=CK, len=2) :: a_good_parametric_character
```

By means of the intrinsic module (F03+) `iso_fortran_env`

```fortran
! parametric portable kinds
use, intrinsic :: iso_fortran_env, only : I4 => int32, R8 => real64, character_kinds
integer, parameter        :: CK = character_kinds(1)

! parametrized portable definition of variables
integer(kind=I4)          :: a_good_parametric_integer
real(kind=R8)             :: a_good_parametric_real
character(kind=CK, len=2) :: a_good_parametric_character
```
}
$endnote

#### Kinds

$note
$style[width:100%]
$caption(none){Literal Constants have their own kind, BE Conscious!}
$content[font-size:100%;]{

```fortran
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
```
What you expect? The output could be surprising...
```bash
 0.314159274101257E+001
 0.314159274101257E+001
 0.314159274101257E+001
 0.314159265358979E+001
```
}
$endnote

#### Operators

$note
$caption(none){Arithmetic Operators}
$content[font-size:90%;]{
+ `+`: addition
+ `-`: subtraction
+ `*`: multiplication
+ `/`: division
+ `**`: exponentiation
}
$endnote

$note
$caption(none){Relational Operators}
$content[font-size:90%;]{
+ `==`: equal to
+ `/=`: not equal to
+ `<`: less than
+ `<=`: less than or equal to
+ `>`: greater than
+ `>=`: greater than or equal to
}
$endnote

$note
$caption(none){Logical Operators}
$content[font-size:90%;]{
+ `.and.`: intersection
+ `.or.`: union
+ `.not.`: negation
+ `.eqv.`: logical equivalence
+ `.neqv.`: exclusive or
}
$endnote

$note
$caption(none){Character Operators}
$content[font-size:90%;]{
+ `//`: concatenation
}
$endnote

$note
$style[width:100%]
$caption(none){Operators Precedence}
$content[font-size:100%;]{
+ all operator evaluations on variables is carried out from left-to-right
+ arithmetic operators have a highest precedence while logical operators have the lowest precedence
+ the order of operator precedence can be changed using parenthesis, '(' and ')'
+ a user can define his/her own operators:
    + user defined monadic operator has a higher precedence than arithmetic operators, while
    + dyadic operators has a lowest precedence than logical operators.
}
$endnote

#### Intrinsic Functions

$figure
$style[width:100%;]
$content{images/intrinsic_functions.png}
$caption(none){A very large set of (optimized) intrinsic functions, especially for mathematical computations}
$endfigure

### Control Flows

#### Conditionals

$note
$style[width:100%]
$caption(none){If (then-else) construct}
$content[font-size:100%;]{

The King of all conditionals, the `if` construct. The minimal usage

```fortran
if (expression) statement
```
where `expression` is a valid logical expression. If the expression is `.true.` (when evaluated), the `statement` is executed. A slightly more complex usage
```fortran
[name_of_if:] if (expression1) then
  statement1
else if (expression2) then
  statement2
else
  statement3
end if [name_of_if]
```

+ the `if-then-else` construct allows **branching** of algorithms;
+ each logical expression (`expression1, expression2,...`) are executed sub-sequentially;
+ when a block `statement` is executed the `if-then-else` is left (i.e. the control goes to `end if`);
+ the last `else` is executed (if present) if all the previous expression are `.false.`.

}
$endnote

#### Switches

$note
$style[width:100%]
$caption(none){select case construct}
$content[font-size:100%;]{

Often `if-then-else` construct is not the optimal **branching control**, `select case` construct could be more efficient

```fortran
[name_of_case:] select case(expression)
case(selectors1)
  statement1
case(selectors2)
  statement2
case default
  statement3
end select [name_of_case]
```
where `expression` is a generic expression (not only of logical type), `selectors1, selectors2,...` are list of (constant) *values* or range of values.

+ a range of values if expressed by a colon, e.g. `case(2:11)` means for all cases where `expression` is in between [2,11];
+ the values in selectors must be unique;
+ the `expression` is evaluated only one time! (efficient branching);
+ the first matching case is executed;
+ the last `case default` is executed (if present) if all the previous cases are not matched.

}
$endnote

#### Loops

$note
$style[width:100%]
$caption(none){do loop (definite)}
$content[font-size:95%;]{

The minimal **definite** loop is of the form

```fortran
[name_of_do:] do counter=lower_bound, upper_bound [, stride]
end do [name_of_do]
```
where `counter` is an integer variable, `lower/upper_bound` are the loop bounds and the optional `stride` is the stride used (default 1). If the stride is negative a count-down is performed, e.g.
```fortran
[name_of_do:] do counter=upper_bound, lower_bound, negative_stride
end do [name_of_do]
```
}
$endnote

$note
$style[width:100%]
$caption(none){do loop conditional (definite)}
$content[font-size:95%;]{

```fortran
[name_of_do:] do while (expression)
end do [name_of_do]
```
where `expression` is evaluated at the beginning of each loop step and if `.true.` the statements are executed.
}
$endnote

#### Loops

$note
$style[width:100%]
$caption(none){do loop (indefinite)}
$content[font-size:100%;]{

It is possible, and often desirable, to loop **indefinitely** at compile time and check for the occurrences of some conditions at run time

```fortran
[name_of_do:] do
end do [name_of_do]
```

The loop execution is controlled by means of the intrinsic function `exit` (to break the loop) and `cycle` to continue the loop with the next step

```fortran
indefinite do
  if (expression1) exit indefinite
  if (expression2) cycle indefinite
end do indefinite
```

If the name of the loop is omitted, the loop exited or cycled is the **nearest**.
}
$endnote

### Input & Output

#### Input & Output

$note
$style[width:100%]
$caption(none){IO Facility}
$content[font-size:120%;]{

+ IO are accomplished by operations on *files*...
+ but files are not strictly **files** in Fortran...
+ files are handled by *unit number*;
+ there are also *internal unit* for **Data Transfer**;
+ Input => `read`, Output => `print, write`;
+ IO formatted and unformatted;
+ File => `open`, `close`;
+ `inquire`;
+ user defined **derived-type-IO**...
}
$endnote

#### Read

`read` has a long list of arguments...

$columns

$column[width:50%]

$note
$style[width:100%]
$caption(none){Sequential}
$content[font-size:75%;]{

Formatted

```fortran
READ (eunit, format [, advance] [, asynchronous]
      [, blank] [, decimal] [, id] [, pad] [, pos]
      [, round] [, size] [, iostat] [, err] [, end]
      [, eor] [, iomsg]) [io-list]
```
Formatted, List-Directed

```fortran
READ (eunit, *[, asynchronous] [, blank]
      [, decimal] [, id] [, pad] [, pos]
      [, round] [, size] [, iostat] [, err] [, end]
      [, iomsg]) [io-list]
```

Formatted, Namelist

```fortran
READ (eunit, nml-group[, asynchronous] [, blank]
      [, decimal] [, id] [, pad] [, pos] [, round]
      [, size] [, iostat] [, err] [, end] [, iomsg])
```

Unformatted

```fortran
READ (eunit [, asynchronous] [, id] [, pos]
      [, iostat] [, err][, end] [, iomsg]) [io-list]
```
}
$endnote

$column[width:50%]

$note
$style[width:100%]
$caption(none){Direct-Access}
$content[font-size:75%;]{

Formatted

```fortran
READ (eunit, format, rec [, asynchronous] [, blank]
      [, decimal] [, id] [, pad] [, pos] [, round]
      [, size] [, iostat] [, err] [, iomsg])
      [io-list]
```

Unformatted

```fortran
READ (eunit, rec [, asynchronous] [, id] [, pos]
      [, iostat] [, err] [, iomsg]) [io-list]
```
}
$endnote

$note
$style[width:100%]
$caption(none){Internal}
$content[font-size:75%;]{

```fortran
READ (iunit, format [, nml-group] [, iostat] [, err]
      [, end] [, iomsg]) [io-list]
```

Namelist

```fortran
READ (iunit, nml-group [, iostat] [, err] [, end]
      [, iomsg]) [io-list]
```
}
$endnote

$endcolumns

#### Write

`write` has a long list of arguments too...

$columns

$column[width:50%]

$note
$style[width:100%]
$caption(none){Sequential}
$content[font-size:75%;]{

Formatted

```fortran
WRITE (eunit, format [, advance] [, asynchronous]
       [, decimal] [, id] [, pos] [, round]
       [, sign] [, iostat] [ , err] [, [iomsg])
       [io-list]
```
Formatted, List-Directed

```fortran
WRITE (eunit, * [, asynchronous] [, decimal]
       [, delim] [, id] [, pos] [, round] [, sign]
       [, iostat] [, err] [, iomsg])[io-list]
```

Formatted, Namelist

```fortran
WRITE (eunit, nml-group [, asynchronous] [, decimal]
       [, delim] [, id] [, pos] [, round] [, sign]
       [, iostat] [, err] [, iomsg])
```

Unformatted

```fortran
WRITE (eunit [, asynchronous] [, id] [, pos]
       [, iostat] [, err] [, iomsg])[io-list]
```
}
$endnote

$column[width:50%]

$note
$style[width:100%]
$caption(none){Direct-Access}
$content[font-size:75%;]{

Formatted

```fortran
WRITE (eunit, format, rec [, asynchronous]
       [, decimal] [, delim] [, id] [, pos]
       [, round] [, sign] [, iostat] [, err]
       [, iomsg])[io-list]
```

Unformatted

```fortran
WRITE (eunit, rec [, asynchronous] [, id] [, pos]
       [, iostat] [ , err] [, iomsg])[io-list]
```
}
$endnote

$note
$style[width:100%]
$caption(none){Internal}
$content[font-size:75%;]{

```fortran
WRITE (iunit, format [, nml-group] [, iostat]
       [ , err] [, iomsg])[io-list]
```

Namelist

```fortran
WRITE (iunit, nml-group [, iostat] [ , err]
       [, iomsg])[io-list]
```
}
$endnote

$endcolumns

#### Standard Input/Output & Error Units

$note
$style[width:100%]
$caption(none){Avoid magic numbers, again!}
$content[font-size:85%;]{
Units 5 and 6 are standard input and output: better approach is to use `iso_fortran_env`
}
$endnote

$note
$style[width:100%]
$caption(none){Bad Practice}
$content[font-size:85%;]{
```fortran
read(5, *) user_input
if (error_occur) then
  write(*, '(A)') error_message
else
  write(6, '(A)') output_message
endif
```
}
$endnote

$note
$style[width:100%]
$caption(none){Good Practice}
$content[font-size:85%;]{
```fortran
use, intrinsic :: iso_fortran_env, only : error_unit, input_unit, output_unit
read(input_unit, *) user_input
if (error_occur) then
  write(error_unit, '(A)') error_message
else
  write(output_unit, '(A)') output_message
endif
```
}
$endnote

#### File Operation IO

$note
$style[width:100%]
$caption(none){The basics}
$content[font-size:85%;]{

+ `open`, connect Fortran logical unit to a file or device; declare attributes for read and write operations;
+ `close`, terminate connection between logical unit and file or device
+ `rewind`, position sequential or direct access file at the beginning of the file (the initial point);
+ `inquire`, request information on the status of specified properties of a file or logical unit.

}
$endnote

$columns

$column[width:50%]

$note
$style[width:100%]
$caption(none){Open}
$content[font-size:75%;]{
```fortran
OPEN ([UNIT=] io-unit[, FILE= name] [, ERR= label]
      [, IOMSG=msg-var] [, IOSTAT=i-var], slist)
```
where `slist` is a list of one or more *specifier*, e.g. `access='stream', form='unformatted'` etc...
}
$endnote

$note
$style[width:100%]
$caption(none){Close}
$content[font-size:75%;]{
```fortran
CLOSE ([UNIT=] io-unit[, STATUS = p] [, ERR= label]
       [, IOMSG=msg-var] [, IOSTAT=i-var])
```
}
$endnote

$note
$style[width:100%]
$caption(none){Rewind}
$content[font-size:75%;]{
```fortran
REWIND ([UNIT=] io-unit[, ERR= label]
        [, IOMSG=msg-var] [, IOSTAT=i-var])
```
}
$endnote

$column[width:50%]

$note
$style[width:100%]
$caption(none){Inquire}
$content[font-size:75%;]{
Inquiring by File
```fortran
INQUIRE (FILE=name[, ERR=label] [, ID=id-var]
         [, IOMSG=msg-var] [, SIZE=sz]
         [, IOSTAT=i-var] slist)
```
Inquiring by Unit
```fortran
INQUIRE ([UNIT=]io-unit [, ERR=label] [, ID=id-var]
         [, IOMSG=msg-var] [, SIZE=sz]
         [, IOSTAT=i-var] slist)
```
Inquiring by Output List

```fortran
INQUIRE (IOLENGTH=len) out-item-list
```
}
$endnote

$endcolumns

### Subprogram Units

#### Subprogram Units

$note
$style[width:100%]
$caption(none){KISS, Keep It Simple and Stupid!}
$content[font-size:100%;]{
+ avoid too long *logics*, difficult to debug and maintain;
+ split complex goals in a set of *small and simple* sub-goals;
+ exploit Fortran subprogram units, i.e. **subroutine** and **function**.

This allows to

+ re-use code-blocks;
+ repeat the same operations on different datasets;
+ hide (local) implementation for a safer namespace mangling.
}
$endnote

$columns

$column[width:50%]

$note
$style[width:100%]
$caption(none){Bad Practice}
$content[font-size:100%;]{
```fortran
do i=1, N
  a(i) = cos(b(i-1)) + sin(c(i+1))
end do
do i=1, N
  d(i) = cos(e(i-1)) + sin(f(i+1))
end do
do i=1, N
  g(i) = cos(h(i-1)) + sin(p(i+1))
end do
```
}
$endnote

$column[width:50%]

$note
$style[width:100%]
$caption(none){Good Practice}
$content[font-size:100%;]{
```fortran
subroutine do_something(a, b, d)
...
end subroutine do_something

call do_something(a=a, b=b, c=c)
call do_something(a=d, b=e, c=f)
call do_something(a=g, b=h, c=i)
```
}
$endnote
$endcolumns

#### Subroutine

$note
$style[width:100%]
$caption(none){Declaration}
$content[font-size:83%;]{
```fortran
[prefix [prefix]] subroutine subroutine_name [([d-arg-list]) [lang-binding]]
! modules access, if any
use module_1
...
implicit none
! variables (dummy arguments and local ones) definition
...
! executable statements
[contains]
  ! internal subprogram units
end subroutine subroutine_name
```
where

+ **prefix** (optional) is one of `elemental, impure, module, pure, recursive`;
+ **d-arg-list** (optional) is the list of one or more dummy arguments;
+ **lang-binding** (optional) is the C binding `BIND (C[, name=ext-name])`.
}
$endnote

$note
$style[width:100%]
$caption(none){Usage}
$content[font-size:83%;]{
Exchange data through its dummy arguments list or by IO (impure)
```fortran
call subroutine_name [([d-arg-list])
```
}
$endnote

#### Function

$note
$style[width:100%]
$caption(none){Declaration}
$content[font-size:80%;]{
```fortran
[prefix [prefix]] FUNCTION function_name [([d-arg-list]) [suffix] [lang-binding]]
! modules access, if any
use module_1
...
implicit none
! variables (dummy arguments and local ones) definition
...
! executable statements
[contains]
  ! internal subprogram units
end function function_name
```
where

+ **prefix** (optional) is one of `data type-specifier, elemental, impure, module, pure, recursive`;
+ **d-arg-list** (optional) is the list of one or more dummy arguments;
+ **suffix** (optional) is `result(result_name)`;
+ **lang-binding** (optional) is the C binding `BIND (C[, name=ext-name])`.
}
$endnote

$note
$style[width:100%]
$caption(none){Usage}
$content[font-size:80%;]{
Exchange data through its returned value or by dummy arguments list (impure) or by IO (impure)
```fortran
my_result = function_name [([d-arg-list])
```
}
$endnote

#### Dummy Arguments

$note
$style[width:100%]
$caption(none){Specialities}
$content[font-size:100%;]{
+ call by reference: only the memory addresses of the arguments are passed;
    + any change to arguments changes the actual argument;
+ always specify the **intent** of dummy arguments;
+ compile-time check of calling **signature** mistakes is possible only the procedure **interface** is **explicit**;
    + the actual and dummy arguments must correspond in type, kind and rank.
}
$endnote

$columns

$column[width:45%]

$note
$style[width:100%;padding:0% 1%]
$caption(none){Interface}
$content[font-size:90%;]{
+ interface block is a powerful structure that was introduced in Fortran 90;
+ give a calling procedure the full knowledge of the types and characteristics of the dummy arguments;
+ provide a way to execute some safety checks when compiling the program;
}
$endnote

$column[width:55%]

$note
$style[width:100%]
$caption(none){Intent}
$content[font-size:90%;]{
+ intent attribute was introduced in Fortran 90 and is recommended as it
    + allow compilers to check for coding errors;
    + facilitate efficient compilation and optimization;
+ declare if a dummy argument is
    + Input: `intent(in)`
    + Output: `intent(out)`
    + Both: `intent(inout)`
+ a dummy declared as `intent(in)` in a procedure cannot be changed during the execution of the procedure.
}
$endnote

$endcolumns

#### Optional & Keyword Arguments

$note
$style[width:100%]
$caption(none){Optional}
$content[font-size:90%;]{
+ allow defaults to be used for missing arguments
+ make some procedures easier to use
+ once an argument has been omitted all subsequent arguments must be **keyword arguments**
+ the `present` intrinsic can be used to check for missing arguments

```fortran
subroutine get_temperature(vel, boltz, temperature)
use parameters
real(dp), dimension(:,:), intent(in)  :: vel
real(dp), optional,       intent(in)  :: boltz
real(dp),                 intent(out) :: temperature
integer(ip)                           :: i
real(dp)                              :: ke

if (present(boltz)) then
  kb = boltz
else
  kb = boltz_parameter
endif

ke = 0._dp
do i = 1, natom
   ke = ke + dot_product(vel(i,:), vel(i,:))
end do
temperature = mass * ke / (3._dp * kb * real(natom - 1, dp))

end subroutine get_temperature
```
}
$endnote

#### Optional & Keyword Arguments

$note
$style[width:100%]
$caption(none){Keyword Arguments}
$content[font-size:100%;]{
+ allow arguments to be specified in any order
+ make it easy to add an extra argument - no need to modify any calls
+ helps improve readability of the program
+ are used when a procedure has optional arguments
+ once a keyword is used, all subsequent arguments must be keyword arguments
```fortran
! equivalent callings
call get_temperature(velocity, boltzman, temp)
call get_temperature(vel=velocity, boltz=boltzman, temperature=temp)
call get_temperature(vel=velocity, temperature=temp, boltz=boltzman)
call get_temperature(velocity, temperature=temp, boltz=boltzman)
```
Always prefer **KEYWORD ARGUMENT** style!
```fortran
! a lot of this kind of callings
call baseline_sub(arg1=foo, arg2=bar, arg4=baz)
...
! modify the baseline_sub adding a new optional argument do not
! comport to modify all previous callings

! new callings
call baseline_sub(arg1=foo, arg2=bar, arg4=baz, arg3=fofo)
```
}
$endnote
