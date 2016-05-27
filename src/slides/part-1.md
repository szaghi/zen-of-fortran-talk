## Fortran for newbies

### A short intro

#### Why Fortran?

$note

$caption(none){Fortran is **NOT** a legacy language!}
$content[font-size:140%;]{

+ Fortran is the **BEST** general-purpose programming language for **SCIENTIFIC COMPUTING**:
    + near the best perfomance with easy;
    + the best **ARRAYS HANDLING** you will ever see;
    + **HPC** ready;
    + near all the math functions you will ever want built-in the language;
+ the first high-level language to be **STANDARDIZED**;

}
$endnote

$note

$caption(none){Belive me...}
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

Fortran is capable of implicit typing of variables, i.e. variables obtain a type on the basis of **ITS FIRST CHARCTER**... a nightmare!

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

For backward compatibility reasons, Fortran default is **IMPLICIT**... always start program units with the statement `implicit none` to disable the default implicity
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

#### Kinds

#### Operators

#### Statements

### Intrinsic Functions

#### Intrinsic Functions

### Control Flows

#### Conditionals

#### Loops

#### Switches

### Input & Output

#### Input & Output
