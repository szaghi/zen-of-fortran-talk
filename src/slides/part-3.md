## The OOP Way to Computer Science

### OOP Paradigm

#### OOP Paradigm

$note
$style[width:100%]
$caption(none){Paradigms... what?}
$content[font-size:82%;]{
As poor Fortran people we are used to simply **translate formula**, we do not care about **paradigms**...
What Paradigm is the following?
```fortran
subroutine shock(un_m,p_m,a_m,un_k,cdp,g14,dg1,gp1,gm1,segno,a,p,dp,M)
  real, intent(in)  :: un_m,p_m,a_m,un_k,cdp,g14,dg1,gp1,gm1,segno
  real, intent(out) :: a,p,dp,M
  real              :: app,Mq
  app = g14 * segno * (un_k - un_m)/a_m
  M = app + dsqrt(1._8 + app * app)
  Mq = M * M
  app = 1._8 + dg1 * (Mq - 1._8)
  p = p_m * app
  dp = cdp * M * Mq/(1._8 + Mq)
  a = a_m * dsqrt((gp1 + gm1 * app)/(gp1 + gm1 / app))
end subroutine shock
```
Aside to be very ugly, the above example shows that data is **HIGHLY DECOUPLED**. This is typical of **PROCEDURAL PROGRAMMING** paradigm

+ TOP to down approach
    + focus on the operations, not on the data
        + data handling unsafe, **GLOBALS** are often necessary
    + hard to add new functionality or change the work flow without going back and modifying all other parts of the program
    + not easily reusable, so programmers must often recreate the wheel
    + difficult to maintain
}
$endnote

#### OOP Paradigm

$note
$style[width:100%]
$caption(none){Change your point of view!}
$content[font-size:95%;]{
+ down to TOP approach
+ key idea
```bash
The real world can be accurately described as a collection of OBJECTS that interact
```
+ the focus is on **WHAT WE DO** not in **HOW WE DO**:
    + real world if made of **OBJETCS** not of **algorithms**:
        + identify the base **classes** of objects that dominate the problem;
        + identify the rules of interaction between classes;
        + describe the problem by the definition of **classes dynamics**.
}
$endnote

$note
$style[width:100%]
$caption(none){How-To-OOP, the Pillars}
$content[font-size:115%;]{
+ Encapsulation;
+ Inheritance;
+ Polymorphism.
}
$endnote

### Encapsulation

#### Encapsulation

$note
$style[width:100%]
$caption(none){What?}
$content[font-size:100%;]{
Encapsulation is about grouping of functionality (methods) and related data (members) together into a coherent data structure, **CLASSES**.

**IMPLEMENTATION** is hidden from the rest of the program, aka **Application Program Interface**.

*Do you need to know exactly how every aspect of a car works (engine, carburettor, alternator...)? No - you need to know how to use the steering wheel, brakes, accelerator...*
}
$endnote

$columns

$column[width:50%]

$note
$style[width:100%]
$caption(none){In practice}
$content[font-size:100%;]{
+ restrict access to data (and data hiding):
    + more control;
    + easy to debug;
        + easy to maintain;
    + avoid side effects;
        + easy to parallel.
}
$endnote

$column[width:50%]

$note
$style[width:100%]
$caption(none){The Consequence}
$content[font-size:100%;]{
+ design by-contract
    + modularize;
    + abstraction;
+ develop by tests.
}
$endnote

$endcolumns

#### Encapsulation, a Fluid Example

$columns

$column[width:50%]

$note
$style[width:100%]
$caption(none){Define a Contract}
$content[font-size:80%;]{
Define What is your Object and How it interacts
```fortran
module gas_t
use vecfor, only : vector
implicit none
private
public :: gas ! Only the class GAS is public!
type :: gas
  private ! hide all members!
  ! the suffix '_' means internal implementation
  real         :: Cp_=0.0
  real         :: Cv_=0.0
  real         :: density_=0.0
  real         :: energy_=0.0
  type(vector) :: momentum_
  contains
    private
    ! the setter (constructor)
    procedure, public :: initialize
    ! return the members value (the getters)
    procedure, public :: density
    procedure, public :: momentum
    procedure, public :: energy
    ! interact with the rest of the World...
    procedure, public :: temperature
    procedure, public :: speed_of_sound
    ...
end type gas
contains
! follow the implementation
...
end module gas_t
```
}
$endnote

$column[width:50%]

$note
$style[width:100%]
$caption(none){Respect the Contract!}
$content[font-size:80%;]{
Define your World using the Contract
```fortran
use gas_t, only : gas
implicit none

type(gas) :: air     ! a 'gas' object (instance)
real      :: density ! an alert dentisyt value

! respect the contract, use only public methods
call air%initialize(Cp=1004.8, &
                    Cv=716.0,  &
                    density=0.125)

! well designed API is auto-explicative...
if (air%speed()>air%speed_of_sound()) then
  ! do your supersonic stuffs...
end if
```
So, where is encapsulation? The user of **GAS** class

+ is not aware of *how to compute* the speed of sound..
    + he/she simply use the speed of sound;
+ cannot manipulate gas data directly...
    + a bad user cannot make density negative manually;
+ can re-use the same **names** defined into `gas_t` module:
    + encapsulation allow easy handling of names-collision (safety names space).

}
$endnote

$endcolumns

### Inheritance

#### Inheritance

$note
$style[width:100%]
$caption(none){What?}
$content[font-size:100%;]{
Inheritance enables new objects to take on the properties of existing objects, the **BASE CLASS**. A child inherits visible properties and methods from its parent while adding additional properties and methods of its own.
}
$endnote

$columns

$column[width:50%]

$note
$style[width:100%]
$caption(none){In practice}
$content[font-size:100%;]{
+ re-use existing classes:
    + reduce code-duplication:
        + easy debug;
        + easy maintain;
    + inherit members/methods;
    + re-use implementation;
+ classes hierarchy;
    + abstract is better, but specials happen...
    + specialize only when/where is necessary.
}
$endnote

$column[width:50%]

$note
$style[width:100%]
$caption(none){The Consequence}
$content[font-size:100%;]{
+ design from down to UP...
    + from very down! Abstract all!
        + specialize => inherit from abstract to concrete cases
    + avoid to re-invent the wheel
}
$endnote

$endcolumns

#### Inheritance, a Fluid Example

$columns

$column[width:46%]

$note
$style[width:100%]
$caption(none){Extend a Previous Contract}
$content[font-size:80%;]{
Extend a Previous Contract adding new behaviors
```fortran
module gas_multifluid_t
use gas_t, only : gas
implicit none
private
public :: gas_multifluid
! inherit from gas
type, extends(gas) :: gas_multifluid
  private
  integer           :: species_number=0
  real, allocatable :: concentrations(:)
  contains
    private
    ! overridded methods
    procedure, public :: initialize
    ...
end type gas_multifluid
contains
! follow the implementation
...
end module gas_multifluid_t
```
}
$endnote

$column[width:54%]

$note
$style[width:100%]
$caption(none){Always respect the Contract!}
$content[font-size:75%;]{
Define your new World using the new Contract
```fortran
use gas_t, only : gas
use gas_multifluid_t, only : gas_multifluid
implicit none
type(gas)            :: air
type(gas_multifluid) :: mixture
! respect the contract, use only public methods
call air%initialize(Cp=1004.8, &
                    Cv=716.0,  &
                    density=0.125)
call mixture%initialize(Cp=1004.8,            &
                        Cv=716.0,             &
                        species_number=3,     &
                        concentrations=[1.0,  &
                                        0.0,  &
                                        0.0], &
                        density=0.125)
! well designed API is auto-explicative...
if (air%speed()>air%speed_of_sound()) then
  ! do your supersonic stuffs...
end if
```
So, where is inheritance? The developer of **MULTIFLUID GAS** class

+ do not need to re-implement all the behaviors of gas class:
    + multifluid is a special gas... avoid-re-invent the wheel;

While the user of **MULTIFLUID GAS** class

+ found the same *facility* of the previous gas class:
    + avoid to re-learn from scratch;
+ could re-use his/her own applications designed for gas objects...
}
$endnote

$endcolumns

### Polymorphism

#### Polymorphism

$note
$style[width:100%]
$caption(none){What?}
$content[font-size:100%;]{
Polymorphism is the concept that multiple *types* of objects might be able to work in a given situation.

If you needed to write a message on a piece of paper, you could use a pen, pencil, marker or even a crayon. You only require that the item you use can fit in your hand and can make a mark when pressed against the paper.

In other words it means, one method with multiple implementation, for a certain class of action. And which implementation to be used is decided at both runtime/compiletime (dynamic/static). Overloading is static polymorphism while, overriding is dynamic polymorphism.
}
$endnote

$columns

$column[width:50%]

$note
$style[width:100%]
$caption(none){In practice}
$content[font-size:100%;]{
+ design for *family* of classes (objects);
+ exploit inheritance;
+ exploit encapsulation.
}
$endnote

$column[width:50%]

$note
$style[width:100%]
$caption(none){The Consequence}
$content[font-size:100%;]{
+ be ABSTARCT!
}
$endnote

$endcolumns

#### Polymorphism, a Fluid Example

$columns

$column[width:48%]

$note
$style[width:100%]
$caption(none){Define a Polymorphic Contract}
$content[font-size:80%;]{
Develop your algorithms for family of classes... as you do for your daily-physics
```fortran
module gas_fluxes_t
use gas_t, only : gas
implicit none
private
public :: fluxes
type :: fluxes
  real         :: mass_=0.0
  real         :: energy_=0.0
  type(vector) :: momentum_
  contains
 ...
   procedure, public :: compute
end type fluxes
contains
  subroutine compute(self, gas_cell)
  ! fluxes at interfaces to be computed
  class(fluxes), intent(inout) :: self
  ! gas into the cell from which compute fluxes
  class(gas),    intent(in)    :: gas_cell
  ...
  select type(gas_cell)
  class is (gas_multifluid)
  ...
  class is (gas)
  ...
  end select
  end subroutine compute
end module gas_fluxes_t
```
}
$endnote

$column[width:50%]

$note
$style[width:100%]
$caption(none){Always respect the Contract... Polymorphically is even more easy!}
$content[font-size:75%;]{
```fortran
use gas_t, only : gas
use gas_multifluid_t, only : gas_multifluid
use gas_library, only : compute_fluxes
implicit none
type(gas)            :: air
type(gas_multifluid) :: mixture
type(fluxes)         :: air_fluxes
type(fluxes)         :: mixture_fluxes
! respect the contract, use only public methods
call air%initialize(Cp=1004.8, &
                    Cv=716.0,  &
                    density=0.125)
call mixture%initialize(Cp=1004.8,            &
                        Cv=716.0,             &
                        species_number=3,     &
                        concentrations=[1.0,  &
                                        0.0,  &
                                        0.0], &
                        density=0.125)
! well designed API is auto-explicative...
if (air%speed()>air%speed_of_sound()) then
  call air_fluxes%compute(gas_cell=air)
  call mixture_fluxes%compute(gas_cell=mixture)
end if
```
}
$endnote

$endcolumns

### A case of study: ODE integration

#### A case of study: ODE integration

$note
$style[width:100%]
$caption(none){The Problems... are Abstract!}
$content[font-size:100%;]{
In Mathematical/Physical field problems are **NATURALLY** abstract... Let us consider the Ordinary Differential Equations (ODE) problem
$$
\begin{matrix}
U_t = R(t, U) \\
U_0 = F       \\
\end{matrix}
$$
where

+ *Ut = dU/dt* is time derivative of a *field* U;
+ *U* is the vector of *state* variables being a function of the time-like independent variable *t*;
+ *R* is the (vectorial) residual function;
+ *F* is the (vectorial) initial conditions function.

Such a mathematical formulation is ubiquitous in the mathematical modelling of physical problems (fluid dynamics, chemistry, biology, evolutionary-anthropology, etc...)

*U*, *R* and *F* have different forms for each problem: in a procedural paradigm programming **YOU NEED TO DEVELOP A DIFFERENT IMPLEMENTATION FOR EACH PROBLEM!**... no, we can be more smart...
}
$endnote

#### Be ABSTRACT!

$note
$style[width:100%]
$caption(none){ODE Problem is Abstract!}
$content[font-size:100%;]{
The mathematical formulation of ODE is already **ABSTRACT**:

+ the state-field *U* is an abstract concept:
    + it is required to be (optionally) a function of time;
    + it is required to be *integrable*;
+ the residual function *R* is an abstract concept:
    + it is required to be (optionally) a function of time;
    + it is required to be a function of *U*;
}
$endnote

$columns

$column[width:50%]

$note
$style[width:100%]
$caption(none){Bad Practice}
$content[font-size:100%;]{
Develop an ODE solver for each system of equations:

+ Oscillation Equations (ODE);
+ Lorenz Equations (ODE);
+ Burgers Equations (PDE);
+ Euler Equations (PDE);
+ Navier-Stokes Equations (PDE);
+ ...
}
$endnote

$column[width:50%]

$note
$style[width:100%]
$caption(none){Good Practice}
$content[font-size:100%;]{
Develop an ODE solver for an Abstract ODE system!

*One Ring to rule them all, One Ring to find them, One Ring to bring them all and in the darkness bind them*
}
$endnote

$endcolumns

#### Be High (level)!

$note
$style[width:100%]
$caption(none){(Very) High-Level programming, the AIM}
$content[font-size:80%;]{
We want to code a formula like the following (Euler solver)
$$
U(t+\Delta t) = U(t) + \Delta t U_t
$$
into a Fortran code like the following
```fortran
u = u + Dt * u%t(t=t)
```
The code is very, very similar to the Math... this is our aim!
}
$endnote

$columns

$column[width:30%;padding-right:1%]

$note
$style[width:100%]
$caption(none){Abstract Calculus Pattern will save us!}
$content[font-size:90%;]{
Abstract Calculus Pattern (ACP) is based on the Abstract Data Type (ADT) concept that is the base class upon which solves the (abstract) ODE problem.
}
$endnote

$column[width:70%]

$note
$style[width:100%]
$caption(none){FOODIE, a real application}
$content[font-size:78%;]{
FOODIE, Fortran Object-Oriented Differential-equations Integration Environment

[https://github.com/Fortran-FOSS-Programmers/FOODIE](https://github.com/Fortran-FOSS-Programmers/FOODIE)

Authors:

+ Zaghi, S., [CNR-INSEAN](http://www.insean.cnr.it/);
+ Curcic, M., Dept. of [Ocean Sciences Rosenstiel School](http://www.rsmas.miami.edu/) of University of Miami;
+ Rouson, D., [Sourcery Institute](http://www.sourceryinstitute.org/) and Sourcery, Inc.
+ Beekman, I., Princeton/UMD [CCROCCO LAB](http://croccolab.umd.edu/people/director.html).
}
$endnote

$endcolumns

#### ODE Abstract Integrand

$note
$style[width:100%]
$caption(none){Type Abstract}
$content[font-size:100%;]{
How to define an Abstract Class in (modern) Fortran?
```fortran
type, abstract :: integrand
  contains
    procedure(time_derivative), deferred, public :: t
    ...
end type integrand
```
}
$endnote

$columns

$column[width:50%;padding-right:1%]

$note
$style[width:100%]
$caption(none){Abstract keyword}
$content[font-size:90%;]{
The **ABSTRACT** keyword implies

+ you cannot **instance** a variable of type `integrand`, but only of its (concrete) extensions:
    + an abstract type is really a **CONTRACT**;
+ you can (and want) **develop** for the abstract type `integrand`, you can ignore the implementation:
    + you must be aware (and use) of only its public interface!
}
$endnote

$column[width:50%]

$note
$style[width:100%]
$caption(none){Deferred keyword}
$content[font-size:100%;]{
The **DEFERRED** keyword implies

+ you can (and want) ignore the implementation details of this method;
+ you can (and want) rely on only it public interface;
+ each (concrete) extension of the abstract type `integrand` must provide the implementation of each deferred method.
}
$endnote

$endcolumns

#### ODE Abstract Integrand

$note
$style[width:100%]
$caption(none){The Abstract Integrand}
$content[font-size:90%;]{
How look our (abstract) Integrand?
```fortran
type, abstract :: integrand
  contains
    ! deferred methods
    procedure(time_derivative),      deferred, public :: t
    procedure(symmetric_operator),   deferred         :: integrand_multiply_integrand
    procedure(integrand_op_real),    deferred         :: integrand_multiply_real
    procedure(real_op_integrand),    deferred         :: real_multiply_integrand
    procedure(symmetric_operator),   deferred         :: add
    procedure(symmetric_operator),   deferred         :: sub
    procedure(assignment_integrand), deferred         :: assign_integrand
    ! public interface
    generic, public :: operator(+) => add
    generic, public :: operator(-) => sub
    generic, public :: operator(*) => integrand_multiply_integrand, &
                                      real_multiply_integrand, &
                                      integrand_multiply_real
    generic, public :: assignment(=) => assign_integrand
end type integrand
```
}
$endnote

#### ODE Abstract Integrand

$note
$style[width:100%]
$caption(none){The Abstract Integrand Public Interface}
$content[font-size:100%;]{
The Public Interface is very, very simple, and matches the Math definition of *U*

+ 3 operators (`+-x`);
```fortran
    generic, public :: operator(+)
    generic, public :: operator(-)
    generic, public :: operator(*)
```
+ the assignment (`=`);
```fortran
    generic, public :: assignment(=)
```
+ a time derivative method (`%t`).
```fortran
    procedure(time_derivative), deferred, public :: t
```

With only the knowledge of the **INTERFACE** of these 4 ingredients we can build ALL ODE solvers... just remember the Euler formula...
```fortran
u = u + Dt * u%t(t=t)
```
}
$endnote

#### ODE Abstract Integrand

$columns

$column[width:50%]

$note
$style[width:100%]
$caption(none){The Abstract Integrand Interfaces}
$content[font-size:90%;]{
How to define an **ABSTRACT** method (operator/assignment)? By means of abstract interfaces...
}
$endnote

$note
$style[width:100%]
$caption(none){The Abstract Interfaces}
$content[font-size:70%;]{
```fortran
abstract interface
  function real_op_integrand(lhs, rhs) &
    result(operator_result)
  import :: integrand
  real,             intent(in)  :: lhs
  class(integrand), intent(in)  :: rhs
  class(integrand), allocatable :: operator_result
  end function real_op_integrand

  function time_derivative(self, t) &
    result(dState_dt)
  import :: integrand
  class(integrand),  intent(in) :: self
  real,    optional, intent(in) :: t
  class(integrand), allocatable :: dState_dt
  end function time_derivative

  function integrand_op_real(lhs, rhs) &
    result(operator_result)
  import :: integrand
  class(integrand), intent(in)  :: lhs
  real,             intent(in)  :: rhs
  class(integrand), allocatable :: operator_result
  end function integrand_op_real
end interface
```
}
$endnote

$column[width:50%]

$note
$style[width:100%]
$caption(none){The Abstract Integrand}
$content[font-size:70%;]{
```fortran
type, abstract :: integrand
  contains
    procedure(symmetric_operator),   deferred :: &
      integrand_multiply_integrand
    procedure(integrand_op_real),    deferred :: &
      integrand_multiply_real
    procedure(real_op_integrand),    deferred :: &
      real_multiply_integrand
    procedure(symmetric_operator),   deferred :: add
    ...
end type integrand
```
For all **DEFERRED** methods you must provide an

**EXPLICIT ABSTRACT INTERFACE**
}
$endnote

$note
$style[width:100%]
$caption(none){The Abstract Interfaces (continued)}
$content[font-size:70%;]{
```fortran
abstract interface
  function symmetric_operator(lhs, rhs) &
    result(operator_result)
  import :: integrand
  class(integrand), intent(in)  :: lhs
  class(integrand), intent(in)  :: rhs
  class(integrand), allocatable :: operator_result
  endfunction symmetric_operator

  subroutine assignment_integrand(lhs, rhs)
  import :: integrand
  class(integrand), intent(inout) :: lhs
  class(integrand), intent(in)    :: rhs
  endsubroutine assignment_integrand
end interface
```
}
$endnote

$endcolumns

#### ODE Abstract Integrand

$columns

$column[width:50%]

$note
$style[width:100%]
$caption(none){The Whole Source}
$content[font-size:65%;]{
```fortran
module foodie_adt_integrand
implicit none
private
public :: integrand

type, abstract :: integrand
  contains
    procedure(time_derivative),      deferred :: t
    procedure(symmetric_operator),   deferred :: &
      integrand_multiply_integrand
    procedure(integrand_op_real),    deferred :: &
      integrand_multiply_real
    procedure(real_op_integrand),    deferred :: &
      real_multiply_integrand
    procedure(symmetric_operator),   deferred :: add
    procedure(symmetric_operator),   deferred :: sub
    procedure(assignment_integrand), deferred :: &
      assign_integrand
    generic :: operator(+) => add
    generic :: operator(-) => sub
    generic :: operator(*) => integrand_multiply_integrand,&
                              real_multiply_integrand, &
                              integrand_multiply_real
    generic :: assignment(=) => assign_integrand
endtype integrand

abstract interface
  function time_derivative(self, t) result(dState_dt)
  import :: integrand, R_P
  class(integrand),       intent(IN) :: self
  real(R_P),    optional, intent(IN) :: t
  class(integrand), allocatable      :: dState_dt
  end function time_derivative
  ...
```
}
$endnote

$column[width:50%]

$note
$style[width:100%;]
$caption(none){The Whole Source (continued)}
$content[font-size:65%;]{
```fortran
  ...
  function integrand_op_real(lhs, rhs) &
    result(operator_result)
  import :: integrand, R_P
  class(integrand), intent(IN)  :: lhs
  real(R_P),        intent(IN)  :: rhs
  class(integrand), allocatable :: operator_result
  end function integrand_op_real
  !
  function real_op_integrand(lhs, rhs) &
    result(operator_result)
  import :: integrand, R_P
  real(R_P),        intent(IN)  :: lhs
  class(integrand), intent(IN)  :: rhs
  class(integrand), allocatable :: operator_result
  end function real_op_integrand
  !
  function symmetric_operator(lhs, rhs) &
    result(operator_result)
  import :: integrand
  class(integrand), intent(IN)  :: lhs
  class(integrand), intent(IN)  :: rhs
  class(integrand), allocatable :: operator_result
  end function symmetric_operator
  !
  subroutine assignment_integrand(lhs, rhs)
  import :: integrand
  class(integrand), intent(INOUT) :: lhs
  class(integrand), intent(IN)    :: rhs
  end subroutine assignment_integrand
end interface
end module foodie_adt_integrand
```
}
$endnote

$endcolumns

#### ODE Solver: Euler

$note
$style[width:100%;]
$caption(none){Using the Abstract Integrand}
$content[font-size:85%;]{
*With the so simple abstract integrand we cannot do anything serious*, you are thinking... WRONG! You can build **OPERATIVE** procedures on it. Let us see the Euler solver built on it
}
$endnote

$columns
$column[width:50%]

$note
$style[width:100%;]
$caption(none){}
$content[font-size:85%;]{
```fortran
module foodie_integrator_euler_explicit
use foodie_adt_integrand, only : integrand
implicit none
private
public :: euler_explicit_integrator
!
type :: euler_explicit_integrator
  private
  contains
    private
    procedure, nopass, public :: integrate
end type euler_explicit_integrator
contains
  subroutine integrate(U, Dt, t)
  class(integrand),    intent(inout) :: U
  real(R_P),           intent(in)    :: Dt
  real(R_P), optional, intent(in)    :: t
  U = U + U%t(t=t) * Dt
  return
  end subroutine integrate
end module foodie_integrator_euler_explicit
```
}
$endnote

$column[width:50%]

$note
$style[width:100%;]
$caption(none){}
$content[font-size:85%;]{
Compare with the math
$$
U(t+\Delta t) = U(t) + \Delta t U_t
$$
It is almost the same...

+ no knowldge of the concrete implementation on U is assumed;
+ only the public API of U is used, e.g. `U%t, U+U, U*Dt...`;
+ the `euler` integrator accepts ALL integrand extensions!

```fortran
use foodie_integrator_euler_explicit
use my_integrand
type(euler_explicit_integrator) :: solver
type(my_integrand)              :: U
...
call solver%integrate(U=U, Dt=0.1)
```
}
$endnote

$endcolumns
Well, 20 lines for an ODE solver, and it is ABSTRACT! It can solve **ANY** ODE!

#### ODE Solver: Runge-Kutta

$note
$style[width:100%;]
$caption(none){Using the Abstract Integrand (continued)}
$content[font-size:85%;]{
The Euler solver is **trivial**, *you are not able to do much more*, you are thinking... WRONG AGAIN! Let us see the Low Storage Runge-Kutta solver
}
$endnote

$columns
$column[width:50%]

$note
$style[width:100%;]
$caption(none){}
$content[font-size:52%;]{
```fortran
module foodie_integrator_low_storage_runge_kutta
use foodie_adt_integrand, only : integrand
implicit none
private
public :: ls_runge_kutta_integrator
!
type :: ls_runge_kutta_integrator
  private
  integer           :: stages=0 !< Number of stages.
  real, allocatable :: A(:)     !< Low storage *A* coefficients.
  real, allocatable :: B(:)     !< Low storage *B* coefficients.
  real, allocatable :: C(:)     !< Low storage *C* coefficients.
  contains
    private
    ...
    procedure, pass(self), public :: integrate
endtype ls_runge_kutta_integrator
contains
  ...
  subroutine integrate(self, U, stage, Dt, t)
  class(ls_runge_kutta_integrator), intent(in)    :: self
  class(integrand),                 intent(inout) :: U
  class(integrand),                 intent(inout) :: stage(1:)
  real(R_P),                        intent(in)    :: Dt
  real(R_P),                        intent(in)    :: t
  integer(I_P)                                    :: s
  select type(stage)
  class is(integrand)
    stage(1) = U
    stage(2) = U*0._R_P
    do s=1, self%stages
      stage(2) = stage(2) * self%A(s) + &
        stage(1)%t(t=t + self%C(s) * Dt) * Dt
      stage(1) = stage(1) + stage(2) * self%B(s)
    enddo
    U = stage(1)
  endselect
  end subroutine integrate
end module foodie_integrator_low_storage_runge_kutta
```
}
$endnote

$column[width:50%]

$note
$style[width:100%;]
$caption(none){}
$content[font-size:75%;]{
Compare with the math
$$
\begin{matrix}
 K_1 = U^n \\
 K_2 = 0 \\
\left.\begin{matrix}
 K_2 = A_s K_2 + \Delta t R(t^n + C_s \Delta t, K_1) \\
 K_1 = K_1 + B_s K_2
\end{matrix}\right\} s=1,2,...N_s\\
U^{n+1} = K_1
\end{matrix}
$$
It is almost the same...

+ no knowldge of the concrete implementation on U is assumed;
+ only the public API of U is used, e.g. `U%t, U+U, U*Dt...`;
+ the `euler` integrator accepts ALL integrand extensions!

```fortran
use foodie_integrator_low_storage_runge_kutta
use my_integrand
type(ls_runge_kutta_integrator) :: solver
type(my_integrand)              :: U
...
call solver%integrate(U=U, Dt=0.1)
```
}
$endnote

$endcolumns
Well, 20 lines for a Low Storage RK ODE solver, and it is ABSTRACT! It can solve **ANY** ODE!

#### Concrete Integrand: Oscillation ODE

$note
$style[width:100%;]
$caption(none){How Difficult is to define a Concrete ODE upon the abstract integrand?}
$content[font-size:75%;]{
It is really dead-simple, just **RESPECT THE CONTRACT!**
}
$endnote

$columns
$column[width:50%]

$note
$style[width:100%;]
$caption(none){}
$content[font-size:65%;]{
```fortran
module oscillation_ode
use foodie, only : integrand
implicit none
private
public :: oscillation
type, extends(integrand) :: oscillation
  private
  real :: f=0.0
  real :: U(1:2)=[0.0, 0.0]
  contains
    procedure, pass(self), public :: t => &
      dOscillation_dt
    procedure, pass(lhs),  public :: &
      integrand_multiply_integrand => &
      oscillation_multiply_oscillation
    procedure, pass(lhs),  public :: &
      integrand_multiply_real => &
      oscillation_multiply_real
    procedure, pass(rhs),  public :: &
      real_multiply_integrand => &
      real_multiply_oscillation
    procedure, pass(lhs),  public :: add => &
      add_oscillation
    procedure, pass(lhs),  public :: sub => &
      sub_oscillation
    procedure, pass(lhs),  public :: &
      assign_integrand => &
      oscillation_assign_oscillation
end type oscillation
contains
  ...
end module oscillation_ode
```
}
$endnote

$column[width:50%]

$note
$style[width:100%;]
$caption(none){}
$content[font-size:65%;]{
$$
\begin{matrix}
U_t = R(U) \\
U = [v_1, v_2] R(U) = [-fv_2, fv_1]
\end{matrix}
$$

```fortran
  ...
  function dOscillation_dt(self, t) result(dState_dt)
  class(oscillation),     intent(in) :: self
  real(R_P),    optional, intent(in) :: t
  class(integrand),  allocatable     :: dState_dt
  allocate(oscillation :: dState_dt)
  select type(dState_dt)
  class is(oscillation)
    dState_dt = self
    dState_dt%U(1) = -self%f * self%U(2)
    dState_dt%U(2) =  self%f * self%U(1)
  endselect
  end function dOscillation_dt

  function oscillation_multiply_oscillation(lhs, rhs) &
    result(opr)
  class(oscillation), intent(in) :: lhs
  class(integrand),   intent(in) :: rhs
  class(integrand), allocatable  :: opr
  allocate(oscillation :: opr)
  select type(opr)
  class is(oscillation)
    opr = lhs
    select type(rhs)
    class is (oscillation)
      opr%U = lhs%U * rhs%U
    endselect
  endselect
  end function oscillation_multiply_oscillation
```
}
$endnote

$endcolumns

#### Abstract ODE Integration: Why?
$note
$style[width:100%;]
$caption(none){Why bothering with Abstracts?}
$content[font-size:95%;]{
So, to integrate the Oscillation ODE we have passed through the **EXTENSION of ABSTRACT INTEGRAND**, this is an overhead. *why bothering with it?* you are thinking. Because:

+ for ODE schemes developers:
    + work with only one, standard, derived type without the necessity to care about what the type is actually modelling (it could be the velocity field of a turbulent flow, the magnetic field of a circuit, etc...);
    + express the ODE solver formula with very high-level language; this ensures:
        + clearness!
        + fast-developing!
    + rely only on the time-derivative function and on the operators overloaded definitions;
    + be completely agnostic on which is the actual implementation of the time-derivative residual function!
+ for FOODIE clients:
    + a simple, standard API for many ODE solvers:
        + fast-developing of new schemes;
        + roboustness: the same ODE solver is applied to differnt problems... cross-validation;
}
$endnote

#### Abstract ODE Integration: Performance?

$columns

$column[width:50%]
OpenMP benchmarks

$figure
$content[width:99%;]{images/openmp-strong-scaling-comparison.png}
$caption(none){strong scaling}
$endfigure

$figure
$content[width:99%;]{images/openmp-weak-scaling-comparison.png}
$caption(none){weak scaling}
$endfigure

$column[width:50%]
CAF benchmarks

$figure
$content[width:99%;]{images/caf-strong-scaling-comparison.png}
$caption(none){strong scaling}
$endfigure

$figure
$content[width:99%;]{images/caf-weak-scaling-comparison.png}
$caption(none){weak scaling}
$endfigure

$endcolumns

#### FOODIE Features

$columns
$column[width:50%]
$note
$style[width:100%;]
$caption(none){The Long List}
$content[font-size:60%;]{
+ Pure Fortran implementation;
+ KISS and user-friendly:
    + simple API, presently based on the Rouson's Abstract Data Type Pattern;
    + easy building and porting on heterogeneous architectures;
+ comprehensive solvers set out-of-the-box:
    + explicit schemes:
        + [Adams-Bashforth](http://fortran-foss-programmers.github.io/FOODIE/module/foodie_integrator_adams_bashforth.html) schemes:
            + 1 step, namely the forward explicit Euler scheme, 1st order accurate;
            + 2 to 16 steps, 2nd to 16th accurate, respectively;
        + [Euler](http://fortran-foss-programmers.github.io/FOODIE/module/foodie_integrator_euler_explicit.html) (forward explicit) scheme, 1st order accurate;
        + [Leapfrog](http://fortran-foss-programmers.github.io/FOODIE/module/foodie_integrator_leapfrog.html), 2nd order accurate:
            + unfiltered leapfrog, 2nd order accurate, mostly unstable;
            + Robert-Asselin filtered leapfrog, 1st order accurate;
            + Robert-Asselin-Williams filtered leapfrog, 3rd order accurate;
        + Runge-Kutta schemes:
            + [low-storage](http://fortran-foss-programmers.github.io/FOODIE/module/foodie_integrator_low_storage_runge_kutta.html) schemes:
                + 1 stage, namely the forward explicit Euler scheme, 1st order accurate;
                + 5 stages, 4th order accurate, 2N registers;
                + 6 stages, 4th order accurate, 2N registers;
                + 7 stages, 4th order accurate, 2N registers;
                + 12 stages, 4th order accurate, 2N registers;
                + 14 stages, 4th order accurate, 2N registers;
}
$endnote
$column[width:50%]
$note
$style[width:100%;]
$caption(none){The Long List (continued)}
$content[font-size:60%;]{
+ comprehensive solvers set out-of-the-box:
    + explicit schemes:
        + Runge-Kutta schemes:
            + [TVD/SSP](http://fortran-foss-programmers.github.io/FOODIE/module/foodie_integrator_tvd_runge_kutta.html) schemes:
                + 1 stage, namely the forward explicit Euler scheme, 1st order accurate;
                + 2 stages, 2nd order accurate;
                + 3 stages, 3rd order accurate;
                + 5 stages, 4th order accurate;
            + [embedded (adaptive)](http://fortran-foss-programmers.github.io/FOODIE/module/foodie_integrator_emd_runge_kutta.html) schemes:
                + Heun-Euler, 2 stages, 2nd order accurate;
                + Runge-Kutta-Cash-Karp, 6 stages, 5th order accurate;
                + Prince-Dormand, 7 stages, 4th order accurate;
                + Calvo, 9 stages, 6th order accurate;
                + Feagin, 17 stages, 10th order accurate;
    + implicit schemes:
        + [Adams-Moulton](http://fortran-foss-programmers.github.io/FOODIE/module/foodie_integrator_adams_moulton.html) schemes:
            + 0 step, 1st order accurate;
            + 1 step, 2nd accurate;
            + 2 steps, 3rd accurate;
            + 3 steps, 4th accurate;
        + [Backward Differentiation Formula](http://fortran-foss-programmers.github.io/FOODIE/module/foodie_integrator_backward_differentiation_formula.html) schemes:
            + 1 step, namely the backward implicit Euler scheme, 1st order accurate;
            + 2 to 6 steps, 2nd to 6th accurate, respectively;
    + predictor-corrector schemes:
        + [Adams-Bashforth-Moulton](http://fortran-foss-programmers.github.io/FOODIE/module/foodie_integrator_adams_bashforth_moulton.html) schemes:
            + 1 step, AB(1)-AM(0), 1st order accurate;
            + 2 steps, AB(2)-AM(1), 2nd accurate;
            + 3 steps, AB(3)-AM(2), 3rd accurate;
            + 4 steps, AB(4)-AM(3), 4th accurate;
}
$endnote
$endcolumns
