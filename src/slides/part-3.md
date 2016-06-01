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
}
$endnote

### Encapsulation

#### Encapsulation

### Inheritance

#### Inheritance

### Polymorphism

#### Polymorphism

### Abstraction

#### Abstraction

### A case of study: ODE integration

#### A case of study: ODE integration
