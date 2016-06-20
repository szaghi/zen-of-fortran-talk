## OpenMP

### OpenMP

#### OpenMP, the easy parallel way...

$note
$caption(none){What is OpenMP?}
$content[font-size:100%;]{
+ de-facto standard **Application Program Interface (API)** to write **shared memory** parallel applications in C, C++ and Fortran
    + base on compilers **directives, run-time routines and environment variables**;
+ mean *Open specifications for Multi Processing* maintained by the [OpenMP Architecture Review Board, http://www.openmp.org](http://www.openmp.org);
+ base on **fork-join** model:
    + **workers** (thread) do the work in parallel regions;
    + they **cooperate** through **shared memory**;
+ on the contrary of MPI, base on **memory accesses** instead of **explicit messages**;
    + *local* model parallelization of the serial code;
    + allow an **incremental parallelization**.
}
$endnote

#### Architectures

$note
$caption(none){Shared Memory}
$content[font-size:100%;]{
+ all processors access all memory as **global address space**;
+ multiple processors can operate independently but **share the same memory resources**;
    + changes in a memory location effected by one processor are visible to all other processors, do you say **ray condition?**;
+ historically, shared memory machines have been classified as UMA and NUMA, based upon memory access times.
}
$endnote

$columns

$column[width:50%]

$note
$caption(none){Uniform Memory Access (UMA)}
$content[font-size:95%;]{
+ most commonly represented today by Symmetric Multiprocessor (SMP) machines;
+ identical processors;
+ equal access and access times to memory;

Sometimes called CC-UMA - Cache Coherent UMA. Cache coherent means if one processor updates a location in shared memory, all the other processors know about the update. Cache coherency is accomplished at the hardware level.
}
$endnote

$column[width:50%]

$note
$caption(none){ Non-Uniform Memory Access (NUMA)}
$content[font-size:95%;]{
+ often made by physically linking two or more SMPs;
+ one SMP can directly access memory of another SMP;
+ not all processors have equal access time to all memories
    + memory access across link is slower
}
$endnote

$endcolumns

#### Architectures (continued)

$columns

$column[width:50%]

$figure
$style[width:100%;]
$content{images/shared_mem.png}
$caption(none){**Shared Memory**, courtesy of [LLNL](https://computing.llnl.gov/tutorials/parallel_comp)}
$endfigure

$note
$caption(none){Pros}
$content[font-size:80%;]{
+ user-friendly programming perspective to memory;
+ data sharing between tasks is both fast and uniform due to the proximity of memory to CPUs;
}
$endnote

$note
$caption(none){Cons}
$content[font-size:80%;]{
+ lack of scalability between memory and CPUs;
+ programmer responsibility for synchronization constructs that ensure "correct" access of global memory.
}
$endnote

$column[width:50%]

$figure
$style[width:100%;]
$content{images/distributed_mem.png}
$caption(none){**Distributed Memory**, courtesy of [LLNL](https://computing.llnl.gov/tutorials/parallel_comp)}
$endfigure

$note
$caption(none){Pros}
$content[font-size:80%;]{
+ memory is scalable with the number of processors;
+ cost effectiveness: can use commodity, off-the-shelf processors and networking.
}
$endnote

$note
$caption(none){Cons}
$content[font-size:80%;]{
+ the programmer is responsible for many of the details associated with data communication between processors;
+ difficult to map existing data structures, based on global memory, to this memory organization;
+ non-uniform memory access times - data residing on a remote node takes longer to access than node local data.
}
$endnote

$endcolumns

#### OpenMP Fork-Join Model

$figure
$style[width:90%;]
$content{images/fork_join2.gif}
$caption(none){**Fork-Join Model**, courtesy of [LLNL](https://computing.llnl.gov/tutorials/parallel_comp)}
$endfigure

$note
$caption(none){The model}
$content[font-size:90%;]{
+ all OpenMP programs begin as a single process: the **master thread**. The master thread executes sequentially until the first parallel region construct is encountered;
+ **FORK**: the master thread then creates a team of parallel threads:
    + the statements in the program that are enclosed by the parallel region construct are then executed in parallel among the various team threads;
    + **JOIN**: when the team threads complete the statements in the parallel region construct, they synchronize and terminate, leaving only the master thread.

The number of parallel regions and the threads that comprise them are arbitrary.
}
$endnote

#### In practice, the main ingredients

$note
$caption(none){3 Pillars}
$content[font-size:80%;]{
| Compiler Directives | Environment Variables | | Runtime Library Routines

It is not **Fortran Standard!**
}
$endnote

$note
$caption(none){Compiler Directives}
$content[font-size:95%;]{
+ comments in your source code and are ignored by compilers unless you tell them otherwise:
    + serial and parallel codes **live togheter!**
        + a serial compilation ignores the directives;
        + a compilation with OpenMP support takes them into account;
+ mark a block of code;
+ specify to the compiler how to run in parallel the code block:
    + Dividing blocks of code among threads
    + Distributing loop iterations between threads
    + Serializing sections of code
    + Synchronization of work among threads
```fortran
!$OMP some_directives
! fortran statements to be done in parallel
do i=1...
```
}
$endnote

#### Clauses

$note
$caption(none){Compiler Directives are based on Caluses}
$content[font-size:100%;]{
+ syntax: `directive [clause[ clause]...]`
+ specify additional information to the directives:
    + variables handling:
        + What are shared among all threads (the default);
        + Which are private to each thread;
        + How to initialize the private ones;
        + What is the default;
    + execution control:
        + how many threads in the team;
        + how to distribute the work;

ATTENTION: they may alter code semantic: the code can be corrected in serial but not in parallel or viceversa.
}
$endnote

#### Environment Variables

$note
$caption(none){The most used envs}
$content[font-size:100%;]{
+ `OMP_NUM_THREADS`: sets number of threads
+ `OMP_STACKSIZE size [B|K|M|G]`: size of the stack for threads
+ `OMP_DYNAMIC {TRUE|FALSE}`: dynamic thread adjustment
+ `OMP_SCHEDULE "schedule[,chunk]`: iteration scheduling scheme
+ `OMP_PROC_BIND {TRUE|FALSE}`: bound threads to processors
+ `OMP_NESTED {TRUE|FALSE}`: nested parallelism

To set them

+ In csh/tcsh:
```csh
setenv OMP_NUM_THREADS 4
```
+ In sh/bash:
```bash
export OMP_NUM_THREADS=4
```
}
$endnote

#### Runtime Library

$note
$caption(none){The most used functions}
$content[font-size:100%;]{
+ Query/specify some specific feature or setting:
    + `omp_get_thread_num()`: get thread ID (0 for master thread)
    + `omp_get_num_threads()`: get number of threads in the team
    + `omp_set_num_threads(int n)`: set number of threads
+ Allow you to manage fine-grained access (lock):
    + `omp_init_lock(lock_var)`: initializes the OpenMP lock variable `lock_var` of type `omp_lock_t`
+ Timing functions:
    + `omp_get_wtime()`: returns elapsed wallclock time
    + `omp_get_wtick()`: returns timer precision

**Functions interface**
```fortran
use omp_lib

! or old-style include

include 'omp_lib.h'
```
}
$endnote

#### Parallel Construct

$note
$caption(none){Parallel construct}
$content[font-size:90%;]{
+ create a parallel region
    + a construct is the lexical extent to which an executable directive applies
    + a region is the dynamic extent to which an executable directive applies
    + a parallel region is a block of code executed by all threads in the team
```fortran
!$omp parallel
! some code to execute in parallel
print "(A)", 'Hello (parallel) World!'
!$omp end parallel
```
+ Inside a parallel region, the variables of the serial program can be essentially **shared** or **private**
    + **shared**: there is only one instance of the data
        + Data is accessible by all threads in the team
        + Threads can read and write the data simultaneously
        + All threads access the same address space
    + **private**: each thread has a copy of the data
        + No other thread can access this data
        + Changes are only visible to the thread owning the data
        + Values are undefined on entry and exit

Variables are shared by default but with the clause `default(none)` no implicit default, you have to scope all variables explicitly, sane way!
}
$endnote

#### Race condition

$note
$caption(none){Data races, the `critical` construct}
$content[font-size:100%;]{
+ A data race is when two or more threads access the same(=shared) memory location
    + Asyncronously and
    + Without holding any common exclusive locks and
    + At least one of the accesses is a write/store
    + In this case the resulting values are undefined
+ The block of code inside a critical construct is executed by only one thread at time
+ It is a syncronization to avoid simultaneous access to shared data
```fortran
sum = 0
!$omp parallel private(i, MyThreadID, psum)
MyThreadID = omp_get_thread_num()
NumThreads = omp_get_num_threads()
psum =0
do i=MyThreadID*N/NumThreads+1, min((MyThreadID+1)*N/NumThreads, N)
  psum = psum + x(i)
end do
!$omp critical
sum = sum + psum;
!$omp end critical
!$omp end parallel
```
}
$endnote

#### OpenMP is easy!

$note
$caption(none){What we learned?}
$content[font-size:95%;]{
+ Essentially for a parallelization it could be enough:
    + the `parallel` construct
    + the `critical` construct
    + the `omp_get_thread_num()` function
    + and the `omp_get_num_threads()` function
+ But we need to distribute the serial work among threads
+ And doing it by hand is tiring
+ The **worksharing** constructs automate the process
}
$endnote

$note
$caption(none){Worksharing Constructs}
$content[font-size:95%;]{
+ distribute the execution of the associated parallel region over the threads
+ a worksharing region has **no barrier on entry**, but an **implied barrier** exists at its end
    + if a **nowait** clause is present, an implementation may omit the barrier at the end of the worksharing region
+ The OpenMP API defines the following worksharing constructs:
    + `do` loop construct
    + `sections` construct
    + `single` construct
    + `workshare` construct
}
$endnote

#### Loop construct

$note
$caption(none){The King of Constructs}
$content[font-size:95%;]{
+ The iterations of the loop are distributed over the threads that already exist in the team
+ The iteration variable of the loop is made private by default
+ The inner loops are executed sequentially by each thread
+ Beware the data-sharing attribute of the inner loop iteration variables
    + In Fortran they are private by default

```fortran
program doexample
integer, parameter:: n=10
integer:: i, a(n), b(n), c(n)
!$omp parallel
!$omp do
do i=1, n
  a(i) = i
  b(i) = i
  c(i) = 0
end do
!$omp end do
!$omp do
do i=1, n
  c(i) = a(i) + b(i);
end do
!$omp end do
!$omp end parallel
end program doexample
```
}
$endnote

#### Sections construct

$columns

$column[width:40%]
$note
$caption(none){Sectioning your algorithm}
$content[font-size:95%;]{
+ distribute structured blocks of code among threads in the team
    + each thread receives a section
+ when a thread has finished to execute its section, it receives another section
    + if there are no other sections to execute, threads wait for others to end up
}
$endnote

$column[width:60%]
```fortran
program vec_add_sections
integer, parameter :: n=100
integer            :: i
real               :: a(n), b(n), c(n), d(n)
do i = 1, n
  a(i) = i * 1.5
  b(i) = i + 22.35
enddo
!$omp parallel shared(a,b,c,d), private(i)
!$omp sections

!$omp section
  do i = 1, n
    c(i) = a(i) + b(i)
  enddo

!$omp section
  do i = 1, n
     d(i) = a(i) * b(i)
  enddo

!$omp end sections nowait
!$omp end parallel
end
```
$endcolumns

#### Single construct

$note
$caption(none){This is mine!}
$content[font-size:100%;]{
+ the first thread that reaches it executes the associated block
+ the other threads in the team wait at the implicit barrier at the end of the construct unless a nowait clause is specified

```fortran
integer, parameter :: n=100
integer            :: i
real               :: a(n), b(n), c(n), d(n)
...
!$omp parallel
!$omp single
  a = b / c
  print*, a
!$omp end single

!$omp do
do i = 1, n
  d(i) = a(i) * b(i)
enddo
!$omp end parallel
```
}
$endnote

#### Single construct

$columns

$column[width:40%]
$note
$caption(none){Array syntax all-together!}
$content[font-size:95%;]{
+ The structured block enclosed in the workshare construct is divided into units of work that are then assigned to the thread such that each unit is executed by one thread only once
+ It is only supported in Fortran in order to parallelize the array
syntax

}
$endnote

$column[width:60%]
```fortran
program workshare
integer, parameter :: n=100
integer            :: i, j
real               :: aa(n,n), bb(n,n)
real               :: cc(n,n), dd(n,n)
real               :: first, last

do i = 1, n
  do j = 1, n
    aa(j,i) = i * 1.0
    bb(j,i) = j + 1.0
  enddo
enddo

!$omp parallel shared(aa,bb,cc,dd,first,last)

!$omp workshare
  cc = aa * bb
  dd = aa + bb
  first = cc(1,1) + dd(1,1)
  last = cc(n,n) + dd(n,n)
!$omp end workshare nowait

!$omp end parallel

end
```
$endcolumns

#### Data-sharing Attributes

$note
$caption(none){Data-sharing Attributes}
$content[font-size:95%;]{
+ in a parallel construct the data-sharing attributes are implicitly determined by the **DEFAULT** clause, if present
    + if no default clause is present they are **SHARED**
+ Certain variables have a *predetermined data-sharing attributes*
    + Variables with automatic storage duration that are declared in a scope inside a construct are private
    + Objects with dynamic storage duration are shared
    + The loop iteration variable(s) in the associated for-loop(s) of a for construct is (are) private
    + A loop iteration variable for a sequential loop in a parallel construct is private in the innermost such construct that encloses the loop (only Fortran)
    + Variables with static storage duration that are declared in a scope inside the construct are shared
}
$endnote

#### Data-sharing Attributes (continued)

$columns

$column[width:50%]

$note
$caption(none){Data-sharing Attributes}
$content[font-size:85%;]{
+ **Explicitly determined** data-sharing attributes are those that are referenced in a given construct and are listed in a data-sharing attribute clause
+ `shared(list)`: there is only one instance of the objects in the list accessible by all threads in the team
+ `private(list)`: each thread has a copy of the variables in the list
+ `firstprivate(list)`: same as private but all variables in the list are initialized with the value that the original object had before entering the parallel construct
+ `lastprivate(list)`: same as private but the thread that executes the sequentially last iteration or section updates the value of the objects in the list
+ The default clause sets the implicit default
    + `default(none|shared|private|firstprivate)`
```fortran
!$omp parallel do reduction(+:sum)
do i=1, n
  sum = sum + x(i)
end do
!$omp end parallel do
```
}
$endnote

$column[width:50%]

$note
$caption(none){Reduction}
$content[font-size:80%;]{
}
$endnote

+ With the Data-Sharing attributes clause `reduction(op:list)`
    + For each list item, a private copy is created in each implicit task
    + The local copy is initialized appropriately according to the operator (for example, if op is `+` they are initialized to `0`, if op is `*` they are initialized to `1`)
    + After the end of the region, the original list item is updated with the values of the private copies using the specified operator
+ Supported operators for a reduction clause are:
    + `+, *, -, .and., .or., .eqv., .neqv., max, min, iand, ior, ieor`
+ Reduction variables must be **SHARED** variables
+ The reduction clause is valid on `parallel`, `do` loop and `sections` constructs

$endcolumns

#### Advanced OpenMP

$note
$caption(none){Advanced OpenMP}
$content[font-size:100%;]{
+ Synchronization functions
    + `barrier` construct;
    + `atomic` construct;
+ Task Parallelism (Main addition to OpenMP 3.0 enhanced in 3.1 and 4.0)
    + Allows to parallelize irregular problems
    + Unbounded loop
    + Recursive algorithms
    + Producer/consumer schemes
    + Multiblock grids, Adaptive Mesh Refinement
    + ...

Not covered today!
}
$endnote

#### Examples

$columns

$column[width:50%]

$note
$caption(none){Counting the Prime Numbers}
$content[font-size:80%;]{
```fortran
subroutine prime_number(n, total)
! Return the number of primes between 1 and N.
implicit none
integer, intent(in)  :: n
integer, intent(out) :: total
integer              :: i
integer              :: j
integer              :: prime
total = 0
!$omp parallel &
!$omp shared(n) &
!$omp private(i, j, prime)
!$omp do reduction (+: total)
do i=2, n
  prime = 1
  do j=2, i - 1
    if (mod(i, j) == 0) then
      prime = 0
      exit
    end if
  end do
  total = total + prime
end do
!$omp end do
!$omp end parallel
return
end
```
}
$endnote

$column[width:50%]

$note
$caption(none){Matrix Multiplication}
$content[font-size:70%;]{
```fortran
use omp_lib
real, parameter    :: pi = 3.141592653589793D+00
integer, parameter :: n=2000
real               :: a(n,n), b(n,n), c(n,n)
integer            :: i, j, k
real               :: s, angle
integer            ::
s = 1.0D+00 / sqrt(real(n, kind = 8))
!$omp parallel shared (a, b, c, n, s) &
!$omp private (angle, i, j, k)
!$omp do
do i = 1, n
  do j = 1, n
    angle = 2.0D+00 * pi * (i - 1) * (j - 1) / real(n)
    a(i,j) = s * (sin(angle) + cos(angle))
  end do
end do
!$omp end do
!$omp do
do i = 1, n
  do j = 1, n
    b(i,j) = a(i,j)
  end do
end do
!$omp end do
!$omp do
do i = 1, n
  do j = 1, n
    c(i,j) = 0.0D+00
    do k = 1, n
      c(i,j) = c(i,j) + a(i,k) * b(k,j)
    end do
  end do
end do
!$omp end do
!$omp end parallel
```
}
$endnote

$endcolumns
