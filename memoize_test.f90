program memoize_test
!==============================================================================#
! MEMOIZE_TEST
!------------------------------------------------------------------------------#
! Author:  Edward Higgins <ed.higgins@york.ac.uk>
!------------------------------------------------------------------------------#
! Version: 0.1.1, 2018-03-28
!------------------------------------------------------------------------------#
! This code is distributed under the MIT license.
!==============================================================================#
  implicit none

  integer, parameter :: li = selected_int_kind(12)

  type :: hash_node
    integer :: ref
    integer :: datum
    type(hash_node), pointer :: next
  end type hash_node

  type :: hash_bin
    type(hash_node), pointer  :: node
    integer :: items
  end type hash_bin

  type :: hash_table
    type(hash_bin), allocatable :: bins(:)
    integer :: nbins
    logical :: initialised = .false.
  end type hash_table

  integer :: i, N=40


  do i=1, N
    print *, i, fibonacci(i)
  end do

contains

  recursive function fibonacci(n) result(fib)
    integer :: fib
    integer :: n

    select case(n)
      case(1:2)
        fib = 1

      case (3:)
        fib = fibonacci(n-1) + fibonacci(n-2)
      case default
        stop "Cannot calculate the fibonacci of a negative number"
    end select

  end function fibonacci

  recursive function memoized_fib(n) result(fib)
    integer :: fib
    integer :: n

    type(hash_table), save :: cache

    if (.not. cache%initialised) call hash_initialise(cache, 100)

    if (hash_contains(cache, n)) then
      fib = hash_get(cache, n)
    else
      select case(n)
        case(1:2)
          fib = 1

        case (3:)
          fib = memoized_fib(n-1) + memoized_fib(n-2)
        case default
          stop "Cannot calculate the fibonacci of a negative number"
      end select
      call hash_put(cache, n, fib)
    end if

  end function memoized_fib

  function hash(i, n)
    integer :: hash
    integer :: i, n

    integer(kind=li) :: tmp

    tmp = i*2654435761_li
    hash = mod(tmp,n)+1

  end function hash

  subroutine hash_initialise(h, n)
    type(hash_table) :: h
    integer :: n

    integer :: ibin

    if ( h%initialised ) stop "Hash is already initialised"

    allocate(h%bins(1:n))
    do ibin = 1, n
      h%bins(ibin)%node  => null()
      h%bins(ibin)%items = 0
    end do
    h%nbins = n

    h%initialised = .true.

  end subroutine hash_initialise

  logical function hash_contains(h, x)
    type(hash_table) :: h
    integer :: x

    type(hash_node), pointer :: node
    integer :: c, inode

    c = hash(x,h%nbins)

    node => h%bins(c)%node

    hash_contains = .false.
    do inode = 1, h%bins(c)%items
      if(node%ref == x) then
        hash_contains = .true.
        exit
      end if
      node => node%next
    end do

  end function hash_contains

  subroutine hash_put(h, i, d)
    type(hash_table) :: h
    integer :: i
    integer :: d

    integer :: c, inode
    type(hash_node), pointer :: node

    c = hash(i,h%nbins)

    if (h%bins(c)%items == 0) then
      allocate(h%bins(c)%node)
      node => h%bins(c)%node

    else
      node => h%bins(c)%node
      do inode = 1, h%bins(c)%items
        node = node%next
      end do
      allocate(node%next)
      node => node%next
    end if

    node%next => null()
    node%ref = i
    node%datum = d

    h%bins(c)%items = h%bins(c)%items + 1

  end subroutine hash_put

  function hash_get(h, i) result(d)
    type(hash_table) :: h
    integer :: i
    integer :: d

    integer :: c, inode
    type(hash_node), pointer :: node

    c = hash(i,h%nbins)

    node => h%bins(c)%node

    do inode = 1, h%bins(c)%items
      if(node%ref == i) exit
      node = node%next
    end do
    d = node%datum

  end function hash_get

end program memoize_test
