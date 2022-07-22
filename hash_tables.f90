module hash_tables
!==============================================================================#
! HASH_TABLES
!------------------------------------------------------------------------------#
! Description: A reference implementation of hash tables in Fortran, taking a
!   a character key up to length max_key_len, and returning some associated
!   value.
!------------------------------------------------------------------------------#
! Author:  Edward Higgins <ed.higgins@york.ac.uk>
!------------------------------------------------------------------------------#
! Version: 0.1.1, 2018-03-29
!------------------------------------------------------------------------------#
! This code is distributed under the MIT license.
!==============================================================================#
  implicit none

  private

  integer, parameter  :: max_key_len = 128

  ! A single key/value entry in the hash table
  type, public :: hash_entry
    private
    character(max_key_len), public    :: key  = ""      ! Entry key
    integer,                public    :: val  = 0       ! Entry value
    type(hash_entry),       pointer   :: next => null() ! Next item in the bucket
  end type hash_entry

  ! A bucket containing a linked list of entries
  type :: hash_bucket
    type(hash_entry), pointer :: node => null()  ! The linked list
    integer                   :: items = 0       ! # of items in the list
  end type hash_bucket

  ! The hash table itself
  type, public :: hash_table
    private
    integer, public                :: nbuckets         = 0        ! # of buckets
    logical, public                :: initialised      = .false.  ! Allocated or not
    type(hash_bucket), allocatable :: buckets(:)                  ! The buckets
    integer                        :: iterator_pos(2)  = [1,0]    ! position of the %next iterator
  contains
    procedure :: create   => hash_create  ! Allocate & initialise the hash table
    procedure :: put      => hash_put     ! Add/update an entry in the table
    procedure :: get      => hash_get     ! Get a value from the table
    procedure :: remove   => hash_remove  ! Remove an entry from the table
    procedure :: destroy  => hash_destroy ! Deallocate & reset the table
    procedure :: first    => hash_first   ! Reset the iterator & return the first value
    procedure :: next     => hash_next    ! Increment the iterator & return the next value
  end type hash_table

contains
  !------------------------------------------------------------------------------!
    function compute_hash(key, n) result(hash)                                   !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   A function to compute the hash of a given key, using a Rabin-Karp rolling  !
  !   hash function.                                                             !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   character(len=*), intent(in)  :: key                                       !
  !     The key to be hashed                                                     !
  !                                                                              !
  !   integer,          intent(in)  :: n                                         !
  !     Range of the resultant hash (0 < hash <= n)                              !
  !------------------------------------------------------------------------------!
  ! RESULT                                                                       !
  !   integer :: hash                                                            !
  !     The resultant hash value                                                 !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2018-03-29                                                 !
  !------------------------------------------------------------------------------!
    integer :: hash
    character(len=*), intent(in)  :: key
    integer,          intent(in)  :: n

    integer, parameter :: A = 13513
    integer, parameter :: B = 2173883
    integer :: x, i

    x = 0
    do i=1, len_trim(key)
      x = modulo(x*A + iachar(key(i:i)),  B)
    end do

    hash = mod(x, n)+1

  end function compute_hash

  subroutine hash_create(hash, nbuckets)
    class(hash_table) :: hash
    integer :: nbuckets

    if ( hash%initialised ) stop "Hash is already initialised"

    allocate(hash%buckets(nbuckets))
    hash%nbuckets = nbuckets
    hash%iterator_pos = [1,0]

    hash%initialised = .true.

  end subroutine hash_create

  logical function hash_contains(hash, key)
    class(hash_table) :: hash
    character(len=*) :: key

    type(hash_entry), pointer :: node
    integer :: c, inode

    c = compute_hash(key, hash%nbuckets)

    node => hash%buckets(c)%node

    hash_contains = .false.
    do inode = 1, hash%buckets(c)%items
      if(node%key == key) then
        hash_contains = .true.
        exit
      end if
      node => node%next
    end do

  end function hash_contains

  subroutine hash_put(hash, key, val)
    class(hash_table) :: hash
    character(len=*) :: key
    integer :: val

    integer :: c, inode
    type(hash_entry), pointer :: node
    logical :: found

    c = compute_hash(key, hash%nbuckets)
    found = .false.

    if (hash%buckets(c)%items == 0) then
      allocate(hash%buckets(c)%node)
      node => hash%buckets(c)%node
      node%next => null()
      node%key = key
      hash%buckets(c)%items = 1
    else
      node => hash%buckets(c)%node
      do inode = 1, hash%buckets(c)%items
        if (node%key == key) then
          found = .true.
          exit
        end if
        if(associated(node%next)) node => node%next
      end do

      if(.not. found) then
        allocate(node%next)
        node => node%next
        node%next => null()
        node%key = key
        hash%buckets(c)%items = hash%buckets(c)%items + 1
      end if
    end if
    node%val = val
    hash%iterator_pos = [1,0]

  end subroutine hash_put

  function hash_get(hash, key) result(val)
    class(hash_table) :: hash
    character(len=*) :: key
    integer :: val

    integer :: c, inode
    type(hash_entry), pointer :: node

    val = 0
    c = compute_hash(key, hash%nbuckets)

    node => hash%buckets(c)%node
    if(associated(node)) then
      do inode = 1, hash%buckets(c)%items
        if(node%key == key) then
          val = node%val
          exit
        end if
        node => node%next
      end do
    end if

  end function hash_get

  subroutine hash_remove(hash, key)
    class(hash_table) :: hash
    character(len=*) :: key

    type(hash_entry), pointer :: node, prev_node
    integer :: c, inode

    c = compute_hash(key, hash%nbuckets)

    node => hash%buckets(c)%node

    do inode = 1, hash%buckets(c)%items
      if(node%key == key) then
        if (inode == 1) then
          hash%buckets(c)%node => node%next
        else
          prev_node%next => node%next
        end if
        deallocate(node)
        hash%buckets(c)%items=hash%buckets(c)%items-1
        exit
      end if
      prev_node => node
      node => node%next
    end do

  end subroutine hash_remove

  subroutine hash_destroy(hash)
    class(hash_table) :: hash
    integer :: ibucket, inode
    type(hash_entry), pointer :: node, next_node

    do ibucket=1, hash%nbuckets
      node => hash%buckets(ibucket)%node
      do inode= 1, hash%buckets(ibucket)%items
        next_node => node%next
        deallocate(node)
        node => next_node
      end do
    end do

    deallocate(hash%buckets)
    hash%initialised = .false.

  end subroutine hash_destroy

  function hash_next(hash) result(next)
    type(hash_entry)            :: next
    class(hash_table)            :: hash
    type(hash_entry), pointer   :: node
    integer :: inode

    next%next => null()
    next%key  = ""
    next%val  = 0

    hash%iterator_pos(2)=hash%iterator_pos(2)+1
    if ( hash%iterator_pos(2) > hash%buckets(hash%iterator_pos(1))%items ) then
      hash%iterator_pos(1)=hash%iterator_pos(1)+1
      if (hash%iterator_pos(1) > hash%nbuckets) then
        hash%iterator_pos = [1,0]
        return
      end if

      do while ( (hash%buckets(hash%iterator_pos(1))%items == 0) .and. (hash%iterator_pos(1) < hash%nbuckets) )
        hash%iterator_pos(1)=hash%iterator_pos(1)+1
      end do

      if (hash%buckets(hash%iterator_pos(1))%items > 0) then
        hash%iterator_pos(2) = 1
        next%key = hash%buckets(hash%iterator_pos(1))%node%key
        next%val = hash%buckets(hash%iterator_pos(1))%node%val
        return
      else
        hash%iterator_pos = [1,0]
        return
      end if
    else
      node => hash%buckets(hash%iterator_pos(1))%node
      do inode=1, hash%iterator_pos(2)-1
        node => node%next
      end do
      next%key = node%key
      next%val = node%val
      return
    end if

  end function hash_next

  function hash_first(hash) result(first)
    type(hash_entry)  :: first
    class(hash_table) :: hash

    hash%iterator_pos = [1,0]
    first = hash%next()

  end function hash_first

end module hash_tables
