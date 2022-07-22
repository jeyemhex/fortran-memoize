program hash_test
!==============================================================================#
! HASH_TEST
!------------------------------------------------------------------------------#
! Author:  Edward Higgins <ed.higgins@york.ac.uk>
!------------------------------------------------------------------------------#
! Version: 0.1.1, 2018-03-29
!------------------------------------------------------------------------------#
! This code is distributed under the MIT license.
!==============================================================================#
  use hash_tables

  implicit none

  character(len=64)  :: word
  integer            :: ios, dict = 11

  type(hash_table)   :: hash
  type(hash_entry)   :: record

  open(unit=dict, file="/usr/share/dict/cracklib-small")
  call hash%create(nbuckets = 100)
  do
    read(dict,*, iostat=ios) word
    if (is_iostat_end(ios)) exit
    call hash%put( key=word(1:3), val=hash%get(key=word(1:3))+1 )
  end do

  record = hash%first()
  do while(record%key /= "")
    print *, "'" // trim(record%key) // "': ", record%val
    record = hash%next()
  end do

  call hash%destroy()
  close(dict)

end program hash_test
