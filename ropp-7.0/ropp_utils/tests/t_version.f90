! $Id: t_version.f90 3086 2011-10-14 08:54:50Z idculv $

program t_version

! (Very) basic test that we can link with the ROPP UTILS object library

  use ropp_utils, only : ropp_utils_version
  character (len=40) :: version
  version = ropp_utils_version()
  print *, ''
  print *, "This is ROPP (UTILS) Release " // TRIM(version)
  print *, ''

end program t_version
