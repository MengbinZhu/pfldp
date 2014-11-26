! $Id: nag_interfaces.f90 65 2012-01-23 16:28:16Z frdo $

!****f* BUFR/nag_interfaces *
!
! NAME
!    nag_interfaces - Interfaces to the NAG f90/f95 f90_unix_* routines.
!
! SYNOPSIS
!    When Fortran 90 routines call standard Unix system calls but are
!    compiled with the NAGWare Fortran 90/95 compiler, link the resulting
!    object code with a compiled version of this file.
!
! DESCRIPTION
!    The NAG f90 / f95 compiler provides access to most standard Unix
!    system routines via a set of modules named f90_unix_*. While this
!    is a clear and well defined interface to system routines, it leaves
!    source code developed for the NAG compilers incompatible with other
!    Fortran compilers (which usually provide access to the same routines
!    as part of their standard library), as the corresponding NAG modules
!    are missing in other compiler distributions. This causes a lot of
!    unnecessary recoding work.
!
!    As an alternative, this file collects (part of) the system routines
!    addressed by the various f90_unix_* modules and provides an interface
!    to them. Instead of adding the appropriate use f90_unix_* entries
!    in each affected source file, this file can be compiled with the NAG
!    compiler and linked to the other routines.
!
! NOTES
!    Not all routines are currently implemented; i will add them if they
!    will be requested (or I have a need for them).
!
!    Some of the NAG routines do have optional arguments, like ALARM.
!    Obviously, this is not implemented. Also, all parameter and type
!    definitions available from the NAG modules have been omitted.
!
! EXAMPLE
!
!
! SEE ALSO
!    For the NAG compiler:
!       f90_unix
!       f90_unix_dir
!       f90_unix_dirent
!       f90_unix_env
!       f90_unix_file
!       f90_unix_proc
!
! REFERENCES
!
!
! TODO
!    Complete wrappers for all routines.
!
! BUGS
!
!
! AUTHOR
!    C. Marquardt, Met Office, Exeter    <christian.marquardt@metoffice.com>
!
!****

!-------------------------------------------------------------------------------
! 1. f90_unix_dir
!-------------------------------------------------------------------------------

! 1.1 CHDIR
! 1.2 GETCWD
! 1.3 LINK
! 1.4 MKDIR
! 1.5 MKFIFO
! 1.6 RENAME
! 1.7 RMDIR
! 1.8 UMASK
! 1.9 UNLINK

!-------------------------------------------------------------------------------
! 2. f90_unix_dirent
!-------------------------------------------------------------------------------

! 2.1 CLOSEDIR
! 2.2 OPENDIR
! 2.3 READDIR
! 2.4 REWINDDIR

!-------------------------------------------------------------------------------
! 3. f90_unix_env
!-------------------------------------------------------------------------------

! 3.1 CLK_TCK
! 3.2 CTERMID

! 3.3 GETARG
! ----------

subroutine getarg(k, arg)
  use f90_unix_env, only: nag_getarg => getarg
  character(len = *) :: arg
  integer            :: k, lenarg, errno
  call nag_getarg(k, arg, lenarg, errno)
end subroutine getarg

! 3.4 GETEGID

! 3.5 GETENV
! ----------

subroutine getenv(name, value)
  use f90_unix_env, only: nag_getenv => getenv
  character(len = *) :: name, value
  integer            :: lenvalue, errno
  call nag_getenv(name, value, lenvalue, errno)
end subroutine getenv

! 3.6 GETEUID
! -----------

function geteuid()
  use f90_unix_env, only: nag_geteuid => geteuid
  geteuid = nag_geteuid()
end function geteuid

! 3.7 GETGID
! ----------

function getgid()
  use f90_unix_env, only: nag_getgid => getgid
  getgid = nag_getgid()
end function getgid


! 3.8 GETGROUPS
! 3.9 GETLOGIN

! 3.10 GETPGRP
! ------------

function getpgrp()
  use f90_unix_env, only: nag_getpgrp => getpgrp
  getpgrp = nag_getpgrp()
end function getpgrp


! 3.11 GETPID
! -----------

function getpid()
  use f90_unix_env, only: nag_getpid => getpid
  getpid = nag_getpid()
end function getpid


! 3.12 GETPPID
! ------------

function getppid()
  use f90_unix_env, only: nag_getppid => getppid
  getppid = nag_getppid()
end function getppid


! 3.13 GETUID
! -----------

function getuid()
  use f90_unix_env, only: nag_getuid => getuid
  getuid = nag_getuid()
end function getuid


! 3.14 IARGC
! ----------

function iargc()
  use f90_unix_env, only: nag_iargc => iargc
  iargc = nag_iargc()
end function iargc

! 3.15 ISATTY
! 3.16 SETGID
! 3.17 SETPGID
! 3.18 SETSID
! 3.19 SETUID
! 3.20 SYSCONF
! 3.21 TIME
! 3.22 TIMES
! 3.23 TTYNAME
! 3.24 UNAME

!-------------------------------------------------------------------------------
! 4. f90_unix_io
!-------------------------------------------------------------------------------

! 4.1 FLUSH
! ---------

subroutine flush(lunit, errno)

  use f90_unix_io, only: nag_flush => flush
  integer            :: lunit, errno

  call nag_flush(lunit, errno)

end subroutine flush

!-------------------------------------------------------------------------------
! 5. f90_unix_file
!-------------------------------------------------------------------------------

! 5.1 ACCESS
! 5.2 CHMOD
! 5.3 CHOWN
! 5.4 FSTAT
! 5.5 STAT
! 5.6 S_ISBLK
! 5.7 S_ISCHR
! 5.8 S_ISDIR
! 5.9 S_ISFIFO
! 5.10 S_ISREG
! 5.11 UTIME

!-------------------------------------------------------------------------------
! 6. f90_unix_proc
!-------------------------------------------------------------------------------

! 6.1 ABORT
! ---------

subroutine abort(message)
  use f90_unix_proc, only: nag_abort => abort
  character(len = *) :: message
  call nag_abort(message)
end subroutine abort

! 6.2 ALARM
! 6.3 ATEXIT
! 6.4 EXECV
! 6.5 EXECVE
! 6.6 EXECVP

! 6.7 EXIT
! --------

subroutine exit(status)
  use f90_unix_proc, only: nag_exit => exit
  integer :: status
  call nag_exit(status)
end subroutine exit

! 6.8 FASTEXIT
! ------------

subroutine fastexit(status)
  use f90_unix_proc, only: nag_fastexit => fastexit
  integer :: status
  call nag_fastexit(status)
end subroutine fastexit

! 6.9 FORK
! --------

subroutine fork(pid, errno)
  use f90_unix_proc, only: nag_fork => fork
  integer :: pid, errno
  call nag_fork(pid, errno)
end subroutine fork

! 6.10 PAUSE
! ----------

subroutine pause(errno)
  use f90_unix_proc, only: nag_pause => pause
  integer :: errno
  call nag_pause(errno)
end subroutine pause

! 6.11 SLEEP
! ----------

subroutine sleep(seconds, secleft)
  use f90_unix_proc, only: nag_sleep => sleep
  integer :: seconds, secleft
  call nag_sleep(seconds, secleft)
end subroutine sleep

! 6.12 SYSTEM
! -----------

subroutine system(string)
  use f90_unix_proc, only: nag_system => system
  character(len = *) :: string
  integer            :: status, errno
  call nag_system(string, status, errno)
end subroutine system

! 6.13 WAIT
! 6.14 WAITPID
! 6.15 WEXITSTATUS
! 6.16 WIFEXITED
! 6.17 WIFSIGNALED
! 6.18 WIFSTOPPED
! 6.19 WSTOPSIG
! 6.20 WTERMSIG
! 6.21 EXECL
! 6.22 EXECLP

! NAG doesn't have 'dfloat'

double precision function dfloat(float)
  real float
  dfloat = dble(float)
end function dfloat
