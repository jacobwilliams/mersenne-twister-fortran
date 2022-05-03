!*****************************************************************************************
!>
!  64-bit version of the Mersenne Twister pseudorandom number generator.
!
!### History
!
!  * Contributors: Rémi Piatek, Takuji Nishimura, Makoto Matsumoto, Jacob Williams
!    See LICENSE file for details.
!
!### References
!
!  * T. Nishimura, "Tables of 64-bit Mersenne Twisters" ACM Transactions on Modeling and
!    Computer Simulation 10. (2000) 348--357.
!  * M. Matsumoto and T. Nishimura,
!    "Mersenne Twister: a 623-dimensionally equidistributed uniform pseudorandom number generator"
!    ACM Transactions on Modeling and Computer Simulation 8. (Jan. 1998) 3--30.
!  * Original source: http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/VERSIONS/FORTRAN/mt19937-64.f95

module mt19937_64

  use, intrinsic :: iso_fortran_env

  implicit none

  private

  integer, parameter :: r8 = real64
  integer, parameter :: i4 = int32
  integer, parameter :: i8 = int64

  integer(i8), parameter :: nn       = 312_i8
  integer(i8), parameter :: mm       = 156_i8
  integer(i8), parameter :: seed_def = 5489_i8
  integer(i8), parameter :: matrix_a = -5403634167711393303_i8
  integer(i8), parameter :: um       = -2147483648_i8 !! most significant 33 bits
  integer(i8), parameter :: lm       =  2147483647_i8 !! least significant 31 bits

  real(r8), parameter :: pi253_1  = 1.0_r8/(2.0_r8**53 - 1.0_r8)
  real(r8), parameter :: pi253    = 1.0_r8/(2.0_r8**53)
  real(r8), parameter :: pi252    = 1.0_r8/(2.0_r8**52)

  type,public :: mt19937

    !! main class for random number generator

    private

    integer(i8) :: mt(nn) = 0_i8       !! array for the state vector
    integer     :: mti = nn+1          !! `mti==nn+1` means `mt(nn)` is not initialized

    contains

    private

    generic,public :: initialize => init_genrand64_i4, &
                                    init_genrand64, &
                                    init_by_array64 !! call first to initialize
    procedure :: init_genrand64
    procedure :: init_by_array64
    procedure :: init_genrand64_i4

    procedure, public :: genrand64_real1
    procedure, public :: genrand64_real2
    procedure, public :: genrand64_real3
    procedure, public :: genrand64_int64

  end type mt19937

  contains
!*****************************************************************************************

!*****************************************************************************************
  subroutine init_genrand64_i4(me,seed)
    !! Initializes `me%mt(nn)` with a seed
    implicit none

    class(mt19937),intent(inout) :: me
    integer(i4), intent(in) :: seed

    call me%initialize(int(seed, i8))

  end subroutine init_genrand64_i4
!*****************************************************************************************

!*****************************************************************************************
  subroutine init_genrand64(me,seed)
    !! Initializes `me%mt(nn)` with a seed
    implicit none

    class(mt19937),intent(inout) :: me
    integer(i8), intent(in) :: seed

    integer :: i

    me%mt(1) = seed
    do i = 1, nn-1
      me%mt(i+1) = 6364136223846793005_i8 * ieor(me%mt(i), ishft(me%mt(i), -62)) + i
    end do

    me%mti = nn

  end subroutine init_genrand64
!*****************************************************************************************

!*****************************************************************************************
  subroutine init_by_array64(me,init_key)
    !! Initializes by an array with array-length
    !! `init_key` is the array for initializing keys
    implicit none

    class(mt19937),intent(inout) :: me
    integer(i8), intent(in) :: init_key(:)

    integer(i8), parameter  :: c1 = 3935559000370003845_i8
    integer(i8), parameter  :: c2 = 2862933555777941757_i8
    integer(i8) :: i, j, k, kk, key_length

    call me%init_genrand64(19650218_i8)
    key_length = size(init_key)
    i = 1_i8; j = 0_i8
    k = max(nn, key_length)

    do kk = 1, k
      me%mt(i+1) = ieor(me%mt(i+1), c1 * ieor(me%mt(i), ishft(me%mt(i), -62))) &
                  + init_key(j+1) + j
      i = i+1; j = j+1
      if(i >= nn) then
        me%mt(1) = me%mt(nn)
        i = 1
      end if
      if(j >= key_length) j = 0
    end do

    do kk = 1, nn-1
      me%mt(i+1) = ieor(me%mt(i+1), c2 * ieor(me%mt(i), ishft(me%mt(i), -62))) - i
      i = i+1
      if(i >= nn) then
        me%mt(1) = me%mt(nn)
        i = 1
      end if
    end do

    me%mt(1) = ishft(1_i8, 63)  ! MSB is 1; assuring non-zero initial array

  end subroutine init_by_array64
!*****************************************************************************************

!*****************************************************************************************
  integer(r8) function genrand64_int64(me)
    !! Generates a random number on [-2^63, 2^63-1]-interval

    implicit none

    class(mt19937),intent(inout) :: me

    integer(i8),parameter :: mag01(0:1) = [0_i8, matrix_a]

    integer(i8) :: x
    integer     :: i

    if(me%mti >= nn) then ! generate nn words at one time

      ! if init_genrand64() has not been called, a default initial seed is used
      if(me%mti == nn+1) call me%init_genrand64(seed_def)

      do i = 1, nn-mm
        x = ior(iand(me%mt(i),um), iand(me%mt(i+1), lm))
        me%mt(i) = ieor(ieor(me%mt(i+mm), ishft(x, -1)), mag01(iand(x, 1_i8)))
      end do

      do i = nn-mm+1, nn-1
        x = ior(iand(me%mt(i), um), iand(me%mt(i+1), lm))
        me%mt(i) = ieor(ieor(me%mt(i+mm-nn), ishft(x, -1)), mag01(iand(x, 1_i8)))
      end do

      x = ior(iand(me%mt(nn), um), iand(me%mt(1), lm))
      me%mt(nn) = ieor(ieor(me%mt(mm), ishft(x, -1)), mag01(iand(x, 1_i8)))

      me%mti = 0

    end if

    me%mti = me%mti + 1
    x = me%mt(me%mti)

    x = ieor(x, iand(ishft(x,-29), 6148914691236517205_i8))
    x = ieor(x, iand(ishft(x, 17), 8202884508482404352_i8))
    x = ieor(x, iand(ishft(x, 37),   -2270628950310912_i8))
    x = ieor(x, ishft(x, -43))

    genrand64_int64 = x

  end function genrand64_int64
!*****************************************************************************************

!*****************************************************************************************
  real(r8) function genrand64_real1(me)
    !! Generates a random number on [0,1]-real-interval
    implicit none
    class(mt19937),intent(inout) :: me

    genrand64_real1 = real(ishft(me%genrand64_int64(), -11), kind=r8) * pi253_1

  end function genrand64_real1
!*****************************************************************************************

!*****************************************************************************************
  real(r8) function genrand64_real2(me)
    !! Generates a random number on [0,1)-real-interval
    implicit none
    class(mt19937),intent(inout) :: me

    genrand64_real2 = real(ishft(me%genrand64_int64(), -11), kind=r8) * pi253

  end function genrand64_real2
!*****************************************************************************************

!*****************************************************************************************
  real(r8) function genrand64_real3(me)
    !! Generates a random number on (0,1)-real-interval
    implicit none
    class(mt19937),intent(inout) :: me

    genrand64_real3 = real(ishft(me%genrand64_int64(), -12), kind=r8)
    genrand64_real3 = (genrand64_real3 + 0.5_r8) * pi252

  end function genrand64_real3
!*****************************************************************************************

!*****************************************************************************************
  end module mt19937_64
!*****************************************************************************************