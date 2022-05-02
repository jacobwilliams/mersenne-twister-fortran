!*****************************************************************************************
!>
!  Test program.

program mt19937_64_test

    use mt19937_64
    use iso_fortran_env, only: output_unit, wp => real64

    implicit none

    type(mt19937) :: random
    real(wp) :: r
    integer :: i

    call random%initialize(42)

    do i = 1, 10
        r = random%genrand64_real1()
        write(output_unit, '(E30.16)') r
    end do

end program mt19937_64_test
!*****************************************************************************************