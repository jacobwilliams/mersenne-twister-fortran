!*****************************************************************************************
!>
!  Test program.

program mt19937_64_test

    use mt19937_64
    use iso_fortran_env, only: output_unit, wp => real64, i4 => int32, i8 => int64

    implicit none

    type(mt19937) :: random
    real(wp) :: r
    integer :: i

    call random%initialize(42)

    do i = 1, 10
        r = random%genrand64_real1()
        write(output_unit, '(E30.16)') r
    end do

    call random%initialize(1776_i8)

    do i = 1, 10
        r = random%genrand64_real1()
        write(output_unit, '(E30.16)') r
    end do

    call random%initialize([1_i8,22_i8,333_i8])

    r = random%genrand64_real1()
    write(output_unit, '(E30.16)') r

    r = random%genrand64_real2()
    write(output_unit, '(E30.16)') r

    r = random%genrand64_real3()
    write(output_unit, '(E30.16)') r

end program mt19937_64_test
!*****************************************************************************************