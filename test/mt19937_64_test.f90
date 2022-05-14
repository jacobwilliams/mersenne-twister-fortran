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
    integer, parameter :: n = 10**7
    real(wp),dimension(n) :: x

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

    ! randomness tests:

    do i = 1, n
        x(i) = random%genrand64_real1()
    end do
    call print_results('genrand64_real1')

    do i = 1, n
        x(i) = random%genrand64_real2()
    end do
    call print_results('genrand64_real2')

    do i = 1, n
        x(i) = random%genrand64_real3()
    end do
    call print_results('genrand64_real3')

    contains

        subroutine print_results(method)
            character(len=*),intent(in) :: method
            write(*,'(/A)') method//': test randomness '
            write(*,*) 'theory:', 0.5_wp,   1/12.0_wp
            write(*,*) 'actual:', sum(x)/n, sum((x-0.5_wp)**2)/n
        end subroutine print_results

end program mt19937_64_test
!*****************************************************************************************