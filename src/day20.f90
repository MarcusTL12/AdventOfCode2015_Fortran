module day20_mod
    use math_util_mod
    use vec_int_mod
    implicit none
    !
    private
    public :: part1, part2
    !
    integer, parameter :: input = 29000000
    ! integer, parameter :: input = 1000
contains
    ! logical function highly_divisible(n)
    !     implicit none
    !     !
    !     integer, intent(in) :: n
    !     integer :: i, m, p
    !     !
    !     m = n
    !     do i = 1, amt_primes
    !         p = prime(i)
    !         if (modulo(m, p) /= 0) then
    !             highly_divisible = .false.
    !             return
    !         else
    !             do while (modulo(m, p) == 0)
    !                 m = m / p
    !             end do
    !         end if
    !         !
    !         if (p >= m) then
    !             highly_divisible = .true.
    !             return
    !         end if
    !     end do
    !     !
    !     highly_divisible = .true.
    ! end function
    !
    subroutine part1()
        implicit none
        !
        integer :: i
        type(vec_int) :: d
        !
        call d%new()
        i = 0
        do
            i = i + 1
            !
            call get_divisors(i, d)
            if (sum(d%as_slice()) * 10 >= input) then
                exit
            end if
        end do
        !
        print *, i
    end subroutine
    !
    subroutine part2()
        implicit none
        !
        integer :: i, j, s
        type(vec_int) :: d
        !
        call d%new()
        i = 0
        do
            i = i + 1
            !
            call get_divisors(i, d)
            s = 0
            do j = 1, size(d)
                if (d%at(j) >= i / 50) s = s + d%at(j)
            end do
            !
            if (s * 11 >= input) then
                exit
            end if
        end do
        !
        print *, i
    end subroutine
end module
