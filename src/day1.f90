module day1_mod
    implicit none
    !
    public :: part1, part2
contains
    subroutine part1()
        implicit none
        !
        integer     :: ios
        character   :: buf
        integer     :: ans
        !
        open (1, file="inputfiles/day1/input.txt")
        !
        do
            read (1, '(A)', advance='no', iostat=ios) buf
            if (ios /= 0) exit
            select case (buf)
            case ('(')
                ans = ans + 1
            case (')')
                ans = ans - 1
            end select
        end do
        !
        close (1)
        !
        print *, ans
    end subroutine
    !
    subroutine part2()
        implicit none
        !
        integer     :: ios
        character   :: buf
        integer     :: ans, i
        !
        open (1, file="inputfiles/day1/input.txt")
        !
        i = 0
        do
            read (1, '(A)', advance='no', iostat=ios) buf
            if (ios /= 0) exit
            select case (buf)
            case ('(')
                ans = ans + 1
            case (')')
                ans = ans - 1
            end select
            !
            i = i + 1
            if (ans == -1) then
                print *, i
                exit
            end if
        end do
    end subroutine
end module
