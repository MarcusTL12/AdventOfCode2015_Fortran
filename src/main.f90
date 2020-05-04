program main
    use day1_mod, only: d1a => part1, d1b => part2
    use day2_mod, only: d2a => part1, d2b => part2
    use day3_mod, only: d3a => part1, d3b => part2
    use day4_mod, only: d4a => part1, d4b => part2
    use day5_mod, only: d5a => part1, d5b => part2
    implicit none
    !
    character(len=10) :: arg1, arg2
    !
    call getarg(1, arg1)
    call getarg(2, arg2)
    !
    select case (arg1)
    case ('1')
        select case (arg2)
        case ('1')
            call d1a()
        case ('2')
            call d1b()
        case default
            print *, 'Part not Implemented'
        end select
    case ('2')
        select case (arg2)
        case ('1')
            call d2a()
        case ('2')
            call d2b()
        case default
            print *, 'Not Implemented'
        end select
    case ('3')
        select case (arg2)
        case ('1')
            call d3a()
        case ('2')
            call d3b()
        case default
            print *, 'Not Implemented'
        end select
    case ('4')
        select case (arg2)
        case ('1')
            call d4a()
        case ('2')
            call d4b()
        case default
            print *, 'Not Implemented'
        end select
    case ('5')
        select case (arg2)
        case ('1')
            call d5a()
        case ('2')
            call d5b()
        case default
            print *, 'Not Implemented'
        end select
    case default
        print *, 'Not Implemented'
    end select
end program main
