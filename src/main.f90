program main
    use day1_mod, only: d1a => part1, d1b => part2
    use day2_mod, only: d2a => part1, d2b => part2
    use day3_mod, only: d3a => part1, d3b => part2
    use day4_mod, only: d4a => part1, d4b => part2
    use day5_mod, only: d5a => part1, d5b => part2
    use day6_mod, only: d6a => part1, d6b => part2
    use day7_mod, only: d7a => part1, d7b => part2
    use day8_mod, only: d8a => part1, d8b => part2
    use day9_mod, only: d9a => part1, d9b => part2
    use day10_mod, only: d10a => part1, d10b => part2
    use day11_mod, only: d11a => part1, d11b => part2
    use day12_mod, only: d12a => part1, d12b => part2
    use day13_mod, only: d13a => part1, d13b => part2
    use day14_mod, only: d14a => part1, d14b => part2
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
    case ('6')
        select case (arg2)
        case ('1')
            call d6a()
        case ('2')
            call d6b()
        case default
            print *, 'Not Implemented'
        end select
    case ('7')
        select case (arg2)
        case ('1')
            call d7a()
        case ('2')
            call d7b()
        case default
            print *, 'Not Implemented'
        end select
    case ('8')
        select case (arg2)
        case ('1')
            call d8a()
        case ('2')
            call d8b()
        case default
            print *, 'Not Implemented'
        end select
    case ('9')
        select case (arg2)
        case ('1')
            call d9a()
        case ('2')
            call d9b()
        case default
            print *, 'Not Implemented'
        end select
    case ('10')
        select case (arg2)
        case ('1')
            call d10a()
        case ('2')
            call d10b()
        case default
            print *, 'Not Implemented'
        end select
    case ('11')
        select case (arg2)
        case ('1')
            call d11a()
        case ('2')
            call d11b()
        case default
            print *, 'Not Implemented'
        end select
    case ('12')
        select case (arg2)
        case ('1')
            call d12a()
        case ('2')
            call d12b()
        case default
            print *, 'Not Implemented'
        end select
    case ('13')
        select case (arg2)
        case ('1')
            call d13a()
        case ('2')
            call d13b()
        case default
            print *, 'Not Implemented'
        end select
    case ('14')
        select case (arg2)
        case ('1')
            call d14a()
        case ('2')
            call d14b()
        case default
            print *, 'Not Implemented'
        end select
    case default
        print *, 'Not Implemented'
    end select
end program main
