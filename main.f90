program main
    use mod_person
    use iso_fortran_env, only: int8

    implicit none

    type(Person) :: people(3)
    integer(int8) :: i
    people = [ &
            Person(name="John", age=24, occupation="Programmer"), &
            Person(name="Davy", age=154, occupation="Pirate"), &
            Person(name="Mike", age=57, occupation="Engineer") &
    ]
    call people % greet()
    call people(1) % setAge(age=22)
    call people(1) % setName(name='Luke')
    do i = 1, 3
        print *, people(i) % getName(), ' is ', people(i) % getAge(), &
                ' years old and works as ' // people(i) % getOccupation() // '.'
    end do
    call people(2) % setAge(-1)
    do i = 1, 3
        print *, people(i) % getName(), ' is ', people(i) % getAge(), &
                ' years old and works as ' // people(i) % getOccupation() // '.'
    end do

end program main
