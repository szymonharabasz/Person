! Created by Szymon Harabasz on 01.10.21.

module mod_person
    use iso_fortran_env, only: int32
    implicit none
    type :: Person
        private
        character(len=256) :: name
        integer(kind=int32) :: age
        character(len=256) :: occupation
        character(len=256) :: greeting
    contains
        procedure, pass(self) :: greet
        procedure, pass(self) :: getAge
        procedure, pass(self) :: setAge
        procedure, pass(self) :: getName
        procedure, pass(self) :: setName
        procedure, pass(self) :: getOccupation
        procedure, pass(self) :: setOccupation
    end type Person

    interface Person
        module procedure :: person_constructor
    end interface Person

contains
    impure elemental subroutine greet(self)
        class(Person), intent(in) :: self
        print *, trim(self % greeting) // ' My name is ' // trim(self % name) // '!'
    end subroutine greet

    pure integer function getAge(self) result(age)
        class(Person), intent(in) :: self
        age = self % age
    end function getAge

    subroutine setAge(self, age)
        class(Person), intent(inout) :: self
        integer, intent(in) :: age
        if (age <= 0) then
            stop 'Error: age must be > 0'
        end if
        self % age = age
    end subroutine setAge

    pure function getName(self) result(name)
        class(Person), intent(in) :: self
        character(:), allocatable :: name
        allocate(name, source = self % name)
    end function getName

    subroutine setName(self, name)
        class(Person), intent(inout) :: self
        character(*), intent(in) :: name
        self % name = name
    end subroutine setName

    pure function getOccupation(self) result(occupation)
        class(Person), intent(in) :: self
        character(:), allocatable :: occupation
        allocate(occupation, source = self % occupation)
    end function getOccupation

    subroutine setOccupation(self, occupation)
        class(Person), intent(inout) :: self
        character(*), intent(in) :: occupation
        self % occupation = occupation
    end subroutine setOccupation

    pure type(Person) function person_constructor(name, age, occupation) result(res)
        character(len=*), intent(in) :: name
        integer, intent(in) :: age
        character(len=*), intent(in) :: occupation
        res % name = name
        res % age = age
        res % occupation = occupation

        if (occupation == 'Pirate') then
            res % greeting = 'Ahoy, matey!'
        else
            res % greeting = 'Hi there!'
        end if

    end function person_constructor

end module mod_person