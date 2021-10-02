cmake_minimum_required(VERSION 3.20)
project(Person Fortran)


enable_language(Fortran)
add_executable(Person mod_person.f90 main.f90)