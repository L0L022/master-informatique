cmake_minimum_required(VERSION 3.0)
project(aro-tp3 CXX)

set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_executable(aro-tp3 main.cpp)
set_property(TARGET aro-tp3 PROPERTY CXX_STANDARD 14)
target_link_libraries(aro-tp3 -lglpk)
