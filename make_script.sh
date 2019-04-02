#!/bin/bash

#==============================================================================
# Clear the Terminal Screen
#==============================================================================

clear

#==============================================================================
# Compiler, Linker, and flags Definitions
#==============================================================================

# Name of the compiler
COMPILERTYPE=ifort

# Test: compiler is ifort
if [[ $COMPILERTYPE = "ifort" ]]
then

    # Compiler definition
    COMPILER=ifort
    
    # Linker definition
    LINKER=ifort
    
    # Compiler's flags
    CMPFLAGS="-c -fast"

    # Linker's flags
    LNKFLAGS="-fast"

# Test: compiler is gfortran
elif [[ $COMPILERTYPE = "gfortran" ]]
then

    # Compiler definition
    COMPILER=gfortran
    
    # Linker definition
    LINKER=gfortran
    
    # Compiler's flags
    CMPFLAGS="-c -ffree-line-length-none -Ofast"
    
    # Linker's flags
    LNKFLAGS="-Ofast"

# Test: compiler not supported
else
    
    # Error message
    echo Compiler $COMPILERTYPE not supported!
    
    # Exit script
    exit 1

fi

#==============================================================================
# Source List
#==============================================================================

# Order is very important
# The lists must be ordered from the less dependent to the more dependent sources and objects
# Each file in the list must depend only on files that are before it

SOURCELIST="mod_nrtype.F90"
SOURCELIST="$SOURCELIST mod_nrutil.F90"
SOURCELIST="$SOURCELIST mod_numerical_recipes.F90"
SOURCELIST="$SOURCELIST mod_array_vec.F90"
SOURCELIST="$SOURCELIST mod_globals.F90"
SOURCELIST="$SOURCELIST mod_functions.F90"
SOURCELIST="$SOURCELIST mod_output.F90"
SOURCELIST="$SOURCELIST sub_allocation.F90"
SOURCELIST="$SOURCELIST sub_calibration.F90"
SOURCELIST="$SOURCELIST sub_agents_problem.F90"
SOURCELIST="$SOURCELIST sub_agents_distribution.F90"
SOURCELIST="$SOURCELIST sub_equilibrium_variables.F90"
SOURCELIST="$SOURCELIST sub_equilibrium_computation.F90"
SOURCELIST="$SOURCELIST sub_aggregate_variables.F90"
SOURCELIST="$SOURCELIST main.F90"

#==============================================================================
# Object List
#==============================================================================

OBJLIST="mod_nrtype.F90"
OBJLIST="$OBJLIST mod_nrutil.F90"
OBJLIST="$OBJLIST mod_numerical_recipes.F90"
OBJLIST="$OBJLIST mod_array_vec.F90"
OBJLIST="$OBJLIST mod_globals.F90"
OBJLIST="$OBJLIST mod_functions.F90"
OBJLIST="$OBJLIST mod_output.F90"
OBJLIST="$OBJLIST sub_allocation.F90"
OBJLIST="$OBJLIST sub_calibration.F90"
OBJLIST="$OBJLIST sub_agents_problem.F90"
OBJLIST="$OBJLIST sub_agents_distribution.F90"
OBJLIST="$OBJLIST sub_equilibrium_variables.F90"
OBJLIST="$OBJLIST sub_equilibrium_computation.F90"
OBJLIST="$OBJLIST sub_aggregate_variables.F90"
OBJLIST="$OBJLIST main.F90"

#==============================================================================
# Generate Executable File (by the Linker)
#==============================================================================

EXECUTABLE=a.out

#==============================================================================
# xxx
#==============================================================================

# Sources compilation to generate .o's and .mod's files
for source in $SOURCELIST
do
    
    echo Compiling $source
    
    if [[ $($COMPILER $CMPFLAGS $source) -ne 0 ]]
    then
        echo Error found when compiling $source
        exit 2
    fi
    
done

#==============================================================================
# xxx
#==============================================================================

# Objects linking
echo Linking $EXECUTABLE

if [[ $($LINKER $LNKFLAGS $OBJLIST -o $EXECUTABLE) -ne 0 ]]
then
    echo Error found when linking $EXECUTABLE
    exit 3
fi

echo $EXECUTABLE generated with success

#==============================================================================
# Delete Generated Files: ".mod"
#==============================================================================

rm -f mod_nrtype.mod
rm -f mod_nrutil.mod
rm -f mod_numerical_recipes.mod
rm -f mod_array_vec.mod
rm -f mod_globals.mod
rm -f mod_functions.mod
rm -f mod_output.mod

#==============================================================================
# Delete Generated Files: ".o"
#==============================================================================

rm -f mod_nrtype.o
rm -f mod_nrutil.o
rm -f mod_numerical_recipes.o
rm -f mod_array_vec.o
rm -f mod_globals.o
rm -f mod_functions.o
rm -f mod_output.o
rm -f sub_allocation.o
rm -f sub_calibration.o
rm -f sub_agents_problem.o
rm -f sub_agents_distribution.o
rm -f sub_equilibrium_variables.o
rm -f sub_equilibrium_computation.o
rm -f sub_aggregate_variables.o
rm -f sub_output.o
rm -f main.o

#==============================================================================
# Run Executable File
#==============================================================================

./a.out

#==============================================================================
# Delete Executable File
#==============================================================================

rm -f a.out

#==============================================================================
# End of Script
#==============================================================================

exit 0