# Health Care Reform or More Affordable Health Care?
This repository contains the files that can be used to replicate the results of [Ferreira and Gomes (2017)](https://www.sciencedirect.com/science/article/pii/S0165188917300635). These are the files used to implement and simulate the heterogeneous agents model described in the paper. The model was coded in Fortran 90. The table below presents a brief description of each file:

| File Name | Description |
| ------------- | ------------- |
| `make_script.sh` | A shell script that compiles and runs the Fortran code.
| `mod_nrtype.F90` | Module extracted from *Numerical Recipes* with definitions of types and mathematical constants.
| `mod_nrutil.F90` | Module extracted from *Numerical Recipes* with basic numerical routines.
| `mod_numerical_recipes.F90` | Module extracted from *Numerical Recipes* with advanced numerical routines.
| `mod_array_vec.F90` | Module with routines to vectorize the state space of the model.
| `mod_globals.F90` | Module with the definitions of parameters and variables of the model.
| `mod_functions.F90` | Module with the functions used in the model.
| `mod_output.F90` | Module with routines to generate the outputs of the simulations.
| `sub_allocation.F90` | Subroutine to allocate memory of array objects.
| `sub_calibration.F90` | Subroutine to read the inputs of the arrays used to represent model parameters.
| `sub_agents_problem.F90` | Subroutine to solve the agents' optimization problems.
| `sub_agents_distribution.F90` | Subroutine to calculate the distribution of agents.
| `sub_equilibrium_variables.F90` | Subroutine to calculate the equilibrium variables of the model.
| `sub_equilibrium_computation.F90` | Subroutine to manage the calculation of the model equilibrium.
| `sub_aggregate_variables.F90` | Subroutine to calculate aggregate variables after the convergence of the model.
| `main.F90` | Main program file.
