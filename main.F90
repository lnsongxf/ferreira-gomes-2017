program main

!==============================================================================
! Modules
!==============================================================================

  use mod_numerical_recipes
  use mod_array_vec
  use mod_globals
  use mod_output

!==============================================================================
! General Commands
!==============================================================================

  implicit none

!==============================================================================
! Variables to Measure Execution Time
!==============================================================================
    
  ! Initial time
  real(4) :: time_initial
  
  ! Final time
  real(4) :: time_final
  
  ! Execution time (in seconds)
  real(4) :: time_total
  
  ! Number of hours
  integer :: hours
  
  ! Number of minutes
  integer :: minutes
  
  ! Number of seconds
  integer :: seconds
    
!==============================================================================
! Initial Time
!==============================================================================

  ! Save the initial time
  call cpu_time(time_initial)

!==============================================================================
! Allocation of Memory
!==============================================================================

  call sub_allocation
    
!==============================================================================
! Calibration of Array Parameters
!==============================================================================

  call sub_calibration

!==============================================================================
! Equilibrium Computation
!==============================================================================

  call sub_equilibrium_computation

!==============================================================================
! Aggregate Variables
!==============================================================================

  call sub_aggregate_variables

!==============================================================================
! Program Output
!==============================================================================
  
  ! Main results
  call sub_output_results
  
  ! Equilibrium variables
  call sub_output_equilibrium_variables
  
  ! Utilities
  call sub_output_utilities
  
  ! Inequality measures
  call sub_output_inequality
  
  ! Life cycle macroeconomic variables
  call sub_output_life_cycle_macro
  
  ! Distribution health insurance coverage
  call sub_output_dist_hi
  
  ! Life cycle health insurance coverage
  call sub_output_life_cycle_hi
  
  ! Age group health insurance coverage
  call sub_output_age_group_hi
  
  ! Welfare measures
  call sub_output_welfare_measures
    
!==============================================================================
! Final Time
!==============================================================================

  ! Save the final time
  call cpu_time(time_final)

!==============================================================================
! Execution Time
!==============================================================================

  ! Calculate the execution time (seconds)
  time_total = time_final - time_initial
  
  ! Number of hours
  hours = int(time_total/3600)
  
  ! Number of minutes
  minutes = int((time_total - real(hours)*3600)/60)
  
  ! Number of seconds
  seconds = nint(time_total - real(hours)*3600 - real(minutes)*60)
  
  ! Print the execution time
  print *, ""
  print "(a, i2, a, i2.2, a, i2.2, a)", "Execution Time: ",   &
                                        hours, " hours ",     &
                                        minutes, " minutes ", &
                                        seconds, " seconds."
  print *, ""
    
end program main