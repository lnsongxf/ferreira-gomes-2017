module mod_array_vec

!==============================================================================
! Description
!==============================================================================

! Let a multidimensional array A[i1,...,im] be such that
!   i1 in {1,...,N1},
!   ...
!   im in {1,...,Nm}.

! We want to create a vectorized array a[ind(i1,...,im)], with dimension given
! by N1*N2*...*Nm, such that the index ind() is a function of the dimensions
! of the multidimensional array. We will define ind() such that

!   ind(i1,...,im) = (i1-1)j1 + (i2-1)j2 + ... + (im-1)jm + 1,

! where the jumps {j1,...,jm} are defined as

!   j1 = N2*N3*...*Nm,
!   j2 = N3*N4*...*Nm,
!   ...
!   jm = 1.

! Notice that this rule satisfies

!   ind(1,...1,) = 1,
!   ind(N1,...,Nm) = N1*N2*...*Nm.

!==============================================================================
! Modules
!==============================================================================

  ! No modules

!==============================================================================
! General Commands
!==============================================================================

  implicit none
  
  contains
    
!==============================================================================
! Calculate the Size of the Jumps Made in the Vectorized Array
!==============================================================================

  subroutine sub_calc_jump(n, j)
  
  !----------------------------------------------------------------------------
  ! Input Variables
  !----------------------------------------------------------------------------
      
    ! Vector with the sizes of each dimension of the multidimensional
    ! array
    integer, dimension(:), intent(in) :: n
      
  !----------------------------------------------------------------------------
  ! Output Variables
  !----------------------------------------------------------------------------
      
    ! Vector with the size of the jumps made in the vectorized array
    integer, dimension(:), intent(out) :: j
      
  !----------------------------------------------------------------------------
  ! Local Variables
  !----------------------------------------------------------------------------
      
    ! Size of the input vector
    integer :: Nn
    
    ! Size of the output vector
    integer :: Nj
    
    ! Loop index
    integer :: k 
  
  !----------------------------------------------------------------------------
  ! Subroutine Commands
  !----------------------------------------------------------------------------
  
    ! Size of the input vector
    Nn = size(n)
    
    ! Size of the output vector
    Nj = size(j)
    
    ! Test: input and output vectors have different sizes
    if (Nn /= Nj) then
    
      ! Print error message
      print *, ""
      print *, "Error on calculating the jumps of the vectorized array!"
      print *, "Input and output vectors have different sizes!"
      print *, ""
      
      ! Terminates the program
      stop
    
    end if
    
    ! Size of the jump of the last dimension
    j(NN) = 1
    
    ! Loop: dimensions of the multidimensional array
    do k = NN-1, 1, -1
    
      ! Size of the jump of the k-dimension
      j(k) = j(k+1)*n(k+1)
    
    end do
  
  end subroutine sub_calc_jump

!==============================================================================
! Index of the Vectorized Array
!==============================================================================

  function ind(i, j) result(fres)
  
  !----------------------------------------------------------------------------
  ! Input Variables
  !----------------------------------------------------------------------------
  
    ! Vector with the indexes of the multidimensional array
    integer, dimension(:), intent(in) :: i
    
    ! Vector with the jumps made in the vectorized array
    integer, dimension(:), intent(in) :: j
      
  !----------------------------------------------------------------------------
  ! Output Variables
  !----------------------------------------------------------------------------
      
    ! Corresponding index of the vectorized array
    integer :: fres
  
  !----------------------------------------------------------------------------
  ! Local Variables
  !----------------------------------------------------------------------------
  
    ! Size of the vector with the indexes of the multidimensional array
    integer :: Ni
    
    ! Size of the vector with the jumps made in the vectorized array
    integer :: Nj
    
    ! Loop index
    integer :: k
      
  !----------------------------------------------------------------------------
  ! Function Commands
  !----------------------------------------------------------------------------
      
  ! Notice that we do not check if the size of the two input vectors are the
  ! same. Since this function is called many times during the execution of the
  ! program, this is done in order to improve the speed of execution.
      
    ! Size of the vector with the indexes of the multidimensional array
    Ni = size(i)
    
    ! Size of the vector with the jumps made in the vectorized array
    Nj = size(j)
    
    ! Test: input vectors have different sizes
    !if (Ni /= Nj) then
    
      ! Print error message
      !print *, ""
      !print *, "Error on calculating the index of the vectorized array!"
      !print *, "Input vectors have different sizes!"
      !print *, ""
      
      ! Terminates the program
      !stop
    
    !end if
    
    ! Initialize the index of the vectorized array
    fres = 1
    
    ! Loop: dimension of the input vectors
    do k = 1, Ni
    
      ! Index of the vectorized array
      fres = fres + (i(k) - 1)*j(k)
    
    end do
      
  end function ind

end module mod_array_vec