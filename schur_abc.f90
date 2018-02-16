SUBROUTINE schur_abc(a,b,p,lwork_factor)
  IMPLICIT NONE
  COMPLEX(KIND=8), DIMENSION(:,:), INTENT(IN) :: a
  COMPLEX(KIND=8), DIMENSION(:,:), INTENT(OUT) :: b
  INTEGER, INTENT(OUT) :: p
  INTEGER, INTENT(IN), OPTIONAL :: lwork_factor

  ! Return an asymptotic boundary condition matrix for the ordinary
  ! differential equation 
  !
  !      y'(x) = a y(x) 
  !
  ! to replace 
  !
  !      y(x) -> 0 
  !
  ! for unbounded x with
  !
  !      b y(x) = 0
  !
  ! at some large but finite x.  On output, P is the number of rows;
  ! i.e. the number of eigenvalues of A with positive real part.

  CHARACTER(LEN=1), PARAMETER :: &
       jobvs = 'V', &           ! do compute Schur vectors
       sort = 'S'               ! do order eigenvalues
  INTEGER, PARAMETER :: minimal_lwork_factor = 2
  INTEGER :: lwork

  INTEGER :: info, n
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: bwork
  REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: rwork
  COMPLEX(KIND=8), ALLOCATABLE, DIMENSION(:,:) :: ah, vs
  COMPLEX(KIND=8), ALLOCATABLE, DIMENSION(:) :: lambda ! ZGEEV's W
  COMPLEX(KIND=8), ALLOCATABLE, DIMENSION(:) :: work

  n = SIZE(a,1)

  IF (PRESENT(lwork_factor)) THEN
     lwork = n*MAX(minimal_lwork_factor,lwork_factor)
  ELSE
     lwork = n*minimal_lwork_factor
  END IF

  ALLOCATE(ah(n,n),vs(n,n),lambda(n),work(lwork),rwork(n),bwork(n))

  ah = CONJG(TRANSPOSE(a))

  CALL ZGEES(jobvs, sort, nonnegative_real_part, n, ah, &
       n, p, lambda, vs, n, work, lwork, rwork, bwork, info)

  DEALLOCATE(ah,vs,lambda,work,rwork,bwork)

  CALL check_zgees_info(info, n) ! abort for nonzero info

  ! Put conjugate transpose of first P cols of VS into P rows of B. 

  IF (SIZE(b,1) /= p) THEN
     PRINT *, 'schur_abc: The number of eigenvalues (', p, ') with '
     PRINT *, "positive real part doesn't match rows of B, ", SIZE(b,1)
     STOP 
  END IF 

  b = CONJG(TRANSPOSE(vs(:,1:p))) 

CONTAINS 

  SUBROUTINE check_zgees_info(info,n) 
    INTEGER, INTENT(IN) :: info, n

    SELECT CASE (info)
    CASE (0)
       CONTINUE                   ! `successful exit'
    CASE (:-1)
       PRINT *, 'ZGEES illegal value for argument ', -info
       STOP
    CASE (1:)
       SELECT CASE (info-n)
       CASE (:0)
          PRINT *, 'ZGEES QR algorithm failed to converge.'
          STOP
       CASE (1)  
          PRINT *, 'ZGEES: Indistinct eigenvalues.'
          STOP
       CASE (2)

          ! 'after reordering, roundoff changed values of some
          ! complex eigenvalues so that leading eigenvalues in the
          ! Schur form no longer satisfy SELECT = .TRUE..  This
          ! could be caused also be caused by underflow due to
          ! scaling.' 

          PRINT *, 'ZGEES: Borderline eigenvalues.'
          STOP
       END SELECT
    END SELECT
  END SUBROUTINE check_zgees_info
END SUBROUTINE schur_abc

LOGICAL FUNCTION nonnegative_real_part(z)
  IMPLICIT NONE
  COMPLEX(KIND=8), INTENT(IN) :: z

  ! Return .TRUE. if the real part of Z is nonnegative

  nonnegative_real_part = (REAL(z) >= REAL(0,KIND(z)))

END FUNCTION nonnegative_real_part
