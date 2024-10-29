PROGRAM BankingTransactions
  IMPLICIT NONE
  INTEGER, PARAMETER :: max_transactions = 100
  REAL, ALLOCATABLE :: deposits(:), withdrawals(:)
  INTEGER :: N, M, i
  REAL :: total_deposits = 0.0, total_withdrawals = 0.0, balance

  WRITE(*, *) "Please input the numbers of depositing N:"
  READ(*, *) N
  ALLOCATE(deposits(N))
  DO i = 1, N
    WRITE(*, *) "Please input the amount of depositing of this time $", i
    READ(*, *) deposits(i)
    total_deposits = total_deposits + deposits(i)
  END DO
  WRITE(*, *) "The total amount of depositing is:", total_deposits
  DEALLOCATE(deposits)

  WRITE(*, *) "Please input the numbers of withdrawaling M:"
  READ(*, *) M
  ALLOCATE(withdrawals(M))
  DO i = 1, M
    WRITE(*, *) "Please input the amount of withdrawaling of this time $", i
    READ(*, *) withdrawals(i)
    total_withdrawals = total_withdrawals + withdrawals(i)
  END DO
  WRITE(*, *) "The total amount of withdrawaling is:", total_withdrawals
  DEALLOCATE(withdrawals)

  balance = total_deposits - total_withdrawals
  WRITE(*, *) "The balance is:", balance

END PROGRAM BankingTransactions
