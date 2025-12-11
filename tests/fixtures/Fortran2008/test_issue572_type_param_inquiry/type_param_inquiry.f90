PROGRAM type_param_inquiry
  IMPLICIT NONE

  TYPE param_holder(len_param, kind_param)
    INTEGER, LEN :: len_param = 4
    INTEGER, KIND :: kind_param = 8
    REAL(KIND=kind_param) :: data(len_param)
  END TYPE param_holder

  TYPE(param_holder(len_param=5, kind_param=8)) :: instance
  INTEGER :: observed_len
  INTEGER :: observed_kind

  instance%data = [1.0, 2.0, 3.0, 4.0, 5.0]
  observed_len = instance%len_param
  observed_kind = instance%kind_param

  PRINT *, 'LEN:', observed_len, 'KIND:', observed_kind
END PROGRAM type_param_inquiry
