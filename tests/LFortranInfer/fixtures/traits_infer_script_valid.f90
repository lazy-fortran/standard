abstract interface :: IScalar
    integer | real
end interface IScalar

implements IScalar :: integer
end implements integer

type, sealed, implements(IScalar) :: scalar_box
contains
    initial :: init
end type scalar_box
