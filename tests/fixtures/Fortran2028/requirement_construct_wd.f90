requirement addable{T, add}
    type, deferred :: T
    procedure(add_sig) :: add
end requirement addable
