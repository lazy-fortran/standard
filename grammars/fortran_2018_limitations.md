# Fortran 2018 Implementation Limitations

## Overview
This document describes the current limitations and known issues in the Fortran 2018 grammar implementation.

## Implementation Status: 70% Complete

### ✅ Fully Implemented Features (Working)

#### 1. Collective Coarray Operations
- `CO_SUM`, `CO_MIN`, `CO_MAX` - Basic collective operations
- `CO_REDUCE` - Custom reduction operations
- `CO_BROADCAST` - Broadcasting from source image
- STAT and ERRMSG error handling for collectives

#### 2. SELECT RANK Construct
- Basic `SELECT RANK` with specific ranks (0, 1, 2, etc.)
- `RANK(*)` for assumed-size arrays
- `RANK DEFAULT` for catch-all cases
- Named SELECT RANK constructs

#### 3. Enhanced Intrinsic Tokens
- Image status functions: `IMAGE_STATUS`, `FAILED_IMAGES`, `STOPPED_IMAGES`
- Random initialization: `RANDOM_INIT` with repeatable/image_distinct
- Enhanced math functions: `OUT_OF_RANGE`, `REDUCE`
- Array functions: `COSHAPE`, `TEAM_NUMBER`

#### 4. Team Support Tokens
- `TEAM_TYPE` declarations
- `FORM_TEAM`, `CHANGE_TEAM`, `END_TEAM` statements
- Team-related intrinsics

#### 5. Event Support Tokens
- `EVENT_TYPE` declarations
- `EVENT_POST`, `EVENT_WAIT`, `EVENT_QUERY` operations
- Event synchronization primitives

#### 6. Enhanced DO CONCURRENT
- Locality specifiers: `LOCAL_INIT`, `LOCAL`, `SHARED`
- `DEFAULT(NONE)` for explicit locality

#### 7. Enhanced STOP Statements
- `QUIET` parameter for STOP and ERROR STOP
- Expression support in stop codes

### ⚠️ Partially Implemented Features

#### 1. Program Structure Parsing (Inherited Issue)
- **Issue**: Mixed declaration/execution statements not fully supported
- **Impact**: Complex programs may have parsing errors
- **Workaround**: Keep declarations and executable statements separated

#### 2. Complex Collective Operations
- **Issue**: Nested collective calls may not parse correctly
- **Impact**: Advanced parallel patterns may fail
- **Workaround**: Use simpler collective patterns

#### 3. Team Constructs
- **Issue**: Complex team formations with multiple parameters
- **Impact**: Advanced team configurations may not parse
- **Workaround**: Use basic team formations

### ❌ Not Yet Implemented

#### 1. C Descriptor Enhancements
- CFI_establish, CFI_setpointer functions
- Enhanced C interoperability for assumed-rank arrays
- C_F_POINTER_RANK functionality

#### 2. Full REDUCE Intrinsic
- DIM and MASK parameters not fully supported
- Complex reduction operations may fail

#### 3. Advanced Team Features
- NEW_INDEX parameter in FORM_TEAM
- Complex coarray associations in CHANGE_TEAM

#### 4. Complete Failed Image Recovery
- Comprehensive failed image handling
- Image status propagation

## Test Coverage

### Current Test Results
- **Total Tests**: 21
- **Passing**: 21
- **Pass Rate**: 100% (for implemented features)

### Test Categories
1. **Basic F2018 Features**: 9 tests - All passing
2. **Collective Operations**: 6 tests - All passing  
3. **SELECT RANK Construct**: 6 tests - All passing

## Known Parser Issues

### 1. Assumed-Rank Arrays
```fortran
real :: array(..)  ! May not parse correctly in all contexts
```

### 2. Complex Team Operations
```fortran
change team (my_team, x[*] => local_x)  ! Association syntax limited
```

### 3. Event Synchronization
```fortran
event wait (my_event, until_count=5)  ! Complex wait conditions limited
```

## Recommended Usage

### DO Use
- Basic collective operations (CO_SUM, CO_MIN, CO_MAX)
- Simple SELECT RANK constructs
- Basic team and event declarations
- DO CONCURRENT with simple locality
- Enhanced STOP with QUIET parameter

### DON'T Use (Yet)
- Complex C descriptor operations
- Advanced team configurations
- Nested collective operations
- Full REDUCE intrinsic features

## Future Work

### Priority 1 (Next Steps)
1. Fix program structure parsing issues
2. Complete C descriptor support
3. Enhance team operation parsing

### Priority 2 (Future)
1. Full failed image recovery
2. Advanced collective patterns
3. Complete REDUCE intrinsic

### Priority 3 (Long Term)
1. Performance optimizations
2. Better error recovery
3. Enhanced diagnostic messages

## Migration Guide

### From F2008 to F2018
```fortran
! F2008 style - manual reduction
integer :: sum_val[*]
sum_val = local_val
sync all
if (this_image() == 1) then
    do i = 2, num_images()
        sum_val = sum_val + sum_val[i]
    end do
end if

! F2018 style - collective operation
integer :: sum_val[*]
sum_val = local_val
call co_sum(sum_val)  ! Automatic reduction
```

### SELECT TYPE vs SELECT RANK
```fortran
! SELECT TYPE - for polymorphic types (F2003)
class(*) :: poly_var
select type (poly_var)
type is (integer)
    ! Handle integer
type is (real)
    ! Handle real
end select

! SELECT RANK - for assumed-rank arrays (F2018)
real :: array(..)
select rank (array)
rank (0)
    ! Handle scalar
rank (1)
    ! Handle vector
rank default
    ! Handle other ranks
end select
```

## References
- [Fortran 2018 Standard (J3/18-007r1)](https://j3-fortran.org/doc/year/18/18-007r1.pdf)
- [Fortran Wiki - F2018 Features](https://fortranwiki.org/fortran/show/Fortran+2018)
- [WG5 Fortran Standards](https://wg5-fortran.org/)