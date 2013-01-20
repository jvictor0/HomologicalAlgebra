{-# LANGUAGE CPP, NoImplicitPrelude #-}
module FieldIsPID where

import NumericPrelude 
import qualified Algebra.PrincipalIdealDomain as PID
import qualified Algebra.Field as Field
import qualified Algebra.Units as Units
import qualified Algebra.IntegralDomain as Domain
import qualified Number.Ratio as Ratio

-- I want fields to be PIDS

#define FIELDISDOMAIN(Context, FieldType)                      \
instance (Context) => Domain.C (FieldType) where { divMod x y = (x/y,zero)   }


#define FIELDISUNIT(Context,FieldType)                        \
instance (Context) => Units.C (FieldType) where {             \
  isUnit _ = True;                                            \
  stdUnitInv = (one/)                                           \
}                                                             \

#define FIELDISPID(Context,FieldType)                         \
instance (Context) => PID.C (FieldType) where {               \
 extendedGCD x y = (one,(one/(x+y),one/(x+y)));               \
 gcd x y = one;                                               \
 lcm x y = one }                                               

FIELDISDOMAIN(,Double)
FIELDISUNIT(,Double)
FIELDISPID(,Double)

FIELDISDOMAIN(PID.C t,Ratio.T t)
FIELDISUNIT(PID.C t,Ratio.T t)
FIELDISPID(PID.C t,Ratio.T t)

