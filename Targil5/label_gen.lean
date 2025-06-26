namespace VMGenerator

structure LabelGen where
  counter : Nat
  deriving Repr

namespace LabelGen
def new : LabelGen := ⟨0⟩

def next (lg : LabelGen) (pre : String) : (String × LabelGen) :=
  (s!"{pre}{lg.counter}", ⟨lg.counter + 1⟩)
end LabelGen

end VMGenerator
