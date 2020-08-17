{-
Emulated Stack and Heap
-}
module Brack.Dynamic.Memory where

import Data.Map(Map)
import qualified Data.Map as Map

--import Data.Set(Set)
--import qualified Data.Set as Set

--import Data.List

import Brack.Syntax.Name
import Brack.Syntax.Expr

-- TODO stack frames
data Memory a = Memory { getStack :: [(QName a, Cell)] -- list for push-pop
                       , getHeap  :: Map Address (BoxedValue a)
                       }
                       deriving(Eq, Ord, Show)

data Cell = CInt Integer
            | CChar Char
            | CDouble Double
            | CBool Bool
            | Pointer Address
            deriving(Eq, Ord, Show)

data BoxedValue a = Null -- temporarily used for cyclic data
                  | Closure
                        [QName a] -- arg names
                        (Expr a) -- body
                        (Map (QName a) Cell) -- closed-over variables
                  | Object
                        (QName a) -- class name
                        (Map (QName a) Cell) -- fields
                  | Tuple [Cell]
                  | List [Cell] -- TODO need a better representation
                  deriving(Eq, Ord, Show)

type Address = Integer

emptyMemory :: Memory a
emptyMemory = Memory [] Map.empty

getNextHeapAddress :: Memory a -> Address
getNextHeapAddress = succ . maximum . Map.keys . getHeap

stackLookup :: Eq a => Memory a -> QName a -> Maybe Cell
stackLookup m name = lookup name (getStack m)

heapLookup :: Memory a -> Address -> Maybe (BoxedValue a)
heapLookup m addr = Map.lookup addr (getHeap m)

pushStack :: QName a -> Cell -> Memory a -> Memory a
pushStack name value mem = mem {getStack = (name,value):getStack mem}

-- | reassigns the most recent entry with the given name
updateStack :: Eq a => QName a -> Cell -> Memory a -> Memory a
updateStack name value mem = mem { getStack = stack'}
    where
        stack = getStack mem
        (before, after) = span (\(name',_) -> name /= name') stack
        stack' = case after of
            _:rest -> before++(name, value):rest
            [] -> error "name not found in stack update"



popStack :: Memory a -> ((QName a, Cell), Memory a)
popStack mem =
    let stack = getStack mem
    in if null stack
    then error "cannot pop empty stack"
    else (head stack, mem{getStack = tail stack})

-- TODO garbage collection (delete addresses inaccessible from the stack)