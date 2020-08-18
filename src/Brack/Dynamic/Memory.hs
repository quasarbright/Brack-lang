{-
Emulated Stack and Heap
-}
module Brack.Dynamic.Memory where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Foldable (find)
import Brack.Syntax.Name
import Brack.Syntax.Statement
import Brack.Syntax.Type

data StackItem a = Variable (QName a) Cell
                 | ScopeMarker
                 deriving(Eq, Ord, Show)

data FrameReason a = Main a -- bottom of the call stack
                   | Call -- function call
                       (BoxedValue a) -- function value
                       a -- call site
                   deriving(Eq, Ord, Show)

data StackFrame a = StackFrame (FrameReason a) [StackItem a] deriving(Eq, Ord, Show)

data Memory a = Memory { getStack :: [StackFrame a]
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
                        (Maybe (QName a)) -- function name
                        [(QName a, Type a)] -- typed arguments
                        (Type a)
                        [Statement a] -- body
                        (Map (QName a) Cell) -- closed-over variables
                        a -- definition site
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
getNextHeapAddress mem
    | null (getHeap mem) = 0
    | otherwise = succ . maximum . Map.keys . getHeap $ mem

stackLookup :: Eq a => Memory a -> QName a -> Maybe Cell
stackLookup m name = searchFrames (getStack m)
    where
        searchFrames (StackFrame _ items:frames) =
            case searchItems items of
                Nothing -> searchFrames frames
                Just value -> Just value
        searchFrames [] = Nothing
        searchItems items = getVal <$> find p items
        p (Variable name' _) = name' == name
        p _ = False
        getVal (Variable _ value) = value
        getVal _ = error "predicate failed"

-- | all local variables in current stack frame (ignores scope markers)
getLocals :: Ord a => Memory a -> Map (QName a) Cell
getLocals mem =
    case getStack mem of
        StackFrame _ items:_ -> Map.fromList [(name, value) | Variable name value <- items] -- ok bc list is MonadFail
        _ -> error "no stack frames for getLocals"

heapLookup :: Memory a -> Address -> Maybe (BoxedValue a)
heapLookup m addr = Map.lookup addr (getHeap m)

modifyFrameItems :: ([StackItem a] -> [StackItem a]) -> Memory a -> Memory a
modifyFrameItems f mem = mem {getStack = stack'}
    where
     stack' = case getStack mem of
         StackFrame r items:frames -> StackFrame r (f items):frames
         [] -> error "no stack frames"

pushVariable :: QName a -> Cell -> Memory a -> Memory a
pushVariable name value = modifyFrameItems (Variable name value :)

pushScope :: Memory a -> Memory a
pushScope = modifyFrameItems (ScopeMarker :)

pushFrame :: FrameReason a -> Memory a -> Memory a
pushFrame reason mem = mem{getStack = StackFrame reason []:getStack mem}

-- | reassigns the most recent entry with the given name
updateVariable :: Eq a => QName a -> Cell -> Memory a -> Memory a
updateVariable name value = modifyFrameItems go
    where
        go [] = []
        go (item:items) =
            case item of
                Variable name' _ | name == name' -> Variable name' value:items
                _ -> item:go items

popFrame :: Memory a -> Memory a
popFrame mem = mem{getStack = tail (getStack mem)}

popScope :: Eq a => Memory a -> Memory a
popScope = modifyFrameItems (tail . dropWhile (/= ScopeMarker))

heapAlloc :: BoxedValue a -> Memory a -> (Memory a, Address)
heapAlloc value mem =
    let addr = getNextHeapAddress mem
        mem' = mem{getHeap = Map.insert addr value (getHeap mem)}
    in (mem', addr)

-- TODO garbage collection (delete addresses inaccessible from the stack)