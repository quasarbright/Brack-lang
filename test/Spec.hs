import Test.HUnit

import Brack.Static.TypeCheck
import Brack.Parsing.Parser
import Brack.Parsing.ParseUtils(SS, dummySS)
import Brack.Utils.Common
import Brack.Syntax.Module
import Brack.Syntax.Name
import Brack.Syntax.Type
import Brack.Static.StaticError
import Brack.Dynamic.Memory
import Brack.Dynamic.Interpreter

teq :: (Eq a, Show a) => String -> a -> a -> Test
teq name a b = TestCase (assertEqual name a b)

tpass :: Test
tpass = TestCase $ assertEqual "pass" True True

parseModuleSame :: String -> String -> Module (AllSame SS)
parseModuleSame name input = AllSame <$> parseModuleUnsafe name input

tProgramCheckPass :: String -> String -> Test
tProgramCheckPass name code = teq name (Right ()) actual
    where
        m = parseModuleSame name code
        actual = case runTypeChecking m of
            Left err -> Left err
            Right _ -> Right ()

tProgramCheckFail :: String -> String -> StaticError (AllSame SS) -> Test
tProgramCheckFail name code err = teq name (Left err) actual
    where
      m = parseModuleSame name code
      actual = case runTypeChecking m of
          Left (err, _) -> Left err
          Right r -> Right r

ss :: AllSame SS
ss = AllSame dummySS

tInterpExports :: String -> String -> [(String, Cell)] -> Test
tInterpExports name code stack = teq name (Right stack') actual
    where
        stack' = [(UnQualified (LowerIdent x ss) ss, c) | (x,c) <- stack]
        m = parseModuleSame name code
        actual = case runProgram m of
            Left err -> Left err
            Right (_,s) -> Right (getStack s)

mismatch a b = TypeMismatch (a ss) (b ss) ss

main :: IO Counts
main = runTestTT $ TestList
    [ TestLabel "Type checking tests" (TestList
        [ tpass
        , tProgramCheckPass "one" "1;"
        , tProgramCheckPass "1 is an int" "x :: int = 1;"
        , tProgramCheckPass "1.0 is a double" "x :: double = 1.0;"
        , tProgramCheckPass "'a' is a char" "x :: char = 'a';"
        , tProgramCheckPass "true is a boolean" "x :: bool = true;"
        , tProgramCheckPass "false is a boolean" "x :: bool = false;"
        , tProgramCheckPass "variable references work" "x :: int = 1; y :: int = x;"
        , tProgramCheckPass "simple if" "if (true) { 1; } else { 2; }"
        , tProgramCheckPass "if with variable reference condition" "x :: bool = true; if(x) { 1; } else { 2; }"
        , tProgramCheckFail "1 is not a bool" "x :: bool = 1;" (TypeMismatch (TBool ss) (TInt ss) ss)
        , tProgramCheckFail "assign with different types" "x :: bool = true; x = 1;" (TypeMismatch (TBool ss) (TInt ss) ss)
        , tProgramCheckFail "if condition must be boolean" "if (1) { true; }" (TypeMismatch (TBool ss) (TInt ss) ss)
        , tProgramCheckFail "if condition non-boolean reference" "x :: int = 1; if (x) { true; }" (TypeMismatch (TBool ss) (TInt ss) ss)
        , tProgramCheckFail "while condition must be boolean" "while (1) { true; }" (TypeMismatch (TBool ss) (TInt ss) ss)
        , tProgramCheckPass "variable types are scoped" $
            unlines [ "x :: int = 1;"
                    , "if (true) {"
                    , "  y :: int = x;"
                    , "  x :: bool = true;"
                    , "  z :: bool = x;"
                    , "}"
                    , "y :: int = x;"
                    ]
        , tProgramCheckFail "variable types are scoped (error)"
            (unlines [ "x :: int = 1;"
                    , "if (true) {"
                    , "  y :: int = x;"
                    , "  x :: bool = true;"
                    , "  z :: bool = x;"
                    , "}"
                    , "y :: bool = x;"
                    ])
            (TypeMismatch (TBool ss) (TInt ss) ss)
        , tProgramCheckPass "1 + 1 by itself" "1 + 1;"
        , tProgramCheckPass "int + int = int" "x :: int = 1 + 1;"
        , tProgramCheckPass "int + double = double" "x :: double = 1 + 1.0;"
        , tProgramCheckPass "double + int = double" "x :: double = 1.0 + 1;"
        , tProgramCheckPass "double + double = double" "x :: double = 1.0 + 1.0;"
        , tProgramCheckPass "int ^ int = double" "x :: double = 1 ^ 1;"
        , tProgramCheckPass "int ^ double = double" "x :: double = 1 ^ 1.0;"
        , tProgramCheckPass "double ^ int = double" "x :: double = 1.0 ^ 1;"
        , tProgramCheckPass "double ^ double = double" "x :: double = 1.0 ^ 1.0;"
        , tProgramCheckPass "negative int = int" "x :: int = -1;"
        , tProgramCheckPass "negative double = double" "x :: double = -1.0;"
        , tProgramCheckPass "bool || bool = bool" "x :: bool = true || true;"
        , tProgramCheckPass "!bool = bool" "x :: bool = !true;"
        , tProgramCheckPass "int < int = bool" "x :: bool = 1 < 1;"
        , tProgramCheckPass "int < double = bool" "x :: bool = 1 < 1.0;"
        , tProgramCheckPass "double < int = bool" "x :: bool = 1.0 < 1;"
        , tProgramCheckPass "double < double = bool" "x :: bool = 1.0 < 1.0;"
        , tProgramCheckPass "int == bool = bool" "x :: bool = 1 == true;"
        , tProgramCheckPass "int != bool = bool" "x :: bool = 1 != true;"
        , tProgramCheckFail "int + bool fails" "1 + true;" (mismatch TInt TBool)
        , tProgramCheckFail "bool + int fails" "true + 1;" (mismatch TInt TBool)
        , tProgramCheckFail "double + bool fails" "1.0 + true;" (mismatch TDouble TBool)
        , tProgramCheckFail "bool + double fails" "true + 1.0;" (mismatch TDouble TBool)
        , tProgramCheckFail "bool + bool fails" "true + true;" (mismatch TDouble TBool)
        , tProgramCheckFail "bool < int fails" "true < 1;" (mismatch TInt TBool)
        , tProgramCheckFail "bool || int fails" "true || 1;" (mismatch TBool TInt)
        , tProgramCheckFail "int || bool fails" "1 || true;" (mismatch TBool TInt)
        , tProgramCheckFail "int || int fails" "1 || 1;" (mismatch TBool TInt)
        , tProgramCheckFail "negative bool fails" "-true;" (mismatch TDouble TBool)
        , tProgramCheckFail "not int fails" "!1;" (mismatch TBool TInt)
        ])

    , TestLabel "interpreter tests" $ TestList
        [ tpass
        , tInterpExports "run empty module" "" []
        , tInterpExports "x = 1" "x :: int = 1;" [("x", CInt 1)]
        , tInterpExports "if reassignment"
            (unlines
                [ "x :: int = 1;"
                , "b :: bool = false;"
                , "if (b) { x = 2; } else { x = 3; }"
                ])
            [("b", CBool False), ("x", CInt 3)]
        , tInterpExports "basic while"
            (unlines
                [ "x :: int = 1;"
                , "b :: bool = true;"
                , "while (b) { x = 2; b = false; }"
                ])
            [("b", CBool False), ("x", CInt 2)]
        ]
    ]