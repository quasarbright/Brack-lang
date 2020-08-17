import Test.HUnit

import Brack.Static.TypeCheck
import Brack.Parsing.Parser
import Brack.Parsing.ParseUtils(SS, dummySS)
import Brack.Utils.Common
import Brack.Syntax.Module
import Brack.Syntax.Type
import Brack.Static.StaticError

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

main :: IO Counts
main = runTestTT $ do
    TestLabel "Type checking tests" $ TestList
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
        ]