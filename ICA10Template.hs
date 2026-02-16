{-|
CS 381 — ICA 10
Expr2 with Static Type Checking (tc)

Name:
Date:

--------------------------------------------------
GOAL
--------------------------------------------------

Build a STATIC type checker for the Expr2 language.

The type checker analyzes expressions BEFORE execution
and rejects programs that are not well-typed.

IMPORTANT:
• tc does NOT evaluate expressions.
• Division by zero is NOT a type error.
• Never treat TypeError as a real type.
• Do NOT use mapM, traverse, etc.
• Write the recursion yourself.
-}

--------------------------------------------------
-- Object Language
--------------------------------------------------

data Expr
  = N Int
  | Plus Expr Expr
  | Mult Expr Expr
  | Neg Expr
  | Equal Expr Expr
  | Not Expr
  | Div Expr Expr
  deriving (Eq, Show)

data Val
  = I Int
  | B Bool
  deriving (Eq, Show)

data Error
  = TypeMismatch String
  | DivideByZero
  deriving (Eq, Show)

--------------------------------------------------
-- STATIC TYPES (Object Language)
--------------------------------------------------

data Type
  = TInt
  | TBool
  | TypeError
  deriving (Eq, Show)

--------------------------------------------------
-- Type Checker
--------------------------------------------------
-- The starter file includes completed typing rules
-- for N, Plus, and Not so you can see the expected style.
-- Complete the remaining cases.
--------------------------------------------------

tc :: Expr -> Type
tc expr =
  case expr of

    --------------------------------------------
    -- literals
    --------------------------------------------

    N _ ->
      TInt


    --------------------------------------------
    -- arithmetic
    --------------------------------------------

    Plus e1 e2 ->
      case (tc e1, tc e2) of
        (TInt, TInt) -> TInt
        _            -> TypeError


    Mult e1 e2 ->
      undefined


    Neg e ->
      undefined


    --------------------------------------------
    -- boolean operations
    --------------------------------------------

    Not e ->
      case tc e of
        TBool -> TBool
        _     -> TypeError


    Equal e1 e2 ->
      undefined


    --------------------------------------------
    -- division
    --------------------------------------------
    -- IMPORTANT:
    -- Div has the SAME typing rule as Plus/Mult,
    -- but unlike them it can still fail at runtime.
    --------------------------------------------

    Div e1 e2 ->
      undefined


--------------------------------------------------
-- Helper
--------------------------------------------------

wellTyped :: Expr -> Bool
wellTyped e = tc e /= TypeError


--------------------------------------------------
-- Safe Runner
--------------------------------------------------

runSafe :: Expr -> Either Error Val
runSafe e =
  if wellTyped e
    then sem e
    else Left (TypeMismatch "Expression rejected by type checker")


--------------------------------------------------
-- Interpreter (from ICA 9)
-- DO NOT MODIFY
--------------------------------------------------

sem :: Expr -> Either Error Val

sem (N i) = Right (I i)

sem (Plus e1 e2) =
  case (sem e1, sem e2) of
    (Right (I i1), Right (I i2)) -> Right (I (i1 + i2))
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    _ -> Left (TypeMismatch "Plus: runtime type error (type checker bypassed)")

sem (Mult e1 e2) =
  case (sem e1, sem e2) of
    (Right (I i1), Right (I i2)) -> Right (I (i1 * i2))
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    _ -> Left (TypeMismatch "Mult: runtime type error (type checker bypassed)")

sem (Neg e) =
  case sem e of
    Right (I i) -> Right (I (-i))
    Left err -> Left err
    _ -> Left (TypeMismatch "Neg: runtime type error (type checker bypassed)")

sem (Equal e1 e2) =
  case (sem e1, sem e2) of
    (Right (I i1), Right (I i2)) -> Right (B (i1 == i2))
    (Right (B b1), Right (B b2)) -> Right (B (b1 == b2))
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    _ -> Left (TypeMismatch "Equal: runtime type error (type checker bypassed)")

sem (Not e) =
  case sem e of
    Right (B b) -> Right (B (not b))
    Left err -> Left err
    _ -> Left (TypeMismatch "Not: runtime type error (type checker bypassed)")

sem (Div e1 e2) =
  case (sem e1, sem e2) of
    (Right (I _), Right (I 0)) -> Left DivideByZero
    (Right (I i1), Right (I i2)) -> Right (I (i1 `div` i2))
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    _ -> Left (TypeMismatch "Div: runtime type error (type checker bypassed)")


--------------------------------------------------
-- Equal Trigger (Think Carefully!)
--------------------------------------------------
-- What should the type checker return for this?
--
-- If your checker returns TBool,
-- your Equal rule is probably too permissive.
--------------------------------------------------

equalTrigger :: Expr
equalTrigger =
  Equal
    (Not (N 5))
    (Plus (Not (N 3)) (N 4))

--------------------------------------------------
-- Mult / Div Triggers (Think Carefully!)
--------------------------------------------------
-- These are diagnostic expressions to help you test your tc rules.
-- They should help catch common mistakes such as:
--   • allowing Bool * Bool or Bool / Bool
--   • treating TypeError as a real type
--------------------------------------------------

multTrigger :: Expr
multTrigger =
  Mult (Not (Equal (N 1) (N 2)))
       (Not (Equal (N 3) (N 4)))
-- If your tc returns TInt here, your Mult rule is too permissive.

divTrigger :: Expr
divTrigger =
  Div (N 6)
      (Not (Equal (N 1) (N 2)))
-- If your tc returns TInt here, your Div rule is too permissive.

--------------------------------------------------
-- Student Test Expressions
--------------------------------------------------
-- Create THREE expressions:
--
-- eGood       : well-typed and evaluates successfully
-- eTypeBad    : rejected by the type checker
-- eRuntimeBad : passes the type checker but fails at runtime
--
-- Do NOT use lists/programs for this ICA.
--------------------------------------------------

eGood :: Expr
eGood = undefined

eTypeBad :: Expr
eTypeBad = undefined

eRuntimeBad :: Expr
eRuntimeBad = undefined
