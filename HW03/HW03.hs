module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend state str value x = if x == str then value else state x


empty :: State
empty = const 0

-- Exercise 2 -----------------------------------------

toInt :: Bool -> Int
toInt True = 1
toInt False = 0

evalE :: State -> Expression -> Int
evalE st (Var str) = st str
evalE _ (Val int) = int
evalE st (Op exp1 Plus exp2) = evalE st exp1 + evalE st exp2
evalE st (Op exp1 Minus exp2) = evalE st exp1 - evalE st exp2
evalE st (Op exp1 Times exp2) = evalE st exp1 * evalE st exp2
evalE st (Op exp1 Divide exp2) = evalE st exp1 `div` evalE st exp2
evalE st (Op exp1 Gt exp2) = toInt $ evalE st exp1 > evalE st exp2
evalE st (Op exp1 Ge exp2) = toInt $ evalE st exp1 >= evalE st exp2
evalE st (Op exp1 Lt exp2) = toInt $ evalE st exp1 < evalE st exp2
evalE st (Op exp1 Le exp2) = toInt $ evalE st exp1 <= evalE st exp2
evalE st (Op exp1 Eql exp2) = toInt $ evalE st exp1 == evalE st exp2

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign var expr) = DAssign var expr
desugar (Incr var) = DAssign var (Op (Var var) Plus (Val 1))
desugar (If expr stmt1 stmt2) = DIf expr (desugar stmt1) (desugar stmt2)
desugar (While expr stmt) = DWhile expr (desugar stmt)
desugar (For stmt1 expr stmt2 stmt3) = DSequence (desugar stmt1) (DWhile expr (DSequence (desugar stmt3) (desugar stmt2)))
desugar (Sequence stmt1 stmt2) = DSequence (desugar stmt1) (desugar stmt2)
desugar Skip = DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple pre (DAssign var expr) = extend pre var $ evalE pre expr
evalSimple pre DSkip = pre
evalSimple pre (DSequence l r) = evalSimple (evalSimple pre l) r
evalSimple pre (DIf cond y n)
  | evalE pre cond == 0 = evalSimple pre n
  | otherwise = evalSimple pre y
evalSimple pre (DWhile cond stmt)
  | evalE pre cond == 0 = pre
  | otherwise = evalSimple pre (DSequence stmt (DWhile cond stmt))


run :: State -> Statement -> State
run state program = evalSimple state $ desugar program

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
