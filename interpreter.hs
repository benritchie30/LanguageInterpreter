import Data.Char

-- Variables
type Vars = String
-- Arithmetic expressions
data AExpr = Var Vars | Const Integer
           | Add AExpr AExpr | Sub AExpr AExpr
           | Mul AExpr AExpr | Div AExpr AExpr
  deriving Show
-- Boolean expressions
data BExpr = TT | FF -- the true and false constants
           | And BExpr BExpr | Or BExpr BExpr | Not BExpr -- boolean operations
           | Eql AExpr AExpr -- equality of arithmetic expressions
           | Lt AExpr AExpr -- true i f the f i r s t is less than the second
  deriving Show
-- Instructions
data Instr = Assign Vars AExpr -- assign X to the value of an expression
           | IfThenElse BExpr Instr Instr -- conditional
           | While BExpr Instr -- looping construct
           | Do [ Instr ] -- a block of several instructions
           | Nop -- the "do nothing" instruction
  deriving Show
-- A program is a l i s t of instructions
type Program = [ Instr ]

type Env = [(Vars,Integer)]


lookUp :: Vars -> Env -> Integer
lookUp x [] = error "Variable not in enviroment"
lookUp x ((v,i):xs) | x == v = i
                    | otherwise = lookUp x xs

update :: Vars -> Integer -> Env -> Env
update var ass env = foldr (\(v,i) acc -> if v == var then (v,ass) : acc else (v,i):acc) [] env

inEnv :: Vars -> Env -> Bool
inEnv x [] = False
inEnv x ((v,i):xs) | x == v = True
                   | otherwise = inEnv x xs

evala :: Env -> AExpr -> Integer
evala env (Var str) = lookUp str env
evala env (Const int) = int
evala env (Add e1 e2) = (evala env e1) + (evala env e2)
evala env (Sub e1 e2) = (evala env e1) - (evala env e2)
evala env (Mul e1 e2) = (evala env e1) * (evala env e2)
evala env (Div e1 e2) = (evala env e1) `div` (evala env e2)

evalb :: Env -> BExpr -> Bool
evalb env TT = True
evalb env FF = False
evalb env (And b1 b2) = (evalb env b1) && (evalb env b2)
evalb env (Or b1 b2) = (evalb env b1) || (evalb env b2)
evalb env (Not b1) = not (evalb env b1)
evalb env (Eql e1 e2) = (evala env e1) == (evala env e2)
evalb env (Lt e1 e2) = (evala env e1) < (evala env e2)

exec :: Instr -> Env -> Env
exec (Assign var e1) env | inEnv var env = update var (evala env e1) env
                         | otherwise = (var, (evala env e1)):env
exec (IfThenElse b1 i1 i2) env = if evalb env b1 then exec i1 env else exec i2 env
exec (While b1 i1) env = if evalb env b1 then 
                                          exec (While b1 i1) (exec i1 env)
                                         else env
exec (Do xs) env = foldl (\acc x -> exec x acc) env xs
exec Nop env = env

run :: Program -> Env
run p = exec (Do p) [ ]

sum100 :: Program
sum100 = [
  Assign "X" (Const 0) ,
  Assign "C" (Const 1) ,
  While (Lt (Var "C") (Const 101)) 
  (Do [Assign "X" (Add (Var "X") (Var "C")) ,
  Assign "C" (Add (Var "C") (Const 1))] 
  ) ]
  
data UOps = NotOp deriving Show

data BOps = AddOp | SubOp | MulOp | DivOp | AndOp | OrOp | EqlOp | LtOp | AssignOp
  deriving Show

data Token = VSym String | CSym Integer | BSym Bool
 | UOp UOps | BOp BOps
 | LPar | RPar | LBra | RBra | Semi
 | Keyword String
 | Err
 | PA AExpr | PB BExpr | PI Instr | PDo [Instr]
  deriving Show

preproc :: String -> String
preproc [] = []
preproc ('T':xs) = " T " ++ preproc xs
preproc ('F':xs) = " F " ++ preproc xs
preproc ('+':xs) = " + " ++ preproc xs
preproc ('-':xs) = " - " ++ preproc xs
preproc ('*':xs) = " * " ++ preproc xs
preproc ('/':'\\':xs) = " /\\ " ++ preproc xs
preproc ('/':xs) = " / " ++ preproc xs
preproc ('\\':'/':xs) = " \\/ " ++ preproc xs
preproc ('!':xs) = " ! " ++ preproc xs
preproc ('=':'=':xs) = " == " ++ preproc xs
preproc ('<':xs) = " < " ++ preproc xs
preproc (':':'=':xs) = " := " ++ preproc xs
preproc ('(':xs) = " ( " ++ preproc xs
preproc (')':xs) = " ) " ++ preproc xs
preproc ('{':xs) = " { " ++ preproc xs
preproc ('}':xs) = " } " ++ preproc xs
preproc (';':xs) = " ; " ++ preproc xs
preproc (x:xs) = x : preproc xs

classify :: String -> Token
classify "T" = BSym True
classify "F" = BSym False
classify "+" = BOp AddOp
classify "-" = BOp SubOp  
classify "*" = BOp MulOp
classify "/" = BOp DivOp
classify "/\\" = BOp AndOp
classify "\\/" = BOp OrOp
classify "!" = UOp NotOp
classify "==" = BOp EqlOp
classify "<" = BOp LtOp
classify ":=" = BOp AssignOp
classify "(" = LPar
classify ")" = RPar
classify "{" = LBra
classify "}" = RBra
classify ";" = Semi
classify "while" = Keyword "while"
classify "if" = Keyword "if"
classify "then" = Keyword "then"
classify "else" = Keyword "else"
classify "nop" = Keyword "nop"
classify s | isVSym s = VSym s
classify s | isInt s = CSym (read s::Integer)
classify _ = Err

isVSym :: String -> Bool
isVSym "" = False
isVSym (x:xs) = isLower x && q1 xs
  where q1 "" = True
        q1 (y:ys) = (isAlpha y || isDigit y) && q1 ys

isInt :: String -> Bool
isInt "" = False
isInt (x:xs) = isDigit x && q1 xs
  where q1 "" = True
        q1 (y:ys) = (isDigit y) && q1 ys

lexer :: String -> [Token]
lexer s = map classify (words (preproc s))

parser :: [Token] -> [Instr]
parser ts = map getInstr $ sr [] (LBra : ts ++ [RBra])
  where getInstr (PI i) = i
        getInstr _ = error "The list returned by the parser contains non-instructions"

sr :: [Token] -> [Token] -> [Token]
sr (VSym v : stack) input = sr (PA (Var v) : stack) input
sr (CSym c : stack) input = sr (PA (Const c) : stack) input
sr (BSym False : stack) input = sr ((PB FF) : stack) input
sr (BSym True: stack) input = sr ((PB TT) : stack) input
sr (PA e2 : BOp MulOp : PA e1 : stack) input = sr (PA (Mul e1 e2) : stack) input
sr (PA e2 : BOp DivOp : PA e1 : stack) input = sr (PA (Div e1 e2) : stack) input
sr (PA e2 : BOp SubOp : PA e1 : stack) input = sr (PA (Sub e1 e2) : stack) input
sr (PA e2 : BOp AddOp : PA e1 : stack) input = sr (PA (Add e1 e2) : stack) input
sr (PA a2 : BOp EqlOp : PA a1 : stack)  input = sr (PB (Eql a1 a2) : stack) input -- BEXpr -> AExpr == AExpr
sr (PA a2 : BOp LtOp : PA a1 : stack)  input  = sr (PB (Lt a1 a2) : stack) input -- BExpr -> AExpr (BOp LtOp) AExpr
sr (PB b2 : BOp OrOp : PB b1 : stack)  input  = sr (PB (Or b1 b2) : stack) input     -- BEXpr -> BExpr \/ BExpr
sr (PB b2 : BOp AndOp : PB b1 : stack)  input = sr (PB (And b1 b2) : stack) input     
sr (PB b : UOp NotOp : stack) input              = sr (PB (Not b) : stack) input   -- BExpr -> Not BExpr
sr (PA a : BOp AssignOp : PA (Var c) : stack) input = sr (PI (Assign c a) : stack) input -- Instr -> Var AssignOp AExpr
sr (PI i : PB b : Keyword "while" : stack) input = sr (PI (While b i) : stack) input -- Instr -> While BExpr Instr
sr (PI i2 : Keyword "else" : PI i1 : Keyword "then" : PB b : Keyword "if" : stack) input = sr (PI (IfThenElse b i1 i2) : stack) input -- Instr -> While BExpr Instr
sr (Keyword "nop" : stack) input = sr (PI Nop : stack) input
sr (RPar : PA e : LPar : stack)        input = sr (PA e : stack) input -- AExpr -> ( AExpr )
sr (RPar : PB e : LPar : stack)        input = sr (PB e : stack) input
sr (RBra : PI i : stack) input = sr (PDo [i] : stack) input
sr (RBra : stack) input = sr (PDo [] : stack) input
sr (PDo s : Semi : PI i : stack) input = sr (PDo (i:s) : stack) input
sr (PDo s : LBra : stack) input = sr (PI (Do s) : stack) input
sr  stack (i:input) = sr (i:stack) input
sr stack [] = stack

main :: IO ()
main = do
  putStrLn "Enter filename"
  filename <- getLine
  contents <- readFile filename
  let lex = lexer contents
  let parsed = parser lex
  let out = run parsed
  putStrLn (show out)
 
