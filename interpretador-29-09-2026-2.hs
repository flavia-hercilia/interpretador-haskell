-- stack ghci --package silently --package containers --package HUnit interpretador-25-04-26.hs

module Interp where

import Data.Map (Map)
import qualified Data.Map as Map
import Test.HUnit
import System.IO.Silently (capture_)

-- Map é usado para armazenar os identificadores do programa
-- Funcões utilizadas: Map.empty, Map.insert e Map.lookup

-- ============================================================
-- Tipos da arvore sintatica 
-- ============================================================

data Type
  = TInt
  | TFloat
  | TBool
  | TVoid
  | TArray Type
  deriving (Show, Eq)

data BinOp
  = Add | Sub | Mul | Div | Mod
  | Eq  | Neq | Lt  | Gt  | Le | Ge
  | And | Or
  deriving (Show, Eq)

data Expr
  = Lit   Int
  | FLit  Float
  | BLit  Bool
  | Var   String
  | BinE  BinOp Expr Expr
  | Not   Expr
  | Call  String [Expr]
  | ArrayLit [Expr]
  | Index String Expr
  deriving (Show, Eq)

data Stmt
  = Decl   Type String Expr    -- int x = 10; 
  | Assign String Expr         -- x = 20 + 10
  | If     Expr Stmt Stmt
  | While  Expr Stmt
  | Block  [Stmt]
  | Return Expr                -- return x + y
  | Print  Expr                -- printf (10)
  | Skip
  | Switch Expr [(Expr, Stmt)] Stmt
  | ArrayAssign String Expr Expr
  | For Stmt Expr Stmt Stmt
  deriving (Show, Eq)

data FunDecl = FunDecl
  { funName   :: String
  , funParams :: [(Type, String)]-- [(TInt, "x"), (TInt,"y")]
  , funBody   :: Stmt
  } deriving (Show, Eq)

-- ============================================================
-- Valores e estado do programa (ambiente)
-- ============================================================

-- programas lidam com inteiros e booleanos
data Value
  = VInt  Int
  | VFloat  Float
  | VBool Bool
  | VVoid
  | VArray [Value]
  deriving (Show, Eq)

-- variaveis no escopo do programa
type Env    = Map String Value

-- funções no escopo do programa
type FunEnv = Map String FunDecl

-- ============================================================
-- Resultado da execução de um Stmt
-- ============================================================

data StmtResult
  = Next     Env
  | Returned Value Env
  deriving (Show)

-- ============================================================
-- Avaliação de Expressões 
-- ============================================================

-- refatoração:
-- função auxiliar que força a avaliação da função estritamente da esquerda para a direita
evalArgs :: FunEnv -> Env -> [Expr] -> IO [Value]
evalArgs fenv env [] = return []
evalArgs fenv env (exprAtual : restoExprs) = do
  valorAtual <- evalExpr fenv env exprAtual
  valorRestante <- evalArgs fenv env restoExprs 
  return (valorAtual : valorRestante)

evalExpr :: FunEnv -> Env -> Expr -> IO Value
evalExpr fenv env expr = case expr of

  Lit  n -> return (VInt n)
  BLit b -> return (VBool b)
  FLit m -> return (VFloat m)


  Var x  -> case Map.lookup x env of
               Nothing -> error ("Variavel nao declarada: " ++ x)
               Just v  -> return v

  Not e  -> do
    v <- evalExpr fenv env e
    case v of
      VBool b -> return (VBool (not b))
      _       -> error "Not aplicado a nao-booleano"

  BinE op l r -> do
    vl <- evalExpr fenv env l
    vr <- evalExpr fenv env r
    return (evalBinOp op vl vr)

  Call fname args ->
    case Map.lookup fname fenv of
      Nothing  -> error ("Funcao nao declarada: " ++ fname)
      Just fun -> do
        let nparams = length (funParams fun)
            nargs   = length args
        if nparams /= nargs
          then error ("Funcao " ++ fname ++ " espera " ++
                      show nparams ++ " argumentos, recebeu " ++
                      show nargs)
          else do
            vals <- evalArgs fenv env args 
            let params   = map snd (funParams fun)
                envArgs = Map.fromList (zip params vals)
                -- map.union une as variveis locais com as globais
                envLocal = Map.union envArgs env
            result <- execStmt fenv envLocal (funBody fun)
            case result of
              Returned v _ -> return v
              Next _       -> return VVoid

  ArrayLit exprs -> do
    vals <- mapM (evalExpr fenv env) exprs
    return (VArray vals)

  Index x idxExpr -> do
    idx <- evalExpr fenv env idxExpr
    arr <- case Map.lookup x env of
      Nothing -> error ("Variavel nao declarada: " ++ x)
      Just v  -> return v
    case (arr, idx) of
      (VArray elems, VInt i)
        | i >= 0 && i < length elems -> return (elems !! i)
        | otherwise -> error "Indice de array fora dos limites"
      _ -> error "Acesso a indice invalido ou tipo nao e array/inteiro"

-- ============================================================
-- evalBinOp — permanece pura
-- ============================================================

evalBinOp :: BinOp -> Value -> Value -> Value
evalBinOp Add (VInt  a) (VInt  b) = VInt  (a + b)
evalBinOp Sub (VInt  a) (VInt  b) = VInt  (a - b)
evalBinOp Mul (VInt  a) (VInt  b) = VInt  (a * b)
evalBinOp Div (VInt  a) (VInt  b) = VInt  (a `div` b)
evalBinOp Mod (VInt  a) (VInt  b) = VInt  (a `mod` b)
evalBinOp Eq  va        vb        = VBool (va == vb)
evalBinOp Neq va        vb        = VBool (va /= vb)
evalBinOp Lt  (VInt  a) (VInt  b) = VBool (a <  b)
evalBinOp Gt  (VInt  a) (VInt  b) = VBool (a >  b)
evalBinOp Le  (VInt  a) (VInt  b) = VBool (a <= b)
evalBinOp Ge  (VInt  a) (VInt  b) = VBool (a >= b)
evalBinOp And (VBool a) (VBool b) = VBool (a && b)
evalBinOp Or  (VBool a) (VBool b) = VBool (a || b)

evalBinOp Add (VFloat a) (VFloat b) = VFloat (a + b)
evalBinOp Add (VFloat a) (VInt b)   = VFloat (a + fromIntegral b)
evalBinOp Add (VInt a)   (VFloat b) = VFloat (fromIntegral a + b)
evalBinOp Sub (VFloat a) (VFloat b) = VFloat (a - b)
evalBinOp Sub (VFloat a) (VInt b)   = VFloat (a - fromIntegral b)
evalBinOp Sub (VInt a)   (VFloat b) = VFloat (fromIntegral a - b)
evalBinOp Mul (VFloat a) (VFloat b) = VFloat (a * b)
evalBinOp Mul (VFloat a) (VInt b)   = VFloat (a * fromIntegral b)
evalBinOp Mul (VInt a)   (VFloat b) = VFloat (fromIntegral a * b)
evalBinOp Div (VFloat a) (VFloat b) = VFloat (a / b)
evalBinOp Div (VFloat a) (VInt b)   = VFloat (a / fromIntegral b)
evalBinOp Div (VInt a)   (VFloat b) = VFloat (fromIntegral a / b)
evalBinOp Lt  (VFloat a) (VFloat b) = VBool (a < b)
evalBinOp Lt  (VFloat a) (VInt b)   = VBool (a < fromIntegral b)
evalBinOp Lt  (VInt a)   (VFloat b) = VBool (fromIntegral a < b)
evalBinOp Gt  (VFloat a) (VFloat b) = VBool (a > b)
evalBinOp Gt  (VFloat a) (VInt b)   = VBool (a > fromIntegral b)
evalBinOp Gt  (VInt a)   (VFloat b) = VBool (fromIntegral a > b)
evalBinOp Le  (VFloat a) (VFloat b) = VBool (a <= b)
evalBinOp Le  (VFloat a) (VInt b)   = VBool (a <= fromIntegral b)
evalBinOp Le  (VInt a)   (VFloat b) = VBool (fromIntegral a <= b)
evalBinOp Ge  (VFloat a) (VFloat b) = VBool (a >= b)
evalBinOp Ge  (VFloat a) (VInt b)   = VBool (a >= fromIntegral b)
evalBinOp Ge  (VInt a)   (VFloat b) = VBool (fromIntegral a >= b)


evalBinOp op  _         _         =
  error ("Operacao invalida ou tipos incompativeis: " ++ show op)

-- ============================================================
-- Execução de Statements
-- ============================================================

execStmt :: FunEnv -> Env -> Stmt -> IO StmtResult
execStmt fenv env stmt = case stmt of

  Skip ->
    return (Next env)

  Print e -> do
    v <- evalExpr fenv env e
    putStrLn (showValue v)
    return (Next env)

  Decl _ x e -> do
    v <- evalExpr fenv env e
    let env' = Map.insert x v env
    return (Next env')

  Assign x e ->
    case Map.lookup x env of
      Nothing -> error ("Variavel nao declarada: " ++ x)
      Just valorOriginal  -> do
        v <- evalExpr fenv env e
        case (valorOriginal, v) of

          (VInt _, VInt _)       -> do
            let env' = Map.insert x v env
            return (Next env')

          (VInt _, VFloat f)     -> do
            let vConvertido = VInt (truncate f)
            let env' = Map.insert x vConvertido env
            return (Next env')

          (VFloat _, VFloat _)   -> do
            let env' = Map.insert x v env
            return (Next env')

          (VFloat _, VInt n)     -> do
            let vConvertido = VFloat (fromIntegral n)
            let env' = Map.insert x vConvertido env
            return (Next env')

          (VBool _, VBool _)     -> do
            let env' = Map.insert x v env
            return (Next env')

          _                      -> error "Tipos incompativeis"
      
  If cond t f -> do
    v <- evalExpr fenv env cond
    case v of
      VBool True  -> execStmt fenv env t
      VBool False -> execStmt fenv env f
      _           -> error "Condicao do If nao eh booleana"

  Return e -> do
    v <- evalExpr fenv env e
    return (Returned v env)

  Block stmts ->
    execBlock fenv env stmts

  ArrayAssign x idxExpr valExpr -> do
    idx <- evalExpr fenv env idxExpr
    val <- evalExpr fenv env valExpr
    arr <- case Map.lookup x env of
      Nothing -> error ("Variavel nao declarada: " ++ x)
      Just v  -> return v
    case (arr, idx) of
      (VArray elems, VInt i)
        | i >= 0 && i < length elems -> do
            let newElems = take i elems ++ [val] ++ drop (i + 1) elems
            return (Next (Map.insert x (VArray newElems) env))
        | otherwise -> error "Indice de array fora dos limites"
      _ -> error "Atribuicao invalida em array"

  While cond body -> do
    v <- evalExpr fenv env cond
    case v of
      VBool False -> return (Next env)
      VBool True  -> do
        r <- execStmt fenv env body
        case r of
          Returned v' env' -> return (Returned v' env')
          Next        env' -> execStmt fenv env' (While cond body)
      _ -> error "Condicao do While nao eh booleana"

  Switch expr casos comandoDefault -> do
    v <- evalExpr fenv env expr

    let procuraCaso alvo [] = execStmt fenv env comandoDefault
        procuraCaso alvo ((exprAtual, comandoAtual) : restoDaLista) = do
          valorAtual <- evalExpr fenv env exprAtual
          if valorAtual == alvo
            then execStmt fenv env comandoAtual
            else procuraCaso alvo restoDaLista
    procuraCaso v casos

  For inicio cond passo body -> do
    resultadoInicio <- execStmt fenv env inicio
    case resultadoInicio of
      Returned v env' -> return (Returned v env')
      Next envInicial -> do
        let repeteLaco envAtual = do
              v <- evalExpr fenv envAtual cond
              case v of
                VBool False -> return (Next envAtual)
                VBool True  -> do
                  r <- execStmt fenv envAtual body
                  case r of
                    Returned v' env' -> return (Returned v' env')
                    Next        env' -> do
                      resultadoPasso <- execStmt fenv env' passo
                      case resultadoPasso of
                        Returned v'' env'' -> return (Returned v'' env'')
                        Next         env'' -> repeteLaco env''
                _ -> error "Condicao do For nao eh booleana"
        repeteLaco envInicial
 
-- ============================================================
-- Execução de um bloco
-- ============================================================

execBlock :: FunEnv -> Env -> [Stmt] -> IO StmtResult
execBlock _    env []     = return (Next env)
execBlock fenv env (s:ss) = do
  r <- execStmt fenv env s
  case r of
    Returned v env' -> return (Returned v env')
    Next       env' -> execBlock fenv env' ss

-- ============================================================
-- Utilitários
-- ============================================================

showValue :: Value -> String
showValue (VInt  n) = show n
showValue (VBool b) = if b then "verdadeiro" else "falso"
showValue VVoid     = "()"
showValue (VFloat  m) = show m
showValue (VArray xs) = "["++ showArrayElems xs ++ "]"
  where 
    showArrayElems [] = ""
    showArrayElems [x] = showValue x
    showArrayElems (x:xs) = showValue x ++ ", " ++ showArrayElems xs

  



-- ============================================================
-- Auxiliares para testes
-- ============================================================

-- Executa um programa e extrai o valor retornado via Return
runProg :: FunEnv -> Stmt -> IO Value
runProg fenv prog = do
  result <- execStmt fenv Map.empty prog
  case result of
    Returned v _ -> return v
    Next _       -> return VVoid

-- Executa um programa e captura tudo que foi impresso via Print
runProgIO :: FunEnv -> Stmt -> IO String
runProgIO fenv prog =
  capture_ (execStmt fenv Map.empty prog)

-- ============================================================
-- Funções de exemplo
-- ============================================================

-- int dobro(int x) { retorna x * 2; }
funDobro :: FunDecl
funDobro = FunDecl
  { funName   = "dobro"
  , funParams = [(TInt, "x")]
  , funBody   = Return (BinE Mul (Var "x") (Lit 2))
  }

-- int fatorial(int n) {
--   se (n == 0) retorna 1
--   senao retorna n * fatorial(n - 1)
-- }
funFatorial :: FunDecl
funFatorial = FunDecl
  { funName   = "fatorial"
  , funParams = [(TInt, "n")]
  , funBody   =
      If (BinE Eq (Var "n") (Lit 0))
         (Return (Lit 1))
         (Return (BinE Mul
                    (Var "n")
                    (Call "fatorial" [BinE Sub (Var "n") (Lit 1)])))
  }

-- int soma(int a, int b) { retorna a + b; }
funSoma :: FunDecl
funSoma = FunDecl
  { funName   = "soma"
  , funParams = [(TInt, "a"), (TInt, "b")]
  , funBody   = Return (BinE Add (Var "a") (Var "b"))
  }

funSomaGlobal :: FunDecl
funSomaGlobal = FunDecl
  { funName   = "somaGlobal"
  , funParams = [(TInt, "y")]
  , funBody   = Return (BinE Add (Var "x") (Var "y"))
  }

fenvExemplo :: FunEnv
fenvExemplo = Map.fromList
  [ ("dobro",    funDobro)
  , ("fatorial", funFatorial)
  , ("soma",     funSoma)
  , ("somaGlobal", funSomaGlobal)
  ]

-- ============================================================
-- Programas de teste
-- ============================================================

-- int x = 10; int y = 3; retorna x + y;
prog1 :: Stmt
prog1 = Block
  [ Decl TInt "x" (Lit 10)
  , Decl TInt "y" (Lit 3)
  , Return (BinE Add (Var "x") (Var "y"))
  ]

-- int x = 10; float y = 3.5; retorna x + y;
prog9 :: Stmt
prog9 = Block
  [Decl TInt "x" (Lit 10)
  , Decl TFloat "y" (FLit 3.5)
  , Return (BinE Add (Var "x") (Var "y"))
  ]  

-- float x = 2.5; int y = 8; retorna x + y;
prog10 :: Stmt
prog10 = Block
  [Decl TFloat "x" (FLit 2.5)
  , Decl TInt "y" (Lit 8)
  , Return (BinE Add (Var "x") (Var "y"))
  ] 

-- int x = 7; float y = 2.0; retorna x / y;
prog11 :: Stmt
prog11 = Block
  [ Decl TInt "x" (Lit 7)
  , Decl TFloat "y" (FLit 2.0)
  , Return (BinE Div (Var "x") (Var "y"))
  ]

-- int n = 7; se (n > 5) retorna verdadeiro senao retorna falso
prog2 :: Stmt
prog2 = Block
  [ Decl TInt "n" (Lit 7)
  , If (BinE Gt (Var "n") (Lit 5))
       (Return (BLit True))
       (Return (BLit False))
  ]

-- float m = 4.5; se (m < 5) retorna verdadeiro senao retorna falso
prog12 :: Stmt
prog12 = Block
  [ Decl TFloat "m" (FLit 4.5)
  , If (BinE Lt (Var "m") (Lit 5))
       (Return (BLit True))
       (Return (BLit False))
  ]

-- float x = 14.25; float y = 1.5; retorna x / y;
prog13 :: Stmt
prog13 = Block
  [ Decl TFloat "x" (FLit 14.25)
  , Decl TFloat "y" (FLit 1.5)
  , Return (BinE Div (Var "x") (Var "y"))
  ]

-- int x = 7; int y = 2; retorna x / y;
prog14 :: Stmt
prog14 = Block
  [ Decl TInt "x" (Lit 7)
  , Decl TInt "y" (Lit 2)
  , Return (BinE Div (Var "x") (Var "y"))
  ]

-- int i = 0; enquanto (i < 3) { i = i + 1; } retorna i;
prog3 :: Stmt
prog3 = Block
  [ Decl TInt "i" (Lit 0)
  , While (BinE Lt (Var "i") (Lit 3))
      (Assign "i" (BinE Add (Var "i") (Lit 1)))
  , Return (Var "i")
  ]

-- retorna dobro(5)
prog4 :: Stmt
prog4 = Return (Call "dobro" [Lit 5])

-- retorna fatorial(5)
prog5 :: Stmt
prog5 = Return (Call "fatorial" [Lit 5])

-- int x = 4; retorna soma(dobro(x), 3)
prog6 :: Stmt
prog6 = Block
  [ Decl TInt "x" (Lit 4)
  , Return (Call "soma" [Call "dobro" [Var "x"], Lit 3])
  ]

-- int i = 0; enquanto (i < 3) { imprime(i); i = i + 1; }
prog7 :: Stmt
prog7 = Block
  [ Decl TInt "i" (Lit 0)
  , While (BinE Lt (Var "i") (Lit 3))
      (Block
        [ Print (Var "i")
        , Assign "i" (BinE Add (Var "i") (Lit 1))
        ])
  ]

-- imprime(dobro(4))
prog8 :: Stmt
prog8 = Print (Call "dobro" [Lit 4])

-- imprime(2.5)
prog15 :: Stmt
prog15 = Print (FLit 2.5)

-- retorna 300
prog16 :: Stmt
prog16 = Block
  [ Decl TInt "x" (Lit 100)
  , Return (Call "somaGlobal" [Lit 200])
  ]

-- retorna erro de tipos incompativeis
prog17 :: Stmt
prog17 = Block
  [ Decl TInt "x" (Lit 10)
  , Assign "x" (BLit True)
  , Return (Var "x")
  ]

-- retorna 4
prog18 :: Stmt 
prog18 = Block
  [ Decl TInt "x" (Lit 10)
  , Assign "x" (FLit 4.5)
  , Return (Var "x")
  ]

-- retorna 10.0
prog19 :: Stmt 
prog19 = Block
  [ Decl TFloat "x" (FLit 4.5)
  , Assign "x" (Lit 10)
  , Return (Var "x")
  ]

prog20 :: Stmt
prog20 = Block
  [ Decl TInt "x" (Lit 2)
  , Decl TInt "y" (Lit 0)
  , Switch(Var "x")
      [ (Lit 1, Assign "y" (Lit 10))
      , (Lit 2, Assign "y" (Lit 20)) 
      ]
      (Assign "y" (Lit 30))
  , Return (Var "y")
  ]

-- int[] v = {2, 8, 24}, retorna v[1]
prog21 :: Stmt
prog21 = Block
  [ Decl (TArray TInt) "v" (ArrayLit [Lit 2, Lit 8, Lit 24])
  , Return (Index "v" (Lit 1))
  ]

-- int[] v = {10, 20, 30} e v[0] = 99; retorna v[0];
prog22 :: Stmt
prog22 = Block
  [ Decl (TArray TInt) "v" (ArrayLit [Lit 10, Lit 20, Lit 30])
  , ArrayAssign "v" (Lit 0) (Lit 99)
  , Return (Index "v" (Lit 0))
  ]

-- int[] v = {1, 2, 3} e imprime(v);
prog23 :: Stmt
prog23 = Block
  [ Decl (TArray TInt) "v" (ArrayLit [Lit 1, Lit 2, Lit 3])
  , Print (Var "v")
  ]

-- float[] v = {1.5, 2.5} e retorna v[1];
prog24 :: Stmt
prog24 = Block
  [ Decl (TArray TFloat) "v" (ArrayLit [FLit 1.5, FLit 2.5])
  , Return (Index "v" (Lit 1))
  ]

-- bool[] v = {True, False} e retorna v[1];
prog25 :: Stmt
prog25 = Block
  [ Decl (TArray TBool) "v" (ArrayLit [BLit True, BLit False])
  , Return (Index "v" (Lit 1))
  ]

-- int soma = 0; for(int i = 1; i <= 3; i = i + 1) 
-- { soma = soma + i; } retorna soma (resultado: 6)
prog26 :: Stmt
prog26 = Block
  [ Decl TInt "soma" (Lit 0)
  , For (Decl TInt "i" (Lit 1)) 
        (BinE Le (Var "i") (Lit 3)) 
        (Assign "i" (BinE Add (Var "i") (Lit 1)))
        (Assign "soma" (BinE Add (Var "soma") (Var "i")))
  , Return (Var "soma")
  ]


-- ============================================================
-- Suite de testes HUnit
-- ============================================================

-- testes de valor retornado (usam runProg)

testSomaSimples :: Test
testSomaSimples = TestCase $ do
  v <- runProg Map.empty prog1
  assertEqual "soma 10 + 3" (VInt 13) v

testSomaIntFloat :: Test
testSomaIntFloat = TestCase $ do
  v <- runProg Map.empty prog9
  assertEqual "soma 10 + 3.5" (VFloat 13.5) v

testSomaFloatInt :: Test
testSomaFloatInt = TestCase $ do
  v <- runProg Map.empty prog10
  assertEqual "soma 2.5 + 8" (VFloat 10.5) v

testDivIntFloat :: Test
testDivIntFloat = TestCase $ do
  v <- runProg Map.empty prog11
  assertEqual "divisao de int float 7 / 2.0" (VFloat 3.5) v

testDivIntInt :: Test
testDivIntInt = TestCase $ do
  v <- runProg Map.empty prog14
  assertEqual "divisao de int 7 / 2" (VInt 3) v

testDivFloatFloat :: Test
testDivFloatFloat = TestCase $ do
  v <- runProg Map.empty prog13
  assertEqual "divisao de float 14.25 / 1.5" (VFloat 9.5) v

testCondicionalVerdade :: Test
testCondicionalVerdade = TestCase $ do
  v <- runProg Map.empty prog2
  assertEqual "7 > 5 eh verdadeiro" (VBool True) v

testComparaFloatInt :: Test
testComparaFloatInt = TestCase $ do
  v <- runProg Map.empty prog12
  assertEqual "4.5 < 5 eh verdadeiro" (VBool True) v

testWhileContaAte3 :: Test
testWhileContaAte3 = TestCase $ do
  v <- runProg Map.empty prog3
  assertEqual "while incrementa i ate 3" (VInt 3) v

testDobro :: Test
testDobro = TestCase $ do
  v <- runProg fenvExemplo prog4
  assertEqual "dobro de 5 eh 10" (VInt 10) v

testFatorial :: Test
testFatorial = TestCase $ do
  v <- runProg fenvExemplo prog5
  assertEqual "fatorial de 5 eh 120" (VInt 120) v

testChamadaAninhada :: Test
testChamadaAninhada = TestCase $ do
  v <- runProg fenvExemplo prog6
  assertEqual "soma(dobro(4), 3) eh 11" (VInt 11) v

testEscopoGlobal :: Test
testEscopoGlobal = TestCase $ do
  v <- runProg fenvExemplo prog16
  assertEqual "soma global de x (100) e y (200)" (VInt 300) v

testTipoIncompativel :: Test
testTipoIncompativel = TestCase $ do
  v <- runProg Map.empty prog17
  assertEqual "x eh 10" (VInt 10) v

testAssignIntFloat :: Test
testAssignIntFloat = TestCase $ do
  v <- runProg Map.empty prog18
  assertEqual "x eh 4" (VInt 4) v

testAssignFloatInt :: Test
testAssignFloatInt = TestCase $ do
  v <- runProg Map.empty prog19
  assertEqual "x eh 10.0" (VFloat 10.0) v

testSwitch :: Test
testSwitch = TestCase $ do
  v <- runProg Map.empty prog20
  assertEqual "y eh 20" (VInt 20) v

testLerPosicao :: Test
testLerPosicao = TestCase $ do
  v <- runProg Map.empty prog21
  assertEqual "ler posicao v[1]" (VInt 8) v

testEscreverPosicao :: Test
testEscreverPosicao = TestCase $ do
  v <- runProg Map.empty prog22
  assertEqual "escrever 99 em v[0]" (VInt 99) v

testArrayFloat :: Test
testArrayFloat = TestCase $ do
  v <- runProg Map.empty prog24
  assertEqual "array de float v[1]" (VFloat 2.5) v

testArrayBool :: Test
testArrayBool = TestCase $ do
  v <- runProg Map.empty prog25
  assertEqual "array de bool v[1]" (VBool False) v

-- testes de saída impressa (usam runProgIO)

testWhileImprime :: Test
testWhileImprime = TestCase $ do
  output <- runProgIO Map.empty prog7
  assertEqual "while imprime 0, 1, 2" "0\n1\n2\n" output

testImprimeCallDobro :: Test
testImprimeCallDobro = TestCase $ do
  output <- runProgIO fenvExemplo prog8
  assertEqual "imprime dobro(4)" "8\n" output
  
testImprimeFloat :: Test
testImprimeFloat = TestCase $ do
  output <- runProgIO Map.empty prog15
  assertEqual "imprime 2.5" "2.5\n" output

testImprimirArray :: Test
testImprimirArray = TestCase $ do
  output <- runProgIO Map.empty prog23
  assertEqual "imprimir array [1, 2, 3]" "[1, 2, 3]\n" output

testFor :: Test
testFor = TestCase $ do
  v <- runProg Map.empty prog26
  assertEqual "for soma de 1 ate 3" (VInt 6) v

-- suite completa

suite :: Test
suite = TestList
  [ TestLabel "soma simples"         testSomaSimples
  , TestLabel "soma int float"       testSomaIntFloat
  , TestLabel "soma float int"       testSomaFloatInt
  , TestLabel "condicional"          testCondicionalVerdade
  , TestLabel "compara float int"    testComparaFloatInt
  , TestLabel "while retorna"        testWhileContaAte3
  , TestLabel "chamada dobro"        testDobro
  , TestLabel "fatorial recursivo"   testFatorial
  , TestLabel "chamada aninhada"     testChamadaAninhada
  , TestLabel "while imprime"        testWhileImprime
  , TestLabel "imprime call dobro"   testImprimeCallDobro
  , TestLabel "divisao int float"    testDivIntFloat
  , TestLabel "divisao float float"  testDivFloatFloat
  , TestLabel "divisao int int"      testDivIntInt
  , TestLabel "imprime float"        testImprimeFloat
  , TestLabel "escopo global"        testEscopoGlobal
  , TestLabel "tipo incompativel"  testTipoIncompativel
  , TestLabel "assign int float"     testAssignIntFloat
  , TestLabel "assign float int"     testAssignFloatInt
  , TestLabel "switch case"          testSwitch
  , TestLabel "ler posicao"          testLerPosicao
  , TestLabel "escrever posicao"     testEscreverPosicao
  , TestLabel "imprimir array"       testImprimirArray
  , TestLabel "array de float"       testArrayFloat
  , TestLabel "array de bool"        testArrayBool
  , TestLabel "for soma"             testFor
  ]

main :: IO ()
main = runTestTTAndExit suite
