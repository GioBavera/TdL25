module AST where

-- Identificadores de Variable
type Variable = String

-- Expresiones Aritmeticas
data IntExp = Const Integer
            | Var Variable
            | UMinus IntExp
            | Plus IntExp IntExp
            | Minus IntExp IntExp
            | Times IntExp IntExp
            | Div IntExp IntExp
            | Question BoolExp IntExp IntExp
            | Mod IntExp IntExp     -- Operador Modulo
            | Max IntExp IntExp     -- Operador Maximo entre dos
            | Min IntExp IntExp     -- Operador Maximo entre dos
 deriving (Show,Eq)

-- Expresiones Booleanas
data BoolExp = BTrue
             | BFalse
             | Eq IntExp IntExp
             | Lt IntExp IntExp
             | Gt IntExp IntExp
             | And BoolExp BoolExp
             | Or BoolExp BoolExp
             | Not BoolExp
 deriving (Show,Eq)

-- Comandos (sentencias)
-- Observar que solo se permiten variables de un tipo (entero)
data Comm = Skip
          | Let Variable IntExp
          | Seq Comm Comm
          | Cond BoolExp Comm Comm
          | Repeat Comm BoolExp
          | Print [Printarg]            -- Imprimir en pantalla, tiene un array de strings, bools, enteros
          | Input String Variable       -- Ingresa variable, permite opcionalmente un mensaje previo
          | Switch Variable [Comm]      -- Trabaja con una lista de comandos, previamente pide una variable
          | Case Integer                -- Caso dentro del switch y su valor para comparar
          | BreakCase                   -- Detener la ejecucion del switch
          | Default                     -- Caso final
 deriving (Show,Eq)

data Printarg = Str String | Booleano BoolExp | Entero IntExp
 deriving (Show, Eq)
