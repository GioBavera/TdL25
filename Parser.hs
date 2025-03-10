module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import AST

-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
                  whiteSpace lis
                  t <- p
                  eof
                  return t

-- Analizador de Tokens
-- Se agrearon los operadores, azucares y comandos nuevos a la lista de reservados.
lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentStart  = "/*"
                                  , commentEnd    = "*/"
                                  , commentLine   = "//"
                                  , reservedNames = ["true","false","skip","if",
                                                     "then","else","end", "case", "break", "switch", "default", "break_case", 
                                                     "while","do", "repeat", "print", "input"]
                                  , reservedOpNames = [  "+"
                                                       , "-"
                                                       , "*"
                                                       , "/"
                                                       , "<"
                                                       , ">"
                                                       , "&"
                                                       , "|"
                                                       , "="
                                                       , ";"
                                                       , "~"
                                                       , ":="
                                                       , "%="
                                                       , "^="
                                                       , "mod"
                                                       , "max"
                                                       , "min"
                                                       ]
                                   }
                                 )
----------------------------------
--- Parser de expressiones enteras
-----------------------------------
{--
chainl p op x
parsea 0 o más ocurrencias de p separadas por op
Retorna el valor que se obtiene al aplicar todas las
funciones retornadas por op a los valores retornados
por p

t := t + t | m
term = chainl factor (do {symbol "+"; return (+)})
factor = integer <|> parens term
--}
intexp :: Parser IntExp
intexp  = chainl1 term addopp

term = chainl1 factor multopp

factor = try (parens lis intexp)
         <|> try (do reservedOp lis "-"
                     f <- factor
                     return (UMinus f))
         <|> (do reservedOp lis "mod"                   -- "mod("x","y")"
                 parens lis (do x <- intexp
                                comma lis 
                                y <- intexp
                                return (Mod x y)))
         <|> (do reservedOp lis "max"                   -- "max("x","y")"
                 parens lis (do x <- intexp
                                comma lis 
                                y <- intexp
                                return (Max x y)))
         <|> (do reservedOp lis "min"                   -- "min("x","y")"
                 parens lis (do x <- intexp
                                comma lis 
                                y <- intexp
                                return (Min x y)))
         <|> (do n <- integer lis
                 return (Const n)
              <|> do str <- identifier lis
                     return (Var str))

multopp = do try (reservedOp lis "*")
             return Times
          <|> do try (reservedOp lis "/")
                 return Div

addopp = do try (reservedOp lis "+")
            return Plus
         <|> do try (reservedOp lis "-")
                return Minus

------------------------------------
--- Parser de expressiones booleanas
------------------------------------
boolexp :: Parser BoolExp
boolexp  = chainl1 boolexp2 (try (do reservedOp lis "|"
                                     return Or))

boolexp2 = chainl1 boolexp3 (try (do reservedOp lis "&"
                                     return And))

boolexp3 = try (parens lis boolexp)
           <|> try (do reservedOp lis "~"
                       b <- boolexp3
                       return (Not b))
           <|> intcomp
           <|> boolvalue

intcomp = try (do i <- intexp
                  c <- compopp
                  j <- intexp
                  return (c i j))

compopp = try (do reservedOp lis "="
                  return Eq)
          <|> try (do reservedOp lis "<"
                      return Lt)
          <|> try (do reservedOp lis ">"
                      return Gt)

boolvalue = try (do reserved lis "true"
                    return BTrue)
            <|> try (do reserved lis "false"
                        return BFalse)

-----------------------------------
--- Parser de comandos
-----------------------------------
{-comm :: Parser Comm           // Funcion orginal, descartada
comm = chainl1 comm2 (try (do reservedOp lis ";" 
                              return Seq)) -}
{-      sepEndBy1 comm2 (semi los)
Parse comandos separados por ';'. Si hay un ; del ultimo comm tambien lo lee. Devuelve Parser [Comm2]

        fmap (foldl1 Seq) ...
Lo que hace fmap es aplicar una funcion a los resultados de un parser. Aca se aplica 'foldl1 Seq'.
Esta ultima funcion toma una lista de valores y los agrupa secuencialmente con Seq.
-}
comm :: Parser Comm
comm = fmap (foldl1 Seq) $ sepEndBy1 comm2 (semi lis)

comm2 = try (do reserved lis "skip"
                return Skip)
        <|> try (do reserved lis "if"
                    cond <- boolexp
                    reserved lis "then"
                    case1 <- comm
                    reserved lis "else"
                    case2 <- comm
                    reserved lis "end"
                    return (Cond cond case1 case2))
        <|> try (do reserved lis "repeat"
                    c <- comm
                    reserved lis "until"
                    cond <- boolexp
                    reserved lis "end"
                    return (Repeat c cond))
        <|> try (do str <- identifier lis
                    reservedOp lis ":="
                    e <- intexp
                    return (Let str e))
--- ======================================================================
------- Extension del lenguaje
        <|> try (do str <- identifier lis       -- Azucar de mod
                    reservedOp lis "%="
                    e <- intexp
                    return (Let str (Mod (Var str) e)))  -- x %= y → x = x % y
        <|> try (do str <- identifier lis       -- Azucar de max
                    reservedOp lis "^="
                    e <- intexp
                    return (Let str (Max (Var str) e)))  -- x ^= y → x = x ^ y

--- Entrada y Salida
        <|> try (do reserved lis "print"
                    a <- parens lis (sepBy1 ((do b <- intexp
                                                 return (Entero b))
                                         <|> (do b <- boolexp
                                                 return (Booleano b))
                                         <|> (do s <- stringLiteral lis
                                                 return (Str s))
                                        ) (comma lis))
                    return (Print a))           -- Ej: return (Print [Str "Hola", Entero 42, Booleano True])
        <|> try (do reserved lis "input"
                    parens lis ((do b <- identifier lis
                                    return (Input "" b))
                                <|> (do s <- stringLiteral lis
                                        comma lis 
                                        v <- identifier lis
                                        return (Input s v))))   -- Ej: return (Input "" v) o return (Input "Numero: " v)
--- Condicional Switch
        <|> try (do reserved lis "switch"
                    v <- parens lis (identifier lis)   
                    c <- braces lis cases
                    return (Switch v c))

cases = do 
          reserved lis "case"           -- case
          i <- integer lis              -- i
          colon lis                     -- :
          (try (do 
                 c <- comm
                 cs <- cases
                 return (Case i : c : cs))
           <|> do 
                cs <- cases
                return (Case i : cs))
    <|> do
          reserved lis "break_case"
          semi lis
          cs <- cases 
          return (BreakCase : cs) 
    <|> do
          reserved lis "default"
          colon lis
          cs <- comm
          return (Default : [cs])
    <|> return []
              
------------------------------------
-- Funcion de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
