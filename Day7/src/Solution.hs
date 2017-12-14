module Solution (signal, run, parseInput, readInput, part1, part2)  where

import Data.Bits ((.&.), (.|.), shiftL, shiftR, complement)
import Data.Char (isLower)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Word (Word16)

import Parser


type Input = String

type Program = [Assignment]

data Assignment =
  Assignment
  { targetSignal :: SignalId
  , expression   :: Expression
  } deriving Show


data Expression
  = Val Value
  | Unary UnaryOperator
  | Binary BinaryOperator
  deriving Show


data UnaryOperator
  = Not Value
  deriving Show


data BinaryOperator
  = And Value Value
  | Or Value Value
  | LShift Value Int
  | RShift Value Int
  deriving Show


data Value
  = Word Word16
  | Sig  SignalId
  deriving Show


type Result = Environment

type SignalId = String

type Environment = Map SignalId Word16


part1 :: Input -> Word16
part1 inp =
  let prg = parseInput inp
      res = run prg
  in signal res "a"


part2 :: Input -> ()
part2 inp = ()


run :: Program -> Result
run prg = fixpoint (flip runProgram prg) Map.empty
  where
    fixpoint f x =
      let x' = f x
      in if x == x' then x' else fixpoint f x'


signal :: Result -> SignalId -> Word16
signal env = fromMaybe 0 . getValue env


runProgram :: Environment -> Program -> Environment
runProgram = foldl' runAssignment


runAssignment :: Environment -> Assignment -> Environment
runAssignment env (Assignment sigT exp) =
  case evalExpr env exp of
    Nothing  -> env
    Just val -> setValue env sigT val


evalExpr :: Environment -> Expression -> Maybe Word16
evalExpr env (Val v) = evalValue env v
evalExpr env (Unary (Not si)) = complement <$> evalValue env si
evalExpr env (Binary (And sa sb)) = (.&.) <$> evalValue env sa <*> evalValue env sb
evalExpr env (Binary (Or sa sb)) = (.|.) <$> evalValue env sa <*> evalValue env sb
evalExpr env (Binary (LShift sa n)) = shiftL <$> evalValue env sa <*> pure n
evalExpr env (Binary (RShift sa n)) = shiftR <$> evalValue env sa <*> pure n


evalValue :: Environment -> Value -> Maybe Word16
evalValue _   (Word i) = Just i
evalValue env (Sig s)  = getValue env s


setValue :: Environment -> SignalId -> Word16 -> Environment
setValue env sig val = Map.insert sig val env


getValue :: Environment -> SignalId -> Maybe Word16
getValue = flip Map.lookup


readInput :: IO Input
readInput = readFile "input.txt"


parseInput :: Input -> Program
parseInput = mapMaybe (eval assignmentP) . lines


assignmentP :: Parser Assignment
assignmentP = do
  exp <- expressionP <* ignoreWhiteSpace
  parseString "-> "
  sig <- signalP
  return $ Assignment sig exp


expressionP :: Parser Expression
expressionP = parseOneOf [ Binary <$> binaryOpP, Unary <$> unaryOpP, Val <$> valueP ]


unaryOpP :: Parser UnaryOperator
unaryOpP = parseOneOf [ notP ]


notP :: Parser UnaryOperator
notP = Not <$> (parseString "NOT " *> valueP)


lshiftP :: Parser BinaryOperator
lshiftP = LShift <$> valueP <*> (parseString "LSHIFT " *> parseInt)


rshiftP :: Parser BinaryOperator
rshiftP = RShift <$> valueP <*> (parseString "RSHIFT " *> parseInt)


binaryOpP :: Parser BinaryOperator
binaryOpP = parseOneOf [ andP, orP, lshiftP, rshiftP ]


andP :: Parser BinaryOperator
andP = And <$> (valueP <* parseString "AND ") <*> valueP


orP :: Parser BinaryOperator
orP = Or <$> (valueP <* parseString "OR ") <*> valueP



signalP :: Parser SignalId
signalP = do
  c <- parsePred isLower
  cs <- parseMany (parsePred isLower)
  ignoreWhiteSpace
  return (c:cs)


valueP :: Parser Value
valueP = parseOneOf [ sigP, wordP ]


sigP :: Parser Value
sigP = Sig <$> signalP


wordP :: Parser Value
wordP = Word <$> parseInt
