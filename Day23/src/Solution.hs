module Solution (readInput, part1, part2)  where


import Control.Monad.State as S
import Control.Monad.Reader as R
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromMaybe, fromJust, mapMaybe)
import Parser


type Input = Program

type Computation a = R.ReaderT Program (State Environment) a

data Environment
  = Env
  { registerA :: Int
  , registerB :: Int
  , instructionPointer :: Int
  }

type Program = IM.IntMap Instruction

data Instruction
  = Half Register
  | Tripple Register
  | Increment Register
  | Jump Offset
  | JumpIfEven Register Offset
  | JumpIfOne Register Offset
  | Halt
  deriving Show

type Offset = Int

data Register = RegA | RegB
  deriving Show


part1 :: Input -> Int
part1 = registerB . execute 0


part2 :: Input -> Int
part2 = registerB . execute 1


execute :: Int -> Program -> Environment
execute a prg = flip S.execState startState $ R.runReaderT run prg
  where startState = Env a 0 0


run :: Computation ()
run = do
  inst <- readInstruction
  case inst of
    Half reg           -> modifyRegister reg (`div` 2) >> next 1
    Tripple reg        -> modifyRegister reg (* 3)     >> next 1
    Increment reg      -> modifyRegister reg (+ 1)     >> next 1
    Jump off           -> next off
    JumpIfEven reg off -> getRegister reg >>= \v -> if even v then next off else next 1
    JumpIfOne reg off  -> getRegister reg >>= \v -> if v == 1 then next off else next 1
    Halt               -> return ()
  where next off = do
          moveIP off
          run


readInstruction :: Computation Instruction
readInstruction = do
  ip <- S.gets instructionPointer
  fromMaybe Halt <$> R.asks (IM.lookup ip)


moveIP :: Offset -> Computation ()
moveIP off = S.modify' (\e -> e { instructionPointer = instructionPointer e + off })


getRegister :: Register -> Computation Int
getRegister RegA = S.gets registerA
getRegister RegB = S.gets registerB


setRegister :: Register -> Int -> Computation ()
setRegister reg val = modifyRegister reg (const val)


modifyRegister :: Register -> (Int -> Int) -> Computation ()
modifyRegister RegA upd = S.modify' (\e -> e { registerA = upd $ registerA e })
modifyRegister RegB upd = S.modify' (\e -> e { registerB = upd $ registerB e })


readInput :: IO Input
readInput =
  IM.fromList .
  zip [0..] .
  map (fromJust . eval instructionP) . lines
  <$> readFile "input.txt"


instructionP :: Parser Instruction
instructionP = parseOneOf [ halfP, trippleP, incrP, jumpP, jieP, jioP ]
  where halfP = Half <$> (parseString "hlf " *> regP)
        trippleP = Tripple <$> (parseString "tpl " *> regP)
        incrP = Increment <$> (parseString "inc " *> regP)
        jumpP = Jump <$> (parseString "jmp " *> offP)
        jieP = JumpIfEven <$> (parseString "jie " *> regP) <*> (parseString ", " *> offP)
        jioP = JumpIfOne <$> (parseString "jio " *> regP) <*> (parseString ", " *> offP)
        offP = parseMany (parseChar '+') *> parseInt
        regP  = parseOneOf [ parseString "a" *> ignoreWhiteSpace *> pure RegA
                           , parseString "b" *> ignoreWhiteSpace *> pure RegB
                           ]

