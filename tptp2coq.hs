import System.Environment
import System.IO
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.List.NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Text (Text,unpack)
import qualified Data.Text.IO
import Data.TPTP.Parse.Text
import Data.Attoparsec.Text
import Data.TPTP

--------------------------------------------------------------------------------
-- Coq header/footer printing

coqUniverse :: String
coqUniverse = "Universe"

printCoqVar :: String -> String -> Int -> IO ()
printCoqVar retType p n =
    putStrLn ("Variable " ++ p ++ "_ : " ++ concat (replicate n (coqUniverse ++ " -> ")) ++
                          retType ++ ".")

printCoqHeader :: Map String Int -> Map String Int -> IO ()
printCoqHeader preds funs = do
  putStrLn "From Hammer Require Import Tactics.\n"
  putStrLn "Section FOFProblem.\n"
  putStrLn ("Variable " ++ coqUniverse ++ " : Set.")
  putStrLn ("Variable UniverseElement : " ++ coqUniverse ++ ".\n")
  Map.foldrWithKey (\k n acc -> acc >> printCoqVar "Prop" k n) (return ()) preds
  putStrLn ""
  Map.foldrWithKey (\k n acc -> acc >> printCoqVar coqUniverse k n) (return ()) funs
  putStrLn ""

printCoqFooter :: IO ()
printCoqFooter = putStrLn "\nEnd FOFProblem."

--------------------------------------------------------------------------------
-- Translator monad

-- map names to arity
data TranslState = TranslState {
      filename :: String,
      ident :: Int,
      conjectureRegistered :: Bool,
      predicates :: Map String Int,
      functions :: Map String Int }

type Translator = WriterT (DList Char) (StateT TranslState IO)

tellStr :: String -> Translator ()
tellStr s = tell (DList.fromList s)

tellChar :: Char -> Translator ()
tellChar c = tell (DList.singleton c)

tellStrLn :: String -> Translator ()
tellStrLn s = tellStr s >> tellChar '\n'

nextIdent :: Translator Int
nextIdent = do
  s <- get
  let i = ident s
  put (s{ident = i + 1})
  return i

getName :: Text -> Translator String
getName txt = do
  i <- nextIdent
  return (unpack txt ++ "_" ++ show i)

addPred :: String -> Int -> Translator ()
addPred name n = do
  s <- get
  case Map.lookup name (predicates s) of
    Just k | n == k -> return ()
           | otherwise -> failTransl ("predicate " ++ name ++ " used with different arities")
    Nothing -> put (s{predicates = Map.insert name n (predicates s)})

addFun :: String -> Int -> Translator ()
addFun name n = do
  s <- get
  case Map.lookup name (functions s) of
    Just k | n == k -> return ()
           | otherwise -> failTransl ("function " ++ name ++ " used with different arities")
    Nothing -> put (s{functions = Map.insert name n (functions s)})

failTransl :: String -> Translator ()
failTransl err = do
  s <- get
  fail (filename s ++ ": " ++ err)

registerConjecture :: Translator ()
registerConjecture = do
  s <- get
  if conjectureRegistered s then
      failTransl "more than one conjecture"
  else
      put (s{conjectureRegistered = True})

runTranslator :: String -> Translator a -> IO a
runTranslator filename tr = do
  ((a, w), s) <- runStateT (runWriterT tr) (TranslState filename 1 False Map.empty Map.empty)
  printCoqHeader (predicates s) (functions s)
  putStr (DList.toList w)
  if conjectureRegistered s then
      return ()
  else
      putStrLn ("\nTheorem conjecture_" ++ show (ident s) ++ " : False.\nProof.\n  hprover.\nQed.")
  printCoqFooter
  return a

--------------------------------------------------------------------------------
-- Translation

translateFunc :: Bool -> String -> [Term] -> Translator ()
translateFunc paren name args = do
  let paren2 = paren && args /= []
  if paren2 then tellChar '(' else return ()
  tellStr (name ++ "_")
  mapM (\x -> tellChar ' ' >> translateTerm x) args
  if paren2 then tellChar ')' else return ()

translateTerm :: Term -> Translator ()
translateTerm (Function (Defined (Atom txt)) args) =  do
  let name = unpack txt
  addFun name (length args)
  translateFunc True name args
translateTerm (Variable (Var txt)) = tellStr (unpack txt)
translateTerm _ = failTransl "unsupported term"

translateConnective :: Connective -> UnsortedFirstOrder -> UnsortedFirstOrder -> Translator ()
translateConnective Conjunction left right =
    translateFormula left >> tellStr " /\\ " >> translateFormula right
translateConnective Disjunction left right =
    translateFormula left >> tellStr " \\/ " >> translateFormula right
translateConnective Implication left right =
    translateFormula left >> tellStr " -> " >> translateFormula right
translateConnective Equivalence left right =
    translateFormula left >> tellStr " <-> " >> translateFormula right
translateConnective ExclusiveOr left right = do
  tellStr "("
  translateFormula left
  tellStr " \\/ "
  translateFormula right
  tellStr ") /\\ ~("
  translateFormula left
  tellStr " /\\ "
  translateFormula right
  tellChar ')'
translateConnective NegatedConjunction left right =
    tellStr "~(" >> translateFormula left >> tellStr " /\\ " >>
            translateFormula right >> tellChar ')'
translateConnective NegatedDisjunction left right =
    tellStr "~(" >> translateFormula left >> tellStr " \\/ " >>
            translateFormula right >> tellChar ')'
translateConnective ReversedImplication left right =
    translateFormula right >> tellStr " -> " >> translateFormula left

translateQuantifier :: Quantifier -> Translator ()
translateQuantifier Forall = tellStr "forall"
translateQuantifier Exists = tellStr "exists"

translateFormula :: UnsortedFirstOrder -> Translator ()
translateFormula (Atomic (Predicate (Reserved (Standard Tautology)) args)) =
    tellStr "True"
translateFormula (Atomic (Predicate (Reserved (Standard Falsum)) args)) =
    tellStr "False"
translateFormula (Atomic (Predicate (Defined (Atom txt)) args)) = do
  let name = unpack txt
  addPred name (length args)
  translateFunc False name args
translateFormula (Atomic (Equality left sign right)) = do
  translateTerm left
  if sign == Positive then tellStr " = " else tellStr " <> "
  translateTerm right
translateFormula (Negated formula) = do
  tellStr "~("
  translateFormula formula
  tellChar ')'
translateFormula (Connected left conn right) = do
  tellChar '('
  translateConnective conn left right
  tellChar ')'
translateFormula (Quantified quant nlst body) = do
  tellChar '('
  translateQuantifier quant
  let vars = map fst (Data.List.NonEmpty.toList nlst)
  mapM_ (\(Var txt) -> tellStr (" " ++ (unpack txt))) vars
  tellStr (" : " ++ coqUniverse ++ ", ")
  translateFormula body
  tellChar ')'
translateFormula _ = failTransl "unsupported formula"

translateAxiom :: Text -> UnsortedFirstOrder -> Translator ()
translateAxiom txt formula = do
  name <- getName txt
  tellStr "Variable "
  tellStr name
  tellStr " : "
  translateFormula formula
  tellStrLn "."

translateUnit :: Unit -> Translator ()
translateUnit (Include (Atom txt) Nothing) = do
    h <- liftIO (openFile (unpack txt) ReadMode)
    translateFile h
    liftIO (hClose h)
translateUnit (Include _ (Just _)) =
    failTransl "unsupported include statement"
translateUnit (Unit (Left (Atom txt)) (Formula (Standard ax) (FOF formula)) _)
    | ax == Axiom || ax == Hypothesis || ax == Assumption || ax == Lemma ||
      ax == Theorem || ax == Corollary =
          translateAxiom txt formula
translateUnit (Unit (Left (Atom txt)) (Formula (Standard Conjecture) (FOF formula)) _) = do
  name <- getName txt
  registerConjecture
  tellStr "\nTheorem "
  tellStr name
  tellStr " : "
  translateFormula formula
  tellStrLn "."
  tellStrLn "Proof."
  tellStrLn "  hprover."
  tellStrLn "Qed."
translateUnit _ =
    failTransl "unsupported declaration"

translateFile :: Handle -> Translator ()
translateFile h = do
  txt <- liftIO (Data.Text.IO.hGetContents h)
  case parseTPTPOnly txt of
    Right tptp -> mapM_ translateUnit (units tptp)
    Left s -> failTransl ("cannot parse file: " ++ s)

--------------------------------------------------------------------------------
-- Main program

main = do
  args <- getArgs
  if length args /= 1 then
      hPutStrLn stderr $ "Usage: tptp2coq file.p"
  else
      let filename = head args in
      withFile filename ReadMode (runTranslator filename . translateFile)
