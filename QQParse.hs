{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Data.Default.Class

import UULayout

import Data.Char
import Data.List
import System.Environment
import Text.Read

import Data.Maybe
import Data.List.Split

import Test.Hspec
import Language.Haskell.TH.Ppr (Ppr(ppr))
import Language.Haskell.TH.PprLib (Doc)

import Language.Haskell.TH.Syntax
 (Type(..),
  Dec(..),
  Exp(..),
  Pat(..),
  Name(..),
  OccName(..),
  NameFlavour(..),
  ModName(..),
  NameSpace(..),
  PkgName(..),
  TyVarBndr(..),
  Cxt(..),
  Pred(..),
  Kind,
  TyLit(..),

  Con(..),
  StrictType(..),
  Strict(..),

  FieldExp(..),
  Match(..),
  Lit(..),
  Guard(..),
  Stmt(..), Range(..),

  Body(..),
  Clause(..),
  FieldPat(..),
  )

defHP = def :: HParser

runP :: Show t => (HParser -> HP t) -> String -> String
runP p str = case runParser "p" (fixP (\d -> amb . p d) defHP) str of
  [e] -> show e
  es -> error $ "str:" ++ str ++ " parses as " ++ show (map show es)

runPp :: Ppr t => (HParser -> HP t) -> String -> String
runPp p str = case runParser "p" (fixP (\d -> amb . p d) defHP) str of
  [e] -> show (ppr e)
  es -> error $ "str:" ++ str ++ " parses as " ++ show (map ppr es)


runPpAmb :: Ppr t => (HParser -> HP t) -> String -> Doc
runPpAmb  p str = ppr $ runParser "p" (fixP (\d -> amb . p d) defHP) str

runPAmb :: Show t => (HParser -> HP t) -> String -> String
runPAmb  p str = show $ runParser "p" (fixP (\d -> amb . p d) defHP) str

main =  hspec $ do
    let sid f str = f str `shouldBe` str

    it "AppT" $ do
      runP pType "Ab B" `shouldBe` "AppT (ConT Ab) (ConT B)"
      runP pType "A B C" `shouldBe` "AppT (AppT (ConT A) (ConT B)) (ConT C)"
      runP pType "A B C D" `shouldBe` "AppT (AppT (AppT (ConT A) (ConT B)) (ConT C)) (ConT D)"
    it "AppE" $ do
      runP pExp "A B" `shouldBe` "AppE (ConE A) (ConE B)"
      runP pExp "A B C" `shouldBe` "AppE (AppE (ConE A) (ConE B)) (ConE C)"
    it "UInfix" $ do
      runP pExp "A + B + C D" `shouldBe` "UInfixE (UInfixE (ConE A) (VarE +) (ConE B)) (VarE +) (AppE (ConE C) (ConE D))"

      runPp pExp `sid` "A `b` C"
      runPp pExp "A b`b`C" `shouldBe` "A b `b` C"

    it "PromotedT" $ do
      runPp pExp `sid` "Proxy :: Proxy 'True"
      runPp pExp `sid` "Proxy :: Proxy ('True :: Bool)"
      runPp pExp "Proxy :: Proxy '(Int,String)" `shouldBe` "Proxy :: Proxy ('(Int, String))"
      runPp pExp "undefined :: HList [Int,String,Double]"
           `shouldBe` "undefined :: HList ((':) Int ((':) String ((':) Double '[])))"
      runPp pExp "x :: '[Int]" `shouldBe` "x :: (':) Int '[]"

      runP pExp "Proxy :: Proxy '(x,y)" `shouldBe` "SigE (ConE Proxy) (AppT (ConT Proxy) (AppT (AppT (PromotedTupleT 2) (VarT x)) (VarT y)))"

    it "ConT Infix" $ do
      runP pType "A :+: B" `shouldBe` "AppT (AppT (ConT :+:) (ConT A)) (ConT B)"

    it "ForallT" $ do
      runP pType "Show a => a" `shouldBe` "ForallT [] [ClassP Show [VarT a]] (VarT a)"
      runP pType "(a ~ b) => a" `shouldBe` "ForallT [] [EqualP (VarT a) (VarT b)] (VarT a)"
      runP pType "(Show a) => a" `shouldBe` "ForallT [] [ClassP Show [VarT a]] (VarT a)"
      runP pType "forall a. (Show a) => a" `shouldBe` "ForallT [PlainTV a] [ClassP Show [VarT a]] (VarT a)"

    it "SigT" $ do
      -- ghc bug #10050
      runP pExp "x :: (x :: Constraint)" `shouldBe` "SigE (VarE x) (SigT (VarT x) ConstraintT)"
      runP pExp "x :: (x :: Constrainty)" `shouldBe` "SigE (VarE x) (SigT (VarT x) (ConT Constrainty))"
      runP pExp "x :: (x :: *)" `shouldBe` "SigE (VarE x) (SigT (VarT x) StarT)"
      runP pExp "x :: (x ::* )" `shouldBe` "SigE (VarE x) (SigT (VarT x) StarT)"

    it "TupleT" $ do
      runPp pExp `sid` "(a, b) :: (Int, String)"
      runPp pExp `sid` "(# a, b #) :: (# , #) Int String"
      runP pExp "(#a,b#)" `shouldBe` "UnboxedTupE [VarE a,VarE b]"


    it "CompE" $ do
      runP pExp "[ x | x <- xs ]" `shouldBe` "CompE [BindS (VarP x) (VarE xs),NoBindS (VarE x)]"

    it "LitT" $ do
      runPp pType `sid` show "1"
      runPp pExp `sid` "A :: 3"

    it "sections" $ do
      runPp pExp "(`f` x) y" `shouldBe` "((`f` x)) y"
      runPp pExp "(x `f`) y" `shouldBe` "((x `f`)) y"
      runPp pExp "(x `f` y) y" `shouldBe` "(x `f` y) y"

    it "ArithSeqE" $ do
      runPp pExp `sid` "[a..m] :: [Int]"
      runPp pExp       "[a..] :: [] Int"
          `shouldBe`   "[a..] :: [Int]"
      runPp pExp `sid` "[b,a..n]"
      runPp pExp `sid` "[c,c..]"
      runPp pExp `sid` "[A.c,A.B.c..]"

    it "LitE" $ do
      runPp pExp `sid` show "any random string"
      runPp pExp "12.3" `shouldBe` "3462142213541069 / 281474976710656"
      runPp pExp `sid` "12"


    it "qual" $ do
      runPp pExp `sid` "A.x b"
      runPp pExp `sid` "A.x . y b"
      runPp pExp "A.x.y b" `shouldBe` "A.x . y b"


    it "IfE" $ do
      runPp pExp `sid` "if a then b else c"

    it "MultiIfE" $ do
      runPp pExp "if | a -> b | c -> d" `shouldBe` "if | a -> b\n   | c -> d"
      runPp pExp    "if | f a -> b | c -> d"
         `shouldBe` "if | f a -> b\n   | c -> d"
      runPp pExp   "if | Just 1 <- f, f a -> b | c -> d"
        `shouldBe` "if | Just 1 <- f,\n     f a\n       -> b\n   | c -> d"

    it "LamE" $ do
      runPp pExp `sid` "\\x (Just y) -> y"


    it "RecConE" $ do
      runP pExp "C { z = z g}" `shouldBe` "RecConE C [(z,AppE (VarE z) (VarE g))]"
      runP pExp "c { z = z g}" `shouldBe` "RecUpdE (VarE c) [(z,AppE (VarE z) (VarE g))]"
      runP pExp "f c { z = z g}" `shouldBe` "AppE (VarE f) (RecUpdE (VarE c) [(z,AppE (VarE z) (VarE g))])"
      runP pExp "C { z = z g }" `shouldBe` "RecConE C [(z,AppE (VarE z) (VarE g))]"


    it "SigE" $ do
      runP pSigE "A::B" `shouldBe` "SigE (ConE A) (ConT B)"

    it "ParensE / TupE" $ do
      runPp pExp `sid` "a (b c)"
      runPp pExp `sid` "a (b, c)"

    it "Type" $ do
      runPp pType `sid` "A -> B -> C"
      runPp pType `sid` "A a x y -> B -> C"
      runPp pType "a ': b" `shouldBe` "(':) a b"
      runPp pType "a ': b ': c" `shouldBe` "(':) a ((':) b c)"
      runPp pType "a ': b ': c ': '[]" `shouldBe` "(':) a ((':) b ((':) c '[]))"
      runPp pType `sid` "(A -> B) -> C"
      runPp pType `sid` "'(,,) -> C"


    it "ValD" $ do
      runPp pDec "Just x = b where b = c" `shouldBe`
                 "Just x = b\n    where b = c"
      runPp pDec `sid` "(a, b, Just c) = a"
      runP pDec "a@(a, b, Just c) = a" `shouldBe` "ValD (AsP a (TupP [VarP a,VarP b,ConP Just [VarP c]])) (NormalB (VarE a)) []"
      runP pDec "a@(!a, ~b, Just c) = a" `shouldBe`
          "ValD (AsP a (TupP [BangP (VarP a),TildeP (VarP b),ConP Just [VarP c]])) (NormalB (VarE a)) []"
      runP pDec "[a,b,c] = a" `shouldBe`
          "ValD (ListP [VarP a,VarP b,VarP c]) (NormalB (VarE a)) []"


    it "RecP/ViewP" $ do
      runP pPat "R { a = (f -> Just 3) }" `shouldBe`
        "RecP R [(a,ViewP (VarE f) (ConP Just [LitP (IntegerL 3)]))]"
        
    it "UInfixP" $ do
      runP pPat "f `HCons` x" `shouldBe` "UInfixP (VarP f) HCons (VarP x)"


    it "FunD" $ do
      runP pDec "f (Just 1) = 4 where _ = 2" `shouldBe`
        "FunD f [Clause [ParensP (ConP Just [LitP (IntegerL 1)])] (NormalB (LitE (IntegerL 4))) [ValD WildP (NormalB (LitE (IntegerL 2))) []]]"

    it "LetE" $ do
      runP pExp "let x = 1 in y" `shouldBe` "LetE [ValD (VarP x) (NormalB (LitE (IntegerL 1))) []] (VarE y)"
      let xyz = "LetE [ValD (VarP x) (NormalB (LitE (IntegerL 1))) [],ValD (VarP y) (NormalB (LitE (IntegerL 2))) []] (VarE z)"
      runP pExp "let { x = 1; y = 2 } in z" `shouldBe` xyz
      runP pExp "let x = 1; y = 2 in z" `shouldBe` xyz
      runP pExp "let x = let y = 1 in y in x" `shouldBe`
          "LetE [ValD (VarP x) (NormalB (LetE [ValD (VarP y) (NormalB (LitE (IntegerL 1))) []] (VarE y))) []] (VarE x)"

    it "CaseE" $ do
      runP pExp "case x of A -> B" `shouldBe`
          "CaseE (VarE x) [Match (ConP A []) (NormalB (ConE B)) []]"
      runP pExp "case x of A -> B; C -> D" `shouldBe`
          "CaseE (VarE x) [Match (ConP A []) (NormalB (ConE B)) []\
                         \,Match (ConP C []) (NormalB (ConE D)) []]"
      runP pExp "case x of a -> B; C -> D" `shouldBe`
          "CaseE (VarE x) [Match (VarP a) (NormalB (ConE B)) []\
                         \,Match (ConP C []) (NormalB (ConE D)) []]"
      runP pExp "case x of a | x <- b -> c" `shouldBe`
          "CaseE (VarE x) [Match (VarP a) (GuardedB [(PatG [BindS (VarP x) (VarE b)],VarE c)]) []]"

      runP pExp "case x of (f -> a) | x <- b -> c" `shouldBe`
          "CaseE (VarE x) [Match (ViewP (VarE f) (VarP a))\
                          \ (GuardedB [(PatG [BindS (VarP x) (VarE b)],VarE c)]) []]"

    it "infix decs" $
      runP pDecs "x `f` y = (x,y)" `shouldBe` 
        "[FunD f [Clause [VarP x,VarP y] (NormalB (TupE [VarE x,VarE y])) []]]"

    it "SigD" $ do
      runP pDecs "x :: Int -> Int" `shouldBe` "[SigD x (AppT (AppT ArrowT (ConT Int)) (ConT Int))]"
      runP pDecs "(+) :: Int -> Int" `shouldBe` "[SigD + (AppT (AppT ArrowT (ConT Int)) (ConT Int))]"

    it "DataD" $ do
      runPp pDecs `sid` "data X = X | Y | Z W"
      runPp pDecs `sid` "data X a = X a | Y | Z W"
      runP pDecs "data X a = X { a :: a }"
          `shouldBe` "[DataD [] X [PlainTV a] [RecC X [(a,NotStrict,VarT a)]] []]"
      runP pDecs "data X a = X { a, y :: a B, c :: TT T }" `shouldBe`
              "[DataD [] X [PlainTV a] [RecC X [(a,NotStrict,AppT (VarT a) (ConT B)),(y,NotStrict,AppT (VarT a) (ConT B)),(c,NotStrict,AppT (ConT TT) (ConT T))]] []]"
      runP pDecs "data X a = X deriving Foo" `shouldBe` "[DataD [] X [PlainTV a] [NormalC X []] [Foo]]"
      runP pDecs "data C a => X a = X deriving Foo" `shouldBe` "[DataD [ClassP C [VarT a]] X [PlainTV a] [NormalC X []] [Foo]]"
      runP pDecs "data X a = Y | forall f. (C f) => X a" `shouldBe`
        "[DataD [] X [PlainTV a] [NormalC Y [],ForallC [PlainTV f] [ClassP C [VarT f]] (NormalC X [(NotStrict,VarT a)])] []]"

      runP pDecs "data X a = forall f. (C f) => X | Y deriving Enum" `shouldBe` "[DataD [] X [PlainTV a] [ForallC [PlainTV f] [ClassP C [VarT f]] (NormalC X []),NormalC Y []] [Enum]]"

      runP pDecs "f x = x\nf x = x" `shouldBe`
        "[FunD f [Clause [VarP x] (NormalB (VarE x)) [],Clause [VarP x] (NormalB (VarE x)) []]]"

    it "InfixC" $ do
      runP pConstructor "A := Int" `shouldBe` "InfixC (NotStrict,ConT A) := (NotStrict,ConT Int)"

    it "pType" $ do
      runP pType "(:=)" `shouldBe` "ConT :="


failing = hspec $ do
    let sid f str = f str `shouldBe` str

    it "layout" $ runP pExp "1\n + 2"
        `shouldBe` "UInfixE (LitE (IntegerL 1)) (VarE +) (LitE (IntegerL 2))"

    it "DataD" $ do
      runPp pDecs `sid` "data a ::: b = X a"
      runPp pDecs `sid` "data a ::: b = a :+: b"
      runPp pDecs `sid` "newtype X = X { a = T }"
      runPp pDecs `sid` "data X a = forall f. (C f) => X a | Y"

    let xyz = "LetE [ValD (VarP x) (NormalB (LitE (IntegerL 1))) [],ValD (VarP y) (NormalB (LitE (IntegerL 2))) []] (VarE z)"
    it "letE" $ do
      runP pExp "let x = 1\n\
                \    y = 2\n\
                \ in z" `shouldBe` xyz


type HP t = S -> P (Str Char String LineColPos) t

newtype S = S (forall r. (HParser -> S -> r) -> r)

data HParser = HParser
 {pType :: HP Type
 ,pDecs :: HP [Dec]
 ,pExp :: HP Exp
 ,pPat :: HP Pat
 ,pPat' :: HP Pat

 ,pSigP, pUInfixP


 ,pLitP, pVarP, pParensP, pUnboxedTupP, pConP, pAsP, pTildeP, pBangP, pWildP, pListP, pRecP, pViewP  :: HP Pat

 ,pDec
 -- all the constructors for Dec
 ,pFunD
 ,pValD
 ,pFunDInfix -- ^ p1 `f` p2 = ...
 ,pDataD
 ,pNewtypeD
 ,pTySynD
 ,pClassD
 ,pInstanceD
 ,pSigD
 ,pForeignD
 ,pInfixD
 ,pPragmaD
 ,pFamilyD
 ,pDataInstD
 ,pNewtypeInstD
 ,pTySynInstD
 ,pClosedTypeFamilyD
 ,pRoleAnnotD :: HP Dec

 ,pConstructor :: HP Con 

 ,pBody :: HP Body
 ,pWhere :: HP [Dec]

 ,pType' :: HP Type
                -- ^ a subset of Type that must consume
                -- input to produce a constructor (no leading spaces)

 ,pExp' :: HP Exp
                -- ^ a subset of Exp that must consume
                -- input to produce a constructor (no leading spaces)


 ,pVarE        -- ^ @{ x }@
 ,pConE        -- ^ @data T1 = C1 t1 t2; p = {C1} e1 e2  @
 ,pLitE        -- ^ @{ 5 or 'c'}@
 ,pSpaceSep    {- ^ AppE @{ f x }@
               
               RecConE @{ T { x = y, z = w } }@
               
               RecUpdE @{ (f x) { z = w } }@
               
               UInfixE @{x + y}@

               using classifySpaceSep
               -}

 ,pInfixE      -- ^ @{x + y} or {(x+)} or {(+ x)} or {(+)}@
 ,pLamE        -- ^ @{ \ p1 p2 -> e }@
 ,pLamCaseE    -- ^ @{ \case m1; m2 }@
 ,pTupE        -- ^ @{ (e1,e2) }  @
               -- or ParensE @{ (e) }@
 ,pUnboxedTupE -- @{ (# e1,e2 #) }  @
 ,pCondE       -- ^ @{ if e1 then e2 else e3 }@
 ,pMultiIfE    -- ^ @{ if | g1 -> e1 | g2 -> e2 }@
 ,pLetE        -- ^ @{ let x=e1;   y=e2 in e3 }@
 ,pCaseE       -- ^ @{ case e of m1; m2 }@
 ,pDoE         -- ^ @{ do { p <- e1; e2 }  }@
 ,pCompE       -- ^ @{ [ (x,y) | x <- xs, y <- ys ] }@
 ,pArithSeqE   -- ^ @{ [ 1 ,2 .. 10 ] }@
 ,pListE       -- ^ @{ [1,2,3] }@
 ,pSigE        -- ^ @{ e :: t }@
    :: HP Exp 



 -- helpers for Exp
 ,pMatch :: HP Match
 ,pGuard :: HP Guard
 ,pStmt :: HP Stmt
 ,pFieldExps :: HP [FieldExp]

 ,pForallT        -- ^ @forall \<vars\>. \<ctxt\> -> \<type\>@
 ,pSpaceSepT
 ,pSigT           -- ^ @t :: k@
 ,pVarT           -- ^ @a@
 ,pConAlphaT      -- ^ @T@ or @Constraint@
 ,pConSymT
 ,pPromotedT      -- ^ @'T@
 ,pTupleT         -- ^ @(,), (,,), etc.@ or @(#,#), (#,,#), etc.@
 ,pArrowT         -- ^ @->@
 ,pListT          -- ^ @[]@
 ,pPromotedTupleT -- ^ @'(), '(,), '(,,), etc.@
 ,pStarT          -- ^ @*@

 ,pLitT           :: HP Type -- ^ @0,1,2, etc.@
 -- helpers for Type
 ,pKind      :: HP Kind
 ,pCxt       :: HP Cxt
 ,pPred      :: HP Pred

 ,pLit :: HP Lit

 ,pQual      :: (HParser -> HP Name) -> HP Name

 ,pVar
 ,pVarAlpha
 ,pVarSym
 ,pConSym
 ,pConAlpha
 ,pCon       :: HP Name

 ,pAlpha     :: HP Char
 ,pSy        :: HP Char
 ,pCharLit   :: HP Char
 ,pTyVarBndr :: HP TyVarBndr



 ,pForall :: HP () -- ^ @forall@
 ,pLam    :: HP () -- ^ @\\@
 ,pRArrow :: HP () -- ^ @->@
 ,pLArrow :: HP () -- ^ @<-@
 ,pDColon :: HP () -- ^ @::@
 ,pDotDot :: HP () -- ^ @..@

 -- layout-related
 ,pSemi :: HP ()
 ,pLBrace :: HP ()
 ,pRBrace :: HP ()



 ,isSy :: S -> Char -> Bool

 ,classifySpaceSep :: S -> [Either Exp [FieldExp]] -> Exp
 ,classifySpaceSepT :: S -> [Type] -> Type
 }


-- fixP :: (HParser -> HP t) -> HParser -> Parser t
fixP getP p = getP p s
  where s :: S
        s = S $ \f -> f p s


instance Default HParser where
  def = HParser
   {pType = \(S s) -> (pSpaces *> s pSpaceSepT <* pSpaces)
                  <|> s pSigT
                  <|> pParens (s pConSymT)

   ,pType' = \(S s) ->
                  s pConAlphaT -- so a::* is SigT _ StarT
              <|> s pStarT
              <|> s pForallT
              <|> s pLitT
              <|> s pVarT
              <|> s pPromotedT
              <|> s pTupleT
              <|> s pArrowT
              <|> s pListT
              <|> s pPromotedTupleT

  ,pExp = \(S s) -> (pSpaces *> s pSpaceSep <* pSpaces)
         <|> s pSigE


  ,pExp' = \(S s) -> s pVarE
     <|> s pConE
     <|> s pLitE
     <|> s pInfixE
     <|> s pTupE
     <|> s pLamE
     <|> s pLamCaseE
     <|> s pUnboxedTupE
     <|> s pCondE
     <|> s pMultiIfE
     <|> s pLetE
     <|> s pCaseE
     <|> s pDoE
     <|> s pCompE
     <|> s pArithSeqE
     <|> s pListE

  ,pPat = \(S s) -> pSpaces *> (s pPat' <|> s pSigP <|> s pUInfixP) <* pSpaces

  ,pSigP = \(S s) -> SigP <$> s pPat' <*> (s pDColon *> s pType)

  ,pUInfixP = \(S s) ->
    let infixCon = pSym '`' *> s pConAlpha <* pSym '`'
    in UInfixP <$> s pPat' <*> infixCon <*> s pPat

  ,pPat' = \(S s) -> s pLitP
      <|> s pVarP
      <|> s pParensP
      <|> s pUnboxedTupP
      <|> s pConP
      <|> s pAsP
      <|> s pTildeP
      <|> s pBangP
      <|> s pWildP
      <|> s pListP
      <|> s pRecP
      <|> s pViewP


  ,pLitP = \(S s) -> LitP <$> s pLit
  ,pVarP = \(S s) -> VarP <$> s pVarAlpha
  ,pParensP = \(S s) ->
      let tupP [x] = ParensP x
          tupP xs = TupP xs
      in pParens $ tupP <$> pList1Sep pComma (s pPat)

  ,pUnboxedTupP = \(S s) ->
        pToken "(#" *>  (UnboxedTupP <$> ((:) <$> s pPat <*> (pList1 (pComma *> s pPat))))
        <* pToken "#)"


  ,pConP = \(S s) ->
      ConP <$> s pCon <*> pListSep_ng pSpaces (s pPat)

  ,pAsP = \(S s) -> AsP <$> (s pVarAlpha <* pToken "@") <*> s pPat


  ,pTildeP = \(S s) -> TildeP <$> (pToken "~" *> s pPat)

  ,pBangP = \(S s) -> BangP <$> (pToken "!" *> s pPat)
 
  ,pWildP = \(S s) -> WildP <$ pToken "_"

  ,pListP = \(S s) -> ListP <$> listParser (s pPat)


  ,pRecP = \(S s) ->
     let con = s pConAlpha <* pSpaces
         fieldPats = pListSep pComma fieldPat 
         fieldPat = (,) <$> s pVarAlpha <*> (pToken "=" *> s pPat)
     in RecP <$> con <*> pBraces fieldPats 

  ,pViewP = \(S s) -> pParens $ ViewP <$> s pExp <*> (s pRArrow *> s pPat)


  ,pVar = \(S s) -> micro (s pVarAlpha <|> s pVarSym) 1
    -- micro adds a penalty to choosing a variable name,
    -- so if/then/else are consumed by pToken, A.B.c does not
    -- use . as variable
  ,pVarE = \(S s) -> VarE <$> s (optQual pVar)
        <?> "VarE"
  ,pConE = \(S s) -> ConE <$> s (optQual pCon)
        <?> "ConE"
  ,pLitE = \(S s) -> LitE <$> s pLit

  ,pSpaceSepT = \(S s) -> (\ x xs -> s classifySpaceSepT (x : xs)) <$>
        s pType' <*> (pSpaces *> pListSep_ng pSpaces
                                   (s pType'
                                      <|> micro (s pConSymT) 1 -- so that T::* parses as SigT T * not
                                                               -- a (ConT ::*)
                                    ))
  ,pSpaceSep = \ (S s) ->
    let e = Left <$> s pExp' <|> Right <$> s pFieldExps
    in s classifySpaceSep <$> pList1Sep_ng pSpaces e


  ,classifySpaceSepT = \ (S s) xs ->
      let isOp (VarT (Name (OccName (n:_)) _)) = s isSy n
          isOp (ConT (Name (OccName (':':_)) _)) = True
          isOp ArrowT = True
          isOp PromotedConsT = True
          isOp _ = False


          -- this is not really as general as it could be
          toCxt (AppT (ConT n) t) = [ClassP n (unapp t)]
          toCxt (ConT n) = [ClassP n []]
          toCxt (VarT n) = [ClassP n []]
          toCxt (VarT (Name (OccName "~") NameS) `AppT` x `AppT` y) = [EqualP x y]
          toCxt a = concatMap toCxt (reverse (fromTuple 0 a))

          fromTuple n (AppT a b) = b : fromTuple (n+1) a
          fromTuple n (TupleT m)
              | n == m = []
              | otherwise = error "QQParse.fromTuple"
          fromTuple _ x = error ("QQParse.fromTuple: don't know how to handle " ++ show x)

          unapp (AppT a b) = a : unapp b 
          unapp x = [x]


          infixF (Right a : Left (VarT (Name (OccName "=>") _)) : cs) = ForallT [] (toCxt a) (infixF cs)
          infixF (Right a : Left b : cs) = infixF (Right (AppT b a) : cs)
          infixF (Left a : b : cs) = AppT a (infixF (b:cs))
          infixF (Right a : b : cs) = AppT a (infixF (b:cs))
          infixF [a] = either id id a

      in infixF $
          mapMaybe (\x -> case x of
                   [a] | isOp a -> Just (Left a)
                   (a:b) -> Just (Right (foldl AppT a b))
                   _ -> Nothing)
          $ split (whenElt isOp) xs

  ,classifySpaceSep = \ (S s) xs ->
      let isOp (VarE (Name (OccName (n:_)) _)) = s isSy n
          isOp (ConE (Name (OccName (':':_)) _)) = True
          isOp (InfixE Nothing v Nothing) = not (isOp v)
          isOp _ = False


          minfix (Right a : Left op : Right b : xs)
             | InfixE Nothing op' Nothing <- op = minfix (Right (UInfixE a op' b) : xs)
             | otherwise = minfix (Right (UInfixE a op b) : xs)
          minfix (Right y : Left (InfixE Nothing x Nothing) : xs) = minfix (Left (InfixE (Just y) x Nothing) : xs)
          minfix (Left (InfixE Nothing x Nothing) : Right y : xs) = minfix (Left (InfixE Nothing x (Just y)) : xs)
          minfix (Left x : xs) = x : minfix xs
          minfix (Right x : xs) = x : minfix xs
          minfix [] = []

          applyFieldExps (Left (ConE n) : Right fexp : as) = applyFieldExps (Left (RecConE n fexp) : as)
          applyFieldExps (Left e : Right fexp : as) = applyFieldExps (Left (RecUpdE e fexp) : as)
          applyFieldExps (Left e : es) = e : applyFieldExps es
          applyFieldExps [] = []

      in foldl1 AppE $ minfix
        $ mapMaybe (\x -> case x of
                   [a] | isOp a -> Just (Left a)
                   a:b -> Just (Right (foldl AppE a b))
                   _ -> Nothing
                 )
        $ split (whenElt isOp) 
        $ applyFieldExps xs

  ,pInfixE = \(S s) ->
        InfixE Nothing <$> (pSym '`' *> (VarE <$> s pVarAlpha
                                            <|> s pConE) <* pSym '`')
                <*> pure Nothing


  ,pLamE = \(S s) -> LamE <$> (pSym '\\' *> some (s pPat) <* s pRArrow) <*> s pExp

  ,pLamCaseE = \(S s) -> LamCaseE <$>
         (s pLam *> pSymbol "case" *> pList1Sep (s pSemi) (s pMatch)) -- XXX layout


  ,pTupE = \(S s) -> (\x -> case x of
                              [e] -> ParensE e
                              _ -> TupE x)
                <$> (pLParen *> (pList1Sep pComma (s pExp)) <* pSym ')')

  ,pUnboxedTupE = \(S s) -> pToken "(#" *>
                     (UnboxedTupE <$> pList1Sep pComma (s pExp))
                     <* pSymbol "#)"


  ,pCondE = \(S s) -> CondE <$>
           (pSymbol "if" *> s pExp)
         <*> (pSymbol "then" *> s pExp)
         <*> (pSymbol "else" *> s pExp)
  ,pMultiIfE = \(S s) ->
     let guardE = (,) <$> s pGuard
                      <*> (s pRArrow *> s pExp)
     in MultiIfE <$> (pSymbol "if" *> pSome guardE) -- ^ @{ if | g1 -> e1 | g2 -> e2 }@

  ,pGuard = \(S s) -> 
      let patG [NoBindS e] = NormalG e
          patG stmts = PatG stmts
      in pToken "|" *> ( patG <$> pList1Sep pComma (s pStmt) )

  ,pMatch = \(S s) -> 
      let normalB = s pRArrow *> (NormalB <$> s pExp)
          guardedB = GuardedB <$> pList1 ((,) <$> s pGuard <*> (s pRArrow *> s pExp))
      in Match <$> s pPat <*> (normalB <|> guardedB) <*> s pWhere

  ,pLetE = \(S s) ->
     let decs = pToken "let" *> laidout (s pDec)
     in LetE <$> decs <*> (pSpaces *> pToken "in" *> s pExp)

  ,pCaseE = \(S s) -> CaseE <$> (pToken "case" *> s pExp <* pToken "of")
                            <*> laidout (s pMatch) -- ^ @{ case e of m1; m2 }@

  ,pDoE = \(S s) ->
       let stmts = pToken "do" *> laidout (s pStmt)
       in DoE <$> stmts

  ,pArithSeqE = \(S s) ->
      let toRange a (Just b) (Just c) = FromThenToR a b c
          toRange a Nothing Nothing = FromR a
          toRange a (Just b) Nothing = FromThenR a b
          toRange a Nothing (Just c) = FromToR a c

          addCon a b c = ArithSeqE (toRange a b c)
      in addCon <$> (pSym '[' *> s pExp)
                <*> optional (pComma *> s pExp) 
                <*> (s pDotDot *> optional (s pExp) <* pSpaces)
                <* pSym ']'
  ,pCompE = \(S s) ->
    let toComp e stmts = CompE (stmts ++ [NoBindS e])
    in toComp <$> (pSym '['  *> s pExp)
              <*> (pSym '|' *> pList1Sep pComma (s pStmt) <* pSym ']')
        -- ^ @{ [ (x,y) | x <- xs, y <- ys ] }@

  ,pFieldExps = \(S s) -> pBraces (pList1Sep_ng pComma
                 ((,) <$> (s pVarAlpha <* pToken "=")
                      <*> s pExp))

  ,pListE = \(S s) -> ListE <$> listParser (s pExp)
  ,pSigE = \(S s) -> SigE <$> s pExp' <*> (pSpaces *> s pDColon *> s pType)


  ,pDecs = \(S s) -> 
    let mergeFunDs (FunD x a : FunD y b : xs) | x == y = mergeFunDs (FunD x (a ++ b) : xs)
        mergeFunDs (x : xs) = x : mergeFunDs xs
        mergeFunDs [] = []
    in mergeFunDs <$> laidout (s pDec)


  ,pDec = \(S s) -> s pValD
      <|> micro (s pFunD) 1 -- data X = X
      <|> s pFunDInfix
      <|> s pSigD
      <|> s pDataD

  ,pValD = \(S s) ->

    ValD <$> s pPat <*> s pBody <*> s pWhere

  ,pWhere = \(S s) -> pToken "where" *> s pDecs <|> pure []

  ,pBody = \(S s) ->
    let normalB = NormalB <$> (pToken "=" *> s pExp)
        guardedB = GuardedB <$>
                      pList1 ((,) <$> (pToken "|" *> s pGuard)
                                            <*> (pToken "=" *> s pExp))
    in normalB <|> guardedB

  ,pFunD = \(S s) ->

    let pClause = Clause <$> pList1 (s pPat) <*> s pBody <*> s pWhere
    in FunD <$> s pVarAlpha <* pSpaces <*> ((:[]) <$> pClause)

  ,pFunDInfix = \(S s) ->
    let infixV = (pSym '`' *> s pVarAlpha <* pSym '`')
              <|> s pVarSym

        f p1 n p2 b w = FunD n [Clause [p1,p2] b w]
    in f <$> s pPat <*> infixV <*> s pPat <*> s pBody <*> s pWhere

  ,pDataD = \(S s) -> 
    let cxt = s pCxt <* pToken "=>" <|>
              pure []
        derivingList = pParens (pListSep_ng pComma (s pCon))
                <|> ((:[]) <$> s pCon)
        constructors = pList1Sep (pSpaces *> pSymbol "|") (s pConstructor)
    in DataD <$ pSymbol "data"
          <*> cxt 
          <*> (pSpaces *> s pConAlpha <* pSpaces)
          <*> pListSep pSpaces (s pTyVarBndr) <* pSpaces <* pSymbol "="
          <*> constructors
          <*> (pSymbol "deriving" *> derivingList <|> pure [])

  ,pConstructor = \(S s) ->
    let pUnpack = pToken "{-#" *> pToken "UNPACK" *> pToken "#-}" *> pToken "!"
        pStrict = IsStrict <$ pToken "!"
                  <|> Unpacked <$ pUnpack
                   <|> pure NotStrict

        pStrictType = ((,) <$ pSpaces <*> pStrict <*> s pType')

        pN = s pConAlpha <* pSpaces

        pNormalC = NormalC
          <$> pN
          <*> pList_ng (micro pStrictType 1) -- "deriving" is otherwise a valid type

        pRecC = RecC
          <$> pN
          <*> pBraces (concat <$> pList1Sep_ng pComma pVarStrictType)
                          
        pVarStrictType = spread <$> (pList1Sep pComma (s pVarAlpha) <* s pDColon)
                                <*> pStrict
                                <*> s pType

        spread :: [n] -> s -> t -> [(n,s,t)]
        spread ns s t = map (\n -> (n,s,t)) ns

        pInfixC = InfixC
                      <$> pStrictType
                      <*> (pSpaces *> s pConSym <* pSpaces)
                      <*> pStrictType 

        pForallC = ForallC <$ (s pForall <* pSpaces)
                      <*> (pList1Sep pSpaces (s pTyVarBndr) <* pSymbol ".")
                      <*> (s pCxt <* pSymbol "=>")
                      <*> s pConstructor

    in pInfixC <|> pNormalC <|> pRecC <|> pForallC
    {-  | ForallC [TyVarBndr] Cxt Con
    -}


  ,pSigD = \(S s) -> SigD <$> (s pVarAlpha <|> pParens (s pVarSym))
                          <*> (s pDColon *> s pType)

  ,pStmt = \(S s) ->
                  NoBindS <$> s pExp <|>
                  BindS <$> s pPat <*> (s pLArrow *> s pExp)
               <|>  LetS <$> (pToken "let" *> s pDecs)
                -- ParS XXX


  ,pSigT = \(S s) -> SigT <$> s pType' <*> (pSpaces *> s pDColon *> s pKind)
  ,pVarT = \(S s) -> VarT <$> (s pVarAlpha <|> micro (s pVarSym) 2)
  ,pConAlphaT = \(S s) -> 
      let f (Name (OccName "Constraint") _) = ConstraintT
          f x = ConT x
      in f <$> s pConAlpha
  ,pConSymT = \(S s) -> ConT <$> s pConSym

  ,pForallT = \(S s) ->
    let tyVarBndrs = s pForall *> pSpaces *> some (s pTyVarBndr) <* pSymbol "."
    in ForallT
        <$> tyVarBndrs
        <*> (s pCxt <* pToken "=>")
        <*> s pType


  ,pTupleT = \ (S s) -> 
      let appliedTupT c = f <$> pList1Sep pComma (s pType)
            where f [x] = x
                  f xs = foldl AppT (c (length xs)) xs
          unappliedTupT c = c . (+1) . length <$> some pComma

          tt con = appliedTupT con <|> unappliedTupT con
          
      in pSymbol "(#" *> tt UnboxedTupleT <* pSymbol "#)"
     <|> pParens (tt TupleT) 

  ,pArrowT = \ (S s) -> ArrowT <$ s pRArrow

  ,pLitT = \ _ -> fmap LitT $
          NumTyLit <$> pInteger
      <|> StrTyLit <$> pQuotedString

  ,pPromotedT = \ (S s) ->
    let alph = s (optQual $ \_ _ -> s pCon <|> s pVarAlpha)
        sym = pParens (s (optQual pVarSym))
        alphSym = alph <|> sym

        promotedT (Name (OccName ":") _) = PromotedConsT
        promotedT x = PromotedT x

    in pSym '\'' *> (promotedT <$> alphSym)

  ,pPromotedTupleT = \ (S s) ->
      let pList2Sep_ng sep p = (:) <$> p <*> (sep *> pList1Sep_ng sep p)
          notApp = PromotedTupleT . (+1) . length <$> some pComma
          app = f <$> pList2Sep_ng pComma (s pType)
          f xs = foldl AppT (PromotedTupleT (length xs)) xs
      in pSym '\'' *> pParens (notApp <|> app)

  ,pStarT          = \_     -> StarT       <$ pSymbol "*"

  ,pListT          = \(S s) ->
    let f Nothing [] = ListT
        f Nothing [x] = ListT `AppT` x
        f _ xs = foldr (\a b -> PromotedConsT `AppT` a `AppT` b) PromotedNilT xs
    in f <$> optional (pSym '\'') <*> pBrackets (pListSep pComma (s pType))
  ,pCxt            = \(S s) -> (:[]) <$> s pPred <|> pParens (some (s pPred))

  ,pPred = \(S s) -> ClassP <$> (s pCon <* pSpaces) <*> many (s pType)
            <|> EqualP <$> s pType' <*> (pSymbol "~" *> s pType)

  ,pKind = \(S s) -> s pType

  ,pTyVarBndr = \(S s) -> (PlainTV <$> s pVarAlpha) <|>
            pParens (KindedTV <$> s pVarAlpha <*> (s pDColon *> s pKind))

  ,pQual = \next (S s) -> addQual
                      <$> (pList1 (s pConAlpha <* pSym '.') <?> "module name")
                      <*> s next


  ,pLit = \ (S s) ->
    let toLit x | Just n <- readMaybe x = IntegerL n
          | otherwise = RationalL (toRational (read x :: Double))
    in
      StringL <$> pQuotedString
              <|> CharL <$> s pCharLit
              <|> toLit <$> micro pDoubleStr 1
{-
                <|> IntPrimL <$> pInteger
                <|> WordPrimL <$> pInteger
                 | FloatPrimL Rational
                 | DoublePrimL Rational
                 | StringPrimL [GHC.Word.Word8]
-}


  ,pCharLit = \ _ -> pSym '\'' <*
                  (pSatisfy (/= '\\') (Insertion "\\" '\\' 100)
                    <<|> ('\'' <$ pToken "\\'"))
                <* pSym '\''

  ,pVarSym    = \(S s)  -> toName [] <$> pList1 (s pSy)
  ,pVarAlpha  = \ (S s) -> toName [] <$> ((:) <$> pLower <*> pList (s pAlpha))
                  <* pSpaces
  ,pCon       = \ (S s) -> (s pConAlpha <|> s pConSym)
                  <* pSpaces
  ,pConAlpha  = \ (S s) -> (\ x xs -> toName [] (x:xs)) <$> pUpper <*> pList (s pAlpha)
  ,pConSym    = \ (S s) ->
    let sndSy    = (:) <$> s pSy <*> pList (s pSy <|> pSym ':')
        sndColon = (:) <$> pSym ':' <*> pList1 (s pSy <|> pSym ':')
    in fmap (toName []) $ (:) <$> pSym ':' <*> (sndSy <|> sndColon <<|> pure [])
                  

  ,pAlpha     = \ _s    -> pSatisfy (\x -> isAlpha x || x `elem` "'_") (Insertion "a" 'a' 100)
  ,pSy        = \ (S s)    -> pSatisfy (s isSy) (Insertion "+" '+' 100)

  ,isSy = \ _ x ->  (isSymbol x || x `elem` "-!#$%^&*.?<>~") && x /= '`'

  ,pLam       = \ _     -> () <$ pToken "\\"
  ,pRArrow    = \ _     -> () <$ pToken "->"
  ,pLArrow    = \ _     -> () <$ pToken "<-"
  ,pDColon    = \ _     -> () <$ pToken "::"
  ,pForall    = \ _     -> () <$ pToken "forall"


  ,pDotDot = \_ -> () <$ pToken ".."
  ,pSemi = \ _ -> () <$ pSym ';' -- or do layout here?
  ,pLBrace = \_ -> () <$ pSym '{'
  ,pRBrace = \_ -> () <$ pSym '}'
  }


-- | s (qual pVar)
qual :: (HParser -> HP Name) -> t -> HP Name
qual x _ (S s) = s $ \hp _ -> pQual hp x (S s)

optQual :: (HParser -> HP Name) -> t -> HP Name
optQual x _ (S s) = s $ \hp _ -> pQual hp x (S s) <|> x hp (S s)

toName :: [String] -> String -> Name
toName [] var = Name (OccName var) NameS
toName mn var = Name (OccName var) (NameQ (ModName (intercalate "." mn)))

addQual :: [Name] -> Name -> Name
addQual ms (Name (OccName var) NameS) = toName [ m | ~(Name (OccName m) NameS) <- ms ] var


