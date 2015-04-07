{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{- | This doesn't work as well as it could.


Other work (using parsec):

https://github.com/luqui/parsec-layout/blob/master/Text/Parsec/Layout.hs

http://dev.stephendiehl.com/fun/008_extended_parser.html

https://github.com/glguy/config-value/blob/503f6f34b8ebeeec14f21b320290ec97634fdd28/src/Parser.hs

-}
module UULayout (
  module UULayout,
  module Text.ParserCombinators.UU,
  module Text.ParserCombinators.UU.Idioms,
  module Text.ParserCombinators.UU.BasicInstances,
  module Text.ParserCombinators.UU.Derived,
  module Text.ParserCombinators.UU.Utils,
  ) where
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Idioms
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Derived

import Text.ParserCombinators.UU.Utils hiding (pLBrace, pRBrace, execParser, runParser, pSpaces)

import Text.Printf
import Data.Maybe

import Test.Hspec
import Test.QuickCheck


-- | using this instead of Text.ParserCombinators.UU.Utils.pSpaces
-- helps with one test case "f x = 1\nf x = 2"
-- but it interferes with another "1\n + 2"
pSpaces = pListSep (pAny pSym "\n\r") (pAny pSym "\t " *> pMunch (`elem` "\t ") )
        -- <?> "whitespace"

uulayoutTests = hspec $ do
  it "implicit layout" $ do
      runParser "msg" (laidout pNat)
        " 1\n\
        \ 2"
        `shouldBe` [1,2]
  it "explicit layout" $ do
      runParser "msg" (laidout pNat) "{1;2}"
        `shouldBe` [1,2]
  it "partially explicit" $ do
      runParser "msg" (laidout pNat) "1;2"
        `shouldBe` [1,2]

  it "doE" $ property $ \ (NonNegative n) ->
    let nsp x = replicate n ' ' ++ x
    in
      runParser "doE" (pSymbol "do" *> laidout pNat)
         (unlines ["do",
          nsp "1",
          "",
          nsp "2",
          nsp "3"])
          === [1,2,3]

-- not used see pLaidout
laidout e = do
    pSpaces
    lbrace <- optional (pSym '{')
    LineColPos _ c' _ <- pPos
    pList_ng
        (do
          munched <- pMunch (`elem` " \t\n;")
          LineColPos _ c _ <- pPos
          guard (isJust lbrace || ';' `elem` munched || c == c')
          e)
      <* if isJust lbrace then pSym '}' else pure '}'


-- * copy paste (because UU.Utils exports versions that do not allow accessing the LineColPos)

-- | The lower-level interface. Returns all errors. 
-- execParser :: Parser a -> String -> (a, [Error LineColPos])
execParser p = parse_h ((,) <$> p <*> pEnd) . createStr (LineColPos 0 0 0)


-- runParser :: String -> Parser a -> String -> a
runParser inputName p s | (a,b) <- execParser p s =
    if null b
    then a
    else error (printf "Failed parsing '%s' :\n%s\n" inputName (pruneError s b))
         -- We do 'pruneError' above because otherwise you can end
         -- up reporting huge correction streams, and that's
         -- generally not helpful... but the pruning does discard info...
    where -- | Produce a single simple, user-friendly error message
          pruneError :: String -> [Error LineColPos] -> String
          pruneError _ [] = ""
          pruneError _ (DeletedAtEnd x     : _) = printf "Unexpected '%s' at end." x
          pruneError s (Inserted _ pos exp : _) = prettyError s exp pos
          pruneError s (Deleted  _ pos exp : _) = prettyError s exp pos
          prettyError :: String -> [String] -> LineColPos -> String
          prettyError s exp p@(LineColPos line c abs) = printf "Expected %s at %s :\n%s\n%s\n%s\n"
                                                           (show_expecting p exp)
                                                           (show p)
                                                           aboveString
                                                           inputFrag
                                                           belowString
                             where
                                s' = map (\c -> if c=='\n' || c=='\r' || c=='\t' then ' ' else c) s
                                aboveString = replicate 30 ' ' ++ "v"
                                belowString = replicate 30 ' ' ++ "^"
                                inputFrag   = replicate (30 - abs) ' ' ++ (take 71 $ drop (abs - 30) s')

