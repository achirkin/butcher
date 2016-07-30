module Main where



#include "qprelude/bundle-gamma.inc"

import Test.Hspec

-- import NeatInterpolation

import UI.Butcher.Monadic



main :: IO ()
main = hspec $ tests

tests :: Spec
tests = do
  describe "checkTests" checkTests
  describe "simpleParseTest" simpleParseTest
  describe "simpleRunTest" simpleRunTest


checkTests :: Spec
checkTests = do
  before_ pending $ it "check001" $ True `shouldBe` True


simpleParseTest :: Spec
simpleParseTest = do
  it "failed parse 001" $ cmdRunParser Nothing (InputString "foo") testCmd1
         `shouldSatisfy` Data.Either.Combinators.isLeft . snd
  it "toplevel" $ (testParse testCmd1 "" >>= _cmd_out)
                  `shouldSatisfy` Maybe.isNothing
  it "hasImpl 001" $ (testParse testCmd1 "abc" >>= _cmd_out)
                  `shouldSatisfy` Maybe.isJust
  it "hasImpl 002" $ (testParse testCmd1 "def" >>= _cmd_out)
                  `shouldSatisfy` Maybe.isJust


simpleRunTest :: Spec
simpleRunTest = do
  it "failed run" $ testRun testCmd1 "" `shouldBe` Right Nothing
  describe "no reordering" $ do
    it "cmd 1" $ testRun testCmd1 "abc" `shouldBe` Right (Just 100)
    it "cmd 2" $ testRun testCmd1 "def" `shouldBe` Right (Just 200)
    it "flag 1" $ testRun testCmd1 "abc -f" `shouldBe` Right (Just 101)
    it "flag 2" $ testRun testCmd1 "abc --flong" `shouldBe` Right (Just 101)
    it "flag 3" $ testRun testCmd1 "abc -f -f" `shouldBe` Right (Just 101)
    it "flag 4" $ testRun testCmd1 "abc -f -g" `shouldBe` Right (Just 103)
    it "flag 5" $ testRun testCmd1 "abc -f -g -f" `shouldSatisfy` Data.Either.Combinators.isLeft -- no reordering
    it "flag 6" $ testRun testCmd1 "abc -g -f" `shouldSatisfy` Data.Either.Combinators.isLeft -- no reordering
    it "flag 7" $ testRun testCmd1 "abc -g -g" `shouldBe` Right (Just 102)
  describe "with reordering" $ do
    it "cmd 1" $ testRun testCmd2 "abc" `shouldBe` Right (Just 100)
    it "cmd 2" $ testRun testCmd2 "def" `shouldBe` Right (Just 200)
    it "flag 1" $ testRun testCmd2 "abc -f" `shouldBe` Right (Just 101)
    it "flag 2" $ testRun testCmd2 "abc --flong" `shouldBe` Right (Just 101)
    it "flag 3" $ testRun testCmd2 "abc -f -f" `shouldBe` Right (Just 101)
    it "flag 4" $ testRun testCmd2 "abc -f -g" `shouldBe` Right (Just 103)
    it "flag 5" $ testRun testCmd2 "abc -f -g -f" `shouldBe` Right (Just 103)
    it "flag 6" $ testRun testCmd2 "abc -g -f" `shouldBe` Right (Just 103)
    it "flag 7" $ testRun testCmd2 "abc -g -g" `shouldBe` Right (Just 102)
  describe "with action" $ do
    it "flag 1" $ testRunA testCmd3 "abc" `shouldBe` Right 0
    it "flag 2" $ testRunA testCmd3 "abc -f" `shouldBe` Right 1
    it "flag 3" $ testRunA testCmd3 "abc -g" `shouldBe` Right 2
    it "flag 4" $ testRunA testCmd3 "abc -f -g" `shouldBe` Right 3
    it "flag 5" $ testRunA testCmd3 "abc -g -f" `shouldBe` Right 3


testCmd1 :: CmdParser Identity (WriterS.Writer (Sum Int) ()) ()
testCmd1 = do
  addCmd "abc" $ do
    f <- addSimpleBoolFlag "f" ["flong"] mempty
    g <- addSimpleBoolFlag "g" ["glong"] mempty
    addCmdImpl $ do
      when f $ WriterS.tell 1
      when g $ WriterS.tell 2
      WriterS.tell 100
  addCmd "def" $ do
    addCmdImpl $ do
      WriterS.tell 200

testCmd2 :: CmdParser Identity (WriterS.Writer (Sum Int) ()) ()
testCmd2 = do
  addCmd "abc" $ do
    reorderStart
    f <- addSimpleBoolFlag "f" ["flong"] mempty
    g <- addSimpleBoolFlag "g" ["glong"] mempty
    reorderStop
    addCmdImpl $ do
      when f $ WriterS.tell 1
      when g $ WriterS.tell 2
      WriterS.tell 100
  addCmd "def" $ do
    addCmdImpl $ do
      WriterS.tell 200

testCmd3 :: CmdParser (StateS.State Int) () ()
testCmd3 = do
  addCmd "abc" $ do
    reorderStart
    addSimpleFlagA "f" ["flong"] mempty (StateS.modify (+1))
    addSimpleFlagA "g" ["glong"] mempty (StateS.modify (+2))
    reorderStop
    addCmdImpl ()
  addCmd "def" $ do
    addCmdImpl ()

testParse :: CmdParser Identity out () -> String -> Maybe (CommandDesc out)
testParse cmd s = either (const Nothing) Just
                $ snd
                $ cmdRunParser Nothing (InputString s) cmd

testRun :: CmdParser Identity (WriterS.Writer (Sum Int) ()) () -> String -> Either ParsingError (Maybe Int)
testRun cmd s = fmap (fmap (getSum . WriterS.execWriter) . _cmd_out)
              $ snd
              $ cmdRunParser Nothing (InputString s) cmd

testRunA :: CmdParser (StateS.State Int) () () -> String -> Either ParsingError Int
testRunA cmd str = (\((_, e), s) -> e $> s)
                 $ flip StateS.runState (0::Int)
                 $ cmdRunParserA Nothing (InputString str) cmd