import Test.Hspec
import Application
import Control.Monad.Writer (writer, runWriter)
import Data.IORef

main = hspec $ do
    describe "a test with no stub input and no mock output" $ do 
        it "requires real input" $ do 
            let inputFunction = getLine 
                outputFunction = putStrLn
            application inputFunction outputFunction

    describe "a test with one stub input and mock output" $ do
        it "should execute without read input" $ do
            let inputFunction = return "foo"
                outputFunction = \s -> writer ((), s)
                run = application inputFunction outputFunction
                result = lines (snd (runWriter run))
            result `shouldBe` ["foofoo"]

    describe "a test with one variable stup input and mock output" $ do
        it "should execute without read input" $ do
            ref <- newIORef "foo"
            let inputFunctionWithRef ref = do
                s <- readIORef ref
                return s
            let outputFunctionWithRef ref s = do
                writeIORef ref s

            let inputFunction = inputFunctionWithRef ref
                outputFunction = outputFunctionWithRef ref
            application inputFunction outputFunction
            result <- readIORef ref
            result `shouldBe` "foofoo"



