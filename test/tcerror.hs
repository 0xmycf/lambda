{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use id" #-}
{-# LANGUAGE ExplicitForAll #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Collapse lambdas" #-}
{-# HLINT ignore "Use camelCase" #-}

module Main where

main :: IO ()
main = print "Hello world"

true, false :: Bool
true = True
false = False


x = 10

bar = ( (\x -> (\ y -> (y x))) x )

let_in =
  let foo = ( (\x -> (\ y -> (y x))) x )
  in foo (\x -> x)

another = \x -> x 1

x2 :: forall t. (Bool -> Integer -> t) -> t
x2 =
  \ arg ->
    let z = \ l -> l in
    arg (z true) (z 10)

-- tc_error_dot_lambda =
--     let foo = ( (\x -> (\y -> (y x))) x )
--     in foo 10

