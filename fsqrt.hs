module FastSQRT
where

--import qualified Data.Map as M

{- initialGuess :: Integer -> Integer -}
initialGuess x = let
	d = (length . show) x
	in
	if odd d then
		2*10^((d-1) `div` 2)
	else
		6*10^((d-2) `div` 2)

{- nextStep :: (Fractional t) => t -> t -> t -}
nextStep x x0 = let
	fx0 = x0*x0 - x
	f'x0 = 2*x0
	s = (x0 - fx0/f'x0)
	in
	(s, abs(s-x0))

fsqrtWhile x x0 err = let
	(next, err') = nextStep x x0
	in
	if err' < err then
		next
	else
		fsqrtWhile x next err

{- fsqrt :: (Integral t) => t -> t -}
fsqrt x = let
	x0 = initialGuess x
	in
	fsqrtWhile x x0 0.5

{- hasIntSqrt :: (Fractional a) => a -> Bool -}
hasIntSqrt x = let
	s = fsqrt x
	err = abs ( (fromIntegral $ truncate(s)) - s )
	in
	err < 0.5
