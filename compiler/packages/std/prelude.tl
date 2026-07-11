import std/intrinsics

export >> = ma f. io_join (io_map f ma)
export *> = ma mb. io_join (io_map (const mb) ma)
export pure = io_pure
export getLine = io_getLine
export putStrLn = io_putStrLn
export readFile = io_readFile
export writeFile = io_writeFile

export id = x. x
export const = x y. x
export k = const
export flip = f a b. f b a

export $ = f a. f a
infixr 0 $
export | = a f. f a
infixr 2 |
export ; = f a. f a
infixl 0.1 ;
export ;; = a f. f a
infixr 6 ;;

export & = meld
infixr 6 &
