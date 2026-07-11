import std/intrinsics

export >> = ma f. io_join (io_map f ma)
export getLine = io_getLine
export putStrLn = io_putStrLn

export id = x. x
export const = x y. x
export flip = f a b. f b a
