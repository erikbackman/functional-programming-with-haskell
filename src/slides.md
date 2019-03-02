---
title: Funktionell Programmering
subtitle: med Haskell
author: Erik Bäckman
date: April 2019
---

## Agenda

* Introduktion till Funktionell Programmering
* Ren Funktionell Programmering med Haskell
* Live-kod

<div class="notes">
This is my note.

- It can contain Markdown
- like this list

</div>

## Programmera med matematiska funktioner

Likhet innebär att vi alltid kan ersätta uttryck med dess värde och vice
versa. Detta kallas ofta för _referenstransparens_.
```{.haskell}
square x = x * x

p = square 2
```
. . .
```{.haskell}
p = 2 * 2
```
. . .
```{.haskell}
p = 4
```
Dubbelriktad substitution är alltid möjligt eftersom Haskell är rent.</br>
Ett Haskell-program är ett enda stort referentiellt transparent uttryck.

## Ren Funktionell Programmering med sido-effekter

En effekt är ett värde.
```{.haskell}
getLine :: IO String             -- En effekt som producerar en `String`

putStrLn :: String -> IO ()      -- En funktion från `String` till en effekt
```

Det enda sättet att exekvera en effekt är genom att likställa den med main

```{.haskell}
x = putStrLn "Goodbye World"

main :: IO ()
main = putStrLn "Hello World"
```

```
> ./example
Hello World
```

## Refaktorering
Vi kan alltid simplifiera ett program och vara säkra på att programmet inte ändrats.
```{.haskell}
p = f x + f y * (f x - f x)
```
. . . 

```{.haskell}
p = f x + f y * 0
```

. . . 

```{.haskell}
p = f x + 0
```

. . . 

```{.haskell}
p = f x
```
Om f inte är referentiellt transparent blir det svårare att resonera om vårt
program eftersom vi mentalt måste simulera programmet och dess tillstånd.

## Ekvationella resonemang
Funktionell programmering och referenstransparens möjliggör för ekvationella
resonemang om ett programs egenskaper.

```{.haskell}
reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]
```
. . . 

Vi kan bevisa att [x] = [x], för alla x

```{.haskell}
reverse [x] =
reverse (x: []) =
reverse [] ++ [x] =
[] ++ [x] =
[x]
```

## Parametrisk polymorfism

Typer kan ofta guida oss till en korrekt implementation
```{.haskell}
id :: forall a. a -> a
```
. . .

```{.haskell}
const :: a -> b -> a
```
. . .

```{.haskell}
flip :: (a -> b -> c) -> b -> a -> c
```

## Ad-hoc polymorfism

```{.haskell}
(+) :: Num a => a -> a -> a 
```

```{.haskell}
(==) :: Eq a => a -> a -> Bool
```

```{.haskell}
compare :: Ord a => a -> a -> Ordering
```


## 1
```{.haskell include=src/examples/Examples.hs snippet=simple-sum-product}
```
## 2
```{.haskell emphasize=2:14-2:14,3:16-3:16,6:18-6:18,7:20-7:20 include=src/examples/Examples.hs snippet=simple-sum-product}
```

