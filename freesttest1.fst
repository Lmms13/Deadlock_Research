plus, minus : Int -> Int -> Int
plus x y = x + y
minus x y = x - y

betterDivision : Int -> Int -> (Int, Int)
betterDivision n div =
    let quotient = n / div in
    let rest = mod n div in
    (quotient, rest)

abs' : Int -> Int
abs' x = 
    if x > 0
    then x
    else -x

-- linIncrement : Int 1-> Int
-- linIncrement x = x + 1

type MathService = +{ Negate: !Int ; ?Int
                    , IsZero: !Int ; ?Bool
                    } ; Close

type MathServer = &{ Negate: ?Int ; !Int
                   , IsZero: ?Int ; !Bool
                   } ; Wait


--why does this work, but swapping Int and MathService throws an error?
mathClient : Int -> MathService -> Int
mathClient i c =
--   let c1 = select Negate c in
--   let c2 = send 5 c1 in
--   let (i, c3) = receive c2 in
--   close c3;
--   i
  c |> select Negate |> send i |> receiveAndClose @Int

mathServer : dualof MathService -> ()
mathServer (Negate c1) =
      let (i, c2) = receive c1 in
      sendAndWait @Int (-i) c2
mathServer (IsZero c1) =
      let (i, c2) = receive c1 in
      sendAndWait @Bool (i == 0) c2

-- main : Int
-- main = 
-- --     let (c,s) = new @(MathService) () in
-- --     fork @() (\_:() 1-> mathServer s);
-- --     mathClient 6 c
--     let c = forkWith @MathService @() mathServer in
--     fork (\_: () -> mathClient 7 c);
--     mathClient 8 c
--     forkWith @MathService @() mathServer |> mathClient 7;
--     forkWith @MathService @() mathServer |> mathClient 7;
--     forkWith @MathService @() mathServer |> mathClient 8
    
--     -- mathClient 7 $ forkWith @MathService @() mathServer

-- type IntStream = *!Int

-- produce : Int -> IntStream -> Diverge
-- produce n p = p |> send n |> produce n

-- consume : dualof IntStream -> Diverge
-- consume c =
--   c |> receive_ @Int |> print @Int ;
--   consume c

-- main : Diverge
-- main =
--   let c = forkWith @IntStream @() consume in
--   fork (\_: () -> produce 1 c);
--   produce 2 c

type NewInt = *!Int

sender : Int -> NewInt -> Diverge
sender i n = n |> send i |> sender i

receiver : Int -> dualof NewInt -> Diverge
receiver i c = 
    let n = receive_ @Int c in 
    if n == i 
    then print @Int n; receiver i  c
    else receiver (if i == 1 then 3 else i - 1) c



main : Diverge
main =
  let c = forkWith @NewInt @() (receiver 3) in
  fork (\_: () -> sender 1 c);
  fork (\_: () -> sender 2 c);
  sender 3 c