type ForkExchange = !Int;?();Close

philosopher : Int -> ForkExchange 1-> ForkExchange 1-> ()
philosopher id left right =
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let left = send id left in
    let right = send id right in
    let (_,left) = receive left in
    let (_,right) = receive right in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    close left;
    close right

fork_ : dualof ForkExchange -> dualof ForkExchange 1-> ()
fork_ left right =
    let (id,right) = receive right in
    let (_,left) = receive left in
    if(even id) 
    then
        unitaryFork right;
        unitaryFork left
    else
        unitaryFork left;
        unitaryFork right

unitaryFork : !();Wait -> ()
unitaryFork f =
    let f = send () f in
    wait f

main : ()
main =
    let (p1, f1) = new @ForkExchange () in
    let (p2, f2) = new @ForkExchange () in
    let (p3, f3) = new @ForkExchange () in
    let (p4, f4) = new @ForkExchange () in
    let (p5, f5) = new @ForkExchange () in
    let (p6, f6) = new @ForkExchange () in
    let (p7, f7) = new @ForkExchange () in
    let (p8, f8) = new @ForkExchange () in
    let (p9, f9) = new @ForkExchange () in
    let (p10, f10) = new @ForkExchange () in
    fork @() (\_ : () 1-> fork_ f1 f2);
    fork @() (\_ : () 1-> fork_ f3 f4);
    fork @() (\_ : () 1-> fork_ f5 f6);
    fork @() (\_ : () 1-> fork_ f7 f8);
    fork @() (\_ : () 1-> fork_ f9 f10);
    fork @() (\_ : () 1-> philosopher 1 p1 p10);
    fork @() (\_ : () 1-> philosopher 2 p3 p2);
    fork @() (\_ : () 1-> philosopher 3 p5 p4);
    fork @() (\_ : () 1-> philosopher 4 p7 p6);
    philosopher 5 p9 p8;
    print @String "Done!"

