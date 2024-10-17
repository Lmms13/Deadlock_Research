type ForkExchange = !Int;?();Close

philosopher : Int -> ForkExchange 1-> ForkExchange 1-> ()
philosopher id left right =
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let left = send id left in
    let right = send id right in
    let (_, left) = receive left in
    let (_, right) = receive right in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    -- sendAndClose @() () l;
    -- sendAndClose @() () r
    close left;
    close right

fork_ : dualof ForkExchange -> dualof ForkExchange 1-> ()
fork_ left right =
    let (idr, right) = receive right in
    let (idl, left) = receive left in
    if(idl < idr) 
    then
        let left = send () left in
        wait left;
        -- receiveAndWait @() left;
        let right = send () right in
        wait right
        -- receiveAndWait @() right
    else
        let right = send () right in
        -- receiveAndWait @() right;
        wait right;
        let left = send () left in
        -- receiveAndWait @() left
        wait left

main : ()
main =
    let (p1, f1) = new @ForkExchange () in
    let (p2, f2) = new @ForkExchange () in
    let (p3, f3) = new @ForkExchange () in
    let (p4, f4) = new @ForkExchange () in
    let (p5, f5) = new @ForkExchange () in
    let (p6, f6) = new @ForkExchange () in
    fork @() (\_ : () 1-> fork_ f2 f1);
    fork @() (\_ : () 1-> fork_ f4 f3);
    fork @() (\_ : () 1-> fork_ f6 f5);
    fork @() (\_ : () 1-> philosopher 1 p1 p6);
    fork @() (\_ : () 1-> philosopher 2 p2 p3);
    philosopher 3 p4 p5;
    print @String "Done!"

