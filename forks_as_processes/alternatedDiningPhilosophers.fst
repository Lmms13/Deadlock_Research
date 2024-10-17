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
    let (id, right) = receive right in
    if(mod id 2 == 0) 
    then
        let right = send () right in
        wait right;
        -- receiveAndWait @() right;
        let (_ ,left) = receive left in
        let left = send () left in
        wait left
        -- receiveAndWait @() left
    else
        let (_ ,left) = receive left in
        let left = send () left in
        wait left;
        -- receiveAndWait @() left;
        let right = send () right in
        wait right
        -- receiveAndWait @() right

main : ()
main =
    let (p1, f1) = new @ForkExchange () in
    let (p2, f2) = new @ForkExchange () in
    let (p3, f3) = new @ForkExchange () in
    let (p4, f4) = new @ForkExchange () in
    let (p5, f5) = new @ForkExchange () in
    let (p6, f6) = new @ForkExchange () in
    fork @() (\_ : () 1-> fork_ f1 f2);
    fork @() (\_ : () 1-> fork_ f3 f4);
    fork @() (\_ : () 1-> fork_ f5 f6);
    fork @() (\_ : () 1-> philosopher 1 p1 p6);
    fork @() (\_ : () 1-> philosopher 2 p2 p3);
    philosopher 3 p4 p5;
    print @String "Done!"

