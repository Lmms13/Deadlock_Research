type ForkExchange = !();?();Close

philosopher : Int -> ForkExchange 1-> ForkExchange 1-> ()
philosopher id left right =
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let left = send () left in
    let right = send () right in
    let (_, left) = receive left in
    let (_, right) = receive right in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    close left;
    close right
    -- sendAndClose @() () l;
    -- sendAndClose @() () r

fork_ :dualof ForkExchange -> dualof ForkExchange 1-> ()
fork_ left right =
    let (_,right) = receive right in
    let right = send () right in
    wait right;
    -- receiveAndWait @() right; 
    let (_,left) = receive left in
    let left = send () left in
    wait left
    -- receiveAndWait @() left

-- oppositeFork : dualof ForkExchange -> dualof ForkExchange 1-> ()
-- oppositeFork left right =
--     let (_,left) = receive left in
--     let left = send () left in
--     wait left;
--     -- receiveAndWait @() left;
--     let (_,right) = receive right in
--     let right = send () right in
--     wait right
--     -- receiveAndWait @() right

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
    fork @() (\_ : () 1-> fork_ f5 f6); --here, the forks are passed in the opposite order
    -- fork @() (\_ : () 1-> oppositeFork f6 f5);
    fork @() (\_ : () 1-> philosopher 1 p1 p6);
    fork @() (\_ : () 1-> philosopher 2 p2 p3);
    philosopher 3 p4 p5;
    print @String "Done!"

