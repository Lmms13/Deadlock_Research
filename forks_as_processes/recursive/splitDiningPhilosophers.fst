type Hand = !();?();!();Hand

philosopher : Int -> Hand 1-> Hand 1-> ()
philosopher id left right =
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let left = send () left in
    let right = send () right in
    let (_, left) = receive left in
    let (_, right) = receive right in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    let left = send () left in
    let right = send () right in
    philosopher id left right

fork_ : dualof Hand -> dualof Hand 1-> ()
fork_ left right =
    let right = unitaryFork right in
    let left = unitaryFork left in
    fork_ left right

unitaryFork : dualof Hand -> dualof Hand
unitaryFork f =
    let (_,f) = receive f in
    let f = send () f in
    let (_, f) = receive f in
    f

recUnitaryFork : dualof Hand -> ()
recUnitaryFork f =
    let (_,f) = receive f in
    let f = send () f in
    let (_, f) = receive f in
    recUnitaryFork f

main : ()
main =
    let (p1, f1) = new @Hand () in
    let (p2, f2) = new @Hand () in
    let (p3, f3) = new @Hand () in
    let (p4, f4) = new @Hand () in
    let (p5, f5) = new @Hand () in
    let (p6, f6) = new @Hand () in
    fork @() (\_ : () 1-> fork_ f2 f1);
    fork @() (\_ : () 1-> fork_ f4 f3);
    fork @() (\_ : () 1-> recUnitaryFork f6); --left
    fork @() (\_ : () 1-> recUnitaryFork f5); --right
    fork @() (\_ : () 1-> philosopher 1 p1 p6);
    fork @() (\_ : () 1-> philosopher 2 p2 p3);
    philosopher 3 p4 p5;
    print @String "Done!"

