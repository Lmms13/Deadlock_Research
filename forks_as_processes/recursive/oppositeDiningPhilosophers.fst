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
    let (_,right) = receive right in
    let right = send () right in
    let (_,right) = receive right in
    let (_,left) = receive left in
    let left = send () left in
    let (_,left) = receive left in
    fork_ left right

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
    fork @() (\_ : () 1-> fork_ f5 f6); --here, the forks are passed in the opposite order
    fork @() (\_ : () 1-> philosopher 1 p1 p6);
    fork @() (\_ : () 1-> philosopher 2 p3 p2);
    philosopher 3 p5 p4;
    print @String "Done!"

