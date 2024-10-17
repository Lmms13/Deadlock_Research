type Fork = !();?();Close

philosopher : Int -> Fork -> dualof Fork 1-> ()
philosopher id left right = 
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let left = send () left in
    let (_,right) = receive right in
    let (_,left) = receive left in
    let right = send () right in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    -- sendAndClose @() () l;
    -- receiveAndWait @() r
    close left;
    wait right

oppositePhilosopher : Int -> Fork -> dualof Fork 1-> ()
oppositePhilosopher id left right =
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let (_,right) = receive right in
    let left = send () left in
    let right = send () right in
    let (_,left) = receive left in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    -- receiveAndWait @() r;
    -- sendAndClose @() () l
    wait right;
    close left

main : ()
main = 
    let (fw1, fr1) = new @Fork () in
    let (fw2, fr2) = new @Fork () in
    let (fw3, fr3) = new @Fork () in
    fork @() (\_:()1-> philosopher 1 fw1 fr3);
    fork @() (\_:()1-> philosopher 2 fw2 fr1);
    oppositePhilosopher 3 fw3 fr2;
    print @String "Done!"
