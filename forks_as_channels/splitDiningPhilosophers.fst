type Fork = !();?();!();Close

philosopher : Int -> Fork -> dualof Fork 1-> ()
philosopher id left right = 
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let l = send () left in
    let (_,r) = receive right in
    let (_,l) = receive l in
    let r = send () r in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    sendAndClose @() () l;
    receiveAndWait @() r

splitPhilosopher : Int -> Fork -> dualof Fork 1-> ()
splitPhilosopher id left right = 
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    fork @() (\_:()1-> leftHand left);
    rightHand right;
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.")

leftHand : Fork -> ()
leftHand left = 
    let l = send () left in
    let (_,l) = receive l in
    sendAndClose @() () l

rightHand : dualof Fork -> ()
rightHand right = 
    let (_,r) = receive right in
    let r = send () r in
    receiveAndWait @() r

main : ()
main = 
    let (fw1, fr1) = new @Fork () in
    let (fw2, fr2) = new @Fork () in
    let (fw3, fr3) = new @Fork () in
    fork @() (\_:()1-> philosopher 1 fw1 fr3);
    fork @() (\_:()1-> philosopher 2 fw2 fr1);
    splitPhilosopher 3 fw3 fr2

