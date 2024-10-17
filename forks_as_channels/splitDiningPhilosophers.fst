type Fork = !();?();Close

philosopher : Int -> Fork -> dualof Fork 1-> ()
philosopher id left right = 
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let left = send () left in
    let (_,right) = receive right in
    let (_,left) = receive left in
    let right = send () right in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    close left;
    wait right

splitPhilosopher : Int -> Fork -> dualof Fork 1-> ()
splitPhilosopher id left right = 
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    fork @() (\_:()1-> leftHand left);
    rightHand right;
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.")

leftHand : Fork -> ()
leftHand left = 
    let left = send () left in
    let (_,left) = receive left in
    close left

rightHand : dualof Fork -> ()
rightHand right = 
    let (_,right) = receive right in
    let right = send () right in
    wait right

main : ()
main = 
    let (fw1, fr1) = new @Fork () in
    let (fw2, fr2) = new @Fork () in
    let (fw3, fr3) = new @Fork () in
    fork @() (\_:()1-> philosopher 1 fw1 fr3);
    fork @() (\_:()1-> philosopher 2 fw2 fr1);
    splitPhilosopher 3 fw3 fr2;
    print @String "Done!"

