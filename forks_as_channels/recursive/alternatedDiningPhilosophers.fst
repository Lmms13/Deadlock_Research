type Fork = !();?();!();Fork

philosopher : Int -> Fork -> dualof Fork 1-> ()
philosopher id left right = 
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    if(even id) 
    then
        let left = send () left in
        let (_,left) = receive left in
        let (_,right) = receive right in
        let right = send () right in
        putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
        let left = send () left in
        let (_,right) = receive right in
        philosopher id left right
    else
        let (_,right) = receive right in
        let right = send () right in
        let left = send () left in
        let (_,left) = receive left in
        putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
        let (_,right) = receive right in
        let left = send () left in
        philosopher id left right

main : ()
main = 
    let (fw1, fr1) = new @Fork () in
    let (fw2, fr2) = new @Fork () in
    let (fw3, fr3) = new @Fork () in
    let (fw4, fr4) = new @Fork () in
    fork @() (\_:()1-> philosopher 1 fw1 fr4);
    fork @() (\_:()1-> philosopher 2 fw2 fr1);
    fork @() (\_:()1-> philosopher 3 fw3 fr2);
    -- philosopher 3 fw3 fr2;
    philosopher 4 fw4 fr3;
    print @String "Done!"
