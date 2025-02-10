type Fork = !Int;?();!();Fork

philosopher : Int -> Fork -> dualof Fork 1-> ()
philosopher id left right = 
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let left = send id left in
    let (idr,right) = receive right in
    if(id < idr) 
    then
        putStrLn ((show @Int id) ^^ " receiving left.");
        let (_,left) = receive left in
        putStrLn ((show @Int id) ^^ " sending right.");
        let right = send () right in
        putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
        putStrLn ((show @Int id) ^^ " sending left.");
        let left = send () left in
        putStrLn ((show @Int id) ^^ " receiving right.");
        let (_,right) = receive right in
        philosopher id left right
    else
        putStrLn ((show @Int id) ^^ " sending right.");
        let right = send () right in
        putStrLn ((show @Int id) ^^ " receiving left.");
        let (_,left) = receive left in
        putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
        putStrLn ((show @Int id) ^^ " receiving right.");
        let (_,right) = receive right in
        putStrLn ((show @Int id) ^^ " sending left.");
        let left = send () left in
        philosopher id left right

main : ()
main = 
    let (fw1, fr1) = new @Fork () in
    let (fw2, fr2) = new @Fork () in
    let (fw3, fr3) = new @Fork () in
    let (fw4, fr4) = new @Fork () in
    let (fw5, fr5) = new @Fork () in
    fork @() (\_:()1-> philosopher 1 fw1 fr5);
    fork @() (\_:()1-> philosopher 2 fw2 fr1);
    fork @() (\_:()1-> philosopher 3 fw3 fr2);
    fork @() (\_:()1-> philosopher 4 fw4 fr3);
    -- philosopher 3 fw3 fr2;
    philosopher 5 fw5 fr4;
    print @String "Done!"
