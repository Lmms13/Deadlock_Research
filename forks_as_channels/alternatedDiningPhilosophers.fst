type Fork = !Int;?();Close

philosopher : Int -> Fork -> dualof Fork 1-> ()
philosopher id left right = 
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    if(mod id 2 == 0) 
    then
        let left = send id left in
        let (_,left) = receive left in
        let (_,right) = receive right in
        let right = send () right in
        putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
        -- sendAndClose @() () l;
        -- receiveAndWait @() r
        close left;
        wait right
    else
        let (_,right) = receive right in
        let right = send () right in
        let left = send id left in
        let (_,left) = receive left in
        putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
        -- sendAndClose @() () l;
        -- receiveAndWait @() r
        wait right;
        close left

main : ()
main = 
    let (fw1, fr1) = new @Fork () in
    let (fw2, fr2) = new @Fork () in
    let (fw3, fr3) = new @Fork () in
    fork @() (\_:()1-> philosopher 1 fw1 fr3);
    fork @() (\_:()1-> philosopher 2 fw2 fr1);
    philosopher 3 fw3 fr2;
    print @String "Done!"
