type Fork = !Int;?();Close

philosopher : Int -> Fork -> dualof Fork 1-> ()
philosopher id left right = 
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    if(mod id 2 == 0) 
    then
        let l = send id left in
        let (_,l) = receive l in
        let (_,r) = receive right in
        let r = send () r in
        putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
        -- sendAndClose @() () l;
        -- receiveAndWait @() r
        close l;
        wait r
    else
        let (_,r) = receive right in
        let r = send () r in
        let l = send id left in
        let (_,l) = receive l in
        putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
        -- sendAndClose @() () l;
        -- receiveAndWait @() r
        wait r;
        close l

main : ()
main = 
    let (fw1, fr1) = new @Fork () in
    let (fw2, fr2) = new @Fork () in
    let (fw3, fr3) = new @Fork () in
    fork @() (\_:()1-> philosopher 1 fw1 fr3);
    fork @() (\_:()1-> philosopher 2 fw2 fr1);
    philosopher 3 fw3 fr2;
    print @String "Done!"
