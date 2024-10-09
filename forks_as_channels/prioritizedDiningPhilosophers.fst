type Fork = !Int;?();!();Close

philosopher : Int -> Fork -> dualof Fork 1-> ()
philosopher id left right = 
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let l = send id left in
    let (idr,r) = receive right in
    if(id < idr) 
    then
        let (_,l) = receive l in
        let r = send () r in
        putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
        sendAndClose @() () l;
        receiveAndWait @() r
    else
        let r = send () r in
        let (_,l) = receive l in
        putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
        sendAndClose @() () l;
        receiveAndWait @() r

main : ()
main = 
    let (fw1, fr1) = new @Fork () in
    let (fw2, fr2) = new @Fork () in
    let (fw3, fr3) = new @Fork () in
    fork @() (\_:()1-> philosopher 1 fw1 fr3);
    fork @() (\_:()1-> philosopher 2 fw2 fr1);
    philosopher 3 fw3 fr2
