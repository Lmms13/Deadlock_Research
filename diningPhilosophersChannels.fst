type Fork = +{ Take : !();?();Fork, Put : !();?();Fork, Leave : Close }

philosopher : Int -> Fork -> Fork -> dualof Fork -> dualof Fork -> ()
philosopher id left right = 
    forkWith @Fork @() think |> dine id left right
    -- fork @() (\_:()1-> dine id lf1 rf1);
    -- think id lf2 rf2

dine : Int -> Fork -> Fork 1-> ()
dine id left right = 
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let l = select Take left in 
    let l = send () left in
    let (_,l) = receive l in
    let r = select Take right in
    let r = send () right in
    let (_,r) = receive r in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    let l = select Put left in
    let l = send () left in
    let (_,l) = receive l in
    let r = select Put right in
    let r = send () right in
    let (_,r) = receive r in 
    let l = select Leave left in
    close left;
    let r = select Leave right in
    close right

think : dualof Fork -> ()
think (Take f) = 
    let (_,f) = receive f in
    send () f
think (Put f) =
    let (_,f) = receive f in
    send () f
think (Leave f) = 
    wait f 

main : ()
main = 
    let (fw1, fr1) = new @Fork () in
    let (fw2, fr2) = new @Fork () in
    let (fw3, fr3) = new @Fork () in
    fork @() (\_:()1-> philosopher 1 fw1 fw3 fr1 fr3);
    fork @() (\_:()1-> philosopher 2 fw2 fw1 fr2 fr1);
    philosopher 3 fw3 fw2 fr3 fr2
