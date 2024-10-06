type Fork = +{ Take : !();?();Fork, Put : !();?();Fork, Leave : Close }
type Fork' = +{ Dine : !();?();Fork, Take : !();?();Fork, Put1 : !();?();Fork, Put2 : ?();!();Fork, Leave : Close }


philosopher : Int -> Fork -> Fork -> dualof Fork -> dualof Fork -> ()
philosopher id left right = 
    forkWith @Fork @() think |> dine id left right
    -- fork @() (\_:()1-> dine id lf1 rf1);
    -- think id lf2 rf2

-- philosopher' : Int -> Fork' -> dualof Fork' -> ()
-- philosopher' id left right = 
--     fork @() (\_:()1-> dine' id f);
--     think' id c

dine' : Int -> Fork' -> ()
dine' id left right =
    let l = select Dine left in
    let l = send () l in
    let (_,l) = receive l in

    -- let r = select Dine right in
    -- let r = receive r in
    -- let r = send () r in

    let l = select Take1 left in
    let l = send () l in
    let (_,l) = receive l in

    -- let r = select Take2 right in
    -- let (_,r) = receive r in
    -- let r = send () r in

    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");

    let l = select Put1 left in
    let l = send () l in
    let (_,l) = receive l in

    -- let r = select Put2 right in
    -- let r = receive r in
    -- let r = send () r in

    let l = select Leave left in
    close l

    -- let r = select Leave right in
    -- wait r


dine : Int -> Fork -> Fork 1-> ()
dine id left right = 
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let l = select Take left in 
    let l = send () l in
    let (_,l) = receive l in
    let r = select Take right in
    let r = send () r in
    let (_,r) = receive r in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    let l = select Put l in
    let l = send () l in
    let (_,l) = receive l in
    let r = select Put r in
    let r = send () r in
    let (_,r) = receive r in 
    let l = select Leave l in
    close l;
    let r = select Leave r in
    close r

think : dualof Fork -> ()
think (Take f) = 
    let (_,f) = receive f in
    send () f
think (Put f) =
    let (_,f) = receive f in
    send () f
think (Leave f) = 
    wait f 

think' : Int -> dualof Fork' -> ()
think' id (Dine f) = 
    let (_,f) = receive f in
    send () f
think' id (Take1 f) =
    let (_,f) = receive f in
    send () f
think' id (Take2 f) =
    let f = send () f in
    receive f
think' id (Put1 f) =
    let (_,f) = receive f in
    send () f
think' id (Put2 f) =
    let f = send () f in
    receive f
think' id (Leave f) =
    wait f

main : ()
main = 
    let (fw1, fr1) = new @Fork () in
    let (fw2, fr2) = new @Fork () in
    let (fw3, fr3) = new @Fork () in
    fork @() (\_:()1-> philosopher 1 fw1 fw3 fr1 fr3);
    fork @() (\_:()1-> philosopher 2 fw2 fw1 fr2 fr1);
    philosopher 3 fw3 fw2 fr3 fr2
