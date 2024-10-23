-- type Fork = !();?();Close
type Fork = +{Request: !();Fork, Acquire: ?();Fork, Leave: Close}

philosopher : Int -> Fork 1-> ()
philosopher id left = 
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let left = select Request left in
    let left = send () left in
    let left = select Acquire left in
    let (_,left) = receive left in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    left |> select Leave |> close

forkServer : dualof Fork -> ()
forkServer (Request f) = let (_,f) = receive f in forkServer f
forkServer (Acquire f) = let f = send () f in forkServer f
forkServer (Leave f) = wait f 

main : ()
main = 
    -- forkWith @Fork @() forkServer |> philosopher 1;
    -- forkWith @Fork @() forkServer |> philosopher 2;
    -- forkWith @Fork @() forkServer |> philosopher 3;
    let (fw1, fr1) = new @Fork () in
    let (fw2, fr2) = new @Fork () in
    let (fw3, fr3) = new @Fork () in
    fork @() (\_:()1-> philosopher 1 fw1);
    fork @() (\_:()1-> forkServer fr3);
    fork @() (\_:()1-> philosopher 2 fw2);
    fork @() (\_:()1-> forkServer fr1);
    fork @() (\_:()1-> philosopher 3 fw3);
    forkServer fr2;
    print @String "Done!"