-- type Fork = !();?();Close
type Fork = +{Request: !();Fork, Acquire: ?();Fork, Leave: Close}

philosopher : Int -> Fork -> dualof Fork 1-> ()
philosopher id left right = 
    fork @() (\_:()1-> forkServer right);
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let left = left |> select Request |> send () in
    let (_,left) = left |> select Acquire |> receive in 
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
    fork @() (\_:()1-> philosopher 1 fw1 fr3);
    -- fork @() (\_:()1-> forkServer fr3);
    fork @() (\_:()1-> philosopher 2 fw2 fr1);
    -- fork @() (\_:()1-> forkServer fr1);
    -- fork @() (\_:()1-> philosopher 3 fw3);
    philosopher 3 fw3 fr2;
    -- forkServer fr2;
    print @String "Done!"