-- type Fork = !();?();Close
type Fork = *!();*?();Close

philosopher : Int -> Fork -> Fork -> ()
philosopher id left right = 
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let left = send () left in
    let right = send () right in
    let (_,left) = receive left in
    let (_,right) = receive right in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    close left;
    close right
    -- let left = send () left in
    -- let right = send () right in
    -- philosopher id left right

forkManager : dualof Fork -> ()
forkManager f = 
    let (_,f) = receive f in
    let f = send () f in
    wait f
    -- let (_,f) = receive f in
    -- forkManager f

main : ()
main = 
    -- forkWith @Fork @() forkManager |> philosopher 1;
    -- forkWith @Fork @() forkManager |> philosopher 2;
    -- forkWith @Fork @() forkManager |> philosopher 3;
    -- let (fw1, fr1) = new @Fork () in
    -- let (fw2, fr2) = new @Fork () in
    -- let (fw3, fr3) = new @Fork () in
    -- fork @() (\_:()1-> philosopher 1 fw1 fr3);
    -- fork @() (\_:()1-> philosopher 2 fw2 fr1);
    -- philosopher 3 fw3 fr2;
    let f1 = forkWith @Fork @() forkManager in
    let f2 = forkWith @Fork @() forkManager in
    let f3 = forkWith @Fork @() forkManager in
    fork @() (\_:() -> philosopher 1 f1 f3);
    fork @() (\_:() -> philosopher 2 f2 f1);
    philosopher 3 f3 f2;
    print @String "Done!"
