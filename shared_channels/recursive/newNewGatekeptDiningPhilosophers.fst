type SharedFork = *?Fork
type Fork = Close

type Waiter = *!()

philosopher : Int -> Waiter -> SharedFork -> SharedFork -> ()
philosopher id c left right = 
    let l = receive_ @Fork left in
    putStrLn $ "Philosopher " ^^ (show @Int id) ^^ " acquired left fork.";
    let r = receive_ @Fork right in
    putStrLn $ "Philosopher " ^^ (show @Int id) ^^ " is eating.";
    close l;
    close r;
    send () c; 
    philosopher id c left right

forkServer : dualof SharedFork -> ()
forkServer sf =
    let f = accept @Fork sf in
    wait f;
    forkServer sf

waiter : Int -> SharedFork -> SharedFork -> Waiter -> dualof Waiter -> ()
waiter id lastFork rightFork w r = 
    if id > 20
    then 
        waiter id lastFork rightFork w r
    else
        if id == 1
        then
            let f = forkWith @SharedFork @() forkServer in
            fork @() (\_:()1-> philosopher id w f lastFork);
            waiter (id+1) lastFork f w r
        else
            if id == 20
            then 
                receive r;
                fork @() (\_:()1-> philosopher id w lastFork rightFork);
                -- waiter id (n-1) () () w r
                waiter (id+1) lastFork rightFork w r
            else
                let f = forkWith @SharedFork @() forkServer in
                fork @() (\_:()1-> philosopher id w f rightFork);
                waiter (id+1) lastFork f w r


main : ()
main =   
    let (w, r) = new @Waiter () in
    let f = forkWith @SharedFork @() forkServer in
    -- waiter id (n-1) f () w r
    waiter 1 f f w r;
    print @String "Done!"