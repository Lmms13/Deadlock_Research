-- type Fork = !();?();Close
type SharedFork = *?Fork
type Fork = !();?();Close

type Waiter = *?CheckOut
type CheckOut = Close

philosopher : Int -> Waiter -> SharedFork -> SharedFork -> ()
philosopher id c left right = 
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    let c = receive_ @CheckOut c in
    let left = receive_ @Fork left in
    let right = receive_ @Fork right in
    let left = send () left in
    let right = send () right in
    let (_,left) = receive left in
    let (_,right) = receive right in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    close left;
    close right;
    close c

forkServer : dualof SharedFork -> ()
forkServer sf =
    let f = accept @Fork sf in
    let (_,f) = receive f in
    let f = send () f in
    wait f

waiter : Int -> dualof Waiter -> ()
waiter n w = 
    if n == 0
    then ()
    else
        let c = accept @CheckOut w in
        wait c;
        waiter (n-1) w


main : ()
main = 
    -- let w = forkWith @Waiter @() waiter 7 in  
    let (w, r) = new @Waiter () in
    let f1 = forkWith @SharedFork @() forkServer in
    let f2 = forkWith @SharedFork @() forkServer in
    let f3 = forkWith @SharedFork @() forkServer in
    let f3 = forkWith @SharedFork @() forkServer in
    let f4 = forkWith @SharedFork @() forkServer in
    let f5 = forkWith @SharedFork @() forkServer in
    let f6 = forkWith @SharedFork @() forkServer in
    let f7 = forkWith @SharedFork @() forkServer in
    fork @() (\_:()1-> philosopher 1 w f1 f7);
    fork @() (\_:()1-> philosopher 2 w f2 f1);
    fork @() (\_:()1-> philosopher 3 w f3 f2);
    fork @() (\_:()1-> philosopher 4 w f4 f3);
    fork @() (\_:()1-> philosopher 5 w f5 f4);
    fork @() (\_:()1-> philosopher 6 w f6 f5);
    fork @() (\_:()1-> philosopher 7 w f7 f6);
    waiter 7 r;
    print @String "Done!"
