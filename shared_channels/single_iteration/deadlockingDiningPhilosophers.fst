type SharedFork = *?Fork
type Fork = Close

type Waiter = *!()
-- type CheckOut = Close

philosopher : Int -> Waiter -> SharedFork -> SharedFork -> ()
philosopher id c left right = 
    -- let c = receive_ @CheckOut c in
    let left = receive_ @Fork left in
    putStrLn $ "Philosopher " ^^ (show @Int id) ^^ " got left fork.";
     wait' 50000;
    let right = receive_ @Fork right in
    -- let left = send () left in
    -- let right = send () right in
    -- let (_,left) = receive left in
    -- let (_,right) = receive right in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    close left;
    close right;
    send () c; 
    ()

wait' : Int -> ()
wait' n = if n == 0 then () else wait' (n-1)



forkServer : dualof SharedFork -> ()
forkServer sf =
    let f = accept @Fork sf in
    -- let (_,f) = receive f in
    -- let f = send () f in
   

    wait f;
    forkServer sf

waiter : Int -> dualof Waiter -> ()
waiter n w = 
    if n == 0
    then ()
    else
        -- let c = accept @CheckOut w in
        receive w;
        waiter (n-1) w

main : ()
main = 
    -- let w = forkWith @Waiter @() (waiter 7) in  
    let (w, r) = new @Waiter () in
    -- fork @() (\_:()1-> waiter 7 r);
    let f1 = forkWith @SharedFork @() forkServer in
    let f2 = forkWith @SharedFork @() forkServer in
    let f3 = forkWith @SharedFork @() forkServer in
    let f4 = forkWith @SharedFork @() forkServer in
    let f5 = forkWith @SharedFork @() forkServer in
    let f6 = forkWith @SharedFork @() forkServer in
    let f7 = forkWith @SharedFork @() forkServer in
    let f8 = forkWith @SharedFork @() forkServer in
    let f9 = forkWith @SharedFork @() forkServer in
    let f10 = forkWith @SharedFork @() forkServer in
    let f11 = forkWith @SharedFork @() forkServer in
    let f12 = forkWith @SharedFork @() forkServer in
    let f13 = forkWith @SharedFork @() forkServer in
    let f14 = forkWith @SharedFork @() forkServer in
    let f15 = forkWith @SharedFork @() forkServer in
    let f16 = forkWith @SharedFork @() forkServer in
    let f17 = forkWith @SharedFork @() forkServer in
    let f18 = forkWith @SharedFork @() forkServer in
    let f19 = forkWith @SharedFork @() forkServer in
    let f20 = forkWith @SharedFork @() forkServer in
    -- print @String "Here!";
    fork @() (\_:()1-> philosopher 1 w f1 f20);
    fork @() (\_:()1-> philosopher 2 w f2 f1);
    fork @() (\_:()1-> philosopher 3 w f3 f2);
    fork @() (\_:()1-> philosopher 4 w f4 f3);
    fork @() (\_:()1-> philosopher 5 w f5 f4);
    fork @() (\_:()1-> philosopher 6 w f6 f5);
    -- philosopher 7 f7 f6;
    fork @() (\_:()1-> philosopher 7 w f7 f6);
    fork @() (\_:()1-> philosopher 8 w f8 f7);
    fork @() (\_:()1-> philosopher 9 w f9 f8);
    fork @() (\_:()1-> philosopher 10 w f10 f9);
    fork @() (\_:()1-> philosopher 11 w f11 f10);
    fork @() (\_:()1-> philosopher 12 w f12 f11);
    fork @() (\_:()1-> philosopher 13 w f13 f12);
    fork @() (\_:()1-> philosopher 14 w f14 f13);
    fork @() (\_:()1-> philosopher 15 w f15 f14);
    fork @() (\_:()1-> philosopher 16 w f16 f15);
    fork @() (\_:()1-> philosopher 17 w f17 f16);
    fork @() (\_:()1-> philosopher 18 w f18 f17);
    fork @() (\_:()1-> philosopher 19 w f19 f18);
    fork @() (\_:()1-> philosopher 20 w f20 f19);
    waiter 20 r
    -- print @String "Done!"
