import List

type SharedFork = *?Fork
type Fork = Close

type Waiter = *!()

philosopher : Int -> Waiter -> SharedFork -> SharedFork -> ()
philosopher id c left right = 
    let left = receive_ @Fork left in
    putStrLn $ "Philosopher " ^^ (show @Int id) ^^ " acquired left fork.";
    let right = receive_ @Fork right in
    putStrLn $ "Philosopher " ^^ (show @Int id) ^^ " is eating.";
    close left;
    close right;
    send () c; 
    ()

forkServer : dualof SharedFork -> ()
forkServer sf =
    let f = accept @Fork sf in
    wait f;
    forkServer sf

-- waiter : Int -> dualof Waiter -> ()
-- waiter n w = 
--     if n == 0
--     then ()
--     else
--         receive w;
--         waiter (n-1) w

-- gatekeeper : dualof Waiter -> Int -> Int -> Waiter -> SharedFork -> SharedFork -> ()
-- gatekeeper w n id c left right = 
--     receive w;
--     fork @() (\_:()1-> philosopher id c left right);
--     waiter (n-1) w
--     --I have both n and id here because they may not always be the same

waiter : Int -> [SharedFork] -> Waiter -> dualof Waiter -> ()
waiter n forks w r = 
    let len = length forks in
    if len == 0
    then 
        if n == 0
        then 
            ()
        else
        receive r;
        waiter (n-1) [] w r
    else
        if n == 1
        then 
            fork @() (\_:()1-> philosopher n w (head forks) (last forks));
            waiter (n+1) (tail (init forks)) w r
        else
            if len == 2
            then 
                receive r;
                fork @() (\_:()1-> philosopher n w (head forks) (head (tail forks)));
                waiter (n+1) (tail(tail forks)) w r
            else
                fork @() (\_:()1-> philosopher n w (head forks) (head (tail forks)));
                waiter (n+1) (tail(tail forks)) w r


main : ()
main =   
    let (w, r) = new @Waiter () in
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
    let l = [f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, 
        f12, f13, f14, f15, f16, f17, f18, f19, f20] in
    waiter 1 l w r;
    print @String "Done!"