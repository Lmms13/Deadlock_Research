-- type Fork = !();?();Close
type SharedFork = *?Fork;SharedFork
type Fork = !();?();Close

-- type Waiter = *?CheckOut
-- type CheckOut = Close

philosopher : Int -> SharedFork -> SharedFork -> ()
philosopher id left right = 
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
    -- let c = receive_ @CheckOut c in
    let l = receive_ @Fork left in
    let r = receive_ @Fork right in
    let left = send () left in
    let right = send () right in
    let (_,left) = receive left in
    let (_,right) = receive right in
    putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
    close left;
    close right
    philosopher id l r

forkServer : dualof SharedFork -> ()
forkServer sf =
    let (sf,f) = accept' @Fork sf in
    let (_,f) = receive f in
    let f = send () f in
    wait f;
    forkServer sf

accept' : SharedFork -> (SharedFork, dualof Fork)
accept' ch =
    let (x, y) = new @Fork () in
    let ch = send x ch in
    (ch, y)

-- waiter : Int -> dualof Waiter -> ()
-- waiter n w = 
--     if n == 0
--     then ()
--     else
--         let c = accept @CheckOut w in
--         wait c;
--         waiter (n-1) w


main : ()
main = 
    -- let w = forkWith @Waiter @() waiter 7 in  
    -- let (w, r) = new @Waiter () in
    let f1 = forkWith @SharedFork @() forkServer in
    let f2 = forkWith @SharedFork @() forkServer in
    let f3 = forkWith @SharedFork @() forkServer in
    let f3 = forkWith @SharedFork @() forkServer in
    let f4 = forkWith @SharedFork @() forkServer in
    let f5 = forkWith @SharedFork @() forkServer in
    let f6 = forkWith @SharedFork @() forkServer in
    let f7 = forkWith @SharedFork @() forkServer in
    fork @() (\_:()1-> philosopher 1 f1 f7);
    fork @() (\_:()1-> philosopher 2 f2 f1);
    fork @() (\_:()1-> philosopher 3 f3 f2);
    fork @() (\_:()1-> philosopher 4 f4 f3);
    fork @() (\_:()1-> philosopher 5 f5 f4);
    fork @() (\_:()1-> philosopher 6 f6 f5);
    -- fork @() (\_:()1-> philosopher 7 w f7 f6);
    philosopher 7 f7 f6;
    print @String "Done!"
