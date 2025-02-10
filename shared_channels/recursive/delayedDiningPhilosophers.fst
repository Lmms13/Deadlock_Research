--only works for n > 3

type SharedFork = *?Fork
type Fork = Close

type Waiter = *!()

philosopher : Int -> SharedFork -> SharedFork -> ()
philosopher id left right = 
    let l = receive_ @Fork left in
    putStrLn $ "Philosopher " ^^ (show @Int id) ^^ " acquired left fork.";
    let r = receive_ @Fork right in
    putStrLn $ "Philosopher " ^^ (show @Int id) ^^ " is eating.";
    wait' 5000;
    close l;
    close r

timelyPhilosopher : Int -> SharedFork -> SharedFork -> ()
timelyPhilosopher id left right =
    philosopher id left right;
    timelyPhilosopher id left right

delayedPhilosopher : Int -> SharedFork -> SharedFork -> ()
delayedPhilosopher id left right =
    wait' 5000;
    philosopher id left right;
    timelyPhilosopher id left right

wait' : Int -> ()
wait' n = if n == 0 then () else wait' (n-1)

forkServer : dualof SharedFork -> ()
forkServer sf =
    let f = accept @Fork sf in
    wait f;
    forkServer sf

main : ()
main = 
    let f1 = forkWith @SharedFork @() forkServer in
    let f2 = forkWith @SharedFork @() forkServer in
    let f3 = forkWith @SharedFork @() forkServer in
    -- let f4 = forkWith @SharedFork @() forkServer in
    -- let f5 = forkWith @SharedFork @() forkServer in
    -- let f6 = forkWith @SharedFork @() forkServer in
    -- let f7 = forkWith @SharedFork @() forkServer in
    -- let f8 = forkWith @SharedFork @() forkServer in
    -- let f9 = forkWith @SharedFork @() forkServer in
    -- let f10 = forkWith @SharedFork @() forkServer in
    -- let f11 = forkWith @SharedFork @() forkServer in
    -- let f12 = forkWith @SharedFork @() forkServer in
    -- let f13 = forkWith @SharedFork @() forkServer in
    -- let f14 = forkWith @SharedFork @() forkServer in
    -- let f15 = forkWith @SharedFork @() forkServer in
    -- let f16 = forkWith @SharedFork @() forkServer in
    -- let f17 = forkWith @SharedFork @() forkServer in
    -- let f18 = forkWith @SharedFork @() forkServer in
    -- let f19 = forkWith @SharedFork @() forkServer in
    -- let f20 = forkWith @SharedFork @() forkServer in
    fork @() (\_:()1-> timelyPhilosopher 1 f1 f3);
    fork @() (\_:()1-> timelyPhilosopher 2 f2 f1);
    delayedPhilosopher 3 f3 f2;
    -- fork @() (\_:()1-> timelyPhilosopher 3 f3 f2);
    -- fork @() (\_:()1-> timelyPhilosopher 4 f4 f3);
    -- fork @() (\_:()1-> timelyPhilosopher 5 f5 f4);
    -- fork @() (\_:()1-> timelyPhilosopher 6 f6 f5);
    -- fork @() (\_:()1-> timelyPhilosopher 7 f7 f6);
    -- fork @() (\_:()1-> delayedPhilosopher 8 f8 f7);
    -- fork @() (\_:()1-> timelyPhilosopher 9 f9 f8);
    -- fork @() (\_:()1-> timelyPhilosopher 10 f10 f9);
    -- fork @() (\_:()1-> timelyPhilosopher 11 f11 f10);
    -- fork @() (\_:()1-> timelyPhilosopher 12 f12 f11);
    -- fork @() (\_:()1-> timelyPhilosopher 13 f13 f12);
    -- fork @() (\_:()1-> timelyPhilosopher 14 f14 f13);
    -- fork @() (\_:()1-> timelyPhilosopher 15 f15 f14);
    -- fork @() (\_:()1-> timelyPhilosopher 16 f16 f15);
    -- fork @() (\_:()1-> delayedPhilosopher 17 f17 f16);
    -- fork @() (\_:()1-> timelyPhilosopher 18 f18 f17);
    -- fork @() (\_:()1-> timelyPhilosopher 19 f19 f18);
    -- timelyPhilosopher 20 f20 f19;
    print @String "Done!"

-- for ($i = 0; $i -lt 1000; $i++) {}