type SharedFork = *?Fork
type Fork = Close

type Waiter = *!()

philosopher : Int -> SharedFork -> SharedFork -> ()
philosopher id left right = 
    let left = receive_ @Fork left in
    putStrLn $ "Philosopher " ^^ (show @Int id) ^^ " acquired left fork.";
    let right = receive_ @Fork right in
    putStrLn $ "Philosopher " ^^ (show @Int id) ^^ " is eating.";
    close left;
    close right

recPhilosopher : Int -> SharedFork -> SharedFork -> ()
recPhilosopher id left right = 
    philosopher id left right;
    recPhilosopher id left right

leadingPhilosopher : Int -> Waiter -> SharedFork -> SharedFork -> ()
leadingPhilosopher id c left right = 
    philosopher id left right;
    send () c;
    recPhilosopher id left right

forkServer : dualof SharedFork -> ()
forkServer sf =
    let f = accept @Fork sf in
    wait f;
    forkServer sf

gatekeeper : dualof Waiter -> Int -> Waiter -> SharedFork -> SharedFork 1-> ()
gatekeeper w id c left right = 
    receive w;
    recPhilosopher id left right

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
    fork @() (\_:()1-> leadingPhilosopher 1 w f1 f20);
    fork @() (\_:()1-> recPhilosopher 2 f2 f1);
    fork @() (\_:()1-> recPhilosopher 3 f3 f2);
    fork @() (\_:()1-> recPhilosopher 4 f4 f3);
    fork @() (\_:()1-> recPhilosopher 5 f5 f4);
    fork @() (\_:()1-> recPhilosopher 6 f6 f5);
    fork @() (\_:()1-> recPhilosopher 7 f7 f6);
    fork @() (\_:()1-> recPhilosopher 8 f8 f7);
    fork @() (\_:()1-> recPhilosopher 9 f9 f8);
    fork @() (\_:()1-> recPhilosopher 10 f10 f9);
    fork @() (\_:()1-> recPhilosopher 11 f11 f10);
    fork @() (\_:()1-> recPhilosopher 12 f12 f11);
    fork @() (\_:()1-> recPhilosopher 13 f13 f12);
    fork @() (\_:()1-> recPhilosopher 14 f14 f13);
    fork @() (\_:()1-> recPhilosopher 15 f15 f14);
    -- fork @() (\_:()1-> philosopher 16 w f16 f15);
    fork @() (\_:()1-> recPhilosopher 17 f17 f16);
    fork @() (\_:()1-> recPhilosopher 18 f18 f17);
    fork @() (\_:()1-> recPhilosopher 19 f19 f18);
    fork @() (\_:()1-> recPhilosopher 20 f20 f19);
    gatekeeper r 16 w f16 f15;
    print @String "Done!"

-- for ($i = 0; $i -lt 1000; $i++) {}