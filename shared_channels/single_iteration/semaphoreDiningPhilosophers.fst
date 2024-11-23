type SharedFork = *?Fork
type Fork = Close

type Waiter = *!()

type Semaphore = *?Mutex
type Mutex = Close

philosopher : Int -> Waiter -> Semaphore -> SharedFork -> SharedFork -> ()
philosopher id waiter semaphore left right =
    let sem = receive_ @Mutex semaphore in
    let left = receive_ @Fork left in
    putStrLn $ "Philosopher " ^^ (show @Int id) ^^ " acquired left fork.";
    -- sleep 40000;
    --either version works, I'll keep the print version because it's faster
    let right = receive_ @Fork right in
    putStrLn $ "Philosopher " ^^ (show @Int id) ^^ " is eating.";
    close left;
    close right;
    close sem;
    send () waiter; 
    ()

sleep : Int -> ()
sleep n = if n == 0 then () else sleep (n-1)

semaphore : dualof Semaphore -> ()
semaphore s = 
    let sem = accept @Mutex s in
    wait sem;
    semaphore s

forkServer : dualof SharedFork -> ()
forkServer sf =
    let f = accept @Fork sf in
    wait f;
    forkServer sf

waiter : Int -> dualof Waiter -> ()
waiter n w = 
    if n == 0
    then ()
    else
        receive w;
        waiter (n-1) w

main : ()
main =   
    let (w, r) = new @Waiter () in
    let (sem_w, sem_r) = new @Semaphore () in
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
    fork @() (\_:()1-> philosopher 1 w sem_w f1 f20);
    fork @() (\_:()1-> philosopher 2 w sem_w f2 f1);
    fork @() (\_:()1-> philosopher 3 w sem_w f3 f2);
    fork @() (\_:()1-> philosopher 4 w sem_w f4 f3);
    fork @() (\_:()1-> philosopher 5 w sem_w f5 f4);
    fork @() (\_:()1-> philosopher 6 w sem_w f6 f5);
    fork @() (\_:()1-> philosopher 7 w sem_w f7 f6);
    fork @() (\_:()1-> philosopher 8 w sem_w f8 f7);
    fork @() (\_:()1-> philosopher 9 w sem_w f9 f8);
    fork @() (\_:()1-> philosopher 10 w sem_w f10 f9);
    fork @() (\_:()1-> philosopher 11 w sem_w f11 f10);
    fork @() (\_:()1-> philosopher 12 w sem_w f12 f11);
    fork @() (\_:()1-> philosopher 13 w sem_w f13 f12);
    fork @() (\_:()1-> philosopher 14 w sem_w f14 f13);
    fork @() (\_:()1-> philosopher 15 w sem_w f15 f14);
    fork @() (\_:()1-> philosopher 16 w sem_w f16 f15);
    fork @() (\_:()1-> philosopher 17 w sem_w f17 f16);
    fork @() (\_:()1-> philosopher 18 w sem_w f18 f17);
    fork @() (\_:()1-> philosopher 19 w sem_w f19 f18);
    fork @() (\_:()1-> philosopher 20 w sem_w f20 f19);
    fork @() (\_:()1-> semaphore sem_r);
    waiter 20 r;
    print @String "Done!"