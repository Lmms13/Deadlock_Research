type SharedFork = *?Fork
type Fork = Close

type Waiter = *!()

type Semaphore = *?Mutex
type Mutex = Close


philosopher : Int -> Waiter -> SharedFork -> SharedFork -> Semaphore -> Semaphore -> ()
philosopher id waiter left right leftSem rightSem  =
    let leftSem = receive_ @Mutex leftSem in
    let rightSem = receive_ @Mutex rightSem in
    let left = receive_ @Fork left in
    putStrLn $ "Philosopher " ^^ (show @Int id) ^^ " acquired left fork.";
    -- sleep 40000;
    --either version works, I'll keep the print version because it's faster
    let right = receive_ @Fork right in
    putStrLn $ "Philosopher " ^^ (show @Int id) ^^ " is eating.";
    close left;
    close right;
    close leftSem;
    close rightSem;
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
    let s1 = forkWith @Semaphore @() semaphore in
    let s2 = forkWith @Semaphore @() semaphore in
    let s3 = forkWith @Semaphore @() semaphore in
    let s4 = forkWith @Semaphore @() semaphore in
    let s5 = forkWith @Semaphore @() semaphore in
    let s6 = forkWith @Semaphore @() semaphore in
    let s7 = forkWith @Semaphore @() semaphore in
    let s8 = forkWith @Semaphore @() semaphore in
    let s9 = forkWith @Semaphore @() semaphore in
    let s10 = forkWith @Semaphore @() semaphore in
    let s11 = forkWith @Semaphore @() semaphore in
    let s12 = forkWith @Semaphore @() semaphore in
    let s13 = forkWith @Semaphore @() semaphore in
    let s14 = forkWith @Semaphore @() semaphore in
    let s15 = forkWith @Semaphore @() semaphore in
    let s16 = forkWith @Semaphore @() semaphore in
    let s17 = forkWith @Semaphore @() semaphore in
    let s18 = forkWith @Semaphore @() semaphore in
    let s19 = forkWith @Semaphore @() semaphore in
    let s20 = forkWith @Semaphore @() semaphore in
    fork @() (\_:()1-> philosopher 1 w f1 f20 s1 s20);
    fork @() (\_:()1-> philosopher 2 w f2 f1 s2 s1);
    fork @() (\_:()1-> philosopher 3 w f3 f2 s3 s2);
    fork @() (\_:()1-> philosopher 4 w f4 f3 s4 s3);
    fork @() (\_:()1-> philosopher 5 w f5 f4 s5 s4);
    fork @() (\_:()1-> philosopher 6 w f6 f5 s6 s5);
    fork @() (\_:()1-> philosopher 7 w f7 f6 s7 s6);
    fork @() (\_:()1-> philosopher 8 w f8 f7 s8 s7);
    fork @() (\_:()1-> philosopher 9 w f9 f8 s9 s8);
    fork @() (\_:()1-> philosopher 10 w f10 f9 s10 s9);
    fork @() (\_:()1-> philosopher 11 w f11 f10 s11 s10);
    fork @() (\_:()1-> philosopher 12 w f12 f11 s12 s11);
    fork @() (\_:()1-> philosopher 13 w f13 f12 s13 s12);
    fork @() (\_:()1-> philosopher 14 w f14 f13 s14 s13);
    fork @() (\_:()1-> philosopher 15 w f15 f14 s15 s14);
    fork @() (\_:()1-> philosopher 16 w f16 f15 s16 s15);
    fork @() (\_:()1-> philosopher 17 w f17 f16 s17 s16);
    fork @() (\_:()1-> philosopher 18 w f18 f17 s18 s17);
    fork @() (\_:()1-> philosopher 19 w f19 f18 s19 s18);
    fork @() (\_:()1-> philosopher 20 w f20 f19 s20 s19);
    waiter 20 r;
    print @String "Done!"