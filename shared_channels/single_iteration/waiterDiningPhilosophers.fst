import Semaphore

type SharedFork = *?Fork
type Fork = Close

type Waiter = *!()

philosopher : Int -> Waiter -> Semaphore -> SharedFork -> SharedFork -> ()
philosopher id w sem left right = 
    match sem |> receive_ @Sem |> select SemWait with {
        Go s ->
            let left = receive_ @Fork left in
            putStrLn $ "Philosopher " ^^ (show @Int id) ^^ " acquired left fork.";
            let right = receive_ @Fork right in
            -- sleep 1000;
            wait s;
            putStrLn $ "Philosopher " ^^ (show @Int id) ^^ " is eating.";
            close left;
            close right;
            receive_ @Sem sem |> select SemSignal |> wait;
            send () w;
            ()
    }

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
    let sem = launchSemServer 19 in
    fork @() (\_:()1-> philosopher 1 w sem f1 f20);
    fork @() (\_:()1-> philosopher 2 w sem f2 f1);
    fork @() (\_:()1-> philosopher 3 w sem f3 f2);
    fork @() (\_:()1-> philosopher 4 w sem f4 f3);
    fork @() (\_:()1-> philosopher 5 w sem f5 f4);
    fork @() (\_:()1-> philosopher 6 w sem f6 f5);
    fork @() (\_:()1-> philosopher 7 w sem f7 f6);
    fork @() (\_:()1-> philosopher 8 w sem f8 f7);
    fork @() (\_:()1-> philosopher 9 w sem f9 f8);
    fork @() (\_:()1-> philosopher 10 w sem f10 f9);
    fork @() (\_:()1-> philosopher 11 w sem f11 f10);
    fork @() (\_:()1-> philosopher 12 w sem f12 f11);
    fork @() (\_:()1-> philosopher 13 w sem f13 f12);
    fork @() (\_:()1-> philosopher 14 w sem f14 f13);
    fork @() (\_:()1-> philosopher 15 w sem f15 f14);
    fork @() (\_:()1-> philosopher 16 w sem f16 f15);
    fork @() (\_:()1-> philosopher 17 w sem f17 f16);
    fork @() (\_:()1-> philosopher 18 w sem f18 f17);
    fork @() (\_:()1-> philosopher 19 w sem f19 f18);
    fork @() (\_:()1-> philosopher 20 w sem f20 f19);
    waiter 20 r;
    print @String "Done!"