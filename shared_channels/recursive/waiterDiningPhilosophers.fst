import Semaphore

type SharedFork = *?Fork
type Fork = Close

philosopher : Int -> Semaphore -> SharedFork -> SharedFork -> ()
philosopher id sem left right = 
    match sem |> receive_ @Sem |> select SemWait with {
        Go s ->
            let l = receive_ @Fork left in
            putStrLn $ "Philosopher " ^^ (show @Int id) ^^ " acquired left fork.";
            let r = receive_ @Fork right in
            -- sleep 5000;
            wait s;
            putStrLn $ "Philosopher " ^^ (show @Int id) ^^ " is eating.";
            close l;
            close r;
            receive_ @Sem sem |> select SemSignal |> wait;
            philosopher id sem left right
    }

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
    let sem = launchSemServer 10 in
    fork @() (\_:()1-> philosopher 1 sem f1 f20);
    fork @() (\_:()1-> philosopher 2 sem f2 f1);
    fork @() (\_:()1-> philosopher 3 sem f3 f2);
    fork @() (\_:()1-> philosopher 4 sem f4 f3);
    fork @() (\_:()1-> philosopher 5 sem f5 f4);
    fork @() (\_:()1-> philosopher 6 sem f6 f5);
    fork @() (\_:()1-> philosopher 7 sem f7 f6);
    fork @() (\_:()1-> philosopher 8 sem f8 f7);
    fork @() (\_:()1-> philosopher 9 sem f9 f8);
    fork @() (\_:()1-> philosopher 10 sem f10 f9);
    fork @() (\_:()1-> philosopher 11 sem f11 f10);
    fork @() (\_:()1-> philosopher 12 sem f12 f11);
    fork @() (\_:()1-> philosopher 13 sem f13 f12);
    fork @() (\_:()1-> philosopher 14 sem f14 f13);
    fork @() (\_:()1-> philosopher 15 sem f15 f14);
    fork @() (\_:()1-> philosopher 16 sem f16 f15);
    fork @() (\_:()1-> philosopher 17 sem f17 f16);
    fork @() (\_:()1-> philosopher 18 sem f18 f17);
    fork @() (\_:()1-> philosopher 19 sem f19 f18);
    -- fork @() (\_:()1-> philosopher 20 w sem f20 f19);
    philosopher 20 sem f20 f19;
    print @String "Done!"