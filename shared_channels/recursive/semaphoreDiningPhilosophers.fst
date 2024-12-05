import Semaphore

type SharedFork = *?Fork
type Fork = Close

type Waiter = *!()

philosopher : Int -> SharedFork -> SharedFork -> Semaphore -> Semaphore -> ()
philosopher id left right leftSem rightSem  =
    match leftSem |> receive_ @Sem |> select SemWait with {
        Go sl ->
            match rightSem |> receive_ @Sem |> select SemWait with {
                Go sr ->
                    let l = receive_ @Fork left in
                    putStrLn $ "Philosopher " ^^ (show @Int id) ^^ " acquired left fork.";
                    let r = receive_ @Fork right in
                    wait sl;
                    wait sr;
                    putStrLn $ "Philosopher " ^^ (show @Int id) ^^ " is eating.";
                    sleep 50000;
                    close l;
                    close r;
                    receive_ @Sem leftSem |> select SemSignal |> wait;
                    receive_ @Sem rightSem |> select SemSignal |> wait;
                    philosopher id left right leftSem rightSem
            }
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
    let s1 = launchSemServer 1 in
    let s2 = launchSemServer 1 in
    let s3 = launchSemServer 1 in
    let s4 = launchSemServer 1 in
    let s5 = launchSemServer 1 in
    let s6 = launchSemServer 1 in
    let s7 = launchSemServer 1 in
    let s8 = launchSemServer 1 in
    let s9 = launchSemServer 1 in
    let s10 = launchSemServer 1 in
    let s11 = launchSemServer 1 in
    let s12 = launchSemServer 1 in
    let s13 = launchSemServer 1 in
    let s14 = launchSemServer 1 in
    let s15 = launchSemServer 1 in
    let s16 = launchSemServer 1 in
    let s17 = launchSemServer 1 in
    let s18 = launchSemServer 1 in
    let s19 = launchSemServer 1 in
    let s20 = launchSemServer 1 in
    fork @() (\_:()1-> philosopher 1 f1 f20 s1 s20);
    fork @() (\_:()1-> philosopher 2 f2 f1 s2 s1);
    fork @() (\_:()1-> philosopher 3 f3 f2 s3 s2);
    fork @() (\_:()1-> philosopher 4 f4 f3 s4 s3);
    fork @() (\_:()1-> philosopher 5 f5 f4 s5 s4);
    fork @() (\_:()1-> philosopher 6 f6 f5 s6 s5);
    fork @() (\_:()1-> philosopher 7 f7 f6 s7 s6);
    fork @() (\_:()1-> philosopher 8 f8 f7 s8 s7);
    fork @() (\_:()1-> philosopher 9 f9 f8 s9 s8);
    fork @() (\_:()1-> philosopher 10 f10 f9 s10 s9);
    fork @() (\_:()1-> philosopher 11 f11 f10 s11 s10);
    fork @() (\_:()1-> philosopher 12 f12 f11 s12 s11);
    fork @() (\_:()1-> philosopher 13 f13 f12 s13 s12);
    fork @() (\_:()1-> philosopher 14 f14 f13 s14 s13);
    fork @() (\_:()1-> philosopher 15 f15 f14 s15 s14);
    fork @() (\_:()1-> philosopher 16 f16 f15 s16 s15);
    fork @() (\_:()1-> philosopher 17 f17 f16 s17 s16);
    fork @() (\_:()1-> philosopher 18 f18 f17 s18 s17);
    fork @() (\_:()1-> philosopher 19 f19 f18 s19 s18);
    philosopher 20 f20 f19 s20 s19;
    print @String "Done!"