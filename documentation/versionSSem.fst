type Semaphore = *?Sem
type Sem = +{SemWait: WaitAck, SemSignal: Wait}
type WaitAck = &{Go: Wait}

type SharedFork = *?Fork
type Fork = Close

philosopher : Int -> SharedFork -> SharedFork -> Semaphore -> Semaphore -> ()
philosopher id left right leftSem rightSem  =
    sleep 5000;
    match leftSem |> receive_ @Sem |> select SemWait with {
        Go sl ->
            match rightSem |> receive_ @Sem |> select SemWait with {
                Go sr ->
                    let l = receive_ @Fork left in
                    --stall
                    let r = receive_ @Fork right in
                    wait sl;
                    wait sr;
                    --eating state
                    close l;
                    close r;
                    receive_ @Sem leftSem |> select SemSignal |> wait;
                    receive_ @Sem rightSem |> select SemSignal |> wait;
                    philosopher id left right leftSem rightSem
            }
    }