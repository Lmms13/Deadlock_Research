type Semaphore = *?Sem
type Sem = +{SemWait: WaitAck, SemSignal: Wait}
type WaitAck = &{Go: Wait}

type SharedFork = *?Fork
type Fork = Close

philosopher : Int -> Semaphore -> SharedFork -> SharedFork -> ()
philosopher id sem left right = 
    match sem |> receive_ @Sem |> select SemWait with {
        Go s ->
            let l = receive_ @Fork left in
            --stall
            let r = receive_ @Fork right in
            wait s;
            --eating state
            close l;
            close r;
            receive_ @Sem sem |> select SemSignal |> wait;
            philosopher id sem left right
    }