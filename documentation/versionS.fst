type SharedFork = *?Fork
type Fork = Close

philosopher : Int -> SharedFork -> SharedFork -> ()
philosopher id left right = 
    let l = receive_ @Fork left in
    --stall
    let r = receive_ @Fork right in
    --eating state
    close l;
    close r;
    philosopher id left right

forkServer : dualof SharedFork -> ()
forkServer sf =
    let f = accept @Fork sf in
    wait f;
    forkServer sf