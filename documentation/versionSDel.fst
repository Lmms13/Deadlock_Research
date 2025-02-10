philosopher : Int -> SharedFork -> SharedFork -> ()
philosopher id left right = 
    let l = receive_ @Fork left in
    --stall
    let r = receive_ @Fork right in
    --eating state
    sleep 5000;
    close l;
    close r

timelyPhilosopher : Int -> SharedFork -> SharedFork -> ()
timelyPhilosopher id left right =
    philosopher id left right;
    timelyPhilosopher id left right

delayedPhilosopher : Int -> SharedFork -> SharedFork -> ()
delayedPhilosopher id left right =
    sleep 5000;
    philosopher id left right;
    timelyPhilosopher id left right