philosopher : Int -> SharedFork -> SharedFork -> ()
philosopher id left right = 
    checkOdd id;
    let l = receive_ @Fork left in
    --stall
    let r = receive_ @Fork right in
    --eating state
    close l;
    close r;
    philosopher id left right

checkOdd : Int -> ()
checkOdd n = if odd n then sleep 500 else () 