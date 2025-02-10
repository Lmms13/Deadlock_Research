type Hand = !Int;?();!();Hand

philosopher : Int -> Hand 1-> Hand 1-> ()
philosopher id left right =
    let left = send id left in
    let right = send id right in
    let (_, left) = receive left in
    let (_, right) = receive right in
    --eating state
    let left = send () left in
    let right = send () right in
    philosopher id left right

fork_ : dualof Hand -> dualof Hand 1-> ()
fork_ left right =
    let (_,right) = receive right in
    let (id,left) = receive left in
    if(even id) 
    then
        let left = unitaryFork left in
        let right = unitaryFork right in
        fork_ left right
    else
        let right = unitaryFork right in
        let left = unitaryFork left in
        fork_ left right

unitaryFork : !();?();dualof Hand -> dualof Hand
unitaryFork f =
    let f = send () f in
    let (_, f) = receive f in
    f