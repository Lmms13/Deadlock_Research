type Hand = !();?();!();Hand

philosopher : Int -> Hand -> Hand 1-> ()
philosopher id left right =
    let left = send () left in
    let right = send () right in
    let (_, left) = receive left in
    let (_, right) = receive right in
    --eating state
    let left = send () left in
    let right = send () right in
    philosopher id left right

fork_ : dualof Hand -> dualof Hand 1-> ()
fork_ left right =
    let (_,right) = receive right in
    let right = send () right in
    let (_,right) = receive right in
    let (_,left) = receive left in
    let left = send () left in
    let (_,left) = receive left in
    fork_ left right