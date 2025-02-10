type Fork = !Int;?();!();Fork

philosopher : Int -> Fork -> dualof Fork 1-> ()
philosopher id left right = 
    let left = send id left in
    let (idr,right) = receive right in
    if(id < idr) 
    then
        let (_,left) = receive left in
        let right = send () right in
        --eating state
        let left = send () left in
        let (_,right) = receive right in
        philosopher id left right
    else
        let right = send () right in
        let (_,left) = receive left in
        --eating state
        let (_,right) = receive right in
        let left = send () left in
        philosopher id left right