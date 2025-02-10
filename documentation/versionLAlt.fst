type Fork = !();?();!();Fork

philosopher : Int -> Fork -> dualof Fork 1-> ()
philosopher id left right = 
    if(even id) 
    then
        let left = send () left in
        let (_,left) = receive left in
        let (_,right) = receive right in
        let right = send () right in
        --eating state
        let left = send () left in
        let (_,right) = receive right in
        philosopher id left right
    else
        let (_,right) = receive right in
        let right = send () right in
        let left = send () left in
        let (_,left) = receive left in
        --eating state
        let (_,right) = receive right in
        let left = send () left in
        philosopher id left right