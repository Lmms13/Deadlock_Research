type Fork = !();?();!();Fork

philosopher : Int -> Fork -> dualof Fork 1-> ()
philosopher id left right = 
    let left = send () left in
    let (_,right) = receive right in
    let (_,left) = receive left in
    let right = send () right in
    --eating state
    let left = send () left in
    let (_,right) = receive right in
    philosopher id left right