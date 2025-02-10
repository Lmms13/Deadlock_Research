type IdChecker = !Int; ?Bool; Close

client : IdChecker -> Int 1-> ()
client c id =
    let c = send id c in
    let (b,c) = receive c in
    putStrLn (if b then "Id is valid" else "Id is invalid"); 
    close c

server : dualof IdChecker 1-> ()
server c =
    let (id,c) = receive c in 
    let c = send (isValid id) c in
    wait c

isValid : Int -> Bool
isValid id = id < 10

main: ()
main = 
    let (c, s) = new @IdChecker () in
    fork @() (\_ : () 1-> server s);
    client c 1
    -- let c = forkWith @IdChecker @() server in
    -- client c 1