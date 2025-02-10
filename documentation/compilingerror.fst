type X = !();X

server : Int -> dualof X 1-> ()
server n x = 
    let (_,x) = receive x in
    server n x

client : Int -> X 1-> ()
client n x = 
    putStrLn $ "CLI";
    let x = send () x in
    client n x

main : ()
main = 
    let (x1,x2) = new @X () in
    fork @() (\_:()1-> server 0 x2);
    client 1 x1

-------------------------------------------------------

-- server : dualof X 1-> ()
-- server x = 
--     let (_,x) = receive x in
--     server x


-- client : X 1-> ()
-- client x = 
--     let x = send () x in
--     client x

-- main : ()
-- main = 
--     let (x1,x2) = new @X () in
--     fork @() (\_:()1-> server x2);
--     client x1