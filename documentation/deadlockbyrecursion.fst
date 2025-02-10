type X = !Int;X

sleep : Int -> ()
sleep n = if n == 0 then () else sleep (n-1)

server : Int -> dualof X 1-> ()
server n x = 
    -- sleep 10000;
    let (n,x) = receive x in
    putStrLn $ "received " ^^ (show @Int n);
    server n x

client : Int -> X 1-> ()
client n x = 
    sleep 1000000;
    let x = send n x in
    putStrLn $ "sent " ^^ (show @Int n);
    -- if n == 10000 
    --     then sleep 1000000; client (n-1) x 
    --     else client (n+1) x
    client (n+1) x

main : ()
main = 
    let (x1,x2) = new @X () in
    fork @() (\_:()1-> server 0 x2);
    client 1 x1