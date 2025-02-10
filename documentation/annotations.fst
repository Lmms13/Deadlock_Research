send: ! T;S  ->  T  ->  S
receive: ? T;S  ->  T×S

type X = ! ();X
type Y = ! ();Y

server : dualof X -> Y 1-> ()
server x y = 
    let (_,x) = receive x in
    let y = send () y in
    server x y 

client : X -> dualof Y 1-> ()
client x y = 
    let (_,y) = receive y in
    let x = send () x in
    client x y

main : ()
main = 
    let (x1,x2) = new @X () in
    let (y1,y2) = new @Y () in
    fork @() (\_:()1-> server x2 y1);
    client x1 y2