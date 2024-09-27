type Fork = +{ Take: !(); ?Bool; Put: !(); ?Bool} ; Close

takeFork : Fork -> ()
takeFork f =
    let b = f |> select Take |> send () |> receive in
    print @Bool b;

putFork : Fork -> ()
putFork f =
    let b = f |> select Put |> send () |> receive in
    print @Bool b;

forkServer : dualof Fork -> ()
forkServer (Take f) =

    sendAndWait @Bool True c;
    forkServer c



type Philosopher = +{ Eat: !(), Think: !()} ; Close
--type Fork = +{ Take: ?(), Put: ?()} ; Close 

philosopher : Philosopher -> Fork -> Fork -> ()
philosopher p left_fork right_fork =
  p |> select Think |> send ();
  left_fork |> select Take |> receive;
  right_fork |> select Take |> receive;
  p |> select Eat |> send ();
  right_fork |> select Put |> receive;
  left_fork |> select Put |> receive;
  philosopher p left_fork right_fork

fork_ : Fork -> ()
fork_ f =
  f |> select Take |> receive;
  f |> select Put |> receive;
  fork_ f

main : ()
main =
  let (p1, f1) = new @(Philosopher, Fork) () in
  let (p2, f2) = new @(Philosopher, Fork) () in
  let (p3, f3) = new @(Philosopher, Fork) () in
  let (p4, f4) = new @(Philosopher, Fork) () in
  let (p5, f5) = new @(Philosopher, Fork) () in
  fork @() (\_:() 1-> philosopher p1 f1 f2);
  fork @() (\_:() 1-> philosopher p2 f2 f3);
  fork @() (\_:() 1-> philosopher p3 f3 f4);
  fork @() (\_:() 1-> philosopher p4 f4 f5);
  fork @() (\_:() 1-> philosopher p5 f5 f1);
  fork @() (\_:() 1-> fork_ f1);
  fork @() (\_:() 1-> fork_ f2);
  fork @() (\_:() 1-> fork_ f3);
  fork @() (\_:() 1-> fork_ f4);
  fork @() (\_:() 1-> fork_ f5)
