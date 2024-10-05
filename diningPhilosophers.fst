-- -- type Fork = +{ Take: !(); ?Bool; Put: !(); ?Bool} ; Close

-- -- takeFork : Fork -> ()
-- -- takeFork f =
-- --     let b = f |> select Take |> send () |> receive in
-- --     print @Bool b;

-- -- putFork : Fork -> ()
-- -- putFork f =
-- --     let b = f |> select Put |> send () |> receive in
-- --     print @Bool b;

-- -- forkServer : dualof Fork -> ()
-- -- forkServer (Take f) =

-- --     sendAndWait @Bool True c;
-- --     forkServer c



-- -- type Philosopher = +{ Eat: !(), Think: !()} ; Close
-- -- --type Fork = +{ Take: ?(), Put: ?()} ; Close 

-- -- philosopher : Philosopher -> Fork -> Fork -> ()
-- -- philosopher p left_fork right_fork =
-- --   p |> select Think |> send ();
-- --   left_fork |> select Take |> receive;
-- --   right_fork |> select Take |> receive;
-- --   p |> select Eat |> send ();
-- --   right_fork |> select Put |> receive;
-- --   left_fork |> select Put |> receive;
-- --   philosopher p left_fork right_fork

-- -- fork_ : Fork -> ()
-- -- fork_ f =
-- --   f |> select Take |> receive;
-- --   f |> select Put |> receive;
-- --   fork_ f

-- -- main : ()
-- -- main =
-- --   let (p1, f1) = new @(Philosopher, Fork) () in
-- --   let (p2, f2) = new @(Philosopher, Fork) () in
-- --   let (p3, f3) = new @(Philosopher, Fork) () in
-- --   let (p4, f4) = new @(Philosopher, Fork) () in
-- --   let (p5, f5) = new @(Philosopher, Fork) () in
-- --   fork @() (\_:() 1-> philosopher p1 f1 f2);
-- --   fork @() (\_:() 1-> philosopher p2 f2 f3);
-- --   fork @() (\_:() 1-> philosopher p3 f3 f4);
-- --   fork @() (\_:() 1-> philosopher p4 f4 f5);
-- --   fork @() (\_:() 1-> philosopher p5 f5 f1);
-- --   fork @() (\_:() 1-> fork_ f1);
-- --   fork @() (\_:() 1-> fork_ f2);
-- --   fork @() (\_:() 1-> fork_ f3);
-- --   fork @() (\_:() 1-> fork_ f4);
-- --   fork @() (\_:() 1-> fork_ f5)



-- ---------------------------------------------------


-- -- philosopher : Int -> ?Int;Wait -> !Int;Close -> ?Int;Wait -> !Int;Close -> ()
-- -- philosopher id left_fork_r left_fork_s right_fork_r right_fork_s =
-- --   let lf = left_fork_r |> receiveAndWait @Int in
-- --   let rf = right_fork_r |> receiveAndWait @Int in
-- --   if (validfork id lf) && (validfork id rf)
-- --   then
-- --     -- eat id left_fork right_fork
-- --     right_fork_s |> send id |> close;
-- --     left_fork_s |> send id |> close;
-- --     print @String "Philosopher is eating."
-- --   else
-- --     -- think id left_fork right_fork;
-- --     right_fork_s |> sendAndClose @Int id;
-- --     left_fork_s |> sendAndClose @Int id;
-- --     print @String "Philosopher is thinking.";
-- --     philosopher id left_fork_r left_fork_s right_fork_r right_fork_s



-- -- type Fork = ?Int;!Int;Fork

-- philosopher' : Int -> Fork -> Fork -> ()
-- philosopher' id left_fork right_fork =
--   let lf = left_fork |> receive @Int in
--   let rf = right_fork |> receive @Int in
--   right_fork |> send @Int id |> philosopher' id left_fork right_fork;
--   left_fork |> send @Int id |> philosopher' id left_fork right_fork




--     -- if (validfork id lf) && (validfork id rf)
--     -- then
--     --   -- eat id left_fork right_fork
--     --   right_fork |> send @Int id |> philosopher' id left_fork right_fork;
--     --   left_fork |> send @Int id |> philosopher' id left_fork right_fork;
--     --   print @String "Philosopher is eating."
--     -- else
--     --   -- think id left_fork right_fork;
--     --   right_fork |> send @Int id |> philosopher' id left_fork right_fork;
--     --   left_fork |> send @Int id |> philosopher' id left_fork right_fork;
--     --   print @String "Philosopher is thinking.";
--     --   -- philosopher' id left_fork right_fork

--     -- philosopher id left_fork_r left_fork_s right_fork_r right_fork_s

-- -- eat : Int -> !Int;Close -> !Int;Close -> ()
-- -- eat id left_fork right_fork = 
-- --   right_fork |> send id |> close;
-- --   left_fork |> send id |> close;
-- --   print @String "Philosopher is eating."


-- -- think : Int -> !Int;Wait -> !Int;Wait -> ()
-- -- think id left_fork right_fork = 
-- --   print @String ("Philosopher " + (show id) + " is thinking.");
-- --   right_fork |> sendAndWait @Int id;
-- --   left_fork |> sendAndWait @Int id


-- validfork : Int -> Int -> Bool
-- validfork id fork_id = id == fork_id || mod (id + 1) 5 == fork_id 

-- main : ()
-- main = 
--   let (f0s, f0r) = new @Fork () in
--   let (f1s, f1r) = new @Fork () in
--   -- let (f2s, f2r) = new @(!Int;Close) () in
--   -- let (f3s, f3r) = new @(!Int;Close) () in
--   -- let (f4s, f4r) = new @(!Int;Close) () in
--   fork @() (\_:() 1-> philosopher' 0 f0s f1s);
--   philosopher' 1 f1s f0s

--   -- fork @() (\_:() 1-> philosopher 1 f1r f1s f2r f2s);
--   -- fork @() (\_:() 1-> philosopher 2 f2r f2s f3r f3s);
--   -- fork @() (\_:() 1-> philosopher 3 f3r f3s f4r f4s);
--   -- fork @() (\_:() 1-> philosopher 4 f4r f4s f0r f0s)



-- type Fork = !Bool;Close
type Fork = !();!();Close

-- philosopher : ()
-- philosopher = 
--   let (lf1, lf2) = new @Fork () in
--   let (rf1, rf2) = new @Fork () in
--   fork @() (\_:()1-> take lf1);
--   fork @() (\_:()1-> take rf1);
--   think lf2 rf2
--   --how to give 2 philosophers the same fork if the channels are consumed?

philosopher : Int -> ()
philosopher id = 
  let (lf1, lf2) = new @Fork () in
  let (rf1, rf2) = new @Fork () in
  fork @() (\_:()1-> dine id lf1 rf1);
  think id lf2 rf2

dine : Int -> Fork -> Fork 1-> ()
dine id left right = 
  let l = send () left in
  let r = send () right in
  -- send () left;
  -- send () right;
  putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is eating.");
  --putStrLn " is eating.";
  -- print @String "Philosopher is eating.";
  sendAndClose @() () l;
  sendAndClose @() () r
  -- send () left;
  -- send () right;
  -- close left;
  -- close right
  -- send () left;
  -- send () right;
  -- close left;
  -- close right
  -- left |> send () |> send () |> close;
  -- right |> send () |> send () |> close;
  -- print @String "Philosopher is eating."
  -- right |> send () |> close

think : Int -> dualof Fork -> dualof Fork 1-> ()
think id left right = 
  -- print @String "Philosopher is thinking.";
  let (i,l) = receive left in
  let (j,r) = receive right in 
  putStrLn ( "Philosopher " ^^ (show @Int id) ^^ " is thinking.");
  -- putStr "Philosopher ";
  -- putStr (show @Int id);
  -- putStrLn " is thinking.";
  receiveAndWait @() l;
  receiveAndWait @() r
  -- receiveAndWait @() left;
  -- receiveAndWait @() right;
  -- think left right
  -- receive () right;
  -- receiveAndWait @() left;
  -- receiveAndWait @() right;



-- take : Fork -> ()
-- take f = f |> send True |> close


-- think : dualof Fork -> dualof Fork 1-> ()
-- think left right = 
--   print @String "Philosopher is thinking.";
--   let l = left |> receiveAndWait @Bool in
--   let r = right |> receiveAndWait @Bool in
--   if l && r
--   then print @String "Philosopher is eating."
--   else think left right
-- --how to pass the channels through recursion?


main : ()
main =
  philosopher 1;
  philosopher 2;
  philosopher 3


  

