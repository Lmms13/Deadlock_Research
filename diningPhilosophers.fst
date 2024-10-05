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
  fork @() (\_:()1-> philosopher 1);
  fork @() (\_:()1-> philosopher 2);
  -- philosopher 1;
  -- philosopher 2;
  philosopher 3


  

