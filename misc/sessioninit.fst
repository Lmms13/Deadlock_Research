type Cell = +{ Read: ?Int
             , Write: !Int
             }; Close

type SharedCell = *?Cell

cellClient : SharedCell -> ()
cellClient c =
  c |> receive_ @Cell  -- acquire a linear channel 
    |> select Write    -- interact on the linear channel
    |> send 5
    |> close

cellServer : dualof SharedCell -> ()
cellServer c =
  match accept @Cell c with {
    Read s ->
      send 0 s,
    Write s ->
      let (i, c) = receive s in c
  } |> wait


main : ()
main = 
    let c = forkWith @SharedCell @() cellServer in
    fork @() (\_:() -> cellClient c)
