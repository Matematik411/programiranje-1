type 'a resize_array = (int * 'a option array) ref

let testni = ref (3, [|Some 1; Some 2; Some 3; None|])

let append a resize_array =
  let porabljeni, array = !resize_array
  in
  let d = Array.length array
  in
  let () =
    (if porabljeni = d then 
      (
        let f = function
          | n when n < d -> array.(n)
          | n -> None
        in
        let nov = Array.init (2*d) f 
        in
        resize_array := (porabljeni, nov)
      )
    )
  in
  let porabljeni, array = !resize_array 
  in
  let () = array.(porabljeni) <- Some a 
  in
  resize_array := (porabljeni + 1, array)
