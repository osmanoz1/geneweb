open OUnit2

let display_note _ =
  let is =
    "Aliquam egestas varius ligula. Mauris et ante ut lectus hendrerit \
     varius a quis elit. Phasellus fringilla purus id diam finibus \
     laoreet. Morbi ac erat vitae massa dapibus volutpat eu a lorem."
  in
  let es =
    "1 NOTE Aliquam egestas varius ligula. Mauris et ante ut lectus hendrerit variu\n\
     2 CONC s a quis elit. Phasellus fringilla purus id diam finibus laoreet. Morbi\n\
     2 CONC  ac erat vitae massa dapibus volutpat eu a lorem.\n"
  in
  let test exp inp =
    let ic, oc = Unix.pipe () in
    let ic = Unix.in_channel_of_descr ic in
    let oc = Unix.out_channel_of_descr oc in
    Gwb2gedlib.display_note oc 1 is ;
    close_out oc ;
    let b = Buffer.create (String.length is) in
    let rec loop () =
      match input_char ic with
      | c -> Buffer.add_char b c ; loop ()
      | exception End_of_file -> close_in ic ;
    in loop () ;
    let s = Buffer.contents b in
    assert_equal ~printer:(fun s -> s) exp s
  in
  test es is

let suite =
  [ "Gwb2ged" >:::
    [ "display_note" >:: display_note ]
  ]
